open Promiwag_std

module Stiel_types = Promiwag_stiel
module Stiel = Promiwag_stiel.Construct
module MP_format = Promiwag_meta_packet.Packet_structure
(*
protocol stack description:

ethernet: 
   (eq protocol "IP"), IPv4
   boolean expression, packet_format name

...


stack handling request:

ethernet, passed_exprs @ [ request ] -> block * passed_expressions

*)


type atomic_transition = {
  condition: Stiel_types.bool_expression;
  next_format: string;
  passed_expressions: Stiel_types.typed_expression list;
}
  

type protocol = {
  name: string;
  format: MP_format.format;
  transitions: atomic_transition list;
}

type protocol_stack = {
  (* packet_database: MPDB.t; *)
  stack_description: (string, protocol) Ht.t;
}

type input_case =
  | Case_int_value of Stiel_types.int_expression * string
  | Case_int_range of Stiel_types.int_expression * Stiel_types.int_expression * string

type input_transition =
  | Switch of Stiel_types.int_expression * input_case list
  | Atomic of atomic_transition
  | No_transition

let empty_protcol_stack () =
  {stack_description = Ht.create 42; }
 

let atomic_transition
    ?(condition=Stiel_types.Bool_expr_false)
    ?(passed_expressions=[])
    next_format =
  {condition = condition; next_format = next_format; 
   passed_expressions = passed_expressions; }

let transform_input_case int_expr = function
  | Case_int_value (ie, next) -> 
    let condition = Stiel.eq int_expr ie in
    atomic_transition ~condition next
  | Case_int_range (iea, ieb, next) -> 
    let condition =
      Stiel.bool 
        (`And (`E (Stiel.le iea int_expr),
               `E (Stiel.le int_expr ieb))) in
    atomic_transition ~condition
      ~passed_expressions:[Stiel.expr_unat int_expr] next

let transform_input_transitions = function
  | No_transition -> []
  | Switch (iexpr, cases) -> Ls.map cases ~f:(transform_input_case iexpr)
  | Atomic a -> [a]

let case_int_value int_value next_format =
  Case_int_value (Stiel.uint int_value, next_format)

let case_int_range bottom top next_format =
  Case_int_range (Stiel.uint bottom, Stiel.uint top, next_format)

let switch field cases =
  Switch (Stiel.int_var field, cases)

let empty_transition =
  No_transition


let add_protocol ps name format packet_transitions =
  Ht.add ps.stack_description name {
    name = name;
    format = format;
    transitions = transform_input_transitions packet_transitions;
  };
  ()
    
  
module To_string = struct

  module Stiel2S = Promiwag_stiel.To_string
  module Packet2S = Promiwag_meta_packet.To_string

  let make_indent i = Str.make (2 * i) ' '

  let atomic_transition ?(indent=0) at = 
    let indent_str = make_indent indent in
    let more_indent = make_indent (indent + 1) in
    let more_more_indent = make_indent (indent + 2) in
    let passed_expressions =
      let pref, sep, suf = 
        if Ls.length at.passed_expressions > 0 then
          (sprintf "\n%s"  more_more_indent, 
           sprintf ";\n%s" more_more_indent,
           sprintf "\n%s"  more_indent)
        else 
          ("", "", "")
      in
      sprintf "%s%s%s" pref
        (Str.concat sep (Ls.map Stiel2S.typed_expression at.passed_expressions))
        suf
    in 
    sprintf "%sWhen [%s]\n%s-> Next Is \"%s\" with [%s]"
      indent_str (Stiel2S.bool_expression at.condition) more_indent
      at.next_format passed_expressions
  
      
  let protocol ?(indent=0) p =
    let indent_str = make_indent indent in
    let next_indent = make_indent (indent + 1) in
    let transitions =
      match p.transitions with
      | [] -> sprintf "%sNo transitions." next_indent
      | l ->
        (Str.concat "\n" (Ls.map l ~f:(atomic_transition ~indent:(indent + 1))))
    in
    sprintf "%sProtocol \"%s\": {\n%s%s%s} ==> {\n%s\n%s}"
      indent_str p.name next_indent
      (Packet2S.format ~sep:next_indent ~suffix:";\n" p.format) 
      indent_str transitions indent_str
      
      
  let protocol_stack ps =
    let r = ref [] in
    Ht.iter (fun name p ->
      r := (protocol ~indent:1 p) :: !r;
    ) ps.stack_description;
    sprintf "Protocol Stack: {\n%s\n}" (Str.concat "\n" (Ls.rev !r))

end
