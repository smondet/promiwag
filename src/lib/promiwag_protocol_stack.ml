open Promiwag_std

module Stiel_types = Promiwag_stiel
module Stiel = Promiwag_stiel.Construct
module MPDB = Promiwag_meta_packet.Packet_database
module MPS = Promiwag_meta_packet.Packet_structure
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
  
type protocol_transitions = {
  packet_format: string;
  transitions:  atomic_transition list;
}



type protocol_stack = {
  packet_database: MPDB.t;
  stack_description: (string, protocol_transitions) Ht.t;
}


let protocol_stack_with_packet_database packet_db =
  { packet_database = packet_db; stack_description = Ht.create 42; }
    
let empty_protcol_stack () =
  { packet_database = MPDB.empty (); stack_description = Ht.create 42; }


let add_protocol_transitions ps pts =
  Ht.add ps.stack_description pts.packet_format pts
    
let add_protocol ps packet_format packet_transitions =
  let name, format = packet_format in
  if name <> packet_transitions.packet_format then (
    failwith (sprintf "Promiwag_protocol_stack.add_protcol: \
                Format and Transitions do not match: %S Vs %S."
                name packet_transitions.packet_format)
  );
  MPDB.add ps.packet_database packet_format;
  add_protocol_transitions ps packet_transitions;
  ()

let atomic_transition
    ?(condition=Stiel_types.Bool_expr_false)
    ?(passed_expressions=[])
    next_format =
  {condition = condition; next_format = next_format; 
   passed_expressions = passed_expressions; }

type input_case =
  | Case_int_value of Stiel_types.int_expression * string
  | Case_int_range of Stiel_types.int_expression * Stiel_types.int_expression * string

type input_transition =
  | Switch of Stiel_types.int_expression * input_case list
  | Atomic of atomic_transition
  | No_transition
  



let transform_input_case int_expr = function
  | Case_int_value (ie, next) -> 
    let condition = Stiel.eq int_expr ie in
    atomic_transition ~condition next
  | Case_int_range (iea, ieb, next) -> 
    let condition =
      Stiel.ls_and [(Stiel.le iea int_expr); (Stiel.ge int_expr ieb)] in
    atomic_transition ~condition next

let transform_input_transitions = function
  | No_transition -> []
  | Switch (iexpr, cases) -> []
  | Atomic a -> [a]


let case_int_value int_value next_format =
  Case_int_value (Stiel.uint int_value, next_format)

let case_int_range bottom top next_format =
  Case_int_range (Stiel.uint bottom, Stiel.uint top, next_format)

let switch field cases =
  Switch (Stiel.int_var field, cases)




let transitions_switch format_name field cases =
  { packet_format = format_name;
    transitions = transform_input_transitions (switch field cases); }


let transitions format_name transitions =
  { packet_format = format_name;
    transitions = transform_input_transitions transitions; }
    
let empty_transitions format_name =
  transitions format_name No_transition
  
