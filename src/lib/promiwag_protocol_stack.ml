open Promiwag_std

module Stiel_types = Promiwag_stiel
module Stiel = Promiwag_stiel.Construct_legacy
module MP_format = Promiwag_meta_packet.Packet_structure
module Partial_eval = Promiwag_stiel.Transform.Partial_evaluation

type packet_value =  Promiwag_meta_packet.Parser_generator.request
(*  [ 
| `value of string
| `pointer of string
| `offset of string
| `size of string
] *)

let stiel_var = function
  | `value   s -> sprintf "<v:%s" s 
  | `pointer s -> sprintf "<p:%s" s 
  | `offset  s -> sprintf "<o:%s" s 
  | `size    s -> sprintf "<s:%s" s 
let packet_value_of_string s =
  let pre = Str.sub s 0 3 in
  let suf = Str.sub s 3 (Str.length s - 3) in
  match pre with
  | "<v:" -> `value   suf
  | "<p:" -> `pointer suf
  | "<o:" -> `offset  suf
  | "<s:" -> `size    suf
  | _ -> failwith "packet_value_of_string: Wrong prefix"


type atomic_transition = {
  condition: Stiel_types.bool_expression;
  next_format: string;
  passed_expressions: packet_value list;
  next_payload: string;
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
  | Case_int_value of Stiel_types.int_expression * string * string
  | Case_int_range of
      Stiel_types.int_expression * Stiel_types.int_expression * string * string

type input_transition =
  | Switch of string * input_case list
  | Atomic of atomic_transition
  | No_transition

let empty_protcol_stack () =
  {stack_description = Ht.create 42; }
 

let atomic_transition
    ?(condition=Stiel_types.Bool_expr_false)
    ?(passed_expressions=[])
    ~next_payload next_format =
  {condition = condition; next_format = next_format; 
   passed_expressions = passed_expressions; 
   next_payload = next_payload;}

let transform_input_next (name, ofs) =
  (name, fun e -> Stiel.offset e ofs)

let transform_input_case field = 
    let var_field = stiel_var (`value field) in
    function
  | Case_int_value (ie, next_format, next_payload) -> 
    let condition = Stiel.eq (Stiel.int_var var_field) ie in
    atomic_transition ~condition ~next_payload next_format
  | Case_int_range (iea, ieb, next_format, next_payload) -> 
    let condition =
      Stiel.bool 
        (`And (`E (Stiel.le iea (Stiel.int_var var_field)),
               `E (Stiel.le (Stiel.int_var var_field) ieb))) in
    atomic_transition ~condition 
      ~passed_expressions:[`value field] 
      ~next_payload next_format

let transform_input_transitions = function
  | No_transition -> []
  | Switch (field, cases) ->
    Ls.map cases ~f:(transform_input_case field)
  | Atomic a -> [a]

let case_int_value int_value next_format next_payload =
  Case_int_value (Stiel.uint int_value, next_format, next_payload)

let case_int_range bottom top next_format next_payload =
  Case_int_range (Stiel.uint bottom, Stiel.uint top, next_format, next_payload)

let switch field cases =
  Switch (field, cases)

let empty_transition =
  No_transition

let atomic ?(condition=Stiel_types.Bool_expr_false)
    ?(passed_expressions=[])
    next_format next_payload =
  Atomic (atomic_transition ~condition 
            ~passed_expressions ~next_payload next_format)



let add_protocol ps name format packet_transitions =
  Ht.add ps.stack_description name {
    name = name;
    format = format;
    transitions = transform_input_transitions packet_transitions;
  };
  ()
    
let get_protocol ps name =
  Ht.find_opt ps.stack_description name
  
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
        (Str.concat sep (Ls.map (function
          | `value f -> sprintf "field %s" f
          | `pointer f -> sprintf "pointer %s" f
          | `offset f -> sprintf "offset of %s" f
          | `size f -> sprintf "size of %s" f) at.passed_expressions))
        suf
    in 
    sprintf "%sWhen [%s]\n%s-> Next Is \"%s\"\n%sAt Offset Of \"%s\"\n%sPassing: [%s]"
      indent_str (Stiel2S.bool_expression at.condition) more_indent
      at.next_format more_indent 
      at.next_payload more_indent passed_expressions
  
      
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


module Automata_generator = struct

  module Packet_parsing = Promiwag_meta_packet.Parser_generator

  type passed_expressions = Stiel_types.typed_expression list

  type protocol_handler = {
    handled_format: string;
    user_request: Packet_parsing.request list;
    make_handler: 
      passed_expressions * passed_expressions -> Stiel_types.statement;
  }

  type error =
    [ `buffer_over_flow of string 
    | `unknown of string ]

  type error_handler = error -> Stiel_types.statement

  let default_error_handler = function
    | `buffer_over_flow s -> Stiel.log (sprintf "Error BOF: %s" s) []
    | `unknown s -> Stiel.log (sprintf "Error unknown: %s" s) []

  type protocol_stack_handler = {
    protocol_handlers: protocol_handler list;
    error_handler: error -> Stiel_types.statement;
    continue_expression: Stiel_types.bool_expression;
    initial_protocol: string;
  }

  let handler 
      ?(error_handler=default_error_handler)
      ?(continue_expression=Stiel.t)
      ~initial_protocol handler_list =
    {error_handler          = error_handler;
     continue_expression = continue_expression;
     initial_protocol       = initial_protocol;   
     protocol_handlers      = 
        Ls.map handler_list ~f:(fun (a, b, c) ->
          {handled_format = a; user_request = b; make_handler = c;}
        );}

  type packet_expression = {
    stiel_pointer: Stiel_types.buffer_expression;
    stiel_size: Stiel_types.int_expression option;
  }

  let packet ?size expr = 
    {stiel_pointer = expr; stiel_size = size}

  let typed_pointer p = Stiel.expr_pointer p.stiel_pointer
  let typed_size p = Opt.map Stiel.expr_unat p.stiel_size

(*


generate (just for handlers and their dependencies):

  while (current_format != NONE && <continue_expression>) {
    current_format = next_format;
    current_payload = offsetize(previous_payload);
    current_packet_size = previous - offset;
    if (current_format == FORMAT_INT) {

       informed_block (payload rightly offseted)
       next_format = COMPUTED
    }

    if (current_format ...) {

    }  

  }

*)

  type compiler = {
    protocol_stack: protocol_stack;
    handler: protocol_stack_handler;
    todo_queue: protocol_handler FIFO.t;
    var_current_packet     : Stiel_types.typed_variable;
    var_current_packet_size: Stiel_types.typed_variable option;
    var_current_format     : Stiel_types.typed_variable;
    var_next_format        : Stiel_types.typed_variable;
    format_values_ht: (string, Stiel_types.int_expression) Ht.t;
    compiled_handlers: (string, Stiel_types.statement) Ht.t;
  }

  let int_expression_for_format compiler format =
    match Ht.find_opt compiler.format_values_ht format with
    | Some ie -> ie
    | None ->
      let ie = Stiel.uint (Unique.int ()) in
      Ht.add  compiler.format_values_ht format ie;
      ie

  let get_out_value compiler =
    int_expression_for_format compiler ""

  let get_out_statement compiler =
    Stiel.assign compiler.var_next_format
      (Stiel.expr_unat (get_out_value compiler))


  let compile_transition compiler cond_req_exprs ofs_req_exprs
      req_passed_exprs transition =

(*
  condition: Stiel_types.bool_expression;
  next_format: string;
  passed_expressions: packet_value list;
  next_payload: string;
 *)
    let _, offset = 
      Ls.find ofs_req_exprs 
        ~f:(fun (r, t) -> r = `offset transition.next_payload) in


    let actual_condition = 
      let variables = 
        Environment.of_list
          (Ls.map cond_req_exprs ~f:(fun (r, te) -> (stiel_var r, te))) in
      let env =
        Partial_eval.environment
          ~do_symbolic_equality:true ~use_purity:true ~variables () in
      Partial_eval.bool_expression env transition.condition in
    let statement_then =
      Stiel.block [
        Stiel.cmt (sprintf "Next protocol is %s" transition.next_format);
        Stiel.assign compiler.var_next_format
          (Stiel.expr_unat 
             (int_expression_for_format compiler transition.next_format));
        Stiel.assign compiler.var_current_packet
          (Stiel.expr_pointer 
             (Stiel.offset (Stiel.buffer_expr (Stiel.expr_var 
                                                 compiler.var_current_packet))
                (Stiel.int_expr offset)));
        (Opt.map_default
           (fun varsize ->
             Stiel.assign varsize
               (Stiel.expr_unat (Stiel.add  
                                   (Stiel.int_expr (Stiel.expr_var varsize))
                                   (Stiel.int_expr offset))))
           (Stiel.cmt "No packet size to update")
           compiler.var_current_packet_size);

      ] in
    Stiel.conditional actual_condition
      ~statement_then



  let find_handler compiler name =
    let default_handler format =
      { handled_format = format;
        user_request = [];
        make_handler= fun (_, _) ->
          let block = [
            Stiel.cmt (sprintf "Protocol %s is not handled by user" format);
          ] in
          (Stiel.block block); } in
    let handlers = compiler.handler.protocol_handlers in
    match 
      Ls.find_opt handlers ~f:(fun h -> h.handled_format = name)
    with
    | None -> default_handler name
    | Some h -> h

  let compile_protocol_handler compiler protocol protocol_handler =
    let format = protocol_handler.handled_format in
    let statements =
      let packet_format = protocol.format in
      let transitions = protocol.transitions in
      let transition_conditions_request =
        let module V =  Promiwag_stiel.Visit in
        let l = ref [] in
        let get_variables_compiler =
          V.compiler
            ~on_variables:(fun s ->
              l := (packet_value_of_string s) :: !l) () in
        Ls.iter transitions ~f:(fun t ->
          V.bool_expression get_variables_compiler t.condition);
        (Ls.unique !l) in
      let transition_passed_request =
        Ls.flatten (Ls.map transitions ~f:(fun t -> t.passed_expressions)) in
      let transition_offset_request =
        Ls.unique (Ls.map transitions ~f:(fun t -> `offset t.next_payload)) in
      Ls.iter transitions ~f:(fun t ->
        FIFO.push compiler.todo_queue 
          (find_handler compiler t.next_format));

      let request =
        transition_offset_request @ transition_passed_request 
        @ transition_conditions_request @ protocol_handler.user_request in
      let stage_1 =
        Packet_parsing.Stage_1.compile_with_dependencies
          ~packet_format request in
      let packet = Stiel.expr_var compiler.var_current_packet in
      let packet_size =
        Opt.map Stiel.expr_var compiler.var_current_packet_size in
      let make_user_block l =
        let from_next_offset, the_rest =
          Ls.split_nth (Ls.length transition_offset_request)  l in
        let from_the_passed_expressions, the_rest =
          Ls.split_nth (Ls.length transition_passed_request) the_rest in
        let from_the_transition_conditions, from_the_user_request =
          Ls.split_nth (Ls.length transition_conditions_request)  the_rest in
        [ (Stiel.cmt "Parsing User Block TODO") ]
        (* :: (Ls.map from_the_transition_conditions *)
        (*       ~f:(fun te -> *)
        (*         Stiel.log "Receiving @hex from transition conditions" [te];)) *)
        @ [ protocol_handler.make_handler ([] (* TODO ! *), 
                                           from_the_user_request)]
        @ [ Stiel.cmt "Default behaviour is to stop and get out of the While:";
            get_out_statement compiler; ]
        @ (Ls.map transitions 
             ~f:(compile_transition compiler 
                   (Ls.combine transition_conditions_request
                      from_the_transition_conditions)
                   (Ls.combine transition_offset_request from_next_offset)
                   (Ls.combine transition_passed_request
                      from_the_passed_expressions)))
      in
      (Stiel.cmt (sprintf "Protocol %s:" format))
      :: (Packet_parsing.Stage_2_stiel.informed_block
            ~create_variables:`for_all
            ~stage_1 ~packet ?packet_size ~make_user_block ())
    (* @ [ Stiel.cmt "Prepare the following... TODO" ] *)
    in
    Ht.add compiler.compiled_handlers format (Stiel.block statements);
    ()

  let try_compile_handler compiler handler =
    let format = handler.handled_format in
    match Ht.find_opt compiler.compiled_handlers format with
      | None ->
        begin match get_protocol compiler.protocol_stack format with
        | None -> 
          Ht.add compiler.compiled_handlers format (Stiel.block [
            Stiel.cmt (sprintf "Protocol %s not known" format);
            get_out_statement compiler;
          ]);
        | Some protocol -> 
          compile_protocol_handler compiler protocol handler
        end
      | Some _ -> ()

  let automata_block protocol_stack handler packet_expression =

    let compiler = 
      {todo_queue = FIFO.of_list handler.protocol_handlers;
       protocol_stack = protocol_stack;
       handler = handler;
       var_current_packet = Stiel.var_pointer "current_packet_pointer";
       var_current_packet_size =
          Opt.map (fun _ -> Stiel.var_unat "current_packet_size")
            (typed_size packet_expression);
       var_current_format = Stiel.var_unat "current_packet_format";
       var_next_format =    Stiel.var_unat "next_packet_format";
       format_values_ht = Ht.create 42;
       compiled_handlers = Ht.create 42;
      } in

    FIFO.consume compiler.todo_queue ~f:(try_compile_handler compiler);

    let get_out_value = get_out_value compiler in

    let before_the_while =
      (Stiel.declare_and_assign compiler.var_current_packet
         (typed_pointer packet_expression))
      @ (Opt.map_default
           (fun v ->
             Stiel.declare_and_assign
               (Opt.get compiler.var_current_packet_size) v)
           [] (typed_size packet_expression))
      @ [ Stiel.declare compiler.var_current_format;
          Stiel.assign compiler.var_current_format 
            (Stiel.expr_unat
               (int_expression_for_format compiler handler.initial_protocol));
          Stiel.declare compiler.var_next_format; ]
    in
    let while_condition =
      Stiel.bool (`And (`Neq
                           (`E (Stiel.int_expr
                                  (Stiel.expr_var compiler.var_current_format)),
                            `E get_out_value),
                        `E handler.continue_expression)) in
    let while_block =
      (* TODO one day: arrange them (optionaly) as a binary decision tree *)
      let r = ref [] in
      let cur_fmt_var_ie =
        Stiel.int_expr (Stiel.expr_var compiler.var_current_format) in
      Ht.iter (fun format statement_then ->
        let format_ie = int_expression_for_format compiler format in
        let st =
          Stiel.conditional
            (Stiel.bool (`Eq (`E cur_fmt_var_ie, `E format_ie)))
            ~statement_then in
        r := st :: !r;
      ) compiler.compiled_handlers;
      (Ls.rev !r) @ [
        Stiel.assign compiler.var_current_format
          (Stiel.expr_var compiler.var_next_format);
      ]
    in
    before_the_while
    @ [Stiel.while_loop while_condition (Stiel.block while_block)]
      

end


