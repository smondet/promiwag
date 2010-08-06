open Promiwag_std

module MP_format = Promiwag_meta_packet.Packet_structure

type switch_case =
  | Case_int_value of int * string * string
  | Case_int_range of int * int * string * string

type transition =
  | Switch of string * switch_case list
  | Sequence of transition list
  | No_transition


type protocol = {
  name: string;
  format: MP_format.format;
  transitions: transition;
}

type protocol_stack = {
  (* packet_database: MPDB.t; *)
  stack_description: (string, protocol) Ht.t;
}

let empty_protcol_stack () =
  {stack_description = Ht.create 42; }


let case_int_value int_value next_format next_payload =
  Case_int_value (int_value, next_format, next_payload)

let case_int_range bottom top next_format next_payload =
  Case_int_range (bottom, top, next_format, next_payload)

let switch field cases =
  Switch (field, cases)

let empty_transition =
  No_transition

let sequence l = Sequence l
  
let add_protocol ps name format packet_transitions =
  Ht.add ps.stack_description name {
    name = name;
    format = format;
    transitions = packet_transitions;
  };
  ()
    
let get_protocol ps name =
  Ht.find_opt ps.stack_description name
  
module To_string = struct

  module Stiel2S = Promiwag_stiel.To_string
  module Packet2S = Promiwag_meta_packet.To_string

  let make_indent i = Str.make (2 * i) ' '

  let switch_case = function
    | Case_int_value (v, format, payload) ->
      sprintf "| %d -> format \"%s\" at \"%s\"" v format payload
    | Case_int_range (a, b, format, payload) ->
      sprintf "| [%d .. %d] -> format \"%s\" at \"%s\"" a b format payload

  let rec transition indent =
    let sindent = make_indent indent in
    let nindent = make_indent (indent + 1) in
    function
      | Switch (field, scl) -> 
        sprintf "\n%sswitch (value of \"%s\") {\n%s%s\n%s}" sindent field
          nindent
          (Str.concat (sprintf "\n%s" nindent) (Ls.map scl ~f:switch_case))
          sindent
      | Sequence l ->
        sprintf "\n%ssequence {%s\n%s}" 
          sindent (Str.concat "," (Ls.map (transition (indent + 1)) l)) sindent
      | No_transition -> "None"


      
  let protocol ?(indent=0) p =
    let indent_str = make_indent indent in
    let next_indent = make_indent (indent + 1) in
    let next_next_indent = make_indent (indent + 2) in
    sprintf "%sProtocol \"%s\":\n%sFormat: {\n%s%s%s}\n%sTransitions ==> %s"
      indent_str p.name next_indent
      next_next_indent
      (Packet2S.format ~sep:next_next_indent ~suffix:";\n" p.format) 
      next_indent next_indent
      (transition (indent + 2) p.transitions)
      
      
  let protocol_stack ps =
    let r = ref [] in
    Ht.iter (fun name p ->
      r := (protocol ~indent:1 p) :: !r;
    ) ps.stack_description;
    sprintf "Protocol Stack: {\n%s\n}" (Str.concat "\n" (Ls.rev !r))

end


module Automata_generator = struct

  module Partial_eval = Promiwag_stiel.Transform.Partial_evaluation
  open Promiwag_stiel.Standard_renaming

  module Packet_parsing = Promiwag_meta_packet.Parser_generator

  type packet_value =  Promiwag_meta_packet.Packet_structure.packet_value

  type protocol_handler = {
    handled_format: string;
    user_request: packet_value list;
    make_handler:  STIEL.typed_expression list -> STIEL.statement;
  }

  type error =
    [ `buffer_over_flow of
        string * STIEL.typed_expression * STIEL.typed_expression
    | `unknown of string ]

  type error_handler = error -> STIEL.statement

  let default_error_handler = function
    | `buffer_over_flow (f, a, b) ->
      Do.log (sprintf "Error BOF: fmt: %s, @int < @int\n" f) [a; b]
    | `unknown s -> Do.log (sprintf "Error unknown: %s\n" s) []

  type protocol_stack_handler = {
    protocol_handlers: protocol_handler list;
    error_handler: error -> STIEL.statement;
    continue_expression: STIEL.typed_expression;
    initial_protocol: string;
  }

  let handler 
      ?(error_handler=default_error_handler)
      ?(continue_expression=Expr.t)
      ~initial_protocol handler_list =
    {error_handler          = error_handler;
     continue_expression = continue_expression;
     initial_protocol       = initial_protocol;   
     protocol_handlers      = 
        Ls.map handler_list ~f:(fun (a, b, c) ->
          {handled_format = a; user_request = b; make_handler = c;}
        );}

  type packet_expression = {
    stiel_pointer: STIEL.typed_expression;
    stiel_size: STIEL.typed_expression option;
  }

  let packet ?size expr = 
    {stiel_pointer = expr; stiel_size = size}

  let typed_pointer p = p.stiel_pointer
  let typed_size p =    p.stiel_size

  type compiler = {
    protocol_stack: protocol_stack;
    handler: protocol_stack_handler;
    todo_queue: protocol_handler FIFO.t;
    var_current_packet     : STIEL.typed_variable;
    var_current_packet_size: STIEL.typed_variable option;
    var_current_format     : STIEL.typed_variable;
    var_next_format        : STIEL.typed_variable;
    format_values_ht: (string, STIEL.typed_expression) Ht.t;
    compiled_handlers: (string, STIEL.statement) Ht.t;
  }

  let int_expression_for_format compiler format =
    match Ht.find_opt compiler.format_values_ht format with
    | Some ie -> ie
    | None ->
      let ie = Expr.unat (Unique.int ()) in
      Ht.add  compiler.format_values_ht format ie;
      ie

  let get_out_value compiler =
    int_expression_for_format compiler ""

  let get_out_statement compiler =
    match compiler.var_current_packet_size with
    | Some vps ->
      Annot.why 
        (Do.block [
          Var.assign vps (Expr.sub (Var.expression vps) (Expr.unat 1));
          Do.exit_while ])
        Do.exit_while
    | None ->
      Do.exit_while

  let assign_next_to_out compiler =
    Var.assign compiler.var_next_format (get_out_value compiler)

  type atomic_transition = {
    condition: STIEL.typed_expression;
    next_format: string;
    next_payload: string;
  }
  
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

  let atomic_transition ?(condition=Expr.f) ~next_payload next_format =
    {condition = condition; next_format = next_format; 
     next_payload = next_payload;}

  let transform_input_case field = 
    let var_field = Var.unat ~unique:false (stiel_var (`value field)) in
    function
      | Case_int_value (ie, next_format, next_payload) -> 
        let condition = Expr.eq (Var.expression var_field) (Expr.unat ie) in
        atomic_transition ~condition ~next_payload next_format
      | Case_int_range (iea, ieb, next_format, next_payload) -> 
        let condition =
          Expr.band
            (Expr.le (Expr.unat iea) (Var.expression var_field))
            (Expr.le (Var.expression var_field) (Expr.unat ieb)) in
        atomic_transition ~condition ~next_payload next_format

  let rec transform_transitions = function
    | No_transition -> []
    | Switch (field, cases) ->
      Ls.map cases ~f:(transform_input_case field)
    | Sequence l ->
      Ls.flatten (Ls.map transform_transitions l)



  let compile_transition compiler cond_req_exprs ofs_req_exprs transition =

(*
  condition: STIEL.bool_expression;
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
      Partial_eval.typed_expression env transition.condition in
    let statement_then =
      Do.block [
        Do.cmt (sprintf "Next protocol is %s" transition.next_format);
        Var.assign compiler.var_next_format
          (int_expression_for_format compiler transition.next_format);
        Var.assign compiler.var_current_packet
          (Expr.offset (Var.expression compiler.var_current_packet) offset);
        (Opt.map_default
           (fun varsize ->
             Var.assign varsize (Expr.sub (Var.expression varsize) offset))
           (Do.cmt "No packet size to update")
           compiler.var_current_packet_size);

      ] in
    Do.conditional actual_condition ~statement_then



  let find_handler compiler name =
    let default_handler format =
      { handled_format = format;
        user_request = [];
        make_handler= fun _ ->
          let block = [
            Do.cmt (sprintf "Protocol %s is not handled by user" format);
          ] in
          (Do.block block); } in
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
      let transitions = transform_transitions protocol.transitions in
      let transition_conditions_request =
        let module Visit =  Promiwag_stiel.Visit in
        let l = ref [] in
        let get_variables_compiler =
          Visit.compiler
            ~on_variables:(fun s ->
              l := (packet_value_of_string s) :: !l) () in
        Ls.iter transitions ~f:(fun t ->
          Visit.typed_expression get_variables_compiler t.condition);
        (Ls.unique !l) in
      let transition_offset_request =
        Ls.unique (Ls.map transitions ~f:(fun t -> `offset t.next_payload)) in
      Ls.iter transitions ~f:(fun t ->
        FIFO.push compiler.todo_queue 
          (find_handler compiler t.next_format));

      let request =
        transition_offset_request
        @ transition_conditions_request @ protocol_handler.user_request in
      let stage_1 =
        Packet_parsing.Stage_1.compile_with_dependencies
          ~packet_format request in
      let packet = Var.expression compiler.var_current_packet in
      let packet_size =
        Opt.map Var.expression compiler.var_current_packet_size in
      let on_size_error tea teb =
        Do.block [
          compiler.handler.error_handler (`buffer_over_flow (format, tea, teb));
          get_out_statement compiler;
        ] in
      let make_user_block l =
        let from_next_offset, the_rest =
          Ls.split_nth (Ls.length transition_offset_request)  l in
        let from_the_transition_conditions, from_the_user_request =
          Ls.split_nth (Ls.length transition_conditions_request)  the_rest in
        [ Annot.cmt "Parsing User Block:"
            (Annot.why (Do.cmt "Removed for Why.")
               (protocol_handler.make_handler from_the_user_request)) ]
        @ [ Annot.cmt "Default behaviour is to stop and get out of the While:"
              (assign_next_to_out compiler); ]
        @ (Ls.map transitions 
             ~f:(compile_transition compiler 
                   (Ls.combine transition_conditions_request
                      from_the_transition_conditions)
                   (Ls.combine transition_offset_request from_next_offset)))
      in
      (Do.cmt (sprintf "Protocol %s:" format))
      :: (Packet_parsing.Stage_2_stiel.informed_block
            ~on_size_error
            ~create_variables:`for_all
            ~stage_1 ~packet ?packet_size ~make_user_block ())
    in
    Ht.add compiler.compiled_handlers format (Do.block statements);
    ()

  let try_compile_handler compiler handler =
    let format = handler.handled_format in
    match Ht.find_opt compiler.compiled_handlers format with
      | None ->
        begin match get_protocol compiler.protocol_stack format with
        | None -> 
          Ht.add compiler.compiled_handlers format (Do.block [
            Annot.cmt (sprintf "Protocol %s not known" format)
              (get_out_statement compiler);
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
       var_current_packet = Var.pointer "current_packet_pointer";
       var_current_packet_size =
          Opt.map (fun _ -> Var.unat "current_packet_size")
            (typed_size packet_expression);
       var_current_format = Var.unat "current_packet_format";
       var_next_format =    Var.unat "next_packet_format";
       format_values_ht = Ht.create 42;
       compiled_handlers = Ht.create 42;
      } in

    FIFO.consume compiler.todo_queue ~f:(try_compile_handler compiler);

    let before_the_while =
      (Var.declare_and_assign compiler.var_current_packet
         (typed_pointer packet_expression))
      @ (Opt.map_default
           (fun v ->
             Ls.map (Annot.why (Promiwag_stiel.To_why_string.mark_packet_length))
               (Var.declare_and_assign
                  (Opt.get compiler.var_current_packet_size) v))
           [] (typed_size packet_expression))
      @ [ Var.declare compiler.var_current_format;
          Var.assign compiler.var_current_format 
            (int_expression_for_format compiler handler.initial_protocol);
          Var.declare compiler.var_next_format; ]
    in
    let while_condition =
      Expr.band
        (Expr.neq
           (Var.expression compiler.var_current_format) (get_out_value compiler))
        handler.continue_expression in
    let while_contents, format_values =
      (* TODO one day: arrange them (optionaly) as a binary decision tree *)
      let r = ref [] in
      let format_values = ref [] in
      let cur_fmt_var_ie = Var.expression compiler.var_current_format in
      Ht.iter (fun format statement_then ->
        let format_ie = int_expression_for_format compiler format in
        format_values := format_ie :: !format_values;
        let st =
          Do.conditional (Expr.eq cur_fmt_var_ie format_ie) ~statement_then in
        r := st :: !r;
      ) compiler.compiled_handlers;
      ((Ls.rev !r) @ [
        Var.assign compiler.var_current_format
          (Var.expression compiler.var_next_format);],
       !format_values)
    in
    let while_block =
      match compiler.var_current_packet_size with
      | None -> Do.block while_contents
      | Some vps ->
        Annot.variability
          (Expr.band
             (Expr.ge (Var.expression vps) (Expr.unat 0)) 
             (Expr.ls_or 
                (Ls.map format_values
                   ~f:(Expr.eq (Var.expression compiler.var_current_format)))))
          (Var.expression vps)
          (Do.block 
             (while_contents @
                [ Annot.why
                    (Do.conditional 
                       (Expr.eq (Var.expression compiler.var_current_format)
                          (get_out_value compiler))
                       ~statement_then:(get_out_statement compiler))
                    Do.nop
                ]))
           in
    before_the_while
    @ [Do.while_loop while_condition while_block]
      

end


