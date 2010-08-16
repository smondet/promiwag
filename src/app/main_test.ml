
open Promiwag_std


module Normal_string = struct
  include String
  let str s = s
  let cat = concat ""
  let new_line () = "\n"
end
module C2S = Promiwag.C_backend.To_big_string(Normal_string)

module String_tree = struct
  type t = 
    | Str of string
    | Cat of t list
        
  let str s = Str s
  let cat l = Cat l
  let new_line () = Str "\n"
    
  let rec print ?(out=Io.stdout) = function
    | Str s -> Io.nwrite out s
    | Cat l -> Ls.iter (print ~out) l

end
module C2StrTree = Promiwag.C_backend.To_big_string(String_tree)


module System = struct

  (** create a directory but doesn't raise an exception if the
      directory * already exist *)
  let mkdir_safe dir perm =
    try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

  (** create a directory, and create parent if doesn't exist
      i.e. mkdir -p *)
  let mkdir_p ?(perm=0o700) dir =
    let rec p_mkdir dir =
      let p_name = Filename.dirname dir in
      if p_name <> "/" && p_name <> "."
      then p_mkdir p_name;
      mkdir_safe dir perm in
    p_mkdir dir 

  exception Command_error of string * Unix.process_status
  let run_command c =
    match Unix.system c with
    | Unix.WEXITED 0 -> ()
    | err -> raise (Command_error (c, err))


  let read_command_output f s =
    let ic = Unix.open_process_in s in
    (try
       while true do
         f (input_char ic)
       done
     with End_of_file -> ());
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> ()
    | e -> raise (Command_error (s, e))

  let slurp_command s =
    let buf = Buffer.create 100 in
    read_command_output (Buffer.add_char buf) s;
    Buffer.contents buf


  let timestamp () =
    let module U = Unix in
    let t = U.time () in
    let st = U.localtime t in
    let sec   = st.U.tm_sec in 
    let min   = st.U.tm_min in 
    let hour  = st.U.tm_hour in
    let mday  = st.U.tm_mday in
    let mon   = st.U.tm_mon + 1 in 
    let year  = st.U.tm_year + 1900 in
    sprintf "%d_%02d_%02d_%02dh%02dm%02ds"
      year mon mday hour min sec

  let global_tmp_dir = ref "./tmp_promiwag/"
  let tmp s = 
    mkdir_p !global_tmp_dir;
    !global_tmp_dir ^ s

  module Feed = struct
  (* Warning: the following function crashes in OCaml 3.09.2,
     because of that bug: http://caml.inria.fr/mantis/view.php?id=4062
     (close_out is applied a second time during Unix.close_process) *)
    let kfeed f command data =
      let (ic, oc) as channels = Unix.open_process command in
      output_string oc data;
      close_out oc;
      let exn = ref None in
      begin 
        try
          while true do
            f (input_char ic)
          done
        with
        | End_of_file -> ()
        | e -> exn := Some e
      end;
      begin match Unix.close_process channels with
      | Unix.WEXITED 0 -> ()
      | _ -> invalid_arg ("feed_command: " ^ command)
      end;
      (match !exn with Some e -> raise e | None -> ())

    let feed = kfeed print_char
      
    let ffeed oc command data = kfeed (output_char oc) command data
      
    let bfeed buf command data = kfeed (Buffer.add_char buf) command data
      
    let str command data = 
      let buf = Buffer.create (2 * String.length data) in
      bfeed buf command data;
      Buffer.contents buf
  end

end

module Test_protocol_stacks = struct

  open Promiwag.Meta_packet.Packet_structure
  module Protocol_stack = Promiwag_protocol_stack
  open  Promiwag.Protocol_stack

  let req_max_field format =
    Ls.flatten $ Ls.map format ~f:(function
    | Item_field (name, Type_unsigned_integer size)
    | Item_field (name, Type_signed_integer size)
    | Item_field (name, Type_little_endian (Type_unsigned_integer size))
    | Item_field (name, Type_little_endian (Type_signed_integer size)) ->
      [ `value   name; `size    name; ]
    | Item_field ("unsized_payload" as name, Type_string size) ->
      [ `pointer name; `offset  name;]
    | Item_field (name, Type_string size) ->
      [ `pointer name; `offset  name; `size    name; ]
    | _-> failwith "req_max_field")

  let a_name = "complex_right_a"
  let b_name = "simple_right_b"
  let c_name = "complex_right_c"

  let a_format =
    packet_format [
      field "field_00" (fixed_string 8);
      field "field_01" (fixed_string 6);
      field "field_02" (fixed_string 6); (* 20 bytes *)
      field "field_byte" (fixed_int 8);
      field "field_04" (fixed_int 3);
      field "field_05" (fixed_int 4);
      field "field_06" (fixed_int 1);
      field "field_07" (fixed_int 16); (* 32 bits *)
      field "field_08" (fixed_int 16);
      field "field_09" (fixed_int 3);
      field "field_10" (fixed_int 12);
      field "field_11" (fixed_int 1); (* 32 bits, 28 bytes whole *)
      string_field "field_12" (size (`var "field_byte"));
      fixed_int_field "field_13" 16;
      string_field "like_ip_options" 
        (size
           (`align32 (`sub (`mul (`var "field_byte", `int 4),
                            `offset "field_13"))));
      fixed_int_field "after_like_ip_options" 16;
      payload 
        ~size:(size (`sub (`var "field_byte",
                           `add (`offset "field_13", `int 2))))
        ~name:"a_payload"
      ();

    ]
  let a_transitions =
    sequence [
      switch "field_09" [
        case_int_value 3  b_name "a_payload";
        case_int_value 2  c_name "a_payload";
      ];
      switch "field_11" [
        case_int_value 3  b_name "like_ip_options";
      ];
    ]

  let b_format =
    packet_format [
      field "bone" (fixed_int 15);
    ]
  let b_transitions = empty_transition
  let b_transitions_wrong =
    switch "bone" [
      case_int_value 0 a_name "bone" (* potential infinite loop *)
    ]

  let c_format =
    packet_format [
      field "a" (fixed_int 8);
      string_field "aa" (size (`int 2));
      field "b" (fixed_int 24);
      string_field "s" (size (`sub (`int 10, `var "b")));
      payload ~name:"unsized_payload" ();
    ]
  let c_format_does_not_compile =
    packet_format [
      field "a" (fixed_int 4); (* 4 <> 0 mod 8 *)
      string_field "aa" (size (`int 2));
      field "b" (fixed_int 24);
      string_field "s" (size (`sub (`int 10, `var "b")));
      payload ~name:"unsized_payload" ();
    ]
  let c_transitions = empty_transition
  let c_transitions_inifinite_loop =
    switch "b" [
      case_int_value 0 "another" "b";
      case_int_value 1 "another" "a"; (* infinite loop *)
    ]
  let c_transitions_unknown_size =
    switch "a" [
      case_int_value 0 "another" "b";
      case_int_value 1 "another" "unsized_payload"; (* offset not 'reachable',
                                                       10 - b may be negative. *)
    ]
  let c_checks_for_unknown_size = [
    check_range "b" 0 4
  ]

  let make_handled_stack l =
    let stack = Protocol_stack.empty_protcol_stack () in
    Ls.iter l ~f:(fun (n,format,transitions,runtime_checks) ->
      Protocol_stack.add_protocol stack ~format ~transitions ~runtime_checks n);
    let handlers =
      Ls.map l
        ~f:(fun (n,f,t,c) -> (n, req_max_field f, 
                            fun l -> 
                              (Promiwag.Stiel.Statement.nop, 
                               Promiwag.Stiel.Expression.t))) in
    (stack, (let (a, _, _, _) = Ls.hd l in a), handlers)

  let right_1 () =
    let good_ones = [
      a_name, a_format, a_transitions, [];      
      b_name, b_format, b_transitions, [];
      c_name, c_format, c_transitions, [];
    ] in
    make_handled_stack good_ones

  let wrong_1 () =
    make_handled_stack [
      a_name, a_format, a_transitions, [];      
      b_name, b_format, b_transitions_wrong, [];
      c_name, c_format, c_transitions, [];
    ]
  let wrong_2 () =
    make_handled_stack [
      a_name, a_format, a_transitions, [];      
      b_name, b_format, b_transitions, [];
      c_name, c_format, c_transitions_inifinite_loop, [];
    ]
  let wrong_3 () =
    make_handled_stack [
      a_name, a_format, a_transitions, [];      
      b_name, b_format, b_transitions, [];
      c_name, c_format, c_transitions_unknown_size, [];
    ]
  let right_2 () =
    make_handled_stack [
      a_name, a_format, a_transitions, [];      
      b_name, b_format, b_transitions, [];
      c_name, c_format, c_transitions_unknown_size, c_checks_for_unknown_size;
    ]
  let wrong_4 () =
    make_handled_stack [
      a_name, a_format, a_transitions, [];      
      b_name, b_format, b_transitions, [];
      c_name, c_format_does_not_compile, c_transitions, [];
    ]

  let make_why (stack, initial_protocol, handlers) =
    let module Stiel = Promiwag.Stiel in
    let module Expr = Stiel.Expression in
    let module Var = Stiel.Variable in
    let module Do = Stiel.Statement in
    let module Meta_stack = Promiwag.Protocol_stack in
    let module Generator = Meta_stack.Automata_generator in
    let automata_treatment packet_pointer packet_size =
      let error_handler = function _ -> Do.log "??" [] in
      let stack_handler =
        Generator.handler ~error_handler ~initial_protocol  handlers in
      let packet = Generator.packet ~size:packet_size packet_pointer in
      let automata_block =
        Do.block 
          ( (Do.log "===== New Packet =====\n" [])
            :: (Generator.automata_block stack stack_handler packet)) in
      automata_block in
    let why_checkable_program = 
      Promiwag.Stiel.To_why_string.statement_to_string 
        (automata_treatment
           (Var.expression (Var.pointer ~unique:false "packet_buffer_expression"))
           (Var.expression (Var.pointer ~unique:false "packet_buffer_length"))) in
    why_checkable_program


end


let call_printf format_str exp_list =
  `expression (`cast (`void, `call (`variable "printf",
                                    `literal_string format_str :: exp_list)))

let printf_of_typed_expression ?(prefix="") te =
  let fmt, init =
    match Promiwag.C_backend.Typed_expression.c_type te with
    | `unsigned_char      -> ("(dec: %hhu, hex: %02hhx)" , 2)    
    | `signed_char        -> ("(dec: %hhd, hex: %02hhx)" , 2)    
    | `unsigned_short     -> ("(dec: %hu,  hex: %04hx)"  , 2)   
    | `signed_short       -> ("(dec: %hd,  hex: %04hx)"  , 2)   
    | `unsigned_int       -> ("(dec: %u,   hex: %04x)"   , 2)   
    | `signed_int         -> ("(dec: %d,   hex: %04x)"   , 2)   
    | `unsigned_long      -> ("(dec: %lu,  hex: %08lx)"  , 2)   
    | `signed_long        -> ("(dec: %ld,  hex: %08lx)"  , 2)   
    | `unsigned_long_long -> ("(dec: %llu, hex: %08llx)" , 2)    
    | `signed_long_long   -> ("(dec: %lld, hex: %08llx)" , 2)    
    | `float              -> ("(dec: %f,   hex: %08llx)" , 2)    
    | `double             -> ("(dec: %f,   hex: %08llx)" , 2)    
    | `long_double        -> ("(dec: %f,   hex: %16llx)" , 2)    
    | `pointer _          -> ("(pointer    hex: %08lx)"  , 1)   
    | _ -> "not printable", 0
  in
  let expr =  Promiwag.C_backend.Typed_expression.expression te in
  let format_str =
    `literal_string (sprintf "%sTyped Expr %s is %s\n" prefix
                       (C2S.expression expr) fmt) in
  let arg_list = 
    Ls.init init (fun i -> expr) in
  `expression (`cast (`void, `call (`variable "printf", format_str :: arg_list)))

let printf_packet ?(prefix="packet") ?(suffix="\n") expr size =
  let fmt = Str.concat ":" (Ls.init size (fun i -> "%02hhx")) in
  let idx e i = `array_index (e, `literal_int i) in
  let arg = Ls.init size (idx expr) in
  call_printf (sprintf "%s[0..%d]: %s%s" prefix size fmt suffix) arg





let test_c_ast () =
  let module C_AST = Promiwag.C_backend.C_LightAST in
  printf "Start 'test_c_ast'\n";
  let main_signature = 
    (`signed_int, "main", ["argc", `signed_int; "argv",
                           `pointer (`pointer `signed_char)]) in
  let buffer_type: C_AST.c_type = 
    let buffer_size = 42 in
    `structure [
      "contents", `array ([`literal_int buffer_size], `unsigned_char);
      "size", `unsigned_long] in
  let main_def =
    let show_var s f =
      call_printf (sprintf "%s is %s\n" s f) [`variable s;] in
    let dowhile_or_while =
      (`binary (`bin_lt, `variable "i_var",`literal_int 50),
       `block ([], 
               [`assignment (`variable "i_var",
                              (`binary (`bin_add, 
                                        `variable "i_var",
                                        `literal_int 1)));
                show_var "i_var" "%d";
               ])) in
    (main_signature, ([
      `uninitialized ("i_var", `signed_int);
      `initialized ("buf", `pointer (`named_type "buffer"), `variable "NULL");
     ], [
      `expression (`cast (`void, `call (`variable "printf",
                                        [`literal_string "Hello World!\n"])));
      show_var "i_var" "%d";
      `assignment (`variable "i_var", `literal_int 42);
      show_var "i_var" "%d";
      show_var "buf" "%p";

      `while_loop dowhile_or_while;
      `assignment (`variable "i_var", `literal_int 48);
      `dowhile_loop dowhile_or_while;
      `simple_switch (`variable "argc", [
        `literal_int 0, ([], []);
        `literal_char 'B', ([], []);
        `default, ([], []);]);
      `conditional (`binary (`bin_eq, `variable "argc", `variable "i_var"),
                    `empty,
                    call_printf "In the else: %d\n" [`variable "i_var"]);
      `conditional (`binary (`bin_ne, `variable "argc", `variable "i_var"),
                    call_printf "In the then\n" [],
                    `empty);
      `return (`variable "i_var");
    ]))
  in
  let my_prog: C_AST.file = [
    `sharp_include "stdio.h";
    `type_definition ("buffer", buffer_type);
    `type_definition ("matrix_of_ints",
                      `array ([`empty;`literal_int 28;], `signed_int));
    `function_declaration (`local main_signature);
    `function_definition main_def;
  ] in
  
  printf "buffer_type: %s\n" (C2S.c_type buffer_type);

  printf "File:\n\n";
  String_tree.print (C2StrTree.file my_prog);
  
  Io.with_file_out "test.c" (fun out ->
    String_tree.print ~out (C2StrTree.file my_prog);
  );
  ()

let test_pcap_basic () =
  let module C = Promiwag.C_backend in
  let module Pcap = Promiwag.Pcap.C_legacy in
  printf "Start test_pcap_basic\n";
  let passed_structure =
    C.Variable.create ~name:"passed_structure"
      ~c_type:(`unsigned_long) ~initialisation:(`literal_int 1) () in
  let toplevels, (block_vars, block_sts) =
    let passed_expression =
      C.Variable.address_typed_expression passed_structure in
    let to_open = `device (`literal_string "eth0") in
    let on_error s e = 
      `block (C.Construct.block () ~statements:[
        call_printf "PCAP ERROR: %s: %s\n" [`literal_string s; e];
        `return (`literal_int 2);
      ]) in
    let packet_treatment ~passed_argument ~packet_length ~packet_buffer =
      let counter_mem_contents =
        `unary (`unary_memof, C.Variable.expression passed_argument) in
      C.Construct.block () ~statements:[
        call_printf "packet_treatment: arg:%d lgth: %d buf: %x\n"
          [counter_mem_contents;
           C.Variable.expression packet_length;
           C.Variable.expression packet_buffer;
          ];
        `assignment (counter_mem_contents,
                     `binary (`bin_add, `literal_int 1, counter_mem_contents));
      ]
    in
    Pcap.make_capture ~to_open ~on_error ~passed_expression ~packet_treatment in
  let main_pcap, _, _ = 
    C.Construct.standard_main 
      ((C.Variable.declaration passed_structure) :: block_vars, 
       block_sts @ [`return (`literal_int 0)]) in
  let test_pcap = 
    toplevels @ [ C.Function.definition main_pcap ] in
  String_tree.print (C2StrTree.file test_pcap);

  Io.with_file_out "pcaptest.c" (fun out ->
    String_tree.print ~out (C2StrTree.file test_pcap);
  );
  ()

let print_the_internet () =
  let the_internet =
    Promiwag.Standard_protocols.internet_stack_from_ethernet () in
  printf "The Internet:\n%s\n"
    (Promiwag.Protocol_stack.To_string.protocol_stack the_internet);
  ()

let make_clean_protocol_stack to_open =
  let module Stiel = Promiwag.Stiel in
  let module Expr = Stiel.Expression in
  let module Var = Stiel.Variable in
  let module Do = Stiel.Statement in
  let module Meta_stack = Promiwag.Protocol_stack in
  let module Standard_protocols = Promiwag.Standard_protocols in
  let module Generator = Meta_stack.Automata_generator in
  let the_internet =
    Standard_protocols.internet_stack_from_ethernet () in

  let my_log =
    Do.meta_log [
      ("@ethaddr", "@hex:@hex:@hex:@hex:@hex:@hex",
       (fun be ->
         [ Expr.u8_at (Expr.offset be (Expr.unat 0));
           Expr.u8_at (Expr.offset be (Expr.unat 1));
           Expr.u8_at (Expr.offset be (Expr.unat 2));
           Expr.u8_at (Expr.offset be (Expr.unat 3));
           Expr.u8_at (Expr.offset be (Expr.unat 4));
           Expr.u8_at (Expr.offset be (Expr.unat 5));]));
      ("@ipv4addr", "@int.@int.@int.@int",
       (fun ie ->
         let byte_ones = Expr.i64_to_u32 (Expr.ones 8) in
         [ Expr.bin_and byte_ones $ Expr.bin_shr ie (Expr.u32 24);
           Expr.bin_and byte_ones $ Expr.bin_shr ie (Expr.u32 16);
           Expr.bin_and byte_ones $ Expr.bin_shr ie (Expr.u32  8);
           Expr.bin_and byte_ones $ Expr.bin_shr ie (Expr.u32  0);]));
    ] in

  let automata_treatment packet_pointer packet_size =
    let error_handler = function
      | `buffer_over_flow (f, a, b) ->
        let message =
          sprintf "### \"BOF-attempt\": In %s, \n\
                \   computed offset (@expr = @int)\n\
                \   is bigger than\n\
                \   packet size (@expr = @int).\n" f in
        Do.log message [b; b; a; a]
      | `range_check (f, a, b, te) ->
        let message =
          sprintf "### \"Runtime check:\": \"%s\" (= @int) not in [%d, %d]\n"
            f a b in
        Do.log message [te]
      | `unknown s -> Do.log (sprintf "UNKNOWN ERROR:: %s\n" s) []
    in
    let stack_handler =
      Generator.handler
        ~error_handler
        ~initial_protocol:Standard_protocols.ethernet [
          ( Standard_protocols.ethernet,
            [ `pointer "dest_addr"; `pointer "src_addr"; ],
            function
              | [ pointer_dest; pointer_src ] ->
                (Do.block [
                  my_log "Ethernet: @ethaddr -> @ethaddr.\n" 
                    [pointer_src; pointer_dest;];
                ], Expr.t)
              | _ -> failwith "should have two typed expressions");
          ( Standard_protocols.arp,
            [ `value "htype"; `value "ptype"; `value "op"],
            function
              | [ htype; ptype; op ] ->
                (Do.block $ Ls.flatten [
                  [my_log "  ARP: htype = '" [] ];
                  Do.switch_int htype 
                    (Ls.map Standard_protocols.string_arp_htype
                       ~f:(fun (i, s) -> (i, my_log s [])));
                  [my_log "' (@int), ptype = @hex, op = '" [ptype; htype]]; 
                  Do.switch_int op 
                    (Ls.map Standard_protocols.string_arp_op
                       ~f:(fun (i, s) -> (i, my_log s [])));
                  [my_log "' (@int).\n" [op]];
                ], Expr.t)
              | _ -> failwith "should have three typed expressions");
          ( Standard_protocols.gre,
            [ `value "checksum_present"; `value "version"; `value "protocol"; ],
            fun te_list ->
              (my_log "  GRE: checksum_present: @int, \
                     \  version: @int, protocol: @hex.\n" te_list, Expr.t));
          ( Promiwag_standard_protocols.ipv4,
            [ `value "src"; `value "dest"; `value "protocol";`value "ttl";
              `value "can_fragment"; `value "frag_offset"; 
              `size "options"; `value "length";],
            fun te_list ->
              (my_log "  IPv4: @ipv4addr -> @ipv4addr\n\
                     \  protocol: @hex, TTL: @int, \
                     fragment: (can: @int, offset: @int)\n\
                     \  size of options: @int, length: @int.\n"
                te_list, Expr.t));
          ( Promiwag_standard_protocols.udp,
            [ `value "src_port"; `value "dst_port"; `size "udp_payload"; ],
            fun te_list ->
              (my_log "    UDP: @int -> @int\n\
                     \    payload size: @int.\n" te_list, Expr.t));
          ( Promiwag_standard_protocols.tcp,
            [ `value "src_port"; `value "dst_port"; 
              `value "seq_number"; `value "ack_number";
              `value "data_offset"; `value "window"; `size "options"; ],
            fun te_list ->
              (my_log "    TCP: @int -> @int\n\
                     \    seq: @int, ack: @int, \n\
                     \    data_offset: @int, window: @int, \
                     size of options: @int.\n" te_list, Expr.t));
          ( Promiwag_standard_protocols.dhcp,
            [ `value "op"; `value "xid"; `value "yiaddr"; `value "siaddr" ],
            fun te_list ->
              (my_log "      DHCP: op: @int, xid: @hex,\n\
                     \      You: @ipv4addr, Server: @ipv4addr.\n" te_list,
               Expr.t));
          ( Promiwag_standard_protocols.dns,
            [ `value "id"; `value "opcode"; ],
            fun te_list ->
              (my_log "      DNS: id: @hex, opcode: @int.\n" te_list, Expr.t));
          (* Example adding one "Empty protcol": *)
          ( let dccp = Promiwag_standard_protocols.dccp in
            Promiwag_protocol_stack.add_protocol the_internet dccp;
            dccp,
            [],
            fun te_list ->
              (my_log "      DCCP not handled.\n" te_list, Expr.f));
              (* Do.exit_named_while "brout"); *)
        ] in

    let packet = Generator.packet ~size:packet_size packet_pointer in
    
    let automata_block =
      Do.block 
        ( (Do.log "===== New Packet =====\n" [])
          :: (Generator.automata_block the_internet stack_handler packet)) in

    (* printf "Automata Block:\n%s\n" (Stiel_to_str.statement automata_block); *)
    
    automata_block
  in

  let stiel_parsing_block =
    (automata_treatment
       (Var.expression (Var.pointer ~unique:false "packet_buffer_expression"))
       (Var.expression (Var.unat ~unique:false "packet_buffer_length"))) in

  let c_parsing_block =
    let c_compiler =
      Promiwag.Stiel.To_C.compiler ~platform:Promiwag.Platform.default in
    let c_statement =
      Promiwag.Stiel.To_C.statement c_compiler stiel_parsing_block in
    Promiwag.C_backend.C_to_str.statement c_statement
  in
  let c_pcap_capture_app =
    Promiwag.Pcap.C.basic_loop
      ~call:(fun pckt lgth ->
        sprintf "  const unsigned char *packet_buffer_expression = %s;\n\
                \  unsigned int packet_buffer_length = %s;\n\
                %s" pckt lgth c_parsing_block) in

  let why_checkable_program = 
    Promiwag.Stiel.To_why_string.statement_to_string stiel_parsing_block in

  let ocaml_function =
    Promiwag.Stiel.To_ocaml_string.statement_to_string 
      ~with_default_prelude:true
      ~function_name:"promiwag_clean_parsing" stiel_parsing_block in
  let ocaml_pcap_capture = 
    Promiwag.Pcap.OCaml.basic_loop
      ~call:(fun pckt lgth ->
        sprintf "promiwag_clean_parsing\n\
                \   ~packet_buffer_expression:(ref (Unsafe_buffer.create %s))\n\
                \   ~packet_buffer_length:(ref (Integer.of_int %s))" pckt lgth) in

  (c_pcap_capture_app, why_checkable_program, 
   ocaml_function ^ ocaml_pcap_capture)

let test_clean_protocol_stack dev () =
  let (full_test_c_file, why_checkable_program, ocaml_function) =
    make_clean_protocol_stack dev in

  let pcap_prefix = System.tmp "pcap_procotol_parser" in
  let c_file = sprintf "%s.c" pcap_prefix in
  Io.with_file_out c_file (fun out ->
    Io.nwrite out full_test_c_file;
  );
  printf "Now you can compile and run (as root?):\n\
         \  gcc -lpcap %s -o %s \n\
         \  %s\n" c_file pcap_prefix pcap_prefix;
  let mlw_prefix = System.tmp "verify_parsing_automaton" in
  Io.with_file_out (sprintf "%s.mlw" mlw_prefix) (fun o ->
    Io.nwrite o why_checkable_program;
  );
  printf "Or you can prove: \n\
         \  why -fast-wp -alt-ergo %s.mlw\n\
         \  why-dp -prover Alt-Ergo -timeout 42 %s_why.why\n"
    mlw_prefix mlw_prefix;
  let ml_prefix = System.tmp "ocaml_procotol_parser" in
  Io.with_file_out (sprintf "%s.ml" ml_prefix) (fun o ->
    Io.nwrite o ocaml_function;
  );
  printf "Or play with OCaml:\n\
          \  %s %s.ml -o %s\n" 
    Promiwag.Pcap.OCaml.compilation_string ml_prefix ml_prefix;
  ()

let test_proving () =
  let module Stacks = Test_protocol_stacks in
  let dir_prefix = 
    let dir = 
      System.tmp $ sprintf "promiwag_proving_%s" (System.timestamp ()) in
    System.run_command (sprintf "rm -fr %s" dir);
    System.mkdir_p dir; dir 
  in

  let do_bench (name, do_program) =
    let file_prefix =
      sprintf "%s/%s" dir_prefix name in

    let file = fun s -> file_prefix ^ s in
    
    let str_result =
      try
        let program = do_program () in
        
        Io.with_file_out (file ".mlw") (fun o ->
          Io.nwrite o program;
        );
        let why = sprintf "why -fast-wp -alt-ergo %s" (file ".mlw") in
        printf "Running `%s'.\n%!" why;
        System.run_command why;
        let why_dp =
          sprintf "why-dp -timeout 60 -prover Alt-Ergo %s > %s" 
            (file "_why.why") 
            (file "_alt_ergo.out") in
        printf "Running `%s'.\n%!" why_dp;
        System.run_command why_dp;
        let grep =
          sprintf "egrep '(^total(   :| w)|^valid   :)' %s" (file "_alt_ergo.out")
        in
        let greped = System.slurp_command grep in
        greped
      with
      | e -> sprintf "Exception:\n  %s\n" (Printexc.to_string e)
    in
    (fun () -> printf "=== Test '%s':\n%s\n" name str_result;)
  in
  let recap =
    let (_, why_cps, _) =  make_clean_protocol_stack "dummy" in
    Ls.map do_bench [
      ("clean_ps", fun () ->  why_cps);
      ("right_1",  fun () ->  Stacks.make_why (Stacks.right_1 ()));
      ("wrong_1",  fun () ->  Stacks.make_why (Stacks.wrong_1 ()));
      ("wrong_2",  fun () ->  Stacks.make_why (Stacks.wrong_2 ()));
      ("wrong_3",  fun () ->  Stacks.make_why (Stacks.wrong_3 ()));
      ("right_2",  fun () ->  Stacks.make_why (Stacks.right_2 ()));
      ("wrong_4",  fun () ->  Stacks.make_why (Stacks.wrong_4 ()));
    ]in
  printf "===== Recapitulation:\n";
  Ls.iter (fun f -> f ()) recap;
  ()




let statement_to_string = ref Promiwag.Stiel.With_formatter.statement_to_string 

let test_minimal_parsing_code  () =
  let module Stiel = Promiwag.Stiel in
  let module Expr = Stiel.Expression in
  let module Var = Stiel.Variable in
  let module Do = Stiel.Statement in
  let module Meta_stack = Promiwag.Protocol_stack in
  let module Standard_protocols = Promiwag.Standard_protocols in
  let module Generator = Meta_stack.Automata_generator in
  let module Stiel_to_str = Stiel.To_string in
  let eth_and_ipv4 =
    let module SP = Standard_protocols in
    let s = Meta_stack.empty_protcol_stack () in
    Meta_stack.add_protocol s SP.ethernet 
      ~format:SP.ethernet_format ~transitions:SP.ethernet_transitions;
    Meta_stack.add_protocol s SP.ipv4 
      ~format:SP.ipv4_format ~transitions:SP.ipv4_transitions;
    s in
 
  let automata_treatment =
    let packet_pointer = Var.expression (Var.pointer "packet_pointer") in
    let packet_size = Var.expression (Var.unat "packet_size") in
    let stack_handler =
      Generator.handler
        (*~error_handler*)
        ~initial_protocol:Standard_protocols.ethernet [
          ( Standard_protocols.ethernet,
            [ ],
            function
              | [ ] ->
                (Do.log "Ethernet\n" [], Expr.t)
              | _ -> failwith "should have two typed expressions");
          ( Promiwag_standard_protocols.ipv4,
            [ `value "length" ],
            fun te_list ->
              (Do.log "IPv4\n" [], Expr.t));
        ] in

    let packet = Generator.packet ~size:packet_size packet_pointer in
    
    let automata_block =
      Do.block 
        ( (Do.log "===== New Packet =====\n" [])
          :: (Generator.automata_block eth_and_ipv4 stack_handler packet)) in

      automata_block
  in
  
  printf "Automata Block:\n%s\n" (!statement_to_string automata_treatment);
  ()

let test_why_output () =
  statement_to_string := (fun at ->
    let s = Promiwag.Stiel.To_why_string.statement_to_string at in
    let prefix = System.tmp "minimal_parsing" in
    Io.with_file_out (sprintf "%s.mlw" prefix) (fun o ->
      Io.nwrite o s;
    );
    (sprintf "=> Just try: \n\
             why -fast-wp -alt-ergo %s.mlw\n\
             why-dp -prover Alt-Ergo %s_why.why" prefix prefix));
  test_minimal_parsing_code ()


let () =
  printf "Promiwag's main test.\n";
  if Array.length Sys.argv <= 1 then
    printf "Nothing to do\n"
  else (
    (* Printexc.record_backtrace true; *)
    let test =
      match Sys.argv.(1) with
      | "pcap" ->  test_pcap_basic
      | "base" -> test_c_ast
      | "cps" -> test_clean_protocol_stack (try Sys.argv.(2) with _ -> "")
      | "minpars" -> test_minimal_parsing_code
      | "pi" -> print_the_internet
      | "why" -> test_why_output
      | "prove" -> test_proving
      | s -> failwith (sprintf "Unknown test: %s\n" s)
    in
    Printexc.print test (); 
  );
  printf "\nDone.\n";
  ()
