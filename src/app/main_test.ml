
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
    
  let rec print ?(out=stdout) = function
    | Str s -> output_string out s
    | Cat l -> Ls.iter (print ~out) l

end
module C2StrTree = Promiwag.C_backend.To_big_string(String_tree)


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
  
  let out = open_out "test.c" in
  String_tree.print ~out (C2StrTree.file my_prog);
  close_out out;
  (* gcc -Wall -ansi -pedantic test.c && ./a.out  *)
  (* indent -linux test.c && cat test.c  *)
  ()

let test_pcap_basic () =
  let module C = Promiwag.C_backend in
  let module Pcap = Promiwag.Pcap_C in
  printf "Start test_pcap_basic\n";
  let passed_structure =
    C.Variable.create ~name:"passed_structure"
      ~c_type:(`unsigned_long) ~initialisation:(`literal_int 1) () in
  let toplevels, (block_vars, block_sts) =
    let passed_expression =
      C.Variable.address_typed_expression passed_structure in
    let device = `literal_string "eth0" in
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
    Pcap.make_capture ~device ~on_error ~passed_expression ~packet_treatment in
  let main_pcap, _, _ = 
    C.Construct.standard_main 
      ((C.Variable.declaration passed_structure) :: block_vars, 
       block_sts @ [`return (`literal_int 0)]) in
  let test_pcap = 
    toplevels @ [ C.Function.definition main_pcap ] in
  String_tree.print (C2StrTree.file test_pcap);

  let out = open_out "pcaptest.c" in
  String_tree.print ~out (C2StrTree.file test_pcap);
  close_out out;
  (* gcc -Wall -lpcap  pcaptest.c && ./a.out *)
  ()



(* ocamlfind ocamlc -package "extlib" -linkpkg -g custom_ast.ml && ./a.out mpp any *)
let test_packet_parsing () =
  let module C = Promiwag.C_backend in

  let request, packet_printf_list = 
    let module S = Promiwag.Meta_packet.Packet_structure in
    let (n, fmt) = Promiwag.Standard_packets.test in
    let req =
      Ls.flatten 
        (Ls.map fmt ~f:(function
          | S.Item_field (n, S.Type_unsigned_integer _)
          | S.Item_field (n, S.Type_signed_integer _)
          | S.Item_field (n, S.Type_little_endian _) ->
            [ `value n; `offset n; `size n ]
          | S.Item_field (n, S.Type_string _) ->
            [ `pointer n; `offset n; `size n])) in
    let prs =
      Ls.flatten
        (Ls.map fmt ~f:(function
          | S.Item_field (n, t) ->
            [ call_printf (sprintf "Field %s: %s\n" n (S.string_of_content_type t)) [];
              call_printf "" [];
              call_printf "" []; ]))
    in
    (req, prs)
  in
  let packet_format = Promiwag.Standard_packets.test in
  let module Stage_one = Promiwag.Meta_packet.Parser_generator.Stage_1 in
  let stage_1 =
    Stage_one.compile_with_dependencies ~max_depth:10 ~packet_format request in

  printf "STAGE 1:\n%s\n" (Stage_one.dump stage_1);

  let module Stage_two = Promiwag.Meta_packet.Parser_generator.Stage_2_C in
  let packet_var =
    C.Variable.create ~unique:false ~name:"user_packet"
      ~c_type:(`array ([`literal_int 1000], `unsigned_char))
      ~initialisation:(`compound_literal 
                          (Ls.init 1000 (fun i -> `literal_int 255)))
      () in
  let packet =
    Promiwag.Meta_packet.C_packet.create
      ~size:(C.Typed_expression.create
                 (* ~expression:(`variable "packet_size") *)
               ~expression:(`literal_int 200)
               ~c_type:(`unsigned_long) ())
      (C.Variable.typed_expression packet_var) in
  let block =
    Stage_two.informed_block ~stage_1 ~packet ()
      ~create_variables:`for_all
      ~make_user_block:(fun te_list ->
        let declarations = 
          (C.Variable.declaration packet_var) :: [] in
        let statements =
          Ls.flatten
            (Ls.map2 te_list packet_printf_list 
               ~f:(fun te pr ->
                 [ pr; printf_of_typed_expression ~prefix:"  " te; ])) in
        C.Construct.block ~declarations ~statements ())
  in
  let main, _, _ = 
    C.Construct.standard_main block in
    (*ring_tree.print (C2StrTree.file  [C.Function.definition main]);*)
  
  let out = open_out "/tmp/parsingtest.c" in
  String_tree.print ~out (C2StrTree.file [C.Function.definition main]);
  close_out out;

  printf "gcc /tmp/parsingtest.c -o /tmp/parsingtest && /tmp/parsingtest";
  printf "\n";
  ()

let test_stiel ?(out=`term) () =


  let module IL = Promiwag.Stiel in
  let module Cons = IL.Construct in
  let module Transfo = IL.Transform in
  let module Partial = Transfo.Partial_evaluation in
  let module Env = Environment in
  
  let ( *** ) f g x = f (g x) in
  let out_tt s = if out = `term then s else sprintf "{t|{text}%s{end}}" s in
  let out_sec s =
    match out with
    | `term -> sprintf "==== %s ====\n" s
    | `brtx -> sprintf "{section|%s}\n" s in
  let out_par = match out with | `term -> "\n" | `brtx -> "{p}" in
  let out_code s = if out = `term then s else sprintf "{code endcode}%s{endcode}" s in

  let str_ie = out_tt *** IL.To_string.int_expression in
  let str_be = out_tt *** IL.To_string.bool_expression in

  let compiler =    IL.To_C.compiler ~platform:Promiwag.Platform.default in
  let int_variables = ref Env.empty in
  let add_int_var v e =
    int_variables := Env.add !int_variables v e;
    printf "%s" (out_sec "Integer-environment");
    let list_of_levels =
      Env.metamap !int_variables
        ~map:(fun (v, e) ->
          sprintf "('%s' is %s)" v (str_ie e))
        ~reduce:(fun strlist -> 
          sprintf "  [%s]" (Str.concat "; " strlist)) in
    printf "[\n%s\n]\n" (Str.concat "\n" list_of_levels);
  in
  let bool_variables = ref Env.empty in
  let add_bool_var v e =
    bool_variables := Env.add !bool_variables v e;
    printf "%s [\n" (out_sec "Boolean-environment");
    Env.iter ~f:(fun (v, e) ->
      printf " (%s = %s) " v (str_be e);
    ) !bool_variables;
    printf "]\n";
  in
  let pr t =
    let env =
      Partial.environment ~do_symbolic_equality:true ~use_purity:true
        ~int_variables:!int_variables ~bool_variables:!bool_variables () in
    let nope = "Not implemented or relevant" in
    let tryify s f e =
      try s (f e) with
      | exn -> 
        sprintf "Expception: %s" (out_tt (Printexc.to_string exn))
    in
    let title, str, c, str_prop, str_prop_pure, str_prop_eqsym, str_part =
      let i_prop =  Transfo.propagate_constants_in_int in
      let i_prop_pure = Transfo.propagate_constants_in_int ~use_purity:true in
      let i_prop_eqsym =
        Transfo.propagate_constants_in_int ~use_purity:true
          ~do_symbolic_equality:true in
      let i_part = Partial.int_expression env in
      let b_prop =  Transfo.propagate_constants_in_bool in
      let b_prop_pure = Transfo.propagate_constants_in_bool ~use_purity:true in
      let b_prop_eqsym =
        Transfo.propagate_constants_in_bool ~use_purity:true
          ~do_symbolic_equality:true in
      let b_part = Partial.bool_expression env in
      match t with
      | `int e ->
        ("Integer Expression",
         tryify str_ie (fun e -> e) e, 
         tryify (out_tt *** C2S.expression) (IL.To_C.int_expression compiler) e,
         tryify str_ie i_prop e,
         tryify str_ie i_prop_pure e,
         tryify str_ie i_prop_eqsym e,
         tryify str_ie i_part e)
      | `b e->
        ("Boolean Expression",
         tryify str_be (fun e -> e) e, 
         tryify (out_tt *** C2S.expression) (IL.To_C.bool_expression compiler) e,
         tryify str_be b_prop e,
         tryify str_be b_prop_pure e,
         tryify str_be b_prop_eqsym e,
         tryify str_be b_part e)
      | `block e ->
        ("Block",
         tryify (out_code *** IL.To_string.statement) (fun e -> e) e, 
         tryify (out_code *** C2S.statement) (IL.To_C.statement compiler) e,
         nope, nope, nope, nope)
    in
    printf "\n%s\n%s%s%!" (out_sec title) str out_par;
    printf "  To_C: %s%s%!" c out_par;
    printf "  Propagated: %s%s%!" str_prop out_par;
    printf "  Propagated(+purity): %s%s%!" str_prop_pure out_par;
    printf "  Propagated(+purity+eqsymb): %s%s%!" str_prop_eqsym out_par;
    printf "  Propagated(+purity+eqsymb+env): %s%s%!" str_part out_par;
  in
  pr$ `int  (Cons.int (`Add (`U 42, `U 28)));
  pr$ `int  (Cons.int (`Mul (`U 42, `U 28)));
  pr$ `int  (Cons.int (`Minus (`Plus (`U 42))));
  pr$ `int  (Cons.int 
               (`Add (`Sub (`Mul (`Div (`U 2, `U 2),`Mod (`U 42, `U 2)),
                            `Band (`U 0xff, `Bor (`U 7, `U 5))),
                      `Bxor (`U 0, `Bshl (`U 4, `U 5)))));
  pr$ `int (Cons.int (`Bshr (`U 17, `Var "brout")));
  add_int_var "brout" (Cons.int (`U 3));
  pr$ `int (Cons.int (`Bshr (`U 17, `Var "brout")));
  add_int_var "with_var" (Cons.int (`Add (`U 2, `Var "other_var")));
  pr$ `int (Cons.int (`Mul (`U 2, `Var "with_var")));
  add_int_var "other_var" (Cons.int (`Add (`U 2, `U 8)));
  pr$ `int (Cons.int (`Mul (`U 2, `Var "with_var")));
  pr$ `int (Cons.int (`U64 (Int64.max_int)));

  pr$ `int (Cons.int (`LsAdd [`U 1; `U 1; `Var "with_var"]));
  pr$ `int (Cons.int (`Prod  [`U 2; `U 2; `Var "with_var"]));
  pr$ `b   (Cons.bool (`Eq (`U8_Big_at (Cons.buffer (`Var "buf")),
                            `U8_Big_at (Cons.buffer (`Var "buf")))));
  pr$ `b   (Cons.bool (`Eq (`U16_Big_at (Cons.buffer (`Var "buf")), 
                            `U32_Big_at (Cons.buffer (`Var "buf")))));
  pr$ `int (Cons.int (  `U64_at (Cons.buffer (`Offset (`Var "Buf", `U 42)))));
  pr$ `int (Cons.int ( `Unat_Little_at (Cons.buffer (`Var "buf"))));

  add_bool_var "the_truth" (Cons.bool `T);
  pr$ `b   (Cons.bool (`And (`T, `Or (`Var "the_truth", `Not `F))));
  pr$ `b   (Cons.bool (`Eq (`U 42, `U 42)));
  pr$ `b   (Cons.bool (`Neq(`U 42, `U 42)));
  pr$ `b   (Cons.bool (`Gt (`U 42, `U 42)));
  pr$ `b   (Cons.bool (`Lt (`U 42, `U 42)));
  pr$ `b   (Cons.bool (`Ge (`U 42, `U 42)));
  pr$ `b   (Cons.bool (`Le (`U 42, `U 42)));
  pr$ `b   (Cons.bool (`LsAnd [`T; `T; `T; `Var "unknown"]));
  pr$ `b   (Cons.bool (`LsOr [`T; `T; `Var "the_truth"]));

  let b = 
    let nop = Cons.nop () in
    Cons.block [
      nop;
      Cons.conditional (Cons.bool (`Eq (`U 42, `U 0)))
        ~statement_then:(Cons.block [nop;nop;nop])
        ~statement_else:nop;
      Cons.while_loop (Cons.bool `T) (Cons.block [nop; nop]);
(*      Cons.declare (`Sized_buffer ("buf", 42));
      Cons.declare (`Pointer "pointer");
      Cons.declare (`U8   "vara");
      Cons.declare (`U16  "varz");
      Cons.declare (`U32  "vare");
      Cons.declare (`U64  "varr");
      Cons.declare (`Unat "varq");
      Cons.declare (`Bool "vars"); *)
      Cons.cmt "Some comments...";
(*      Cons.assignment "var" (Cons.int (`U 42));
      Cons.assignment "var" (Cons.buffer (`Var "buf"));
      Cons.assignment "var" (Cons.bool `F);
*)  ] in
  pr$ `block b;

  printf "====== Testing partial evaluation:\n";

  let v = (`Var "variable") in
  let u = (`Var "unknown") in
  let zero = `U 0 in
  let one = `U 1 in

  pr$ `int (Cons.int (`Bxor (u, v)));
  pr$ `int (Cons.int (`Add  (u, one)));
  pr$ `int (Cons.int (`Sub  (u, one)));
  pr$ `int (Cons.int (`Mul  (u, one)));
  pr$ `int (Cons.int (`Div  (u, one)));
  pr$ `int (Cons.int (`Mod  (u, one)));
  pr$ `int (Cons.int (`Band (u, one)));
  pr$ `int (Cons.int (`Bor  (u, one)));
  pr$ `int (Cons.int (`Bxor (u, one)));
  pr$ `int (Cons.int (`Bshl (u, one)));
  pr$ `int (Cons.int (`Bshr (u, one)));

  pr$ `int (Cons.int (`Add  (one, u)));
  pr$ `int (Cons.int (`Sub  (one, u)));
  pr$ `int (Cons.int (`Mul  (one, u)));
  pr$ `int (Cons.int (`Div  (one, u)));
  pr$ `int (Cons.int (`Mod  (one, u)));
  pr$ `int (Cons.int (`Band (one, u)));
  pr$ `int (Cons.int (`Bor  (one, u)));
  pr$ `int (Cons.int (`Bxor (one, u)));
  pr$ `int (Cons.int (`Bshl (one, u)));
  pr$ `int (Cons.int (`Bshr (one, u)));

  pr$ `int (Cons.int (`Add  (zero, u)));
  pr$ `int (Cons.int (`Sub  (zero, u)));
  pr$ `int (Cons.int (`Mul  (zero, u)));
  pr$ `int (Cons.int (`Div  (zero, u)));
  pr$ `int (Cons.int (`Mod  (zero, u)));
  pr$ `int (Cons.int (`Band (zero, u)));
  pr$ `int (Cons.int (`Bor  (zero, u)));
  pr$ `int (Cons.int (`Bxor (zero, u)));
  pr$ `int (Cons.int (`Bshl (zero, u)));
  pr$ `int (Cons.int (`Bshr (zero, u)));

  pr$ `int (Cons.int (`Add  (u, zero)));
  pr$ `int (Cons.int (`Sub  (u, zero)));
  pr$ `int (Cons.int (`Mul  (u, zero)));
  pr$ `int (Cons.int (`Div  (u, zero)));
  pr$ `int (Cons.int (`Mod  (u, zero)));
  pr$ `int (Cons.int (`Band (u, zero)));
  pr$ `int (Cons.int (`Bor  (u, zero)));
  pr$ `int (Cons.int (`Bxor (u, zero)));
  pr$ `int (Cons.int (`Bshl (u, zero)));
  pr$ `int (Cons.int (`Bshr (u, zero)));

  let compa = (`Add  (u, (`Mod (v, `U 42)))) in
  let compb = (`Sub  (u, (`Mod (v, `U 42)))) in

  pr$ `int (Cons.int compa);
  pr$ `int (Cons.int compb);

  pr$ `int (Cons.int (`Add  (compa, compb)));
  pr$ `int (Cons.int (`Sub  (compa, compb)));
  pr$ `int (Cons.int (`Mul  (compa, compb)));
  pr$ `int (Cons.int (`Div  (compa, compb)));
  pr$ `int (Cons.int (`Mod  (compa, compb)));
  pr$ `int (Cons.int (`Band (compa, compb)));
  pr$ `int (Cons.int (`Bor  (compa, compb)));
  pr$ `int (Cons.int (`Bxor (compa, compb)));
  pr$ `int (Cons.int (`Bshl (compa, compb)));
  pr$ `int (Cons.int (`Bshr (compa, compb)));

  pr$ `int (Cons.int (`Add  (compa, compa)));
  pr$ `int (Cons.int (`Sub  (compa, compa)));
  pr$ `int (Cons.int (`Mul  (compa, compa)));
  pr$ `int (Cons.int (`Div  (compa, compa)));
  pr$ `int (Cons.int (`Mod  (compa, compa)));
  pr$ `int (Cons.int (`Band (compa, compa)));
  pr$ `int (Cons.int (`Bor  (compa, compa)));
  pr$ `int (Cons.int (`Bxor (compa, compa)));
  pr$ `int (Cons.int (`Bshl (compa, compa)));
  pr$ `int (Cons.int (`Bshr (compa, compa)));

  ()


(* ocamlfind ocamlc -package "extlib" -linkpkg -g custom_ast.ml && ./a.out mp *)
let test_pcap_parsing dev () =
  let module C = Promiwag.C_backend in
  let module Pcap = Promiwag.Pcap_C in

  printf "Start test_pcap_parsing\n";

  let module C = Promiwag.C_backend in

  let module Stage_one = Promiwag.Meta_packet.Parser_generator.Stage_1 in
  let stage_1_ethernet =
    let request_list = [
      `pointer "dest_addr"; `pointer "src_addr";
      `value "ethertype_length"; `offset "ethertype_length";
      `pointer "eth_payload" ] in
    let packet_format = Promiwag.Standard_packets.ethernet in
    Stage_one.compile_with_dependencies
      ~max_depth:10 ~packet_format request_list in

  (* printf "STAGE 1:\n%s\n" (Stage_one.dump stage_1_ethernet); *)
  
  let stage_1_ipv4 =
    let request_list = [
      `value "version";
      `value "ihl";
      `value "length"; 
      `value "id";
      `value "can_fragment"; 
      `value "ttl";
      `value "protocol";
      `value "checksum";
      `pointer "src";
      `pointer "dest";
      `size "options";
      `offset "ip_payload";
      `pointer "ip_payload";
      `size "ip_payload";
    ] in
    let packet_format = Promiwag.Standard_packets.ipv4 in
    Stage_one.compile_with_dependencies
      ~max_depth:10 ~packet_format request_list in

  let stage_1_udp =
    let request_list = [
      `value "src_port"; `value "dst_port";
      `value "length"; `value "checksum";
      `pointer "udp_payload"; `size "udp_payload" ] in
    let packet_format = Promiwag.Standard_packets.udp in
    Stage_one.compile_with_dependencies
      ~max_depth:10 ~packet_format request_list in



  (* printf "%s\n" (Stage_one.dump stage_1_ipv4); *)

  let module Stage_two = Promiwag.Meta_packet.Parser_generator.Stage_2_C in
  let expr_of_te =  C.Typed_expression.expression in
  let idx te i =
    `array_index (C.Typed_expression.expression ~cast:true te,
                  `literal_int i) in
  
  let block_for_ethernet packet_buffer block_for_ipv4 =
    let packet =
      Promiwag.Meta_packet.C_packet.create
        (C.Variable.typed_expression packet_buffer) in
    Stage_two.informed_block ~stage_1:stage_1_ethernet ~packet 
      ~platform:Promiwag.Platform.default ()
      ~make_user_block:(fun te_list ->
        match te_list with 
        | [var_offset_dest_addr; var_field_src_addr;
           var_field_ethertype_length; var_offset_ethertype_length;
           var_payload] ->
          
          let big_printf =
            call_printf "  Ethernet:\n\
                          \    dest: %02x:%02x:%02x:%02x:%02x:%02x,\
                          \    src: %02x:%02x:%02x:%02x:%02x:%02x,\n\
                          \    ethertype_length: %hd (at %x + %d).\n" [
              idx var_offset_dest_addr 0;
              idx var_offset_dest_addr 1;
              idx var_offset_dest_addr 2;
              idx var_offset_dest_addr 3;
              idx var_offset_dest_addr 4;
              idx var_offset_dest_addr 5;
              idx var_field_src_addr 0;
              idx var_field_src_addr 1;
              idx var_field_src_addr 2;
              idx var_field_src_addr 3;
              idx var_field_src_addr 4;
              idx var_field_src_addr 5;
              C.Typed_expression.expression var_field_ethertype_length;
              C.Variable.expression packet_buffer;
              C.Typed_expression.expression var_offset_ethertype_length;
            ] in
          let ethertypelegnth_expr =
            C.Typed_expression.expression var_field_ethertype_length in
          let if_ipv4 =
            C.Construct.if_then_else 
              (`binary (`bin_eq, ethertypelegnth_expr, `literal_int 0x800))
              ~block_then:(block_for_ipv4 var_payload) in
          let if_arp =
            let block_then = ([], [call_printf "  ARP.\n" []]) in
            C.Construct.if_then_else 
              (`binary (`bin_eq, ethertypelegnth_expr, `literal_int 0x806))
              ~block_then in
          ([], [big_printf; if_ipv4; if_arp])
        | _ -> failwith "Wrong number of typed_exprs"
      )
  in
  let block_for_ipv4 block_udp var_payload = 
    let packet =
      Promiwag.Meta_packet.C_packet.create var_payload in
    Stage_two.informed_block ()
      ~stage_1:stage_1_ipv4 ~packet
      ~platform:Promiwag.Platform.default
      ~make_user_block:(function
        | [field_version; field_ihl;
           field_length; field_id;
           field_can_fragment; 
           field_ttl; field_protocol;
           field_checksum;
           pointer_src;
           pointer_dest; 
           size_options;
           offset_payload;
           pointer_payload;
           size_payload;] ->
          ([], [call_printf "  IPv4:\n\
                                  \    version: %d, ihl: %d, length: %d, \
                                  id: %d,\n\
                                  \    can fragment: %d, TTL: %d, protocol: %d,\n\
                                  \    checksum: %d, \
                                  length of IP options: %d bits,\n\
                                  \    SRC: %d.%d.%d.%d DEST: %d.%d.%d.%d,\n\
                                  \    Payload (offset: %d, size: %d bytes) is:\n" 
                   [expr_of_te field_version;
                    expr_of_te field_ihl;
                    expr_of_te field_length;
                    expr_of_te field_id;
                    expr_of_te field_can_fragment; 
                    expr_of_te field_ttl;
                    expr_of_te field_protocol;
                    expr_of_te field_checksum;
                    expr_of_te size_options;
                    idx pointer_src 0;
                    idx pointer_src 1;
                    idx pointer_src 2;
                    idx pointer_src 3;
                    idx pointer_dest 0;
                    idx pointer_dest 1;
                    idx pointer_dest 2;
                    idx pointer_dest 3;
                    expr_of_te offset_payload;
                    `binary(`bin_div, expr_of_te size_payload,
                            `literal_int 8);
                   ];
                printf_packet ~prefix:"    p"
                  (expr_of_te pointer_payload) 20;
                C.Construct.if_then_else 
                  (`binary (`bin_eq, expr_of_te field_protocol, `literal_int 17))
                  ~block_then:(block_udp pointer_payload)

               ])
            
        | _ -> failwith "Wrong number of typed_exprs"
      ) in

  let block_for_udp packet_expr =
    let packet =
      Promiwag.Meta_packet.C_packet.create packet_expr in
    Stage_two.informed_block ()
      ~stage_1:stage_1_udp ~packet
      ~platform:Promiwag.Platform.default
      ~make_user_block:(function
        | [ value_src_port; value_dst_port;
            value_length; value_checksum;
            pointer_udp_payload; size_udp_payload ]  ->
          ([], [
            call_printf "  UDP:\n\
                                  \    src: %d, dest: %d,\n\
                                  \    length: %d, checksum: %d,\n\
                                  \    Payload (%d bytes) is:\n" 
              ((Ls.map expr_of_te [ value_src_port; value_dst_port;
                                    value_length; value_checksum; ]) @ 
                  [`binary(`bin_div, expr_of_te size_udp_payload,
                           `literal_int 8)]);
            printf_packet ~prefix:"    p"
              (expr_of_te pointer_udp_payload) 20;
           ];)
        | _ -> failwith "Wrong number of typed_exprs"
      ) 
  in

  let parsing_block packet_buffer =
    `block (block_for_ethernet packet_buffer (block_for_ipv4 block_for_udp))
  in

  (* PCAP part: *)

  let passed_structure =
    C.Variable.create ~name:"passed_structure"
      ~c_type:(`unsigned_long) ~initialisation:(`literal_int 1) () in
  let toplevels, (block_vars, block_sts) =
    let passed_expression =
      C.Variable.address_typed_expression passed_structure in
    let device = `literal_string dev in
    let on_error s e = 
      `block (C.Construct.block () ~statements:[
        call_printf "PCAP ERROR: %s: %s\n" [`literal_string s; e];
        `return (`literal_int 2);
      ]) in
    let packet_treatment ~passed_argument ~packet_length ~packet_buffer =
      let counter_mem_contents =
        `unary (`unary_memof, C.Variable.expression passed_argument) in
      C.Construct.block () ~statements:[
        call_printf "=== Packet %d === (length: %d, address: %x)\n"
          [counter_mem_contents;
           C.Variable.expression packet_length;
           C.Variable.expression packet_buffer;
          ];
        printf_packet ~prefix:"  Content" (C.Variable.expression packet_buffer) 20;
        `assignment (counter_mem_contents,
                     `binary (`bin_add, `literal_int 1, counter_mem_contents));

        parsing_block packet_buffer;
      ]
    in
    Pcap.make_capture ~device ~on_error ~passed_expression ~packet_treatment in

  (* Main: *)

  let main_pcap, _, _ = 
    C.Construct.standard_main 
      ((C.Variable.declaration passed_structure) :: block_vars, 
       block_sts @ [`return (`literal_int 0)]) in
  let test_pcap = 
    toplevels @ [ C.Function.definition main_pcap ] in
  (* String_tree.print (C2StrTree.file test_pcap); *)

  let out = open_out "/tmp/pcaptest.c" in
  String_tree.print ~out (C2StrTree.file test_pcap);
  close_out out;
  printf "\  Now you can:\n\
          \  gcc -lpcap /tmp/pcaptest.c -o /tmp/pcaptest \n\
          \  /tmp/pcaptest\n";
  ()





let () =
  printf "Promiwag's main test.\n";
  if Array.length Sys.argv <= 1 then
    printf "Nothing to do\n"
  else (
    Printexc.record_backtrace true;
    let test =
      match Sys.argv.(1) with
      | "pcap" ->  test_pcap_basic
      | "base" -> test_c_ast
      | "mp" -> test_packet_parsing
      | "il" -> test_stiel ~out:`term
      | "ilbrtx" -> test_stiel ~out:`brtx
      | "mpp" -> test_pcap_parsing Sys.argv.(2)
      | s -> failwith (sprintf "Unknown test: %s\n" s)
    in
    Printexc.print test (); (* with
                               | e -> 
                               printf "Excpetion occured: %s\nBacktrace:\n%s\n" 
                               (Printexc.to_string e) (Printexc.get_backtrace ()) *)
  );
  printf "\nDone.\n";
  ()
