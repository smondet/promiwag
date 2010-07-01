
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

  (* printf "%s\n" (Stage_one.dump stage_1_ipv4); *)



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

begin
        let module Stage_two = Promiwag.Meta_packet.Parser_generator.Stage_2_C in
        let block_ethernet block_ipv4 =
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
                let idx te i =
                  `array_index (C.Typed_expression.expression ~cast:true te,
                                `literal_int i) in

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
                let ipv4_treatment =
                  let block_then = 
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
                          let expr_of_te =  C.Typed_expression.expression in
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

                               ])
                          
                        | _ -> failwith "Wrong number of typed_exprs"
                      ) in
                  C.Construct.if_then_else 
                    (`binary (`bin_eq, ethertypelegnth_expr, `literal_int 0x800))
                    ~block_then
                in
                let arp_treatment =
                  let block_then = ([], [call_printf "  ARP.\n" []]) in
                  C.Construct.if_then_else 
                    (`binary (`bin_eq, ethertypelegnth_expr, `literal_int 0x806))
                    ~block_then in
                ([], [big_printf; ipv4_treatment; arp_treatment])
              | _ -> failwith "Wrong number of typed_exprs")
        in
        `block (block_ethernet (C.Construct.block ()))
end;
      ]
    in
    Pcap.make_capture ~device ~on_error ~passed_expression ~packet_treatment in
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
