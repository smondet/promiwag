
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

  let request_list = Ls.init 14 (fun i -> `field (sprintf "field_%02d" i)) in
  let packet_var =
    C.Variable.create ~unique:false ~name:"user_packet"
      ~c_type:(`pointer `unsigned_char) () in
  let packet_expression = C.Variable.typed_expression packet_var in
  let packet_format = snd Promiwag.Standard_packets.test in

  let block =
    Promiwag.Meta_packet.C_parsing.informed_block
      ~packet_format ~packet_expression ~request_list
      ~make_user_block:(fun te_list ->
        let vars =
          Ls.map request_list ~f:(function `field name -> C.Variable.create ~name ())
        in
        (Ls.map (packet_var :: vars) ~f:C.Variable.declaration,
         Ls.map2 vars te_list
           ~f:(fun var expr ->
            C.Variable.assignment var (C.Typed_expression.expression expr))))
  in
  let main, _, _ = 
    C.Construct.standard_main block in
  String_tree.print (C2StrTree.file  [C.Function.definition main]);
  
  let out = open_out "parsingtest.c" in
  String_tree.print ~out (C2StrTree.file [C.Function.definition main]);
  close_out out;
  ()

(* ocamlfind ocamlc -package "extlib" -linkpkg -g custom_ast.ml && ./a.out mp *)
let test_pcap_parsing dev () =
  let module C = Promiwag.C_backend in
  let module Pcap = Promiwag.Pcap_C in

  printf "Start test_pcap_parsing\n";
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
        call_printf "packet_treatment: arg:%d lgth: %d buf: %x\n"
          [counter_mem_contents;
           C.Variable.expression packet_length;
           C.Variable.expression packet_buffer;
          ];
        `assignment (counter_mem_contents,
                     `binary (`bin_add, `literal_int 1, counter_mem_contents));
(*        begin 
          let paths = [
            `field ("Ethernet", "dest_addr");
            `field ("Ethernet", "src_addr");
            `field ("Ethernet", "ethertype_length"); ] in
          let packet = ("Ethernet", Variable.typed_expression packet_buffer) in
          let format_db = Standard_packets.whole_database in
          let (_, stmts) =
            Meta_packet.C_parsing.get_fields ~paths ~packet ~format_db in
          let vars = [
            ("dest_addr", `pointer `unsigned_char, None);
            ("src_addr", `pointer `unsigned_char, None);
            ("ethertype_length", `unsigned_int, None); ] in
          let big_printf =
            call_printf "ethernet:\n    dest: %02x:%02x:%02x:%02x:%02x:%02x\n\
                          \    src: %02x:%02x:%02x:%02x:%02x:%02x\
                          \    ethertype_length: %x\n" [
              `array_index (`variable "dest_addr", `literal_int 0);
              `array_index (`variable "dest_addr", `literal_int 1);
              `array_index (`variable "dest_addr", `literal_int 2);
              `array_index (`variable "dest_addr", `literal_int 3);
              `array_index (`variable "dest_addr", `literal_int 4);
              `array_index (`variable "dest_addr", `literal_int 5);
              `array_index (`variable "src_addr",  `literal_int 0);
              `array_index (`variable "src_addr",  `literal_int 1);
              `array_index (`variable "src_addr",  `literal_int 2);
              `array_index (`variable "src_addr",  `literal_int 3);
              `array_index (`variable "src_addr",  `literal_int 4);
              `array_index (`variable "src_addr",  `literal_int 5);
              `variable "ethertype_length";
            ] in
          `block (vars, stmts @ [ big_printf ])
        end;*)
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
