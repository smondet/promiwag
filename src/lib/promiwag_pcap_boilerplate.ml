open Promiwag_std

module C = struct

  (*
    With help from:
    http://piffle.googlecode.com/svn/trunk/piffle/src/pcap.c
  *)

  open Promiwag_c_backend

  (* Pcap configuration options *)
  let pcap_max_packet_size = `literal_int 65536 (* man page says this is enough *)
  let pcap_time_out = `literal_int 0 (* no timeout *)
  let pcap_promiscuity = `literal_int 0 (* don't force promiscuity *)


  type packet_treatment_fun =
      passed_argument:Variable.t -> packet_length:Variable.t -> 
      packet_buffer:Variable.t -> C_LightAST.block
      
  type make_capture_fun =
      device:C_LightAST.expression ->
      packet_treatment:packet_treatment_fun ->
      passed_expression:Typed_expression.t ->
      on_error:(string -> C_LightAST.expression -> C_LightAST.statement)  ->
      C_LightAST.toplevel list * C_LightAST.block

  let make_capture: make_capture_fun =
    fun ~device ~packet_treatment ~passed_expression ~on_error ->
      let error_buffer = 
        Variable.create ~name:"error_buffer" 
          ~c_type:(`array ([`variable "PCAP_ERRBUF_SIZE"], `signed_char))
          () in
      let pcap_source = 
        Variable.create ~c_type:(`pointer (`named_type "pcap_t"))
          ~name:"pcap_source"
          ~initialisation:(`variable "NULL") () in
      let locals = [
        `comment "Pcap local variables:";
        Variable.declaration error_buffer;
        Variable.declaration pcap_source;
      ] in
      let handler = 
        let name = "testpcap_handler" in
        let return_type = `void in
        let arg_user = 
          Variable.create ~name:"user" ~c_type:(`pointer `unsigned_char) () in
        let arg_header =
          Variable.create ~name:"packet_header"
            ~c_type:(`pointer (`named_type "const struct pcap_pkthdr")) () in
        let arg_packet =
          Variable.create ~name:"packet"
            ~c_type:(`pointer (`named_type "const u_char")) () in
        let arguments = [ arg_user; arg_header; arg_packet ] in
        let block =
          let packet_length =
            Variable.create ~name:"packet_length" ~c_type:`unsigned_int () in
          let packet_buffer = 
            Variable.create ~name:"packet_buffer"
              ~c_type:(`pointer `unsigned_char) () in
          let passed_argument =
            Variable.create ~name:"passed_argument" 
              ~c_type:(Typed_expression.c_type passed_expression) () in
          let user_block_delcarations, user_block_statements = 
            packet_treatment ~passed_argument ~packet_buffer ~packet_length in
          let declarations =
            (`comment "PCAP's declarations:") ::
              (Variable.declaration passed_argument) ::
              (Variable.declaration packet_length) ::
              (Variable.declaration packet_buffer) ::
              (`comment "PCAP's user declarations:") ::
              user_block_delcarations in
          let statements =
            (`comment "PCAP's statements:") ::
              (Variable.assignment ~cast:true passed_argument
                 (Variable.expression arg_user)) ::
              (Variable.assignment ~cast:true packet_length
                 (`arrow_field (Variable.expression arg_header, "caplen"))) ::
              (Variable.assignment ~cast:true packet_buffer
                 (Variable.expression arg_packet)) ::
              (`comment "PCAP's user statements:") ::
              user_block_statements in
          (declarations, statements) in
        Function.create ~name ~return_type ~arguments ~block () in
      let statements = [
        `comment "Pcap statements:";
        Construct.assignment_call pcap_source "pcap_open_live"
          [ device; pcap_max_packet_size;  pcap_promiscuity; 
            pcap_time_out; Variable.expression error_buffer ];
        `conditional 
          (`binary (`bin_eq, Variable.expression pcap_source, `variable "NULL"),
           on_error "pcap_open_live" (Variable.expression error_buffer), `empty);
        Construct.void_named_call "pcap_loop" [
          Variable.expression pcap_source;
          `literal_int (-1); Function.variable handler;
          `cast (`pointer `unsigned_char, 
                 Typed_expression.expression passed_expression)];
        Construct.void_named_call "pcap_close" [ Variable.expression pcap_source ];
      ] in

      ((`comment "Pcap's includes:") ::
          (Construct.sharp_includes ["stdio.h"; "pcap.h";]) @ 
          [(`comment "Pcap's callback handler:"); Function.definition handler],
       (locals, statements))

end
