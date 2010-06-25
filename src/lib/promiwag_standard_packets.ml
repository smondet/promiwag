module MPS = Promiwag_meta_packet.Packet_structure

let ethernet =
  let mac_addr_type = MPS.fixed_string 6 in
  MPS.packet_format "Ethernet" [
    MPS.field "dest_addr" mac_addr_type;
    MPS.field "src_addr" mac_addr_type;
    MPS.field "ethertype_length" (MPS.fixed_int 16);
      (* value(offset(end_of_packet)-offset(length)); *)
      (*      MPS.switch "ethertype_length" [
              MPS.case_uint 0x800 [ MPS.payload ~packet_type:"IPv4" () ];
              MPS.case_uint 0x806 [ MPS.payload ~packet_type:"Arp" () ];
              MPS.case_uint 0x86dd [ MPS.payload ~packet_type:"IPv6" ()];
              MPS.case_range 46 1500 [
              MPS.payload ~size_variable:"ethertype_length" ()];
              ]; *)
    MPS.payload ();
    MPS.field "crc32" (MPS.fixed_int 32);
  ]
(*
    version: bit[4] const(4);
    ihl: bit[4] min(5) value(offset(options) / 4);
    tos_precedence: bit[3] variant {
        |0 => Routine |1 -> Priority
        |2 -> Immediate |3 -> Flash
        |4 -> Flash_override |5 -> ECP
        |6 -> Internetwork_control |7 -> Network_control
    };
    tos_delay: bit[1] variant {|0 => Normal |1 -> Low};
    tos_throughput: bit[1] variant {|0 => Normal |1 -> Low};
    tos_reliability: bit[1] variant {|0 => Normal |1 -> Low};
    tos_reserved: bit[2] const(0);
    length: uint16 value(offset(data));
    id: uint16;
    reserved: bit[1] const(0);
    dont_fragment: bit[1] default(0);
    can_fragment: bit[1] default(0);
    frag_offset: bit[13] default(0);
    ttl: byte;
    protocol: byte variant {|1->ICMP |2->IGMP |6->TCP |17->UDP};
    checksum: uint16 default(0);
    src: uint32;
    dest: uint32;
    options: byte[(ihl * 4) - offset(dest)] align(32);
    header_end: label;
    data: byte[length-(ihl*4)];
*)
let ipv4 =
  MPS.packet_format "IPv4" [
    MPS.fixed_int_field "version" 4;
    MPS.fixed_int_field "ihl" 4;
    MPS.fixed_int_field "tos_precedence" 3;
    MPS.fixed_int_field "tos_delay" 1;
    MPS.fixed_int_field "tos_throughput" 1;
    MPS.fixed_int_field "tos_reliability" 1;
    MPS.fixed_int_field "tos_reserved" 2;
    MPS.fixed_int_field "length" 16;
    MPS.fixed_int_field "id" 16;
    MPS.fixed_int_field "reserved" 1;
    MPS.fixed_int_field "dont_fragment" 1;
    MPS.fixed_int_field "can_fragment" 1;
    MPS.fixed_int_field "frag_offset" 13;
    MPS.fixed_int_field "ttl" 8;
    MPS.fixed_int_field "protocol" 8;
    MPS.fixed_int_field "checksum" 16;
    MPS.fixed_int_field "src" 32;
    MPS.fixed_int_field "dest" 32;
    MPS.string_field "options" 
      (MPS.size
         (`align (32, (`sub (`mul (`var "ihl", `int 4), `offset "dest")))));
      (* : byte[(ihl * 4) - offset(dest)] align(32); *)
    MPS.payload 
      ~size:(MPS.size (`sub (`var "length", `mul (`var "ihl", `int 4))))
      ~name:"ip_payload"
      ();
    (* byte[length-(ihl*4)]; *)
  ]


let test =
  MPS.packet_format "Test" [
    MPS.field "field_00" (MPS.fixed_string 8);
    MPS.field "field_01" (MPS.fixed_string 6);
    MPS.field "field_02" (MPS.fixed_string 6); (* 20 bytes *)
    MPS.field "field_03" (MPS.fixed_int 8);
    MPS.field "field_04" (MPS.fixed_int 3);
    MPS.field "field_05" (MPS.fixed_int 4);
    MPS.field "field_06" (MPS.fixed_int 1);
    MPS.field "field_07" (MPS.fixed_int 16); (* 32 bits *)
    MPS.field "field_08" (MPS.fixed_int 16);
    MPS.field "field_09" (MPS.fixed_int 3);
    MPS.field "field_10" (MPS.fixed_int 12);
    MPS.field "field_11" (MPS.fixed_int 1); (* 32 bits, 28 bytes whole *)
    MPS.string_field "field_12" (MPS.size (`var "field_03"));
    MPS.fixed_int_field "field_13" 32;
  ]

let whole_database =
  Promiwag_meta_packet.Packet_database.of_list [ test; ethernet; ipv4 ]
