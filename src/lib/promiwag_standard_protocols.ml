module MPS = Promiwag_meta_packet.Packet_structure
module Protocol_stack = Promiwag_protocol_stack
module PS = Promiwag_protocol_stack


let ethernet_name = "Ethernet"
let ipv4_name = "IPv4"
let arp_name = "ARP"
let ipv6_name = "IPv6"
let tcp_name = "TCP"
let udp_name = "UDP"
let dccp_name = "DCCP" 
let icmp_name = "ICMP"
let igmp_name = "IGMP"

let ethernet_format =
  let mac_addr_type = MPS.fixed_string 6 in
  MPS.packet_format ethernet_name [
    MPS.field "dest_addr" mac_addr_type;
    MPS.field "src_addr" mac_addr_type;
    MPS.field "ethertype_length" (MPS.fixed_int 16);
    MPS.payload ~name:"eth_payload" ();
    MPS.field "crc32" (MPS.fixed_int 32);
  ]
let ethernet_transitions =
  PS.transitions_switch ethernet_name "ethertype_length" [
    PS.case_int_value 0x800 ipv4_name;
    PS.case_int_value 0x806 arp_name;
    PS.case_int_value 0x86dd ipv6_name;
    PS.case_int_range 46 1500 "EthernetUnknownPayload_46-1500";
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
let ipv4_format =
  MPS.packet_format ipv4_name [
    MPS.fixed_int_field "version" 4;
    MPS.fixed_int_field "ihl" 4;
    MPS.fixed_int_field "tos_precedence" 3;
    MPS.fixed_int_field "tos_delay" 1;
    MPS.fixed_int_field "tos_throughput" 1;
    MPS.fixed_int_field "tos_reliability" 1;
    MPS.fixed_int_field "tos_reserved" 2; (* 16 bits *)
    MPS.fixed_int_field "length" 16;
    MPS.fixed_int_field "id" 16; (* 48 bits, 6 bytes *)
    MPS.fixed_int_field "reserved" 1;
    MPS.fixed_int_field "dont_fragment" 1;
    MPS.fixed_int_field "can_fragment" 1;
    MPS.fixed_int_field "frag_offset" 13; (* 8 bytes *)
    MPS.fixed_int_field "ttl" 8;
    MPS.fixed_int_field "protocol" 8;
    MPS.fixed_int_field "checksum" 16; (* 12 bytes *)
    MPS.fixed_int_field "src" 32;
    MPS.fixed_int_field "dest" 32; (* 20 bytes *)
    MPS.string_field "options" 
      (MPS.size
         (`align32 (`sub (`mul (`var "ihl", `int 4),
                          `add (`offset "dest", `int 4)))));
      (* : byte[(ihl * 4) - offsetafter(dest)] align(32); *)
      (* options are of size 0 when ihl = 5 *)
    (* TODO in the future maybe replace the last 4 by (`size "dest") ? *)
    MPS.payload 
      ~size:(MPS.size (`sub (`var "length", `mul (`var "ihl", `int 4))))
      ~name:"ip_payload"
      ();
    (* byte[length-(ihl*4)]; *)
  ]
let ipv4_transitions =
  PS.transitions_switch ipv4_name "protocol" [
    PS.case_int_value  1 icmp_name;
    PS.case_int_value  2 igmp_name;
    PS.case_int_value  6 tcp_name;
    PS.case_int_value 17 udp_name;
    PS.case_int_value 33 dccp_name;
  ]

let udp_format = 
  MPS.packet_format udp_name [
    MPS.fixed_int_field "src_port" 16;
    MPS.fixed_int_field "dst_port" 16;
    MPS.fixed_int_field "length" 16;
    MPS.fixed_int_field "checksum" 16;
    MPS.payload 
      ~size:(MPS.size (`sub (`var "length", `add (`offset "checksum", `int 4))))
      ~name:"udp_payload"
      ();
  ]
let udp_transitions = PS.empty_transitions udp_name

let test =
  MPS.packet_format "Test" [
    MPS.field "field_00" (MPS.fixed_string 8);
    MPS.field "field_01" (MPS.fixed_string 6);
    MPS.field "field_02" (MPS.fixed_string 6); (* 20 bytes *)
    MPS.field "field_byte" (MPS.fixed_int 8);
    MPS.field "field_04" (MPS.fixed_int 3);
    MPS.field "field_05" (MPS.fixed_int 4);
    MPS.field "field_06" (MPS.fixed_int 1);
    MPS.field "field_07" (MPS.fixed_int 16); (* 32 bits *)
    MPS.field "field_08" (MPS.fixed_int 16);
    MPS.field "field_09" (MPS.fixed_int 3);
    MPS.field "field_10" (MPS.fixed_int 12);
    MPS.field "field_11" (MPS.fixed_int 1); (* 32 bits, 28 bytes whole *)
    MPS.string_field "field_12" (MPS.size (`var "field_byte"));
    MPS.fixed_int_field "field_13" 32;
    MPS.string_field "like_ip_options" 
      (MPS.size
         (`align32 (`sub (`mul (`var "field_byte", `int 4),
                          `offset "field_13"))));
    MPS.fixed_int_field "after_like_ip_options" 32;

  ]

let format_database =
  Promiwag_meta_packet.Packet_database.of_list 
    [ ethernet_format; ipv4_format; udp_format ]

let internet_stack_from_ethernet () =
  let s = Protocol_stack.empty_protcol_stack () in
  Protocol_stack.add_protocol s ethernet_format ethernet_transitions;
  Protocol_stack.add_protocol s ipv4_format ipv4_transitions;
  Protocol_stack.add_protocol s udp_format udp_transitions;
  s
