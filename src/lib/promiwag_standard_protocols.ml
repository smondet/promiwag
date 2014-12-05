(**************************************************************************)
(*  Copyright (c) 2010, 2011, Sebastien MONDET                            *)
(*                                                                        *)
(*  Permission is hereby granted, free of charge, to any person           *)
(*  obtaining a copy of this software and associated documentation        *)
(*  files (the "Software"), to deal in the Software without               *)
(*  restriction, including without limitation the rights to use,          *)
(*  copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*  copies of the Software, and to permit persons to whom the             *)
(*  Software is furnished to do so, subject to the following              *)
(*  conditions:                                                           *)
(*                                                                        *)
(*  The above copyright notice and this permission notice shall be        *)
(*  included in all copies or substantial portions of the Software.       *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*  OTHER DEALINGS IN THE SOFTWARE.                                       *)
(*                                                                        *)
(**************************************************************************)

module MPS = Promiwag_meta_packet.Packet_structure
module Protocol_stack = Promiwag_protocol_stack
module PS = Promiwag_protocol_stack


let ethernet = "Ethernet"
let ipv4 = "IPv4"
let arp = "ARP"
let ipv6 = "IPv6"
let tcp = "TCP"
let udp = "UDP"
let dccp = "DCCP" 
let icmp = "ICMP"
let igmp = "IGMP"
let gre = "GRE"
let dhcp = "DHCP"
let dns = "DNS"

let ethernet_format =
  let mac_addr_type = MPS.fixed_string 6 in
  MPS.packet_format [
    MPS.field "dest_addr" mac_addr_type;
    MPS.field "src_addr" mac_addr_type;
    MPS.field "ethertype_length" (MPS.fixed_int 16);
    MPS.payload ~name:"eth_payload" ();
    MPS.field "crc32" (MPS.fixed_int 32);
  ]
let ethernet_transitions =
  PS.switch "ethertype_length" [
    PS.case_int_value 0x800 ipv4 "eth_payload";
    PS.case_int_value 0x806 arp "eth_payload";
    PS.case_int_value 0x86dd ipv6 "eth_payload";
    PS.case_int_range 46 1500 "EthernetUnknownPayload_46-1500" "eth_payload";
  ]
  
(*
From Ion's:
/* http://www.iana.org/assignments/arp-parameters, 
   don't ask me why ATM has three values, Internet is what
   it is... */

ptype: uint16 variant `0x800->Ipv4 |0x806->Ipv6 `;
*)

let arp_format =
  MPS.packet_format [
    MPS.fixed_int_field "htype" 16;
    MPS.fixed_int_field "ptype" 16;
    MPS.fixed_int_field "hlen"   8;
    MPS.fixed_int_field "plen"   8;
    MPS.fixed_int_field "op"    16;
    MPS.string_field    "sha"   (MPS.size (`var "hlen"));
    MPS.string_field    "spa"   (MPS.size (`var "plen"));
    MPS.string_field    "tha"   (MPS.size (`var "hlen"));
    MPS.string_field    "tpa"   (MPS.size (`var "plen"));
  ]
let arp_transitions = PS.empty_transition

let string_arp_htype = [
  (0, "Reserved");
  (1, "Ethernet");
  (2, "ExperimentalEthernet");
  (3, "AmateurRadioAXDOT25");
  (4, "ProteonProNETTokenRing");
  (5, "Chaos");
  (6, "IEEE802Networks");
  (7, "ARCNET");
  (8, "Hyperchannel");
  (9, "Lanstar");
  (0, "AutonetShortAddress");
  (11, "LocalTalk");
  (12, "LocalNet");
  (31, "Ultralink");
  (14, "SMDS");
  (15, "FrameRelay");
  (16, "ATM");
  (17, "HDLC");
  (18, "FibreChannel");
  (19, "ATM1");
  (20, "SerialLine");
  (21, "ATM3");
  (22, "MILSTD188220");
  (23, "Metricom");
  (24, "IEEE1394DOT1995");
  (25, "MAPOS");
  (26, "Twinaxial");
  (27, "EUI64");
  (28, "HIPARP");
  (29, "IPandARPoverISO78163");
  (30, "ARPSec");
  (31, "IPsectunnel");
  (32, "InfiniBand");
  (33, "TIA102Project25CAI");
  (34, "WiegandInterface");
  (35, "PureIP");
  (36, "HW_EXP1" );
]
let string_arp_op = [
  (0, "Reserved");
  (1, "REQUEST");
  (2, "REPLY");
  (3, "RequestReverse");
  (4, "ReplyReverse");
  (5, "DRARPRequest");
  (6, "DRARPReply");
  (7, "DRARPError");
  (8, "InARPRequest");
  (9, "InARPReply");
  (10, "ARPNAK");
  (11, "MARSRequest");
  (12, "MARSMulti");
  (13, "MARSMServ");
  (14, "MARSJoin");
  (15, "MARSLeave");
  (16, "MARSNAK");
  (17, "MARSUnserv");
  (18, "MARSSJoin");
  (19, "MARSSLeave");
  (20, "MARSGrouplistRequest");
  (21, "MARSGrouplistReply");
  (22, "MARSRedirectMap");
  (23, "MAPOSUNARP");
  (24, "OP_EXP1");
  (25, "OP_EXP2");
]

let gre_format = 
  MPS.packet_format [
    MPS.fixed_int_field "checksum_present" 1;
    MPS.fixed_int_field "reserved" 12;
    MPS.fixed_int_field "version" 3;
    MPS.fixed_int_field "protocol" 16; (* variant 2048->IP, 2054->ARP *)
    MPS.string_field "checksum"    (MPS.size (`mul (`var "checksum_present", `int 2)));
    MPS.string_field "reserved_cs" (MPS.size (`mul (`var "checksum_present", `int 2)));
    MPS.payload ~name:"gre_payload" ();
  ]
(*let gre_transitions = 
  PS.switch "checksum_present" [
    PS.case_int_value 0 gre_without_checksum "gre_payload";
    PS.case_int_value 1 gre_with_checksum "gre_payload";
  ]*)

let gre_transitions =
  PS.switch "protocol" [
    PS.case_int_value 0x800  ipv4 "gre_payload";
    PS.case_int_value 0x806  arp  "gre_payload";
    PS.case_int_value 0x86dd ipv6 "gre_payload";
    PS.case_int_range 46 1500 "GREUnknownPayload_46-1500" "gre_payload";
  ]


(*
    tos_delay: bit[1] variant |0 => Normal |1 -> Low;
    tos_throughput: bit[1] variant |0 => Normal |1 -> Low;
    tos_reliability: bit[1] variant |0 => Normal |1 -> Low;
    protocol: byte variant |1->ICMP |2->IGMP |6->TCP |17->UDP;
*)
let ipv4_format =
  MPS.packet_format [
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
  PS.switch "protocol" [
    PS.case_int_value  1 icmp "ip_payload";
    PS.case_int_value  2 igmp "ip_payload";
    PS.case_int_value  6  tcp "ip_payload";
    PS.case_int_value 17  udp "ip_payload";
    PS.case_int_value 33 dccp "ip_payload";
    PS.case_int_value 47 gre  "ip_payload";
  ]
let ipv4_checks =
  [ PS.check_range "ihl" 5 15 ]

let ipv4_tos_precedence_string = [
  (0, "Routine");
  (1, "Priority");
  (2, "Immediate");
  (3, "Flash");
  (4, "Flash_override");
  (5, "ECP");
  (6, "Internetwork_control");
  (7, "Network_control");
]


let udp_format = 
  MPS.packet_format [
    MPS.fixed_int_field "src_port" 16;
    MPS.fixed_int_field "dst_port" 16;
    MPS.fixed_int_field "length" 16;
    MPS.fixed_int_field "checksum" 16;
    MPS.payload 
      ~size:(MPS.size (`sub (`var "length", `add (`offset "checksum", `int 2))))
      ~name:"udp_payload"
      ();
  ]
let udp_transitions =
  PS.sequence [
    PS.switch "dst_port" [
      PS.case_int_value 53  dns "udp_payload";
      PS.case_int_value 67 dhcp "udp_payload";
      PS.case_int_value 68 dhcp "udp_payload";
    ];
    PS.switch "src_port" [
      PS.case_int_value 53  dns "udp_payload";
      PS.case_int_value 67 dhcp "udp_payload";
      PS.case_int_value 68 dhcp "udp_payload";
    ];
  ]

let tcp_format =
  MPS.packet_format [
    MPS.fixed_int_field "src_port" 16;
    MPS.fixed_int_field "dst_port" 16;
    MPS.fixed_int_field "seq_number" 32;
    MPS.fixed_int_field "ack_number" 32;

    MPS.fixed_int_field "data_offset" 4;
    MPS.fixed_int_field "reserved"    4;
    MPS.fixed_int_field  "cwr" 1;
    MPS.fixed_int_field  "ece" 1;
    MPS.fixed_int_field  "urg" 1;
    MPS.fixed_int_field  "ack" 1;
    MPS.fixed_int_field  "psh" 1;
    MPS.fixed_int_field  "rst" 1;
    MPS.fixed_int_field  "syn" 1;
    MPS.fixed_int_field  "fin" 1;

    MPS.fixed_int_field  "window" 16;
    MPS.fixed_int_field  "checksum" 16;
    MPS.fixed_int_field  "urg_pointer" 16;
    MPS.string_field  "options"
      (MPS.size (`sub (`mul (`var "data_offset", `int 4),
                       `add (`offset "urg_pointer", `int 2))));
    MPS.payload ~name:"tcp_payload" ();
  ]
let tcp_transitions = PS.empty_transition




(*
packet dhcp {
    op: byte variant {
 |1 3-> BootRequest |2-> BootReply };
    htype: byte variant { |1 -> Ethernet };
    hlen: byte value(sizeof(chaddr));
    hops: byte;
    xid: uint32;
    secs: uint16;
	broadcast: bit[1];
	reserved: bit[15] const(0);
    ciaddr: uint32;
    yiaddr: uint32;
    siaddr: uint32;
    giaddr: uint32;
    chaddr: byte[16];
    sname: byte[64];
    file: byte[128];
    options: byte[remaining()];
}

There is more (options) in the file
*)

let dhcp_format =
  MPS.packet_format [
    MPS.fixed_int_field "op"     8;
    MPS.fixed_int_field "htype"  8;
    MPS.fixed_int_field "hlen"   8;
    MPS.fixed_int_field "hops"   8;
    MPS.fixed_int_field "xid"   32;
    MPS.fixed_int_field "secs"  16;
    MPS.fixed_int_field "broadcast" 1;
    MPS.fixed_int_field "reserved" 15;
    MPS.fixed_int_field "ciaddr"   32;
    MPS.fixed_int_field "yiaddr"   32;
    MPS.fixed_int_field "siaddr"   32;
    MPS.fixed_int_field "giaddr"   32;
    MPS.string_field    "chaddr"  (MPS.size (`int  16));
    MPS.string_field    "sname"   (MPS.size (`int  64));
    MPS.string_field    "file"    (MPS.size (`int 128));
(*    MPS.fixed_int_field "options_code" 8;
    MPS.fixed_int_field "option_len"   8;
    MPS.string_field    "option_content" (MPS.size (`var "option_len")); *)
  ]
let dhcp_transitions = PS.empty_transition



let dns_format =
  MPS.packet_format [
    MPS.fixed_int_field "id" 16;
    MPS.fixed_int_field "qr" 1;
    MPS.fixed_int_field "opcode" 4;
    MPS.fixed_int_field "authoritative" 1;
    MPS.fixed_int_field "truncation" 1;
    MPS.fixed_int_field "rd" 1;
    MPS.fixed_int_field "ra" 1;
    MPS.fixed_int_field "zv" 3;
    MPS.fixed_int_field "rcode" 4;
    MPS.fixed_int_field "qdcount" 16;
    MPS.fixed_int_field "ancount" 16;
    MPS.fixed_int_field "nscount" 16;
    MPS.fixed_int_field "arcount" 16;
    MPS.string_field "questions" (MPS.size (`mul (`int 4, `var "qdcount")));
  (* TODO *)
  ]
let dns_transitions = PS.empty_transition





let internet_stack_from_ethernet () =
  let s = Protocol_stack.empty_protcol_stack () in
  let add n format transitions =
    Protocol_stack.add_protocol s ~format ~transitions n in
  add ethernet ethernet_format ethernet_transitions;
  add udp      udp_format udp_transitions;
  add tcp      tcp_format tcp_transitions;
  add arp      arp_format arp_transitions;
  add gre      gre_format gre_transitions;
  add dhcp     dhcp_format dhcp_transitions;
  add dns      dns_format dns_transitions;
  Protocol_stack.add_protocol s ipv4 
    ~format:ipv4_format ~transitions:ipv4_transitions
    ~runtime_checks:ipv4_checks;
  s
