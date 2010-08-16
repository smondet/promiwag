open Promiwag_std

module C = struct


  let basic_loop ~call =
sprintf "%s%s%s"
"

#include <stdio.h>
#include <stdlib.h>
#include <pcap.h>

/* Pcap's callback handler: */
void the_handler(unsigned char * user,
   const struct pcap_pkthdr * packet_header, 
   const u_char * packet) {
"
  (call "packet" "packet_header->caplen")
"

}

int main(int argc, char * * argv) {

  char error_buffer[PCAP_ERRBUF_SIZE];
  pcap_t * pcap_source = NULL;
  
  
  if (argc != 3) {
    printf(\"usage: %s [file|device] <name>\\n\", argv[0]);
    exit(2);
  }
  
  if (strncmp(argv[1], \"file\", 5) == 0) {
    pcap_source = pcap_open_offline((argv)[2], error_buffer);
  } else if ( strncmp(argv[1], \"device\", 7) == 0 ||  strncmp(argv[1], \"dev\", 4) == 0) {
    pcap_source = pcap_open_live(argv[2], 65536, 0, 0, error_buffer);
  }
  
  if ((pcap_source) == (NULL)) {
    printf(\"libPCAP ERROR: pcap_open_offline: %s\\n\", error_buffer);
    exit(3);
  }
  
  (void)(pcap_loop(pcap_source, -1, the_handler, NULL));
  (void)(pcap_close(pcap_source));
  return 0;
}

"


end


module OCaml = struct

  let compilation_string =
    "ocamlfind ocamlopt -package pcap -linkpkg pcap.cmxa"

  let basic_loop ~call =
sprintf
"
open Pcap
let () =
  let callbkfun (s:string) (h:pcap_pkthdr) (t:string) =
    %s
  in
  if Array.length Sys.argv <> 3 then
    failwith \"usage: ./a.out [file|device] <name>\";
  let opened =
    match Sys.argv.(1) with
    | \"file\" ->  pcap_open_offline Sys.argv.(2)
    | \"device\" | \"dev\" ->
      pcap_open_live Sys.argv.(2) 65535 0 0
    | _ ->
      failwith \"usage: ./a.out [file|device] <name>\";
  in
  ignore (pcap_loop opened (-1) callbkfun \"\")
"
(call "t" "h.caplen")

end


