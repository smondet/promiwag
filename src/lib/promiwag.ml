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

open Promiwag_std

module C_backend = Promiwag_c_backend

module Meta_packet = Promiwag_meta_packet

module Pcap = Promiwag_pcap_boilerplate

module Standard_protocols = Promiwag_standard_protocols

module Platform = Promiwag_platform

module Stiel = struct
  include Promiwag_stiel
  include Promiwag_stiel_backends
end

module Protocol_stack = Promiwag_protocol_stack
