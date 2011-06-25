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


type platform = {
  endianism: [ `little | `big ];
  word_size: int;
}

let endianism p = p.endianism

module C = struct

  let native_uint p = `unsigned_long
  let uint_native = native_uint
  let size_of_native_uint p = p.word_size

  let uint8 p = `unsigned_char
  let uint16 p = `unsigned_short

  let uint32 p = `unsigned_int

  let uint64 p = `unsigned_long_long

  let fitted_uint ?(fail=failwith) p i =
    if 1 <= i && i <= 8 then (uint8 p, 8)
    else if   9 <= i && i <= 16 then (uint16 p, 16)
    else if  17 <= i && i <= 32 then (uint32 p, 32)
    else if  33 <= i && i <= 64 then (uint64 p, 64)
    else 
      fail (sprintf "Promiwag_platform.C.fitted_int: too long integer: %d" i)

end

let default = {endianism = `little; word_size = Sys.word_size}

