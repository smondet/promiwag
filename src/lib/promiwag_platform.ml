open Promiwag_std


type platform = {
  endianism: [ `little | `big ];
  word_size: int;
}


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

