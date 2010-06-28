open Promiwag_std


type platform = {
  endianism: [ `little | `big ];

}


module C = struct

  let native_uint p = `unsigned_int

  let int8 p = `unsigned_char

  let int32 p = `unsigned_int

end

