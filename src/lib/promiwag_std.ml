
include Printf
(* include Promiwag_Yaboon_PolyComp.CompAndOveridePoly *)



module Ls = struct

    include ExtList.List
    include ListLabels

    let find_opt f l =
        try Some (List.find f l) with Not_found -> None

end
let (@) = ExtList.List.concat


module Opt = Option

module Str = struct
  (* This module name voluntarily forbids the Str module of the str
     library *)
  include ExtString
  include ExtString.String
  (* include StringLabels *)
end

module Ht = struct
  include ExtHashtbl.Hashtbl
  let find_opt ht key =
    try Some (find ht key) with Not_found -> None
  let value_list ht = Ls.of_enum (values ht)
end

module Io = struct
    include IO
    let open_in f =
        let i = Pervasives.open_in f in
        IO.input_channel i
    let open_out f = 
        IO.output_channel (Pervasives.open_out f)
end


