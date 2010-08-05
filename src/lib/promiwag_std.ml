
include Printf

include Promiwag_Yaboon_PolyComp.CompAndNoPolyPhy



module Ls = struct

    include ExtList.List
    include ListLabels

    let find_opt ~f l =
        try Some (List.find f l) with Not_found -> None

end
let (@) = ExtList.List.append


module Opt = Option

module Str = struct
  (* This module name voluntarily forbids the Str module of the str
     library *)
  include ExtString
  include ExtString.String
  (* include StringLabels *)

  let multi_replace ~str ~sub ~by =
    let s = ref (copy str) in
    let rec loop () =
      let f, ns = replace ~str:!s ~sub ~by in
      if f then (
        s := ns;
        loop ();
      );
    in 
    loop ();
    !s

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

    let with_file_out filename f = 
      let o = open_out filename in
      try let r = f o in close_out o; r with e -> close_out o; raise e

    let with_file_in filename f = 
      let i = open_in filename in
      try let r = f i in close_in i; r with e -> close_in i; raise e

    let with_new_tmp ?(suffix=".tmp") ?(prefix="promiwag_") f =
      let name, o = Filename.open_temp_file prefix suffix in
      let o = output_channel o in
      try let r = f o name in close_out o; r with e -> close_out o; raise e


end


module Environment = struct

  type ('variable, 'value) environment = 
    | Base of ('variable * 'value) list
    | Pushed of ('variable * 'value) list * ('variable, 'value) environment
    (* which is a "never-empty-list of lists". *)

  type ('variable, 'value) t = ('variable, 'value) environment

  let empty = Base []

  let of_list l = Base l

  let push t =
    Pushed ([], t)
      
  let pop = function
    | Pushed (h, t) -> t
    | Base _ -> failwith "Env.pop: There is no environment to pop"
      
  let add t n v =
    match t with
    | Base e -> Base ((n, v) :: e)
    | Pushed (h, t) -> Pushed ((n, v) :: h, t)

  let rec find_opt ?(cmp=(=)) env variable =
    match env with
    | Pushed (h, t) ->
      begin match Ls.find_opt ~f:(fun (n, v) -> cmp n variable) h with
      | Some (_, e) -> Some e
      | None -> find_opt ~cmp t variable
      end
    | Base l ->
      Opt.map snd (Ls.find_opt ~f:(fun (n, v) -> cmp n variable) l)

  let rec iter ~f = function
    | Pushed (h, t) ->
      Ls.iter ~f h; iter ~f t
    | Base l -> Ls.iter ~f l

  let rec metamap ~map ~reduce = function
    | Pushed (h, t) ->
      (reduce (Ls.map ~f:map h)) :: (metamap ~map ~reduce t)
    | Base l -> 
      [reduce (Ls.map ~f:map l)]


end

module FIFO = struct
  type 'a t = 'a Queue.t

  let of_list l =
    let t = Queue.create () in
    Ls.iter l ~f:(fun e ->
      Queue.add e t;
    );
    t

  let push t e = Queue.add e t
  let pop t = try Some (Queue.take t) with Queue.Empty -> None

  let rec consume ~f t =
    match pop t with
    | Some s -> f s ; consume ~f t
    | None -> ()


end



module Unique = struct

  let var_count = ref 0

  let name n = 
    incr var_count;
    Printf.sprintf "%s_%d" n !var_count

  let int_count = ref 0
  let int () =
    incr int_count;
    !int_count

end
  
let ($) f x = f x

let debug_mode: [`stdout | `silent ] ref = ref `stdout

let debug s = 
  match !debug_mode with
  | `stdout -> 
    (if s.[0] <> ' ' then printf "------\n%!"); printf "DEBUG| %s\n%!" s
  | `silent -> ()

let code s = sprintf "[[%s]]" s

