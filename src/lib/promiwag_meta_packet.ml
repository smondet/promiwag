open Promiwag_std

module Packet_structure = struct

  type int_operator =
    | Op_add
    | Op_sub
    | Op_mul
    | Op_div

  type size =
    | Size_fixed of int
    | Size_variable of string
    | Size_binary_expression of int_operator * size * size
    | Size_alignment of int * size
    | Size_offset_of of string
    | Size_unknown

  let size_int i = Size_fixed i
  let size_var v = Size_variable v

  let rec parse_size = function
    | `int i -> Size_fixed i
    | `var v -> Size_variable v
    | `add (a, b) -> Size_binary_expression (Op_add, parse_size a, parse_size b)
    | `sub (a, b) -> Size_binary_expression (Op_sub, parse_size a, parse_size b)
    | `mul (a, b) -> Size_binary_expression (Op_mul, parse_size a, parse_size b)
    | `div (a, b) -> Size_binary_expression (Op_div, parse_size a, parse_size b)
    | `align (i, e) -> Size_alignment (i, parse_size e)
    | `offset v -> Size_offset_of v

  let size = parse_size

  type content_type =
    | Type_unsigned_integer of size
    | Type_signed_integer of size
    | Type_little_endian of content_type
    | Type_string of size
        
  let fixed_int ?(unsigned=true) ?(little_endian=false) s = 
    let t = 
      if unsigned then Type_unsigned_integer (Size_fixed s) 
      else Type_signed_integer (Size_fixed s)
    in
    if little_endian then Type_little_endian t else t

  let fixed_string i = Type_string (Size_fixed i)
  let variable_string v = Type_string (Size_variable v)


    (*    type content_value =
          | Value_unsigned_integer of int
          | Value_range of content_value * content_value
    *)  

  type content_item =
    | Item_field of string * content_type
    (* | Item_switch of string * (content_value * content_item list) list *)
    (* | Item_payload of size option * string option *)


  let field name the_type = Item_field (name, the_type)
  let fixed_int_field name size =
    Item_field (name, Type_unsigned_integer (Size_fixed size))
  let string_field name size = Item_field (name,  Type_string size)

    (* let switch name cases = Item_switch (name, cases) *)
    
    (*    let case_uint i fields = (Value_unsigned_integer i, fields)
          let case_range i j fields = 
          (Value_range (Value_unsigned_integer i, Value_unsigned_integer j),
          fields)
    *)

  let payload ?size ?(name="payload") () =
    let actual_size = 
      match size with None -> Size_unknown | Some s -> s in
    Item_field (name, Type_string actual_size)

  type format = content_item list
  type packet = string * format

  let packet_format name format = ((name, format): packet)

end

module Packet_database = struct

  type t = (string, Packet_structure.packet) Hashtbl.t

  let add t (name, packet) = Hashtbl.add t name (name, packet)

  let of_list l = 
    let t = Hashtbl.create 42 in
    List.iter (fun p -> add t p) l;
    t

  let get_format (t:t) name =
    try let _, fmt = Hashtbl.find t name in fmt with
    | Not_found ->
      failwith (sprintf "ERROR: Meta_packet.Packet_database.find \
                   packet format \"%s\" not found" name)

end

module C_parsing = struct

  module C = Promiwag_c_backend.C_LightAST
  module Cons = Promiwag_c_backend.Construct
  open Promiwag_c_backend

  type request = [ `field of string ]

  exception Field_not_found
  exception Max_dependency_depth_reached of int

  module Internal = struct 

    open Packet_structure

    let cmp_str a b = (String.compare a b) = 0
    let to_do s =
      failwith (sprintf "Meta_packet.C_parsing.Internal: \
                           %s: NOT IMPLEMENTED" s)
        
      (*      type int_computation_item =
              | ICI_Constant of int
              | 

              let computation_of_size = function
              | Size_fixed s -> ICI_Constant s
              | Size_variable vof string
              | Size_binary_expression of int_operator * size * size
              | Size_alignment of int * size
              | Size_offset_of of string
              | Size_unknown
      *)

    type dependency =
      | Depend_on_value_of of string
      | Depend_on_offset_of of string
      | Depend_on_unknown

    let dependencies_of_size sz =
      let rec tree_descent deps = function
        | Size_fixed s -> deps
        | Size_variable v -> (Depend_on_value_of v :: deps)
        | Size_binary_expression (op, size_a, size_b) ->
          (tree_descent deps size_a) @ (tree_descent deps size_b)
        | Size_alignment (i, s) -> tree_descent deps s
        | Size_offset_of v -> (Depend_on_offset_of v :: deps)
        | Size_unknown -> (Depend_on_unknown :: deps)
      in
      (tree_descent [] sz)

    type computation_item =
      | Compute_byte_offset of size
      | Compute_bit_offset of size

    let rec computation_of_content_type = function
      | Type_little_endian ct -> computation_of_content_type ct
      | Type_unsigned_integer sz
      | Type_signed_integer sz -> 
        (Compute_bit_offset sz, dependencies_of_size sz)
      | Type_string sz -> 
        (Compute_byte_offset sz, dependencies_of_size sz)

    type final_computation =
      | Finally_get_integer of [`big | `little] * [`signed | `unsigned] * size
      | Finally_get_pointer

    let rec final_computation_of_content_type ?(endianism=`big) = function
      | Type_little_endian ct -> 
        final_computation_of_content_type ~endianism:`little ct
      | Type_unsigned_integer sz ->
        (Finally_get_integer (endianism, `unsigned, sz),
         dependencies_of_size sz)
      | Type_signed_integer sz ->
        (Finally_get_integer (endianism, `unsigned, sz),
         dependencies_of_size sz)
      | Type_string sz -> (Finally_get_pointer, [])

    let find_field_in_packet packet_format field_name =
      let rec f computations field_name = function
        | [] ->
          raise Field_not_found
        | (Item_field (name, tp)) :: l ->
          if cmp_str name field_name then
            (final_computation_of_content_type tp, List.rev computations)
          else
            let comp = computation_of_content_type tp in
            f (comp :: computations) field_name l
      in
      (f [] field_name packet_format)

        
    let resolve_dependencies max_depth packet_format (request:request list) =
      let request_as_dependencies =
        List.map (function `field f -> Depend_on_value_of f) request in
      let computable_variables = Hashtbl.create 42 in
      let die_on_max_depth d =
        if d > max_depth then raise (Max_dependency_depth_reached d); in
      let rec go_deeper ?(depth=0) = function
        | [] -> (* Done! *) ()
        | Depend_on_value_of f :: l ->
          die_on_max_depth depth;
          begin match Hashtbl.find_all computable_variables (`value f) with
          | [] ->
            let (final_comp, final_deps) as final, computes =
              find_field_in_packet packet_format f in
            let deps = 
              final_deps @ 
                (List.flatten (List.map (fun (_, d) -> d) computes)) in
            go_deeper ~depth:(depth + 1) deps;
            Hashtbl.add computable_variables (`value f) (final, computes, deps);
            go_deeper ~depth:(depth + 1) l;
          | one :: [] -> ()
          | more -> 
            failwith (sprintf "field %s added %d times to \
                                 computable_variables as `value"
                        f (List.length more))
          end
        | Depend_on_offset_of f :: l -> 
          die_on_max_depth depth;
          begin match Hashtbl.find_all computable_variables (`offset f) with
          | [] ->
            let (final_comp, final_deps) as final, computes =
              find_field_in_packet packet_format f in
            let deps = 
              (List.flatten (List.map (fun (_, d) -> d) computes)) in
            go_deeper ~depth:(depth + 1) deps;
            Hashtbl.add computable_variables (`offset f) (final, computes, deps);
            go_deeper ~depth:(depth + 1) l;
          | one :: [] -> ()
          | more -> 
            failwith (sprintf "field %s added %d times to \
                                 computable_variables as `offset"
                        f (List.length more))
          end
        | Depend_on_unknown :: _ ->
          failwith "Depends on something Unknown/Uncomputable"
      in
      go_deeper request_as_dependencies;
      computable_variables

    type compiled_chunk = 
      | Compiled_value_expression of Typed_expression.t
      | Compiled_offset_expression of Typed_expression.t
      | Compiled_value_variable of Variable.t * Typed_expression.t
      | Compiled_offset_variable of Variable.t * Typed_expression.t

    let caml_op_of_size_op = function
      | Op_add -> (+)
      | Op_sub -> (-)
      | Op_mul -> ( * )
      | Op_div -> (/)

    let rec propagate_constants_in_size = function
      | Size_fixed s -> Size_fixed s
      | Size_variable v -> Size_variable v
      | Size_binary_expression (op, size_a, size_b) ->
        let propagated_a = propagate_constants_in_size size_a in
        let propagated_b = propagate_constants_in_size size_b in
        begin match propagated_a, propagated_b with
        | Size_fixed a, Size_fixed b ->
          Size_fixed (caml_op_of_size_op op a b)
        | sa, sb ->
          Size_binary_expression (op, sa, sb)
        end
      | Size_alignment (i, s) ->
        begin match propagate_constants_in_size s with
        | Size_fixed s -> Size_fixed (s + (s mod i))
        | other -> other
        end
      | Size_offset_of v -> Size_offset_of v
      | Size_unknown -> Size_unknown

    let compile_computations computations = 
      let byte_offset, bit_offset =
        let f (prev_byte, prev_bit) = function
          | Compute_byte_offset s ->
            (Size_binary_expression (Op_add, prev_byte, s), prev_bit)
          | Compute_bit_offset s ->
            (prev_byte, Size_binary_expression (Op_add, prev_bit, s)) in
        Ls.fold_left ~f computations ~init:(Size_fixed 0, Size_fixed 0) in
      let propagated_byte_ofs = propagate_constants_in_size byte_offset in
      begin match propagate_constants_in_size bit_offset with
          | Size_fixed bits as propagated_bit_ofs -> 
            if bits / 8 = 0 then 
              (propagated_byte_ofs, propagated_bit_ofs)
            else
              begin match propagated_byte_ofs with
              | Size_fixed bytes ->
                (Size_fixed (bytes + (bits / 8)), Size_fixed (bits mod 8))
              | any ->   (propagated_byte_ofs, propagated_bit_ofs)
              end
          | bits ->
            to_do "Non-constant bit ofset"
      end

    let compile_offset already_compiled final computes name =
      assert (final = (Finally_get_pointer, []));
      printf "Offset of %s\n" name

    let compile_value already_compiled final compute name =
      printf "Value of %s\n" name

    let compile computables_ht request =
      let compiled_chunks = Ht.create 42 in
      Ht.iter (fun f  (final, computes, deps) ->
        match f with
        | `offset s -> compile_offset compiled_chunks final computes s
        | `value s -> compile_value compiled_chunks final computes s
      ) computables_ht;

      let f = function 
        | `field field ->
          begin match Ht.find_opt compiled_chunks field with
          | Some (Compiled_value_expression e) -> e
          | Some (Compiled_value_variable (v, e)) -> Variable.typed_expression v
          | Some (Compiled_offset_expression _)  
          | None
          | Some (Compiled_offset_variable _) ->
            failwith (sprintf "Field %s should have been compiled!" field)
          end
      in
      Ls.map request ~f

  end


  let informed_block
      ~(packet_format:Packet_structure.format)
      ~(request_list: request list)
      ~(packet_expression: Typed_expression.t)
      (* ?(packet_size: Typed_expression.t option)  TODO: maybe something else? *)
      ~(make_user_block: Typed_expression.t list -> C.block) =
      (*
        - for each field 'compile' it to: (computation, dependencies)
        - compute dependencies (Hashtbl / Lazy style)
        - if something does not seem computable -> fail
        - else:
        - for dependencies which are needed more than once create local variable
        - generate C code for every computation
      *)
    
    let computables = 
      Internal.resolve_dependencies 42 packet_format request_list in

    let compiled_c = Internal.compile computables request_list in
    ignore compiled_c;

    let user_decl, user_stms = make_user_block [] in
    (user_decl, user_stms)



end


(*


  let rec compile_final_computation
  ?(network_order=true) bit_offset expression = 
  function
  | `get_integer size ->
  begin match size + bit_offset with
  | 0 -> `literal_int 0
  | s when 1 <= s && s <= 8 ->
  let aligned =
  `binary (`bin_shr, expression,
  `literal_int (8 - bit_offset - size)) in
  `binary (`bin_band, aligned, Cons.ones_int_literal size)
  | s when 9 <= s && s <= 32 ->
  let the_uint =
  `cast (`unsigned_int, `call (`variable "ntohl", [expression])) in
  let aligned =
  `binary (`bin_shr, the_uint, `literal_int (32 - size - bit_offset))
  in
  `binary (`bin_band, aligned, Cons.ones_int_literal size)
  | _ ->
  failwith "compile_final_computation: size + bit_offset > 32;\
  NOT IMPLEMENTED"
  end
  | `get_pointer ->
  if bit_offset <> 0 then
  failwith
  "Error: Attempting to get a pointer with non-zero bit-offset."
  else
  (`unary (`unary_addrof, expression))
  | `little_endian fc ->
  compile_final_computation 
  ~network_order:false bit_offset expression fc


    let compile_computation c_expression (final_one, computation) =
      let byte_offset = ref 0 in
      let bit_offset = ref 0 in
      List.iter (function
        | `byte_offset o -> byte_offset := !byte_offset + o
        | `bit_offset o -> bit_offset := !bit_offset + o
      ) computation;
      let more_bytes = !bit_offset / 8 in
      byte_offset := !byte_offset + more_bytes;
      bit_offset := !bit_offset - (more_bytes * 8);
      let expr_as_buffer = `cast (`pointer `unsigned_char, c_expression) in
      let the_byte = `array_index (expr_as_buffer, `literal_int !byte_offset) in
      (* let expr_as_buffer = `cast (`pointer `unsigned_char, expression) in *)
      (compile_final_computation !bit_offset the_byte final_one : C.expression)


    let get_fields ~paths ~packet ~format_db =
      let computations =
        List.map (find_computation ~packet ~format_db) paths in
      let expressions = 
        let expr =
          let _, te = packet in
          Typed_expression.expression te in
        List.map (compile_computation expr) computations in

      let statements =
        List.map2 (fun e (`field (_, p)) -> `assignment (`variable p, e))
          expressions paths in
      Construct.block ~statements ()

  end
*)
