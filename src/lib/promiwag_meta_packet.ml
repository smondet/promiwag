open Promiwag_std

module Packet_structure = struct

  type int_operator =
    | Op_add
    | Op_sub
    | Op_mul
    | Op_div

  let caml_op_of_size_op = function
    | Op_add -> (+) | Op_sub -> (-) | Op_mul -> ( * ) | Op_div -> (/)
  let string_of_size_op = function
    | Op_add -> "+" | Op_sub -> "-" | Op_mul -> "*" | Op_div -> "/"

  type size =
    | Size_fixed of int
    | Size_variable of string
    | Size_binary_expression of int_operator * size * size
    | Size_alignment of int * size
    | Size_offset_of of string
    | Size_unknown

  let size_int i = Size_fixed i
  let size_var v = Size_variable v

  let rec string_of_size = function
    | Size_fixed o -> sprintf "%d" o
    | Size_variable v -> v
    | Size_binary_expression (op, size_a, size_b) ->
      sprintf "(%s %s %s)" (string_of_size size_a)
        (string_of_size_op op) (string_of_size size_b)
    | Size_alignment (i, s) -> sprintf "align%d(%s)" i (string_of_size s)
    | Size_offset_of o -> sprintf "offset_of(%s)" o
    | Size_unknown ->  "[Unknown]"

  let rec parse_size = function
    | `int i -> Size_fixed i
    | `var v -> Size_variable v
    | `add (a, b) -> Size_binary_expression (Op_add, parse_size a, parse_size b)
    | `sub (a, b) -> Size_binary_expression (Op_sub, parse_size a, parse_size b)
    | `mul (a, b) -> Size_binary_expression (Op_mul, parse_size a, parse_size b)
    | `div (a, b) -> Size_binary_expression (Op_div, parse_size a, parse_size b)
    | `align64 e -> Size_alignment (8, parse_size e)
    | `align32 e -> Size_alignment (4, parse_size e)
    | `align16 e -> Size_alignment (2, parse_size e)
    | `align8 e -> Size_alignment  (1, parse_size e)
    | `offset v -> Size_offset_of v
    | `size s -> s

  let size = parse_size

  type content_type =
    | Type_unsigned_integer of size
    | Type_signed_integer of size
    | Type_little_endian of content_type
    | Type_string of size

  let rec string_of_content_type ?(little_endian=false) ct =
    let lestr = if little_endian then " LE" else "" in
    match ct with
    | Type_unsigned_integer s -> sprintf "uint of %s bits%s" (string_of_size s) lestr
    | Type_signed_integer s -> sprintf "sint of %s bits%s" (string_of_size s) lestr
    | Type_little_endian ct -> string_of_content_type ~little_endian:true ct
    | Type_string s -> sprintf "string of %s Bytes%s" (string_of_size s) lestr

  let fixed_int ?(unsigned=true) ?(little_endian=false) s = 
    let t = 
      if unsigned then Type_unsigned_integer (Size_fixed s) 
      else Type_signed_integer (Size_fixed s)
    in
    if little_endian then Type_little_endian t else t

  let fixed_string i = Type_string (Size_fixed i)
  let variable_string v = Type_string (Size_variable v)

  type content_item =
    | Item_field of string * content_type

  let field name the_type = Item_field (name, the_type)
  let fixed_int_field name size =
    Item_field (name, Type_unsigned_integer (Size_fixed size))
  let string_field name size = Item_field (name,  Type_string size)


  let payload ?size ?(name="payload") () =
    let actual_size = 
      match size with None -> Size_unknown | Some s -> s in
    Item_field (name, Type_string actual_size)

  type format = content_item list

  let packet_format format = (format: format)

  type packet_value = [ 
  | `value of string
  | `pointer of string
  | `offset of string
  | `size of string
  ]

end

module To_string = struct

  open Packet_structure

  let size = string_of_size 

  let rec content_type =
    let plural = function Size_fixed 1 -> "" | _ -> "s" in
    function
    | Type_unsigned_integer s -> sprintf "unsigned int of %s bit%s" (size s) (plural s)
    | Type_signed_integer   s -> sprintf "signed int of %s bit%s" (size s) (plural s)
    | Type_little_endian    t -> sprintf "%s (little-endian)" (content_type t)
    | Type_string           s -> sprintf "string of %s byte%s" (size s) (plural s)

  let content_item ?(suffix="") = function
    | Item_field (s, t) ->
      sprintf "%s: %s%s" s (content_type t) suffix

  let format ?(sep="\n") ?(suffix=";") fmt =
    Str.concat sep (Ls.map fmt ~f:(content_item ~suffix))


end


module Parser_generator = struct

  (*
    a request of offset should be a difference from current packet
    a request of pointer should be a C-like pointer
    a value is for now an unsigned integer of minimal size
  *)
  type request = Packet_structure.packet_value
      

  module Stage_1 = struct 
      
    open Packet_structure

    let cmp_str a b = (Str.compare a b) = 0

    let error_prefix = "Meta_packet.Parser_generator.Stage_1"
    let to_do s =
      failwith (sprintf "%s; %s: NOT IMPLEMENTED" error_prefix s)
    let fail s =
      failwith (sprintf "%s: ERROR %s" error_prefix s)

    type dependency =
      | Depend_on_value_of of string
      | Depend_on_offset_of of string
      | Depend_on_pointer_to of string
      | Depend_on_size_of of string
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

    let string_of_dependency = function
      | Depend_on_value_of o -> sprintf "(value-of: %s)" o
      | Depend_on_offset_of o -> sprintf "(offset-of: %s)" o
      | Depend_on_pointer_to o -> sprintf "(pointer-to: %s)" o
      | Depend_on_size_of o -> sprintf "(size-of: %s)" o
      | Depend_on_unknown -> "(depends-on-unknown)"
        
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
          
    let string_of_computation_item = function
      | Compute_byte_offset o -> sprintf "(byte-offset: %s)" (string_of_size o)
      | Compute_bit_offset o -> sprintf "(bit-offset: %s)" (string_of_size o)


    type final_computation =
      | Finally_get_integer of [`big | `little] * [`signed | `unsigned] * size
      | Finally_get_string of size

    let string_of_final_computation = function
      | Finally_get_integer (`big, `signed, s) -> 
        sprintf "(get-integer %s, signed, big endian)" (string_of_size s)
      | Finally_get_integer (`big, `unsigned, s) -> 
        sprintf "(get-integer %s, unsigned, big endian)" (string_of_size s)
      | Finally_get_integer (`little, `signed, s) -> 
        sprintf "(get-integer %s, signed, little endian)" (string_of_size s)
      | Finally_get_integer (`little, `unsigned, s) -> 
        sprintf "(get-integer %s, unsigned, little endian)" (string_of_size s)
      | Finally_get_string size -> 
        sprintf "(get-string %s)" (string_of_size size)

    let rec final_computation_of_content_type ?(endianism=`big) = function
      | Type_little_endian ct -> 
        final_computation_of_content_type ~endianism:`little ct
      | Type_unsigned_integer sz ->
        (Finally_get_integer (endianism, `unsigned, sz),
         dependencies_of_size sz)
      | Type_signed_integer sz ->
        (Finally_get_integer (endianism, `unsigned, sz),
         dependencies_of_size sz)
      | Type_string sz -> (Finally_get_string sz, dependencies_of_size sz)

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
      | Size_alignment (al, s) ->
        begin match propagate_constants_in_size s with
        | Size_fixed i -> Size_fixed (i + (i mod al))
        | sz -> Size_alignment (al, sz)
        end
      | Size_offset_of v -> Size_offset_of v
      | Size_unknown -> Size_unknown

    let aggregate_computations computations = 
      let byte_offset, bit_offset =
        let f (prev_byte, prev_bit) = function
          | Compute_byte_offset s ->
            (Size_binary_expression (Op_add, prev_byte, s), prev_bit)
          | Compute_bit_offset s ->
            (prev_byte, Size_binary_expression (Op_add, prev_bit, s)) in
        Ls.fold_left ~f computations ~init:(Size_fixed 0, Size_fixed 0) in
      let total_byte_offset, in_byte_bit_offset =
        size (`add (`size byte_offset, `div (`size bit_offset, `int 8))),
        size (`sub (`size bit_offset,
                    `mul (`int 8, `div (`size bit_offset, `int 8))))
      in
      let propagated_byte_ofs, propagated_bit_ofs =
        propagate_constants_in_size total_byte_offset,
        propagate_constants_in_size in_byte_bit_offset in
      (propagated_byte_ofs, propagated_bit_ofs)

    let find_field_in_packet packet_format field_name =
      let rec f computations dependencies field_name = function
        | [] ->
          fail (sprintf "Field %s not found" field_name)
        | (Item_field (name, tp)) :: l ->
          if cmp_str name field_name then
            let fcomp, fdeps = final_computation_of_content_type tp in
            (fcomp, fdeps, Ls.rev computations, Ls.rev dependencies)
          else
            let comp, dep = computation_of_content_type tp in
            f (comp :: computations) (dep :: dependencies) field_name l
      in
      (f [] [] field_name packet_format)

    type needer = [`request | `other  of stage_1_compiled_expression]
    and need = [`value of needer | `pointer of needer
               | `offset of needer | `size of needer]
    and stage_1_compiled_expression = {  (* TODO find a better name... *)
      s1_field: string;
      s1_byte_offset: size;
      s1_bit_offset: size;
      s1_final: final_computation;
      s1_dependencies: dependency list;
      mutable s1_needed_as_by: need list;
    }

    let explode_stage_1_compiled_expression ce =
      (ce.s1_field, 
       ce.s1_byte_offset,
       ce.s1_bit_offset,
       ce.s1_final,
       ce.s1_dependencies,
       ce.s1_needed_as_by)

    let stage_1_compile_field needed_as_by packet_format field_name =
      let final_comp, final_deps, comps, deps =
        find_field_in_packet packet_format field_name in
      let total_deps = final_deps @ (Ls.flatten deps) in
      let byte_ofs, bit_ofs = aggregate_computations comps in
      {s1_field = field_name; s1_byte_offset = byte_ofs; s1_bit_offset = bit_ofs;
       s1_final = final_comp; s1_dependencies = total_deps;
       s1_needed_as_by = [needed_as_by]; }

    let stage_1_compile_dependency packet_format (needed_by:needer) = function
      | Depend_on_value_of f ->
        stage_1_compile_field (`value needed_by) packet_format f
      | Depend_on_offset_of f ->
        stage_1_compile_field (`offset needed_by) packet_format f
      | Depend_on_pointer_to f ->
        stage_1_compile_field (`pointer needed_by) packet_format f
      | Depend_on_size_of f ->
        stage_1_compile_field (`size needed_by) packet_format f
      | Depend_on_unknown ->
        fail "stage_1_compile_dependency: should not be here"

    let dependencies_of_request: request list -> dependency list =
      Ls.map ~f:(function 
        | `value f -> Depend_on_value_of f
        | `offset f -> Depend_on_offset_of f
        | `pointer f -> Depend_on_pointer_to f
        | `size f -> Depend_on_size_of f)

    type result = {
      packet_format: format;
      request_list: request list;
      compiled_expressions: stage_1_compiled_expression list;
    }

    let compile_with_dependencies
        ?(max_depth=42) ~packet_format (request:request list) =
      let first_dependencies = dependencies_of_request request in
      let stage_compiled_things = ref [] in
      let die_on_max_depth depender d =
        if d > max_depth then 
          fail (sprintf "Stage 1: Maximal dependency reached for %s"
                  (match depender with 
                  | `request -> "request (this should never happen)"
                  | `other c -> c.s1_field)) in
      let rec go_deeper depth depender = function
        | [] -> (* Done! *) ()
        | (Depend_on_value_of fname as d) :: l
        | (Depend_on_pointer_to fname as d) :: l
        | (Depend_on_size_of fname as d) :: l
        | (Depend_on_offset_of fname as d) :: l ->
          die_on_max_depth depender depth;
          let f ct = cmp_str ct.s1_field fname in
          begin match Ls.find_all !stage_compiled_things ~f with
          | [] ->
            let compiled =
              stage_1_compile_dependency packet_format depender d in
            go_deeper (depth + 1) (`other compiled) compiled.s1_dependencies;
            stage_compiled_things := compiled :: !stage_compiled_things;
          | one :: [] ->
            begin match d with
            | Depend_on_value_of _ ->
              one.s1_needed_as_by <- (`value depender) :: one.s1_needed_as_by;
            | Depend_on_offset_of _ -> 
              one.s1_needed_as_by <- (`offset depender) :: one.s1_needed_as_by;
            | Depend_on_pointer_to _ -> 
              one.s1_needed_as_by <- (`pointer depender) :: one.s1_needed_as_by;
            | Depend_on_size_of _ -> 
              one.s1_needed_as_by <- (`size depender) :: one.s1_needed_as_by;
            | Depend_on_unknown -> 
              fail "compile_with_dependencies: Depend_on_unknown"
            end;
          | more -> 
            fail (sprintf "field %s added %d times to stage_compiled_things"
                    fname (Ls.length more))
          end;
          go_deeper depth depender l;
        | Depend_on_unknown :: l ->
          (* We do nothing for now. Failure may appear on Stage_2, or not, 
             depending on what is requested. *)
          go_deeper depth depender l;
      in
      go_deeper 0 `request first_dependencies;
      {packet_format = packet_format; 
       request_list = request;
       compiled_expressions = Ls.rev !stage_compiled_things}

    let string_of_stage_1_compiled_expression
        ?(before="") ?(sep_parens=" ") ce =
      sprintf "%s[\"%s\" (byte_offset: %s, bit_offset: %s)%s\
              (final: %s)%s(dependencies: [%s])%s(%s)]"
        before ce.s1_field
        (string_of_size ce.s1_byte_offset) 
        (string_of_size ce.s1_bit_offset) sep_parens
        (string_of_final_computation ce.s1_final) sep_parens
        (Str.concat "; " (Ls.map string_of_dependency ce.s1_dependencies))
        sep_parens
        (Str.concat "; " (Ls.map (function
          | `value `request -> "requested value" 
          | `offset `request -> "requested offset" 
          | `pointer `request -> "requested pointer" 
          | `size `request -> "requested size" 
          | `value (`other c) -> sprintf "value needed by %s" c.s1_field
          | `offset (`other c) -> sprintf "offset needed by %s" c.s1_field
          | `pointer (`other c) -> sprintf "pointer needed by %s" c.s1_field
          | `size (`other c) -> sprintf "size needed by %s" c.s1_field
         ) ce.s1_needed_as_by))

    let string_of_stage_1_compilation_ht
        ?(sep_items="\n") ?(before="") ?(sep_parens="\n  ") ht =
      let l = ref [] in
      Ls.iter (fun s1 -> 
        let expr = 
          string_of_stage_1_compiled_expression ~before ~sep_parens s1 in
        l := expr :: !l;
      ) ht;
      Str.concat sep_items (Ls.rev !l)

    let dump s1 =
      sprintf "{Stage 1 for [%s]:\n%s\n}"
        (Str.concat ", " (Ls.map (function
          | `value f -> sprintf "field %s" f
          | `pointer f -> sprintf "pointer  %s" f
          | `offset f -> sprintf "offset of %s" f
          | `size f -> sprintf "size of %s" f) s1.request_list))
        (string_of_stage_1_compilation_ht
           ~before:"  " ~sep_parens:"\n    " s1.compiled_expressions)

  end

  module Stage_2_stiel = struct
    
    open Packet_structure

    module Stiel_types = Promiwag_stiel.Definition
    open Promiwag_stiel.Standard_renaming

    type variable_creation_preference =
      [`minimalistically (** Only when absolutely needed, this may mean 
                             duplication of computations. *)
      | `as_needed (** Normal behaviour: create variables when
                       absolutely needed, to save intermediary
                       results, and avoid duplication of
                       computations. *)
      | `for_all (** Create variables for everything that has to be
                     compiled, in particular, all the typed
                     expressions passed to [make_user_block] will be
                     wrappings of variable names. *)
      ]

    type size_error_handler =
        STIEL.typed_expression -> STIEL.typed_expression -> STIEL.statement

    exception Compilation_error of string

    (* Internal representations: *)

    type compiled =
      | Stiel_variable of
          Stiel_types.typed_variable * Stiel_types.typed_expression
      | Stiel_expression of Stiel_types.typed_expression
      | Stiel_not_compiled

    type compiled_entity = {
      value:   compiled;
      offset:  compiled;
      pointer: compiled;
      size:    compiled;
      stage_1_expression: Stage_1.stage_1_compiled_expression;
      buffer_access: Stiel_types.typed_expression * int;
    }

    type compiler = {
      stage_1: Stage_1.result;
      compiled_entities: (string, compiled_entity) Ht.t;
      variable_creation_preference: variable_creation_preference;
      mutable location: string option;
    }
        
    let error_prefix = "Meta_packet.Parser_generator.Stage_2_stiel"
      
    let to_do s =
      failwith (sprintf "%s; %s: NOT IMPLEMENTED" error_prefix s)

    let fail compiler s =
      let field_str = 
        match compiler.location with | None -> "" | Some s -> s in
      failwith (sprintf "%s%s: COMPILER-ERROR %s" error_prefix field_str s)

    let error compiler s =
      let field_str =
        match compiler.location with | None -> "" | Some s -> s in
      raise (Compilation_error (sprintf "%s%s: %s" error_prefix field_str s))


    let set_location_field compiler field =
      compiler.location <- Some (sprintf "{Compiling field  %s]" field)
        
    let get_stiel_expression compiler cc =
      match cc with
      | Stiel_variable (v, ev) -> Var.expression v
      | Stiel_expression e -> e
      | _ -> fail compiler "asking for a non-compiled part of entity"
        
    let get_stiel_dependency compiler needed_as name =
      match Ht.find_opt compiler.compiled_entities name with
      | None -> fail compiler "asking for a non-compiled entitie"
      | Some ce ->
        begin match needed_as with
        | `value ->   get_stiel_expression compiler ce.value
        | `offset ->  get_stiel_expression compiler ce.offset 
        | `pointer -> get_stiel_expression compiler ce.pointer
        | `size ->    get_stiel_expression compiler ce.size
        end

    let expr_op_of_size_op = function
      | Op_add -> Expr.add
      | Op_sub -> Expr.sub
      | Op_mul -> Expr.mul
      | Op_div -> Expr.div

    let rec compile_size compiler needed_as = function
      | Size_fixed s -> Expr.unat s
      | Size_variable v ->
        if needed_as = `size then 
          get_stiel_dependency compiler `value v
        else
          get_stiel_dependency compiler needed_as v
      | Size_binary_expression (op, size_a, size_b) ->
        (* debug$ sprintf "in size: %s" *)
          (* (Stiel_to_str.typed_expression (compile_size compiler needed_as size_a)); *)
        (expr_op_of_size_op op)
          (Expr.to_unat (compile_size compiler needed_as size_a))
          (Expr.to_unat (compile_size compiler needed_as size_b))
      | Size_alignment (i, s) ->
        let c = compile_size compiler needed_as s in
        (* debug$ sprintf "in size: %s" *)
          (* (Stiel_to_str.typed_expression c); *)
        Expr.add c (Expr.modulo c (Expr.unat i))
      | Size_offset_of v ->
        get_stiel_dependency compiler `offset v
      | Size_unknown -> 
        fail compiler "Trying to compile unknown size"

    let offset compiler byte_offset =
      (compile_size compiler `value byte_offset)
        
    let pointer_in_packet compiler packet byte_offset =
      let int_offset = compile_size compiler `value byte_offset in
      Expr.offset packet int_offset

    let size  compiler final =
      let size =
        match final with
        | Stage_1.Finally_get_integer (_, _, sz) -> sz
        | Stage_1.Finally_get_string sz -> 
          Size_binary_expression (Op_mul, sz, Size_fixed 8) in
      let expression = 
        compile_size compiler `size
          (Stage_1.propagate_constants_in_size size) in
      expression
        
    let value_type_of_size compiler = function
      | Size_fixed i -> Expr.fitted_uint_type_and_size i
      | _ ->
        (* Emit warning ??? *)
        error compiler "Cannot type value non-fixed size"

    let value  compiler pointer bit_offset final =
      let (endianism, signedism, sz) =
        (* [`big | `little] * [`signed | `unsigned] * size *)
        match final with
        | Stage_1.Finally_get_integer (e,s,c) -> (e,s,c)
        | Stage_1.Finally_get_string _ ->
          error compiler "Cannot compile the 'value' of a string/payload" in
      let stiel_type, type_size =
        match signedism with
        | `unsigned -> value_type_of_size compiler sz 
        | `signed -> to_do "Signed integers" in
      let get_integer e = Expr.ufitted_at ~how:endianism ~size:type_size e in
      let the_bits =
        match bit_offset, sz with
        | Size_fixed bofs, Size_fixed psz ->
          begin match bofs + psz with
          | 0 -> Expr.unat 0
          | s when 1 <= s && s <= 32 ->
            (* let pointer_expr = Stiel.buffer_expr pointer in *)
            let value_at_pointer = get_integer pointer in
            (* debug$ sprintf "value_at_pointer: %s" *)
              (* (Stiel_to_str.typed_expression value_at_pointer); *)
            let aligned =
              Expr.bin_shr value_at_pointer
                (Expr.unat (type_size - bofs - psz)) in
            Expr.bin_and aligned (Expr.ufitted ~size:type_size (Expr.ones psz))
          | s ->
            to_do (sprintf "Integer's offset + size = %d (>= 32)" s)
          end
        | _, _ ->
          to_do "Integer's offset or size not resolved to constants"
      in
      (the_bits, type_size)
      
    let compile_expression compiler packet expression = 
      let (field, 
           byte_offset,
           bit_offset,
           final,
           dependencies,
           needed_as_by) =
        Stage_1.explode_stage_1_compiled_expression expression in
      set_location_field compiler field;
      let compile_value_to, compile_offset_to, 
        compile_pointer_to, compile_size_to =
        let value_needness, offset_needness, pointer_needness, size_needness =
          Ls.fold_left ~f:(fun (v, o, p, s) nab ->
            match nab with
            | `value _ -> (1 + v, o, p, s)
            | `offset _ -> (v, o + 1, p, s)
            | `size _ -> (v, o, p, s + 1)
            | `pointer _ -> (v, o, p + 1, s))
            ~init:(0, 0, 0, 0) needed_as_by in
        let comp n = match n, compiler.variable_creation_preference with
          | 0, _ -> `none
          | _, `for_all -> `variable
          | 1, `as_needed -> `expression
          | n, `minimalistically -> `expression
          | n, `as_needed -> `variable
        in
        (comp value_needness, comp offset_needness,
         comp pointer_needness, comp size_needness) in
      let offset = offset compiler byte_offset in
      let pointer = pointer_in_packet compiler packet byte_offset in

      let compiled_offset =
        match compile_offset_to with
        | `none -> Stiel_not_compiled
        | `expression -> Stiel_expression offset
        | `variable ->
          let stiel_var =
            Var.typed_variable (sprintf "offset_of_%s" field)
              (Var.kind_of_expr offset) in
          Stiel_variable (stiel_var, offset)
      in
      let compiled_pointer =
        match compile_pointer_to with
        | `none -> Stiel_not_compiled
        | `expression -> Stiel_expression pointer
        | `variable ->
          let stiel_var =
            Var.typed_variable (sprintf "pointer_to_%s" field)
              (Var.kind_of_expr pointer) in
          Stiel_variable (stiel_var, pointer)
      in
      let compiled_size =
        match compile_size_to with
        | `none -> Stiel_not_compiled
        | `expression -> Stiel_expression (size compiler final)
        | `variable ->
          let te = size compiler final in
          let stiel_var =
            Var.typed_variable (sprintf "size_of_%s" field)
              (Var.kind_of_expr te) in
          Stiel_variable (stiel_var, te)
      in
      (* Things related to the size are a bit redundant here: *)
      let compiled_value, access_size =
        match compile_value_to with
        | `none -> (Stiel_not_compiled, 0)
        | `expression ->
          let value, size = value compiler pointer bit_offset final in
          (Stiel_expression value, size)
        | `variable ->
          let value, size = value compiler pointer bit_offset final in
          let stiel_var =
            Var.typed_variable 
              (sprintf "value_of_%s" field) (Var.kind_of_expr value) in
          (Stiel_variable (stiel_var, value), size)
      in
      let entity =
        {value = compiled_value;
         offset = compiled_offset;
         pointer = compiled_pointer;
         size = compiled_size;
         stage_1_expression = expression;
         buffer_access = (offset, access_size);} in
      Ht.add compiler.compiled_entities field entity;
      entity

    let generate_constant_size_checks compiler entities
        size_expression escape_block =
      let maximal_constant_access=
        Ls.fold_left ~init:(-1) entities ~f:(fun m e ->
          match Expr.int (fst e.buffer_access), snd e.buffer_access with
          | (Stiel_types.Int_expr_literal i64, s) ->
            let i = Int64.to_int i64 in
            max m (i + (s / 8))
          | _ -> m) in
      debug$ sprintf "Maximal: %d" maximal_constant_access;
      let stop_now, size_is_literal =
        match Expr.int size_expression with
        | Stiel_types.Int_expr_literal i64 ->
          ((Int64.compare i64 (Int64.of_int maximal_constant_access)) <= 0,
           true)
        | _ -> (false, false)
      in
      if stop_now then
        error compiler
          (sprintf  "Found statically that the packet size is inferior \
                     to the maximal buffer access: %s < %d"
             (Stiel_to_str.typed_expression size_expression)
             maximal_constant_access);
      let statements =
        let mca = (Expr.unat maximal_constant_access) in
        if (maximal_constant_access <> -1) && (not size_is_literal) then
          [ Do.cmt "Checking maximal_constant_access against packet size.";
            Do.conditional ~statement_then:(escape_block size_expression mca)
              (Expr.le size_expression mca)]
        else [] in
      statements



    let generate_size_check compiler entity size_expr_opt escape_block =
      let buffer_access =
        Expr.int (fst entity.buffer_access), snd entity.buffer_access in
      match size_expr_opt, buffer_access with
      | None, _  
      | _ , (Stiel_types.Int_expr_literal _, _) -> []
      | Some size_expr, (offset_expr, type_bits) ->
debug$ sprintf " (fst entity.buffer_access): %s"
  (Stiel_to_str.typed_expression (fst entity.buffer_access));
        let bufacc_expr = 
          Expr.add (Expr.to_unat (fst entity.buffer_access))
            (Expr.unat (type_bits / 8)) in
        let statements = 
          [Do.cmt (sprintf 
                        "Checking buffer-access of field %s against packet size."
                        entity.stage_1_expression.Stage_1.s1_field);
           Do.conditional (Expr.le size_expr bufacc_expr)
             ~statement_then:(escape_block size_expr bufacc_expr); ] in
        statements
        

    let get_variables_and_size_checks
        compiler entities size_expression_option escape_block  = 
      let statements = ref [] in
      Ls.iter (fun c ->
        let check_stms = 
          generate_size_check compiler c size_expression_option escape_block in
        statements := !statements @ check_stms;
        let add = function 
          | Stiel_variable (v, e) -> 
            statements := !statements @ [Var.declare v ; Var.assign v e];
          | _ -> ()
        in
        add c.value;
        add c.offset;
        add c.pointer;
        add c.size;
      ) entities;
      !statements

    (* 'Exported' function: *)

    let informed_block ~stage_1
        ?(create_variables:variable_creation_preference=`as_needed)
        ~(packet:Stiel_types.typed_expression)
        ?(packet_size:Stiel_types.typed_expression option)
        ?(on_size_error:size_error_handler option)
        ~(make_user_block: Stiel_types.typed_expression list -> 
          Stiel_types.statement list) () =

      let compiler = 
        {stage_1 = stage_1; variable_creation_preference = create_variables;
         compiled_entities = Ht.create 42; location = None} in
     
      let ordered_entities =
        Ls.map compiler.stage_1.Stage_1.compiled_expressions 
          ~f:(compile_expression compiler packet) in

      let size_error_block =
        Opt.default (fun _ _ -> Do.cmt "Default size_error_block.") 
          on_size_error  in

      compiler.location <- 
        Some "{Generating constant size/buffer-access checks}";
      let constant_size_checks =
        match packet_size with
        | None -> []
        | Some var ->
          generate_constant_size_checks
            compiler ordered_entities var size_error_block in

      compiler.location <- Some "{Generating declarations, assignments, and \
                                  size/buffer-access checks}";
      let declarations_assignments_size_checks = 
        get_variables_and_size_checks compiler ordered_entities 
          packet_size size_error_block in
      
      compiler.location <- Some "{Generating user_expressions}";
      let user_expressions =
        Ls.map compiler.stage_1.Stage_1.request_list
          ~f:(function
            | `value f   -> get_stiel_dependency compiler `value f
            | `offset f  -> get_stiel_dependency compiler `offset f
            | `pointer f -> get_stiel_dependency compiler `pointer f
            | `size f    -> get_stiel_dependency compiler `size f) in
      let user_statements = make_user_block user_expressions in
      (constant_size_checks 
       @ declarations_assignments_size_checks
       @ user_statements)
  end


end



