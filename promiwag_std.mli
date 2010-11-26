(*b 
{header|{title|{t|Module Promiwag_std}}{subtitle|The Pervasived Module}}

{section 1|From The Libraries}

Instead of using directly modules from the standard library, extlib,
or others. {i|Promiwag} tries to use renamed modules, enhanced and
chosen in the right library.
  
*)

(*bq
{link http://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html
|Printf} from the standard library.
*)
include module type of Printf

(*bq PolyComp is from
{link http://code.google.com/p/yaboon/
|YaBoon}
(c.f.
{link http://yaboon.googlecode.com/svn/trunk/PolyComp/PolyComp.ml
|PolyComp.ml}).
*)
include module type of Promiwag_Yaboon_PolyComp.CompAndNoPolyPhy

(*bq Common {q|List} module.
It uses
{link http://ocaml-extlib.googlecode.com/svn/doc/apiref/ExtList.List.html
|ExtList.List}
and
{link http://caml.inria.fr/pub/docs/manual-ocaml/libref/ListLabels.html
|ListLabels}.{br}
The {t|List} module is then forbidden.
 *)
module Ls: sig
  include module type of ExtList.List
  include module type of ListLabels
  val find_opt : f:('a -> bool) -> 'a list -> 'a option
end
val (@): 'a list -> 'a list -> 'a list
module List: sig end

(*bq The 
    {link http://ocaml-extlib.googlecode.com/svn/doc/apiref/Option.html
    |Option} module from Extlib. *)
module Opt: module type of Option
module Option: sig end

(*bq This module name voluntarily forbids the Str module of the str
    library.
It uses
{link http://ocaml-extlib.googlecode.com/svn/doc/apiref/ExtString.html
|ExtString}
and
{link http://ocaml-extlib.googlecode.com/svn/doc/apiref/ExtString.String.html
|ExtString.String}.
 *)
module Str: sig
  include module type of ExtString
  include module type of ExtString.String
  (* include module type of StringLabels *)
  val multi_replace : str:string -> sub:string -> by:string -> string
end
module String: sig end

(*bq Hash tables, from
{link http://ocaml-extlib.googlecode.com/svn/doc/apiref/ExtHashtbl.Hashtbl.html
|ExtHashtbl.Hashtbl}. *)
module Ht: sig
  include module type of ExtHashtbl.Hashtbl
  val find_opt : ('a, 'b) t -> 'a -> 'b option
  val value_list : ('a, 'b) t -> 'b list
end
module Hashtbl: sig end

(*bq Input/Output, also from Extlib:
{link http://ocaml-extlib.googlecode.com/svn/doc/apiref/IO.html |IO}. *)
module Io: sig
  include module type of IO
  val open_in : string -> input
  val open_out : string -> unit output
  val stdout : unit output
  val with_file_out : string -> (unit output -> 'a) -> 'a
  val with_file_in : string -> (input -> 'a) -> 'a
  val with_new_tmp :
    ?suffix:string ->
    ?prefix:string -> (unit output -> string -> 'a) -> 'a
end
module IO: sig end

(*bq The 
    {link http://martin.jambon.free.fr/easy-format-doc/Easy_format.html
    |Easy_format} library. *)
module Easy_format: module type of Easy_format


(*b {section 1|New modules} *)

(*bq A Stack of Maps. *)
module Environment : sig
  type ('a, 'b) environment
  type ('a, 'b) t = ('a, 'b) environment
  val empty : ('a, 'b) environment
  val of_list : ('a * 'b) list -> ('a, 'b) environment
  val push : ('a, 'b) environment -> ('a, 'b) environment
  val pop : ('a, 'b) environment -> ('a, 'b) environment
  val add : ('a, 'b) environment -> 'a -> 'b -> ('a, 'b) environment
  val find_opt :
    ?cmp:('a -> 'a -> bool) -> ('a, 'b) environment -> 'a -> 'b option
  val iter : f:('a * 'b -> unit) -> ('a, 'b) environment -> unit
  val metamap :
    map:('a * 'b -> 'c) ->
    reduce:('c list -> 'd) -> ('a, 'b) environment -> 'd list
end

(*bq This is actually a wrapper around the {t|Queue} module of the stdlib. *)
module FIFO : sig
  type 'a t
  val empty: unit -> 'a t
  val of_list : 'a list -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
  val push_list : 'a t -> 'a list -> unit
  val consume : f:('a -> 'b) -> 'a t -> unit
end

(*bq Imperatively returns unique identifiers. *)
module Unique : sig
  val name : string -> string
  val int : unit -> int
end

(*bq Associate integers to strings (for now it is not a hash; it is
     kind of {q|bijective}). *)
module Hash : sig
  val string : string -> int 
end

(*bq Debug printf-ing. *)
val ( $ ) : ('a -> 'b) -> 'a -> 'b
val debug_mode : [ `silent | `stdout ] ref
val debug : string -> unit
val code : string -> string

(*b 
{section 1|Meta}
Document generated with:
{code}
caml2html -nf  -ext "b: brtx" -ext "i: echo ''" promiwag_std.mli
{end}
Or
{code}
mk() {
echo "{bypass endofbigbypass}" > /tmp/caml2html.brtx
sed '/^$/d' promiwag_std.mli | \
 caml2html -nf -inline -body -ext "b: brtx" -ext "i: echo ''" \
  -ext "bq: cat > /tmp/ttt; echo '{~}{b|•} ' > /tmp/uuu ; cat /tmp/ttt >> /tmp/uuu ; echo '' >> /tmp/uuu ; brtx -i /tmp/uuu" | \
 sed -n '1h;1!H;${;g;s/<pre>[\n ]*<\/pre>//g;p;}' \
 >> /tmp/caml2html.brtx
echo "{endofbi""gbypass}" >> /tmp/caml2html.brtx
mv /tmp/caml2html.brtx module_std.brtx
  echo Done
}
{end}
(for the super-multi-line sed see 
{link http://austinmatzko.com/2008/04/26/sed-multi-line-search-and-replace/
|this post})
Or
{code}
caml2html -make-latex-defs /tmp/ttt.tex 
echo "{bypass endofbigbypass}" > /tmp/caml2htmltex.brtx
cat /tmp/ttt.tex >> /tmp/caml2htmltex.brtx
sed '/^$/d' promiwag_std.mli | \
caml2html -body -latex  -ext "b: brtx -latex" -ext "i: echo ''" \
>> /tmp/caml2htmltex.brtx
echo "{endofbi""gbypass}" >> /tmp/caml2htmltex.brtx
brtx -doc -latex -use-package alltt  -i /tmp/caml2htmltex.brtx -o /tmp/ttt.tex

{end}
*)
