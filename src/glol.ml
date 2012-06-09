(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

(* Let's link some glo! *)
open Glo_lib

(* We'll need some maps from strings to structures. *)
module M = Map.Make(String)

(* A link path segment is either local, hyper, or environmental. *)
type seg = Local of string | Hyper of string | Env of string

(* Each unit of SL source has a path and a (mostly) meaningless glo unit
   index that determines link precedence. *)
type path = seg list
type unit_addr = seg list * int
type addr = Path of path | Unit of unit_addr

type nu = int -> int * string

(* A named glo is a serialized glo with a tag. *)
type 'a nglo = 'a * nu glo

(* An index is a projection of a namespace onto a map type. *)
type index = { sym : unit_addr nglo M.t; mac : unit_addr nglo M.t;
               shadow : unit_addr nglo list M.t }

exception MissingSymbol of string * string
exception MissingMacro of string * string
exception CircularDependency of string list
exception SymbolConflict of string * string * string * string
exception UnknownBehavior of string * string
exception UnknownGloVersion of string * version
exception BlockedMacro of string * string

let string_of_path path =
  let rec build a = function
    | (Local s)::pr -> build (Printf.sprintf "/%s%s" s a) pr
    | (Hyper s)::pr -> build ("/<"^s^">"^a) pr
    | (Env s)::pr -> build ("/["^s^"]"^a) pr
    | [] -> a
  in build "" path

let string_of_ua (path,un) =
  Printf.sprintf "%s#n=%d" (string_of_path path) un

let string_of_error = function
  | CircularDependency uasl -> List.fold_left
    (fun s uas -> s ^ uas ^ "\n")
    "Circular dependency linking:\n" uasl
  | MissingMacro (uas,mn) ->
    Printf.sprintf "%s requires macro '%s' which cannot be found.\n"
      uas mn
  | MissingSymbol (uas,mn) ->
    Printf.sprintf "%s requires symbol '%s' which cannot be found.\n"
      uas mn
  | SymbolConflict (ssym,csym,suas,cuas) ->
    Printf.sprintf "%s provides '%s' but exposes '%s' which conflicts with %s\n"
      suas ssym csym cuas
  | UnknownBehavior (uas,b) ->
    Printf.sprintf "%s uses unknown extension behavior '%s'.\n"
      uas b
  | UnknownGloVersion (path,(maj,min,rev)) ->
    Printf.sprintf "%s declares unsupported version %d.%d.%d.\n"
      path maj min rev
  | exn -> raise exn

module type World = sig
  type url
  type resource = Glom of string glo glom | Glol of Glol_t.glol

  exception GetError of string * string
  exception ParseError of string * string

  val url : string -> url
  val string : url -> string

  val get : url -> (string * resource) Lwt.t

  val resolve_ref : url -> url -> url
  val is_absolute_url : url -> bool
end

module Make(W : World) = struct

(* A context contains ambient state like base URI,
   module lenses, scopes, and continuations. *)
type resolver = string -> (string * W.resource) Lwt.t

type ctxt = {
  rel_base: bool; (* Is the base relative to the environment? *)
  base: string; (* The base URI for relative URI resolution *)
  src: path; (* The audit path of the resource providing this context *)
  cache: (string * Glol_t.glol) list; (* The nearest bundled transmission cache *)

  lens: ctxt -> path nglo -> path nglo; (* Contextual module maps *)

  (* The general stream continuation *)
  k: (ctxt -> resolver -> (ctxt * path nglo list) Lwt.t) option;
}

type stream = ctxt
type sequence = Seq of unit_addr nglo list | Index of index (* TODO *)

let glol_version = (1,1,0)

(* To satisfy the various symbol dependency constraints, we zip through the
   link list tracking Required symbols, Top symbols satisfied, and Bottom
   symbols exposed by the above teeth. *)
type tooth = { rsym : string list;   rmac : string list;
               tsym : unit_addr M.t; tmac : unit_addr M.t;
               bsym : unit_addr M.t; bmac : unit_addr M.t;
               addr : unit_addr nglo }
    (* Bottom * Top *)
type zipper = tooth list * tooth list

(* Prepare the packaged source for string concatenation. *)
let armor meta (url,linkmap,fs_ct) opmac s =
  (* Replace special symbols in line directives to satisfy linkmap *)
  let intpatt = Re_str.regexp "GLOC_\\([0-9]+\\)" in
  let offset s =
    let fn = int_of_string (Re_str.string_after (Re_str.matched_string s) 5) in
    let anno = try "/* "^url^(List.assoc (string_of_int fn) linkmap)^" */"
    with Not_found -> ""
    in (string_of_int (fn + fs_ct))^anno
  in
  let s = Re_str.global_substitute intpatt offset s in
  let head = match meta with None -> "" | Some meta ->
    let field name (entity,href) = "// "^name^": "^entity^" <"^href^">\n" in
    let authors = List.fold_left
      (fun s link -> s^(field "Author" link))
      "" meta.author in
    let license = match meta.license with None -> ""
      | Some link -> field "License" link in
    let library = match meta.library with None -> ""
      | Some link -> field "Library" link in
    let version = match meta.version with None -> ""
      | Some (v,url) ->
        "// Version: "^(string_of_version v)^" <"^url^">\n" in
    let build = match meta.build with None -> ""
      | Some link -> field "Build" link in
    let (year,(holder,url)) = meta.copyright in
      "// Copyright "^(string_of_int year)^" "^holder^" <"^url^"> "
      ^"All rights reserved.\n"^license^authors^library^version^build
  in let s = head^s in
    (* Undefine local open macros so they do not leak *)
    List.fold_left (fun s mac -> s^"\n#undef "^mac) s opmac

(* Search a glo for a unit satisfying the supplied predicate p. *)
let search p glo =
  let rec loop = function
    | [] -> None
    | (i,u)::r -> if p u then Some i else loop r
  in loop (Array.to_list (Array.mapi (fun i u -> (i,u)) glo.units))

let get s reluri =
  if List.mem_assoc reluri s.cache
  then Lwt.return (reluri, W.Glol (List.assoc reluri s.cache))
  else let uri = W.resolve_ref (W.url s.base) (W.url reluri) in
       let url = W.string uri in
       if List.mem_assoc url  s.cache
       then Lwt.return (url, W.Glol (List.assoc url s.cache))
       else W.get uri

let rec pump ctxt c x a =
  match ctxt.k with
    | None -> x a
    | Some k -> lwt (nc,resl) = k ctxt (get ctxt) in
                c (pump nc c x) a
                  (List.map (fun (addr,glo) -> addr@ctxt.src,glo) resl)

(* Linearly search for an exported macro. *)
let rec satisfy_mac addr macro stream =
  pump stream
    (fun pump () ->
      let rec loop = function
        | (addr,glo)::rest ->
          begin match search (fun u -> List.mem macro u.outmac) glo with
            | Some i -> Lwt.return ((addr,i),glo)
            | None -> loop rest
          end
        | [] -> pump ()
      in loop)
    (fun () -> raise (MissingMacro (string_of_ua addr, macro)))
    ()

(* Linearly search for an exported symbol or macro. *)
let rec satisfy_sym addr sym stream =
  pump stream
    (fun pump () ->
      let rec loop = function
        | (addr,glo)::rest ->
          begin match search
              (fun u -> (List.mem sym u.outmac) || (List.mem sym u.outsym)) glo
            with
              | Some i -> Lwt.return ((addr,i),glo)
              | None -> loop rest
          end
        | [] -> pump ()
      in loop)
    (fun () -> raise (MissingSymbol (string_of_ua addr, sym)))
    ()

let map_of_list v = List.fold_left (fun m n -> M.add n v m) M.empty

(* Construct the constraint data structure from a named glo unit. *)
let tooth nglo =
  let u = (snd nglo).units.(snd (fst nglo)) in
  { rsym=u.insym; (* to be satisfied *)
    rmac=u.inmac;
    tsym=M.empty; (* locally satisfied *)
    tmac=M.empty;
    bsym=map_of_list (fst nglo) u.outsym; (* cumulatively satisfies *)
    bmac=map_of_list (fst nglo) u.outmac;
    addr=nglo }

let has_addr addr_a ({addr=addr_b}) = (fst addr_a) = (fst addr_b)
(* Advertize prior units to later units. *)
let mergeb b = function [] -> b
  | {bsym; bmac}::r -> {b with bsym=M.fold M.add b.bsym bsym;
                          bmac=M.fold M.add b.bmac bmac}
(* Satisfy a macro dependency with an already included dependency. *)
let connect_mac b n addr =
  {b with rmac=List.filter (fun m -> not (m=n)) b.rmac; tmac=M.add n addr b.tmac}
(* Satisfy a symbol dependency with an already included dependency. *)
let connect_sym b n addr =
  {b with rsym=List.filter (fun m -> not (m=n)) b.rsym; tsym=M.add n addr b.tsym}
(* Search for an already included macro. *)
let provided_mac n = function [] -> None
  | t::r -> try let addr = M.find n t.bmac in Some addr
    with Not_found -> None
(* Search for an already included symbol. *)
let provided_sym n = function [] -> None
  | t::r -> try let addr = M.find n t.bmac in Some addr
    with Not_found -> try let addr = M.find n t.bsym in Some addr
    with Not_found -> None
(* Given a tooth and a prefix tooth list, find conflicts. *)
let conflicted a = function [] -> None
  | t::r -> let keys = List.map fst ((M.bindings a.bsym)@(M.bindings a.bmac)) in
      begin match List.filter (fun (sym,addr) -> List.mem sym keys)
        ((M.bindings t.bsym)@(M.bindings t.bmac))
      with [] -> None | b::_ -> Some b end

(* Check for symbol conflicts *)
let check_conflicts n tooth (b,t) = match conflicted tooth t with
  | Some (sym, caddr) ->
    raise (SymbolConflict (n,sym,
                           string_of_ua (fst tooth.addr),
                           string_of_ua caddr))
  | None -> ()

(* Check for circular dependency *)
let check_circdep addr (b,t) =
  if List.exists (has_addr addr) b
  then raise (CircularDependency
                (List.map (fun {addr} -> string_of_ua (fst addr)) b))

let seq_of_index reqsyms i =
  let rec sat = function
    | ([],t) -> ([],t)
    | (({rmac=[]; rsym=[]} as b)::r,t) ->
      sat (r,(mergeb b t)::t)
    | (({rmac=n::_} as b)::r,t) ->
      begin match provided_mac n t with
        | Some addr -> sat ((connect_mac b n addr)::r,t)
        | None -> let addr = M.find n i.mac in (* TODO: error handling *)
                  check_circdep addr (b::r,t);
                  let tooth = tooth addr in
                  check_conflicts n tooth (b::r,t);
                  sat (tooth::(connect_mac b n (fst addr))::r,t)
      end
    | (({rmac=[]; rsym=n::_} as b)::r,t) ->
      begin match provided_sym n t with
        | Some addr -> sat ((connect_sym b n addr)::r,t)
        | None -> let addr = M.find n i.sym in (* TODO: error handling *)
                  check_circdep addr (b::r,t);
                  let tooth = tooth addr in
                  check_conflicts n tooth (b::r,t);
                  sat (tooth::(connect_mac b n (fst addr))::r,t)
      end
  in let addrs = List.map (fun s -> tooth (M.find s i.sym)) reqsyms in
     Seq (List.rev_map (fun tooth -> tooth.addr) (snd (sat (addrs, []))))

(* Build a list of units with internal requirements satisfied. *)
let rec satisfy_zipper stream = function
    (* At the bottom of the zipper, we must be done. *)
  | ([],t) -> Lwt.return ([],t)
    (* Without further needs from below, we're ready to descend. *)
  | (({rmac=[]; rsym=[]} as b)::r,t) ->
      satisfy_zipper stream (r,(mergeb b t)::t)
    (* Subsequent units require a macro. *)
  | (({rmac=n::_} as b)::r,t) ->
      begin match provided_mac n t with
        | Some addr -> satisfy_zipper stream ((connect_mac b n addr)::r,t)
        | None -> lwt addr = satisfy_mac (fst b.addr) n stream in
                  check_circdep addr (b::r,t);
                  let tooth = tooth addr in
                  check_conflicts n tooth (b::r,t);
                  satisfy_zipper stream (tooth::(connect_mac b n (fst addr))::r,t)
      end
    (* Subsequent units require a symbol. *)
  | (({rmac=[]; rsym=n::_} as b)::r,t) ->
      begin match provided_sym n t with
        | Some addr -> satisfy_zipper stream ((connect_sym b n addr)::r,t)
        | None -> lwt addr = satisfy_sym (fst b.addr) n stream in
            check_circdep addr (b::r,t);
            let tooth = tooth addr in
              check_conflicts n tooth (b::r,t);
              satisfy_zipper stream (tooth::(connect_sym b n (fst addr))::r,t)
      end

(* Generate a glo store from a list of required symbols and a stream. *)
let satisfy required stream =
  let required = match required with [] -> ["main"] | l -> l in
  lwt addrs = Lwt_list.fold_left_s (fun al sym ->
    lwt addr = satisfy_sym ([Env ("-u "^sym)],0) sym stream in
    if List.mem addr al then Lwt.return al else Lwt.return (addr::al)
  ) [] (List.rev required) in
  lwt z = satisfy_zipper stream (List.rev_map tooth addrs,[]) in
  Lwt.return (Seq (List.rev_map (fun tooth -> tooth.addr) (snd z)))

(* Generate the shader preamble. *)
let preamble glol =
  (* Precedence for conflicting behaviors *)
  let b_order = ["require"; "warn"; "enable"; "disable"] in
  let rec b_max x y = function [] -> x
    | b::r when b=x -> x
    | b::r when b=y -> y
    | _::r -> b_max x y r
  in
  let ext_merge addr m (ext,b) =
    match (try Some (M.find ext m) with Not_found -> None) with
      | None -> if List.mem b b_order
        then M.add ext b m
        else raise (UnknownBehavior (string_of_ua addr, b))
      | Some pb -> M.add ext (b_max b pb b_order) m
  in
  let ext_decl ext b = "#extension "^ext^" : "^b^"\n" in
  let ext_segment m =
    (try ext_decl "all" (M.find "all" m) with Not_found -> "")
    ^(M.fold (fun ext b s -> if ext="all" then s else s^(ext_decl ext b)) m "")
  in
  let (version,pragmas,exts) = List.fold_left
    (fun (version,pragmas,exts) ((name,i),glo) ->
      let u = try glo.units.(i) with _ -> raise (Failure "preamble") in
      (begin match u.vdir,version with
        | None,v -> v
        | Some uv,None -> Some uv
        | Some uv,Some pv -> Some (max uv pv)
       end,
        List.fold_left (fun p s -> p^s^"\n") pragmas u.pdir,
        List.fold_left (ext_merge (name,i)) exts u.edir
      )
    ) (None,"",M.empty) glol
  in (match version with Some v -> "#version "^(string_of_int v)^"\n"
        | None -> "")^pragmas^(ext_segment exts)

let wrap_glo_with_nu glo =
  {glo with units=Array.map
      (fun u -> {u with source=(fun nu -> nu, u.source)}) glo.units}

let parse_path s =
  let split s = Re_str.split_delim (Re_str.regexp_string "/") s in
  let env = ((fun s -> Local s),']') in
  let hyper = ((fun s -> Hyper s),'>') in
  let rec local left = function
    | x::xs -> if x.[0] = '<'
      then paren hyper left ((Re_str.string_after x 1)::xs)
      else if x.[0] = '['
      then paren env left ((Re_str.string_after x 1)::xs)
      else local ((Local x)::left) xs
    | [] -> List.rev left
  and paren (construct,closechar) left = function
    | x::x'::xs ->
      let len = String.length x in
      if x.[len - 1] = closechar
      then local ((construct (Re_str.string_before x (len - 1)))::left) (x'::xs)
      else paren (construct,closechar) left ((x^"/"^x')::xs)
    | x::[] ->
      let len = String.length x in
      if x.[len - 1] = closechar
      then local ((construct (Re_str.string_before x (len - 1)))::left) []
      else local ((construct x)::left) []
    | [] -> local left []
  in local [] (split s)

(* Flatten a glom into an association list and remove non-(glo|glol) elements *)
let flatten_ prefix glom =
  let rec descend prefix l = function
    | (n,Leaf glo) -> (((parse_path n)@prefix,wrap_glo_with_nu glo))::l
    | (n,Glom glom) -> List.fold_left
      (fun l p -> descend ((parse_path n)@prefix) l p) l glom
    | (n,Source _) | (n,Other _) -> l
  in match glom with
    | Glom glom -> List.rev
      (List.fold_left (descend prefix) [] glom)
    | Leaf glo -> [prefix, wrap_glo_with_nu glo]
    | Source _ | Other _ -> []
let flatten prefix glom = flatten_ (parse_path prefix) glom

(* Nest a glo alist back into a glom *)
let nest glo_alist =
  let rec group prefix prev = function
    | ((_::[],_)::_) as glom -> (glom, List.rev prev)
    | (x::xs,glo)::r when x=prefix -> group prefix ((xs,glo)::prev) r
    | glom -> (glom,List.rev prev)
  in
  let rec nest prev = function
    | [] -> Glom (List.rev prev)
    | (([],glo)::r) -> nest (("", glo)::prev) r
    | ((x::[],glo)::r) -> nest ((x, glo)::prev) r
    | ((x::_,_)::_) as r ->
      let (rest,g) = group x [] r in
      nest ((x, nest [] g)::prev) rest
  in let split s = Re_str.split (Re_str.regexp_string "/") s in
     nest []
       (List.map (fun (n,glo) -> (split n, glo)) glo_alist)

let empty_u =
  {pdir=[]; edir=[]; vdir=None;
   inu=[]; outu=[]; ina=[]; outa=[]; vary=[]; insym=[]; outsym=[];
   inmac=[]; opmac=[]; Glo_lib.bmac=[]; outmac=[]; source=""}

let ctxt src k =
  let proto = { rel_base=true; base=""; src=[];
                cache=[]; lens=(fun _ x -> x); k } in
  if W.is_absolute_url (W.url src)
  then { proto with rel_base=false; base=src; src=parse_path ("<"^src^">") }
  else let p = parse_path src in match p with
    | (Local _)::_ | (Env _)::_ | [] -> proto
    | (Hyper s)::_ ->
      { proto with rel_base=not (W.is_absolute_url (W.url s)); base=s; src=p }

let stream ctxt = ctxt

let macro_name m =
  let fn_patt = Re_str.regexp "\\([^(]+\\)\\(([^)]+)\\)?" in
  let _ = Re_str.search_forward fn_patt m 0 in
  Re_str.matched_group 1 m

let glo_of_macro_define src m omv =
  {glo=glo_version;
   target=("webgl",(1,0,0));
   meta=None;
   units=[|
     { empty_u with outmac=[macro_name m];
       source=(fun nu -> match omv with
         | Some mv -> nu,"#line 0 GLOC_0\n#define "^m^" "^mv^"\n"
         | None -> nu,"#line 0 GLOC_0\n#define "^m^"\n");
     }|];
   linkmap=["0",string_of_ua (src,0)]
  }

(* Create a stream from a macro definition. *)
let define_ m omv = fun ctxt resolve ->
  Lwt.return (ctxt, [ctxt.src, glo_of_macro_define ctxt.src m omv])
let define ?(src="[glol.define]") m omv =
  stream (ctxt src (Some (define_ m omv)))

(* Concatenate two streams. *)
let rec concat_ sa sb = fun ctxt resolve ->
  match sa.k, sb.k with
    | None, None -> Lwt.return ({ctxt with k=None}, [])
    | Some ak, None -> ak ctxt resolve
    | None, Some bk -> bk ctxt resolve
    | Some ak, Some bk ->
      lwt nc,nglo_l = ak ctxt resolve in
      Lwt.return ({nc with k=Some (concat_ nc sb)},nglo_l)
let concat ?(src="[glol.concat]") sa sb = stream (ctxt src (Some (concat_ sa sb)))

(* Add a glo lens to a stream. *)
let lens s m = fun c r -> s {c with lens=fun c glo -> m c (c.lens c glo)} r

type ig = {i: int; g: unit -> ig}

let nextfn glo =
  let rec next n () =
    if List.mem_assoc (string_of_int n) glo.linkmap
    then next (n+1) ()
    else {i=n;g=next (n+1)}
  in next 0

let add_lex_coord (l,f) unit =
  let ldir = Printf.sprintf "#line %d GLOC_%d\n" l f in
  let source = (fun nu -> let nu, s = unit.source nu in nu, ldir^s) in
  {unit with source}

(* The rename lens. *)
let rename_ remapl ctxt (path,glo) =
  let {i=auditfn} = nextfn glo () in
  let units, audit = List.fold_right (fun (i,u) (us,audit) ->
    let u, a = List.fold_left (fun (u,a) (src,dst) ->
      if List.mem src u.outmac
      then raise (BlockedMacro (string_of_ua (path,i),src))
      else if (List.mem src u.inmac)
          || (List.mem src u.insym)
          || (List.mem src u.outsym)
      then if (List.mem src u.opmac)
          || (List.mem src u.Glo_lib.bmac)
        then raise (BlockedMacro (string_of_ua (path,i),src))
        else let r = List.map (fun s -> if s=src then dst else s) in
             {u with inu=r u.inu; outu=r u.outu;
               ina=r u.ina; outa=r u.outa;
               vary=r u.vary; inmac=r u.inmac;
               insym=r u.insym; outsym=r u.outsym;
               opmac=src::u.opmac;
               source=(fun nu ->
                 let nu, source = u.source nu in
                 nu,"#define "^src^" "^dst^"\n"^source)},
             true
      else u, a
    ) (u,false) remapl
    in if a then ((add_lex_coord (0,auditfn) u)::us,true) else (u::us, audit)
  ) (Array.to_list (Array.mapi (fun i u -> (i,u)) glo.units)) ([],false)
  in if audit
    then (path,{glo with units=Array.of_list units;
      linkmap=(string_of_int auditfn,string_of_path ctxt.src)::glo.linkmap})
    else (path,glo)

let rename ?(src="[glol.rename]") remapl s = match s.k with
  | None -> stream (ctxt src None)
  | Some k -> stream (ctxt src (Some (lens k (rename_ remapl))))

let hide unit sym =
  if List.mem sym unit.outmac
  then {unit with opmac=sym::unit.opmac;
    outmac=List.filter (fun s -> s <> sym) unit.outmac}
  else if List.mem sym unit.inmac || List.mem sym unit.insym
  then unit
  else {unit with outsym=List.filter (fun s -> s <> sym) unit.outsym;
    source=(fun i ->
      let j, s = unit.source (i+1) in
      (j, "#define "^sym^" GLOC_NU_"^(string_of_int i)^"\n"^s)
    )}

(* The export lens. *)
let export_ ifacel ctxt (path,glo) =
  let {i=auditfn} = nextfn glo () in
  let units, audit = List.fold_right (fun (i,u) (us,audit) ->
    let is_private m = not (List.mem m ifacel) in
    let hidden = List.filter is_private (u.outsym@u.outmac) in
    let audit = audit || (List.length (List.filter is_private u.outsym)) <> 0 in
    let u = List.fold_left hide u hidden in
    ((if audit then (add_lex_coord (0,auditfn) u) else u)::us, audit)
  ) (Array.to_list (Array.mapi (fun i u -> (i,u)) glo.units)) ([],false)
  in if audit
    then (path,{glo with units=Array.of_list units;
      linkmap=(string_of_int auditfn,string_of_path ctxt.src)::glo.linkmap})
    else (path,glo)

let export ?(src="[glol.export]") ifacel s = match s.k with
  | None -> stream (ctxt src None)
  | Some k -> stream (ctxt src (Some (lens k (export_ ifacel))))

(* Create a link object from a glom. *)
let stream_of_glom_ glom = fun ctxt resolve ->
  Lwt.return (ctxt, flatten_ ctxt.src glom)

(* Create a ctxt continuation function from a glolli. *)
let rec li_ = function
  | `A u -> a_ u
  | `Define (m, omv) -> define_ m omv
  | `Concat [] -> (fun ctxt resolve -> Lwt.return (ctxt, []))
  | `Concat [expr] -> li_ expr
  | `Concat (e::es) -> (fun ctxt resolve -> concat_
    {ctxt with k=Some (li_ e)}
    {ctxt with k=Some (li_ (`Concat es))}
    ctxt resolve)
  | `Rename (remapl, expr) -> lens (li_ expr) (rename_ remapl)
  | `Export (ifacel, expr) -> lens (li_ expr) (export_ ifacel)
and li ?(src="[glol.li]") expr = stream (ctxt src (Some (li_ expr)))
and stream_of_glol_ glol = fun ctxt resolve ->
  let ctxt = {ctxt with cache=glol.Glol_t.cache} in
  match glol.Glol_t.glom, glol.Glol_t.li with
    | None, None -> Lwt.return ({ctxt with k=None}, [])
    | None, Some expr -> Lwt.return ({ctxt with k=Some (li_ expr)}, [])
    | Some glom, _ -> Lwt.return (ctxt, flatten_ ctxt.src (glom_of_json glom))
(* Create a stream from a search a(href). *)
and a_ u = fun ctxt resolve ->
  lwt src, res = resolve u in
  let p = (Hyper src)::ctxt.src in
  let ctxt = {ctxt with src=p; base=src;
    rel_base=not (W.is_absolute_url (W.url src)); k=None} in
  (match res with
    | W.Glom glom -> stream_of_glom_ glom
    | W.Glol glol -> stream_of_glol_ glol) ctxt resolve

let stream_of_glom ?(src="[glol.stream_of_glom]") glom =
  stream (ctxt src (Some (stream_of_glom_ glom)))
let stream_of_glol ?(src="[glol.stream_of_glol]") glol =
  stream (ctxt src (Some (stream_of_glol_ glol)))
let a ?(src="[glol.a]") u = stream (ctxt src (Some (a_ u)))

let rev_nu_glom resl =
  let resl = snd
    (List.fold_left
       (fun (i,l) (p,nglo) ->
         let i, units = Array.fold_left
           (fun (i,l) u ->
             let i,source = u.source i in
             (i,{u with source}::l)
           ) (i,[]) nglo.units in
         let units = Array.of_list (List.rev units) in
         (i,((string_of_path p), Leaf ({nglo with units}))::l))
       (0,[]) resl
    )
  in nest resl

(* TODO: aggregate glo? *)
let glom_of_sequence = function
  | Seq ual ->
    let resl = List.rev_map
      (fun ((p,un), nglo) -> (p,{nglo with units=[|nglo.units.(un)|]}))
      ual in
    rev_nu_glom resl
  | Index i -> Glom [] (* TODO: do *)

let glom_of_stream stream = pump stream
  (fun pump a nglo_l -> pump (nglo_l@a))
  (fun a -> Lwt.return (rev_nu_glom a))
  []

let make_shadow symbol smap idx =
  if M.mem symbol smap
  then let shadows = try M.find symbol idx.shadow with Not_found -> [] in
       let shadow_stack = (M.find symbol smap)::shadows in
       {idx with shadow=M.add symbol shadow_stack idx.shadow}
  else idx

let project_unit ua idx =
  let (name,i),glo = ua in
  let idx = List.fold_left
    (fun idx m ->
      {(make_shadow m idx.sym (make_shadow m idx.mac idx)) with
        sym=M.add m ua idx.sym; mac=M.add m ua idx.mac}
    ) idx (List.rev (glo.units.(i).outmac)) in
  List.fold_left
    (fun idx s -> {(make_shadow s idx.sym idx) with sym=M.add s ua idx.sym}
    ) idx (List.rev (glo.units.(i).outsym))

let project_nglo idx (name,glo) =
  Array.fold_right project_unit
    (Array.mapi (fun i _ -> ((name,i),glo)) glo.units) idx

let empty_index = {sym=M.empty; mac=M.empty; shadow=M.empty}

let index s =
  let rec collect acc_l s = match s.k with
    | None -> Lwt.return acc_l
    | Some k -> lwt ns, nglo_l = k s (get s) in
      collect (List.rev_append nglo_l acc_l) ns
  in
  let rec occlude idx = function
    | [] -> Lwt.return idx
    | nglo::nglo_l -> occlude (project_nglo idx nglo) nglo_l
  in lwt nglo_l = collect [] s in occlude empty_index nglo_l

let rec shadow = function
  | Seq resl -> shadow (Index (List.fold_right project_unit resl empty_index))
  | Index i -> List.map fst (M.bindings i.shadow)
let meta = function (* TODO: dedupe? *)
  | Seq resl -> List.fold_right
    (fun (_,glo) l -> match glo.meta with
      | Some m -> m::l
      | None -> l
    ) resl []
  | Index i -> List.fold_right
    (fun (_,(_,glo)) l -> match glo.meta with
      | Some m -> m::l
      | None -> l
    ) ((M.bindings i.sym)@(M.bindings i.mac)) []
let rec linkmap = function (* TODO: dedupe? *)
  | Seq resl ->
    (*List.fold_right
      (fun (_,glo) al -> if List.mem glo al then al else glo::al)
      resl []*)
    List.rev (* TODO: relative linkmap resolve? *)
      (snd (List.fold_left
              (fun (o,linkmap) ((name,_),glo) ->
                let sup = List.fold_left
                  (fun sup (is,_) -> max sup (int_of_string is))
                  0 glo.linkmap
                in (o+sup+1, (List.map
                                (fun (is,p) -> (int_of_string is, p))
                                glo.linkmap)@linkmap))
              (0,[]) resl))
  | Index i -> linkmap (seq_of_index ["main"] i)

(* TODO: use *)
let is_supported (addr,glo) =
  let support = [|[||];[|true|];[|true|]|] in
  let (maj,min,_) = glo.glo in
  if try not support.(maj).(min) with Invalid_argument _ -> true
    then raise (UnknownGloVersion (addr, glo.glo))

(* Produce a string representing a valid SL program given a list of required
   symbols and a search list. *)
let link prologue store =
  let rec get_seq = function
    | Seq resl -> resl (* List.map (fun (p,g) -> string_of_ua p, g) resl*)
    | Index i -> get_seq (seq_of_index ["main"] i)
  in let seq = get_seq store in
     fst begin List.fold_left
         begin fun (src,(pname,o,n)) ((name,u),glo) ->
           let sup = List.fold_left
             (fun sup (is,_) -> max sup (int_of_string is))
             0 glo.linkmap
           in
           let meta = if name=pname then None else glo.meta in
           let u = try glo.units.(u) with _ -> raise (Failure "u") in
           let unit_begin = if name=pname || pname=[]
             then "" else "// End: Copyright\n"
           in let (n,s) = u.source n in
              (src^unit_begin
               ^(armor meta (string_of_path name, glo.linkmap, o)
                   u.opmac s)^"\n",
               (name,o+sup+1,n))
         end ((preamble seq)^prologue,([],0,0)) seq
     end
  end
