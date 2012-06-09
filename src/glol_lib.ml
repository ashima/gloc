(* Copyright (c) 2012 Ashima Arts. All rights reserved.
 * Author: David Sheets
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *)

include Glol_t

let rec normalize_li = function
  (* a's varargs of refs are concatenated *)
  | `List ((`String "a")::rest) ->
    if (List.length rest)=1 then `List ((`String "a")::rest)
    else let hrefs = List.map
           (fun href -> `List [`String "a"; href]) rest in
         `List ((`String "concat")::hrefs)

  (* define's optional value is totally optional *)
  | `List [`String "define"; m] ->
    `List [`String "define"; `List [m; `String "None"]]
  | `List [`String "define"; m; v] ->
    `List [`String "define"; `List [m; `List [`String "Some"; v]]]

  (* concat concatenates its varargs *)
  | `List ((`String "concat")::rest) ->
    `List [`String "concat"; `List (List.map normalize_li rest)]

  (* varargs of rename and export are concatenated inside its scope *)
  | `List (((`String "rename") as f)::arg::rest)
  | `List (((`String "export") as f)::arg::rest) ->
    if (List.length rest)=1
    then `List [f; `List [arg; normalize_li (List.hd rest)]]
    else `List [f; `List [arg;
                          `List [`String "concat";
                                 `List (List.map normalize_li rest)]]]

  | j -> j

let rec denormalize_li = function
  (* Collapse homogeneous concats of hrefs *)
  | `List [`String "concat"; `List rest] ->
    if List.for_all
      (function `List [`String "a"; _] -> true | _ -> false) rest
    then let hrefs = List.fold_right (function
      | `List [`String "a"; href] -> (fun l -> href::l)
      | _ -> (fun l -> l)) rest [] in
         `List ((`String "a")::hrefs)
    else `List ((`String "concat")::(List.map denormalize_li rest))

  (* Remove 'a option constructors *)
  | `List [`String "define";
           `List [m; `String "None"]] -> `List [`String "define"; m]
  | `List [`String "define";
           `List [m; `List [`String "Some"; v]]] ->
    `List [`String "define"; m; v]

  (* Concat under rename|export evaporates *)
  | `List [(`String "rename") as f;
           `List [map; `List [`String "concat"; `List rest]]]
  | `List [(`String "export") as f;
           `List [map; `List [`String "concat"; `List rest]]] ->
    `List (f::map::(List.map denormalize_li rest))

  | j -> j

let map_li map = function
  | `Assoc ajl ->
    let li = List.assoc "li" ajl in
    `Assoc (("li",map li)::(List.remove_assoc "li" ajl))
  | j -> j

let glol_of_string s =
  (* TODO: don't serialize! *)
  let json = map_li normalize_li (Yojson.Safe.from_string s) in
  Glol_j.glol_of_string (Yojson.Safe.to_string ~std:true json)

let string_of_glol glol =
  (* TODO: don't reparse! *)
  let s = Glol_j.string_of_glol glol in
  let json = map_li denormalize_li (Yojson.Safe.from_string s) in
  Yojson.Safe.to_string ~std:true json

