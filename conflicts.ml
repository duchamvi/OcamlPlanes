(** Conflict detection module *)

(*module Data = Types*)
open Types
		
let distance = fun a b ->
  (** returns the distance between to points, without changing the unit *)
  let xa = float a.x in
  let ya = float a.y in
  let xb = float b.x in
  let yb = float b.y in
  ((xa -. xb)**2. +. (ya -. yb)**2.)**0.5;;

let assemble = fun conflict_clusters ->
  (** assembles the conclict clusters together *)
  ();;

let compare_tuple = fun elt tuple ->
    match tuple with
      (a,_) -> a = elt
 
let common_beacon = fun p1 p2 ->
  (** renvoie une structure contenant les balises communes *)
  let balises_communes = [||] in
  let rec cbrec = fun balises1 balises2 communes ->
    match balises1 with
      [] -> communes
    | (b,t) :: reste ->
       if List.exists (compare_tuple b) balises2
       then
	 cbrec reste balises2 (Array.append communes [|b|])
       else
	 cbrec reste balises2 communes					 
  in
  cbrec p1.liste_balises p2.liste_balises balises_communes;;
  
let local_detection = fun p1 p2 beacon ->
  (** detects if p1 and p2 are in conflict near the beacon *)
  (* 
     algo :
     on se limite à un groupe de points à une distance < Dref de la balise 
   *)
  (*
     on compare un point avec celui de temps inferieur et celui de temps superieur de l'autre avion
     on renvoie des groupes en conflit (array de array a priori)
   *)
  ();;

let two_planes_detection = fun conflict_clusters p1 p2 ->
  (** Detects a conflict between p1 and p2 and adds it to the conflictclusters *)
  if p1.fl = p2.fl
  then
    let communes = common_beacon p1 p2 in
    if communes != [||]
    then
      Array.iter (local_detection p1 p2) communes;
    else
      ();;
  

let one_plane_detection = fun p planesinactivity conflict_clusters ->
  (** actualises the conflicts clusters with the new plane *)
  Array.iter (two_planes_detection conflict_clusters p) planesinactivity;
 (*
    If a plane shares a FL and a beacon with p, find whether it's in conflict with p, and actualises the conflict clusters
  *)
  assemble conflict_clusters;;

let () =
  Printf.printf "ok"
