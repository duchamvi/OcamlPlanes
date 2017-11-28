(** Conflict detection module *)


(*module Data = Types*)
open Types

       
(* Reglages *)
type environnement = {
  dseparation: float;
  conflict_clusters: avion array array;
}

		       
let distance = fun a b ->
  (** returns the distance between to points, without changing the unit *)
  let xa = float a.x in
  let ya = float a.y in
  let xb = float b.x in
  let yb = float b.y in
  ((xa -. xb)**2. +. (ya -. yb)**2.)**0.5;;

let vitesse = fun a ->
  (** returns the speed at the point a  *)
  let vxa = float a.vx in
  let vya = float a.vy in
  (vxa**2. +. vya**2.)**0.5;;

let assemble = fun env ->
  (** assembles the conclict clusters together *)
  ();;0

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

let pointproche = fun tabpoints t ->
  (** Trouve le point le plus proche du temps t dans la liste *)
  let pointfound = ref tabpoints.(0) in
  for i=0 to Array.length tabpoints -1
  do
    let pointobs = tabpoints.(i) in
    if abs (t - pointobs.temps) < abs (t - (!pointfound).temps)
    then
      pointfound := pointobs
  done;
  !pointfound;;

let selectpoint = fun t1 t2 point ->
  (** renvoie le point dans un array s'il est entre t1 et t2 *)
  if t1 < point.temps && point.temps < t2
  then
    [| point |]
  else
    [||]
  
let local_detection = fun env p1 p2 beacon ->
  (** detects if p1 and p2 are in conflict near the beacon *)
  let t1 = List.assoc beacon p1.liste_balises in
  let deltat = int_of_float (env.dseparation /. (vitesse p1.tableau_point4D.(0))) in
  (* 
     algo :
     on se limite à un groupe de points proches de la balise 
   *)
  let points1intermediaire = Array.map (selectpoint (t1 - deltat) (t1 + deltat)) p1.tableau_point4D in
  let points1 = Array.fold_right Array.append points1intermediaire [||] in
  
  (*
     on compare un point avec celui de temps inferieur et celui de temps superieur de l'autre avion
     on renvoie des groupes en conflit (array de array a priori)
   *)
  ();;

let two_planes_detection = fun env p1 p2 ->
  (** Detects a conflict between p1 and p2 and adds it to the conflictclusters *)
  if p1.fl = p2.fl
  then
    let communes = common_beacon p1 p2 in
    if communes != [||]
    then
      Array.iter (local_detection env p1 p2) communes
    else
      ()
  else
    ();;

let one_plane_detection = fun p planesinactivity env ->
  (** actualises the conflicts clusters with the new plane *)
  Array.iter (two_planes_detection env p) planesinactivity;
 (*
    If a plane shares a FL and a beacon with p, find whether it's in conflict with p, and actualises the conflict clusters
  *)
  assemble env;;

let () =
  Printf.printf "ok"
