(** Conflict detection module *)


(*module Data = Types*)
open Types

       
(* Reglages *)
type environnement = {
  dseparation: float;
}

		       
let distance = fun a b ->
  (** returns the distance between to points, without changing the unit *)
  let xa = float a.x in
  let ya = float a.y in
  let xb = float b.x in
  let yb = float b.y in
  ((xa -. xb)**2. +. (ya -. yb)**2.)**0.5

let vitesse = fun a ->
  (** returns the speed at the point a  *)
  let vxa = float a.vx in
  let vya = float a.vy in
  (vxa**2. +. vya**2.)**0.5

let assemble = fun known_clusters new_clusters ->
  (** assembles the conclict clusters together A AMELIORER*)
  Array.append known_clusters new_clusters

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
  cbrec p1.liste_balises p2.liste_balises balises_communes

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
  !pointfound

let selectpoint = fun t1 t2 point ->
  (** renvoie le point dans un array s'il est entre t1 et t2 *)
  if t1 < point.temps && point.temps < t2
  then
    [| point |]
  else
    [||]
  
let local_detection = fun env p1 p2 beacon already_found ->
  (** detects if p1 and p2 are in conflict near the beacon *)
  if already_found
  then
    already_found
  else
    let t1 = List.assoc beacon p1.liste_balises in
    let deltat = int_of_float (env.dseparation /. (vitesse p1.tableau_point4D.(0))) in
    (* on se limite à un groupe de points proches de la balise *)
    let points1intermediaire = Array.map (selectpoint (t1 - deltat) (t1 + deltat)) p1.tableau_point4D in
    let points1 = Array.fold_right Array.append points1intermediaire [||] in
    (*
     on compare un point avec celui de temps inferieur et celui de temps superieur de l'autre avion
     on renvoie des groupes en conflit (array de array a priori)
     *)
    let counter = ref (Array.length points1) in
    let conflict_found = ref false in
    while !counter > 0 && not !conflict_found
    do
      counter := !counter -1;
      let pointp1 = points1.(!counter) in
      let pointp2 = pointproche p2.tableau_point4D pointp1.temps in 
      if distance pointp1 pointp2 < env.dseparation
      then
	conflict_found := true;
    done;
    !conflict_found

let two_planes_detection = fun env p1 p2 conflict_clusters ->
  (** Detects a conflict between p1 and p2 and adds it to a cluster *)
  if p1.fl = p2.fl
  then
    let communes = common_beacon p1 p2 in
    if communes = [||]
    then
      conflict_clusters
    else
      if  Array.fold_right (local_detection env p1 p2) communes false
      then
	Array.append conflict_clusters [|[| p1; p2|]|]
      else
	conflict_clusters
  else
    conflict_clusters
    
let added_plane_detection = fun p planesinactivity env known_conflicts ->
  (** returns the conflicts clusters with the new plane *)
  let new_conflicts = Array.fold_right (two_planes_detection env p) planesinactivity [||] in
  assemble known_conflicts new_conflicts

let () =
  let conflict_clusters = ref [||] in
  let env = {dseparation=5. *. 64.} in
  (* avions *)
  let a1 = {nom="a1";liste_balises=[("b",1)];tableau_point4D=[|{x=50;y=50;fl=1;temps=1;vx=1;vy=0};{x=1;y=1;fl=1;temps=1;vx=1;vy=0}|]; fl=1} in
  let planesinactivity = ref [|a1|] in
  let a2 = {nom="a2";liste_balises=[("a",1);("b",1)];tableau_point4D=[|{x=100;y=100;fl=1;temps=1;vx=1;vy=0};{x=1;y=1;fl=1;temps=1;vx=1;vy=0}|]; fl=1} in
  let a3 = {nom="a3";liste_balises=[("a",1)];tableau_point4D=[|{x=100;y=100;fl=1;temps=1;vx=1;vy=0};{x=10000;y=10000;fl=1;temps=1;vx=1;vy=0}|]; fl=1} in

  (* detection *)
  let ajout = fun a -> added_plane_detection a !planesinactivity env !conflict_clusters in
  
  conflict_clusters := ajout a2;
  planesinactivity := Array.append !planesinactivity [|a2|];
  conflict_clusters := ajout a3;

  Array.iter (fun x -> Printf.printf "(%s;%s)\n" x.(0).nom x.(1).nom) !conflict_clusters
	     
 
			
