(** Conflict detection module *)


(*module Data = Types*)
open Types

       
(* Reglages *)
type parametres = {
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

	       	     
let common_beacon = fun p1 p2 ->
  (** renvoie une structure contenant les balises communes à deux avions
   used by two_planes_detection A AMELIORER*)
  let balises_communes = [||] in
  let rec cbrec = fun balises1 balises2 communes ->
    match balises1 with
      [] -> communes
    | x::xs ->
       if List.exists (fun b -> x.nom_balise_avion = b.nom_balise_avion) balises2
       then
	 cbrec xs balises2 (Array.append communes [|x.nom_balise_avion|])
       else
	 cbrec xs balises2 communes					 
  in
  cbrec p1.liste_balises p2.liste_balises balises_communes

let time_of_beacon = fun beacon_name liste_balises ->
  let rec tob_rec = fun balises ->
    match balises with
      [] -> raise (Failure "Conflicts.time_of_beacon : No beacon of this name was found")
    | x::xs -> if x.nom_balise_avion = beacon_name
	       then
		 x.temps_passage
	       else
		 tob_rec xs in
  tob_rec liste_balises
	
let pointproche = fun listpoints t ->
  (** Trouve le point le plus proche du temps t dans la liste
   used by local_detection *)
  let rec pprec = fun pt lpoints ->
    match lpoints with
      [] -> pt
    | x::xs -> if abs (t - x.temps) < abs (t - pt.temps)
	       then
		 pprec x xs
	       else
		 pprec pt xs in
  pprec (List.hd listpoints) listpoints

	
let selectpoint = fun t1 t2 point ->
  (** renvoie le point dans une liste s'il est entre t1 et t2
   used by local_detection *)
  if (t1 < point.temps) && (point.temps < t2)
  then
    [ point ]
  else
    []

let selectinterval = fun t1 t2 liste_points ->
  let rec interval_rec = fun lorigine ldestination ->
    match lorigine with
      [] -> List.rev ldestination
    | x::xs -> if (t1 < x.temps) && (x.temps < t2)
	       then
		 interval_rec xs (x::ldestination)
	       else
		 interval_rec xs ldestination
  in
  interval_rec liste_points []

	       
let local_detection = fun env p1 p2 beacon already_found ->
  (** detects if p1 and p2 are in conflict near the beacon.
   used by two_planes_detection *)
  if already_found
  then
    already_found
  else

    (* on commence par se limiter a un groupe de points proches de la balise *)

    (* on selectionne l'intervalle de temps A COMPLETER : on doit ajouter des choix selon les vitesses traitees Idee : choix transmis dans l'env *)
    let t1 = time_of_beacon beacon p1.liste_balises in
    let deltat = int_of_float (env.dseparation /. (vitesse (List.hd p1.tableau_point4D))) in

    (* on prend les points dans cet intervalle A COMPLETER : on doit ajouter des choix selon les vitesses traitees *)
    let points1 = selectinterval (t1 - deltat) (t1 + deltat) p1.tableau_point4D in
    let points2 = selectinterval (t1 - deltat) (t1 + deltat) p2.tableau_point4D in
    (*
     on compare un point avec celui le plus proche temporellement de l'autre avion
     on renvoie true en cas de conflit
     *)
    let rec loc_detec_rec = fun listepoints1 ->
      match listepoints1 with
	[] -> false
      | x::xs -> let pointp2 = pointproche points2 x.temps in
		 (distance x pointp2 < env.dseparation) || loc_detec_rec xs
    in
    loc_detec_rec points1
		      

let two_planes_detection = fun env p1 p2 conflict_clusters ->
  (** Detects a conflict between p1 and p2 and adds it to a cluster. 
   used by added_plane_detection *)
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
  (* demo *)
  let conflict_clusters = ref [||] in
  let env = {dseparation=5. *. 64.} in
  
  (* creation des avions de test*)
  let traj1 = [{x=50;y=50;z=1;temps=1;vx=1;vy=0};{x=1;y=1;z=1;temps=1;vx=1;vy=0}] in
  let balises1 = [("b",1)] in
  let a1 = {nom="a1";
	    liste_balises=balises1;
	    balisesmoins5=balises1;
	    balisesplus5=balises1;
	    tableau_point4D=traj1;
	    trajmoins5=traj1;
	    trajplus5=traj1;
	    fl=1} in
  
  let planesinactivity = ref [|a1|] in

  let traj2 = [{x=100;y=100;z=1;temps=1;vx=1;vy=0};{x=1;y=1;z=1;temps=1;vx=1;vy=0}] in
  let balises2 = [("a",1);("b",1)] in
  let a2 = {nom="a2";
	    liste_balises=balises2;
	    balisesmoins5=balises2;
	    balisesplus5=balises2;
	    tableau_point4D=traj2;
	    trajmoins5=traj2;
	    trajplus5=traj2;
	    fl=1} in
  
  let traj3 = [{x=100;y=100;z=1;temps=1;vx=1;vy=0};{x=10000;y=10000;z=1;temps=1;vx=1;vy=0}] in
  let balises3 = [("a",1)] in
  let a3 = {nom="a3";
	    liste_balises=balises3;
	    balisesmoins5=balises3;
	    balisesplus5=balises3;
	    tableau_point4D=traj3;
	    trajmoins5=traj3;
	    trajplus5=traj3;
	    fl=1} in

  (* detection *)
  let ajout = fun a -> added_plane_detection a !planesinactivity env !conflict_clusters in
  
  conflict_clusters := ajout a2;
  planesinactivity := Array.append !planesinactivity [|a2|];
  conflict_clusters := ajout a3;

  Array.iter (fun x -> Printf.printf "(%s;%s)\n" x.(0).nom x.(1).nom) !conflict_clusters
