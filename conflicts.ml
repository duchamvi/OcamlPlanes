(** Conflict detection module *)


(*module Data = Types*)
open Types

       
(* Reglages *)
type parametres = {
  dseparation: float;
  actions_to_test: Types.conflit list
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


let selecttraj = fun plane action_chosen ->
  (** returns the chosen trajectory of a plane *)
  match action_chosen with
    Acceleration -> plane.trajectoires.acceleree
  | Ralentissement -> plane.trajectoires.ralentie
  | Constante -> plane.trajectoires.initiale
  
let local_detection = fun separation  p1 p2 action1 action2 ->
  (** detects if p1 and p2 are in conflict near the beacon by comparing all of their 4D points.
   used by two_planes_detection *)
    
  (* on prend les points à comparer A COMPLETER : on doit ajouter des choix selon les vitesses traitees *)
  let points1 = p1.trajectoires.initiale in
  let points2 = p2.trajectoires.initiale in
  (*
    on compare un point avec celui le plus proche temporellement de l'autre avion
    on renvoie true en cas de conflit
   *)
  let rec loc_detec_rec = fun listepoints1 ->
    match listepoints1 with
      [] -> false
    | x::xs -> let pointp2 = pointproche points2 x.temps in
	       (distance x pointp2 < separation) || loc_detec_rec xs
  in
  loc_detec_rec points1		      
		
let two_planes_detection = fun env known_conflicts p1 p2 ->
  (** Detects a conflict between p1 and p2 and adds it to a cluster. 
   used by added _plane_detection *)
  if p1.fl = p2.fl
  then
    let communes = common_beacon p1 p2 in
    if not (communes = [||])
    then
      let select_real_conflicts = fun situation ->
	match situation with
	  Conflit (action1, action2) -> local_detection env.dseparation p1 p2 action1 action2 in
      let liste_conflits = List.filter select_real_conflicts env.actions_to_test in
      Hashtbl.add known_conflicts (p1.nom,p2.nom) liste_conflits

      
let added_plane_detection = fun p planesinactivity env known_conflicts ->
  (** returns the conflicts clusters with the new plane *)
  Array.iter (two_planes_detection env known_conflicts p) planesinactivity

	   
let () =
  (* demo *)
  let all_possibilities = [Conflit (Acceleration,Acceleration);
							       Conflit (Acceleration,Ralentissement);
							       Conflit (Acceleration,Constante);
							       Conflit (Ralentissement,Acceleration);
							       Conflit (Ralentissement,Ralentissement);
							       Conflit (Ralentissement,Constante);
							       Conflit (Constante,Acceleration);
							       Conflit (Constante,Ralentissement);
							       Conflit (Constante,Constante)]in
  let table_conflits = Hashtbl.create 1000 in
  let env = {dseparation=5. *. 64.; actions_to_test= all_possibilities} in
  
  (* creation des avions de test*)
  let traj1 = [{x=50;y=50;temps=1;vx=1;vy=0};{x=1;y=1;temps=1;vx=1;vy=0}] in
  let balises1 = [{nom_balise_avion="b"; temps_passage=1}] in
  let a1 = {nom="a1";
	    liste_balises=balises1;
	    trajectoires={acceleree=traj1;
	    ralentie=traj1;
	    initiale=traj1};
	    fl=1} in
  
  let planesinactivity = ref [|a1|] in

  let traj2 = [{x=100;y=100;temps=1;vx=1;vy=0};{x=1;y=1;temps=1;vx=1;vy=0}] in
  let balises2 = [{nom_balise_avion="a"; temps_passage=1};{nom_balise_avion="b"; temps_passage=1}] in
  let a2 = {nom="a2";
	    liste_balises=balises2;
	    trajectoires={acceleree=traj2;
	    ralentie=traj2;
	    initiale=traj2};
	    fl=1} in
  
  let traj3 = [{x=100;y=100;temps=1;vx=1;vy=0};{x=10000;y=10000;temps=1;vx=1;vy=0}] in
  let balises3 = [{nom_balise_avion="a"; temps_passage=1}] in
  let a3 = {nom="a3";
	    liste_balises=balises3;
	    trajectoires={acceleree=traj3;
	    ralentie=traj3;
	    initiale=traj3};
	    fl=1} in

  (* detection *)
  let ajout = fun a -> added_plane_detection a !planesinactivity env table_conflits in
  
  ajout a2;
  planesinactivity := Array.append !planesinactivity [|a2|];
  ajout a3;
  (* affichage *)
  let affichage_conf = fun duo_avions liste_conflits ->
    Printf.printf "%s & %s" (fst duo_avions) (snd duo_avions);
    let i = ref 0 in
    List.iter (fun x -> incr i;
			Printf.printf "\nConflit %d : " !i;
			match x with
			  Conflit (action1, action2) -> (afficher_action action1;
							 afficher_action action2);) liste_conflits;
    Printf.printf "\n"
  in
  Hashtbl.iter affichage_conf table_conflits
