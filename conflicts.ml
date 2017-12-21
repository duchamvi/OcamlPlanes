(** Conflict detection module *)
       
(* Reglages *)
type parametres = {
  dseparation: float;
  all_actions: Types.conflit list;
  actions_1_out: Types.conflit list;
  actions_2_out: Types.conflit list
}
	       	     
let common_beacon = fun p1 p2 ->
  (** renvoie une structure contenant les balises communes à deux avions
   used by two_planes_detection A AMELIORER*)
  let balises_communes = [||] in
  let rec cbrec = fun balises1 balises2 communes ->
    match balises1 with
      [] -> communes
    | x::xs ->
       if List.exists (fun b -> x.Types.nom_balise_avion = b.Types.nom_balise_avion) balises2
       then
	 cbrec xs balises2 (Array.append communes [|x.Types.nom_balise_avion|])
       else
	 cbrec xs balises2 communes					 
  in
  cbrec p1.Types.liste_balises p2.Types.liste_balises balises_communes
     

let selecttraj = fun plane action_chosen ->
  (** returns the chosen trajectory of a plane *)
  match action_chosen with
    Types.Acceleration -> plane.Types.trajectoires.Types.acceleree
  | Types.Ralentissement -> plane.Types.trajectoires.Types.ralentie
  | Types.Constante -> plane.Types.trajectoires.Types.initiale
  
let local_detection = fun separation  p1 p2 action1 action2 ->
  (** detects if p1 and p2 are in conflict near the beacon by comparing all of their 4D points.
   used by two_planes_detection *)
    
  (* on prend les points à comparer selon les vitesses traitees *)
  let points1 = selecttraj p1 action1 in
  let points2 = selecttraj p2 action2 in
  (*
    on compare un point avec celui le plus proche temporellement de l'autre avion
    on renvoie true en cas de conflit
   *)
  let rec loc_detec_rec = fun listepoints1 ->
    match listepoints1 with
      [] -> false
    | x::xs -> let pointp2 = Types.point_interpole points2 x.Types.temps in
	       (Types.distance x pointp2 < separation) || loc_detec_rec xs
  in
  loc_detec_rec points1		      
		
let two_planes_detection = fun env known_conflicts p1 p2 ->
  (** Detects a conflict between p1 and p2 and adds it to a cluster. 
   used by added _plane_detection *)
  if (p1.Types.fl = p2.Types.fl)
  then
    let communes = common_beacon p1 p2 in
    if not (communes = [||])
    then
      let select_real_conflicts = fun situation ->
	match situation with
	  Types.Conflit (action1, action2) -> local_detection env.dseparation p1 p2 action1 action2 in
      let actions_to_test = ref env.all_actions in
      if p1.Types.tp_secteur = -1 then
	actions_to_test := env.actions_1_out
      else if p2.Types.tp_secteur = -1 then
	actions_to_test := env.actions_2_out;     
      let liste_conflits = List.filter select_real_conflicts !actions_to_test in
      if liste_conflits !=[] then
	Hashtbl.add known_conflicts (p1.Types.nom,p2.Types.nom) liste_conflits

      
let added_plane_detection = fun p planesinactivity env known_conflicts ->
  (** returns the conflicts clusters with the new plane *)
  if p.Types.tp_secteur != -1 then
    List.iter (two_planes_detection env known_conflicts p) planesinactivity

