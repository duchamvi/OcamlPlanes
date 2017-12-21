type point = {
    x : int;
    y : int;
    temps : int;
    vx : int;
    vy : int;
  }

type trajectoire = {
    acceleree : point list;
    ralentie : point list;
    initiale : point list}

		     
type balises_avion = {
    nom_balise_avion : string;
    temps_passage : int}

		       
type avion = {
    nom : string ;
    liste_balises : balises_avion list;
    trajectoires : trajectoire;
    fl : int;
    tp_secteur : int
  }


type action =
    Acceleration
  |Ralentissement
  |Constante;;

type conflit =
    Conflit of action*action;;

  
let vitesse = fun point ->
  (** Renvoie la vitesse d'un point *)
  let vxp = float point.vx in
  let vyp = float point.vy in
  (vxp**2. +. vyp**2.)**0.5

let norme  = fun point ->
  let xp = float point.x in
  let yp = float point.y in
  (xp**2. +. yp**2.)**0.5

let distance = fun point1 point2 ->
  (** renvoie la distance entre deux points *)
  let xp1 = float point1.x in
  let xp2 = float point2.x in
  let yp1 = float point1.y in
  let yp2 = float point2.y in
  ((xp1-.xp2)**2. +. (yp1 -. yp2)**2.)**0.5
				      

let point_interpole = fun lp t ->
    (** Trouve le point de temps t dans la liste en interpolant les deux plus proches
   used by local_detection *)
  (* si le temps est avant le premier point de la liste, prendre le premier point et estimer sa position a t *)
  let depart = List.hd lp in
  if depart.temps > t then
    {
      x = depart.x - depart.vx*(depart.temps-t);
      y = depart.y - depart.vy*(depart.temps-t);
      temps = t;
      vx = depart.vx;
      vy = depart.vy;
    }
  else
    (* Trouver le point de temps juste inferieur et le suivant *)
    let rec pi_rec = fun liste lastpoint->
      match liste with
	[] -> let fin = lastpoint in (* le dernier point est avant le temps t *)
	      {
		x = fin.x + fin.vx*(t-fin.temps);
		y = fin.y + fin.vy*(t-fin.temps);
		temps = t;
		vx = fin.vx;
		vy = fin.vy;
	      }
      | p::ps -> let dt2 = p.temps - t in
		 if dt2 > 0 then
		   let dt1 = t - lastpoint.temps in
		   let interpole_param = fun u v ->
		     int_of_float ((float (u*dt2 + v*dt1)) /. (float (dt1+dt2))) in
		   {		       
		     x = interpole_param lastpoint.x p.x; 
		     y = interpole_param lastpoint.y p.y;
		       temps = t;
		       vx = interpole_param lastpoint.vx p.vx;
		       vy =  interpole_param lastpoint.vy p.vy;
		   }
		 else
		   pi_rec ps p in
    pi_rec lp depart
		     
  
let convert_time = fun time ->
  (** convertit le temps en secondes *)
  let list_time = Str.split (Str.regexp ":") time in
  3600 * int_of_string (List.nth list_time 0) + 60 * int_of_string (List.nth list_time 1) + int_of_string (List.nth list_time 2)


let diminution_vitesse = fun point ->
  (** calcule la vitesse reduite *)
  (vitesse point)*.0.95

let augmentation_vitesse = fun point ->
  (** calcule la vitesse augmentee *)
  (vitesse point)*.1.05

let calcul_trajectoires = fun liste_point temps_arrivee ->
  (** Renvoie l'objet trajectoires pour la creation de l'avion *)
  let modif_point = fun point action ->
    match action with
      Acceleration -> if point.temps > temps_arrivee - 600 then {x = point.x ;
                       y = point.y ;
                       (* temps = int_of_float (3600.*.64.*. (distance point (List.hd liste_point)) /. (augmentation_vitesse point))*)
                       temps = int_of_float (float (temps_arrivee - 600) +. (float (point.temps - (temps_arrivee - 600))) /. 1.05);(*à modifier*)
                       vx =int_of_float (float point.vx*.1.05) ;
                                                                 vy = int_of_float (float point.vy*.1.05) }
      else
        point
    |Ralentissement -> if point.temps > temps_arrivee -600 then {x = point.x ;
                        y = point.y ;
                        (*temps = int_of_float (3600.*.64.*. (distance point (List.hd liste_point)) /. (diminution_vitesse point))*)
                        temps = int_of_float (float (temps_arrivee - 600) +. (float (point.temps - (temps_arrivee - 600))) /. 0.95) ;
                        vx = int_of_float (float point.vx*.0.95) ;
                                                                 vy = int_of_float (float point.vy*.0.95) }(*MODIFIER LE POINT DE DEPART DE LA MODIFICATION DE VITESSE *)
    else
        point
    |Constante -> point in
  let rec mp_rec = fun lnormale lmodif action  ->
    match lnormale with
      [] -> List.rev lmodif
    | x::xs ->
        let newpoint = modif_point x action in
        mp_rec xs (newpoint::lmodif) action in

  let trajectoire_ralentie = mp_rec liste_point [] Ralentissement in
  let trajectoire_acceleree = mp_rec liste_point [] Acceleration in
  {acceleree = trajectoire_acceleree ; ralentie = trajectoire_ralentie ; initiale = liste_point};;

    
  


let creer_avion = fun name liste_balise tableau_point temps_arrivee->
  (** cree un objet avion à partir des donnees d'entree *)
  Printf.printf "Nom avion : %s \n" name;
  let list_pt4D = ref [] in
  for i=1 to Array.length tableau_point - 1 do
    list_pt4D := List.append !list_pt4D [{x = int_of_string tableau_point.(i).(1) ;
                                          y = int_of_string tableau_point.(i).(2) ;
                                          temps = convert_time tableau_point.(i).(0) ;
                                          vx =int_of_string tableau_point.(i).(3) ;
                                          vy = int_of_string tableau_point.(i).(4) }]
  done;
  {nom = name ;
   liste_balises = liste_balise ;
   trajectoires = calcul_trajectoires !list_pt4D temps_arrivee ;
   fl = int_of_string tableau_point.(1).(5);
 tp_secteur= temps_arrivee};;


let creer_balise_avion = fun liste_balise ->
  (** cree un objet balise_avion a partir des donnees *)
  let rec balise_rec = fun lbal liste_balise_avion ->
    match lbal with
      [] -> List.rev liste_balise_avion
    |x::xs -> balise_rec xs ({nom_balise_avion = fst x ; temps_passage = snd x}::liste_balise_avion) in
  balise_rec liste_balise [];;


let afficher_avion = fun avion ->
  Printf.printf "Nom : %s\n" avion.nom;
  List.iter (fun balise -> Printf.printf "Balise : %s %d\n" balise.nom_balise_avion balise.temps_passage) avion.liste_balises;
  List.iter (fun point4d-> Printf.printf "x = %d, y = %d , temps = %d , vx = %d, vy = %d \n " point4d.x point4d.y point4d.temps point4d.vx point4d.vy ) avion.trajectoires.initiale;;


let print_action = fun action ->
  match action with
    Acceleration -> Printf.printf " Acceleration "
  |Ralentissement -> Printf.printf " Ralentissement "
  |Constante -> Printf.printf " Constante "

let cout_action = fun action ->
  match action with
    Acceleration -> 2
  |Ralentissement -> 1
  |Constante -> 0
			      
let affichage_conf = fun duo_avions liste_conflits ->
  Printf.printf "%s & %s" (fst duo_avions) (snd duo_avions);
  let i = ref 0 in
  List.iter (fun x -> incr i;
		      Printf.printf "\nConflit %d : " !i;
		      match x with
			Conflit (action1, action2) -> (print_action action1;
							     print_action action2);) liste_conflits;
  Printf.printf "\n"

let compare_conflits = fun liste_conflits action1 action2 ->
  let rec ccrec = fun liste ->
    match liste with
      [] -> false
    | (Conflit (a1,a2))::xs ->
        ((a1=action1) && (a2=action2)) || ccrec xs
  in
  ccrec liste_conflits

  
