type point = {
    x : int;
    y : int;
    temps : int;
    vx : int;
    vy : int;
  };;

type trajectoire = {
    acceleree : point list;
    ralentie : point list;
    initiale : point list};;

type balises_avion = {
    nom_balise_avion : string;
    temps_passage : int};;

type avion = {
    nom : string ;
    liste_balises : balises_avion list;
    trajectoires : trajectoire;
    fl : int;
  };;

type balise = {
    nom_balise : string;
    bx : int;
    by : int;
  };;


type action =
    Acceleration
  |Ralentissement;;

let vitesse = fun point ->
  let vxp = float point.vx in
  let vyp = float point.vy in
  (vxp**2. +. vyp**2.)**0.5;;

let norme  = fun point ->
  let xp = float point.x in
  let yp = float point.y in
  (xp**2. +. yp**2.)**0.5;;

let distance = fun point1 point2 ->
  let xp1 = float point1.x in
  let xp2 = float point2.x in
  let yp1 = float point1.y in
  let yp2 = float point2.y in
  ((xp1-.xp2)**2. +. (yp1 -. yp2)**2.)**0.5;;

let convert_time = fun time ->
  let list_time = Str.split (Str.regexp ":") time in
  3600 * int_of_string (List.nth list_time 0) + 60 * int_of_string (List.nth list_time 1) + int_of_string (List.nth list_time 2);;


let diminution_vitesse = fun point ->
  (vitesse point)*.0.95;;

let augmentation_vitesse = fun point ->
   (vitesse point)*.1.05;; 

let calcul_trajectoires = fun liste_point ->       
  let modif_point = fun point action ->
    match action with
      Acceleration -> {x = point.x ;
                       y = point.y ;
                       temps = int_of_float (3600.*.64.*. (distance point (List.hd liste_point)) /. (augmentation_vitesse point)) ;
                       vx =int_of_float (float point.vx*.1.05) ;
                       vy = int_of_float (float point.vy*.1.05) }
    |Ralentissement -> {x = point.x ;
                        y = point.y ;
                        temps = int_of_float (3600.*.64.*. (distance point (List.hd liste_point)) /. (diminution_vitesse point)) ;
                        vx = int_of_float (float point.vx*.0.95) ;
                        vy = int_of_float (float point.vy*.0.95) }(*MODIFIER LE POINT DE DEPART DE LA MODIFICATION DE VITESSE *) in
  let rec mp_rec = fun lnormale lmodif action  ->
    match lnormale with
      [] -> List.rev lmodif
    | x::xs ->
        let newpoint = modif_point x action in
        mp_rec xs (newpoint::lmodif) action in

  let trajectoire_ralentie = mp_rec liste_point [] Ralentissement in
  let trajectoire_acceleree = mp_rec liste_point [] Acceleration in
  {acceleree = trajectoire_acceleree ; ralentie = trajectoire_ralentie ; initiale = liste_point};;

    
  


let creer_avion = fun name liste_balise tableau_point ->
  let list_pt4D = ref [] in
  for i=0 to Array.length tableau_point - 1 do
    list_pt4D := List.append !list_pt4D [{x = int_of_string tableau_point.(i).(1) ; y = int_of_string tableau_point.(i).(2)  ; temps = convert_time tableau_point.(i).(0) ; vx =int_of_string tableau_point.(i).(3) ; vy = int_of_string tableau_point.(i).(4) }]
  done;
  {nom = name ; liste_balises = liste_balise ; trajectoires = calcul_trajectoires (!list_pt4D)  ; fl = int_of_string tableau_point.(0).(5) };;


let creer_balise_avion = fun liste_balise ->
  let rec balise_rec = fun lbal liste_balise_avion ->
    match lbal with
      [] -> List.rev liste_balise_avion
    |x::xs -> balise_rec xs ({nom_balise_avion = fst x ; temps_passage = snd x}::liste_balise_avion) in
  balise_rec liste_balise [];;

    


let creer_balises = fun liste_balise ->
  let liste_balises = ref [] in
  for i=0 to List.length liste_balise - 1 do
    liste_balises := List.append !liste_balises [{nom_balise = List.nth (List.nth liste_balise i) 0 ; bx = int_of_string (List.nth (List.nth liste_balise i) 1 ); by = int_of_string (List.nth (List.nth liste_balise i) 2)} ]
  done;
  !liste_balises;;


let afficher_avion = fun avion ->
  Printf.printf "%s\n" avion.nom;
  List.iter (fun balise -> Printf.printf "%s %d\n" balise.nom_balise_avion balise.temps_passage) avion.liste_balises;
  List.iter (fun point4d-> Printf.printf "x = %d, y = %d , temps = %d , vx = %d, vy = %d \n " point4d.x point4d.y point4d.temps point4d.vx point4d.vy ) avion.trajectoires.initiale;;


let afficher_balise = fun balise ->
  Printf.printf "%s\n" balise.nom_balise;
  Printf.printf "(%d,%d)\n" balise.bx balise.by;;






    


  
