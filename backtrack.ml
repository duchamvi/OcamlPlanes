type branche =
    {
     manoeuvres : (string*Types.action) list;
     cout : int;
     retraits : string list
    }

      
let filtrage = fun liste avion ->
  List.filter (fun x -> avion.Types.nom!=x.Types.nom) liste ;;


let choose = fun v hashconflit ->
  List.hd v;;

  
let choose2 = fun v hashconflit ->
  (** Prend l'avion qui est en conflit avec le plus d'autres avions *)
  let counter = ref 0 in
  let conflit_max = ref (-1) in
  let avion_max_conflit = ref (List.hd v) in

  let test_avion = fun avion keys liste ->
    match keys with
      (nom1, nom2) ->
        if (nom1 = avion.Types.nom) || (nom2 = avion.Types.nom)
        then
          counter:= !counter+1; in
           
  let rec choose_rec = fun list ->
    match list with
      [] -> !avion_max_conflit
    |avion :: vs ->
        counter := 0;
        Hashtbl.iter (test_avion avion) hashconflit;
        if !counter > !conflit_max
        then
          (conflit_max := !counter;
           avion_max_conflit := avion);
        choose_rec vs in
  choose_rec v

	     
let choose4 = fun v hashconflit ->
  (** Prend l'avion qui a le plus de d'objets Conflit *)
  let counter = ref 0 in
  let conflit_max = ref (-1) in
  let avion_max_conflit = ref (List.hd v) in

  let test_avion = fun avion keys liste ->
    match keys with
      (nom1, nom2) ->
        if (nom1 = avion.Types.nom) || (nom2 = avion.Types.nom)
        then
          counter:= (!counter + (List.length liste));
  in
           
  let rec choose_rec = fun list ->
    match list with
      [] -> !avion_max_conflit
    |avion :: vs ->
        counter := 0;
        Hashtbl.iter (test_avion avion) hashconflit;
        if !counter > !conflit_max
        then
          (conflit_max := !counter;
           avion_max_conflit := avion);
        choose_rec vs in
  choose_rec v

let revchoose4 = fun v hashconflit ->
  (** Prend l'avion qui a le moins de d'objets Conflit *)
  let counter = ref 0 in
  let conflit_min = ref max_int in
  let avion_min_conflit = ref (List.hd v) in

  let test_avion = fun avion keys liste ->
    match keys with
      (nom1, nom2) ->
        if (nom1 = avion.Types.nom) || (nom2 = avion.Types.nom)
        then
          counter:= (!counter + (List.length liste));
  in
           
  let rec choose_rec = fun list ->
    match list with
      [] -> !avion_min_conflit
    |avion :: vs ->
        counter := 0;
        Hashtbl.iter (test_avion avion) hashconflit;
        if !counter < !conflit_min
        then
          (conflit_min := !counter;
           avion_min_conflit := avion);
        choose_rec vs in
  choose_rec v
	     
let pre_tri = fun listeavion hashconflit ->
  (** trie les avions a l'avance selon choose4 *)
  let rec ptrec = fun lentree lsortie ->
    match lentree with
      [] -> lsortie
    | x::xs ->
       let avion = revchoose4 lentree hashconflit in
       ptrec (filtrage lentree avion) (avion::lsortie)
  in
  ptrec listeavion []
  

let conflit = fun avion action a hashconflits ->
  let rec conflit_rec = fun list ->
    match list with
      [] -> false
    | (nom_avion,action_avion) :: xs ->
        if Hashtbl.mem hashconflits (nom_avion , avion.Types.nom)
        then
          let list_conflit = Hashtbl.find hashconflits (nom_avion , avion.Types.nom ) in
          Types.compare_conflits list_conflit action_avion action || conflit_rec xs
        else if Hashtbl.mem hashconflits (avion.Types.nom , nom_avion)
        then
          let list_conflit = Hashtbl.find hashconflits (avion.Types.nom , nom_avion ) in
          Types.compare_conflits list_conflit action action_avion || conflit_rec xs
        else
          conflit_rec xs
  in
  conflit_rec a;;


let choix = fun sol ->
  let rec chrec = fun liste soltemp ->
    match liste with
      [] -> soltemp
    | x::xs ->
        if x.cout < soltemp.cout then chrec xs x
        else chrec xs soltemp
  in
  chrec sol (List.hd sol)
        


let rec backtrack = fun liste_avions choose d conflit hashtbl_conflit choix ->
  (**resout avec l'algorithme backtrack *)
  let coutmin = ref max_int in
  
  let rec btrec = fun v a c r ->
    (* cas ou l'on arrive au bout de l'arbre de possibilites *)
    if v = []  then (coutmin := c ; {manoeuvres = a; cout = c; retraits = r})
		      
    else
      (* avancement dans l'arbre *)
      let avion = choose v hashtbl_conflit in
      let solutions = ref [] in

      (* test des nouvelles possibilites *)
      for i = 0 to Array.length d - 1 do
	let actioni = d.(i) in
	let couti = Types.cout_action actioni in
        if not (conflit avion actioni a hashtbl_conflit)
        then
          (* enregistrer cette possibilite *)
          if (c+couti) < !coutmin
	  then
            let possibilite = btrec (filtrage v avion) ((avion.Types.nom,d.(i))::a) (c+couti) r in
            solutions:= List.append !solutions [possibilite];
      done;
      
      (* choisir la possibilite qu'on renvoie et traiter le cas ou aucune solution n'existe *)
      if !solutions = []
      then
        if (c+1000) < !coutmin
        then
          btrec (filtrage v avion) a (c+1000) ((avion.Types.nom)::r)
        else
          {manoeuvres =[] ; cout = (c+1000) ; retraits =[]}
      else
        choix !solutions
          
  in

  btrec (pre_tri liste_avions hashtbl_conflit) [] 0 []


let rec semiglouton = fun liste_avions choose d conflit hashtbl_conflit choix ->
  
  let coutmin = ref max_int in
  
  let rec btrec = fun v a c r ->
    (* cas ou l'on arrive au bout de l'arbre de possibilites *)
    if v = []  then (coutmin := c ; {manoeuvres = a; cout = c; retraits = r})
		      
    else
      (* avancement dans l'arbre *)
      let avion = choose v hashtbl_conflit in
      let solutions = ref [] in

      (* test des nouvelles possibilites *)
      let i = ref 0 in
      while (!i < (Array.length d - 1)) && (List.length !solutions < 2) do
	let actioni = d.(!i) in
	i:= !i +1;
	let couti = Types.cout_action actioni in
        if not (conflit avion actioni a hashtbl_conflit)
        then
          (* enregistrer cette possibilite *)
          if (c+couti) < !coutmin
	  then
            let possibilite = btrec (filtrage v avion) ((avion.Types.nom,d.(!i))::a) (c+couti) r in
            solutions:= List.append !solutions [possibilite];
      done;
      
      (* choisir la possibilite qu'on renvoie et traiter le cas ou aucune solution n'existe *)
      if !solutions = []
      then
        if (c+1000) < !coutmin
        then
          btrec (filtrage v avion) a (c+1000) ((avion.Types.nom)::r)
        else
          {manoeuvres =[] ; cout = (c+1000) ; retraits =[]}
      else
        choix !solutions
          
  in

  btrec liste_avions [] 0 []

	
let rec glouton = fun liste_avions choose d conflit hashtbl_conflit choix ->
  (** resout avec un algorithme glouton *)
  let coutmin = ref max_int in
  
  let rec btrec = fun v a c r ->
    (* cas ou l'on arrive au bout de l'arbre de possibilites *)
    if v = []  then (coutmin := c ; {manoeuvres = a; cout = c; retraits = r})
		      
    else
      (* avancement dans l'arbre *)
      let avion = choose v hashtbl_conflit in
      let solutions = ref [] in

      (* test des nouvelles possibilites *)
      let i = ref 0 in
      while (!i < (Array.length d - 1)) && (List.length !solutions < 1) do
	let actioni = d.(!i) in
	i:= !i +1;
	let couti = Types.cout_action actioni in
        if not (conflit avion actioni a hashtbl_conflit)
        then
          (* enregistrer cette possibilite *)
          if (c+couti) < !coutmin
	  then
            let possibilite = btrec (filtrage v avion) ((avion.Types.nom,d.(!i))::a) (c+couti) r in
            solutions:= List.append !solutions [possibilite];
      done;
      
      (* choisir la possibilite qu'on renvoie et traiter le cas ou aucune solution n'existe *)
      if !solutions = []
      then
        if (c+1000) < !coutmin
        then
          btrec (filtrage v avion) a (c+1000) ((avion.Types.nom)::r)
        else
          {manoeuvres =[] ; cout = (c+1000) ; retraits =[]}
      else
        choix !solutions
          
  in
  (*let liste_avions =(pre_tri liste_avions hashtbl_conflit) in*)
  btrec liste_avions [] 0 []

	
let affichage_manoeuvres = fun manoeuvre ->
  Printf.printf "%s ->" (fst manoeuvre);
  Types.print_action (snd manoeuvre);
  Printf.printf "\n"
		

let affichage_branche = fun branche ->
  List.iter affichage_manoeuvres branche.manoeuvres;

  List.iter (fun a -> Printf.printf "%s -> Non resolu \n" a) branche.retraits
    
