(** Main module *)

let () =
  let distance_sep = float_of_string Sys.argv.(1) in
  let num_airplane = int_of_string Sys.argv.(2) in  
  (* demo *)
  let all_possibilities = [Types.Conflit (Types.Acceleration,Types.Acceleration);
			   Types.Conflit (Types.Acceleration,Types.Ralentissement);
			   Types.Conflit (Types.Acceleration,Types.Constante);
			   Types.Conflit (Types.Ralentissement,Types.Acceleration);
			   Types.Conflit (Types.Ralentissement,Types.Ralentissement);
			   Types.Conflit (Types.Ralentissement,Types.Constante);
			   Types.Conflit (Types.Constante,Types.Acceleration);
			   Types.Conflit (Types.Constante,Types.Ralentissement);
			   Types.Conflit (Types.Constante,Types.Constante)] in
  let one_out = [Types.Conflit (Types.Constante,Types.Acceleration);
	       Types.Conflit (Types.Constante,Types.Ralentissement);
	       Types.Conflit (Types.Constante,Types.Constante)] in
  let two_out = [Types.Conflit (Types.Acceleration,Types.Constante);
	       Types.Conflit (Types.Ralentissement,Types.Constante);
	       Types.Conflit (Types.Constante,Types.Constante)] in
  let table_conflits = Hashtbl.create 1000 in
  let env = {Conflicts.dseparation=(distance_sep *. 64.);
	     Conflicts.all_actions = all_possibilities;
	     Conflicts.actions_1_out = one_out;
	     Conflicts.actions_2_out = two_out} in
  
  let filename = "exo2.txt" in
  (* creation des objets avions *)
  let liste_avions = Lecture.read_file filename num_airplane in

  Printf.printf "\nDetection des conflits\n";
  flush stdout;
  
  (* detection *)
  let rec parcours_avions = fun liste_prevus liste_connus ->
    let ajout = fun a -> Conflicts.added_plane_detection a liste_connus env table_conflits in
    match liste_prevus with
      [] -> liste_connus
    | x::xs -> (ajout x;
                parcours_avions xs (x::liste_connus)) in

  let avions_ajoutes = parcours_avions liste_avions [] in
  
  (* affichage *)
  Hashtbl.iter Types.affichage_conf table_conflits;
  Printf.printf "\nResolution des conflits\n";
  flush stdout;

  (* calcul de la solution *)
  let solution = Backtrack.backtrack
      avions_ajoutes
      Backtrack.choose
      [|Types.Constante; Types.Ralentissement; Types.Acceleration|]
      Backtrack.conflit
      table_conflits
      Backtrack.choix in

  Backtrack.affichage_branche solution


