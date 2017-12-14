(** Main module *)

let () =
  (* demo *)
  let all_possibilities = [Types.Conflit (Types.Acceleration,Types.Acceleration);
			   Types.Conflit (Types.Acceleration,Types.Ralentissement);
			   Types.Conflit (Types.Acceleration,Types.Constante);
			   Types.Conflit (Types.Ralentissement,Types.Acceleration);
			   Types.Conflit (Types.Ralentissement,Types.Ralentissement);
			   Types.Conflit (Types.Ralentissement,Types.Constante);
			   Types.Conflit (Types.Constante,Types.Acceleration);
			   Types.Conflit (Types.Constante,Types.Ralentissement);
			   Types.Conflit (Types.Constante,Types.Constante)]in
  let table_conflits = Hashtbl.create 1000 in
  let env = {Conflicts.dseparation=(6. *. 64.); Conflicts.actions_to_test= all_possibilities} in
  
  let filename = "exo2.txt" in
  let num_airplane = 164 in
  (* creation des objets avions *)
  let liste_avions = Lecture.read_file filename num_airplane in

  (* detection *)
  let rec parcours_avions = fun liste_prevus liste_connus ->
    let ajout = fun a -> Conflicts.added_plane_detection a liste_connus env table_conflits in
    match liste_prevus with
      [] -> liste_connus
    | x::xs -> (ajout x;
                parcours_avions xs (x::liste_connus)) in

  let avions_ajoutes = parcours_avions liste_avions [] in
  
  (* affichage *)
  let affichage_conf = fun duo_avions liste_conflits ->
    Printf.printf "%s & %s" (fst duo_avions) (snd duo_avions);
    let i = ref 0 in
    List.iter (fun x -> incr i;
			Printf.printf "\nConflit %d : " !i;
			match x with
			  Types.Conflit (action1, action2) -> (Types.print_action action1;
							       Types.print_action action2);) liste_conflits;
    Printf.printf "\n"
  in
  Hashtbl.iter affichage_conf table_conflits
