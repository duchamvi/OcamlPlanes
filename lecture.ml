open Types

let convert_time_sector = fun time ->
  let list_time = Str.split (Str.regexp ":") time in
  3600 * int_of_string (List.nth list_time 0) + 60 * int_of_string (List.nth list_time 1);;  

let select_sector = fun sector_list sector ->
  let rec select = fun liste ->
    match liste with
	x::xs ->
	  if x = sector then(
	    convert_time_sector (List.hd xs)
	  )
	  else select xs
      | [] -> -1
  in
  select sector_list

let read_file = fun filename num_airplane ->
  let chan = open_in filename in
  
  let regexp_nb_plot = Str.regexp "^NbPlots:  " in
  let regexp_airplane = Str.regexp "^\$  " in
  let regexp_airplane_beacon = Str.regexp "^! " in
  let regexp_sector = Str.regexp "^>" in

  let nb_plane = ref 0 in
  let end_plane = ref true in

  let airplanes = ref [] in
  let name = ref "" in
  let airplane_beacons = ref [] in
  let time_sector = ref 0 in

  try
    while !end_plane; do
      let new_line = ref "" in

      let airplane_plots = ref [|[||]|] in

      let n = ref 0 in

      new_line := input_line chan;

      if Str.string_match regexp_airplane !new_line 0 then(
	nb_plane := !nb_plane + 1;
	Printf.printf "\n***************** Plane %d ******************\n" !nb_plane;
	name := List.nth (Str.split (Str.regexp "[ \n\r\x0c\t]+") !new_line) 6;
	Printf.printf "Name : %s\n" !name;
      );
      
      if Str.string_match regexp_airplane_beacon !new_line 0 then(
        airplane_beacons := [];
	let plane_beacons_line = Str.split (Str.regexp "[ \n\r\x0c\t]+") !new_line in
	Printf.printf "*********** Airplane Beacons ********\n";
	for i = 0 to ((List.length plane_beacons_line -1) / 5 - 1) do
	  let one_beacon = [{nom_balise_avion = (List.nth plane_beacons_line (1+5*i)); temps_passage = (convert_time (List.nth plane_beacons_line (3+5*i)))}] in
	  airplane_beacons := List.append !airplane_beacons one_beacon;
	done;	
      );
      
      if Str.string_match regexp_sector !new_line 0 then(
	let sector_line = Str.split (Str.regexp "[ \n\r\x0c\t]+") !new_line in
	time_sector := select_sector sector_line "RU";
	Printf.printf "Entry Time in sector RU : %d\n" !time_sector;	
      );
      
      if Str.string_match regexp_nb_plot !new_line 0 then(
	Printf.printf "******* Plots ********\n";
	let num_plot = int_of_string (List.nth (Str.split regexp_nb_plot !new_line) 0) in
	Printf.printf "Num Plots : %d\n" num_plot;
	while !n < num_plot; do
	  n := !n + 1;
	  new_line := input_line chan;
	  airplane_plots := Array.append !airplane_plots [| Array.of_list (Str.split (Str.regexp "[ \n\r\x0c\t]+") !new_line) |];
	  flush stdout;
	done;
	if !nb_plane >= num_airplane then
	  end_plane := false;    
	airplanes := List.append !airplanes [creer_avion !name !airplane_beacons !airplane_plots !time_sector];

      );
    done;
    Printf.printf "sortie lecture\n";
    !airplanes;

  with End_of_file ->
    close_in chan;
    !airplanes;;




(*let ()=
  let filename = "exo2.txt" in
  let num_airplane = 3 in
  let a = read_file filename num_airplane in
  Printf.printf "****** Fini ******\n";
;;
 *)
