open Types

let read_file = fun filename num_airplane ->
  let chan = open_in filename in
  
  let regexp_nb_plot = Str.regexp "^NbPlots:  " in
  let regexp_airplane = Str.regexp "^\$  " in
  let regexp_airplane_beacon = Str.regexp "^! " in

  let nb_plane = ref 0 in
  let end_plane = ref true in

  let airplanes = ref [] in
  let name = ref "" in
  let airplane_beacons = ref [] in

  try
    while !end_plane; do
      let new_line = ref "" in
      

      let plane_beacons_line = ref [] in
      let one_beacon = ref [] in

      let airplane_plots = ref [|[||]|] in

      let n = ref 0 in
      let num_plot = ref 0 in

      new_line := input_line chan;

      if Str.string_match regexp_airplane !new_line 0 then(
	nb_plane := !nb_plane + 1;
	Printf.printf "\n************ New Plane %d *********\n" !nb_plane;
	name := List.nth (Str.split (Str.regexp "[ \n\r\x0c\t]+") !new_line) 6;
	Printf.printf "Name : %s\n" !name;
      );
      
      if Str.string_match regexp_airplane_beacon !new_line 0 then(
        airplane_beacons := [];
	plane_beacons_line := Str.split (Str.regexp "[ \n\r\x0c\t]+") !new_line;
	Printf.printf "*********** Airplane Beacons ********\n";
	for i = 0 to ((List.length !plane_beacons_line -1) / 5 - 1) do
	  one_beacon := [{nom_balise_avion = (List.nth !plane_beacons_line (1+5*i)); temps_passage = (convert_time (List.nth !plane_beacons_line (3+5*i)))}];
	  airplane_beacons := List.append !airplane_beacons !one_beacon;
	done;	
      );

      if Str.string_match regexp_nb_plot !new_line 0 then(
	Printf.printf "******* Plots ********\n";
	num_plot := int_of_string (List.nth (Str.split regexp_nb_plot !new_line) 0);
	Printf.printf "Num Plots : %d\n" !num_plot;
	while !n < !num_plot; do
	  n := !n + 1;
	  new_line := input_line chan;
	  airplane_plots := Array.append !airplane_plots [| Array.of_list (Str.split (Str.regexp "[ \n\r\x0c\t]+") !new_line) |];
	  flush stdout;
	done;
	if !nb_plane >= num_airplane then
	  end_plane := false;      
	airplanes := List.append !airplanes [creer_avion !name !airplane_beacons !airplane_plots 100];

      );
    done;
    Printf.printf "sortie lecture\n";
    !airplanes;

  with End_of_file ->
    close_in chan;
    !airplanes;;

