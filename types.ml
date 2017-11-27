type point = {
    x : int;
    y : int;
    fl : int;
    temps : int;
    vx : int;
    vy : int;
  };;

type avion = {
    nom : string ;
    liste_balises : (string*int) list;(*nom balise , temps*)
    tableau_point4D : point array;
    fl : int;
  };;

type balise = {
    nom : string;
    x : int;
    y : int;
  };;


