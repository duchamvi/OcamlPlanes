type point = {
    x : int;
    y : int;
    z : int;
    temps : int;
    vx : int;
    vy : int;
  };;

type avion = {
    nom : string ;
    liste_balises : (string*int) list;(*nom balise , temps*)
    balisesmoins5 : (string*int) list;(*nom balise , temps*)
    balisesplus5 : (string*int) list;(*nom balise , temps*)
    tableau_point4D : point list;
    trajmoins5 : point list;
    trajplus5 : point list;
    fl : int;
  };;

type balise = {
    bnom : string;
    bx : int;
    by : int;
  };;


