let sample = [(1, [2; 6]); (2, [3; 4]); (3, [5]); (4, [5]); (5, [6]); (8, [7; 3]); (7, [6])];;

topol sample;;

let sample2 = [("skarpetki", ["buty"]); ("buty", ["kurtka"]); ("majtki", ["spodnie"]); ("spodnie", ["buty"]);
	("koszulka", ["bluza"; "kurtka"; "szalik"]); ("bluza", ["kurtka"; "szalik"]); ("kurtka", ["czapka"; "rekawiczki"]);
	("szalik", ["kurtka"]) ];;

topol sample2;;

let sample3 = [(1, [2]); (2, [1]); (3, [1])];;
topol sample3;;