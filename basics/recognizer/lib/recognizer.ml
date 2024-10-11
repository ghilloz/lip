let rec lang1 = function
  [] -> false;
 |a :: t -> if (a = "0" || a = "1") then true else lang1 t;;

let lang2 l =
  let rec lang' acc = function
    [] -> true;
   |a::t when a = "0" -> if (acc) then false else lang' true t
   |_::t -> lang' true t

  in lang' false l;; 

let lang3 l =
  let rec lang' acc = function
    [] -> false
   |[a] -> if (a = "0") then true else false
   |a::t -> if (not(acc) && (a="1")) then false else lang' true t

  in lang' false l;;

let lang4 l =
  let rec lang' acc = function
    [] -> if acc=2 then true else false
   |a::t when acc<2 -> if a="0" then lang' acc t else lang' (acc+1) t
   |a::t when acc=2 -> if a="0" then lang' acc t else false
   |_ -> false

  in lang' 0 l;;

let lang5 l =
  let rec lang' acc = function
    [] when acc -> true
   |a::b::t -> if a=b then lang' true t else false
   |_ -> false

  in lang' false l;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
