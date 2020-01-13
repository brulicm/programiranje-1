(*1.naloga vredna 25*)

let razlika_kvadratov x y = (x + y) * (x + y)  - (x * x + y * y)

let uporabi_na_paru  f (x, y) = (f x, f y)


let rec ponovi_seznam n sez=
  match sez with
  | [] -> []
  | x :: xs when n<=0 -> []
  | x :: xs -> x :: xs @ (ponovi_seznam (n-1) sez)

let rec ponovi_seznam2 n sez=
  if n<= 0 then [] else sez @ (ponovi_seznam2 (n-1) sez)



let rec razdeli sez=
  let rec deli n_acc p_acc = function
    | [] -> (n_acc, p_acc)
    | x :: xs when x<0-> deli (x :: n_acc) p_acc xs
    | x :: xs -> deli n_acc (x :: p_acc) xs
  in
  deli [] [] sez

(*3.*)
type 'a veriga = 
| Filter of ('a -> bool) * 'a list * 'a veriga
| Ostalo of 'a list

(*a*)
let test = Filter ((>) 0,  [], Filter((<)10, [], Ostalo []))

(*b*)
let rec vstavi x = function
  | Filter (f, ys, rest) when f x -> Filter (f, x::ys, rest)
  | Filter (f, ys, rest) -> Filter(f, ys, vstavi x rest)
  | Ostalo ys -> Ostalo (x::ys)

let test2 = List.fold_right vstavi [-5; 7; 100; -7; 2] test

(*c*)
let rec poisci x = function 
  | Filter (f, ys, rest) when f x -> List.mem x ys
  | Filter (f, ys, rest) -> poisci x rest
  | Ostalo ys -> List.mem x ys

(*d*)
let rec izprazni_filtre = function
  | Filter(f, ys, rest) -> 
    let (prazni_rest, elementi) = izprazni_filtre rest in 
    (Filter(f, [], prazni_rest), ys @ elementi)
  | Ostalo ys -> (Ostalo [], ys)

(*e*)
let rec dodaj_filter f rest =
  let (empty_rest, elements) = izprazni_filtre rest in
  let filters = Filter (f, [], empty_rest)in
  List.fold_left (fun fil x -> vstavi x fil ) filters elements 



                