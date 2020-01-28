(*1.naloga*)
let razlika kvadratov x y = x * x - y * y
let uporabi_na_paru f (x, y) = (f x, f y)
let rec ponovi_seznam n = function
  | [] -> []
  | x :: xs when n <= 0 -> []
  | x :: xs -> (x :: xs) @ (ponovi_seznam (n-1) (x :: xs))
let rec razdeli sez=
  let rec deli n_acc p_acc = function
    | [] -> (n_acc, p_acc)
    | x :: xs when x<0-> deli (x :: n_acc) p_acc xs
    | x :: xs -> deli n_acc (x :: p_acc) xs
  in
  deli [] [] sez

(*3. naloga*)

type 'a veriga = 
  | Filter of ('a -> bool) * 'a list * 'a veriga 
  | Ostalo of 'a list

let test = Filter((>) 0, [], Filter((>) 10, [], Ostalo([])))

let rec vstavi a = function 
  | Ostalo(x) -> Ostalo(a :: x)
  | Filter(f, s, x) -> 
    if f a then 
      Filter(f, a :: s, x)
    else 
      vstavi a x 


let rec poisci a veriga = 

  let rec vsebovan a = function (*obstaja List.mem dumbo!!*)
    | [] -> false
    | x :: xs -> 
      if x = a then
        true
      else
        vsebovan a xs
  in 

  match veriga with
  | Ostalo(sez) -> vsebovan a sez
  | Filter(f, sez, v) -> 
    if f a then 
      vsebovan a sez
    else 
      poisci a v

let rec izprazni_filtre = function
  | Ostalo(sez) -> (Ostalo([]), sez)
  | Filter(f, sez, v) -> 
    let (prazni_v, elementi) = izprazni_filtre v in 
  (Filter(f, [], prazni_v), sez @ elementi)

let rec dodaj_filter f veriga = 
  let (prazne, elementi) = izprazni_filtre veriga in
  let filtri = Filter(f, [], prazne) in 
    List.fold_left (fun fil x -> vstavi x fil ) filtri elementi