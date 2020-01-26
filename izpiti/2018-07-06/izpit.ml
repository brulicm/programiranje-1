(*1.naloga*)
let uporabi f x = f x
let ibaropu x f = f x

let zacetnih_tailrec n xs =
  let rec aux n acc xs =
    if n <= 0 then
      Some (List.rev acc)
    else
      match xs with
      | [] -> None
      | x::xs -> aux (n-1) (x::acc) xs
  in
  aux n [] xs

(*2.naloga*)
type 'a neprazen_sez = Konec of 'a | Sestavljen of 'a * 'a neprazen_sez

let prvi = function
  | Sestavljen(a, _) -> a
  | Konec a -> a

let rec zadnji = function
    | Konec a -> a
    | Sestavljen(_, a) -> zadnji a

let rec dolzina = function
    | Konec a -> 1
    | Sestavljen(_, a) -> 1 + dolzina a
let rec pretvori_v_seznam = function 
    | Konec a -> [a]
    | Sestavljen(x, xs) -> x :: pretvori_v_seznam xs