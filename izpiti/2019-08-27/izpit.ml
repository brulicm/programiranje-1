(*1. naloga*)
let odstej_trojici (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

let rec max_rezultat_do_n f n =
  if n<= 0 then f 0
  else max (f n) (max_rezultat_do_n f n-1)

let rec pocisti_seznam xs =
  let rec pocisti_seznam' acc= function
    | None :: xs -> pocisti_seznam' acc xs
    | Some x :: xs -> pocisti_seznam' (x :: acc) xs
    | [] -> List.rev acc
  in
  pocisti_seznam' []

let preveri_urejenost list = 
  let rec narasca = function
    | [] | _ :: [] -> true
    | x1 :: x2 :: xs -> if x1 < x2 then narasca (x2 :: xs) else false
    in 
  (list |> List.filter (fun x -> x mod 2 == 0) |> narasca) (* sodi narascajo*)
  && (list |> List.filter (fun x -> x mod 2 == 1) |> List.rev |> narasca) (*obrnjen list lihih*)

(*2.naloga*)
type 'a gnezdenje = 
| Element of 'a
| Podseznam of 'a gnezdenje list

let gnezdenje_primer =  [
  Element(1);
  Element(2);
  Podseznam( [
    Element(3);
    Podseznam( [Element(4)]); 
    Podseznam([]) ]);
  Podseznam([
    Element(5)])]

let rec najvecja_globina = function
| [] -> 1
| Element _ :: xs -> najvecja_globina xs
| Podseznam podsez :: xs ->
    max (najvecja_globina podsez + 1) (najvecja_globina xs)

let rec preslikaj f = function
| [] -> []
| Element x :: xs -> Element (f x) :: (preslikaj f xs)
| Podseznam podsez :: xs -> Podseznam (preslikaj f  podsez):: (preslikaj f xs)

let rec splosci = function
| [] -> []
| Element x :: xs -> x :: splosci xs
| Podseznam podsez :: xs -> (splosci podsez) @ (splosci xs)

let rec alternirajoci_konstruktorji = function
  | [] | _ :: [] -> true
  | Element _ :: (Podseznam _ :: xs as ys)
  | Podseznam _ :: (Element _ :: xs as ys) -> alternirajoci_konstruktorji ys
  | Element _ :: Element _ :: _
  | Podseznam _ :: Podseznam _ :: _ -> false

  let rec zlozi_preko_gnezdenja f acc gnezdenje =
    (* Napišemo lastno repno rekurzivno funkcijo za združevanje. *)
    let zdruzi xs ys =
      let rec prelozi ys = function
        | [] -> ys
        | x :: xs -> prelozi (x :: ys) xs
      in
      prelozi ys (List.rev xs)
    in
    let rec zlozi f acc = function
      | [] -> acc
      | Element x :: xs -> zlozi f (f acc x) xs
      | Podseznam podsez :: xs -> zlozi f acc (zdruzi podsez xs)
    in
    zlozi f acc gnezdenje