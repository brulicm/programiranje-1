(*1.naloga*)

let podvoji_vsoto x y = 2*(x+y)
let povsod_vecji (x1, x2, x3) (y1, y2, y3) = 
  x1 > y1 && x2 > y2 && x3 > y3

let rec uporabi_ce_lahko f = function
  | None -> None
  | Some x -> Some (f x)

let rec pojavi_dvakrat a list = 
  let rec pojavi_enkrat a = function
    | [] -> false
    | x :: xs -> 
      if x = a && (List.mem a xs = false) then
        true 
      else if x != a then
        pojavi_enkrat a xs
      else 
        false 
  in 
  match list with 
  | [] -> false
  | x :: xs -> if x = a then pojavi_enkrat a xs else pojavi_dvakrat a xs

(*let rec izracunaj_v_tocki a = function
  | [] -> []
  | f :: fs -> f a :: izracunaj_v_tocki a fs*)

let rec izracunaj_v_tocki a list = 
  let rec izracunaj acc = function
    | [] -> List.rev acc
    | f :: fs -> izracunaj ((f a) :: acc) fs
  in izracunaj [] list

(*let rec eksponent x = function
  | 0 -> 1
  | p -> x * (eksponent x (p - 1))*)


let rec eksponent x =
  let rec eksponent' acc x = function
  | 0 -> acc
  | p -> eksponent' (acc * x) x (p-1) 
  in eksponent' 1 x 
  
(*2. naloga*)
type 'a mm_drevo =
  | Prazno
  | Drevo of 'a mm_drevo * 'a * int * 'a mm_drevo

let rec vstavi drevo x =
  match drevo with
  | Prazno -> Drevo(Prazno, x, 1, Prazno)
  | Drevo(drevo1, a, stevec, drevo2) -> 
    if x == a then 
      Drevo(drevo1, a, stevec + 1, drevo2)
    else if x < a then
      Drevo(vstavi drevo1 x, a, stevec, drevo2)
    else
      Drevo(drevo1, a, stevec, vstavi drevo2 x)

let rec multimnozica_iz_seznama = function
  | [] -> Prazno
  | x :: xs -> vstavi (multimnozica_iz_seznama xs) x 

let test = multimnozica_iz_seznama [2;5;1;4;1;1;2;8;8]

let rec velikost_multimnozice = function
  |Prazno -> 0
  | Drevo(levo_drevo, _, x, desno_drevo) -> 
    x + velikost_multimnozice levo_drevo + velikost_multimnozice desno_drevo


let rec seznam_iz_multimnozice drevo =
  let rec seznam_n_ponovitev n x = 
    match n with
    | 1 -> [x]
    | _ -> x :: seznam_n_ponovitev (n - 1) x
  in 
  match drevo with 
  |Prazno -> []
  | Drevo(ld, x, n, dd) -> (seznam_iz_multimnozice ld) @ (seznam_n_ponovitev n x) @ (seznam_iz_multimnozice dd)