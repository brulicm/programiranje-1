(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y = 
    if x<0 
    then false 
    else x * x = y

let pack3 x y z = (x, y, z)

let rec sum_if_not predikat list = 
    match list with
    | [] -> 0
    | x :: xs -> 
        if not (predikat x)
        then x + (sum_if_not predikat xs) 
        else 
        sum_if_not predikat xs


(*let rec apply list1 list2 =
    match list1, list2 with 
    | [], [] -> [[]]
    | f :: fs, [] -> [[]]
    | [], x :: xs -> [] :: (apply [] xs)
    | f :: fs, x :: xs -> f x ::*)

    

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = 
    | Predavanja of int
    | Vaje of int

type srecanje = {predmet: string; vrsta: vrsta_srecanja}

type urnik = Urnik of srecanje list list

let vaje = {predmet = "Analiza 2a"; vrsta = Vaje(3)}

let predavanje = {predmet = "Programiranje 1"; vrsta = Predavanja(2)}

let urnik_profesor = [[{predmet = "Analiza 2a"; vrsta = Vaje(2)}];[]; [{predmet = "Algebra 2"; vrsta = Predavanja(1) }]; [];[];[{predmet = "Programiranje 1"; vrsta = Vaje(1)}];[]]

(*let je_preobremenjen Urnik(list) = 
    match list with 
    | [] -> false
    | s :: ss -> *)

let bogastvo () = ()
