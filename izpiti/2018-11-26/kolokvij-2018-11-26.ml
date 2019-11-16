(* -------- 1 -------- *)
let rec vsota list =
  let rec vsota' list acc =
    match list with
    | [] -> acc
    | x :: xs -> vsota' xs (x + acc)
    in
    vsota' list 0

(* -------- 2 -------- *)
let rec ali_je_narascajoce = function
  | [] -> true
  | x :: [] -> true
  | x :: (y :: ys) -> (x <= y) && (ali_je_narascajoce (y :: ys))
(* -------- 3 -------- *)
let rec vstavi_v_urejen_seznam n list =
  match list with
  | [] -> n :: []
  | x :: xs -> if n <= x then n :: (x :: xs) else x ::( vstavi_v_urejen_seznam n xs)

let rec uredi list =
  match list with
  | [] -> []
  | x :: xs -> vstavi_v_urejen_seznam x (uredi xs)
(* -------- 4 -------- *)
let rec vstavi' cmp list x =
  match list with 
  | [] -> [x]
  | y :: ys -> if (cmp x y) then x :: (y :: ys) else y :: (vstavi' cmp ys x)

let rec uredi' cmp list=
  match list with
  | [] -> []
  | x :: xs -> vstavi' cmp (uredi' cmp xs) x
(* -------- 5 -------- *)
type priority = Group of int | Top
type status = Staff | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)
let cmp_flyer x y =
  match x.status, y.status with
  | Staff, _ -> true
  | _, Staff -> false
  | Passenger Top, Passenger _ -> true
  | Passenger _, Passenger Top -> false
  | Passenger (Group j), Passenger (Group k) -> j > k

let rec vkrcavanje list = uredi' cmp_flyer list
(* -------- 7 -------- *)
