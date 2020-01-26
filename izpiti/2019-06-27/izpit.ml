(*1. naloga*)
type complex = { re : float ; im : float }

let complex_add z w = {re= z.re +. w.re; im = z.im +. w.im }
let complex_conjugate z = {re = z.re; im = -. z.im}
let rec list_apply_either pred f g = function
  | [] -> []
  | x :: xs -> 
    if pred x then
     (f x) :: list_apply_either pred f g xs
    else
     (g x) :: list_apply_either pred f g xs

let eval_poly list x =
  let rec eval acc list x_n =
    match list with
      | [] -> acc
      | a :: xs -> eval (acc + a * x_n) xs (x * x_n)
      in
      eval 0 list 1

(*2. naloga*)
type najemnik = string
type vrt = 
  |Obdelovan of najemnik
  | Oddan of najemnik * (vrt * vrt list) 
  | Prost

let vrt_primer = Oddan("Kovalevskaya", (Obdelovan("Galois"), Prost :: [Obdelovan("Lagrange")]))

let obdelovalec_vrta = function
  | Prost -> None
  | Obdelovan(x) -> Some x
  | _ -> None

let rec globina_oddajanja = function
  | Prost -> 0
  | Obdelovan(x) -> 0
  | Oddan(_, (podvrt, podvrtovi)) -> 
    let globine = (List.map globina_oddajanja podvrtovi) in
    1 + List.fold_left max (globina_oddajanja podvrt) globine

let rec v_uporabi = function
  | Prost -> false
  | Obdelovan(_) -> true
  | Oddan(_, (podvrt, podvrtovi)) -> v_uporabi podvrt && List.exists v_uporabi podvrtovi

let rec vsi_najemniki = function
  | Prost -> []
  | Obdelovan(najemnik) -> [najemnik]
  | Oddan(lastnik, (podvrt, podvrtovi)) -> 
    let vsi_podnajemniki =
      List.fold_left (fun acc vrt -> vsi_najemniki vrt @ acc) [] podvrtovi
    in
    lastnik :: vsi_najemniki podvrt @ vsi_podnajemniki

let rec vsi_obdelovalci = function
    | Prost -> []
    | Obdelovan(najemnik) -> [najemnik]
    | Oddan(lastnik, (podvrt, podvrtovi)) ->
    List.fold_left
    (fun acc vrt -> vsi_obdelovalci vrt @ acc) [] (podvrt :: podvrtovi)