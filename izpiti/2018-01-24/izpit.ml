(*1 naloga*)
let rec izpisi_vsa_stevila = function
  | [] -> ()
  | x :: xs -> List.iter print_int (x :: xs)

let rec map2_opt f list1 list2 =
  let rec map2 f list1 list2 acc = 
    if List.length list1 != List.length list2 then
      None
    else 
      match list1, list2 with
      | [], [] -> Some (List.rev acc)
      | _ :: _, [] -> None
      | [], _ :: _ -> None
      | x :: xs, y :: ys -> map2 f xs ys ((f x y) :: acc)
  in 
  map2 f list1 list2 []

(*2.naloga*)
type filter_tree =
  | Node of filter_tree * int * filter_tree
  | List of int list

let filter_tree = Node(Node(List([1]), 5, List([])), 10, Node(List([]), 15, List([19; 20])))

let rec vstavi x = function
  | List(xs) -> List(x :: xs )
  | Node(lt, a, rt) when x > a -> Node(lt, a, vstavi x rt) 
  | Node(lt, a, rt) -> Node(vstavi x lt, a, rt)

let rec vstavi_seznam seznam = function 
  | List(xs) -> List(xs @ seznam)
  | Node(lt, a, rt) -> 
    match seznam with
    | [] -> Node(lt, a, rt)
    | x :: xs -> vstavi_seznam xs (vstavi x (Node(lt, a, rt)))


let rec preverjanje drevo =
  (*Pomožna funkcija: ali so vsi elementi seznama manjsi od a*)
  let rec manjsi a = function 
  | [] -> true
  | x :: xs -> x <= a && manjsi a xs 
  in
  (*Pomožna funkcija: ali so vsi elementi seznama vecji od a*)
  let rec vecji a = function
  | [] -> true
  | x :: xs -> x > a && vecji a xs
  in 
  match drevo with 
  |List(_) -> true
  |Node(lt, a, rt) -> 
    match lt, rt with
    |Node(l1, x1, r1), Node(l2, x2, r2) -> 
    x1 <= a && x2 > a && preverjanje (Node(l1, x1, r1)) && preverjanje (Node(l2, x2, r2))
    | Node(l, x, r), List(s) -> x <= a && preverjanje (Node(l, x, r)) && (vecji a s)
    | List(s), Node(l, x, r) -> x > a && preverjanje (Node(l, x, r)) && (manjsi a s)
    | List(s1), List(s2) -> (manjsi a s1) && (vecji a s2)
