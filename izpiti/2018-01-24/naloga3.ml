type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  (* Tip linearnih preslikav *)
  type t
  (* Identiteta *)
  val id : t
  (* Dano preslikavo uporabi na vektorju *)
  val uporabi : t -> vektor -> vektor
  (* Vrne linearno preslikavo, določeno z matriko *)
  val iz_matrike : matrika -> t
  (* Vrne linearno preslikavo, določeno s funkcijo
     (predpostavite lahko, da je funkcija linearna) *)
  val iz_funkcije : (vektor -> vektor) -> t
  (* Vrne kompozitum danih preslikav. *)
  val kompozitum : t -> t -> t
end

module Matrika: Linearna = struct
  type t = matrika

  let id = (1, 0, 0, 1)

  let uporabi matrika v =
    match matrika, v with
    (a, b, c, d), (x, y) -> (a * x + b * y, c * x + d * y)

  let iz_matrike matrika = matrika

  let iz_funkcije f =
    let prvi (x, y) = x in
    let drugi (x, y) = y in
    (prvi (f (1,0)), prvi (f (0,1)), drugi (f (1,0)), drugi (f (0,1)))

  let kompozitum matrika1 matrika2 =
    match matrika1, matrika2 with
    (a, b, c, d), (x, y, z, w) -> (a*x+b*z, a*y+b*w, c*x+d*z, c*y+d*w)

end

module Funkcija: Linearna = struct
  type t = int * int -> int * int
  let id = (fun x -> x)
  let uporabi f v = f v
  let iz_matrike matrika = 
    match matrika with
    (a, b, c, d) -> (fun (x, y) -> (a*x+b*y, c*x+d*y))
  let iz_funkcije f = f
  let kompozitum f g = fun x -> f (g x)
  
end 

