(** This module [Turtle] is my OCaml version of python's Turtle module. *)

type color = int
(** [color] is the color of the trail the turtle will leave as it moves. *)

type turtle = {
  mutable x : int;
  mutable y : int;
  mutable angle : int;
  mutable color : color;
}
(** [turtle] is an object that represents the current location. [x] and [y]
    stands for current coordinates, and [angle] is the direction that the turtle
    is facing. [color] is the color of the trail the turtle will leave as it
    moves. *)

val make_turtle : int -> int -> int -> color -> turtle
(** [make_turtle x y angle color] is a new turtle at position [x] and [y],
    facing [angle] in [color]. *)

val change_color : turtle -> color -> unit
(** [change_color turtle c] changes the color of the turtle to [c]. *)

val change_angle : turtle -> int -> unit
(** [change_angle turtle angle] changes the angle of the [turtle] to [angle]. *)

val to_rad : int -> float
(** [to_rad angle] is angle in radians. *)

val forward : turtle -> float -> unit
(** [forward turtle l] draws a line with length [l] in the direction of
    [turtle.angle] *)

val backward : turtle -> float -> unit
(** [backward turtle l] draws a line with length [l] in the opposite direction
    of [turtle.angle] *)

val left : turtle -> int -> unit
(** [left turtle angle] turns the turtle to the left by [angle] degrees. *)

val right : turtle -> int -> unit
(** [right turtle angle] turns the turtle to the right by [angle] degrees. *)