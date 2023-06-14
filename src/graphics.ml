(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

exception Graphic_failure of string

(* Initializations *)

let _ =
  Callback.register_exception "Graphics.Graphic_failure" (Graphic_failure "")

external raw_open_graph : string -> unit = "caml_gr_open_graph"

external raw_close_graph : unit -> unit = "caml_gr_close_graph"

external sigio_signal : unit -> int = "caml_gr_sigio_signal"

external sigio_handler : int -> unit = "caml_gr_sigio_handler"

let unix_open_graph arg =
  Sys.set_signal (sigio_signal ()) (Sys.Signal_handle sigio_handler);
  raw_open_graph arg

let unix_close_graph () =
  Sys.set_signal (sigio_signal ()) Sys.Signal_ignore;
  raw_close_graph ()

let open_graph, close_graph =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> (unix_open_graph, unix_close_graph)
  | "Win32" -> (raw_open_graph, raw_close_graph)
  | "MacOS" -> (raw_open_graph, raw_close_graph)
  | _ -> invalid_arg ("Graphics: unknown OS type: " ^ Sys.os_type)

external set_window_title : string -> unit = "caml_gr_set_window_title"

external resize_window : int -> int -> unit = "caml_gr_resize_window"

external clear_graph : unit -> unit = "caml_gr_clear_graph"

external size_x : unit -> int = "caml_gr_size_x"

external size_y : unit -> int = "caml_gr_size_y"

(* Double-buffering *)

external display_mode : bool -> unit = "caml_gr_display_mode"

external remember_mode : bool -> unit = "caml_gr_remember_mode"

external synchronize : unit -> unit = "caml_gr_synchronize"

let auto_synchronize = function
  | true ->
      display_mode true;
      remember_mode true;
      synchronize ()
  | false ->
      display_mode false;
      remember_mode true

(* Colors *)

type color = int

type color_scheme =
  | Blank
  | Red of int
  | Orange of int
  | Yellow of int
  | LightGreen of int
  | Green of int
  | BlueGreen of int
  | Skyblue of int
  | LightBlue of int
  | Blue of int
  | Purple of int
  | Pink of int
  | RedPink of int
  | BlackWhite of int

let rgb r g b = (r lsl 16) + (g lsl 8) + b

external set_color : color -> unit = "caml_gr_set_color"

let schme_to_color = function
  | Blank -> -1
  | Red 1 -> rgb 255 204 204
  | Red 2 -> rgb 255 153 153
  | Red 3 -> rgb 255 102 102
  | Red 4 -> rgb 255 51 51
  | Red 5 -> rgb 255 0 0
  | Red 6 -> rgb 204 0 0
  | Red 7 -> rgb 153 0 0
  | Red 8 -> rgb 102 0 0
  | Red 9 -> rgb 51 0 0
  | Red _ -> failwith "Red color scheme int must be between 1 and 9 inclusive."
  | Orange 1 -> rgb 255 229 204
  | Orange 2 -> rgb 255 204 153
  | Orange 3 -> rgb 255 178 102
  | Orange 4 -> rgb 255 153 51
  | Orange 5 -> rgb 255 128 0
  | Orange 6 -> rgb 204 102 0
  | Orange 7 -> rgb 153 76 0
  | Orange 8 -> rgb 102 51 0
  | Orange 9 -> rgb 51 25 0
  | Orange _ ->
      failwith "Orange color scheme int must be between 1 and 9 inclusive."
  | Yellow 1 -> rgb 255 255 204
  | Yellow 2 -> rgb 255 255 153
  | Yellow 3 -> rgb 255 255 102
  | Yellow 4 -> rgb 255 255 51
  | Yellow 5 -> rgb 255 255 0
  | Yellow 6 -> rgb 204 204 0
  | Yellow 7 -> rgb 153 153 0
  | Yellow 8 -> rgb 102 102 0
  | Yellow 9 -> rgb 51 51 0
  | Yellow _ ->
      failwith "Yellow color scheme int must be between 1 and 9 inclusive."
  | LightGreen 1 -> rgb 229 255 204
  | LightGreen 2 -> rgb 204 255 153
  | LightGreen 3 -> rgb 178 255 102
  | LightGreen 4 -> rgb 153 255 51
  | LightGreen 5 -> rgb 128 255 0
  | LightGreen 6 -> rgb 102 204 0
  | LightGreen 7 -> rgb 76 153 0
  | LightGreen 8 -> rgb 51 102 0
  | LightGreen 9 -> rgb 25 51 0
  | LightGreen _ ->
      failwith "LightGreen color scheme int must be between 1 and 9 inclusive."
  | Green 1 -> rgb 204 255 204
  | Green 2 -> rgb 153 255 153
  | Green 3 -> rgb 102 255 102
  | Green 4 -> rgb 204 255 204
  | Green 5 -> rgb 0 255 0
  | Green 6 -> rgb 0 204 0
  | Green 7 -> rgb 0 153 0
  | Green 8 -> rgb 0 102 0
  | Green 9 -> rgb 0 51 0
  | Green _ ->
      failwith "Green color scheme int must be between 1 and 9 inclusive."
  | BlueGreen 1 -> rgb 204 255 229
  | BlueGreen 2 -> rgb 153 255 204
  | BlueGreen 3 -> rgb 102 255 178
  | BlueGreen 4 -> rgb 51 255 153
  | BlueGreen 5 -> rgb 0 255 128
  | BlueGreen 6 -> rgb 0 204 102
  | BlueGreen 7 -> rgb 0 153 76
  | BlueGreen 8 -> rgb 0 102 51
  | BlueGreen 9 -> rgb 0 51 25
  | BlueGreen _ ->
      failwith "BlueGreen color scheme int must be between 1 and 9 inclusive."
  | Skyblue 1 -> rgb 204 255 255
  | Skyblue 2 -> rgb 153 255 255
  | Skyblue 3 -> rgb 102 255 255
  | Skyblue 4 -> rgb 51 255 255
  | Skyblue 5 -> rgb 0 255 255
  | Skyblue 6 -> rgb 0 204 204
  | Skyblue 7 -> rgb 0 153 153
  | Skyblue 8 -> rgb 0 102 102
  | Skyblue 9 -> rgb 0 51 51
  | Skyblue _ ->
      failwith "Skyblue color scheme int must be between 1 and 9 inclusive."
  | LightBlue 1 -> rgb 204 229 255
  | LightBlue 2 -> rgb 153 204 255
  | LightBlue 3 -> rgb 102 178 255
  | LightBlue 4 -> rgb 51 153 255
  | LightBlue 5 -> rgb 0 128 255
  | LightBlue 6 -> rgb 0 102 204
  | LightBlue 7 -> rgb 0 76 153
  | LightBlue 8 -> rgb 0 51 102
  | LightBlue 9 -> rgb 0 25 51
  | LightBlue _ ->
      failwith "LightBlue color scheme int must be between 1 and 9 inclusive."
  | Blue 1 -> rgb 204 204 255
  | Blue 2 -> rgb 153 153 255
  | Blue 3 -> rgb 102 102 255
  | Blue 4 -> rgb 51 51 255
  | Blue 5 -> rgb 0 0 255
  | Blue 6 -> rgb 0 0 204
  | Blue 7 -> rgb 0 0 153
  | Blue 8 -> rgb 0 0 102
  | Blue 9 -> rgb 0 0 51
  | Blue _ ->
      failwith "Blue color scheme int must be between 1 and 9 inclusive."
  | Purple 1 -> rgb 229 204 255
  | Purple 2 -> rgb 204 153 255
  | Purple 3 -> rgb 178 102 255
  | Purple 4 -> rgb 153 51 255
  | Purple 5 -> rgb 127 0 255
  | Purple 6 -> rgb 102 0 204
  | Purple 7 -> rgb 76 0 153
  | Purple 8 -> rgb 51 0 102
  | Purple 9 -> rgb 25 0 51
  | Purple _ ->
      failwith "Purple color scheme int must be between 1 and 9 inclusive."
  | Pink 1 -> rgb 255 204 255
  | Pink 2 -> rgb 255 153 255
  | Pink 3 -> rgb 255 102 255
  | Pink 4 -> rgb 255 51 255
  | Pink 5 -> rgb 255 0 255
  | Pink 6 -> rgb 204 0 204
  | Pink 7 -> rgb 153 0 153
  | Pink 8 -> rgb 102 0 102
  | Pink 9 -> rgb 51 0 51
  | Pink _ ->
      failwith "Pink color scheme int must be between 1 and 9 inclusive."
  | RedPink 1 -> rgb 255 204 229
  | RedPink 2 -> rgb 255 153 204
  | RedPink 3 -> rgb 255 102 178
  | RedPink 4 -> rgb 255 51 153
  | RedPink 5 -> rgb 255 0 127
  | RedPink 6 -> rgb 204 0 102
  | RedPink 7 -> rgb 153 0 76
  | RedPink 8 -> rgb 102 0 51
  | RedPink 9 -> rgb 51 0 25
  | RedPink _ ->
      failwith "RedPink color scheme int must be between 1 and 9 inclusive."
  | BlackWhite 1 -> rgb 255 255 255
  | BlackWhite 2 -> rgb 224 224 224
  | BlackWhite 3 -> rgb 192 192 192
  | BlackWhite 4 -> rgb 160 160 160
  | BlackWhite 5 -> rgb 128 128 128
  | BlackWhite 6 -> rgb 96 96 96
  | BlackWhite 7 -> rgb 64 64 64
  | BlackWhite 8 -> rgb 32 32 32
  | BlackWhite 9 -> rgb 0 0 0
  | BlackWhite _ ->
      failwith "BlackWhite color scheme int must be between 1 and 9 inclusive."

let black = 0x000000

and white = 0xFFFFFF

and red = 0xFF0000

and green = 0x00FF00

and blue = 0x0000FF

and yellow = 0xFFFF00

and cyan = 0x00FFFF

and magenta = 0xFF00FF

let background = white

and foreground = black

(* Drawing *)

external plot : int -> int -> unit = "caml_gr_plot"

let plots points =
  for i = 0 to Array.length points - 1 do
    let x, y = points.(i) in
    plot x y
  done

external point_color : int -> int -> color = "caml_gr_point_color"

external moveto : int -> int -> unit = "caml_gr_moveto"

external current_x : unit -> int = "caml_gr_current_x"

external current_y : unit -> int = "caml_gr_current_y"

let current_point () = (current_x (), current_y ())

external lineto : int -> int -> unit = "caml_gr_lineto"

let rlineto x y = lineto (current_x () + x) (current_y () + y)

let rmoveto x y = moveto (current_x () + x) (current_y () + y)

external raw_draw_rect : int -> int -> int -> int -> unit = "caml_gr_draw_rect"

let draw_rect x y w h =
  if w < 0 || h < 0 then raise (Invalid_argument "draw_rect")
  else raw_draw_rect x y w h

let draw_poly, draw_poly_line =
  let dodraw close_flag points =
    if Array.length points > 0 then (
      let savex, savey = current_point () in
      moveto (fst points.(0)) (snd points.(0));
      for i = 1 to Array.length points - 1 do
        let x, y = points.(i) in
        lineto x y
      done;
      if close_flag then lineto (fst points.(0)) (snd points.(0));
      moveto savex savey)
  in
  (dodraw true, dodraw false)

let draw_segments segs =
  let savex, savey = current_point () in
  for i = 0 to Array.length segs - 1 do
    let x1, y1, x2, y2 = segs.(i) in
    moveto x1 y1;
    lineto x2 y2
  done;
  moveto savex savey

external raw_draw_arc : int -> int -> int -> int -> int -> int -> unit
  = "caml_gr_draw_arc" "caml_gr_draw_arc_nat"

let draw_arc x y rx ry a1 a2 =
  if rx < 0 || ry < 0 then raise (Invalid_argument "draw_arc/ellipse/circle")
  else raw_draw_arc x y rx ry a1 a2

let draw_ellipse x y rx ry = draw_arc x y rx ry 0 360

let draw_circle x y r = draw_arc x y r r 0 360

external raw_set_line_width : int -> unit = "caml_gr_set_line_width"

let set_line_width w =
  if w < 0 then raise (Invalid_argument "set_line_width")
  else raw_set_line_width w

external raw_fill_rect : int -> int -> int -> int -> unit = "caml_gr_fill_rect"

let fill_rect x y w h =
  if w < 0 || h < 0 then raise (Invalid_argument "fill_rect")
  else raw_fill_rect x y w h

external fill_poly : (int * int) array -> unit = "caml_gr_fill_poly"

external raw_fill_arc : int -> int -> int -> int -> int -> int -> unit
  = "caml_gr_fill_arc" "caml_gr_fill_arc_nat"

let fill_arc x y rx ry a1 a2 =
  if rx < 0 || ry < 0 then raise (Invalid_argument "fill_arc/ellipse/circle")
  else raw_fill_arc x y rx ry a1 a2

let fill_ellipse x y rx ry = fill_arc x y rx ry 0 360

let fill_circle x y r = fill_arc x y r r 0 360

(* Text *)

external draw_char : char -> unit = "caml_gr_draw_char"

external draw_string : string -> unit = "caml_gr_draw_string"

external set_font : string -> unit = "caml_gr_set_font"

external set_text_size : int -> unit = "caml_gr_set_text_size"

external text_size : string -> int * int = "caml_gr_text_size"

(* Images *)

type image

let transp = -1

external make_image : color array array -> image = "caml_gr_make_image"

external dump_image : image -> color array array = "caml_gr_dump_image"

external draw_image : image -> int -> int -> unit = "caml_gr_draw_image"

external create_image : int -> int -> image = "caml_gr_create_image"

external blit_image : image -> int -> int -> unit = "caml_gr_blit_image"

let get_image x y w h =
  let image = create_image w h in
  blit_image image x y;
  image

(* Events *)

type status = {
  mouse_x : int;
  mouse_y : int;
  button : bool;
  keypressed : bool;
  key : char;
}

type event = Button_down | Button_up | Key_pressed | Mouse_motion | Poll

external wait_next_event : event list -> status = "caml_gr_wait_event"

let mouse_pos () =
  let e = wait_next_event [ Poll ] in
  (e.mouse_x, e.mouse_y)

let button_down () =
  let e = wait_next_event [ Poll ] in
  e.button

let read_key () =
  let e = wait_next_event [ Key_pressed ] in
  e.key

let key_pressed () =
  let e = wait_next_event [ Poll ] in
  e.keypressed

let loop_at_exit events handler =
  let events = List.filter (fun e -> e <> Poll) events in
  at_exit (fun _ ->
      try
        while true do
          let e = wait_next_event events in
          handler e
        done
      with
      | Exit -> close_graph ()
      | e ->
          close_graph ();
          raise e)

(*** Sound *)

external sound : int -> int -> unit = "caml_gr_sound"

(* Splines *)
let sub (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)

and middle (x1, y1) (x2, y2) = ((x1 +. x2) /. 2.0, (y1 +. y2) /. 2.0)

and area (x1, y1) (x2, y2) = abs_float ((x1 *. y2) -. (x2 *. y1))

and norm (x1, y1) = sqrt ((x1 *. x1) +. (y1 *. y1))

let test a b c d =
  let v = sub d a in
  let s = norm v in
  area v (sub a b) <= s && area v (sub a c) <= s

let spline a b c d =
  let rec spl accu a b c d =
    if test a b c d then d :: accu
    else
      let a' = middle a b and o = middle b c in
      let b' = middle a' o and d' = middle c d in
      let c' = middle o d' in
      let i = middle b' c' in
      spl (spl accu a a' b' i) i c' d' d
  in
  spl [ a ] a b c d

let curveto b c ((x, y) as d) =
  let float_point (x, y) = (float_of_int x, float_of_int y) in
  let round f = int_of_float (f +. 0.5) in
  let int_point (x, y) = (round x, round y) in
  let points =
    spline
      (float_point (current_point ()))
      (float_point b) (float_point c) (float_point d)
  in
  draw_poly_line (Array.of_list (List.map int_point points));
  moveto x y
