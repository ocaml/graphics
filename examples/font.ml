
open Graphics

let () =
  open_graph "";
  set_font "Liberation Mono";
  moveto 10 10;
  draw_string "Hello, graphics";
  ignore (wait_next_event [Key_pressed])
