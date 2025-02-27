
open Graphics

let () =
  open_graph "";
  set_font "monospace";
  moveto 10 200;
  draw_string "Hello, graphics";
  set_font "monospace-20";
  moveto 10 130;
  draw_string "20 point type";
  set_font "monospace-40";
  moveto 10 10;
  draw_string "40 point type";
  ignore (wait_next_event [Key_pressed])
