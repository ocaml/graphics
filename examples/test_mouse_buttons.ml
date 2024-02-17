open Graphics;;

open_graph "";;

try
  while true do
    let st = wait_next_event [ Button_down; Button_up; Key_pressed ] in
    if st.keypressed then raise Exit;
    List.iter
      (fun (b,v) ->
        Printf.printf "button %i %s\n" b (if v then "vrai" else "faux"))
      [ (1, st.button1); (2, st.button2); (3, st.button3);
        (4, st.button4); (5, st.button5) ];
    print_newline()
  done
with Exit -> ()
