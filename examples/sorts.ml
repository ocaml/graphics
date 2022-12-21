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

(* Animation of sorting algorithms. *)

open Graphics

(* Information on a given sorting process *)

type graphic_context = {
  id: int;                (* Identifier *)
  array : int array;      (* Data to sort *)
  x0 : int;               (* X coordinate, lower left corner *)
  y0 : int;               (* Y coordinate, lower left corner *)
  width : int;            (* Width in pixels *)
  height : int;           (* Height in pixels *)
  nelts : int;            (* Number of elements in the array *)
  maxval : int;           (* Max val in the array + 1 *)
  rad : int               (* Dimension of the rectangles *)
}

(* Array assignment and exchange with screen update *)

let screen_mutex = Mutex.create ()

let draw gc i v =
  fill_rect
    (gc.x0 + (gc.width * i / gc.nelts))
    (gc.y0 + (gc.height * v / gc.maxval))
    gc.rad gc.rad

let assign gc i v =
  Mutex.lock screen_mutex;
  set_color background;
  draw gc i gc.array.(i);
  set_color foreground;
  draw gc i v;
  gc.array.(i) <- v;
  Mutex.unlock screen_mutex

let exchange gc i j =
  let val_i = gc.array.(i) in
  assign gc i gc.array.(j);
  assign gc j val_i

(* Construction of a graphic context *)

let initialize i name array maxval x y w h =
  let _, label_height = text_size name in
  let rad = ((w - 2) / Array.length array) - 1 in
  let gc =
    {
      id = i;
      array = Array.copy array;
      x0 = x + 1;
      (* Leave one pixel left for Y axis *)
      y0 = y + 1;
      (* Leave one pixel below for X axis *)
      width = w - 2;
      (* 1 pixel left, 1 pixel right *)
      height = h - 1 - label_height - rad;
      nelts = Array.length array;
      maxval;
      rad;
    }
  in
  moveto (gc.x0 - 1) (gc.y0 + gc.height);
  lineto (gc.x0 - 1) (gc.y0 - 1);
  lineto (gc.x0 + gc.width) (gc.y0 - 1);
  moveto (gc.x0 - 1) (gc.y0 + gc.height);
  draw_string name;
  for i = 0 to Array.length array - 1 do
    draw gc i array.(i)
  done;
  gc

(* Synchronization barrier *)

type barrier = {
  lock: Mutex.t;
  mutable participants: int; (* number of participating threads *)
  mutable flag: bool;   (* true = can go ahead, false = should wait *)
  mutable arrive: int;  (* number of threads that arrived *)
  mutable leave: int;   (* number of threads that left *)
  restart: Condition.t; (* signaled when status becomes false again *)
  all_arrived: Condition.t; (* signaled when arrive = participants *)
  all_left: Condition.t (* signaled when leave = participants *)
}

let b = {
  lock = Mutex.create();
  participants = 0;
  flag = true;
  arrive = 0;
  leave = 0;
  restart = Condition.create();
  all_arrived = Condition.create();
  all_left = Condition.create()
}

let barrier_init num_participants =
  Mutex.lock b.lock;
  b.participants <- num_participants;
  b.leave <- num_participants;
  Mutex.unlock b.lock

let barrier_enter () =
  Mutex.lock b.lock;
  if b.arrive = 0 then begin
    (* Wait for all to leave before clearing flag *)
    while b.leave < b.participants do Condition.wait b.all_left b.lock done;
    (* First arriver clears flag *)
    b.flag <- false
  end;
  b.arrive <- b.arrive + 1;
  if b.arrive = b.participants then begin
    (* Last arriver signals the manager, who will set the flag *)
    Condition.signal b.all_arrived
  end;
  (* Wait for flag *)
  while not b.flag do Condition.wait b.restart b.lock done;
  b.leave <- b.leave + 1;
  if b.leave = b.participants then Condition.broadcast b.all_left;
  Mutex.unlock b.lock
    
let barrier_terminate () =
  Mutex.lock b.lock;
  b.participants <- b.participants - 1;
  if b.arrive = b.participants then Condition.signal b.all_arrived;
  Mutex.unlock b.lock

let barrier_wait_all () =
  Mutex.lock b.lock;
  while b.arrive <> b.participants do Condition.wait b.all_arrived b.lock done
  (* keep the lock *)

let barrier_restart_all () =
  (* lock must be held *)
  b.arrive <- 0;
  b.leave <- 0;
  b.flag <- true;
  Condition.broadcast b.restart;
  Mutex.unlock b.lock

(* To stop all threads cleanly *)

let terminated = ref false

exception Terminated

(* Main animation function *)

let delta_t = 0.02

let display functs nelts maxval =
  let a = Array.make nelts 0 in
  for i = 0 to nelts - 1 do
    a.(i) <- Random.int maxval
  done;
  barrier_init (Array.length functs);
  terminated := false;
  let th =
    Array.mapi
      (fun i (name, funct, x, y, w, h) ->
        let gc = initialize i name a maxval x y w h in
        Thread.create
          (fun () -> try funct gc; barrier_terminate() with Terminated -> ())
          ())
      functs in
  let delay = ref (3.0 *. delta_t) in
  while not !terminated do
    barrier_wait_all();
    if b.participants = 0 then begin
      ignore (read_key());
      terminated := true
    end;
    Unix.sleepf !delay;
    if key_pressed() then begin
      match read_key() with
      | 'q'|'Q' ->
          terminated := true
      | '0'..'9' as c ->
          delay := float (Char.code c - 48) *. delta_t
      | _ ->
          ()
    end;
    barrier_restart_all()
  done;
  Array.iter Thread.join th

(* Comparison functions that synchronize *)

let sync () = 
  if !terminated then raise Terminated else barrier_enter ()

let (<?) x y = sync(); x < y
let (>?) x y = sync(); x > y
let (<=?) x y = sync(); x <= y
let (>=?) x y = sync(); x >= y

(* The sorting functions. *)

(* Bubble sort *)

let bubble_sort gc =
  let ordered = ref false in
  while not !ordered do
    ordered := true;
    for i = 0 to Array.length gc.array - 2 do
      if gc.array.(i + 1) <? gc.array.(i) then (
        exchange gc i (i + 1);
        ordered := false)
    done
  done

(* Insertion sort *)

let insertion_sort gc =
  for i = 1 to Array.length gc.array - 1 do
    let val_i = gc.array.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && val_i <? gc.array.(!j) do
      assign gc (!j + 1) gc.array.(!j);
      decr j
    done;
    assign gc (!j + 1) val_i
  done

(* Selection sort *)

let selection_sort gc =
  for i = 0 to Array.length gc.array - 1 do
    let min = ref i in
    for j = i + 1 to Array.length gc.array - 1 do
      if gc.array.(j) <? gc.array.(!min) then min := j
    done;
    exchange gc i !min
  done

(* Quick sort *)

let quick_sort gc =
  let rec quick lo hi =
    if lo < hi then (
      let i = ref lo in
      let j = ref hi in
      let pivot = gc.array.(hi) in
      while !i < !j do
        while !i < hi && gc.array.(!i) <=? pivot do
          incr i
        done;
        while !j > lo && gc.array.(!j) >=? pivot do
          decr j
        done;
        if !i < !j then exchange gc !i !j
      done;
      exchange gc !i hi;
      quick lo (!i - 1);
      quick (!i + 1) hi)
  in
  quick 0 (Array.length gc.array - 1)

(* Heap sort *)

let rec heapify gc n i =
  let l = 2 * i + 1 and r = 2 * i + 2 in
  (* Find largest among root, left child, right child *)
  let largest = ref i in
  if l < n && gc.array.(l) >? gc.array.(!largest) then largest := l;
  if r < n && gc.array.(r) >? gc.array.(!largest) then largest := r;
  if !largest <> i then begin
    (* Swap and continue heapify-ing *)
    exchange gc i !largest;
    heapify gc n !largest
  end

let heap_sort gc =
  let n = Array.length gc.array in
  (* Build max heap *)
  for i = n / 2 - 1 downto 0 do
    heapify gc n i
  done;
  (* Repeatedly extract max element and restore heap structure *)
  for i = n - 1 downto 1 do
    exchange gc 0 i;
    heapify gc i 0
  done

(* Merge sort *)

let merge_sort gc =
  let rec merge i l1 l2 =
    match (l1, l2) with
    | [], [] -> ()
    | [], v2 :: r2 ->
        assign gc i v2;
        merge (i + 1) l1 r2
    | v1 :: r1, [] ->
        assign gc i v1;
        merge (i + 1) r1 l2
    | v1 :: r1, v2 :: r2 ->
        if v1 <? v2 then (
          assign gc i v1;
          merge (i + 1) r1 l2)
        else (
          assign gc i v2;
          merge (i + 1) l1 r2)
  in
  let rec msort start len =
    if len < 2 then ()
    else
      let m = len / 2 in
      msort start m;
      msort (start + m) (len - m);
      merge start
        (Array.to_list (Array.sub gc.array start m))
        (Array.to_list (Array.sub gc.array (start + m) (len - m)))
  in
  msort 0 (Array.length gc.array)

(* Main program *)

let animate () =
  Random.self_init();
  open_graph "";
  moveto 0 0;
  draw_string "Resize window, then press a key to start...";
  ignore (read_key () : char);
  clear_graph ();
  let prompt = "0: fastest ... 9: slowest, press 'q' to quit" in
  moveto 0 0;
  draw_string prompt;
  let _, h = text_size prompt in
  let sx = size_x () / 2 and sy = (size_y () - h) / 3 in
  display
       [|
         ("Bubble", bubble_sort, 0, h, sx, sy);
         ("Insertion", insertion_sort, 0, h + sy, sx, sy);
         ("Selection", selection_sort, 0, h + (2 * sy), sx, sy);
         ("Quicksort", quick_sort, sx, h, sx, sy);
         ("Heapsort", heap_sort, sx, h+sy, sx, sy);
         ("Mergesort", merge_sort, sx, h + (2 * sy), sx, sy);
       |]
       100 1000;
  close_graph ()

let _ = animate ()
