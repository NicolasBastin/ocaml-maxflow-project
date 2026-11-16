open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
(* clone_nodes récursif avec liste d'accumulateur/construction *)
let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = e_fold gr (fun b_graph arc -> new_arc b_graph {arc with lbl = f arc.lbl}) (clone_nodes gr)

let add_arc gr n1 n2 v =
  match find_arc gr n1 n2 with
  |Some arc -> new_arc gr (arc with arc.lbl += v)
  |None -> new_arc gr 
