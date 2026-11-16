open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
(* clone_nodes récursif avec liste d'accumulateur/construction *)
let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = e_fold gr (fun b_graph arc -> new_arc b_graph {arc with lbl = f arc.lbl}) (clone_nodes gr)