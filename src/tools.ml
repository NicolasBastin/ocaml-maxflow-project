open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
(* clone_nodes récursif avec liste d'accumulateur/construction *)
let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = e_fold gr (fun b_graph arc -> new_arc b_graph {arc with lbl = f arc.lbl}) (clone_nodes gr)

let add_arc gr n1 n2 v =
  match find_arc gr n1 n2 with
  |Some arc -> new_arc gr {arc with lbl = arc.lbl + v}
  |None -> new_arc gr {src=n1 ; tgt= n2 ; lbl=v } 

let trouve_chemin gr source puit = 
  let rec aux gr id acu liste_arcs =
    match liste_arcs with
    |[] -> 
    |x::rest -> if (List.mem x acu) then aux gr id acu rest else 
      if x.tgt=puit then acu else aux gr (x.tgt) (x::acu) (out_arcs gr x.tgt)
  in aux gr source [] (out_arcs gr source)



  (*
  edge(a, b).
  edge(b, c).
  edge(c, d).
  
  path(X, Y) :- edge(X, Z), path(Z,Y).
  *)