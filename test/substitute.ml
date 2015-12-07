module Z3 = ZZ3.Make (struct let ctx = Z3.mk_context [] end)
open Z3

let () =
  Printf.printf "\nSubstitution test\n%!";

  let x : (Z.t, zint) symbol = Symbol.declare Int "x" in
  let y = Symbol.declare Int "y" in
  let a = Symbol.declare Int "a" in
  let b = Symbol.declare Real "b" in

  (* This assignment is type safe because both `a` and `b` are
     typed as `znum term`.  Be careful that the following substitution
     causes a runtime error:

     let t = T.substitute ~assigns:[ (T.symbol a, T.rat (Q.of_int 2)) ] t
  *)
  let t = T.( !x + !y >= !a && !x - !y >= !b) in
  let assigns = [ (T.symbol a, T.int 1)
                ; (T.symbol b, T.rat (Q.of_int 2))
                ]
  in
  let t = T.substitute ~assigns t in

  let solver = Solver.make None in

  Solver.add ~solver t;

  match Solver.check ~solver [] with
  | Unsat _ | Unkown _ -> failwith "Oh noees"
  | Sat (lazy model) ->
    let vx = Model.get_value ~model x
    and vy = Model.get_value ~model y
    in
    Printf.printf "x = %d\ny = %d\n" (Z.to_int vx) (Z.to_int vy)
      
