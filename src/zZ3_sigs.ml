(** Signatures for z3overlay. *)

(** Input signature of the functor, only a context. *)
module type Context = sig

  val ctx : Z3.context

end

module type SOLVER = sig

  type t
  type sat
  type _ term

  val make : Z3.Symbol.symbol option -> t

  val push : t -> unit
  val pop : t -> unit

  val add : solver:t -> [`Bool] term -> unit
  val check : solver:t -> [`Bool] term list -> sat

end

(** Output signature of the functor. *)
module type S = sig

  val ctx : Z3.context

  type zint  = [ `Int ]
  type zbool = [ `Bool ]
  type zreal = [ `Real ]

  type znum = [ zint | zreal ]
  type zany = [ zint | zbool | zreal ]

  type ('domain, 'range) zarray = [ `Zarray of ('domain * 'range) ]

  type (_,_) typ =
    | Int : (Z.t, [> zint]) typ
    | Bool : (bool, [> zbool]) typ
    | Real : (Q.t, [> zreal]) typ
    | Num : (Q.t, [> znum] ) typ
    | Array : ('a, 'x) typ * ('b, 'y) typ -> ('a -> 'b, ('x, 'y) zarray ) typ

  type +'a term = private Z3.Expr.expr

  type ('a,'b) symbol

  module Symbol : sig

    val get_typ : ('a, 'b) symbol -> ('a, 'b) typ

    val declare : ('a, 'b) typ -> string -> ('a, 'b) symbol

    val term : ('a, 'b) typ -> 'b term -> ('a, 'b) symbol

    (** Unsafe cast. Use at your own risks. *)
    val trustme : ('a, 'b) typ -> Z3.Expr.expr -> ('a, 'b) symbol
  end

  (** Term constructors. Direct calls to the Z3 api. *)
  module T : sig

    val symbol : (_,'a) symbol -> 'a term
    val simplify : ?params:Z3.Params.params -> 'a term -> 'a term
    val eq : 'a term -> 'a term -> [> zbool] term
    val distinct : 'a term list -> [> zbool] term
    val ite : [< zbool ] term -> ([< zany ] as 'a) term -> 'a term -> 'a term

    val int : int -> [> zint ] term
    val bigint : Z.t -> [> zint ] term
    val rat : Q.t -> [> zreal ] term
    val i2q : [< zint ] term -> [> zreal ] term
    val q2i : [< zreal ] term -> [> zint ] term

    val true_ : [> zbool ] term
    val false_ : [> zbool ] term
    val bool : bool -> [> zbool ] term
    val and_ : [< zbool ] term list -> [> zbool ] term
    val or_ : [< zbool ] term list -> [> zbool ] term
    val not : [< zbool ] term -> [> zbool ] term
    val imply : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val iff : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val xor : [< zbool ] term -> [< zbool ] term -> [> zbool ] term

    val ge : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val le : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val gt : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val lt : [< znum ] term -> [< znum ] term -> [> zbool ] term

    val neg : ([< znum ] as 'a) term -> 'a term
    val add : ([< znum ] as 'a) term list -> 'a term
    val sub : ([< znum ] as 'a) term list -> 'a term
    val mul : ([< znum ] as 'a) term list -> 'a term
    val ixor : [< zint ] term -> [< zint ] term -> [> zint ] term

    val div : ([< znum ] as 'a) term -> 'a term -> 'a term
    val mod_ : [< zint ] term -> [< zint ] term -> [> zint ] term
    val rem : [< znum ] term -> [< znum ] term -> [> znum ] term

    val pow : ([< znum ] as 'a) term -> 'a term -> 'a term

    val ( ! ) : (_,'a) symbol -> 'a term
    val ( = ) : 'a term -> 'a term -> [> zbool] term
    val ( <> ) : 'a term -> 'a term -> [> zbool] term

    val ( && )   : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val ( || )   : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val ( <=> )  : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val ( ==> )  : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val ( lxor ) : [< zbool ] term -> [< zbool ] term -> [> zbool ] term

    val ( < )  : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val ( <= ) : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val ( > )  : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val ( >= ) : [< znum ] term -> [< znum ] term -> [> zbool ] term

    val ( + ) : ([< znum ] as 'a) term -> 'a term -> 'a term
    val ( - ) : ([< znum ] as 'a) term -> 'a term -> 'a term
    val ( * ) : ([< znum ] as 'a) term -> 'a term -> 'a term
    val ( / ) : ([< znum ] as 'a) term -> 'a term -> 'a term
    val ( ^ ) : ([< znum ] as 'a) term -> 'a term -> 'a term

    val ( mod ) : [< zint ] term -> [< zint ] term -> [> zint ] term

    val with_typ : ('a, 'b) typ -> 'a -> 'b term

    val to_string : 'a term -> string
    val raw : 'a term -> Z3.Expr.expr

    val substitute_one : assign:('b term * 'b term) -> 'a term -> 'a term
    (** `substitute` doesn't allow heterogeneous assignments. *)
    val substitute : assigns:('b term * 'b term) list -> 'a term -> 'a term

  end

  module Z3Array : sig
    val get : [< ('d, 'r) zarray] term -> 'd term -> 'r term

    val set :
      [< ('d, 'r) zarray] term -> 'd term -> 'r term -> [> ('d, 'r) zarray] term
    val make :
      ('a -> 'b, ('d, 'r) zarray) typ -> 'r term -> [> ('d, 'r) zarray] term

    val default : [< ('d, 'r) zarray] term -> 'r term

    val of_indexed :
      typ:('a, 'r) typ -> default:'r term ->
      'r term array -> ([> zint ], 'r) zarray term

    val of_array :
      typ:('a -> 'b, ('d, 'r) zarray) typ -> default:'r term ->
      ('d term * 'r term) array -> ('d, 'r) zarray term

    val of_list :
      typ:('a -> 'b, ('d, 'r) zarray) typ -> default:'r term ->
      ('d term * 'r term) list -> ('d, 'r) zarray term

  end


  type sat =
    | Unsat of Z3.Expr.expr Lazy.t (** Proof *)
    | Sat of Z3.Model.model Lazy.t (** Model *)
    | Unkown of string (** Reason *)

  module Solver : SOLVER
    with type t = Z3.Solver.solver
     and type sat := sat
     and type 'a term := 'a term

  module Optimize : sig

    include SOLVER
      with type t = Z3.Optimize.optimize
       and type sat := sat
       and type 'a term := 'a term

    type handle

    val make : unit -> t

    val add_soft :
      id:Z3.Symbol.symbol ->
      solver:t ->
      weight:string -> zbool term -> handle

    val maximize :
      solver:t ->
      [< znum] term -> handle
    val minimize :
      solver:t ->
      [< znum] term -> handle

    val get_upper :
      objective:handle -> int -> (Q.t, [> znum ]) symbol

    val get_lower :
      objective:handle -> int -> (Q.t, [> znum ]) symbol

  end

  module Model : sig

    val get_value : model:Z3.Model.model -> ('a, 'b) symbol -> 'a

  end

end
