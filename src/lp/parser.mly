%{
open Lpfile
let rev_poly tl = Poly.of_term_list (List.rev tl)
let rev_neg_head_poly tl = match List.rev tl with
    | hd :: rest -> Poly.of_term_list (Term.neg hd :: rest)
    | _-> failwith "empty polynomial expression"

(* avoid dropping non-zero terms by simplification *)
let eps = min_float
%}

%token <float> NUM
%token <string> ID
%token MIN MAX ST BOUND GENERAL BINARY FREE INF END
%token COLON EQ LT GT
%token PLUS MINUS TIMES SQ PLB MLB

%start <Lpfile.t> sections

%%

sections :
  o = objective c = cnstrs b = bounds v = vtypes END
    { o @ c @ b @ v }


objective: (* objective section cannot be empty *)
  | MIN p = poly { [Sobj (Objective.minimize ~eps (Poly.half_quad p))] }
  | MAX p = poly { [Sobj (Objective.maximize ~eps (Poly.half_quad p))] }

cnstrs: (* constraints section cannot be empty *)
  ST l = nonempty_list(cnstr) { [Scnstr l] }

cnstr:
  | l = ID COLON p = poly EQ rhs = const { Cnstr.eq ~eps ~name:l p rhs }
  | l = ID COLON p = poly LT rhs = const { Cnstr.lt ~eps ~name:l p rhs }
  | l = ID COLON p = poly GT rhs = const { Cnstr.gt ~eps ~name:l p rhs }
  | p = poly EQ rhs = const { Cnstr.eq ~eps p rhs }
  | p = poly LT rhs = const { Cnstr.lt ~eps p rhs }
  | p = poly GT rhs = const { Cnstr.gt ~eps p rhs }

const :
  n = signed { Poly.c n }

poly :
  | p = poly_in { p }
  | PLB p = poly_in { p }
  | MLB p = poly_in { Poly.neg p }
  | p0 = poly_in PLB p1 = poly_in { Poly.(p0 ++ p1) }
  | p0 = poly_in MLB p1 = poly_in { Poly.(p0 -- p1) }

poly_in :
  | p = poly_rev { rev_poly p }
  | PLUS p = poly_rev { rev_poly p }
  | MINUS p = poly_rev { rev_neg_head_poly p }

poly_rev :
  | t = term { [t] }
  | p = poly_rev PLUS  t = term { t :: p }
  | p = poly_rev MINUS t = term { Term.neg t :: p }

term :
  | n = NUM { Term.Const n }
  | v = ID { Term.Linear (Float.one, Var.make v) }
  | n = NUM  v = ID { Term.Linear (n, Var.make v) }
  | v0 = ID TIMES v1 = ID { Term.Quad (Float.one, Var.make v0, Var.make v1) }
  | n = NUM v0 = ID TIMES v1 = ID { Term.Quad (n, Var.make v0, Var.make v1) }
  | v = ID SQ { Term.Quad (Float.one, Var.make v, Var.make v) }
  | n = NUM  v = ID SQ { Term.Quad (n, Var.make v, Var.make v) }

bounds:
  | (*empty*) { [] }
  | BOUND l = nonempty_list(bound) { [Sbound l] }

bound:
  | v = ID FREE { {name=v; lb=Float.neg_infinity; ub=Float.infinity} }
  | lb = lower LT v = ID { {name=v; lb; ub=Float.infinity} }
  | v = ID LT ub = upper { {name=v; lb=Float.zero; ub} }
  | lb = lower LT v = ID LT ub = upper { {name=v; lb; ub} }

lower:
  | s = signed { s }
  | MINUS INF { Float.neg_infinity }

upper:
  | s = signed { s }
  | INF { Float.infinity }
  | PLUS INF { Float.infinity }

signed:
  | n = NUM { n }
  | PLUS n = NUM { n }
  | MINUS n = NUM { Float.neg n }

vtypes:
  l = list(vtype) { l }

vtype:
  | GENERAL l = nonempty_list(ID) { Sgeneral l }
  | BINARY l = nonempty_list(ID)  { Sbinary l }
