%{
open Lpfile
module Cnstr = Constraint
%}

%token <float> NUM
%token <string> ID
%token MIN MAX ST BOUND GENERAL BINARY FREE
%token COLON EQ LT GT
%token PLUS MINUS TIMES SQ
%token END

%start <Lpfile.problem> problem

%%

problem :
  o = objective c = cnstrs b = bounds v = vtype END
    { o @ c @ b @ v }


objective:
  | (*empty*) { [Sobj (Objective.No_obj)] }
  | MIN p = poly { [Sobj (Objective.Min p)] }
  | MAX p = poly { [Sobj (Objective.Max p)] }

cnstrs: (* constraints section cannot be empty *)
  ST l = nonempty_list(cnstr) { [Scnstr l] }

cnstr:
  | l = ID; COLON; p = poly; EQ; rhs = const { Cnstr.eq ~name:(Some l) p rhs }
  | l = ID; COLON; p = poly; LT; rhs = const { Cnstr.lt ~name:(Some l) p rhs }
  | l = ID; COLON; p = poly; GT; rhs = const { Cnstr.gt ~name:(Some l) p rhs }
  | p = poly; EQ; rhs = const { Cnstr.eq p rhs }
  | p = poly; LT; rhs = const { Cnstr.lt p rhs }
  | p = poly; GT; rhs = const { Cnstr.gt p rhs }

const :
  n = signed { [Term.Const n] }

poly :
  | p = pl { p }
  | PLUS p = pl { p }
  | MINUS p = pl
  { match p with
    | hd :: rest -> Term.neg hd :: rest
    | _-> failwith "empty polynomial expression" }

pl :
  | p = pl PLUS  t = term { p @ [t] }
  | p = pl MINUS t = term { p @ [Term.neg t] }
  | t = term { [t] }

term:
  | v = ID { Term.Linear (1.0, Var.make v) }
  | n = NUM { Term.Const n }
  | n = NUM  v = ID { Term.Linear (n, Var.make v) }
  | n = NUM v0 = ID TIMES v1 = ID { Term.Quad (n, Var.make v0, Var.make v1) }
  | n = NUM  v = ID SQ { Term.Quad (n, Var.make v, Var.make v) }

bounds:
  | (*empty*) { [] }
  | BOUND l = nonempty_list(bound) { [Sbound l] }

bound:
  | v = ID FREE { {name=v; lb=Float.neg_infinity; ub=Float.infinity} }
  | lb = signed LT v = ID { {name=v; lb; ub=Float.infinity} }
  | v = ID LT ub = signed { {name=v; lb=Float.zero; ub} }
  | lb = signed LT v = ID LT ub = signed { {name=v; lb; ub} }

signed:
  | n = NUM { n }
  | PLUS n = NUM { n }
  | MINUS n = NUM { Float.neg n }

vtype:
  | (*empty*) { [] }
  | g = general { [g] }
  | b = binary { [b] }
  | g = general b = binary { [g; b] }
  | b = binary g = general { [g; b] }

general:
  GENERAL l = nonempty_list(ID) { Sgeneral l }

binary:
  BINARY l = nonempty_list(ID) { Sbinary l }
