open List;
open Terms;

let valid_name = x => String.trim(x) != "";

let rec extend_context = (x: string, t: typ, c: context): context =>
  switch (c) {
  | [] => [(x, t)]
  | [(y, _), ...c] when x == y => extend_context(x, t, c)
  | [(y, t'), ...c] => [(y, t'), ...extend_context(x, t, c)]
  };

let rec lookup = (x: string, c: context) => {
  switch (c) {
  | [] => None
  | [(y, t), ..._] when x == y => Some(t)
  | [_, ...c] => lookup(x, c)
  };
};

let name_of_zname = (z: zname): name =>
  switch (z) {
  | Cursor(x) => x
  };

let rec typ_of_ztyp = (z: ztyp): typ => {
  switch (z) {
  | Cursor(t) => t
  | LArrow(z, t) => Arrow(typ_of_ztyp(z), t)
  | RArrow(t, z) => Arrow(t, typ_of_ztyp(z))
  };
};
let rec exp_of_zexp = (z: zexp): exp => {
  switch (z) {
  | Cursor(e) => e
  | Mark(m, z) => Mark(m, exp_of_zexp(z))
  | XFun(z, t, e) => Fun(name_of_zname(z), t, e)
  | TFun(x, z, e) => Fun(x, typ_of_ztyp(z), e)
  | EFun(x, t, z) => Fun(x, t, exp_of_zexp(z))
  | LAp(z, e) => Ap(exp_of_zexp(z), e)
  | RAp(e, z) => Ap(e, exp_of_zexp(z))
  | XLet(z, t, e1, e2) => Let(name_of_zname(z), t, e1, e2)
  | TLet(x, z, e1, e2) => Let(x, typ_of_ztyp(z), e1, e2)
  | E1Let(x, t, z, e2) => Let(x, t, exp_of_zexp(z), e2)
  | E2Let(x, t, e1, z) => Let(x, t, e1, exp_of_zexp(z))
  };
};

let complete_name = (x: name) =>
  switch (x) {
  | Hole => false
  | Text(_) => true
  };

let rec complete_typ = (t: typ) =>
  switch (t) {
  | Hole => false
  | Base(_) => true
  | Arrow(t1, t2) => complete_typ(t1) && complete_typ(t2)
  };

let rec complete_exp = (e: exp) =>
  switch (e) {
  | Hole
  | Mark(_, _) => false
  | Var(_) => true
  | Fun(x, t, e) => complete_name(x) && complete_typ(t) && complete_exp(e)
  | Ap(e1, e2) => complete_exp(e1) && complete_exp(e2)
  | Let(x, t, e1, e2) =>
    complete_name(x)
    && complete_typ(t)
    && complete_exp(e1)
    && complete_exp(e2)
  };

let rec consist = (t1, t2: typ): bool => {
  switch (t1: typ, t2: typ) {
  | (Hole, _)
  | (_, Hole) => true
  | (Arrow(t1, t2), Arrow(t3, t4)) => consist(t1, t3) && consist(t2, t4)
  | (Base(x), Base(y)) => x == y
  | _ => false
  };
};

let arrow_of_typ = (t: typ): option((typ, typ)) => {
  switch (t) {
  | Hole => Some((Hole, Hole))
  | Arrow(t1, t2) => Some((t1, t2))
  | _ => None
  };
};

let rec syn = (c: context, e: exp): (exp, typ) => {
  switch (e) {
  | Hole => (Hole, Hole)
  | Mark(_, e) =>
    let (e', _) = syn(c, e);
    (e', Hole);
  | Var(x) =>
    switch (lookup(x, c)) {
    | None => (Mark(UnknownVar(x), Var(x)), Hole)
    | Some(t) => (Var(x), t)
    }
  | Fun(x, t, e) =>
    let c' =
      switch (x) {
      | Hole => c
      | Text(x') => extend_context(x', t, c)
      };
    let (e', t') = syn(c', e);
    (Fun(x, t, e'), Arrow(t, t'));
  | Ap(e1, e2) =>
    switch (syn(c, e1)) {
    | (e1', t) =>
      switch (arrow_of_typ(t)) {
      | None => (
          Ap(Mark(FunNotArrow(t), e1'), ana(c, Hole: typ, e2)),
          Hole,
        )
      | Some((t1, t2)) => (Ap(e1', ana(c, t1, e2)), t2)
      }
    }
  | Let(x, t, e1, e2) =>
    let c' =
      switch (x) {
      | Hole => c
      | Text(x') => extend_context(x', t, c)
      };

    let (e2', t2) = syn(c', e2);
    (Let(x, t, ana(c, t, e1), e2'), t2);
  };
}
and ana = (c: context, t: typ, e: exp): exp =>
  switch (e, arrow_of_typ(t)) {
  | (Fun(x, t1, e), Some((t2, t3))) when consist(t1, t2) =>
    let c' =
      switch (x) {
      | Hole => c
      | Text(x') => extend_context(x', t1, c)
      };
    Fun(x, t1, ana(c', t3, e));
  | _ =>
    let (e', t') = syn(c, e);
    if (consist(t, t')) {
      e';
    } else {
      Mark(Mismatch(t, t'), e');
    };
  };

let rec mark_merge = (z: zexp, e: exp): zexp =>
  switch (z, e) {
  | (Cursor(_), e') => Cursor(e')
  | (z, Mark(m, e)) => Mark(m, mark_merge(z, e))
  | (XFun(z, t, _), Fun(_, _, e')) => XFun(z, t, e')
  | (TFun(x, z, _), Fun(_, _, e')) => TFun(x, z, e')
  | (EFun(x, t, z), Fun(_, _, e')) => EFun(x, t, mark_merge(z, e'))
  | (LAp(z, _), Ap(e1, e2)) => LAp(mark_merge(z, e1), e2)
  | (RAp(_, z), Ap(e1, e2)) => RAp(e1, mark_merge(z, e2))
  | (XLet(z, t, _, _), Let(_, _, e1', e2')) => XLet(z, t, e1', e2')
  | (TLet(x, z, _, _), Let(_, _, e1', e2')) => TLet(x, z, e1', e2')
  | (E1Let(x, t, z, _), Let(_, _, e1', e2')) =>
    E1Let(x, t, mark_merge(z, e1'), e2')
  | (E2Let(x, t, _, z), Let(_, _, e1', e2')) =>
    E2Let(x, t, e1', mark_merge(z, e2'))
  | _ => failwith("merge mismatch")
  };

let check_let = (c, t, e1) =>
  switch (syn(c, e1)) {
  | (_, t') => complete_typ(t) && complete_exp(e1) && consist(t, t')
  };

let rec local_context = (c: context, z: zexp) =>
  switch (z) {
  | Cursor(_) => c
  | Mark(_, z) => local_context(c, z)
  | XFun(_, _, _)
  | TFun(_, _, _) => c
  | EFun(x, t, z) =>
    let c' =
      switch (x) {
      | Hole => c
      | Text(x') => extend_context(x', t, c)
      };
    local_context(c', z);
  | LAp(z, _)
  | RAp(_, z) => local_context(c, z)
  | XLet(_, _, _, _)
  | TLet(_, _, _, _) => c
  | E1Let(_, _, z, _) => local_context(c, z)
  | E2Let(x, t, _, z) =>
    let c' =
      switch (x) {
      | Hole => c
      | Text(x') => extend_context(x', t, c)
      };
    local_context(c', z);
  };

let rec local_goal = (c: context, g: typ, z: zexp) =>
  switch (z) {
  | Cursor(_) => g
  | Mark(_, z) => local_goal(c, g, z)
  | XFun(_, _, _)
  | TFun(_, _, _) => g
  | EFun(x, t, z) =>
    let c' =
      switch (x) {
      | Hole => c
      | Text(x') => extend_context(x', t, c)
      };
    let g' =
      switch (arrow_of_typ(g)) {
      | Some((_, t')) => t'
      | None => Hole
      };
    local_goal(c', g', z);
  | LAp(z, e) =>
    let (_, g') = syn(c, e);
    local_goal(c, Arrow(g', g), z);
  | RAp(e, z) =>
    let g': typ =
      switch (syn(c, e)) {
      | (_, Arrow(g', _)) => g'
      | _ => Hole
      };
    local_goal(c, g', z);
  | XLet(_, _, _, _)
  | TLet(_, _, _, _) => g
  | E1Let(_, t, z, _) => local_goal(c, t, z)
  | E2Let(x, t, _, z) =>
    let c' =
      switch (x) {
      | Hole => c
      | Text(x') => extend_context(x', t, c)
      };
    local_goal(c', g, z);
  };

let rec local_marks = (z: zexp): list(mark) =>
  switch (z) {
  | Cursor(Mark(m, e)) => [m, ...local_marks(Cursor(e))]
  | Cursor(_) => []
  | Mark(_, z) => local_marks(z)
  | XFun(_, _, _)
  | TFun(_, _, _) => []
  | EFun(_, _, z) => local_marks(z)
  | LAp(z, _)
  | RAp(_, z) => local_marks(z)
  | XLet(_, _, _, _)
  | TLet(_, _, _, _) => []
  | E1Let(_, _, z, _)
  | E2Let(_, _, _, z) => local_marks(z)
  };
