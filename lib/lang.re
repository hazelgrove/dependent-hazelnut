open List;
open Terms;

let valid_name = x => String.trim(x) != "";

let rec extend_context = (x: string, t: term, c: context): context =>
  switch (c) {
  | [] => [(x, t)]
  | [(y, _), ...c] when x == y => extend_context(x, t, c)
  | [(y, t'), ...c] => [(y, t'), ...extend_context(x, t, c)]
  };

let extend_context_name = (x: name, t: term, c: context): context =>
  switch (x) {
  | Hole => c
  | Text(x') => extend_context(x', t, c)
  };

let extend_list_name = (x: name, c: list(string)): list(string) =>
  switch (x) {
  | Hole => c
  | Text(x') => [x', ...c]
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

let rec term_of_zterm = (z: zterm): term => {
  switch (z) {
  | Cursor(t) => t
  | Mark(m, z) => Mark(m, term_of_zterm(z))
  | LArrow(z, t) => Arrow(term_of_zterm(z), t)
  | RArrow(t, z) => Arrow(t, term_of_zterm(z))
  | XFun(z, t, e) => Fun(name_of_zname(z), t, e)
  | TFun(x, z, e) => Fun(x, term_of_zterm(z), e)
  | EFun(x, t, z) => Fun(x, t, term_of_zterm(z))
  | LAp(z, e) => Ap(term_of_zterm(z), e)
  | RAp(e, z) => Ap(e, term_of_zterm(z))
  | XLet(z, t, e1, e2) => Let(name_of_zname(z), t, e1, e2)
  | TLet(x, z, e1, e2) => Let(x, term_of_zterm(z), e1, e2)
  | E1Let(x, t, z, e2) => Let(x, t, term_of_zterm(z), e2)
  | E2Let(x, t, e1, z) => Let(x, t, e1, term_of_zterm(z))
  };
};

// let rec term_at_cursor = (z: zterm) =>
//   switch (z) {
//   | Cursor(e) => Some(e)
//   | Mark(_, z) => term_at_cursor(z)
//   | XFun(_, _, _)
//   | TFun(_, _, _) => None
//   | EFun(_, _, z)
//   | LAp(z, _)
//   | RAp(_, z) => term_at_cursor(z)
//   | XLet(_, _, _, _)
//   | TLet(_, _, _, _) => None
//   | E1Let(_, _, z, _)
//   | E2Let(_, _, _, z) => term_at_cursor(z)
//   };

let complete_name = (x: name) =>
  switch (x) {
  | Hole => false
  | Text(_) => true
  };

let extend_complete_list_fun = (x, t, c) =>
  complete_term(t) ? extend_list_name(x, c) : c;
let rec extend_complete_list_let = (x, t, e, c) =>
  complete_term(t) && complete_term(c, e) ? extend_list_name(x, c) : c
and complete_term = (c: list(string), e: term) =>
  switch (e) {
  | Hole
  | Typ
  | Mark(_, _) => false
  | Var(x) => List.mem(x, c)
  | Base(_) => true
  | Arrow(t1, t2) => complete_term(c, t1) && complete_term(c, t2)
  | Fun(x, t, e) =>
    complete_name(x)
    && complete_term(c, t)
    && complete_term(extend_list_name(x, c), e)
  | Ap(e1, e2) => complete_term(c, e1) && complete_term(c, e2)
  | Let(x, t, e1, e2) =>
    complete_term(extend_complete_list_let(x, t, e1, c), e2)
  };

let rec consist = (t1, t2: term): bool => {
  switch (t1: term, t2: term) {
  | (Hole, _)
  | (_, Hole) => true
  | (Arrow(t1, t2), Arrow(t3, t4)) => consist(t1, t3) && consist(t2, t4)
  | (Base(x), Base(y)) => x == y
  | _ => false
  };
};

let arrow_of_term = (t: term): option((term, term)) => {
  switch (t) {
  | Hole => Some((Hole, Hole))
  | Arrow(t1, t2) => Some((t1, t2))
  | _ => None
  };
};

let rec syn = (c: context, e: term): (term, term) => {
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
    let c' = extend_context_name(x, t, c);
    let (e', t') = syn(c', e);
    (Fun(x, t, e'), Arrow(t, t'));
  | Ap(e1, e2) =>
    switch (syn(c, e1)) {
    | (e1', t) =>
      switch (arrow_of_term(t)) {
      | None => (
          Ap(Mark(FunNotArrow(t), e1'), ana(c, Hole: term, e2)),
          Hole,
        )
      | Some((t1, t2)) => (Ap(e1', ana(c, t1, e2)), t2)
      }
    }
  | Let(x, t, e1, e2) =>
    let c' = extend_context_name(x, t, c);
    let (e2', t2) = syn(c', e2);
    (Let(x, t, ana(c, t, e1), e2'), t2);
  };
}
and ana = (c: context, t: term, e: term): term =>
  switch (e, arrow_of_term(t)) {
  | (Fun(x, t1, e), Some((t2, t3))) when consist(t1, t2) =>
    let c' = extend_context_name(x, t1, c);
    Fun(x, t1, ana(c', t3, e));
  | _ =>
    let (e', t') = syn(c, e);
    if (consist(t, t')) {
      e';
    } else {
      Mark(Mismatch(t, t'), e');
    };
  };

let rec mark_merge = (z: zterm, e: term): zterm =>
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

let rec local_complete_list = (c: list(string), z: zterm) =>
  switch (z) {
  | Cursor(_) => c
  | Mark(_, z) => local_complete_list(c, z)
  | XFun(_, _, _)
  | TFun(_, _, _) => c
  | EFun(x, t, z) =>
    let c' = extend_complete_list_fun(x, t, c);
    local_complete_list(c', z);
  | LAp(z, _)
  | RAp(_, z) => local_complete_list(c, z)
  | XLet(_, _, _, _)
  | TLet(_, _, _, _) => c
  | E1Let(_, _, z, _) => local_complete_list(c, z)
  | E2Let(x, t, e, z) =>
    let c' = extend_complete_list_let(x, t, e, c);
    local_complete_list(c', z);
  };

let check_let = (c, t, completes, e1) =>
  switch (syn(c, e1)) {
  | (_, t') =>
    complete_term(t) && complete_term(completes, e1) && consist(t, t')
  };

let rec local_context = (c: context, z: zterm) =>
  switch (z) {
  | Cursor(_) => c
  | Mark(_, z) => local_context(c, z)
  | XFun(_, _, _)
  | TFun(_, _, _) => c
  | EFun(x, t, z) =>
    let c' = extend_context_name(x, t, c);
    local_context(c', z);
  | LAp(z, _)
  | RAp(_, z) => local_context(c, z)
  | XLet(_, _, _, _)
  | TLet(_, _, _, _) => c
  | E1Let(_, _, z, _) => local_context(c, z)
  | E2Let(x, t, _, z) =>
    let c' = extend_context_name(x, t, c);
    local_context(c', z);
  };

let rec local_goal = (c: context, g: term, z: zterm) =>
  switch (z) {
  | Cursor(_) => g
  | Mark(_, z) => local_goal(c, g, z)
  | XFun(_, _, _)
  | TFun(_, _, _) => g
  | EFun(x, t, z) =>
    let c' = extend_context_name(x, t, c);
    let g' =
      switch (arrow_of_term(g)) {
      | Some((_, t')) => t'
      | None => Hole
      };
    local_goal(c', g', z);
  | LAp(z, e) =>
    let (_, g') = syn(c, e);
    local_goal(c, Arrow(g', g), z);
  | RAp(e, z) =>
    let g': term =
      switch (syn(c, e)) {
      | (_, Arrow(g', _)) => g'
      | _ => Hole
      };
    local_goal(c, g', z);
  | XLet(_, _, _, _)
  | TLet(_, _, _, _) => g
  | E1Let(_, t, z, _) => local_goal(c, t, z)
  | E2Let(x, t, _, z) =>
    let c' = extend_context_name(x, t, c);
    local_goal(c', g, z);
  };

let rec local_marks = (z: zterm): list(mark) =>
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
