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

let rec extend_env = (x: string, e: term, en: env): env =>
  switch (en) {
  | [] => [(x, e)]
  | [(y, _), ...c] when x == y => extend_env(x, e, c)
  | [(y, t'), ...c] => [(y, t'), ...extend_env(x, e, c)]
  };

let extend_env_name = (x: name, e: term, en: env): env =>
  switch (x) {
  | Hole => en
  | Text(x') => extend_env(x', e, en)
  };

let maybe_extend_env_name = (x: name, e: term, en: env): env =>
  switch (e) {
  | Hole => en
  | _ => extend_env_name(x, e, en)
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
  | XArrow(z, t1, t2) => Arrow(name_of_zname(z), t1, t2)
  | LArrow(x, z, t) => Arrow(x, term_of_zterm(z), t)
  | RArrow(x, t, z) => Arrow(x, t, term_of_zterm(z))
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

let rec term_at_cursor = (z: zterm) =>
  switch (z) {
  | Cursor(e) => Some(e)
  | Mark(_, z) => term_at_cursor(z)
  | XArrow(_, _, _) => None
  | LArrow(_, z, _)
  | RArrow(_, _, z) => term_at_cursor(z)
  | XFun(_, _, _) => None
  | TFun(_, z, _)
  | EFun(_, _, z)
  | LAp(z, _)
  | RAp(_, z) => term_at_cursor(z)
  | XLet(_, _, _, _) => None
  | TLet(_, z, _, _)
  | E1Let(_, _, z, _)
  | E2Let(_, _, _, z) => term_at_cursor(z)
  };

let complete_name = (x: name) =>
  switch (x) {
  | Hole => false
  | Text(_) => true
  };

let rec extend_complete_list_let = (x, t, e, c) =>
  complete_term(c, t) && complete_term(c, e) ? extend_list_name(x, c) : c
and complete_term = (c: list(string), e: term) =>
  switch (e) {
  | Hole
  | Mark(_, _) => false
  | Var(x) => List.mem(x, c)
  | Typ
  | Base(_) => true
  | Arrow(x, t1, t2) =>
    complete_term(c, t1) && complete_term(extend_list_name(x, c), t2)
  | Fun(x, t, e) =>
    complete_name(x)
    && complete_term(c, t)
    && complete_term(extend_list_name(x, c), e)
  | Ap(e1, e2) => complete_term(c, e1) && complete_term(c, e2)
  | Let(x, t, e1, e2) =>
    complete_term(extend_complete_list_let(x, t, e1, c), e2)
  };
let extend_complete_list_fun = (x, t, c) =>
  complete_term(c, t) ? extend_list_name(x, c) : c;

let rec no_holes_term = (e: term) =>
  switch (e) {
  | Hole
  | Mark(_, _) => false
  | Var(_)
  | Typ
  | Base(_) => true
  | Arrow(_, t1, t2) => no_holes_term(t1) && no_holes_term(t2)
  | Fun(x, t, e) =>
    complete_name(x) && no_holes_term(t) && no_holes_term(e)
  | Ap(e1, e2) => no_holes_term(e1) && no_holes_term(e2)
  | Let(_, _, _, e2) => no_holes_term(e2)
  };

let rec sub = (x: string, e1: term, e2: term): term => {
  switch (e2) {
  | Hole
  | Typ
  | Base(_) => e2
  | Var(y) => x == y ? e1 : e2
  | Mark(m, e) => Mark(m, sub(x, e1, e))
  | Ap(e3, e4) => Ap(sub(x, e1, e3), sub(x, e1, e4))
  | Arrow(y, t1, t2) =>
    y == Text(x) ? e2 : Arrow(y, sub(x, e1, t1), sub(x, e1, t2))
  | Fun(y, t, e) =>
    y == Text(x) ? e2 : Fun(y, sub(x, e1, t), sub(x, e1, e))
  | Let(y, t, e3, e4) =>
    y == Text(x)
      ? e2 : Let(y, sub(x, e1, t), sub(x, e1, e3), sub(x, e1, e4))
  };
};

let sub_name = (x: name, e1: term, e2: term): term => {
  switch (x) {
  | Hole => e2
  | Text(x) => sub(x, e1, e2)
  };
};

let rec reduce = (en: env, e: term): term => {
  switch (e) {
  | Hole
  | Typ
  | Base(_)
  | Arrow(_, _, _)
  | Fun(_, _, _)
  | Mark(_, _)
  | Let(_, _, _, _) => e
  | Var(x) =>
    switch (List.assoc_opt(x, en)) {
    | None => e
    | Some(e') => reduce(en, e')
    }
  | Ap(Fun(x, _, e1), e2) => sub_name(x, e2, e1)
  | Ap(_, _) => e
  };
};

let consist_name = (x1, x2: name): bool => {
  switch (x1: name, x2: name) {
  | (Hole, _)
  | (_, Hole) => true
  | (Text(x1), Text(x2)) => x1 == x2
  };
};

let rec head_consist = (en: env, t1, t2: term): bool => {
  switch (t1: term, t2: term) {
  | (Hole, _)
  | (_, Hole)
  | (Typ, Typ) => true
  | (Mark(_, _), _)
  | (_, Mark(_, _)) => false
  | (Var(x), Var(y)) => x == y
  | (Base(x), Base(y)) => x == y
  | (Arrow(x, t1, t2), Arrow(y, t3, t4)) =>
    consist_name(x, y) && consist(en, t1, t3) && consist(en, t2, t4)
  | (Fun(x, t1, e1), Fun(y, t2, e2)) =>
    consist_name(x, y) && consist(en, t1, t2) && consist(en, e1, e2)
  | (Ap(e1, e2), Ap(e3, e4)) => consist(en, e1, e3) && consist(en, e2, e4)
  | (Let(x, t1, e1, e2), Let(y, t2, e3, e4)) =>
    consist_name(x, y)
    && consist(en, t1, t2)
    && consist(en, e1, e3)
    && consist(en, e2, e4)
  | _ => false
  };
}
and consist = (en: env, t1, t2: term): bool => {
  let (t1', t2') = (reduce(en, t1), reduce(en, t2));
  head_consist(en, t1', t2');
};

let typ_of_term = (t: term): bool => {
  switch (t) {
  | Hole
  | Typ => true
  | _ => false
  };
};

let arrow_of_term = (t: term): option((name, term, term)) => {
  switch (t) {
  | Hole => Some((Hole, Hole, Hole))
  | Arrow(x, t1, t2) => Some((x, t1, t2))
  | _ => None
  };
};

// Synthetic marking judgement. returns (marked_expression, synthesized_type)
let rec syn = (c: context, en: env, e: term): (term, term) => {
  switch (e) {
  | Hole => (Hole, Hole)
  | Typ => (Typ, Typ)
  | Mark(_, e) =>
    let (e', _) = syn(c, en, e);
    (e', Hole);
  | Var(x) =>
    switch (lookup(x, c)) {
    | None => (Mark(UnknownVar(x), Var(x)), Hole)
    | Some(t) => (Var(x), t)
    }
  | Base(x) => (Base(x), Typ)
  | Arrow(x, t1, t2) =>
    let (t1', t1t) = syn(c, en, t1);
    let (c', t1'': term) =
      switch (typ_of_term(t1t)) {
      | false => (c, Mark(NotTyp(t1t), t1'))
      | true => (extend_context_name(x, t1', c), t1')
      };
    let (t2', t2t) = syn(c', en, t2);
    let t2'': term =
      switch (typ_of_term(t2t)) {
      | false => Mark(NotTyp(t2t), t2')
      | true => t2'
      };
    (Arrow(x, t1'', t2''), Typ);
  | Fun(x, t, e) =>
    let (t', tt) = syn(c, en, t);
    let (c', t'': term) =
      switch (typ_of_term(tt)) {
      | false => (c, Mark(NotTyp(tt), t'))
      | true => (extend_context_name(x, t', c), t')
      };
    let (e', et) = syn(c', en, e);
    (Fun(x, t', e'), Arrow(x, t'', et));
  | Ap(e1, e2) =>
    switch (syn(c, en, e1)) {
    | (e1', t) =>
      switch (arrow_of_term(t)) {
      | None => (
          Ap(Mark(FunNotArrow(t), e1'), ana(c, en, Hole: term, e2)),
          Hole,
        )
      | Some((x, t1, t2)) =>
        let e2' = ana(c, en, t1, e2);
        let t = sub_name(x, e2', t2);
        (Ap(e1', e2'), t);
      }
    }
  | Let(x, t, e1, e2) =>
    let (t', tt) = syn(c, en, t);
    let (c', t'': term, ana_t) =
      switch (typ_of_term(tt)) {
      | false => (c, Mark(NotTyp(tt), t'), Hole)
      | true => (extend_context_name(x, t', c), t', t')
      };
    let en' = maybe_extend_env_name(x, e1, en);
    let (e2', t2) = syn(c', en', e2);
    (Let(x, t'', ana(c, en, ana_t, e1), e2'), t2);
  };
}
// Analytic marking judgement. returns marked_expression
and ana = (c: context, en: env, t: term, e: term): term =>
  switch (e, arrow_of_term(t)) {
  | (Fun(x, t1, e), Some((y, t2, t3)))
      when consist_name(x, y) && consist(en, t1, t2) =>
    let (t', _) = syn(c, en, t1);
    let c' = extend_context_name(x, t', c);
    Fun(x, t1, ana(c', en, t3, e));
  | _ =>
    let (e', t') = syn(c, en, e);
    if (consist(en, t, t')) {
      e';
    } else {
      Mark(Mismatch(t, t'), e');
    };
  };
// Precondition: z and e are the same except z has a cursor and e has marks.
// Returns the expression with both the cursor and the marks.
let rec mark_merge = (z: zterm, e: term): zterm =>
  switch (z, e) {
  | (Cursor(_), e') => Cursor(e')
  | (z, Mark(m, e)) => Mark(m, mark_merge(z, e))
  | (XArrow(z, _, _), Arrow(_, t1, t2)) => XArrow(z, t1, t2)
  | (LArrow(_, z, _), Arrow(x, t1, t2)) =>
    LArrow(x, mark_merge(z, t1), t2)
  | (RArrow(_, _, z), Arrow(x, t1, t2)) =>
    RArrow(x, t1, mark_merge(z, t2))
  | (XFun(z, _, _), Fun(_, t, e)) => XFun(z, t, e)
  | (TFun(_, z, _), Fun(x, t, e)) => TFun(x, mark_merge(z, t), e)
  | (EFun(_, _, z), Fun(x, t, e)) => EFun(x, t, mark_merge(z, e))
  | (LAp(z, _), Ap(e1, e2)) => LAp(mark_merge(z, e1), e2)
  | (RAp(_, z), Ap(e1, e2)) => RAp(e1, mark_merge(z, e2))
  | (XLet(z, _, _, _), Let(_, t, e1, e2)) => XLet(z, t, e1, e2)
  | (TLet(_, z, _, _), Let(x, t, e1, e2)) =>
    TLet(x, mark_merge(z, t), e1, e2)
  | (E1Let(_, _, z, _), Let(x, t, e1, e2)) =>
    E1Let(x, t, mark_merge(z, e1), e2)
  | (E2Let(_, _, _, z), Let(x, t, e1, e2)) =>
    E2Let(x, t, e1, mark_merge(z, e2))
  | _ => failwith("merge mismatch")
  };
// Returns the complete context (list of vars that are completely defined)
// at the cursor (appended to the argument c)
let rec local_complete_list = (c: list(string), z: zterm) =>
  switch (z) {
  | Cursor(_) => c
  | Mark(_, z) => local_complete_list(c, z)
  | XArrow(_, _, _) => c
  | LArrow(_, z, _) => local_complete_list(c, z)
  | RArrow(x, t, z) =>
    let c' = extend_complete_list_fun(x, t, c);
    local_complete_list(c', z);
  | XFun(_, _, _) => c
  | TFun(_, z, _) => local_complete_list(c, z)
  | EFun(x, t, z) =>
    let c' = extend_complete_list_fun(x, t, c);
    local_complete_list(c', z);
  | LAp(z, _)
  | RAp(_, z) => local_complete_list(c, z)
  | XLet(_, _, _, _) => c
  | TLet(_, z, _, _) => local_complete_list(c, z)
  | E1Let(_, _, z, _) => local_complete_list(c, z)
  | E2Let(x, t, e, z) =>
    let c' = extend_complete_list_let(x, t, e, c);
    local_complete_list(c', z);
  };
// Whether a let should get a green box next to it
let check_let = (c, en, t, completes, e1) =>
  switch (syn(c, en, e1)) {
  | (_, t') =>
    complete_term(completes, t)
    && complete_term(completes, e1)
    && consist(en, t, t')
  };
// Returns the context at the cursor (appended to the argument c)
let rec local_context = (c: context, z: zterm) =>
  switch (z) {
  | Cursor(_) => c
  | Mark(_, z) => local_context(c, z)
  | XArrow(_, _, _) => c
  | LArrow(_, z, _) => local_context(c, z)
  | RArrow(x, t1, z) =>
    let c' = extend_context_name(x, t1, c);
    local_context(c', z);
  | XFun(_, _, _) => c
  | TFun(_, z, _) => local_context(c, z)
  | EFun(x, t, z) =>
    let c' = extend_context_name(x, t, c);
    local_context(c', z);
  | LAp(z, _)
  | RAp(_, z) => local_context(c, z)
  | XLet(_, _, _, _) => c
  | TLet(_, z, _, _) => local_context(c, z)
  | E1Let(_, _, z, _) => local_context(c, z)
  | E2Let(x, t, _, z) =>
    let c' = extend_context_name(x, t, c);
    local_context(c', z);
  };
let rec local_env = (en: env, z: zterm) =>
  switch (z) {
  | Cursor(_) => en
  | Mark(_, z) => local_env(en, z)
  | XArrow(_, _, _) => en
  | LArrow(_, z, _)
  | RArrow(_, _, z) => local_env(en, z)
  | XFun(_, _, _) => en
  | TFun(_, z, _)
  | EFun(_, _, z) => local_env(en, z)
  | LAp(z, _)
  | RAp(_, z) => local_env(en, z)
  | XLet(_, _, _, _) => en
  | TLet(_, z, _, _)
  | E1Let(_, _, z, _) => local_env(en, z)
  | E2Let(x, _, e, z) =>
    let en' = maybe_extend_env_name(x, e, en);
    local_env(en', z);
  };
// Returns the expected type at the cursor, if the argument's expected type is g
let rec local_goal = (c: context, en: env, g: term, z: zterm) =>
  switch (z) {
  | Cursor(_) => g
  | Mark(_, z) => local_goal(c, en, g, z)
  | XArrow(_, _, _) => g
  | LArrow(_, _, _) => Typ
  | RArrow(x, t1, z) =>
    let c' = extend_context_name(x, t1, c);
    local_goal(c', en, g, z);
  | XFun(_, _, _) => g
  | TFun(_, _, _) => Typ
  | EFun(x, t, z) =>
    let c' = extend_context_name(x, t, c);
    let g' =
      switch (arrow_of_term(g)) {
      | Some((y, _, t')) when consist_name(x, y) => t'
      | _ => Hole
      };
    local_goal(c', en, g', z);
  | LAp(z, e) =>
    let (_, g') = syn(c, en, e);
    local_goal(c, en, Arrow(Hole, g', g), z);
  | RAp(e, z) =>
    let g': term =
      switch (syn(c, en, e)) {
      | (_, Arrow(_, g', _)) => g'
      | _ => Hole
      };
    local_goal(c, en, g', z);
  | XLet(_, _, _, _) => g
  | TLet(_, _, _, _) => Typ
  | E1Let(_, t, z, _) => local_goal(c, en, t, z)
  | E2Let(x, t, e, z) =>
    let c' = extend_context_name(x, t, c);
    let en' = maybe_extend_env_name(x, e, en);
    local_goal(c', en', g, z);
  };
// Returns the marks at the cursor
let rec local_marks = (z: zterm): list(mark) =>
  switch (z) {
  | Cursor(Mark(m, e)) => [m, ...local_marks(Cursor(e))]
  | Cursor(_) => []
  | Mark(_, z) => local_marks(z)
  | XArrow(_, _, _) => []
  | LArrow(_, z, _)
  | RArrow(_, _, z) => local_marks(z)
  | XFun(_, _, _) => []
  | TFun(_, z, _)
  | EFun(_, _, z) => local_marks(z)
  | LAp(z, _)
  | RAp(_, z) => local_marks(z)
  | XLet(_, _, _, _) => []
  | TLet(_, z, _, _)
  | E1Let(_, _, z, _)
  | E2Let(_, _, _, z) => local_marks(z)
  };
