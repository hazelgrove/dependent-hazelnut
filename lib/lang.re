open List;
open Terms;

let name_of_zname = (z: zname): name =>
  switch (z) {
  | Cursor(x) => x
  };

let rec pterm_of_zterm = (z: zterm): pterm =>
  switch (z) {
  | Cursor(t) => t
  | XArrow(z, t1, t2) => Arrow(name_of_zname(z), t1, t2)
  | LArrow(x, z, t) => Arrow(x, pterm_of_zterm(z), t)
  | RArrow(x, t, z) => Arrow(x, t, pterm_of_zterm(z))
  | XFun(z, t, e) => Fun(name_of_zname(z), t, e)
  | TFun(x, z, e) => Fun(x, pterm_of_zterm(z), e)
  | EFun(x, t, z) => Fun(x, t, pterm_of_zterm(z))
  | LAp(z, e) => Ap(pterm_of_zterm(z), e)
  | RAp(e, z) => Ap(e, pterm_of_zterm(z))
  | XLet(z, t, e1, e2) => Let(name_of_zname(z), t, e1, e2)
  | TLet(x, z, e1, e2) => Let(x, pterm_of_zterm(z), e1, e2)
  | E1Let(x, t, z, e2) => Let(x, t, pterm_of_zterm(z), e2)
  | E2Let(x, t, e1, z) => Let(x, t, e1, pterm_of_zterm(z))
  };

let rec pterm_at_cursor = (z: zterm) =>
  switch (z) {
  | Cursor(e) => Some(e)
  | XArrow(_, _, _) => None
  | LArrow(_, z, _)
  | RArrow(_, _, z) => pterm_at_cursor(z)
  | XFun(_, _, _) => None
  | TFun(_, z, _)
  | EFun(_, _, z)
  | LAp(z, _)
  | RAp(_, z) => pterm_at_cursor(z)
  | XLet(_, _, _, _) => None
  | TLet(_, z, _, _)
  | E1Let(_, _, z, _)
  | E2Let(_, _, _, z) => pterm_at_cursor(z)
  };

let default_info: info = {
  ctx: [],
  goal: None,
  syn: None,
  cursed: false,
  name_cursed: false,
  cursor_inside: false,
};

let default_hole: term = Hole({i: default_info});

let rec term_of_pterm = (e: pterm) =>
  switch (e) {
  | Hole => Hole({i: default_info})
  | Typ => Typ({i: default_info})
  | Var(x) => Var({i: default_info, x, idx: None})
  | Arrow(x, t1, t2) =>
    Arrow({
      i: default_info,
      x,
      t1: term_of_pterm(t1),
      t2: term_of_pterm(t2),
    })
  | Fun(x, t, e) =>
    Fun({i: default_info, x, t: term_of_pterm(t), e: term_of_pterm(e)})
  | Ap(e1, e2) =>
    Ap({i: default_info, e1: term_of_pterm(e1), e2: term_of_pterm(e2)})
  | Let(x, t, e1, e2) =>
    Let({
      i: default_info,
      x,
      t: term_of_pterm(t),
      e1: term_of_pterm(e1),
      e2: term_of_pterm(e2),
    })
  };

let rec pterm_of_term = (e: term): pterm =>
  switch (e) {
  | Hole(_) => Hole
  | Typ(_) => Typ
  | Mark(r) => pterm_of_term(r.e)
  | Var(r) => Var(r.x)
  | Arrow(r) => Arrow(r.x, pterm_of_term(r.t1), pterm_of_term(r.t2))
  | Fun(r) => Fun(r.x, pterm_of_term(r.t), pterm_of_term(r.e))
  | Ap(r) => Ap(pterm_of_term(r.e1), pterm_of_term(r.e2))
  | Let(r) =>
    Let(r.x, pterm_of_term(r.t), pterm_of_term(r.e1), pterm_of_term(r.e2))
  };

let rec term_at_cursor = (z: zterm, e: term): term =>
  switch (z, e) {
  | (Cursor(_), e) => e
  | (z, Mark(r)) => term_at_cursor(z, r.e)
  | (XArrow(_, _, _), _) => e
  | (LArrow(_, z, _), Arrow(r)) => term_at_cursor(z, r.t1)
  | (RArrow(_, _, z), Arrow(r)) => term_at_cursor(z, r.t2)
  | (XFun(_, _, _), _) => e
  | (TFun(_, z, _), Fun(r)) => term_at_cursor(z, r.t)
  | (EFun(_, _, z), Fun(r)) => term_at_cursor(z, r.e)
  | (LAp(z, _), Ap(r)) => term_at_cursor(z, r.e1)
  | (RAp(_, z), Ap(r)) => term_at_cursor(z, r.e2)
  | (XLet(_, _, _, _), _) => e
  | (TLet(_, z, _, _), Let(r)) => term_at_cursor(z, r.t)
  | (E1Let(_, _, z, _), Let(r)) => term_at_cursor(z, r.e1)
  | (E2Let(_, _, _, z), Let(r)) => term_at_cursor(z, r.e2)
  | _ => failwith("term misalignment")
  };

let get_info = (e: term): info =>
  switch (e) {
  | Hole(r) => r.i
  | Typ(r) => r.i
  | Mark(r) => r.i
  | Var(r) => r.i
  | Arrow(r) => r.i
  | Fun(r) => r.i
  | Ap(r) => r.i
  | Let(r) => r.i
  };

let set_info = (e: term, i): term =>
  switch (e) {
  | Hole(_) => Hole({i: i})
  | Typ(_) => Typ({i: i})
  | Mark(r) => Mark({...r, i})
  | Var(r) => Var({...r, i})
  | Arrow(r) => Arrow({...r, i})
  | Fun(r) => Fun({...r, i})
  | Ap(r) => Ap({...r, i})
  | Let(r) => Let({...r, i})
  };

let rec place_cursor = (z: zterm, e: term): term => {
  let name_curse = (i: info) => {...i, name_cursed: true};
  let curse_inside = (i: info) => {...i, cursor_inside: true};
  switch (z, e) {
  | (Cursor(_), e) => set_info(e, {...get_info(e), cursed: true})
  | (z, Mark(r)) => Mark({...r, e: place_cursor(z, r.e)})
  | (XArrow(_, _, _), Arrow(r)) => Arrow({...r, i: name_curse(r.i)})
  | (LArrow(_, z, _), Arrow(r)) =>
    Arrow({...r, i: curse_inside(r.i), t1: place_cursor(z, r.t1)})
  | (RArrow(_, _, z), Arrow(r)) =>
    Arrow({...r, i: curse_inside(r.i), t2: place_cursor(z, r.t2)})
  | (XFun(_, _, _), Fun(r)) => Fun({...r, i: name_curse(r.i)})
  | (TFun(_, z, _), Fun(r)) =>
    Fun({...r, i: curse_inside(r.i), t: place_cursor(z, r.t)})
  | (EFun(_, _, z), Fun(r)) =>
    Fun({...r, i: curse_inside(r.i), e: place_cursor(z, r.e)})
  | (LAp(z, _), Ap(r)) =>
    Ap({...r, i: curse_inside(r.i), e1: place_cursor(z, r.e1)})
  | (RAp(_, z), Ap(r)) =>
    Ap({...r, i: curse_inside(r.i), e2: place_cursor(z, r.e2)})
  | (XLet(_, _, _, _), Let(r)) => Let({...r, i: name_curse(r.i)})
  | (TLet(_, z, _, _), Let(r)) =>
    Let({...r, i: curse_inside(r.i), t: place_cursor(z, r.t)})
  | (E1Let(_, _, z, _), Let(r)) =>
    Let({...r, i: curse_inside(r.i), e1: place_cursor(z, r.e1)})
  | (E2Let(_, _, _, z), Let(r)) =>
    Let({...r, i: curse_inside(r.i), e2: place_cursor(z, r.e2)})
  | _ => failwith("term misalignment")
  };
};

let valid_name = x => String.trim(x) != "";

let extend_list_name = (x: name, c: list(string)): list(string) =>
  switch (x) {
  | Hole => c
  | Text(x') => [x', ...c]
  };

// let rec lookup = (x: string, c: context) => {
//   switch (c) {
//   | [] => None
//   | [(y, t), ..._] when x == y => Some(t)
//   | [_, ...c] => lookup(x, c)
//   };
// };

// SECTION: CONSISTENCY, MATCHING, AND REDUCTION

let index_ctx_opt = (ctx, n) => {
  // print_endline(string_of_int(n));
  switch (n) {
  | (-1) => None
  | _ => List.nth_opt(ctx, n)
  };
};

// shift indices above threshold by n
let rec shift_indices = (n: int, t: int, e: term) =>
  switch (e) {
  | Hole(_)
  | Typ(_) => e
  | Ap(r) =>
    Ap({...r, e1: shift_indices(n, t, r.e1), e2: shift_indices(n, t, r.e2)})
  | Mark(r) => Mark({...r, e: shift_indices(n, t, r.e)})
  | Arrow(r) =>
    Arrow({
      ...r,
      t1: shift_indices(n, t, r.t1),
      t2: shift_indices(n, 1 + t, r.t2),
    })
  | Fun(r) =>
    Fun({
      ...r,
      t: shift_indices(n, t, r.t),
      e: shift_indices(n, 1 + t, r.e),
    })
  | Let(r) =>
    Let({
      ...r,
      t: shift_indices(n, t, r.t),
      e1: shift_indices(n, t, r.e1),
      e2: shift_indices(n, 1 + t, r.e2),
    })
  | Var(r) =>
    let index_map = i =>
      if (i >= t) {
        i + n;
      } else {
        i;
      };
    Var({...r, idx: Option.map(index_map, r.idx)});
  };

let rec sub = (n: int, e1: term, e2: term): term => {
  switch (e2) {
  | Hole(_)
  | Typ(_) => e2
  | Var(r) => r.idx == Some(n) ? e1 : e2
  | Mark(r) => Mark({...r, e: sub(n, e1, r.e)})
  | Ap(r) => Ap({...r, e1: sub(n, e1, r.e1), e2: sub(n, e1, r.e2)})
  | Arrow(r) =>
    Arrow({
      ...r,
      t1: sub(n, e1, r.t1),
      t2: sub(1 + n, shift_indices(1, 0, e1), r.t2),
    })
  | Fun(r) =>
    Fun({
      ...r,
      t: sub(n, e1, r.t),
      e: sub(1 + n, shift_indices(1, 0, e1), r.e),
    })
  | Let(r) =>
    Let({
      ...r,
      t: sub(n, e1, r.t),
      e1: sub(n, e1, r.e1),
      e2: sub(1 + n, shift_indices(1, 0, e1), r.e2),
    })
  };
};

// substitute e1 into e2, shifting other fv's in e2 down by 1
let beta_sub = (e1, e2) => {
  let e1' = shift_indices(1, 0, e1);
  let e = sub(0, e1', e2);
  shift_indices(-1, 0, e);
};

// // Replaces all occurrences of x in e with the value of x in en
// let delta_remove_occurrences = (e: term, en:env, x: string): term =>
//   switch (e) {
// }

// Beta reduce until head is exposed, if possible
let rec head_reduce = (ctx: context, e: term): term => {
  switch (e) {
  | Hole(_)
  | Typ(_)
  | Arrow(_)
  | Fun(_)
  | Mark(_)
  | Let(_) => e
  | Var(r) =>
    switch (Option.map(List.nth(ctx), r.idx)) {
    | None => e
    | Some(c) =>
      switch (c.e) {
      | None
      | Some(Hole(_)) => e // Declarations with a hole body are considered axioms. This could be changed.
      | Some(e') => head_reduce(ctx, e')
      }
    }
  | Ap(r) =>
    switch (head_reduce(ctx, r.e1)) {
    | Fun(r2) => head_reduce(ctx, sub(0, r.e2, r2.e))
    | Hole(r2) => Hole(r2)
    | _ => Ap(r)
    }
  };
};

let consist_name = (x1, x2: name): bool => {
  switch (x1: name, x2: name) {
  | (Hole, _)
  | (_, Hole) => true
  | (Text(x1), Text(x2)) => x1 == x2
  };
};

// Checks consistency of head-reduced terms
let rec head_consist = (ctx: context, t1, t2: term): bool => {
  switch (t1: term, t2: term) {
  | (Hole(_), _)
  | (_, Hole(_))
  | (Typ(_), Typ(_))
  | (Mark(_), _)
  | (_, Mark(_)) => true
  | (Var(r1), Var(r2)) => r1.idx == r2.idx
  | (Arrow(r1), Arrow(r2)) =>
    consist(ctx, r1.t1, r2.t1) && consist(ctx, r1.t2, r2.t2)
  | (Fun(r1), Fun(r2)) =>
    consist(ctx, r1.t, r2.t) && consist(ctx, r1.e, r2.e)
  | (Ap(r1), Ap(r2)) =>
    consist(ctx, r1.e1, r2.e1) && consist(ctx, r1.e2, r2.e2)
  | (Let(r1), Let(r2)) =>
    consist(ctx, r1.t, r2.t)
    && consist(ctx, r1.e1, r2.e1)
    && consist(ctx, r1.e2, r2.e2)
  | _ => false
  };
}
and consist = (ctx: context, t1, t2: term): bool => {
  let (t1', t2') = (head_reduce(ctx, t1), head_reduce(ctx, t2));
  head_consist(ctx, t1', t2');
};

// Structural equality

let names_equal = (x1, x2: name): bool => {
  switch (x1: name, x2) {
  | (Hole, Hole) => true
  | (Text(x1), Text(x2)) => x1 == x2
  | _ => false
  };
};

let rec terms_equal = (t1, t2: term): bool => {
  switch (t1: term, t2: term) {
  | (Hole(_), Hole(_))
  | (Typ(_), Typ(_)) => true
  | (Mark(r1), Mark(r2)) when r1.m == r2.m => terms_equal(r1.e, r2.e)
  | (Var(r1), Var(r2)) => r1.x == r2.x
  | (Arrow(r1), Arrow(r2)) =>
    names_equal(r1.x, r2.x)
    && terms_equal(r1.t1, r2.t1)
    && terms_equal(r1.t2, r2.t2)
  | (Fun(r1), Fun(r2)) =>
    names_equal(r1.x, r1.x)
    && terms_equal(r1.t, r2.t)
    && terms_equal(r1.e, r2.e)
  | (Ap(r1), Ap(r2)) =>
    terms_equal(r1.e1, r2.e1) && terms_equal(r1.e2, r2.e2)
  | (Let(r1), Let(r2)) =>
    names_equal(r1.x, r2.x)
    && terms_equal(r1.t, r2.t)
    && terms_equal(r1.e1, r2.e1)
    && terms_equal(r1.e2, r2.e2)
  | _ => false
  };
};

// Matched Set type. Invariant: either returns None or Some(Typ(_))
let typ_of_term = (t: term): option(term) => {
  switch (t) {
  | Hole(_) => Some(Typ({i: default_info}))
  | Typ(_) => Some(t)
  | _ => None
  };
};

// Matched arrow type. Invariant: either returns None or Some(Arrow(_))
let arrow_of_term = (t: term): option(term) => {
  switch (t) {
  | Hole(_) =>
    Some(
      Arrow({i: default_info, x: Hole, t1: default_hole, t2: default_hole}),
    )
  | Arrow(_) => Some(t)
  | _ => None
  };
};

// SECTION: STATIC ELABORATION

// Synthetic static judgement. returns new_expression
// Info is filled out, and marks are inserted
// Existing info is not necessarily paid attention to
let rec syn = (ctx: context, e: term): term => {
  let i = {...get_info(e), ctx};
  switch (e) {
  | Hole(_) =>
    let i = {...i, syn: Some(default_hole)};
    Hole({i: i});
  | Typ(_) =>
    let i = {...i, syn: Some(Typ({i: default_info}))};
    Typ({i: i});
  | Mark(_) => failwith("unreachable...?")
  // let (e', _) = syn(c, en, e);
  // (e', Hole);
  | Var(r) =>
    // returns the index and type of r.x if present
    let rec lookup_name = (idx, ctx_arg) =>
      switch (ctx_arg) {
      | [] => ((-1), None)
      | [c, ..._] when c.x == Text(r.x) => (idx, Some(c.t))
      | [_, ...ctx_arg] => lookup_name(idx + 1, ctx_arg)
      };
    let (idx, t) = lookup_name(0, ctx);
    switch (t) {
    | None =>
      let i = {...i, syn: Some(default_hole)};
      Mark({i, m: UnknownVar(r.x), e: Var({...r, i})});
    | Some(t) =>
      let t = head_reduce(ctx, t);
      let t = shift_indices(idx + 1, 0, t); // Adjust context down
      let i = {...i, syn: Some(t)};
      Var({...r, i, idx: Some(idx)});
    };
  | Arrow(r) =>
    let t1' = syn(ctx, r.t1);
    let t1t = get_info(t1').syn;
    let t1'': term =
      switch (Option.bind(Option.map(head_reduce(ctx), t1t), typ_of_term)) {
      | None =>
        let i = {...i, syn: Some(default_hole)};
        Mark({i, m: NotTyp(t1t), e: t1'});
      | Some(_) => t1'
      };
    let ctx = [{x: r.x, t: t1'', e: None}, ...ctx];
    let t2' = syn(ctx, r.t2);
    let t2t = get_info(t2').syn;
    let t2'': term =
      switch (Option.bind(Option.map(head_reduce(ctx), t2t), typ_of_term)) {
      | None =>
        let i = {...i, syn: Some(default_hole)};
        Mark({i, m: NotTyp(t2t), e: t2'});
      | Some(_) => t2'
      };
    // Adjust syn up
    let i = {...i, syn: Some(Typ({i: default_info}))};
    Arrow({...r, i, t1: t1'', t2: t2''});
  | Fun(r) =>
    let t = syn(ctx, r.t);
    let tt = get_info(t).syn;
    let t: term =
      switch (Option.bind(Option.map(head_reduce(ctx), tt), typ_of_term)) {
      | None =>
        let i = {...i, syn: Some(default_hole)};
        Mark({i, m: NotTyp(tt), e: t});
      | Some(_) => t
      };
    let ctx = [{x: r.x, t, e: None}, ...ctx];
    let e = syn(ctx, r.e);
    let et = get_info(e).syn;
    let syn =
      switch (et) {
      | None => None
      | Some(et) => Some(Arrow({i: default_info, x: r.x, t1: t, t2: et}))
      };
    // Adjust syn up
    let i = {...i, syn};
    Fun({...r, i, t, e});
  | Ap(r) =>
    let e1 = syn(ctx, r.e1);
    let t1 = get_info(e1).syn;
    switch (Option.bind(Option.map(head_reduce(ctx), t1), arrow_of_term)) {
    | None =>
      let i = {...i, syn: Some(default_hole)};
      let e1 = Mark({i, m: FunNotArrow(t1), e: e1});
      let e2 = ana(ctx, default_hole, r.e2);
      Ap({i, e1, e2});
    | Some(Arrow(r1)) =>
      let e2 = ana(ctx, head_reduce(ctx, r1.t1), r.e2);
      let syn = beta_sub(e2, r1.t2);
      // let syn = shift_indices(1, 0, syn);
      let syn = Some(head_reduce(ctx, syn));
      let i = {...i, syn};
      Ap({i, e1, e2});
    | Some(_) => failwith("impossible")
    };
  | Let(r) =>
    let t = syn(ctx, r.t);
    let tt = get_info(t).syn;
    let (t: term, ana1) =
      switch (Option.bind(Option.map(head_reduce(ctx), tt), typ_of_term)) {
      | None =>
        let i = {...i, syn: Some(default_hole)};
        (Mark({i, m: NotTyp(tt), e: t}), default_hole);
      | Some(_) => (t, t)
      };
    let e1 = ana(ctx, head_reduce(ctx, ana1), r.e1);
    let ctx = [{x: r.x, t, e: Some(e1)}, ...ctx];
    let e2 = syn(ctx, r.e2);
    let syn = get_info(e2).syn;
    let syn = Option.map(sub(0, e1), syn); // Delta reduce synthesized type as it leaves the scope of the let binding
    let syn = Option.map(head_reduce(ctx), syn);
    // Adjust syn up
    let i = {...i, syn};
    Let({...r, i, t, e1, e2});
  };
}
// Analytic static judgement. returns new_expression
and ana = (ctx: context, ana_t: term, e: term): term => {
  let i = {...get_info(e), ctx, goal: Some(ana_t)};
  let subsume = () => {
    let e = syn(ctx, e);
    let i = get_info(e);
    let i = {...i, goal: Some(ana_t)};
    let e = set_info(e, i);
    let t = Option.get(get_info(e).syn);
    if (consist(ctx, ana_t, t)) {
      e;
    } else {
      let i = {...i, syn: Some(default_hole)};
      let m = Mismatch(ana_t, t);
      Mark({i, m, e});
    };
  };
  switch (e) {
  | Fun(r1) =>
    switch (arrow_of_term(head_reduce(ctx, ana_t))) {
    | Some(Arrow(r2)) =>
      let t = syn(ctx, r1.t);
      if (consist(ctx, t, r2.t1)) {
        let tt = get_info(t).syn;
        let t: term =
          switch (
            Option.bind(Option.map(head_reduce(ctx), tt), typ_of_term)
          ) {
          | None =>
            let i = {...i, syn: Some(default_hole)};
            Mark({i, m: NotTyp(tt), e: t});
          | Some(_) => t
          };
        let ctx = [{x: r1.x, t, e: None}, ...ctx];
        let ana_t = head_reduce(ctx, r2.t2);
        // let ana_t = shift_indices(1, 0, ana_t); // Adjust ana down
        let e = ana(ctx, ana_t, r1.e);
        Fun({...r1, i, t, e});
      } else {
        subsume();
      };
    | _ => subsume()
    }
  | Let(r) =>
    let t = syn(ctx, r.t);
    let tt = get_info(t).syn;
    let (t: term, ana1) =
      switch (Option.bind(Option.map(head_reduce(ctx), tt), typ_of_term)) {
      | None =>
        let i = {...i, syn: Some(default_hole)};
        (Mark({i, m: NotTyp(tt), e: t}), default_hole);
      | Some(_) => (t, t)
      };
    let e1 = ana(ctx, head_reduce(ctx, ana1), r.e1);
    let ctx = [{x: r.x, t, e: Some(e1)}, ...ctx];
    // Adjust ana down
    let e2 = ana(ctx, ana_t, r.e2);
    Let({...r, i, t, e1, e2});
  | _ => subsume()
  };
};
