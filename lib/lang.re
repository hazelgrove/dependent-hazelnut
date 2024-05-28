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

// Structural equality - not used in core logic

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

// SECTION: CONSISTENCY, MATCHING, AND REDUCTION

// shift indices above threshold t by n
// todo: shift inside info? inside marks?
let rec shift_indices = (n: int, t: int, e: term) =>
  switch (e) {
  | Hole(_)
  | Typ(_) => e
  | Mark(r) => Mark({...r, e: shift_indices(n, t, r.e)})
  | Ap(r) =>
    Ap({...r, e1: shift_indices(n, t, r.e1), e2: shift_indices(n, t, r.e2)})
  | Arrow(r) =>
    Arrow({
      ...r,
      t1: shift_indices(n, t, r.t1),
      t2: shift_indices(n, 1 + t, r.t2) // increase threshold under binding
    })
  | Fun(r) =>
    Fun({
      ...r,
      t: shift_indices(n, t, r.t),
      e: shift_indices(n, 1 + t, r.e) // increase threshold under binding
    })
  | Let(r) =>
    Let({
      ...r,
      t: shift_indices(n, t, r.t),
      e1: shift_indices(n, t, r.e1),
      e2: shift_indices(n, 1 + t, r.e2) // increase threshold under binding
    })
  | Var(r) =>
    // if index is at least threshold, increase by n
    let index_map = i =>
      if (i >= t) {
        i + n;
      } else {
        i;
      };
    Var({...r, idx: Option.map(index_map, r.idx)});
  };

// replaces variables in e2 that are "free by a distance of n" with e1
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
      t2: sub(1 + n, shift_indices(1, 0, e1), r.t2) // increase n under binding
    })
  | Fun(r) =>
    Fun({
      ...r,
      t: sub(n, e1, r.t),
      e: sub(1 + n, shift_indices(1, 0, e1), r.e) // increase n under binding
    })
  | Let(r) =>
    Let({
      ...r,
      t: sub(n, e1, r.t),
      e1: sub(n, e1, r.e1),
      e2: sub(1 + n, shift_indices(1, 0, e1), r.e2) // increase n under binding
    })
  };
};

// replaces variables in e2 that are free by a distance of 0 with e1 AND shifts the other free variables in e2 down by one
// specifically, (lambda e2)(e1) beta reduces to beta_sub(e1, e2)
let beta_sub = (e1, e2) => {
  let e1' = shift_indices(1, 0, e1);
  let e = sub(0, e1', e2);
  shift_indices(-1, 0, e);
};

// Beta and delta reduce until head is exposed, if possible
let rec head_reduce = (ctx: context, offset: int, e: term): term => {
  switch (e) {
  | Hole(_)
  | Typ(_)
  | Arrow(_)
  | Fun(_)
  | Mark(_)
  | Let(_) => e
  // DELTA REDUCTION:
  | Var(r) =>
    // if the variable is bound...
    switch (r.idx) {
    | None => e
    | Some(idx) =>
      // ... in the passed-in context ...
      if (idx < offset) {
        e;
      } else {
        // ... and is associated with an actual value ...
        switch (List.nth(ctx, idx - offset).e) {
        | None
        | Some(Hole(_)) => e // (declarations with a hole body are considered axioms. this could be changed)
        | Some(e) =>
          // ... THEN you can replace it with its definition and continue reducing
          let e = shift_indices(idx + 1, 0, e); // but first we have to shift indices to account for the context change
          head_reduce(ctx, offset, e);
        };
      }
    }
  // BETA REDUCTION
  | Ap(r) =>
    switch (head_reduce(ctx, offset, r.e1)) {
    | Fun(r2) => head_reduce(ctx, offset, beta_sub(r.e2, r2.e)) // if it's an application of a function abstraction, substitute and continue
    | Hole(r2) => Hole(r2) // if it's an application of a hole, reduce to a hole
    | _ => Ap(r)
    }
  };
};

// let rec full_reduce = (ctx: context, offset: int, e : term) => {
//   switch(e) {
//     | Var(_) => head_reduce(ctx, offset, e);
//     | Ap(r) => {
//       let e1 = full_reduce(ctx, offset, r.e1);
//       let e = head_reduce
//     }
//   }
// }

let consist_name = (x1, x2: name): bool => {
  switch (x1: name, x2: name) {
  | (Hole, _)
  | (_, Hole) => true
  | (Text(x1), Text(x2)) => x1 == x2
  };
};

type consist_return = {
  consistent: bool,
  reduct1: term,
  reduct2: term,
};

// Checks consistency of head-reduced terms
// if they have the same head, check consistency of the children
let rec head_consist =
        (ctx: context, offset: int, t1, t2: term): consist_return => {
  // let unmarked =
  switch (t1: term, t2: term) {
  | (Hole(_), _)
  | (_, Hole(_))
  | (Typ(_), Typ(_))
  | (Mark(_), _)
  | (_, Mark(_)) => {consistent: true, reduct1: t1, reduct2: t2}
  | (Var(r1), Var(r2)) => {
      consistent: r1.idx == r2.idx,
      reduct1: t1,
      reduct2: t2,
    }
  | (Arrow(r1), Arrow(r2)) =>
    let consist_t1 = consist(ctx, offset, r1.t1, r2.t1);
    let consist_t2 = consist(ctx, offset + 1, r1.t2, r2.t2);
    {
      consistent: consist_t1.consistent && consist_t2.consistent,
      reduct1: Arrow({...r1, t1: consist_t1.reduct1, t2: consist_t2.reduct1}),
      reduct2: Arrow({...r2, t1: consist_t1.reduct2, t2: consist_t2.reduct2}),
    };
  | (Fun(r1), Fun(r2)) =>
    let consist_t = consist(ctx, offset, r1.t, r2.t);
    let consist_e = consist(ctx, offset + 1, r1.e, r2.e);
    {
      consistent: consist_t.consistent && consist_e.consistent,
      reduct1: Fun({...r1, t: consist_t.reduct1, e: consist_e.reduct1}),
      reduct2: Fun({...r2, t: consist_t.reduct2, e: consist_e.reduct2}),
    };
  | (Ap(r1), Ap(r2)) =>
    let consist_e1 = consist(ctx, offset, r1.e1, r2.e1);
    let consist_e2 = consist(ctx, offset, r1.e2, r2.e2);
    {
      consistent: consist_e1.consistent && consist_e2.consistent,
      reduct1: Ap({...r1, e1: consist_e1.reduct1, e2: consist_e2.reduct1}),
      reduct2: Ap({...r2, e1: consist_e1.reduct2, e2: consist_e2.reduct2}),
    };
  | (Let(r1), Let(r2)) =>
    let consist_t = consist(ctx, offset, r1.t, r2.t);
    let consist_e1 = consist(ctx, offset, r1.e1, r2.e1);
    let consist_e2 = consist(ctx, offset + 1, r1.e2, r2.e2);
    {
      consistent:
        consist_t.consistent && consist_e1.consistent && consist_e2.consistent,
      reduct1:
        Let({
          ...r1,
          t: consist_t.reduct1,
          e1: consist_e1.reduct1,
          e2: consist_e2.reduct1,
        }),
      reduct2:
        Let({
          ...r2,
          t: consist_t.reduct2,
          e1: consist_e1.reduct2,
          e2: consist_e2.reduct2,
        }),
    };
  | _ => {
      consistent: false,
      reduct1: Mark({i: default_info, m: Inconsistent, e: t1}),
      reduct2: Mark({i: default_info, m: Inconsistent, e: t2}),
    }
  // let mark_inconsistent = t => Mark({i: default_info, m: Inconsistent, e: t});
  // if (unmarked.consistent) {
  //   unmarked;
  // } else {
  //   {
  //     ...unmarked,
  //     reduct1: mark_inconsistent(unmarked.reduct1),
  //     reduct2: mark_inconsistent(unmarked.reduct2),
  //   };
  // };
  };
}
and consist = (ctx: context, offset, t1, t2: term): consist_return => {
  let (t1', t2') = (
    head_reduce(ctx, offset, t1),
    head_reduce(ctx, offset, t2),
  );
  head_consist(ctx, offset, t1', t2');
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

let apply_mark = (i, m, e) => {
  let i = {...i, syn: Some(default_hole)}; // marks synthesize hole
  Mark({i, m, e});
};

let mark_if_not_typ = (i, t, e) => {
  // does t match typ?
  switch (Option.bind(t, typ_of_term)) {
  | None => apply_mark(i, NotTyp(t), e) // if not, mark e
  | Some(_) => e // if so, just e
  };
};

// Idea : instead of storing contexts, make a new "bind" node that is invisible, just adds an entry to the context. goes between a let and its .e2, for example
// Why? Because when a type "changed locations" (e.g. teleports to be synthesized by a variable), its context changes. this matters for a more natural implementation of delta reduction (no offset parameter)
// and for checking whether a variable has been captured, or is in scope, when displaying a syn or ana type.

// Synthetic static judgement. returns new_expression with info filled out and marks inserted
// Postcondition: synthesized type is head reduced
let rec syn = (ctx: context, e: term): term => {
  let i = {...default_info, ctx}; // set the context in the info of the synthesizing term
  switch (e) {
  | Hole(_) =>
    let i = {...i, syn: Some(default_hole)}; // hole synthesizes hole
    Hole({i: i});
  | Typ(_) =>
    let i = {...i, syn: Some(Typ({i: default_info}))}; // type synthesizes type
    Typ({i: i});
  | Mark(_) => failwith("unreachable") // there are no marks in unmarked terms
  | Var(r) =>
    // scans the list ctx_arg for the first entry with the same name as the variable
    // optionally returns (the index, the type) of that entry if found
    let rec lookup_name = (idx, ctx_arg) =>
      switch (ctx_arg) {
      | [] => None
      | [c, ..._] when c.x == Text(r.x) => Some((idx, c.t))
      | [_, ...ctx_arg] => lookup_name(idx + 1, ctx_arg)
      };
    switch (lookup_name(0, ctx)) {
    | None => apply_mark(i, UnknownVar(r.x), e) // mark free variable
    | Some((idx, t)) =>
      // If the variable is bound to type t...
      let t = shift_indices(idx + 1, 0, t); // ... shift t's indices to account for context difference (equivalent to shifting context entries as they go under binders)
      let t = head_reduce(ctx, 0, t); // ... and head reduce it...
      let i = {...i, syn: Some(t)}; // ... and synthesize it.
      Var({...r, i, idx: Some(idx)});
    };
  | Arrow(r) =>
    let t1 = syn(ctx, r.t1); // process left side
    let t1t = get_info(t1).syn; // get synthesized type
    let t1 = mark_if_not_typ(i, t1t, t1); // ensure it's a typ
    let ctx = [{x: r.x, t: t1, e: None}, ...ctx]; // uupdate context with arrow's binding (it's a dependent arrow)
    let t2 = syn(ctx, r.t2); // process right side
    let t2t = get_info(t2).syn; // get synthesized type
    let t2 = mark_if_not_typ(i, t2t, t2); // ensure it's a typ
    let i = {...i, syn: Some(Typ({i: default_info}))}; // synthesize typ
    Arrow({...r, i, t1, t2});
  | Fun(r) =>
    let t = syn(ctx, r.t); // process annotation
    let tt = get_info(t).syn; // get synthesized type
    let t = mark_if_not_typ(i, tt, t); // ensure it's a typ
    let ctx = [{x: r.x, t, e: None}, ...ctx]; // update context with binding
    let e = syn(ctx, r.e); // process body
    let et = get_info(e).syn; // get synthesized type
    let syn =
      Option.map(et => Arrow({i: default_info, x: r.x, t1: t, t2: et}), et); // synthesize arrow type
    let i = {...i, syn};
    Fun({...r, i, t, e});
  | Ap(r) =>
    let e1 = syn(ctx, r.e1); // process left side
    let t1 = get_info(e1).syn; // get synthesized type
    // does t1 match arrow?
    switch (Option.bind(t1, arrow_of_term)) {
    | None =>
      // if not:
      let e1 = apply_mark(i, FunNotArrow(t1), e1); // mark the left side
      let e2 = ana(ctx, default_hole, r.e2); // analyze the right side against hole
      Ap({i, e1, e2});
    | Some(Arrow(r1)) =>
      // if so:
      let e2 = ana(ctx, head_reduce(ctx, 0, r1.t1), r.e2); // analyze the argument against the domain type
      let syn = beta_sub(e2, r1.t2); // synthesize the return type with the argument substituted in
      let syn = Some(head_reduce(ctx, 0, syn)); // head reduce synthesized type
      let i = {...i, syn};
      Ap({i, e1, e2});
    | Some(_) => failwith("impossible") // because arrow_of_term only returns None or Some(Arrow(_))
    };
  | Let(r) =>
    let t = syn(ctx, r.t); // process annotation
    let tt = get_info(t).syn; // get synthesized type
    let t = mark_if_not_typ(i, tt, t); // ensure it's a typ
    let ana1 =
      // the expected type is hole if the annotation is not a typ
      switch (t) {
      | Mark(_) => default_hole
      | t => t
      };
    let e1 = ana(ctx, head_reduce(ctx, 0, ana1), r.e1); // analyze bound term
    let ctx = [{x: r.x, t, e: Some(e1)}, ...ctx]; // update the context to reflect the binding
    let e2 = syn(ctx, r.e2); // process rest
    let syn = get_info(e2).syn; // get synthesized type of rest
    let syn = Option.map(sub(0, e1), syn); // delta reduce synthesized type as it leaves the scope of the let binding
    let syn = Option.map(head_reduce(ctx, 0), syn); // head reduce it
    // adjust syn up? I would think the indices need to be shifted as the synthesized type passes over the binding, but I haven't need it yet
    let i = {...i, syn}; // and synthesize it
    Let({...r, i, t, e1, e2});
  };
}
// Analytic static judgement. returns new_expression with info filled out and marks inserted
// Precondition: analyzed type is head reduced
and ana = (ctx: context, ana_t: term, e: term): term => {
  let i = {...default_info, ctx, goal: Some(ana_t)}; // set the context and the goal in the info of the analyzing term
  let subsume = () => {
    // the expression if subsumption is used:
    let e = syn(ctx, e); // synthesize the expression
    let e = set_info(e, {...get_info(e), goal: Some(ana_t)}); // add the goal to the info
    // print_endline("getting option...");
    let t = Option.get(get_info(e).syn); // get synthesized type
    // print_endline("gotten!");
    // is it consistent with the analyzed type?
    // we can use head_consist because both arguments are already head reduced
    let consistent = head_consist(ctx, 0, ana_t, t);
    if (consistent.consistent) {
      e; // if so, do nothing
    } else {
      // if not, mark it
      apply_mark(
        i,
        Mismatch(consistent.reduct1, consistent.reduct2),
        e,
      );
    };
  };
  switch (e) {
  | Fun(r1) =>
    // does the analyzed type match arrow?
    switch (arrow_of_term(ana_t)) {
    | Some(Arrow(r2)) =>
      // if so:
      let t = syn(ctx, r1.t); // process annotation
      // is the annotation consistent with the domain of the goal type?
      if (consist(ctx, 0, t, r2.t1).consistent) {
        // if so:
        let tt = get_info(t).syn; // get the synthesized type of the annotation
        let t = mark_if_not_typ(i, tt, t); // ensure it's a typ
        let ctx = [{x: r1.x, t, e: None}, ...ctx]; // extend context with binding
        let e = ana(ctx, head_reduce(ctx, 0, r2.t2), r1.e); // analyze body against return type of goal type
        Fun({...r1, i, t, e});
      } else {
        subsume(); // if inconsistent, subsume
      };
    | _ => subsume() // if goal doesn't match arrow, subsume
    }
  | Let(r) =>
    let t = syn(ctx, r.t); // process annotation
    let tt = get_info(t).syn; // get synthesized type
    let t = mark_if_not_typ(i, tt, t); // ensure it's a typ
    let ana1 =
      // the expected type is hole if the annotation is not a typ
      switch (t) {
      | Mark(_) => default_hole
      | t => t
      };
    let e1 = ana(ctx, head_reduce(ctx, 0, ana1), r.e1); // analyze bound term
    let ctx = [{x: r.x, t, e: Some(e1)}, ...ctx]; // update the context to reflect the binding
    let ana_t = shift_indices(1, 0, ana_t); // adjust ana down - shift free indices up by one as the type descends under the binding
    let e2 = ana(ctx, ana_t, r.e2); // analyze body against goal
    Let({...r, i, t, e1, e2});
  | _ => subsume() // if not arrow or fun, subsume
  };
};
