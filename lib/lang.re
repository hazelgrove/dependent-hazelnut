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

// let default_hole: term = Hole({i: default_info});

// let rec term_of_pterm = (e: pterm) =>
//   switch (e) {
//   | Hole => Hole({i: default_info})
//   | Typ => Typ({i: default_info})
//   | Var(x) => Var({i: default_info, x, idx: None})
//   | Arrow(x, t1, t2) =>
//     Arrow({
//       i: default_info,
//       x,
//       t1: term_of_pterm_bare(t1),
//       t2: term_of_pterm_bare(t2),
//     })
//   | Fun(x, t, e) =>
//     Fun({i: default_info, x, t: term_of_pterm_bare(t), e: term_of_pterm_bare(e)})
//   | Ap(e1, e2) =>
//     Ap({i: default_info, e1: term_of_pterm_bare(e1), e2: term_of_pterm_bare(e2)})
//   | Let(x, t, e1, e2) =>
//     Let({
//       i: default_info,
//       x,
//       t: term_of_pterm_bare(t),
//       e1: term_of_pterm_bare(e1),
//       e2: term_of_pterm_bare(e2),
//     })
//   };

let rec pterm_of_term_bare = (e: term_bare('a)): pterm =>
  switch (e) {
  | Hole => Hole
  | Typ => Typ
  | Mark(_, e) => pterm_of_term(e)
  | Var(x, _) => Var(x)
  | Arrow(x, t1, t2) => Arrow(x, pterm_of_term(t1), pterm_of_term(t2))
  | Fun(x, t, e) => Fun(x, pterm_of_term(t), pterm_of_term(e))
  | Ap(e1, e2) => Ap(pterm_of_term(e1), pterm_of_term(e2))
  | Let(x, t, e1, e2) =>
    Let(x, pterm_of_term(t), pterm_of_term(e1), pterm_of_term(e2))
  }
and pterm_of_term = ((_, e): term('a)): pterm => pterm_of_term_bare(e);

let rec sterm_of_term_bare = (e: term_bare('a)): sterm =>
  switch (e) {
  | Hole => Hole
  | Typ => Typ
  | Mark(m, e) => Mark(m, sterm_of_term(e))
  | Var(x, idx) => Var(x, idx)
  | Arrow(x, t1, t2) => Arrow(x, sterm_of_term(t1), sterm_of_term(t2))
  | Fun(x, t, e) => Fun(x, sterm_of_term(t), sterm_of_term(e))
  | Ap(e1, e2) => Ap(sterm_of_term(e1), sterm_of_term(e2))
  | Let(x, t, e1, e2) =>
    Let(x, sterm_of_term(t), sterm_of_term(e1), sterm_of_term(e2))
  }
and sterm_of_term = ((_, e): term('a)): sterm => sterm_of_term_bare(e);

// Given a zterm and a corresponding term, find the subterm selected by the
// cursor.
let rec term_at_cursor_bare = (z: zterm, e: term_bare('a)): term('a) =>
  switch (z, e) {
  | (z, Mark(_, e)) => term_at_cursor(z, e)
  | (LArrow(_, z, _), Arrow(_, t1, _)) => term_at_cursor(z, t1)
  | (RArrow(_, _, z), Arrow(_, _, t2)) => term_at_cursor(z, t2)
  | (TFun(_, z, _), Fun(_, t, _)) => term_at_cursor(z, t)
  | (EFun(_, _, z), Fun(_, _, e)) => term_at_cursor(z, e)
  | (LAp(z, _), Ap(e1, _)) => term_at_cursor(z, e1)
  | (RAp(_, z), Ap(_, e2)) => term_at_cursor(z, e2)
  | (TLet(_, z, _, _), Let(_, t, _, _)) => term_at_cursor(z, t)
  | (E1Let(_, _, z, _), Let(_, _, e1, _)) => term_at_cursor(z, e1)
  | (E2Let(_, _, _, z), Let(_, _, _, e2)) => term_at_cursor(z, e2)
  | _ => failwith("term misalignment")
  }

and term_at_cursor = (z: zterm, e: term('a)): term('a) =>
  switch (z, e) {
  | (Cursor(_), _) => e
  // If the cursor is on a name, return the whole parent node
  | (XArrow(_, _, _), _)
  | (XFun(_, _, _), _)
  | (XLet(_, _, _, _), _) => e
  | (z, (_, e)) => term_at_cursor_bare(z, e)
  };

// let get_info = (e: term): info =>
//   switch (e) {
//   | Hole(r) => r.i
//   | Typ(r) => r.i
//   | Mark(r) => r.i
//   | Var(r) => r.i
//   | Arrow(r) => r.i
//   | Fun(r) => r.i
//   | Ap(r) => r.i
//   | Let(r) => r.i
//   };

// let set_info = (e: term, i): term =>
//   switch (e) {
//   | Hole(_) => Hole({i: i})
//   | Typ(_) => Typ({i: i})
//   | Mark(r) => Mark({...r, i})
//   | Var(r) => Var({...r, i})
//   | Arrow(r) => Arrow({...r, i})
//   | Fun(r) => Fun({...r, i})
//   | Ap(r) => Ap({...r, i})
//   | Let(r) => Let({...r, i})
//   };

// Given a zterm and a corresponding term with info, update the info to reflect
// the location of the cursor. This involves three info fields: [cursed],
// [name_cursed], and [cursor_inside].
let rec place_cursor_bare = (z: zterm, e: term_bare(info)): term_bare(info) => {
  switch (z, e) {
  | (z, Mark(m, e)) => Mark(m, place_cursor(z, e))
  | (LArrow(_, z, _), Arrow(x, t1, t2)) =>
    Arrow(x, place_cursor(z, t1), t2)
  | (RArrow(_, _, z), Arrow(x, t1, t2)) =>
    Arrow(x, t1, place_cursor(z, t2))
  | (TFun(_, z, _), Fun(x, t, e)) => Fun(x, place_cursor(z, t), e)
  | (EFun(_, _, z), Fun(x, t, e)) => Fun(x, t, place_cursor(z, e))
  | (LAp(z, _), Ap(e1, e2)) => Ap(place_cursor(z, e1), e2)
  | (RAp(_, z), Ap(e1, e2)) => Ap(e1, place_cursor(z, e2))
  | (TLet(_, z, _, _), Let(x, t, e1, e2)) =>
    Let(x, place_cursor(z, t), e1, e2)
  | (E1Let(_, _, z, _), Let(x, t, e1, e2)) =>
    Let(x, t, place_cursor(z, e1), e2)
  | (E2Let(_, _, _, z), Let(x, t, e1, e2)) =>
    Let(x, t, e1, place_cursor(z, e2))
  | _ => failwith("term misalignment")
  };
}
and place_cursor = (z: zterm, e: info_term): info_term => {
  let name_curse = (i: info) => {...i, name_cursed: true};
  switch (z, e) {
  | (Cursor(_), (i, e)) => ({...i, cursed: true}, e)
  | (XArrow(_, _, _), (i, Arrow(_, _, _) as e)) => (name_curse(i), e)
  | (XFun(_, _, _), (i, Fun(_, _, _) as e)) => (name_curse(i), e)
  | (XLet(_, _, _, _), (i, Let(_, _, _, _) as e)) => (name_curse(i), e)
  | (z, (i, e)) => ({...i, cursor_inside: true}, place_cursor_bare(z, e))
  };
};

// no marks or holes
let rec complete = (e: sterm): bool =>
  switch (e) {
  | Typ
  | Var(_) => true
  | Hole
  | Mark(_) => false
  | Ap(e1, e2) => complete(e1) && complete(e2)
  | Arrow(_, t1, t2) => complete(t1) && complete(t2)
  | Fun(_, t, e) => complete(t) && complete(e)
  | Let(_, t, e1, e2) => complete(t) && complete(e1) && complete(e2)
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

let rec terms_equal = (e1, e2: sterm): bool => {
  switch (e1: sterm, e2: sterm) {
  | (Hole, Hole)
  | (Typ, Typ) => true
  | (Mark(m1, e1), Mark(m2, e2)) when m1 == m2 => terms_equal(e1, e2)
  | (Var(x1, _), Var(x2, _)) => x1 == x2
  | (Arrow(x1, t1, t2), Arrow(x2, t3, t4)) =>
    names_equal(x1, x2) && terms_equal(t1, t3) && terms_equal(t2, t4)
  | (Fun(x1, t1, e1), Fun(x2, t2, e2)) =>
    names_equal(x1, x2) && terms_equal(t1, t2) && terms_equal(e1, e2)
  | (Ap(e1, e2), Ap(e3, e4)) => terms_equal(e1, e3) && terms_equal(e2, e4)
  | (Let(x1, t1, e1, e2), Let(x2, t2, e3, e4)) =>
    names_equal(x1, x2)
    && terms_equal(t1, t2)
    && terms_equal(e1, e3)
    && terms_equal(e2, e4)
  | _ => false
  };
};

// SECTION: CONSISTENCY, MATCHING, AND REDUCTION

// shift indices above threshold t by n
// todo: shift inside info? inside marks?
let rec shift_indices = (n: int, t: int, e: sterm): sterm =>
  switch (e) {
  | Hole
  | Typ => e
  | Mark(m, e) => Mark(m, shift_indices(n, t, e))
  | Ap(e1, e2) => Ap(shift_indices(n, t, e1), shift_indices(n, t, e2))
  | Arrow(x, t1, t2) =>
    Arrow(
      x,
      shift_indices(n, t, t1),
      shift_indices(n, 1 + t, t2) // increase threshold under binding
    )
  | Fun(x, t1, e) =>
    Fun(
      x,
      shift_indices(n, t, t1),
      shift_indices(n, 1 + t, e) // increase threshold under binding
    )
  | Let(x, t1, e1, e2) =>
    Let(
      x,
      shift_indices(n, t, t1),
      shift_indices(n, t, e1),
      shift_indices(n, 1 + t, e2) // increase threshold under binding
    )
  | Var(x, idx) =>
    // if index is at least threshold, increase by n
    let index_map = i =>
      if (i >= t) {
        i + n;
      } else {
        i;
      };
    Var(x, Option.map(index_map, idx));
  };

// replaces variables in e2 that are "free by a distance of n" with e1
let rec sub = (n: int, e1: sterm, e2: sterm): sterm => {
  switch (e2) {
  | Hole
  | Typ => e2
  | Var(_, idx) => idx == Some(n) ? e1 : e2
  | Mark(m, e) => Mark(m, sub(n, e1, e))
  | Ap(e1, e2) => Ap(sub(n, e1, e1), sub(n, e1, e2))
  | Arrow(x, t1, t2) =>
    Arrow(
      x,
      sub(n, e1, t1),
      sub(1 + n, shift_indices(1, 0, e1), t2) // increase n under binding
    )
  | Fun(x, t, e) =>
    Fun(
      x,
      sub(n, e1, t),
      sub(1 + n, shift_indices(1, 0, e1), e) // increase n under binding
    )
  | Let(x, t, e1, e2) =>
    Let(
      x,
      sub(n, e1, t),
      sub(n, e1, e1),
      sub(1 + n, shift_indices(1, 0, e1), e2) // increase n under binding
    )
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
let rec head_reduce = (ctx: context, offset: int, e: sterm): sterm => {
  switch (e) {
  | Hole
  | Typ
  | Arrow(_)
  | Fun(_)
  | Mark(_)
  | Let(_) => e
  // DELTA REDUCTION:
  | Var(_, idx) =>
    // if the variable is bound...
    switch (idx) {
    | None => e
    | Some(idx) =>
      // ... in the passed-in context ...
      if (idx < offset) {
        e;
      } else {
        // ... and is associated with an actual value ...
        switch (List.nth(ctx, idx - offset).e) {
        | None
        | Some(Hole) => e // (declarations with a hole body are considered axioms. this could be changed)
        | Some(e) =>
          // ... THEN you can replace it with its definition and continue reducing
          let e = shift_indices(idx + 1, 0, e); // but first we have to shift indices to account for the context change
          head_reduce(ctx, offset, e);
        };
      }
    }
  // BETA REDUCTION
  | Ap(e1, e2) =>
    switch (head_reduce(ctx, offset, e1)) {
    | Fun(_, _, e) => head_reduce(ctx, offset, beta_sub(e2, e)) // if it's an application of a function abstraction, substitute and continue
    | Hole
    | Mark(_) => Hole // if it's an application of a hole or mark, reduce to a hole
    | _ => Ap(e1, e2)
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
  reduct1: sterm,
  reduct2: sterm,
};

// Checks consistency of head-reduced terms
// if they have the same head, check consistency of the children
let rec head_consist =
        (ctx: context, offset: int, t1, t2: sterm): consist_return => {
  // let unmarked =
  switch (t1: sterm, t2: sterm) {
  | (Hole, _)
  | (_, Hole)
  | (Typ, Typ)
  | (Mark(_), _)
  | (_, Mark(_)) => {consistent: true, reduct1: t1, reduct2: t2}
  | (Var(_, idx1), Var(_, idx2)) => {
      consistent: idx1 == idx2,
      reduct1: t1,
      reduct2: t2,
    }
  | (Arrow(x1, t1, t2), Arrow(x2, t3, t4)) =>
    let consist_t1 = consist(ctx, offset, t1, t3);
    let consist_t2 = consist(ctx, offset + 1, t2, t4);
    {
      consistent: consist_t1.consistent && consist_t2.consistent,
      reduct1: Arrow(x1, consist_t1.reduct1, consist_t2.reduct1),
      reduct2: Arrow(x2, consist_t1.reduct2, consist_t2.reduct2),
    };
  | (Fun(x1, t1, e1), Fun(x2, t2, e2)) =>
    let consist_t = consist(ctx, offset, t1, t2);
    let consist_e = consist(ctx, offset + 1, e1, e2);
    {
      consistent: consist_t.consistent && consist_e.consistent,
      reduct1: Fun(x1, consist_t.reduct1, consist_e.reduct1),
      reduct2: Fun(x2, consist_t.reduct2, consist_e.reduct2),
    };
  | (Ap(e1, e2), Ap(e3, e4)) =>
    let consist_e1 = consist(ctx, offset, e1, e3);
    let consist_e2 = consist(ctx, offset, e2, e4);
    {
      consistent: consist_e1.consistent && consist_e2.consistent,
      reduct1: Ap(consist_e1.reduct1, consist_e2.reduct1),
      reduct2: Ap(consist_e1.reduct2, consist_e2.reduct2),
    };
  | (Let(x1, t1, e1, e2), Let(x2, t2, e3, e4)) =>
    let consist_t = consist(ctx, offset, t1, t2);
    let consist_e1 = consist(ctx, offset, e1, e3);
    let consist_e2 = consist(ctx, offset + 1, e2, e4);
    {
      consistent:
        consist_t.consistent && consist_e1.consistent && consist_e2.consistent,
      reduct1:
        Let(x1, consist_t.reduct1, consist_e1.reduct1, consist_e2.reduct1),
      reduct2:
        Let(x2, consist_t.reduct2, consist_e1.reduct2, consist_e2.reduct2),
    };
  | _ => {
      consistent: false,
      reduct1: Mark(Inconsistent, t1),
      reduct2: Mark(Inconsistent, t2),
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
and consist = (ctx: context, offset, t1, t2: sterm): consist_return => {
  let (t1', t2') = (
    head_reduce(ctx, offset, t1),
    head_reduce(ctx, offset, t2),
  );
  head_consist(ctx, offset, t1', t2');
};

// Matched Set type. Invariant: either returns None or Some(Typ(_))
let typ_of_term = (t: sterm): option(sterm) => {
  switch (t) {
  | Hole
  | Typ => Some(Typ)
  | _ => None
  };
};

// Matched arrow type. Invariant: either returns None or Some(Arrow(_))
let arrow_of_term = (t: sterm): option(sterm) => {
  switch (t) {
  | Hole => Some(Arrow(Hole, Hole, Hole))
  | Arrow(_) => Some(t)
  | _ => None
  };
};

// SECTION: STATIC ELABORATION

let apply_mark = (i, m, e) => {
  let i = {...i, syn: Some(Hole)}; // marks synthesize hole
  (i, Mark(m, e));
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
let rec syn = (ctx: context, e: pterm): info_term => {
  let i = {...default_info, ctx}; // set the context in the info of the synthesizing term
  switch (e) {
  | Hole =>
    let i = {...i, syn: Some(Hole)}; // hole synthesizes hole
    (i, Hole);
  | Typ =>
    let i = {...i, syn: Some(Typ)}; // type synthesizes type
    (i, Typ);
  | Var(x) =>
    // scans the list ctx_arg for the first entry with the same name as the variable
    // optionally returns (the index, the type) of that entry if found
    let rec lookup_name = (idx, ctx_arg) =>
      switch (ctx_arg) {
      | [] => None
      | [c, ..._] when c.x == Text(x) => Some((idx, c.t))
      | [_, ...ctx_arg] => lookup_name(idx + 1, ctx_arg)
      };
    switch (lookup_name(0, ctx)) {
    | None => apply_mark(i, UnknownVar(x), (i, Var(x, None))) // mark free variable
    | Some((idx, t)) =>
      // If the variable is bound to type t...
      let t = shift_indices(idx + 1, 0, t); // ... shift t's indices to account for context difference (equivalent to shifting context entries as they go under binders)
      let t = head_reduce(ctx, 0, t); // ... and head reduce it...
      let i = {...i, syn: Some(t)}; // ... and synthesize it.
      (i, Var(x, Some(idx)));
    };
  | Arrow(x, t1, t2) =>
    let t1 = syn(ctx, t1); // process left side
    let (t1i, _) = t1; // get left side info
    let t1t = t1i.syn; // get left side synthesized type
    let t1 = mark_if_not_typ(i, t1t, t1); // ensure it's a typ
    let ctx = [{x, t: sterm_of_term(t1), e: None}, ...ctx]; // update context with arrow's binding (it's a dependent arrow)
    let t2 = syn(ctx, t2); // process right side
    let (t2i, _) = t2; // get right side info
    let t2t = t2i.syn; // get right side synthesized type
    let t2 = mark_if_not_typ(i, t2t, t2); // ensure it's a typ
    let i = {...i, syn: Some(Typ)}; // synthesize typ
    (i, Arrow(x, t1, t2));
  | Fun(x, t, e) =>
    let t = syn(ctx, t); // process annotation
    let (ti, _) = t; // get annotation info
    let tt = ti.syn; // get annotation synthesized type
    let t = mark_if_not_typ(i, tt, t); // ensure it's a typ
    let ctx = [{x, t: sterm_of_term(t), e: None}, ...ctx]; // update context with binding
    let e = syn(ctx, e); // process body
    let (ei, _) = e; // get body info
    let et = ei.syn; // get synthesized type
    let syn =
      Option.map((et): sterm => Arrow(x, sterm_of_term(t), et), et); // synthesize arrow type
    let i = {...i, syn};
    (i, Fun(x, t, e));
  | Ap(e1, e2) =>
    let e1 = syn(ctx, e1); // process left side
    let (i1, _) = e1;
    let t1 = i1.syn; // get synthesized type
    // does t1 match arrow?
    switch (Option.bind(t1, arrow_of_term)) {
    | None =>
      // if not:
      let e1 = apply_mark(i, FunNotArrow(t1), e1); // mark the left side
      let e2 = ana(ctx, Hole: sterm, e2); // analyze the right side against hole
      let i = {...i, syn: Some(Hole)}; // and synthesize Hole
      (i, Ap(e1, e2));
    | Some(Arrow(_, t1, t2)) =>
      // if so:
      let e2 = ana(ctx, head_reduce(ctx, 0, t1), e2); // analyze the argument against the domain type
      let syn = beta_sub(sterm_of_term(e2), t2); // synthesize the return type with the argument substituted in
      let syn = Some(head_reduce(ctx, 0, syn)); // head reduce synthesized type
      let i = {...i, syn};
      (i, Ap(e1, e2));
    | Some(_) => failwith("impossible") // because arrow_of_term only returns None or Some(Arrow(_))
    };
  | Let(x, t, e1, e2) =>
    let t = syn(ctx, t); // process annotation
    let (ti, _) = t; // get annotation info
    let tt = ti.syn; // get annotation synthesized type
    let t = mark_if_not_typ(i, tt, t); // ensure it's a typ
    let ana1: sterm =
      // the expected type is hole if the annotation is not a typ
      switch (t) {
      | (_, Mark(_)) => Hole
      | t => sterm_of_term(t)
      };
    let e1 = ana(ctx, head_reduce(ctx, 0, ana1), e1); // analyze bound term
    let se1 = sterm_of_term(e1); // stripped e1
    let ctx = [{x, t: sterm_of_term(t), e: Some(se1)}, ...ctx]; // update the context to reflect the binding
    let e2 = syn(ctx, e2); // process rest
    let (e2i, _) = e2; // get rest info
    let syn = e2i.syn; // get rest synthesized type
    let syn = Option.map(sub(0, se1), syn); // delta reduce synthesized type as it leaves the scope of the let binding
    let syn = Option.map(head_reduce(ctx, 0), syn); // head reduce it
    // adjust syn up? I would think the indices need to be shifted as the synthesized type passes over the binding, but I haven't needed it yet
    let i = {...i, syn}; // and synthesize it
    (i, Let(x, t, e1, e2));
  };
}
// Analytic static judgement. returns new_expression with info filled out and marks inserted
// Precondition: analyzed type is head reduced
and ana = (ctx: context, ana_t: sterm, e: pterm): info_term => {
  let i = {...default_info, ctx, goal: Some(ana_t)}; // set the context and the goal in the info of the analyzing term
  let subsume = (): info_term => {
    // the expression if subsumption is used:
    let (i, e) = syn(ctx, e); // synthesize the expression
    let i = {...i, goal: Some(ana_t)}; // add the goal to the info
    let t = Option.get(i.syn); // get synthesized type
    // is it consistent with the analyzed type?
    // we can use head_consist because both arguments are already head reduced
    let consistent = head_consist(ctx, 0, ana_t, t);
    if (consistent.consistent) {
      // if so, do nothing
      let e = (i, e);
      e;
    } else {
      // if not, mark it
      let m = Mismatch(consistent.reduct1, consistent.reduct2);
      apply_mark(i, m, (i, e));
    };
  };
  switch (e) {
  | Fun(x, t, e) =>
    // does the analyzed type match arrow?
    switch (arrow_of_term(ana_t)) {
    | Some(Arrow(_, t1, t2)) =>
      // if so:
      let t = syn(ctx, t); // process annotation
      let st = sterm_of_term(t); // stripped term
      // is the annotation consistent with the domain of the goal type?
      if (consist(ctx, 0, st, t1).consistent) {
        // if so:
        let (ti, _) = t; // get annotation info
        let tt = ti.syn; // get annotation synthesized type
        let t = mark_if_not_typ(i, tt, t); // ensure it's a typ
        let ctx = [{x, t: st, e: None}, ...ctx]; // extend context with binding
        let e = ana(ctx, head_reduce(ctx, 0, t2), e); // analyze body against return type of goal type
        (i, Fun(x, t, e));
      } else {
        subsume(); // if inconsistent, subsume
      };
    | _ => subsume() // if goal doesn't match arrow, subsume
    }
  | Let(x, t, e1, e2) =>
    let t = syn(ctx, t); // process annotation
    let (ti, _) = t; // get annotation info
    let tt = ti.syn; // get annotation synthesized type
    let t = mark_if_not_typ(i, tt, t); // ensure it's a typ
    let st = sterm_of_term(t); // stripped term
    let ana1: sterm =
      // the expected type is hole if the annotation is not a typ
      switch (t) {
      | (_, Mark(_)) => Hole
      | t => sterm_of_term(t)
      };
    let e1 = ana(ctx, head_reduce(ctx, 0, ana1), e1); // analyze bound term
    let ctx = [{x, t: st, e: Some(sterm_of_term(e1))}, ...ctx]; // update the context to reflect the binding
    let ana_t = shift_indices(1, 0, ana_t); // adjust ana down - shift free indices up by one as the type descends under the binding
    let e2 = ana(ctx, ana_t, e2); // analyze body against goal
    (i, Let(x, t, e1, e2));
  | _ => subsume() // if not arrow or fun, subsume
  };
};
