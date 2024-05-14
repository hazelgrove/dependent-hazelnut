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
  c: [],
  // completes: [],
  en: [],
  goal: None,
  syn: None,
  cursed: false,
  name_cursed: false,
};

let default_hole: term = Hole({i: default_info});

let rec term_of_pterm = (e: pterm) =>
  switch (e) {
  | Hole => Hole({i: default_info})
  | Typ => Typ({i: default_info})
  | Var(x) => Var({i: default_info, x})
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

let string_of_name = (x: name): string =>
  switch (x) {
  | Hole => "Hole"
  | Text(x) => "Text(\"" ++ x ++ "\")"
  };

// let rec string_of_mark = (m: mark): string =>
//   switch (m) {
//   | UnknownVar(x) => "UnknownVar(\"" ++ x ++ "\")"
//   | FunNotArrow(t) => "FunNotArrow(" ++ string_of_pterm(t) ++ ")"
//   | Mismatch(t1, t2) =>
//     " Mismatch(" ++ string_of_pterm(t1) ++ "," ++ string_of_pterm(t2) ++ ")"
//   | NotTyp(t) => "NotTyp(" ++ string_of_pterm(t) ++ ")"
// }

let rec string_of_pterm = (e: pterm): string =>
  switch (e) {
  | Hole => "Hole"
  | Typ => "Typ"
  | Var(x) => "Var(\"" ++ x ++ "\")"
  | Arrow(x, t1, t2) =>
    "Arrow("
    ++ string_of_name(x)
    ++ ","
    ++ string_of_pterm(t1)
    ++ ","
    ++ string_of_pterm(t2)
    ++ ")"
  | Fun(x, t, e) =>
    "Fun("
    ++ string_of_name(x)
    ++ ","
    ++ string_of_pterm(t)
    ++ ","
    ++ string_of_pterm(e)
    ++ ")"
  | Ap(e1, e2) =>
    "Ap(" ++ string_of_pterm(e1) ++ "," ++ string_of_pterm(e2) ++ ")"
  | Let(x, t, e1, e2) =>
    "Let("
    ++ string_of_name(x)
    ++ ","
    ++ string_of_pterm(t)
    ++ ","
    ++ string_of_pterm(e1)
    ++ ","
    ++ string_of_pterm(e2)
    ++ ")"
  };

let rec place_cursor = (z: zterm, e: term): term => {
  print_endline(string_of_pterm(pterm_of_zterm(z)));
  // let curse = (i: info) => {...i, cursed: true};
  let name_curse = (i: info) => {...i, name_cursed: true};
  switch (z, e) {
  | (Cursor(_), e) => set_info(e, {...get_info(e), cursed: true})
  | (z, Mark(r)) => Mark({...r, e: place_cursor(z, r.e)})
  | (XArrow(_, _, _), Arrow(r)) => Arrow({...r, i: name_curse(r.i)})
  | (LArrow(_, z, _), Arrow(r)) => Arrow({...r, t1: place_cursor(z, r.t1)})
  | (RArrow(_, _, z), Arrow(r)) => Arrow({...r, t2: place_cursor(z, r.t2)})
  | (XFun(_, _, _), Fun(r)) => Fun({...r, i: name_curse(r.i)})
  | (TFun(_, z, _), Fun(r)) => Fun({...r, t: place_cursor(z, r.t)})
  | (EFun(_, _, z), Fun(r)) => Fun({...r, e: place_cursor(z, r.e)})
  | (LAp(z, _), Ap(r)) => Ap({...r, e1: place_cursor(z, r.e1)})
  | (RAp(_, z), Ap(r)) => Ap({...r, e2: place_cursor(z, r.e2)})
  | (XLet(_, _, _, _), Let(r)) => Let({...r, i: name_curse(r.i)})
  | (TLet(_, z, _, _), Let(r)) => Let({...r, t: place_cursor(z, r.t)})
  | (E1Let(_, _, z, _), Let(r)) => Let({...r, e1: place_cursor(z, r.e1)})
  | (E2Let(_, _, _, z), Let(r)) => Let({...r, e2: place_cursor(z, r.e2)})
  | _ => failwith("term misalignment")
  };
};

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
  | Hole(_) => en
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

// let rec term_of_pure_term = (e: pure_term): term => {
//   let e' =
//     switch (e) {
//     | Hole => Hole
//     | Typ => Typ
//     | Var(x) => Var(x)
//     | Arrow(x, t1, t2) =>
//       Arrow(x, t1), t2))
//     | Fun(x, t, e) => Fun(x, t), e))
//     | Ap(e1, e2) => Ap(e1), e2))
//     | Let(x, t, e1, e2) =>
//       Let(
//         x,
//         t),
//         e1),
//         e2),
//       )
//     };
//   e');
// };
// let rec term_of_zterm = (z: zterm): term => {
//   switch (z) {
//   | Cursor(t) => t)
//   | XArrow(z, t1, t2) =>
//     Info(
//       None,
//       Arrow(
//         name_of_zname(z),
//         t1),
//         t2),
//       ),
//     )
//   | LArrow(x, z, t) =>
//     Arrow(x, term_of_zterm(z), t)))
//   | RArrow(x, t, z) =>
//     Arrow(x, t), term_of_zterm(z)))
//   | XFun(z, t, e) =>
//     Info(
//       None,
//       Fun(name_of_zname(z), t), e)),
//     )
//   | TFun(x, z, e) =>
//     Fun(x, term_of_zterm(z), e)))
//   | EFun(x, t, z) =>
//     Fun(x, t), term_of_zterm(z)))
//   | LAp(z, e) => Ap(term_of_zterm(z), e)))
//   | RAp(e, z) => Ap(e), term_of_zterm(z)))
//   | XLet(z, t, e1, e2) =>
//     Info(
//       None,
//       Let(
//         name_of_zname(z),
//         t),
//         e1),
//         e2),
//       ),
//     )
//   | TLet(x, z, e1, e2) =>
//     Info(
//       None,
//       Let(
//         x,
//         term_of_zterm(z),
//         e1),
//         e2),
//       ),
//     )
//   | E1Let(x, t, z, e2) =>
//     Info(
//       None,
//       Let(
//         x,
//         t),
//         term_of_zterm(z),
//         e2),
//       ),
//     )
//   | E2Let(x, t, e1, z) =>
//     Info(
//       None,
//       Let(
//         x,
//         t),
//         e1),
//         term_of_zterm(z),
//       ),
//     )
//   };
// };

// let rec term_at_cursor = (z: zterm) =>
//   switch (z) {
//   | Cursor(e) => Some(e)
//   | XArrow(_, _, _) => None
//   | LArrow(_, z, _)
//   | RArrow(_, _, z) => term_at_cursor(z)
//   | XFun(_, _, _) => None
//   | TFun(_, z, _)
//   | EFun(_, _, z)
//   | LAp(z, _)
//   | RAp(_, z) => term_at_cursor(z)
//   | XLet(_, _, _, _) => None
//   | TLet(_, z, _, _)
//   | E1Let(_, _, z, _)
//   | E2Let(_, _, _, z) => term_at_cursor(z)
//   };

let complete_name = (x: name) =>
  switch (x) {
  | Hole => false
  | Text(_) => true
  };

// let rec extend_complete_list_let = (x, t, e, c) =>
//   complete_term(t) && complete_term(e) ? extend_list_name(x, c) : c
// and complete_plain_term = (c: list(string), e: plain_term) =>
//   switch (e) {
//   | Hole
//   | Mark(_, _) => false
//   | Var(x) => List.mem(x, c)
//   | Typ => true
//   | Arrow(x, t1, t2) =>
//     complete_term(t1) && complete_term(t2)
//   | Fun(x, t, e) =>
//     complete_name(x)
//     && complete_term(t)
//     && complete_term(e)
//   | Ap(e1, e2) => complete_term(e1) && complete_term(e2)
//   | Let(x, t, e1, e2) =>
//     complete_term(e2)
//   }
// and complete_term = (e: term) =>
//   switch (e) {
//   | Info(i, e) => complete_plain_term(e)
//   };

// let extend_complete_list_fun = (x, t, c) =>
//   complete_term(t) ? extend_list_name(x, c) : c;

// let rec no_holes_term = (e: term) =>
//   switch (e) {
//   | Hole
//   | Mark(_, _) => false
//   | Var(_)
//   | Typ => true
//   | Arrow(_, t1, t2) => no_holes_term(t1) && no_holes_term(t2)
//   | Fun(x, t, e) =>
//     complete_name(x) && no_holes_term(t) && no_holes_term(e)
//   | Ap(e1, e2) => no_holes_term(e1) && no_holes_term(e2)
//   | Let(_, _, _, e2) => no_holes_term(e2)
//   };

// SECTION: CONSISTENCY, MATCHING, AND REDUCTION

let rec sub = (x: string, e1: term, e2: term): term => {
  switch (e2) {
  | Hole(_)
  | Typ(_) => e2
  | Var(r) => r.x == x ? e1 : e2
  | Mark(r) => Mark({...r, e: sub(x, e1, r.e)})
  | Ap(r) => Ap({...r, e1: sub(x, e1, r.e1), e2: sub(x, e1, r.e2)})
  | Arrow(r) =>
    r.x == Text(x)
      ? e2 : Arrow({...r, t1: sub(x, e1, r.t1), t2: sub(x, e1, r.t2)})
  | Fun(r) =>
    r.x == Text(x)
      ? e2 : Fun({...r, t: sub(x, e1, r.t), e: sub(x, e1, r.e)})
  | Let(r) =>
    r.x == Text(x)
      ? e2
      : Let({
          ...r,
          t: sub(x, e1, r.t),
          e1: sub(x, e1, r.e1),
          e2: sub(x, e1, r.e2),
        })
  };
};

let sub_name = (x: name, e1: term, e2: term): term => {
  switch (x) {
  | Hole => e2
  | Text(x) => sub(x, e1, e2)
  };
};

// Beta reduce until head is exposed, if possible
let rec head_reduce = (en: env, e: term): term => {
  switch (e) {
  | Hole(_)
  | Typ(_)
  | Arrow(_)
  | Fun(_)
  | Mark(_)
  | Let(_) => e
  | Var(r) =>
    switch (List.assoc_opt(r.x, en)) {
    | None => e
    | Some(e') => head_reduce(en, e')
    }
  | Ap(r) =>
    switch (head_reduce(en, r.e1)) {
    | Fun(r2) => head_reduce(en, sub_name(r2.x, r.e2, r2.e))
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
let rec head_consist = (en: env, t1, t2: term): bool => {
  switch (t1: term, t2: term) {
  | (Hole(_), _)
  | (_, Hole(_))
  | (Typ(_), Typ(_)) => true
  | (Mark(_), _)
  | (_, Mark(_)) => false
  | (Var(r1), Var(r2)) => r1.x == r2.x
  | (Arrow(r1), Arrow(r2)) =>
    consist_name(r1.x, r2.x)
    && consist(en, r1.t1, r2.t1)
    && consist(en, r1.t2, r2.t2)
  | (Fun(r1), Fun(r2)) =>
    consist_name(r1.x, r1.x)
    && consist(en, r1.t, r2.t)
    && consist(en, r1.e, r2.e)
  | (Ap(r1), Ap(r2)) =>
    consist(en, r1.e1, r2.e1) && consist(en, r1.e2, r2.e2)
  | (Let(r1), Let(r2)) =>
    consist_name(r1.x, r2.x)
    && consist(en, r1.t, r2.t)
    && consist(en, r1.e1, r2.e1)
    && consist(en, r1.e2, r2.e2)
  | _ => false
  };
}
and consist = (en: env, t1, t2: term): bool => {
  let (t1', t2') = (head_reduce(en, t1), head_reduce(en, t2));
  head_consist(en, t1', t2');
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

type contexts = {
  c: context,
  // completes: list(term),
  en: env,
};

let store_contexts = (cs: contexts, i: info): info => {
  {
    ...i,
    c: cs.c,
    // completes:  cs.completes,
    en: cs.en,
  };
};

// Synthetic static judgement. returns new_expression
// Info is filled out, and marks are inserted
// Existing info is not necessarily paid attention to
let rec syn = (cs: contexts, e: term): term => {
  let i = store_contexts(cs, get_info(e));
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
    switch (lookup(r.x, cs.c)) {
    | None =>
      let i = {...i, syn: Some(default_hole)};
      Mark({i, m: UnknownVar(r.x), e: Var({...r, i})});
    | Some(t) =>
      let i = {...i, syn: Some(t)};
      Var({...r, i});
    }
  | Arrow(r) =>
    let t1' = syn(cs, r.t1);
    let t1t = get_info(t1').syn;
    let (cs, t1'': term) =
      switch (Option.bind(t1t, typ_of_term)) {
      | None =>
        let i = {...i, syn: Some(default_hole)};
        (cs, Mark({i, m: NotTyp(t1t), e: t1'}));
      | Some(_) => ({...cs, c: extend_context_name(r.x, t1', cs.c)}, t1')
      };
    // Missing: Update completes
    let t2' = syn(cs, r.t2);
    let t2t = get_info(t2').syn;
    let t2'': term =
      switch (Option.bind(t2t, typ_of_term)) {
      | None =>
        let i = {...i, syn: Some(default_hole)};
        Mark({i, m: NotTyp(t2t), e: t2'});
      | Some(_) => t2'
      };
    let i = {...i, syn: Some(Typ({i: default_info}))};
    Arrow({...r, i, t1: t1'', t2: t2''});
  | Fun(r) =>
    let t = syn(cs, r.t);
    let tt = get_info(t).syn;
    let (cs, t: term) =
      switch (Option.bind(tt, typ_of_term)) {
      | None =>
        let i = {...i, syn: Some(default_hole)};
        (cs, Mark({i, m: NotTyp(tt), e: t}));
      | Some(_) => ({...cs, c: extend_context_name(r.x, t, cs.c)}, t)
      };
    // Missing: Update completes
    let e = syn(cs, r.e);
    let et = get_info(e).syn;
    let syn =
      switch (et) {
      | None => None
      | Some(et) => Some(Arrow({i: default_info, x: r.x, t1: t, t2: et}))
      };
    let i = {...i, syn};
    Fun({...r, i, t, e});
  | Ap(r) =>
    let e1 = syn(cs, r.e1);
    let t1 = get_info(e1).syn;
    switch (Option.bind(t1, arrow_of_term)) {
    | None =>
      let i = {...i, syn: Some(default_hole)};
      let e1 = Mark({i, m: FunNotArrow(t1), e: e1});
      let e2 = ana(cs, default_hole, r.e2);
      Ap({i, e1, e2});
    | Some(Arrow(r1)) =>
      let e2 = ana(cs, r1.t1, r.e2);
      let syn = Some(sub_name(r1.x, e2, r1.t2));
      let i = {...i, syn};
      Ap({i, e1, e2});
    | Some(_) => failwith("impossible")
    };
  | Let(r) =>
    let t = syn(cs, r.t);
    let tt = get_info(t).syn;
    let (cs, t: term, ana1) =
      switch (Option.bind(tt, typ_of_term)) {
      | None =>
        let i = {...i, syn: Some(default_hole)};
        (cs, Mark({i, m: NotTyp(tt), e: t}), default_hole);
      | Some(_) => ({...cs, c: extend_context_name(r.x, t, cs.c)}, t, t)
      };
    let e1 = ana(cs, ana1, r.e1);
    let cs = {...cs, en: maybe_extend_env_name(r.x, e1, cs.en)};
    // Missing: Update completes
    let e2 = syn(cs, r.e2);
    let syn = get_info(e2).syn;
    let i = {...i, syn};
    Let({...r, i, t, e1, e2});
  };
}
// Analytic static judgement. returns new_expression
and ana = (cs: contexts, ana_t: term, e: term): term => {
  let i = store_contexts(cs, get_info(e));
  let i = {...i, goal: Some(ana_t)};
  let subsume = () => {
    let e = syn(cs, e);
    let t = Option.get(get_info(e).syn);
    if (consist(cs.en, ana_t, t)) {
      e;
    } else {
      let i = {...i, syn: Some(default_hole)};
      let m = Mismatch(ana_t, t);
      Mark({i, m, e});
    };
  };
  switch (e) {
  | Fun(r1) =>
    switch (arrow_of_term(ana_t)) {
    | Some(Arrow(r2)) =>
      if (consist_name(r1.x, r2.x) && consist(cs.en, r1.t, r2.t1)) {
        let t = syn(cs, r1.t);
        let cs = {...cs, c: extend_context_name(r1.x, r1.t, cs.c)};
        // Missing: Update completes
        let e = ana(cs, r2.t2, r1.e);
        Fun({...r1, i, t, e});
      } else {
        subsume();
      }
    | _ => subsume()
    }
  | Let(r) =>
    let t = syn(cs, r.t);
    let tt = get_info(t).syn;
    let (cs, t: term, ana1) =
      switch (Option.bind(tt, typ_of_term)) {
      | None =>
        let i = {...i, syn: Some(default_hole)};
        (cs, Mark({i, m: NotTyp(tt), e: t}), default_hole);
      | Some(_) => ({...cs, c: extend_context_name(r.x, t, cs.c)}, t, t)
      };
    let e1 = ana(cs, ana1, r.e1);
    let cs = {...cs, en: maybe_extend_env_name(r.x, e1, cs.en)};
    // Missing: Update completes
    let e2 = ana(cs, ana_t, r.e2);
    Let({...r, i, t, e1, e2});
  | _ => subsume()
  };
};

// // Precondition: z and e are the same except z has a cursor and e has marks.
// // Returns the expression with both the cursor and the marks.
// let rec mark_merge = (z: zterm, e: term): zterm =>
//   switch (z, e) {
//   | (Cursor(_), e') => Cursor(e')
//   | (z, Mark(m, e)) => Mark(m, mark_merge(z, e))
//   | (XArrow(z, _, _), Arrow(_, t1, t2)) => XArrow(z, t1, t2)
//   | (LArrow(_, z, _), Arrow(x, t1, t2)) =>
//     LArrow(x, mark_merge(z, t1), t2)
//   | (RArrow(_, _, z), Arrow(x, t1, t2)) =>
//     RArrow(x, t1, mark_merge(z, t2))
//   | (XFun(z, _, _), Fun(_, t, e)) => XFun(z, t, e)
//   | (TFun(_, z, _), Fun(x, t, e)) => TFun(x, mark_merge(z, t), e)
//   | (EFun(_, _, z), Fun(x, t, e)) => EFun(x, t, mark_merge(z, e))
//   | (LAp(z, _), Ap(e1, e2)) => LAp(mark_merge(z, e1), e2)
//   | (RAp(_, z), Ap(e1, e2)) => RAp(e1, mark_merge(z, e2))
//   | (XLet(z, _, _, _), Let(_, t, e1, e2)) => XLet(z, t, e1, e2)
//   | (TLet(_, z, _, _), Let(x, t, e1, e2)) =>
//     TLet(x, mark_merge(z, t), e1, e2)
//   | (E1Let(_, _, z, _), Let(x, t, e1, e2)) =>
//     E1Let(x, t, mark_merge(z, e1), e2)
//   | (E2Let(_, _, _, z), Let(x, t, e1, e2)) =>
//     E2Let(x, t, e1, mark_merge(z, e2))
//   | _ => failwith("merge mismatch")
//   };
// // Returns the complete context (list of vars that are completely defined)
// // at the cursor (appended to the argument c)
// let rec local_complete_list = (c: list(string), z: zterm) =>
//   switch (z) {
//   | Cursor(_) => c
//   | Mark(_, z) => local_complete_list(c, z)
//   | XArrow(_, _, _) => c
//   | LArrow(_, z, _) => local_complete_list(c, z)
//   | RArrow(x, t, z) =>
//     let c' = extend_complete_list_fun(x, t, c);
//     local_complete_list(c', z);
//   | XFun(_, _, _) => c
//   | TFun(_, z, _) => local_complete_list(c, z)
//   | EFun(x, t, z) =>
//     let c' = extend_complete_list_fun(x, t, c);
//     local_complete_list(c', z);
//   | LAp(z, _)
//   | RAp(_, z) => local_complete_list(c, z)
//   | XLet(_, _, _, _) => c
//   | TLet(_, z, _, _) => local_complete_list(c, z)
//   | E1Let(_, _, z, _) => local_complete_list(c, z)
//   | E2Let(x, t, e, z) =>
//     let c' = extend_complete_list_let(x, t, e, c);
//     local_complete_list(c', z);
//   };
// // Whether a let should get a green box next to it
// let check_let = (c, en, t, completes, e1) =>
//   switch (syn(c, en, e1)) {
//   | (_, t') =>
//     complete_term(completes, t)
//     && complete_term(completes, e1)
//     && consist(en, t, t')
//   };
// // Returns the context at the cursor (appended to the argument c)
// let rec local_context = (c: context, z: zterm) =>
//   switch (z) {
//   | Cursor(_) => c
//   | Mark(_, z) => local_context(c, z)
//   | XArrow(_, _, _) => c
//   | LArrow(_, z, _) => local_context(c, z)
//   | RArrow(x, t1, z) =>
//     let c' = extend_context_name(x, t1, c);
//     local_context(c', z);
//   | XFun(_, _, _) => c
//   | TFun(_, z, _) => local_context(c, z)
//   | EFun(x, t, z) =>
//     let c' = extend_context_name(x, t, c);
//     local_context(c', z);
//   | LAp(z, _)
//   | RAp(_, z) => local_context(c, z)
//   | XLet(_, _, _, _) => c
//   | TLet(_, z, _, _) => local_context(c, z)
//   | E1Let(_, _, z, _) => local_context(c, z)
//   | E2Let(x, t, _, z) =>
//     let c' = extend_context_name(x, t, c);
//     local_context(c', z);
//   };
// let rec local_env = (en: env, z: zterm) =>
//   switch (z) {
//   | Cursor(_) => en
//   | Mark(_, z) => local_env(en, z)
//   | XArrow(_, _, _) => en
//   | LArrow(_, z, _)
//   | RArrow(_, _, z) => local_env(en, z)
//   | XFun(_, _, _) => en
//   | TFun(_, z, _)
//   | EFun(_, _, z) => local_env(en, z)
//   | LAp(z, _)
//   | RAp(_, z) => local_env(en, z)
//   | XLet(_, _, _, _) => en
//   | TLet(_, z, _, _)
//   | E1Let(_, _, z, _) => local_env(en, z)
//   | E2Let(x, _, e, z) =>
//     let en' = maybe_extend_env_name(x, e, en);
//     local_env(en', z);
//   };
// // Returns the expected type at the cursor, if the argument's expected type is g
// // Keeps the goal reduced
// let rec local_goal = (c: context, en: env, g: term, z: zterm) => {
//   switch (z) {
//   | Cursor(_) => g
//   | Mark(_, z) => local_goal(c, en, g, z)
//   | XArrow(_, _, _) => g
//   | LArrow(_, _, _) => Typ
//   | RArrow(x, t1, z) =>
//     let c' = extend_context_name(x, t1, c);
//     local_goal(c', en, g, z);
//   | XFun(_, _, _) => g
//   | TFun(_, _, _) => Typ
//   | EFun(x, t, z) =>
//     let c' = extend_context_name(x, t, c);
//     let g' =
//       switch (arrow_of_term(g)) {
//       | Some((y, _, t')) when consist_name(x, y) => reduce(en, t')
//       | _ => Hole
//       };
//     local_goal(c', en, g', z);
//   | LAp(z, e) =>
//     let (_, g') = syn(c, en, e);
//     let g' = reduce(en, g');
//     local_goal(c, en, Arrow(Hole, g', g), z);
//   | RAp(e, z) =>
//     let g': term =
//       switch (syn(c, en, e)) {
//       | (_, Arrow(_, g', _)) => reduce(en, g')
//       | _ => Hole
//       };
//     local_goal(c, en, g', z);
//   | XLet(_, _, _, _) => g
//   | TLet(_, _, _, _) => Typ
//   | E1Let(_, t, z, _) => local_goal(c, en, t, z)
//   | E2Let(x, t, e, z) =>
//     let c' = extend_context_name(x, t, c);
//     let en' = maybe_extend_env_name(x, e, en);
//     local_goal(c', en', g, z);
//   };
// };
// // Returns the marks at the cursor
// let rec local_marks = (z: zterm): list(mark) =>
//   switch (z) {
//   | Cursor(Mark(m, e)) => [m, ...local_marks(Cursor(e))]
//   | Cursor(_) => []
//   | Mark(_, z) => local_marks(z)
//   | XArrow(_, _, _) => []
//   | LArrow(_, z, _)
//   | RArrow(_, _, z) => local_marks(z)
//   | XFun(_, _, _) => []
//   | TFun(_, z, _)
//   | EFun(_, _, z) => local_marks(z)
//   | LAp(z, _)
//   | RAp(_, z) => local_marks(z)
//   | XLet(_, _, _, _) => []
//   | TLet(_, z, _, _)
//   | E1Let(_, _, z, _)
//   | E2Let(_, _, _, z) => local_marks(z)
//   };
