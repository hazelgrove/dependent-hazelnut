open String;

open Terms;
open Lang;

open Find_holes;

let apply_at_cursor_zname = (z: zname, f: name => zname): zname =>
  switch (z) {
  | Cursor(x) => f(x)
  };

let rec apply_at_cursor_zterm = (z: zterm, f: pterm => zterm) =>
  switch (z) {
  | Cursor(e) => f(e)
  | XArrow(z, t1, t2) => XArrow(z, t1, t2)
  | LArrow(x, z, t2) => LArrow(x, apply_at_cursor_zterm(z, f), t2)
  | RArrow(x, t1, z) => RArrow(x, t1, apply_at_cursor_zterm(z, f))
  | XFun(z, t, e) => XFun(z, t, e)
  | TFun(x, z, e) => TFun(x, z, e)
  | EFun(x, t, z) => EFun(x, t, apply_at_cursor_zterm(z, f))
  | LAp(z, e) => LAp(apply_at_cursor_zterm(z, f), e)
  | RAp(e, z) => RAp(e, apply_at_cursor_zterm(z, f))
  | XLet(z, t, e1, e2) => XLet(z, t, e1, e2)
  | TLet(x, z, e1, e2) => TLet(x, z, e1, e2)
  | E1Let(x, t, z, e2) => E1Let(x, t, apply_at_cursor_zterm(z, f), e2)
  | E2Let(x, t, e1, z) => E2Let(x, t, e1, apply_at_cursor_zterm(z, f))
  };

let focus_hole = (z: zterm) => {
  let f = e =>
    switch (find_hole_term(e)) {
    | Some(z') => z'
    | None => Cursor(e)
    };
  apply_at_cursor_zterm(z, f);
};

let give_term = (z: zterm, e: pterm) => {
  let f = (e': pterm) =>
    switch (e') {
    | Hole => Cursor(e)
    | e' => Cursor(e')
    };
  apply_at_cursor_zterm(z, f);
};

let var_for_term = (ctx: context) => {
  let rec next_fnum = (acc: int, ctx: context) =>
    switch (ctx) {
    | [] => acc
    | [{x: Hole, _}, ...ctx'] => next_fnum(acc, ctx')
    | [{x: Text(x), _}, ...ctx'] =>
      if (starts_with(x, ~prefix="f")) {
        let substring: string = String.sub(x, 1, String.length(x) - 1);
        switch (int_of_string(substring)) {
        | acc' => next_fnum(Int.max(acc, acc'), ctx')
        | exception _ => next_fnum(acc, ctx')
        };
      } else {
        next_fnum(acc, ctx');
      }
    };
  "f" ++ string_of_int(next_fnum(0, ctx) + 1);
};

// type directed refinement - if the goal is an arrow, instantiates the right fun
let refine = (z: zterm, ec: term) => {
  switch (get_info(ec).goal) {
  | Some(Arrow(r)) =>
    let var_name =
      switch (r.x) {
      | Hole => var_for_term(get_info(ec).ctx)
      | Text(x) => x
      };
    focus_hole(
      give_term(z, Fun(Text(var_name), pterm_of_term(r.t1), Hole)),
    );
  | _ => z
  };
};

let var_for_lemma = (c: context) => {
  let rec next_hnum = (acc: int, c: context) =>
    switch (c) {
    | [] => acc
    | [{x: Hole, _}, ...c'] => next_hnum(acc, c')
    | [{x: Text(x), _}, ...c'] =>
      if (starts_with(x, ~prefix="h")) {
        let substring: string = String.sub(x, 1, String.length(x) - 1);
        switch (int_of_string(substring)) {
        | acc' => next_hnum(Int.max(acc, acc'), c')
        | exception _ => next_hnum(acc, c')
        };
      } else {
        next_hnum(acc, c');
      }
    };
  "h" ++ string_of_int(next_hnum(0, c) + 1);
};
// not actually sure what the reasoning is behind this
let rec insert_lemma = (z: zterm, s: string, g: pterm) =>
  switch (z) {
  | Cursor(_)
  | LAp(_, _)
  | RAp(_, _)
  | XArrow(_, _, _)
  | LArrow(_, _, _)
  | RArrow(_, _, _)
  | XFun(_, _, _)
  | TFun(_, _, _)
  | XLet(_, _, _, _)
  | TLet(_, _, _, _) => Cursor(Let(Text(s), g, Hole, pterm_of_zterm(z)))
  | EFun(x, t, z) => EFun(x, t, insert_lemma(z, s, g))
  | E1Let(x, t, z, e) => E1Let(x, t, insert_lemma(z, s, g), e)
  | E2Let(x, t, e, z) => E2Let(x, t, e, insert_lemma(z, s, g))
  };

// let make_lemma = (z: zterm) => {
//   let lemma_name = var_for_lemma(local_context([], z));
//   let z = give_term(z, Var(lemma_name));
//   let z = insert_lemma(z, lemma_name, local_goal([], [], Hole, z));
//   focus_hole(z);
// };

// let rec term_endswith = (arg_acc: int, t1, t2) =>
//   if (t1 == t2) {
//     Some(arg_acc);
//   } else {
//     switch (t1) {
//     | Arrow(_, _, t1) => term_endswith(arg_acc + 1, t1, t2)
//     | _ => None
//     };
//   };

// // Find an implication in context that can produce the (complete) goal if
// // provided enough arguments, and instantiates it with the right number of args.
// // Prefers those with the fewest arguments.
// let suggest_ap = (z: zterm) => {
//   // let z = focus_hole(z);
//   let g = local_goal([], [], Hole, z);
//   if (complete_term([], g)) {
//     let c = local_context([], z);
//     let f = ((x, t)) => {
//       switch (term_endswith(0, t, g)) {
//       | None => None
//       | Some(n) => Some((x, t, n))
//       };
//     };
//     let c = List.filter_map(f, c);
//     let preference = ((_, _, a), (_, _, b)) => a - b;
//     let rec n_aps = (x, n) =>
//       if (n == 0) {
//         Var(x);
//       } else {
//         Ap(n_aps(x, n - 1), Hole);
//       };
//     switch (List.sort(preference, c)) {
//     | [] => z
//     | [(x, _, n), ..._] => give_term(z, n_aps(x, n))
//     };
//   } else {
//     z;
//   };
// };

// // Fill hole with a variable from the context of matching complete terme
// let fill_var = (z: zterm) => {
//   let g = local_goal([], [], Hole, z);
//   if (no_holes_term(g)) {
//     let c = local_context([], z);
//     let good_var = ((_, t)) => g == t;
//     switch (List.filter(good_var, c)) {
//     | [] => z
//     | [(x, _), ..._] => give_term(z, Var(x))
//     };
//   } else {
//     z;
//   };
// };

// let rec refinable_position = z =>
//   switch (z) {
//   | XArrow(_, _, _)
//   | LArrow(_, _, _)
//   | RArrow(_, _, _)
//   | LAp(Cursor(_), _)
//   | RAp(_, Cursor(_)) => false
//   | Cursor(_)
//   | XFun(_, _, _)
//   | TFun(_, _, _)
//   | XLet(_, _, _, _)
//   | TLet(_, _, _, _) => true
//   | Mark(_, z)
//   | EFun(_, _, z)
//   | LAp(z, _)
//   | RAp(_, z)
//   | E1Let(_, _, z, _)
//   | E2Let(_, _, _, z) => refinable_position(z)
//   };

// let rec chain_tactics = (z: zterm, ts: list(zterm => zterm)) =>
//   switch (ts) {
//   | [] => z
//   | [t, ...ts] =>
//     let z' = t(z);
//     if (z == z') {
//       chain_tactics(z, ts);
//     } else {
//       z';
//     };
//   };

// let auto = (~seek=true, z: zterm) => {
//   let smart_refine = z => {
//     switch (local_goal([], [], Hole, z)) {
//     | Arrow(_, _, _) when refinable_position(z) => refine(z)
//     | _ => z
//     };
//   };
//   let smart_make_lemma = z => {
//     switch (local_goal([], [], Hole, z)) {
//     | Arrow(x, t1, t2)
//         when
//           complete_term([], Arrow(x, t1, t2))
//           && term_at_cursor(z) == Some(Hole) =>
//       make_lemma(z)
//     | _ => z
//     };
//   };
//   let find_hole = z =>
//     switch (find_hole_zterm(~loop=true, z)) {
//     | None => z
//     | Some(z') => z'
//     };
//   chain_tactics(
//     z,
//     [focus_hole, fill_var, smart_refine, suggest_ap, smart_make_lemma]
//     @ (seek ? [find_hole] : []),
//   );
// };

// let set_cursor_to_bounds_name = (bounds: zname, z: zname): zname =>
//   switch (bounds, z) {
//   | (Cursor(_), z) => Cursor(name_of_zname(z))
//   };
// let rec set_cursor_to_bounds_term = (bounds, z) =>
//   switch (bounds, z) {
//   | (Cursor(_), z) => Cursor(term_of_zterm(z))
//   | (XArrow(z, t1, t2), XArrow(z', t1', t2')) when t1 == t1' && t2 == t2' =>
//     XArrow(set_cursor_to_bounds_name(z, z'), t1, t2)
//   | (LArrow(x, z, t), LArrow(x', z', t')) when x == x' && t == t' =>
//     LArrow(x, set_cursor_to_bounds_term(z, z'), t)
//   | (RArrow(x, t, z), RArrow(x', t', z')) when x == x' && t == t' =>
//     RArrow(x, t, set_cursor_to_bounds_term(z, z'))
//   | (XFun(z, t, e), XFun(z', t', e')) when t == t' && e == e' =>
//     XFun(set_cursor_to_bounds_name(z, z'), t, e)
//   | (TFun(x, z, e), TFun(x', z', e')) when x == x' && e == e' =>
//     TFun(x, set_cursor_to_bounds_term(z, z'), e)
//   | (EFun(x, t, z), EFun(x', t', z')) when x == x' && t == t' =>
//     EFun(x, t, set_cursor_to_bounds_term(z, z'))
//   | (LAp(z, e), LAp(z', e')) when e == e' =>
//     LAp(set_cursor_to_bounds_term(z, z'), e)
//   | (RAp(e, z), RAp(e', z')) when e == e' =>
//     RAp(e, set_cursor_to_bounds_term(z, z'))
//   | (XLet(z, t, e1, e2), XLet(z', t', e1', e2'))
//       when t == t' && e1 == e1' && e2 == e2' =>
//     XLet(set_cursor_to_bounds_name(z, z'), t, e1, e2)
//   | (TLet(x, z, e1, e2), TLet(x', z', e1', e2'))
//       when x == x' && e1 == e1' && e2 == e2' =>
//     TLet(x, set_cursor_to_bounds_term(z, z'), e1, e2)
//   | (E1Let(x, t, z, e2), E1Let(x', t', z', e2'))
//       when x == x' && t == t' && e2 == e2' =>
//     E1Let(x, t, set_cursor_to_bounds_term(z, z'), e2)
//   | (E2Let(x, t, e1, z), E2Let(x', t', e1', z'))
//       when x == x' && t == t' && e1 == e1' =>
//     E2Let(x, t, e1, set_cursor_to_bounds_term(z, z'))
//   | _ => failwith("bounds mismatch")
//   };

// let rec full_auto_helper = (n, bounds: zterm, z: zterm) => {
//   let z = set_cursor_to_bounds_term(bounds, z);
//   if (n == 0) {
//     z;
//   } else {
//     full_auto_helper(n - 1, z, auto(~seek=false, focus_hole(z)));
//   };
// };
// let full_auto = z => full_auto_helper(40, z, z);
