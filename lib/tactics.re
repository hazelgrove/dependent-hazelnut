open String;

open Terms;
open Lang;

open Find_holes;

let apply_at_cursor_zname = (z: zname, f: name => zname): zname =>
  switch (z) {
  | Cursor(x) => f(x)
  };

let rec apply_at_cursor_ztyp = (z: ztyp, f: typ => ztyp): ztyp =>
  switch (z) {
  | Cursor(t) => f(t)
  | LArrow(z, t) => LArrow(apply_at_cursor_ztyp(z, f), t)
  | RArrow(t, z) => RArrow(t, apply_at_cursor_ztyp(z, f))
  };

let rec apply_at_cursor_zexp = (z: zexp, f: exp => zexp) =>
  switch (z) {
  | Cursor(e) => f(e)
  | Mark(m, z) => Mark(m, apply_at_cursor_zexp(z, f))
  | XFun(z, t, e) => XFun(z, t, e)
  | TFun(x, z, e) => TFun(x, z, e)
  | EFun(x, t, z) => EFun(x, t, apply_at_cursor_zexp(z, f))
  | LAp(z, e) => LAp(apply_at_cursor_zexp(z, f), e)
  | RAp(e, z) => RAp(e, apply_at_cursor_zexp(z, f))
  | XLet(z, t, e1, e2) => XLet(z, t, e1, e2)
  | TLet(x, z, e1, e2) => TLet(x, z, e1, e2)
  | E1Let(x, t, z, e2) => E1Let(x, t, apply_at_cursor_zexp(z, f), e2)
  | E2Let(x, t, e1, z) => E2Let(x, t, e1, apply_at_cursor_zexp(z, f))
  };

let focus_hole = (z: zexp) => {
  let f = e =>
    switch (find_hole_exp(e)) {
    | Some(z') => z'
    | None => Cursor(e)
    };
  apply_at_cursor_zexp(z, f);
};

let give_exp = (z: zexp, e: exp) => {
  let f = e' =>
    switch (e') {
    | Hole => Cursor(e)
    | e' => Cursor(e')
    };
  apply_at_cursor_zexp(z, f);
};

let var_for_typ = (t: typ, c: context) => {
  let rec next_fnum = (acc: int, c: context) =>
    switch (c) {
    | [] => acc
    | [(x, _), ...c'] =>
      if (starts_with(x, ~prefix="f")) {
        let substring: string = String.sub(x, 1, String.length(x) - 1);
        switch (int_of_string(substring)) {
        | acc' => next_fnum(Int.max(acc, acc'), c')
        | exception _ => next_fnum(acc, c')
        };
      } else {
        next_fnum(acc, c');
      }
    };
  switch (t) {
  | Base(x) => "h-" ++ x
  | _ => "f" ++ string_of_int(next_fnum(0, c) + 1)
  };
};

// Type directed refinement - if the goal is an arrow, instantiates the right fun
let refine = (z: zexp) => {
  switch (local_goal([], Hole, z)) {
  | Arrow(t, _) =>
    focus_hole(
      give_exp(
        z,
        Fun(Text(var_for_typ(t, local_context([], z))), t, Hole),
      ),
    )
  | _ => z
  };
};

let rec typ_endswith = (arg_acc: int, t1, t2) =>
  if (t1 == t2) {
    Some(arg_acc);
  } else {
    switch (t1) {
    | Arrow(_, t1) => typ_endswith(arg_acc + 1, t1, t2)
    | _ => None
    };
  };

// Find an implication in context that can produce the (complete) goal if
// provided enough arguments, and instantiates it with the right number of args.
// Prefers those with the fewest arguments.
let suggest_ap = (z: zexp) => {
  // let z = focus_hole(z);
  let g = local_goal([], Hole, z);
  if (complete_typ(g)) {
    let c = local_context([], z);
    let f = ((x, t)) => {
      switch (typ_endswith(0, t, g)) {
      | None => None
      | Some(n) => Some((x, t, n))
      };
    };
    let c = List.filter_map(f, c);
    let preference = ((_, _, a), (_, _, b)) => a - b;
    let rec n_aps = (x, n) =>
      if (n == 0) {
        Var(x);
      } else {
        Ap(n_aps(x, n - 1), Hole);
      };
    switch (List.sort(preference, c)) {
    | [] => z
    | [(x, _, n), ..._] => give_exp(z, n_aps(x, n))
    };
  } else {
    z;
  };
};

// Fill hole with a variable from the context of matching complete type
let fill_var = (z: zexp) => {
  let g = local_goal([], Hole, z);
  if (complete_typ(g)) {
    let c = local_context([], z);
    let good_var = ((_, t)) => g == t;
    switch (List.filter(good_var, c)) {
    | [] => z
    | [(x, _), ..._] => give_exp(z, Var(x))
    };
  } else {
    z;
  };
};

let rec refinable_position = z =>
  switch (z) {
  | LAp(Cursor(_), _)
  | RAp(_, Cursor(_)) => false
  | Cursor(_)
  | XFun(_, _, _)
  | TFun(_, _, _)
  | XLet(_, _, _, _)
  | TLet(_, _, _, _) => true
  | Mark(_, z)
  | EFun(_, _, z)
  | LAp(z, _)
  | RAp(_, z)
  | E1Let(_, _, z, _)
  | E2Let(_, _, _, z) => refinable_position(z)
  };

let auto = (z: zexp) => {
  let z = fill_var(z); // First try solving directly by assumption
  switch (local_goal([], Hole, z)) {
  | Arrow(_, _) when refinable_position(z) => refine(z) // Then refine, if in a good spot to
  | _ => suggest_ap(focus_hole(z)) // Otherwise, try using an implication in the context
  };
};

let set_cursor_to_bounds_name = (bounds: zname, z: zname): zname =>
  switch (bounds, z) {
  | (Cursor(_), z) => Cursor(name_of_zname(z))
  };
let rec set_cursor_to_bounds_typ = (bounds: ztyp, z: ztyp): ztyp =>
  switch (bounds, z) {
  | (Cursor(_), z) => Cursor(typ_of_ztyp(z))
  | (LArrow(z, t), LArrow(z', t')) when t == t' =>
    LArrow(set_cursor_to_bounds_typ(z, z'), t)
  | (RArrow(t, z), RArrow(t', z')) when t == t' =>
    RArrow(t, set_cursor_to_bounds_typ(z, z'))
  | _ => failwith("bounds mismatch")
  };

let rec set_cursor_to_bounds_exp = (bounds, z) =>
  switch (bounds, z) {
  | (Cursor(_), z) => Cursor(exp_of_zexp(z))
  | (XFun(z, t, e), XFun(z', t', e')) when t == t' && e == e' =>
    XFun(set_cursor_to_bounds_name(z, z'), t, e)
  | (TFun(x, z, e), TFun(x', z', e')) when x == x' && e == e' =>
    TFun(x, set_cursor_to_bounds_typ(z, z'), e)
  | (EFun(x, t, z), EFun(x', t', z')) when x == x' && t == t' =>
    EFun(x, t, set_cursor_to_bounds_exp(z, z'))
  | (LAp(z, e), LAp(z', e')) when e == e' =>
    LAp(set_cursor_to_bounds_exp(z, z'), e)
  | (RAp(e, z), RAp(e', z')) when e == e' =>
    RAp(e, set_cursor_to_bounds_exp(z, z'))
  | (XLet(z, t, e1, e2), XLet(z', t', e1', e2'))
      when t == t' && e1 == e1' && e2 == e2' =>
    XLet(set_cursor_to_bounds_name(z, z'), t, e1, e2)
  | (TLet(x, z, e1, e2), TLet(x', z', e1', e2'))
      when x == x' && e1 == e1' && e2 == e2' =>
    TLet(x, set_cursor_to_bounds_typ(z, z'), e1, e2)
  | (E1Let(x, t, z, e2), E1Let(x', t', z', e2'))
      when x == x' && t == t' && e2 == e2' =>
    E1Let(x, t, set_cursor_to_bounds_exp(z, z'), e2)
  | (E2Let(x, t, e1, z), E2Let(x', t', e1', z'))
      when x == x' && t == t' && e1 == e1' =>
    E2Let(x, t, e1, set_cursor_to_bounds_exp(z, z'))
  | _ => failwith("bounds mismatch")
  };

let rec full_auto_helper = (n, bounds: zexp, z: zexp) => {
  let z = set_cursor_to_bounds_exp(bounds, z);
  if (n == 0) {
    z;
  } else {
    full_auto_helper(n - 1, bounds, auto(focus_hole(z)));
  };
};
let full_auto = z => full_auto_helper(40, z, z);
