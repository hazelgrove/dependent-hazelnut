open Core;
open Terms;
// open Lang;

open Find_holes;
open Movement;
open Tactics;

type state = zexp;

type edit_action =
  | Delete
  | GoUp
  | GoDown
  | GoLeft
  | GoRight
  | NextHole
  | FocusHole
  | AddString(string)
  | Backspace
  | MakeFun
  | MakeAp
  | MakeLet
  | MakeArrow
  | TextAction
  | GiveExp(exp)
  | FillVar
  | Refine
  | MakeLemma
  | Auto
  | FullAuto;

let initial_state: zexp =
  E1Let(
    Text("thm1"),
    Arrow(
      Arrow(Base("a"), Arrow(Base("b"), Base("c"))),
      Arrow(Base("b"), Arrow(Base("a"), Base("c"))),
    ),
    Cursor(Hole),
    Let(
      Text("thm2"),
      Arrow(
        Arrow(Base("a"), Arrow(Base("b"), Base("c"))),
        Arrow(Arrow(Base("a"), Base("b")), Arrow(Base("a"), Base("c"))),
      ),
      Hole,
      Let(
        Text("thm3"),
        Arrow(Arrow(Arrow(Base("a"), Base("a")), Base("b")), Base("b")),
        Hole,
        Hole,
      ),
    ),
  );

let backspace = (s: string) =>
  String.sub(s, ~pos=0, ~len=String.length(s) - 1);

let edit_action_of_text = (s: string) =>
  switch (s) {
  | "fun" => Some(MakeFun)
  | "let" => Some(MakeLet)
  // | "auto" => Some(Auto)
  | _ => None
  // | _ => None
  };

let rec apply_zname = (a: edit_action, z: zname): zname => {
  switch (a, z) {
  | (Delete, Cursor(Text(_))) => Cursor(Hole)
  | (AddString(s), Cursor(Hole)) => Cursor(Text(s))
  | (AddString(s), Cursor(Text(x))) => Cursor(Text(x ++ s))
  | (Backspace, Cursor(Text(x))) when String.length(x) == 1 => Cursor(Hole)
  | (Backspace, Cursor(Text(x))) => Cursor(Text(backspace(x)))
  | (TextAction, Cursor(Hole)) =>
    apply_zname(TextAction, Cursor(Text("")))
  | (TextAction, Cursor(Text(x))) =>
    switch (edit_action_of_text(x)) {
    | Some(a') => apply_zname(a', Cursor(Hole))
    | None => z
    }
  | _ => z
  };
};
let rec apply_ztyp = (a: edit_action, z: ztyp): ztyp => {
  switch (a, z) {
  | (Delete, Cursor(_)) => Cursor(Hole)
  | (GoUp, z) => move_ztyp(Up, z)
  | (GoDown, z) => move_ztyp(Down, z)
  | (GoLeft, z) => move_ztyp(Left, z)
  | (GoRight, z) => move_ztyp(Right, z)
  | (AddString(s), Cursor(Hole)) => Cursor(Base(s))
  | (AddString(s), Cursor(Base(x))) => Cursor(Base(x ++ s))
  | (Backspace, Cursor(Base(x))) when String.length(x) == 1 => Cursor(Hole)
  | (Backspace, Cursor(Base(x))) => Cursor(Base(backspace(x)))
  | (Backspace, Cursor(_)) => Cursor(Hole)
  | (MakeArrow, Cursor(t)) => RArrow(t, Cursor(Hole))
  | (TextAction, Cursor(Hole)) => apply_ztyp(TextAction, Cursor(Base("")))
  | (TextAction, Cursor(Base(x))) =>
    switch (edit_action_of_text(x)) {
    | Some(a') => apply_ztyp(a', Cursor(Hole))
    | None => z
    }
  | (a, LArrow(z, t)) => LArrow(apply_ztyp(a, z), t)
  | (a, RArrow(t, z)) => RArrow(t, apply_ztyp(a, z))
  | _ => z
  };
};
let rec apply_zexp = (a: edit_action, z: zexp): zexp => {
  switch (a, z) {
  | (Delete, Cursor(_)) => Cursor(Hole)
  | (NextHole, z) =>
    switch (find_hole_zexp(~loop=true, z)) {
    | Some(z') => z'
    | None => z
    }
  | (FocusHole, Cursor(e)) =>
    switch (find_hole_exp(e)) {
    | Some(z') => z'
    | None => z
    }
  | (GoUp, z) => move_zexp(Up, z)
  | (GoDown, z) => move_zexp(Down, z)
  | (GoLeft, z) => move_zexp(Left, z)
  | (GoRight, z) => move_zexp(Right, z)
  | (AddString(s), Cursor(Hole)) => Cursor(Var(s))
  | (AddString(s), Cursor(Var(x))) => Cursor(Var(x ++ s))
  | (Backspace, Cursor(Var(x))) when String.length(x) == 1 => Cursor(Hole)
  | (Backspace, Cursor(Var(x))) => Cursor(Var(backspace(x)))
  | (Backspace, Cursor(_)) => Cursor(Hole)
  | (MakeFun, z) => focus_hole(give_exp(z, Fun(Hole, Hole, Hole)))
  | (MakeAp, Cursor(e)) => RAp(e, Cursor(Hole))
  | (MakeLet, z) => focus_hole(give_exp(z, Let(Hole, Hole, Hole, Hole)))
  // | (MakeFun, Cursor(Hole)) => XFun(Cursor(Hole), Hole, Hole)
  // | (MakeAp, Cursor(Hole)) => LAp(Cursor(Hole), Hole)
  // | (MakeLet, Cursor(Hole)) => XLet(Cursor(Hole), Hole, Hole, Hole)
  | (TextAction, Cursor(Hole)) => apply_zexp(TextAction, Cursor(Var("")))
  | (TextAction, Cursor(Var(x))) =>
    switch (edit_action_of_text(x)) {
    | Some(a') => apply_zexp(a', Cursor(Hole))
    | None => apply_zexp(MakeAp, z)
    }
  | (TextAction, z) => apply_zexp(MakeAp, z)
  | (GiveExp(e), z) => give_exp(z, e)
  | (FillVar, z) => fill_var(z)
  | (Refine, z) => refine(z)
  | (MakeLemma, z) => make_lemma(z)
  | (Auto, z) => auto(z)
  | (FullAuto, z) => full_auto(z)
  // | (GiveExp(e), Cursor(Hole)) => Cursor(e)
  // | (FillVar, z) =>
  //   let z = apply_zexp(FocusHole, z);
  //   let g = local_goal([], Hole, z);
  //   if (complete_typ(g)) {
  //     let c = local_context([], z);
  //     let good_var = ((_: string, t: typ)) => {
  //       consist(g, t) && complete_typ(t);
  //     };
  //     switch (List.filter(c, ~f=good_var)) {
  //     | [] => z
  //     | [(x, _), ..._] => apply_zexp(GiveExp(Var(x)), z)
  //     };
  //   } else {
  //     z;
  //   };
  // | (Refine, z) =>
  //   switch (local_goal([], Hole, z)) {
  //   | Arrow(t1, _) =>
  //     apply_zexp(
  //       FocusHole,
  //       apply_zexp(
  //         GiveExp(
  //           Fun(Text(var_for_typ(t1, local_context([], z))), t1, Hole),
  //         ),
  //         z,
  //       ),
  //     )
  // | _ => z
  // }
  // | (Auto, z) => apply_zexp(Refine, apply_zexp(FillVar, z))
  | (a, XFun(z, t, e)) => XFun(apply_zname(a, z), t, e)
  | (a, TFun(x, z, e)) => TFun(x, apply_ztyp(a, z), e)
  | (a, EFun(x, t, z)) => EFun(x, t, apply_zexp(a, z))
  | (a, LAp(z, e)) => LAp(apply_zexp(a, z), e)
  | (a, RAp(e, z)) => RAp(e, apply_zexp(a, z))
  | (a, XLet(z, t, e1, e2)) => XLet(apply_zname(a, z), t, e1, e2)
  | (a, TLet(x, z, e1, e2)) => TLet(x, apply_ztyp(a, z), e1, e2)
  | (a, E1Let(x, t, z, e2)) => E1Let(x, t, apply_zexp(a, z), e2)
  | (a, E2Let(x, t, e1, z)) => E2Let(x, t, e1, apply_zexp(a, z))
  | _ => z
  };
};

let apply = apply_zexp;
