open Core;
open Terms;

open Find_holes;
open Movement;
open Tactics;

type state = zterm;

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
  | GiveTerm(term)
  | FillVar
  | Refine
  | MakeLemma
  | Auto
  | FullAuto;

let initial_state: zterm =
  E1Let(
    Text("thm1"),
    Arrow(
      Hole,
      Arrow(Hole, Base("a"), Arrow(Hole, Base("b"), Base("c"))),
      Arrow(Hole, Base("b"), Arrow(Hole, Base("a"), Base("c"))),
    ),
    Cursor(Hole),
    Let(
      Text("thm2"),
      Arrow(
        Hole,
        Arrow(Hole, Base("a"), Arrow(Hole, Base("b"), Base("c"))),
        Arrow(
          Hole,
          Arrow(Hole, Base("a"), Base("b")),
          Arrow(Hole, Base("a"), Base("c")),
        ),
      ),
      Hole,
      Let(
        Text("thm3"),
        Arrow(
          Hole,
          Arrow(Hole, Arrow(Hole, Base("a"), Base("a")), Base("b")),
          Base("b"),
        ),
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
let rec apply_zterm = (a: edit_action, z: zterm): zterm => {
  switch (a, z) {
  | (Delete, Cursor(_)) => Cursor(Hole)
  | (NextHole, z) =>
    switch (find_hole_zterm(~loop=true, z)) {
    | Some(z') => z'
    | None => z
    }
  | (FocusHole, Cursor(e)) =>
    switch (find_hole_term(e)) {
    | Some(z') => z'
    | None => z
    }
  | (GoUp, z) => move_zterm(Up, z)
  | (GoDown, z) => move_zterm(Down, z)
  | (GoLeft, z) => move_zterm(Left, z)
  | (GoRight, z) => move_zterm(Right, z)
  | (AddString(s), Cursor(Hole)) => Cursor(Var(s))
  | (AddString(s), Cursor(Var(x))) => Cursor(Var(x ++ s))
  | (Backspace, Cursor(Var(x))) when String.length(x) == 1 => Cursor(Hole)
  | (Backspace, Cursor(Var(x))) => Cursor(Var(backspace(x)))
  | (AddString(s), Cursor(Base(x))) => Cursor(Base(x ++ s))
  | (Backspace, Cursor(Base(x))) when String.length(x) == 1 => Cursor(Hole)
  | (Backspace, Cursor(Base(x))) => Cursor(Base(backspace(x)))
  | (Backspace, Cursor(_)) => Cursor(Hole)
  | (MakeArrow, Cursor(t)) => RArrow(Hole, t, Cursor(Hole))
  | (MakeFun, z) => focus_hole(give_term(z, Fun(Hole, Hole, Hole)))
  | (MakeAp, Cursor(e)) => RAp(e, Cursor(Hole))
  | (MakeLet, z) => focus_hole(give_term(z, Let(Hole, Hole, Hole, Hole)))
  // | (MakeFun, Cursor(Hole)) => XFun(Cursor(Hole), Hole, Hole)
  // | (MakeAp, Cursor(Hole)) => LAp(Cursor(Hole), Hole)
  // | (MakeLet, Cursor(Hole)) => XLet(Cursor(Hole), Hole, Hole, Hole)
  | (TextAction, Cursor(Var(x)))
  | (TextAction, Cursor(Mark(_, Var(x))))
  | (TextAction, Cursor(Base(x)))
  | (TextAction, Cursor(Mark(_, Base(x)))) =>
    switch (edit_action_of_text(x)) {
    | Some(a') => apply_zterm(a', Cursor(Hole))
    | None => apply_zterm(MakeAp, z)
    }
  | (TextAction, Cursor(_)) => apply_zterm(MakeAp, z)
  | (GiveTerm(e), z) => give_term(z, e)
  | (FillVar, z) => fill_var(z)
  | (Refine, z) => refine(z)
  | (MakeLemma, z) => make_lemma(z)
  | (Auto, z) => auto(z)
  | (FullAuto, z) => full_auto(z)
  // | (Giveterm(e), Cursor(Hole)) => Cursor(e)
  // | (FillVar, z) =>
  //   let z = apply_zterm(FocusHole, z);
  //   let g = local_goal([], Hole, z);
  //   if (complete_typ(g)) {
  //     let c = local_context([], z);
  //     let good_var = ((_: string, t: typ)) => {
  //       consist(g, t) && complete_typ(t);
  //     };
  //     switch (List.filter(c, ~f=good_var)) {
  //     | [] => z
  //     | [(x, _), ..._] => apply_zterm(Giveterm(Var(x)), z)
  //     };
  //   } else {
  //     z;
  //   };
  // | (Refine, z) =>
  //   switch (local_goal([], Hole, z)) {
  //   | Arrow(t1, _) =>
  //     apply_zterm(
  //       FocusHole,
  //       apply_zterm(
  //         Giveterm(
  //           Fun(Text(var_for_typ(t1, local_context([], z))), t1, Hole),
  //         ),
  //         z,
  //       ),
  //     )
  // | _ => z
  // }
  // | (Auto, z) => apply_zterm(Refine, apply_zterm(FillVar, z))
  | (a, Mark(m, z)) => Mark(m, apply_zterm(a, z))
  | (a, XArrow(z, t1, t2)) => XArrow(apply_zname(a, z), t1, t2)
  | (a, LArrow(x, z, t2)) => LArrow(x, apply_zterm(a, z), t2)
  | (a, RArrow(x, t1, z)) => RArrow(x, t1, apply_zterm(a, z))
  | (a, XFun(z, t, e)) => XFun(apply_zname(a, z), t, e)
  | (a, TFun(x, z, e)) => TFun(x, apply_zterm(a, z), e)
  | (a, EFun(x, t, z)) => EFun(x, t, apply_zterm(a, z))
  | (a, LAp(z, e)) => LAp(apply_zterm(a, z), e)
  | (a, RAp(e, z)) => RAp(e, apply_zterm(a, z))
  | (a, XLet(z, t, e1, e2)) => XLet(apply_zname(a, z), t, e1, e2)
  | (a, TLet(x, z, e1, e2)) => TLet(x, apply_zterm(a, z), e1, e2)
  | (a, E1Let(x, t, z, e2)) => E1Let(x, t, apply_zterm(a, z), e2)
  | (a, E2Let(x, t, e1, z)) => E2Let(x, t, e1, apply_zterm(a, z))
  | _ => z
  };
};

let apply = apply_zterm;
