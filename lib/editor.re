open Core;
open Terms;
open Lang;
open Lang_viz;

open Library;

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
  | MakeTyp
  | MakeFun
  | MakeAp
  | MakeLet
  | MakeArrow
  | TextAction
  | GiveTerm(pterm)
  | FillVar
  | Refine
  | Equational
  | MakeLemma
  | Auto
  | FullAuto
  | Save;

let initial_state: zterm = library;
let initial_context: context = {
  let z = context_library;
  let e = syn([], term_of_pterm(pterm_of_zterm(z)));
  let e = term_at_cursor(z, e);
  get_info(e).ctx;
};

let backspace = (s: string) =>
  String.sub(s, ~pos=0, ~len=String.length(s) - 1);

let edit_action_of_text = (s: string) =>
  switch (s) {
  | "fun" => Some(MakeFun)
  | "let" => Some(MakeLet)
  | "typ" => Some(MakeTyp)
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
let keyword_var = (e: pterm) =>
  switch (e) {
  | Var(x) =>
    switch (edit_action_of_text(x)) {
    | Some(_) => true
    | None => false
    }
  | _ => false
  };
let rec apply_zterm = (a: edit_action, z: zterm, ec: term): zterm => {
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
  | (Backspace, Cursor(_)) => Cursor(Hole)
  | (MakeTyp, Cursor(_)) => Cursor(Typ)
  | (MakeArrow, Cursor(t)) => RArrow(Hole, t, Cursor(Hole))
  | (MakeFun, z) => focus_hole(give_term(z, Fun(Hole, Hole, Hole)))
  // | (TextAction, RAp(e1, Cursor(e2))) when !keyword_var(e2) =>
  //   RAp(Ap(e1, e2), Cursor(Hole))
  | (MakeAp, Cursor(e)) => RAp(e, Cursor(Hole))
  | (MakeLet, z) => focus_hole(give_term(z, Let(Hole, Hole, Hole, Hole)))
  // | (MakeFun, Cursor(Hole)) => XFun(Cursor(Hole), Hole, Hole)
  // | (MakeAp, Cursor(Hole)) => LAp(Cursor(Hole), Hole)
  // | (MakeLet, Cursor(Hole)) => XLet(Cursor(Hole), Hole, Hole, Hole)
  | (TextAction, Cursor(Var(x))) =>
    switch (edit_action_of_text(x)) {
    | Some(a') => apply_zterm(a', Cursor(Hole), ec)
    | None => apply_zterm(MakeAp, z, ec)
    }
  | (TextAction, Cursor(_)) => apply_zterm(MakeAp, z, ec)
  | (GiveTerm(e), z) => give_term(z, e)
  | (FillVar, z) =>
    print_endline("not implemented");
    z; //fill_var(z)
  | (Refine, z) => refine(z, ec)
  | (Equational, z) => equational(z, ec)
  | (MakeLemma, z) =>
    print_endline("not implemented");
    z; //make_lemma(z)
  | (Auto, z) =>
    print_endline("not implemented");
    z; //auto(z)
  | (FullAuto, z) =>
    print_endline("not implemented");
    z; //full_auto(z)
  | (Save, z) =>
    switch (pterm_at_cursor(z)) {
    | None => z
    | Some(e) =>
      print_endline(string_of_pterm(e));
      z;
    }
  | (a, XArrow(z, t1, t2)) => XArrow(apply_zname(a, z), t1, t2)
  | (a, LArrow(x, z, t2)) => LArrow(x, apply_zterm(a, z, ec), t2)
  | (a, RArrow(x, t1, z)) => RArrow(x, t1, apply_zterm(a, z, ec))
  | (a, XFun(z, t, e)) => XFun(apply_zname(a, z), t, e)
  | (a, TFun(x, z, e)) => TFun(x, apply_zterm(a, z, ec), e)
  | (a, EFun(x, t, z)) => EFun(x, t, apply_zterm(a, z, ec))
  | (a, LAp(z, e)) => LAp(apply_zterm(a, z, ec), e)
  | (a, RAp(e, z)) => RAp(e, apply_zterm(a, z, ec))
  | (a, XLet(z, t, e1, e2)) => XLet(apply_zname(a, z), t, e1, e2)
  | (a, TLet(x, z, e1, e2)) => TLet(x, apply_zterm(a, z, ec), e1, e2)
  | (a, E1Let(x, t, z, e2)) => E1Let(x, t, apply_zterm(a, z, ec), e2)
  | (a, E2Let(x, t, e1, z)) => E2Let(x, t, e1, apply_zterm(a, z, ec))
  | _ => z
  };
};

let apply = apply_zterm;
