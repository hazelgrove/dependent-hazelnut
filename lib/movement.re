open Terms;
open Lang;

type direction =
  | Up
  | Down
  | Left
  | Right;

let rec move_zterm = (d, z): zterm =>
  switch (d, z) {
  | _ => z
  };

let rec move_zterm = (d, z): zterm =>
  switch (d, z) {
  | (Up, XArrow(Cursor(x), t1, t2))
  | (Up, LArrow(x, Cursor(t1), t2))
  | (Up, RArrow(x, t1, Cursor(t2))) => Cursor(Arrow(x, t1, t2))
  | (Up, XFun(Cursor(x), t, e)) => Cursor(Fun(x, t, e))
  | (Up, TFun(x, Cursor(t), e)) => Cursor(Fun(x, t, e))
  | (Up, EFun(x, t, Cursor(e))) => Cursor(Fun(x, t, e))
  | (Up, LAp(Cursor(e1), e2)) => Cursor(Ap(e1, e2))
  | (Up, RAp(e1, Cursor(e2))) => Cursor(Ap(e1, e2))
  | (Up, XLet(Cursor(x), t, e1, e2)) => Cursor(Let(x, t, e1, e2))
  | (Up, TLet(x, Cursor(t), e1, e2)) => Cursor(Let(x, t, e1, e2))
  | (Up, E1Let(x, t, Cursor(e1), e2)) => Cursor(Let(x, t, e1, e2))
  | (Up, E2Let(x, t, e1, Cursor(e2))) => Cursor(Let(x, t, e1, e2))
  | (Down, Cursor(Arrow(x, t1, t2))) => XArrow(Cursor(x), t1, t2)
  | (Down, Cursor(Fun(x, t, e))) => XFun(Cursor(x), t, e)
  | (Down, Cursor(Ap(e1, e2))) => LAp(Cursor(e1), e2)
  | (Down, Cursor(Let(x, t, e1, e2))) => XLet(Cursor(x), t, e1, e2)
  | (Left, LArrow(x, z, t2)) =>
    let z' = move_zterm(Left, z);
    z != z' ? LArrow(x, z', t2) : XArrow(Cursor(x), term_of_zterm(z), t2);
  | (Left, RArrow(x, t1, z)) =>
    let z' = move_zterm(Left, z);
    z != z' ? RArrow(x, t1, z') : LArrow(x, Cursor(t1), term_of_zterm(z));
  | (Left, TFun(x, z, e)) =>
    let z' = move_zterm(Left, z);
    z != z' ? TFun(x, z', e) : XFun(Cursor(x), term_of_zterm(z), e);
  | (Left, EFun(x, t, z)) =>
    let z' = move_zterm(Left, z);
    z != z' ? EFun(x, t, z') : TFun(x, Cursor(t), term_of_zterm(z));
  | (Left, RAp(e1, z)) =>
    let z' = move_zterm(Left, z);
    z != z' ? RAp(e1, z') : LAp(Cursor(e1), term_of_zterm(z));
  | (Left, TLet(x, z, e1, e2)) =>
    let z' = move_zterm(Left, z);
    z != z'
      ? TLet(x, z', e1, e2) : XLet(Cursor(x), term_of_zterm(z), e1, e2);
  | (Left, E1Let(x, t, z, e2)) =>
    let z' = move_zterm(Left, z);
    z != z'
      ? E1Let(x, t, z', e2) : TLet(x, Cursor(t), term_of_zterm(z), e2);
  | (Left, E2Let(x, t, e1, z)) =>
    let z' = move_zterm(Left, z);
    z != z'
      ? E2Let(x, t, e1, z') : E1Let(x, t, Cursor(e1), term_of_zterm(z));
  | (Right, XArrow(z, t1, t2)) => LArrow(name_of_zname(z), Cursor(t1), t2)
  | (Right, LArrow(x, z, t2)) =>
    let z' = move_zterm(Right, z);
    z != z' ? LArrow(x, z', t2) : RArrow(x, term_of_zterm(z), Cursor(t2));
  | (Right, XFun(z, t, e)) => TFun(name_of_zname(z), Cursor(t), e)
  | (Right, TFun(x, z, e)) =>
    let z' = move_zterm(Right, z);
    z != z'
      ? TFun(x, z', e)
      : EFun(x, term_of_zterm(z), move_zterm(Down, Cursor(e))); // descend
  | (Right, LAp(z, e2)) =>
    let z' = move_zterm(Right, z);
    z != z' ? LAp(z', e2) : RAp(term_of_zterm(z), Cursor(e2));
  | (Right, XLet(z, t, e1, e2)) =>
    TLet(name_of_zname(z), Cursor(t), e1, e2)
  | (Right, TLet(x, z, e1, e2)) =>
    let z' = move_zterm(Right, z);
    z != z'
      ? TLet(x, z', e1, e2)
      : E1Let(x, term_of_zterm(z), move_zterm(Down, Cursor(e1)), e2); // descend
  | (Right, E1Let(x, t, z, e2)) =>
    let z' = move_zterm(Right, z);
    z != z'
      ? E1Let(x, t, z', e2)
      : E2Let(x, t, term_of_zterm(z), move_zterm(Down, Cursor(e2))); // descend
  // | (d, XFun(z, t, e)) => XFun(apply_zname(a, z), t, e)
  | (d, LArrow(x, z, t)) => LArrow(x, move_zterm(d, z), t)
  | (d, RArrow(x, t, z)) => RArrow(x, t, move_zterm(d, z))
  | (d, TFun(x, z, e)) => TFun(x, move_zterm(d, z), e)
  | (d, EFun(x, t, z)) => EFun(x, t, move_zterm(d, z))
  | (d, LAp(z, e)) => LAp(move_zterm(d, z), e)
  | (d, RAp(e, z)) => RAp(e, move_zterm(d, z))
  // | (d, XLet(z, t, e1, e2)) => XLet(apply_zname(a, z), t, e1, e2)
  | (d, TLet(x, z, e1, e2)) => TLet(x, move_zterm(d, z), e1, e2)
  | (d, E1Let(x, t, z, e2)) => E1Let(x, t, move_zterm(d, z), e2)
  | (d, E2Let(x, t, e1, z)) => E2Let(x, t, e1, move_zterm(d, z))
  | _ => z
  };
