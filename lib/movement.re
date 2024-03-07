open Terms;
open Lang;

type direction =
  | Up
  | Down
  | Left
  | Right;

let rec move_ztyp = (d, z): ztyp =>
  switch (d, z) {
  | (Up, LArrow(Cursor(t1), t2)) => Cursor(Arrow(t1, t2))
  | (Up, RArrow(t1, Cursor(t2))) => Cursor(Arrow(t1, t2))
  | (Down, Cursor(Arrow(t1, t2))) => LArrow(Cursor(t1), t2)
  | (Left, RArrow(t1, z)) =>
    let z' = move_ztyp(Left, z);
    z != z' ? RArrow(t1, z') : LArrow(Cursor(t1), typ_of_ztyp(z));
  | (Right, LArrow(z, t2)) =>
    let z' = move_ztyp(Right, z);
    z != z' ? LArrow(z', t2) : RArrow(typ_of_ztyp(z), Cursor(t2));
  | (d, LArrow(z, t)) => LArrow(move_ztyp(d, z), t)
  | (d, RArrow(t, z)) => RArrow(t, move_ztyp(d, z))
  | _ => z
  };

let rec move_zexp = (d, z): zexp =>
  switch (d, z) {
  | (Up, XFun(Cursor(x), t, e)) => Cursor(Fun(x, t, e))
  | (Up, TFun(x, Cursor(t), e)) => Cursor(Fun(x, t, e))
  | (Up, EFun(x, t, Cursor(e))) => Cursor(Fun(x, t, e))
  | (Up, LAp(Cursor(e1), e2)) => Cursor(Ap(e1, e2))
  | (Up, RAp(e1, Cursor(e2))) => Cursor(Ap(e1, e2))
  | (Up, XLet(Cursor(x), t, e1, e2)) => Cursor(Let(x, t, e1, e2))
  | (Up, TLet(x, Cursor(t), e1, e2)) => Cursor(Let(x, t, e1, e2))
  | (Up, E1Let(x, t, Cursor(e1), e2)) => Cursor(Let(x, t, e1, e2))
  | (Up, E2Let(x, t, e1, Cursor(e2))) => Cursor(Let(x, t, e1, e2))
  | (Down, Cursor(Fun(x, t, e))) => XFun(Cursor(x), t, e)
  | (Down, Cursor(Ap(e1, e2))) => LAp(Cursor(e1), e2)
  | (Down, Cursor(Let(x, t, e1, e2))) => XLet(Cursor(x), t, e1, e2)
  | (Left, TFun(x, z, e)) =>
    let z' = move_ztyp(Left, z);
    z != z' ? TFun(x, z', e) : XFun(Cursor(x), typ_of_ztyp(z), e);
  | (Left, EFun(x, t, z)) =>
    let z' = move_zexp(Left, z);
    z != z' ? EFun(x, t, z') : TFun(x, Cursor(t), exp_of_zexp(z));
  | (Left, RAp(e1, z)) =>
    let z' = move_zexp(Left, z);
    z != z' ? RAp(e1, z') : LAp(Cursor(e1), exp_of_zexp(z));
  | (Left, TLet(x, z, e1, e2)) =>
    let z' = move_ztyp(Left, z);
    z != z'
      ? TLet(x, z', e1, e2) : XLet(Cursor(x), typ_of_ztyp(z), e1, e2);
  | (Left, E1Let(x, t, z, e2)) =>
    let z' = move_zexp(Left, z);
    z != z' ? E1Let(x, t, z', e2) : TLet(x, Cursor(t), exp_of_zexp(z), e2);
  | (Left, E2Let(x, t, e1, z)) =>
    let z' = move_zexp(Left, z);
    z != z'
      ? E2Let(x, t, e1, z') : E1Let(x, t, Cursor(e1), exp_of_zexp(z));
  | (Right, XFun(z, t, e)) => TFun(name_of_zname(z), Cursor(t), e)
  | (Right, TFun(x, z, e)) =>
    let z' = move_ztyp(Right, z);
    z != z'
      ? TFun(x, z', e)
      : EFun(x, typ_of_ztyp(z), move_zexp(Down, Cursor(e))); // descend
  | (Right, LAp(z, e2)) =>
    let z' = move_zexp(Right, z);
    z != z' ? LAp(z', e2) : RAp(exp_of_zexp(z), Cursor(e2));
  | (Right, XLet(z, t, e1, e2)) =>
    TLet(name_of_zname(z), Cursor(t), e1, e2)
  | (Right, TLet(x, z, e1, e2)) =>
    let z' = move_ztyp(Right, z);
    z != z'
      ? TLet(x, z', e1, e2)
      : E1Let(x, typ_of_ztyp(z), move_zexp(Down, Cursor(e1)), e2); // descend
  | (Right, E1Let(x, t, z, e2)) =>
    let z' = move_zexp(Right, z);
    z != z'
      ? E1Let(x, t, z', e2)
      : E2Let(x, t, exp_of_zexp(z), move_zexp(Down, Cursor(e2))); // descend
  // | (d, XFun(z, t, e)) => XFun(apply_zname(a, z), t, e)
  | (d, TFun(x, z, e)) => TFun(x, move_ztyp(d, z), e)
  | (d, EFun(x, t, z)) => EFun(x, t, move_zexp(d, z))
  | (d, LAp(z, e)) => LAp(move_zexp(d, z), e)
  | (d, RAp(e, z)) => RAp(e, move_zexp(d, z))
  // | (d, XLet(z, t, e1, e2)) => XLet(apply_zname(a, z), t, e1, e2)
  | (d, TLet(x, z, e1, e2)) => TLet(x, move_ztyp(d, z), e1, e2)
  | (d, E1Let(x, t, z, e2)) => E1Let(x, t, move_zexp(d, z), e2)
  | (d, E2Let(x, t, e1, z)) => E2Let(x, t, e1, move_zexp(d, z))
  | _ => z
  };
