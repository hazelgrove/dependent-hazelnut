type name =
  | Hole
  | Text(string);

type zname =
  | Cursor(name);

type typ =
  | Hole
  | Base(string)
  | Arrow(typ, typ);
// | Product(exp, exp)

type ztyp =
  | Cursor(typ)
  | LArrow(ztyp, typ)
  | RArrow(typ, ztyp);

type mark =
  | UnknownVar(string)
  | FunNotArrow(typ)
  | Mismatch(typ, typ);

type exp =
  | Hole
  | Mark(mark, exp)
  | Var(string)
  | Fun(name, typ, exp)
  | Ap(exp, exp)
  | Let(name, typ, exp, exp);
// | Pair(exp, exp)

type zexp =
  | Cursor(exp)
  | Mark(mark, zexp)
  | XFun(zname, typ, exp)
  | TFun(name, ztyp, exp)
  | EFun(name, typ, zexp)
  | LAp(zexp, exp)
  | RAp(exp, zexp)
  | XLet(zname, typ, exp, exp)
  | TLet(name, ztyp, exp, exp)
  | E1Let(name, typ, zexp, exp)
  | E2Let(name, typ, exp, zexp);

type context = list((string, typ));
