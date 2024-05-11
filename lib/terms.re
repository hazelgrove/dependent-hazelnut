type name =
  | Hole
  | Text(string);

type zname =
  | Cursor(name);

type mark =
  | UnknownVar(string)
  | FunNotArrow(term)
  | Mismatch(term, term)

and term =
  | Hole
  | Typ
  | Mark(mark, term)
  | Var(string)
  | Base(string)
  | Arrow(term, term)
  | Fun(name, term, term)
  | Ap(term, term)
  | Let(name, term, term, term);

type zterm =
  | Cursor(term)
  | Mark(mark, zterm)
  | LArrow(zterm, term)
  | RArrow(term, zterm)
  | XFun(zname, term, term)
  | TFun(name, zterm, term)
  | EFun(name, term, zterm)
  | LAp(zterm, term)
  | RAp(term, zterm)
  | XLet(zname, term, term, term)
  | TLet(name, zterm, term, term)
  | E1Let(name, term, zterm, term)
  | E2Let(name, term, term, zterm);

type context = list((string, term));
