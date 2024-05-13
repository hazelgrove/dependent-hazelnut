type name =
  | Hole
  | Text(string);

type zname =
  | Cursor(name);

type pure_term =
  | Hole
  | Typ
  | Var(string)
  | Base(string)
  | Arrow(name, pure_term, pure_term)
  | Fun(name, pure_term, pure_term)
  | Ap(pure_term, pure_term)
  | Let(name, pure_term, pure_term, pure_term);

type zterm =
  | Cursor(pure_term)
  | XArrow(zname, pure_term, pure_term)
  | LArrow(name, zterm, pure_term)
  | RArrow(name, pure_term, zterm)
  | XFun(zname, pure_term, pure_term)
  | TFun(name, zterm, pure_term)
  | EFun(name, pure_term, zterm)
  | LAp(zterm, pure_term)
  | RAp(pure_term, zterm)
  | XLet(zname, pure_term, pure_term, pure_term)
  | TLet(name, zterm, pure_term, pure_term)
  | E1Let(name, pure_term, zterm, pure_term)
  | E2Let(name, pure_term, pure_term, zterm);

type info =
  | None
  | Some({
      goal: term,
      context,
      en: env,
    })

and mark =
  | UnknownVar(string)
  | FunNotArrow(term)
  | Mismatch(term, term)
  | NotTyp(term)

and plain_term =
  | Hole
  | Typ
  | Mark(mark, term)
  | Var(string)
  | Base(string)
  | Arrow(name, term, term)
  | Fun(name, term, term)
  | Ap(term, term)
  | Let(name, term, term, term)

and term =
  | Info(info, plain_term)

and context = list((string, term))

and env = list((string, term));
