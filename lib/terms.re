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

type info = {
  c: context,
  // completes: list(term),
  en: env,
  goal: option(term),
  syn: option(term),
}

and mark =
  | UnknownVar(string)
  | Mismatch(term, term)
  | FunNotArrow(option(term))
  | NotTyp(option(term))

and term =
  | Hole({i: info})
  | Typ({i: info})
  | Mark({
      i: info,
      m: mark,
      e: term,
    })
  | Var({
      i: info,
      x: string,
    })
  | Arrow({
      i: info,
      x: name,
      t1: term,
      t2: term,
    })
  | Fun({
      i: info,
      x: name,
      t: term,
      e: term,
    })
  | Ap({
      i: info,
      e1: term,
      e2: term,
    })
  | Let({
      i: info,
      x: name,
      t: term,
      e1: term,
      e2: term,
    })

and context = list((string, term))

and env = list((string, term));
