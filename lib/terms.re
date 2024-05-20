type name =
  | Hole
  | Text(string);

type zname =
  | Cursor(name);

// Or "pure term" - what's under and around the cursor in a zterm
type pterm =
  | Hole
  | Typ
  | Var(string)
  | Arrow(name, pterm, pterm)
  | Fun(name, pterm, pterm)
  | Ap(pterm, pterm)
  | Let(name, pterm, pterm, pterm);

type zterm =
  | Cursor(pterm)
  | XArrow(zname, pterm, pterm)
  | LArrow(name, zterm, pterm)
  | RArrow(name, pterm, zterm)
  | XFun(zname, pterm, pterm)
  | TFun(name, zterm, pterm)
  | EFun(name, pterm, zterm)
  | LAp(zterm, pterm)
  | RAp(pterm, zterm)
  | XLet(zname, pterm, pterm, pterm)
  | TLet(name, zterm, pterm, pterm)
  | E1Let(name, pterm, zterm, pterm)
  | E2Let(name, pterm, pterm, zterm);

type info = {
  ctx: context,
  goal: option(term),
  syn: option(term),
  cursed: bool,
  name_cursed: bool,
  cursor_inside: bool,
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
      idx: option(int),
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

and context_entry = {
  x: name,
  t: term,
  e: option(term),
}
and context = list(context_entry);
