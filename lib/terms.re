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

type sterm =
  | Hole
  | Typ
  | Mark(mark, sterm)
  | Var(string, option(int))
  | Arrow(name, sterm, sterm)
  | Fun(name, sterm, sterm)
  | Ap(sterm, sterm)
  | Let(name, sterm, sterm, sterm)

and mark =
  | UnknownVar(string)
  | Mismatch(sterm, sterm)
  | FunNotArrow(option(sterm))
  | NotTyp(option(sterm))
  | Inconsistent;

type context_entry = {
  x: name,
  t: sterm,
  e: option(sterm),
};

type context = list(context_entry);

type info = {
  ctx: context,
  goal: option(sterm),
  syn: option(sterm),
  cursed: bool,
  name_cursed: bool,
  cursor_inside: bool,
  highlighted: bool,
};

type term_bare('a) =
  | Hole
  | Typ
  | Mark(mark, term('a))
  | Var(string, option(int))
  | Arrow(name, term('a), term('a))
  | Fun(name, term('a), term('a))
  | Ap(term('a), term('a))
  | Let(name, term('a), term('a), term('a))

and term('a) = ('a, term_bare('a));

type info_term = term(info);

let default_info: info = {
  ctx: [],
  goal: None,
  syn: None,
  cursed: false,
  name_cursed: false,
  cursor_inside: false,
  highlighted: false,
};
