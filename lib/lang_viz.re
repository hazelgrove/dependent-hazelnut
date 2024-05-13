open Incr_dom;
open Vdom;
open Terms;
open Lang;
open Web;

let string_aliases = [
  ("exists", "∃"),
  ("exists-con", "∃-con"),
  ("exists-rec", "∃-rec"),
];

let text_of_text = (x: string) =>
  switch (List.assoc_opt(x, string_aliases)) {
  | Some(y) => y
  | None => x
  };

let dom_of_name = (x: name): Node.t =>
  switch (x) {
  | Hole => hole
  | Text(x) => text(text_of_text(x))
  };

let rec dom_of_term =
        (c: context, en: env, completes: list(string), e: term): Node.t =>
  switch (e) {
  | Hole => hole
  | Typ => text("◻")
  | Mark(_, e) => mark([dom_of_term(c, en, completes, e)])
  | Var(x)
  | Base(x) => text(text_of_text(x))
  | Arrow(Hole, t1, t2) =>
    oneline([
      text("("),
      dom_of_term(c, en, completes, t1),
      text("⇒"),
      dom_of_term(c, en, completes, t2),
      text(")"),
    ])
  | Arrow(x, t1, t2) =>
    oneline([
      text("("),
      dom_of_name(x),
      text(":"),
      dom_of_term(c, en, completes, t1),
      text("⇒"),
      dom_of_term(c, en, completes, t2),
      text(")"),
    ])
  | Fun(x, t, e) =>
    let c' = extend_context_name(x, t, c);
    let completes' = extend_complete_list_fun(x, t, completes);
    block_indent(
      [
        dom_of_name(x),
        text(":"),
        dom_of_term(c, en, completes, t),
        text("→"),
      ],
      dom_of_term(c', en, completes', e),
    );
  | Ap(Ap(Var("exists"), t1), Fun(Text(x), t2, t3)) when t1 == t2 =>
    oneline([
      text("("),
      dom_of_term(c, en, completes, Var("exists")),
      dom_of_name(Text(x)),
      text(":"),
      dom_of_term(c, en, completes, t1),
      text("."),
      dom_of_term(c, en, completes, t3),
      text(")"),
    ])
  | Ap(Ap(Var("exists"), t1), t2) =>
    oneline([
      text("("),
      dom_of_term(c, en, completes, Var("exists")),
      dom_of_term(c, en, completes, t1),
      text("."),
      dom_of_term(c, en, completes, t2),
      text(")"),
    ])
  | Ap(e1, e2) =>
    oneline([
      text("("),
      dom_of_term(c, en, completes, e1),
      text(")("),
      dom_of_term(c, en, completes, e2),
      text(")"),
    ])
  | Let(x, t, e1, e2) =>
    let c' = extend_context_name(x, t, c);
    let en' = maybe_extend_env_name(x, e1, en);
    let completes' = extend_complete_list_let(x, t, e1, completes);
    sub_block(
      [
        check_let(c, en, t, completes, e1)
          ? Node.text("🟩") : Node.text(""),
        dom_of_name(x),
        text(":"),
        dom_of_term(c, en, completes, t),
        text("←"),
      ],
      dom_of_term(c, en', completes, e1),
      dom_of_term(c', en, completes', e2),
    );
  };

let doms_of_context = (c: context, en: env): list(Node.t) => {
  let dom_of_entry = ((x: string, t: term)): Node.t => {
    Node.div(
      ~attr=Attr.create("class", "context-entry"),
      [oneline([text(x), text(":"), dom_of_term(c, en, [], t)])],
    );
  };
  List.map(dom_of_entry, c);
};

let dom_of_mark =
    (c: context, en: env, completes: list(string), m: mark): Node.t =>
  switch (m) {
  | UnknownVar(x) => Node.text("Unrecognized variable " ++ x)
  | FunNotArrow(t) =>
    oneline([
      Node.text("Cannot apply term of non-funciton type "),
      dom_of_term(c, en, completes, t),
    ])
  | Mismatch(t1, t2) =>
    oneline([
      Node.text("Expected type "),
      dom_of_term(c, en, completes, t1),
      Node.text(" but found inconsistent type "),
      dom_of_term(c, en, completes, t2),
    ])
  | NotTyp(t) =>
    oneline([
      Node.text("Types must be of type "),
      dom_of_term(c, en, completes, Typ),
      Node.text(" but found inconsistent type "),
      dom_of_term(c, en, completes, t),
    ])
  };

let doms_of_marks =
    (c: context, en: env, completes: list(string), ms: list(mark))
    : list(Node.t) => {
  let dom_of_entry = (m: mark): Node.t => {
    Node.div(
      ~attr=Attr.create("class", "mark-entry"),
      [dom_of_mark(c, en, completes, m)],
    );
  };
  List.map(dom_of_entry, ms);
};

let dom_of_zname = (z: zname): Node.t =>
  switch (z) {
  | Cursor(Hole) => cursor_hole
  | Cursor(x) => cursor([dom_of_name(x)])
  };

let rec dom_of_zterm =
        (c: context, en: env, completes: list(string), z: zterm): Node.t => {
  switch (z) {
  | Cursor(Hole) => cursor_hole
  | Cursor(e) => cursor([dom_of_term(c, en, completes, e)])
  | Mark(_, z) => mark([dom_of_zterm(c, en, completes, z)])
  | XArrow(z, t1, t2) =>
    oneline([
      text("("),
      dom_of_zname(z),
      text(":"),
      dom_of_term(c, en, completes, t1),
      Node.text("⇒"),
      dom_of_term(c, en, completes, t2),
      text(")"),
    ])
  | LArrow(x, z, t2) =>
    oneline([
      text("("),
      dom_of_name(x),
      text(":"),
      dom_of_zterm(c, en, completes, z),
      Node.text("⇒"),
      dom_of_term(c, en, completes, t2),
      text(")"),
    ])
  | RArrow(x, t1, z) =>
    oneline([
      text("("),
      dom_of_name(x),
      text(":"),
      dom_of_term(c, en, completes, t1),
      Node.text("⇒"),
      dom_of_zterm(c, en, completes, z),
      text(")"),
    ])
  | XFun(z, t, e) =>
    let c' = extend_context_name(name_of_zname(z), t, c);
    let completes' =
      extend_complete_list_fun(name_of_zname(z), t, completes);
    block_indent(
      [
        dom_of_zname(z),
        text(":"),
        dom_of_term(c, en, completes, t),
        text("→"),
      ],
      dom_of_term(c', en, completes', e),
    );
  | TFun(x, z, e) =>
    let c' = extend_context_name(x, term_of_zterm(z), c);
    let completes' =
      extend_complete_list_fun(x, term_of_zterm(z), completes);
    block_indent(
      [
        dom_of_name(x),
        text(":"),
        dom_of_zterm(c, en, completes, z),
        text("→"),
      ],
      dom_of_term(c', en, completes', e),
    );
  | EFun(x, t, z) =>
    let c' = extend_context_name(x, t, c);
    let completes' = extend_complete_list_fun(x, t, completes);
    block_indent(
      [
        dom_of_name(x),
        text(":"),
        dom_of_term(c, en, completes, t),
        text("→"),
      ],
      dom_of_zterm(c', en, completes', z),
    );
  | LAp(z, e) =>
    oneline([
      text("("),
      dom_of_zterm(c, en, completes, z),
      text(")("),
      dom_of_term(c, en, completes, e),
      text(")"),
    ])
  | RAp(e, z) =>
    oneline([
      text("("),
      dom_of_term(c, en, completes, e),
      text(")("),
      dom_of_zterm(c, en, completes, z),
      text(")"),
    ])
  | XLet(x, t, e1, e2) =>
    let c' = extend_context_name(name_of_zname(x), t, c);
    let en' = maybe_extend_env_name(name_of_zname(x), e1, en);
    let completes' =
      extend_complete_list_let(name_of_zname(x), t, e1, completes);
    sub_block(
      [
        check_let(c, en, t, completes, e1)
          ? Node.text("🟩") : Node.text(""),
        dom_of_zname(x),
        text(":"),
        dom_of_term(c, en, completes, t),
        text("←"),
      ],
      dom_of_term(c, en, completes, e1),
      dom_of_term(c', en', completes', e2),
    );
  | TLet(x, t, e1, e2) =>
    let c' = extend_context_name(x, term_of_zterm(t), c);
    let en' = maybe_extend_env_name(x, e1, en);
    let completes' =
      extend_complete_list_let(x, term_of_zterm(t), e1, completes);
    sub_block(
      [
        check_let(c, en, term_of_zterm(t), completes, e1)
          ? Node.text("🟩") : Node.text(""),
        dom_of_name(x),
        text(":"),
        dom_of_zterm(c, en, completes, t),
        text("←"),
      ],
      dom_of_term(c, en, completes, e1),
      dom_of_term(c', en', completes', e2),
    );
  | E1Let(x, t, e1, e2) =>
    let c' = extend_context_name(x, t, c);
    let en' = maybe_extend_env_name(x, term_of_zterm(e1), en);
    let completes' =
      extend_complete_list_let(x, t, term_of_zterm(e1), completes);
    sub_block(
      [
        check_let(c, en, t, completes, term_of_zterm(e1))
          ? Node.text("🟩") : Node.text(""),
        dom_of_name(x),
        text(":"),
        dom_of_term(c, en, completes, t),
        text("←"),
      ],
      dom_of_zterm(c, en, completes, e1),
      dom_of_term(c', en', completes', e2),
    );
  | E2Let(x, t, e1, e2) =>
    let c' = extend_context_name(x, t, c);
    let en' = maybe_extend_env_name(x, e1, en);
    let completes' = extend_complete_list_let(x, t, e1, completes);
    sub_block(
      [
        check_let(c, en, t, completes, e1)
          ? Node.text("🟩") : Node.text(""),
        dom_of_name(x),
        text(":"),
        dom_of_term(c, en, completes, t),
        text("←"),
      ],
      dom_of_term(c, en, completes, e1),
      dom_of_zterm(c', en', completes', e2),
    );
  };
};
