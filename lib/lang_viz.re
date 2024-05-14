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

let dom_of_name = (x: name, cursed: bool): Node.t =>
  switch (x) {
  | Hole => if (cursed) {cursor_hole} else {hole}
  | Text(x) =>
    let dom = text(text_of_text(x));
    if (cursed) {
      cursor([dom]);
    } else {
      dom;
    };
  };

let rec dom_of_term = (~check_cursor=true, e: term): Node.t =>
  switch (e) {
  | Hole(r) when r.i.cursed => cursor_hole
  | e when get_info(e).cursed && check_cursor =>
    cursor([dom_of_term(~check_cursor=false, e)])
  | Hole(_) => hole
  | Typ(_) => text("◻")
  | Mark(r) => mark([dom_of_term(r.e)])
  | Var(r) => text(text_of_text(r.x))
  // | Arrow({i: _, x: Hole, t1, t2}) =>
  //   oneline([
  //     text("("),
  //     dom_of_term(t1),
  //     text("⇒"),
  //     dom_of_term(t2),
  //     text(")"),
  //   ])
  | Arrow(r) =>
    oneline([
      text("("),
      dom_of_name(r.x, r.i.name_cursed),
      text(":"),
      dom_of_term(r.t1),
      text("⇒"),
      dom_of_term(r.t2),
      text(")"),
    ])
  | Fun(r) =>
    block_indent(
      [
        dom_of_name(r.x, r.i.name_cursed),
        text(":"),
        dom_of_term(r.t),
        text("→"),
      ],
      dom_of_term(r.e),
    )
  // | Ap(Ap(Var("exists"), t1), Fun(Text(x), t2, t3)) when t1 == t2 =>
  //   oneline([
  //     text("("),
  //     dom_of_term(Var("exists")),
  //     dom_of_name(Text(x)),
  //     text(":"),
  //     dom_of_term(t1),
  //     text("."),
  //     dom_of_term(t3),
  //     text(")"),
  //   ])
  // | Ap(Ap(Var("exists"), t1), t2) =>
  //   oneline([
  //     text("("),
  //     dom_of_term(Var("exists")),
  //     dom_of_term(t1),
  //     text("."),
  //     dom_of_term(t2),
  //     text(")"),
  //   ])
  | Ap(r) =>
    oneline([
      text("("),
      dom_of_term(r.e1),
      text(")("),
      dom_of_term(r.e2),
      text(")"),
    ])
  | Let(r) =>
    sub_block(
      [
        // check_let(c, en, t, completes, e1)
        //   ? Node.text("🟩") :
        Node.text(""),
        dom_of_name(r.x, r.i.name_cursed),
        text(":"),
        dom_of_term(r.t),
        text("←"),
      ],
      dom_of_term(r.e1),
      dom_of_term(r.e2),
    )
  };

let doms_of_context = (c: context): list(Node.t) => {
  let dom_of_entry = ((x: string, t: term)): Node.t => {
    Node.div(
      ~attr=Attr.create("class", "context-entry"),
      [oneline([text(x), text(":"), dom_of_term(t)])],
    );
  };
  List.map(dom_of_entry, c);
};

let dom_of_mark = (m: mark): Node.t => {
  let dom_of_term_option = (t: option(term)) =>
    switch (t) {
    | None => text("")
    | Some(t) => dom_of_term(t)
    };
  switch (m) {
  | UnknownVar(x) => Node.text("Unrecognized variable " ++ x)
  | Mismatch(t1, t2) =>
    oneline([
      Node.text("Expected type "),
      dom_of_term(t1),
      Node.text(" but found inconsistent type "),
      dom_of_term(t2),
    ])
  | FunNotArrow(t) =>
    oneline([
      Node.text("Cannot apply term of non-funciton type "),
      dom_of_term_option(t),
    ])
  | NotTyp(t) =>
    oneline([
      Node.text("Types must be of type "),
      dom_of_term(Typ({i: default_info})),
      Node.text(" but found inconsistent type "),
      dom_of_term_option(t),
    ])
  };
};

let doms_of_marks = (ms: list(mark)): list(Node.t) => {
  let dom_of_entry = (m: mark): Node.t => {
    Node.div(~attr=Attr.create("class", "mark-entry"), [dom_of_mark(m)]);
  };
  List.map(dom_of_entry, ms);
};

// let dom_of_zname = (z: zname): Node.t =>
//   switch (z) {
//   | Cursor(Hole) => cursor_hole
//   | Cursor(x) => cursor([dom_of_name(x)])
//   };

// let rec dom_of_zterm =
//         (c: context, en: env, completes: list(string), z: zterm): Node.t => {
//   switch (z) {
//   | Cursor(Hole) => cursor_hole
//   | Cursor(e) => cursor([dom_of_term(e)])
//   | Mark(_, z) => mark([dom_of_zterm(c, en, completes, z)])
//   | XArrow(z, t1, t2) =>
//     oneline([
//       text("("),
//       dom_of_zname(z),
//       text(":"),
//       dom_of_term(t1),
//       Node.text("⇒"),
//       dom_of_term(t2),
//       text(")"),
//     ])
//   | LArrow(x, z, t2) =>
//     oneline([
//       text("("),
//       dom_of_name(x),
//       text(":"),
//       dom_of_zterm(c, en, completes, z),
//       Node.text("⇒"),
//       dom_of_term(t2),
//       text(")"),
//     ])
//   | RArrow(x, t1, z) =>
//     oneline([
//       text("("),
//       dom_of_name(x),
//       text(":"),
//       dom_of_term(t1),
//       Node.text("⇒"),
//       dom_of_zterm(c, en, completes, z),
//       text(")"),
//     ])
//   | XFun(z, t, e) =>
//     let c' = extend_context_name(name_of_zname(z), t, c);
//     let completes' =
//       extend_complete_list_fun(name_of_zname(z), t, completes);
//     block_indent(
//       [dom_of_zname(z), text(":"), dom_of_term(t), text("→")],
//       dom_of_term(c', en, completes', e),
//     );
//   | TFun(x, z, e) =>
//     let c' = extend_context_name(x, term_of_zterm(z), c);
//     let completes' =
//       extend_complete_list_fun(x, term_of_zterm(z), completes);
//     block_indent(
//       [
//         dom_of_name(x),
//         text(":"),
//         dom_of_zterm(c, en, completes, z),
//         text("→"),
//       ],
//       dom_of_term(c', en, completes', e),
//     );
//   | EFun(x, t, z) =>
//     let c' = extend_context_name(x, t, c);
//     let completes' = extend_complete_list_fun(x, t, completes);
//     block_indent(
//       [dom_of_name(x), text(":"), dom_of_term(t), text("→")],
//       dom_of_zterm(c', en, completes', z),
//     );
//   | LAp(z, e) =>
//     oneline([
//       text("("),
//       dom_of_zterm(c, en, completes, z),
//       text(")("),
//       dom_of_term(e),
//       text(")"),
//     ])
//   | RAp(e, z) =>
//     oneline([
//       text("("),
//       dom_of_term(e),
//       text(")("),
//       dom_of_zterm(c, en, completes, z),
//       text(")"),
//     ])
//   | XLet(x, t, e1, e2) =>
//     let c' = extend_context_name(name_of_zname(x), t, c);
//     let en' = maybe_extend_env_name(name_of_zname(x), e1, en);
//     let completes' =
//       extend_complete_list_let(name_of_zname(x), t, e1, completes);
//     sub_block(
//       [
//         check_let(c, en, t, completes, e1)
//           ? Node.text("🟩") : Node.text(""),
//         dom_of_zname(x),
//         text(":"),
//         dom_of_term(t),
//         text("←"),
//       ],
//       dom_of_term(e1),
//       dom_of_term(c', en', completes', e2),
//     );
//   | TLet(x, t, e1, e2) =>
//     let c' = extend_context_name(x, term_of_zterm(t), c);
//     let en' = maybe_extend_env_name(x, e1, en);
//     let completes' =
//       extend_complete_list_let(x, term_of_zterm(t), e1, completes);
//     sub_block(
//       [
//         check_let(c, en, term_of_zterm(t), completes, e1)
//           ? Node.text("🟩") : Node.text(""),
//         dom_of_name(x),
//         text(":"),
//         dom_of_zterm(c, en, completes, t),
//         text("←"),
//       ],
//       dom_of_term(e1),
//       dom_of_term(c', en', completes', e2),
//     );
//   | E1Let(x, t, e1, e2) =>
//     let c' = extend_context_name(x, t, c);
//     let en' = maybe_extend_env_name(x, term_of_zterm(e1), en);
//     let completes' =
//       extend_complete_list_let(x, t, term_of_zterm(e1), completes);
//     sub_block(
//       [
//         check_let(c, en, t, completes, term_of_zterm(e1))
//           ? Node.text("🟩") : Node.text(""),
//         dom_of_name(x),
//         text(":"),
//         dom_of_term(t),
//         text("←"),
//       ],
//       dom_of_zterm(c, en, completes, e1),
//       dom_of_term(c', en', completes', e2),
//     );
//   | E2Let(x, t, e1, e2) =>
//     let c' = extend_context_name(x, t, c);
//     let en' = maybe_extend_env_name(x, e1, en);
//     let completes' = extend_complete_list_let(x, t, e1, completes);
//     sub_block(
//       [
//         check_let(c, en, t, completes, e1)
//           ? Node.text("🟩") : Node.text(""),
//         dom_of_name(x),
//         text(":"),
//         dom_of_term(t),
//         text("←"),
//       ],
//       dom_of_term(e1),
//       dom_of_zterm(c', en', completes', e2),
//     );
//   };
// };

let string_of_name = (x: name): string =>
  switch (x) {
  | Hole => "Hole"
  | Text(x) => "Text(\"" ++ x ++ "\")"
  };

// let rec string_of_mark = (m: mark): string =>
//   switch (m) {
//   | UnknownVar(x) => "UnknownVar(\"" ++ x ++ "\")"
//   | FunNotArrow(t) => "FunNotArrow(" ++ string_of_pterm(t) ++ ")"
//   | Mismatch(t1, t2) =>
//     " Mismatch(" ++ string_of_pterm(t1) ++ "," ++ string_of_pterm(t2) ++ ")"
//   | NotTyp(t) => "NotTyp(" ++ string_of_pterm(t) ++ ")"
// }

let rec string_of_pterm = (e: pterm): string =>
  switch (e) {
  | Hole => "Hole"
  | Typ => "Typ"
  | Var(x) => "Var(\"" ++ x ++ "\")"
  | Arrow(x, t1, t2) =>
    "Arrow("
    ++ string_of_name(x)
    ++ ","
    ++ string_of_pterm(t1)
    ++ ","
    ++ string_of_pterm(t2)
    ++ ")"
  | Fun(x, t, e) =>
    "Fun("
    ++ string_of_name(x)
    ++ ","
    ++ string_of_pterm(t)
    ++ ","
    ++ string_of_pterm(e)
    ++ ")"
  | Ap(e1, e2) =>
    "Ap(" ++ string_of_pterm(e1) ++ "," ++ string_of_pterm(e2) ++ ")"
  | Let(x, t, e1, e2) =>
    "Let("
    ++ string_of_name(x)
    ++ ","
    ++ string_of_pterm(t)
    ++ ","
    ++ string_of_pterm(e1)
    ++ ","
    ++ string_of_pterm(e2)
    ++ ")"
  };
