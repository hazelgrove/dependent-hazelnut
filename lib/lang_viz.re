open Incr_dom;
open Vdom;
open Terms;
open Lang;
open Web;

let string_aliases = [
  ("exists", "∃"),
  ("exists-con", "∃-con"),
  ("exists-rec", "∃-rec"),
  ("eq", "="),
];

let text_of_text = (x: string) =>
  switch (List.assoc_opt(x, string_aliases)) {
  | Some(y) => y
  | None => x
  };

let dom_of_name = (~cursed: bool=false, x: name): Node.t =>
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

type parens_info = {
  arrow_assoc: bool,
  fun_assoc: bool,
  ap_assoc: bool,
};
let default_parens_info = {
  arrow_assoc: false,
  fun_assoc: false,
  ap_assoc: false,
};

let parens = dom => oneline([text("("), dom, text(")")]);

let rec dom_of_term =
        (
          ~under_cursor=false,
          ~inline=false,
          ~parens_info: parens_info=default_parens_info,
          e: term,
        )
        : Node.t =>
  switch (e) {
  | Hole(r) when r.i.cursed => cursor_hole
  | e when get_info(e).cursed && !under_cursor =>
    cursor([dom_of_term(~under_cursor=true, ~inline, e)])
  | Hole(_) => hole
  | Typ(_) => text("◻")
  | Mark(r) => mark([dom_of_term(~inline, r.e)])
  | Var(r) =>
    let rec check_shadowed = (idx, ctx) =>
      switch (ctx) {
      | [] => false
      | [c, ..._] when c.x == Text(r.x) => r.idx != Some(idx)
      | [_, ...ctx] => check_shadowed(idx + 1, ctx)
      };
    let string_of_idx = idx =>
      switch (idx) {
      | None => ""
      | Some(idx) =>
        if (check_shadowed(0, r.i.ctx)) {
          "." ++ string_of_int(idx);
        } else {
          "";
        }
      };
    text(text_of_text(r.x) ++ string_of_idx(r.idx));
  | Arrow(r) =>
    let dom = {
      let binding =
        switch (r.x) {
        | Hole when !(r.i.name_cursed || r.i.cursed) => []
        | _ => [dom_of_name(r.x, ~cursed=r.i.name_cursed), text(":")]
        };
      oneline(
        binding
        @ [
          dom_of_term(~inline=true, r.t1),
          text("⇒"),
          dom_of_term(
            ~inline=true,
            ~parens_info={...default_parens_info, arrow_assoc: true},
            r.t2,
          ),
        ],
      );
    };
    if (parens_info.arrow_assoc || under_cursor) {
      dom;
    } else {
      parens(dom);
    };
  | Fun(r) =>
    let dom1 = [
      dom_of_name(r.x, ~cursed=r.i.name_cursed),
      text(":"),
      dom_of_term(~inline=true, r.t),
      text("→"),
    ];
    let dom2 =
      dom_of_term(
        ~inline,
        ~parens_info={...default_parens_info, fun_assoc: true},
        r.e,
      );
    if (inline) {
      let dom = oneline(dom1 @ [dom2]);
      if (parens_info.fun_assoc || under_cursor) {
        dom;
      } else {
        parens(dom);
      };
    } else {
      block_indent(dom1, dom2);
    };
  | Ap({
      i: i1,
      e1: Ap({i: i2, e1: Var(r), e2: t1}),
      e2: Fun({i: i3, x: Text(x), t: t2, e: body}),
    })
      when
        r.x == "exists"
        && terms_equal(t1, t2)
        && !(
             i1.cursed
             || i2.cursed
             || r.i.cursed
             || i3.cursed
             || i3.name_cursed
             || get_info(t2).cursed
             || get_info(t2).cursor_inside
           ) =>
    oneline([
      text("("),
      dom_of_term(Var(r)),
      dom_of_name(Text(x), ~cursed=i3.name_cursed),
      text(":"),
      dom_of_term(t1),
      text("."),
      dom_of_term(~inline=true, body),
      text(")"),
    ])
  | Ap({
      i: _,
      e1: Ap({i: i2, e1: Ap({i: i3, e1: Var(r), e2: t1}), e2: t2}),
      e2: t3,
    })
      when
        r.x == "eq"
        && !(
             //  i1.cursed ||
             i2.cursed
             || i3.cursed
             || r.i.cursed
             || get_info(t1).cursed
             || get_info(t1).cursor_inside
           ) =>
    oneline([
      text("("),
      dom_of_term(t2),
      dom_of_term(Var(r)),
      // text("["),
      // dom_of_term(c, en, completes, t1),
      // text("]"),
      dom_of_term(t3),
      text(")"),
    ])
  // | Ap(Ap(Var("refl"), _), e) =>
  //   oneline([
  //     dom_of_term(c, en, completes, Var("refl")),
  //     text("("),
  //     dom_of_term(c, en, completes, e),
  //     text(")"),
  //   ])
  | Ap(r) =>
    let dom =
      oneline([
        dom_of_term(
          ~inline=true,
          ~parens_info={...default_parens_info, ap_assoc: true},
          r.e1,
        ),
        text(" "),
        dom_of_term(~inline=true, r.e2),
      ]);
    if (parens_info.ap_assoc || under_cursor) {
      dom;
    } else {
      parens(dom);
    };
  | Let(r) =>
    sub_block(
      [
        // check_let(c, en, t, completes, e1)
        //   ? Node.text("🟩") :
        Node.text(""),
        dom_of_name(r.x, ~cursed=r.i.name_cursed),
        text(":"),
        dom_of_term(r.t),
        text("←"),
      ],
      dom_of_term(r.e1),
      dom_of_term(r.e2),
    )
  };

let doms_of_context = (ctx: context): list(Node.t) => {
  let dom_of_entry = (r): Node.t => {
    let content =
      switch (r.e) {
      | None
      | Some(Hole(_)) => []
      | Some(_) => [text(" = ...")]
      };
    Node.div(
      ~attr=Attr.create("class", "context-entry"),
      [
        oneline([dom_of_name(r.x), text(":"), dom_of_term(r.t)] @ content),
      ],
    );
  };
  List.map(dom_of_entry, ctx);
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
      Node.text("Cannot apply term of non-function type "),
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
