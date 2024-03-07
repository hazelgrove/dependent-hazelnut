open Incr_dom;
open Vdom;
open Terms;
open Lang;
open Web;

let dom_of_name = (x: name): Node.t =>
  switch (x) {
  | Hole => hole
  | Text(x) => text(x)
  };

let rec dom_of_typ = (t: typ): Node.t =>
  switch (t) {
  | Hole => hole
  | Base(x) => text(x)
  | Arrow(t1, t2) =>
    oneline([
      text("("),
      dom_of_typ(t1),
      text("⇒"),
      dom_of_typ(t2),
      text(")"),
    ])
  };

let dom_of_mark = (m: mark): Node.t =>
  switch (m) {
  | UnknownVar(x) => Node.text("Unrecognized variable " ++ x)
  | FunNotArrow(t) =>
    oneline([
      Node.text("Cannot apply term of non-funciton type "),
      dom_of_typ(t),
    ])
  | Mismatch(t1, t2) =>
    oneline([
      Node.text("Expected type "),
      dom_of_typ(t1),
      Node.text(" but found inconsistent type "),
      dom_of_typ(t2),
    ])
  };

let doms_of_marks = (ms: list(mark)): list(Node.t) => {
  let dom_of_entry = (m: mark): Node.t => {
    Node.div(~attr=Attr.create("class", "mark-entry"), [dom_of_mark(m)]);
  };
  List.map(dom_of_entry, ms);
};

let rec dom_of_exp = (c: context, completes: list(string), e: exp): Node.t =>
  switch (e) {
  | Hole => hole
  | Mark(_, e) => mark([dom_of_exp(c, completes, e)])
  | Var(x) => text(x)
  | Fun(x, t, e) =>
    let c' = extend_context_name(x, t, c);
    let completes' =
      complete_typ(t) ? extend_list_name(x, completes) : completes;
    block_indent(
      [dom_of_name(x), text(":"), dom_of_typ(t), text("→")],
      dom_of_exp(c', completes', e),
    );
  | Ap(e1, e2) =>
    oneline([
      text("("),
      dom_of_exp(c, completes, e1),
      text(")("),
      dom_of_exp(c, completes, e2),
      text(")"),
    ])
  | Let(x, t, e1, e2) =>
    let c' = extend_context_name(x, t, c);
    let completes' =
      complete_typ(t) && complete_exp(completes, e)
        ? extend_list_name(x, completes) : completes;
    sub_block(
      [
        check_let(c, t, completes, e1) ? Node.text("🟩") : Node.text(""),
        dom_of_name(x),
        text(":"),
        dom_of_typ(t),
        text("←"),
      ],
      dom_of_exp(c, completes, e1),
      dom_of_exp(c', completes', e2),
    );
  };

let doms_of_context = (c: context): list(Node.t) => {
  let dom_of_entry = ((x: string, t: typ)): Node.t => {
    Node.div(
      ~attr=Attr.create("class", "context-entry"),
      [oneline([text(x), text(":"), dom_of_typ(t)])],
    );
  };
  List.map(dom_of_entry, c);
};

let dom_of_zname = (z: zname): Node.t =>
  switch (z) {
  | Cursor(Hole) => cursor_hole
  | Cursor(x) => cursor([dom_of_name(x)])
  };

let rec dom_of_ztyp = (z: ztyp): Node.t =>
  switch (z) {
  | Cursor(Hole) => cursor_hole
  | Cursor(t) => cursor([dom_of_typ(t)])
  | LArrow(z, t) =>
    oneline([
      text("("),
      dom_of_ztyp(z),
      Node.text("⇒"),
      dom_of_typ(t),
      text(")"),
    ])
  | RArrow(t, z) =>
    oneline([
      text("("),
      dom_of_typ(t),
      Node.text("⇒"),
      dom_of_ztyp(z),
      text(")"),
    ])
  };

let rec dom_of_zexp = (c: context, completes: list(string), z: zexp): Node.t => {
  switch (z) {
  | Cursor(Hole) => cursor_hole
  | Mark(_, z) => mark([dom_of_zexp(c, completes, z)])
  | Cursor(e) => cursor([dom_of_exp(c, completes, e)])
  | XFun(z, t, e) =>
    let c' = extend_context_name(name_of_zname(z), t, c);
    let completes' =
      extend_complete_list_fun(name_of_zname(z), t, completes);
    block_indent(
      [dom_of_zname(z), text(":"), dom_of_typ(t), text("→")],
      dom_of_exp(c', completes', e),
    );
  | TFun(x, z, e) =>
    let c' = extend_context_name(x, typ_of_ztyp(z), c);
    let completes' = extend_complete_list_fun(x, typ_of_ztyp(z), completes);
    block_indent(
      [dom_of_name(x), text(":"), dom_of_ztyp(z), text("→")],
      dom_of_exp(c', completes', e),
    );
  | EFun(x, t, z) =>
    let c' = extend_context_name(x, t, c);
    let completes' = extend_complete_list_fun(x, t, completes);
    block_indent(
      [dom_of_name(x), text(":"), dom_of_typ(t), text("→")],
      dom_of_zexp(c', completes', z),
    );
  | LAp(z, e) =>
    oneline([
      text("("),
      dom_of_zexp(c, completes, z),
      text(")("),
      dom_of_exp(c, completes, e),
      text(")"),
    ])
  | RAp(e, z) =>
    oneline([
      text("("),
      dom_of_exp(c, completes, e),
      text(")("),
      dom_of_zexp(c, completes, z),
      text(")"),
    ])
  | XLet(x, t, e1, e2) =>
    let c' = extend_context_name(name_of_zname(x), t, c);
    let completes' =
      extend_complete_list_let(name_of_zname(x), t, e1, completes);
    sub_block(
      [
        check_let(c, t, completes, e1) ? Node.text("🟩") : Node.text(""),
        dom_of_zname(x),
        text(":"),
        dom_of_typ(t),
        text("←"),
      ],
      dom_of_exp(c, completes, e1),
      dom_of_exp(c', completes', e2),
    );
  | TLet(x, t, e1, e2) =>
    let c' = extend_context_name(x, typ_of_ztyp(t), c);
    let completes' =
      extend_complete_list_let(x, typ_of_ztyp(t), e1, completes);
    sub_block(
      [
        check_let(c, typ_of_ztyp(t), completes, e1)
          ? Node.text("🟩") : Node.text(""),
        dom_of_name(x),
        text(":"),
        dom_of_ztyp(t),
        text("←"),
      ],
      dom_of_exp(c, completes, e1),
      dom_of_exp(c', completes', e2),
    );
  | E1Let(x, t, e1, e2) =>
    let c' = extend_context_name(x, t, c);
    let completes' =
      extend_complete_list_let(x, t, exp_of_zexp(e1), completes);
    sub_block(
      [
        check_let(c, t, completes, exp_of_zexp(e1))
          ? Node.text("🟩") : Node.text(""),
        dom_of_name(x),
        text(":"),
        dom_of_typ(t),
        text("←"),
      ],
      dom_of_zexp(c, completes, e1),
      dom_of_exp(c', completes', e2),
    );
  | E2Let(x, t, e1, e2) =>
    let c' = extend_context_name(x, t, c);
    let completes' = extend_complete_list_let(x, t, e1, completes);
    sub_block(
      [
        check_let(c, t, completes, e1) ? Node.text("🟩") : Node.text(""),
        dom_of_name(x),
        text(":"),
        dom_of_typ(t),
        text("←"),
      ],
      dom_of_exp(c, completes, e1),
      dom_of_zexp(c', completes', e2),
    );
  };
};
