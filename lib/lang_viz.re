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
  | Hole =>
    if (cursed) {
      cursor_hole;
    } else {
      text("_");
    }
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
  no_parens: bool,
};
let default_parens_info = {
  arrow_assoc: false,
  fun_assoc: false,
  ap_assoc: false,
  no_parens: false,
};

let parens = dom => oneline([text("("), dom, text(")")]);

let rec dom_of_term =
        (
          ~under_cursor=false,
          ~inline=false,
          ~parens_info: parens_info=default_parens_info,
          (i, e): info_term,
        )
        : Node.t => {
  let hideable = ((i, e): info_term): bool => {
    !(i.cursed || i.cursor_inside) && complete(sterm_of_term((i, e)));
  };
  let dom =
    switch (e) {
    | Hole when i.cursed => cursor_hole
    | e when i.cursed && !under_cursor =>
      cursor([dom_of_term(~under_cursor=true, ~inline, (i, e))])
    | Hole => hole
    | Typ => text("◻")
    | Mark(_, e) =>
      mark([
        dom_of_term(
          ~inline,
          ~parens_info={...default_parens_info, no_parens: true},
          e,
        ),
      ])
    | Var(x, _) =>
      // I think this is buggy, the context is wrong
      // let rec check_shadowed = (idx, ctx) =>
      //   switch (ctx) {
      //   | [] => false
      //   | [c, ..._] when c.x == Text(r.x) => r.idx != Some(idx)
      //   | [_, ...ctx] => check_shadowed(idx + 1, ctx)
      //   };
      // let string_of_idx =
      //   switch (r.idx) {
      //   | None => ""
      //   | Some(idx) =>
      //     if (check_shadowed(0, r.i.ctx)) {
      //       "." ++ string_of_int(idx);
      //     } else {
      //       "";
      //     }
      //   };
      // let out_of_scope_message =
      //   switch (r.idx) {
      //   | None => ""
      //   | Some(idx) =>
      //     switch (List.nth(r.i.ctx, idx).x) {
      //     | Hole => " (out of scope)"
      //     | _ => ""
      //     }
      //   };
      text(text_of_text(x)) // ++ string_of_idx);
    | Arrow(x, t1, t2) =>
      let dom = {
        let binding =
          switch (x) {
          | Hole when !(i.name_cursed || i.cursed) => []
          | _ => [dom_of_name(x, ~cursed=i.name_cursed), text(":")]
          };
        oneline(
          binding
          @ [
            dom_of_term(~inline=true, t1),
            text("⇒"),
            dom_of_term(
              ~inline=true,
              ~parens_info={...default_parens_info, arrow_assoc: true},
              t2,
            ),
          ],
        );
      };
      if (parens_info.arrow_assoc || under_cursor || parens_info.no_parens) {
        dom;
      } else {
        parens(dom);
      };
    | Fun(x, t, e) =>
      let dom1 = [
        dom_of_name(x, ~cursed=i.name_cursed),
        text(":"),
        dom_of_term(~inline=true, t),
        text("→"),
      ];
      let dom2 =
        dom_of_term(
          ~inline,
          ~parens_info={...default_parens_info, fun_assoc: true},
          e,
        );
      if (inline) {
        let dom = oneline(dom1 @ [dom2]);
        if (parens_info.fun_assoc || under_cursor || parens_info.no_parens) {
          dom;
        } else {
          parens(dom);
        };
      } else {
        block_indent(dom1, dom2);
      };
    | Ap(
        (i2, Ap((i3, Var("exists", _)) as v, t1)),
        (i4, Fun(Text(x), t2, body)),
      )
        when
          terms_equal(sterm_of_term(t1), sterm_of_term(t2))
          && !(
               i.cursed
               || i2.cursed
               || i3.cursed
               || i4.cursed
               || i4.name_cursed
             )
          && hideable(t2) =>
      oneline([
        text("("),
        dom_of_term(v),
        dom_of_name(Text(x), ~cursed=i3.name_cursed),
        text(":"),
        dom_of_term(t1),
        text("."),
        dom_of_term(~inline=true, body),
        text(")"),
      ])
    | Ap((i2, Ap((i3, Ap((i4, Var("eq", _)) as v, t1)), t2)), t3)
        when !(i2.cursed || i3.cursed || i4.cursed) && hideable(t1) =>
      oneline([
        text("("),
        dom_of_term(t2),
        dom_of_term(v),
        // text("["),
        // dom_of_term(t1),
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
    | Ap(
        (
          i2,
          Ap(
            (i3, Ap((i4, Ap((i5, Var("nat-ind", _)) as v, e1)), scrut)),
            e2,
          ),
        ),
        (fi1, Fun(x, t1, (fi2, Fun(y, t2, body)))),
      )
        when
          hideable(t1)
          && hideable(t2)
          && !fi1.cursed
          && !fi2.cursed
          && !fi1.name_cursed
          && !fi2.name_cursed
          && !i.cursed
          && !i2.cursed
          && !i3.cursed
          && !i4.cursed
          && !i5.cursed =>
      block_indent(
        [
          dom_of_term(v),
          text(" "),
          dom_of_term(e1, ~inline=true),
          text(" @ "),
          dom_of_term(scrut, ~inline=true),
        ],
        Node.div([
          block_indent([text("Z"), text("→")], dom_of_term(e2)),
          block_indent(
            [
              text("S" ++ " "),
              dom_of_name(x),
              text("→"),
              dom_of_name(y),
              text("→"),
            ],
            dom_of_term(body),
          ),
        ]),
      )
    | Ap(
        (
          _,
          Ap(
            (
              _,
              Ap(
                (
                  _,
                  Ap(
                    (
                      _,
                      Ap(
                        (
                          _,
                          Ap(
                            (
                              _,
                              Ap(
                                (
                                  _,
                                  Ap((_, Var("eq-step", _)) as v, hide1),
                                ),
                                hide2,
                              ),
                            ),
                            ea,
                          ),
                        ),
                        hide3,
                      ),
                    ),
                    (_, Fun(_, _, body)) as hide4,
                  ),
                ),
                ec,
              ),
            ),
            ee1,
          ),
        ),
        ee2,
      )
        // | Ap({
        //     i: i1,
        //     e1:
        //       Ap({
        //         i: _, //i2,
        //         e1:
        //           Ap({
        //             i: _, //i3,
        //             e1:
        //               Ap({
        //                 i: _, //i4,
        //                 e1:
        //                   Ap({
        //                     i: _, //i5,
        //                     e1:
        //                       Ap({
        //                         i: _, //i6,
        //                         e1:
        //                           Ap({
        //                             i: _, //i7,
        //                             e1:
        //                               Ap({
        //                                 i: _, //i8,
        //                                 e1: Var(r),
        //                                 e2: hide1,
        //                               }),
        //                             e2: hide2,
        //                           }),
        //                         e2: ea,
        //                       }),
        //                     e2: hide3,
        //                   }),
        //                 e2: Fun({i: _, x: _, t: _, e: body}) as hide4,
        //               }),
        //             e2: ec,
        //           }),
        //         e2: ee1,
        //       }),
        //     e2: ee2,
        //   })
        when
          (
            !i.cursor_inside
            || fst(ea).cursor_inside
            || fst(ea).cursed
            || fst(ee1).cursor_inside
            || fst(ee1).cursed
            // || fst(body).cursor_inside
            // || fst(body).cursed
            || fst(ee2).cursor_inside
            || fst(ee2).cursed
          )
          && hideable(hide1)
          && hideable(hide2)
          && hideable(hide3)
          && hideable(hide4) =>
      // && !get_info(t1).cursor_inside
      // && !get_info(t2).cursor_inside
      // && !get_info(t1).cursed
      // && !get_info(t2).cursed
      // && !fi1.cursed
      // && !fi2.cursed
      // && !fi1.name_cursed
      // && !fi2.name_cursed
      // && !i2.cursed
      // && !i3.cursed
      let highlighted_ea =
        switch (body) {
        | (_, Var(_)) => ea // for trivial congruence, don't highlight the rewritten subterm
        | _ => ({...fst(ea), highlighted: true}, snd(ea))
        };
      let fa = Lang.beta_sub(highlighted_ea, body);
      let eq =
        switch (ee1) {
        | Ap({
            i,
            e1:
              Ap({
                i: _,
                e1:
                  Ap({
                    i: _,
                    e1: Ap({i: _, e1: Var(r), e2: hide1}),
                    e2: hide2,
                  }),
                e2: hide3,
              }),
            e2: inner_eq,
          })
            when
              r.x == "sym"
              && (
                !(i.cursed || i.cursor_inside)
                || get_info(inner_eq).cursor_inside
                || get_info(inner_eq).cursed
              )
              && hideable(hide1)
              && hideable(hide2)
              && hideable(hide3) =>
          Ap({i, e1: Var(r), e2: inner_eq})
        | _ => ee1
        };
      let eq =
        dom_of_term(
          eq,
          ~inline=true,
          ~parens_info={...default_parens_info, no_parens: true},
        );
      let rest =
        switch (ee2) {
        | Ap({i: _, e1: Ap({i: _, e1: Var(r), e2: _}), e2: _})
            when r.x == "refl" && hideable(ee2) =>
          dom_of_term(ec)
        | _ => dom_of_term(ee2)
        };
      Node.div([
        oneline([
          dom_of_term(fa, ~inline=true),
          text("=⟨"),
          eq,
          text("⟩"),
        ]),
        Node.br(),
        rest,
      ]);
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
      if (parens_info.ap_assoc || under_cursor || parens_info.no_parens) {
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
  if (i.highlighted && !under_cursor) {
    highlight([dom]);
  } else {
    dom;
  };
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
  | Inconsistent => failwith("unselectable")
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
