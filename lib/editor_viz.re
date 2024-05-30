open Incr_dom;
open Vdom;

open Terms;
open Lang;
open Lang_viz;

let dom_of_state = (z: zterm, e: term, e': term) => {
  let i = get_info(e');

  let cursed_e = place_cursor(z, e);
  let e_dom = dom_of_term(cursed_e);

  let ctx_dom = doms_of_context(i.ctx);
  let goal_dom =
    switch (i.goal) {
    | None => Node.Text("-")
    | Some(goal) =>
      dom_of_term(
        ~parens_info={...default_parens_info, no_parens: true},
        goal,
      )
    };
  let syn_dom =
    switch (i.syn) {
    | None => Node.Text("-")
    | Some(syn) =>
      dom_of_term(
        ~parens_info={...default_parens_info, no_parens: true},
        syn,
      )
    };
  let rec top_marks = z =>
    switch (z) {
    | Mark(r) => [r.m, ...top_marks(r.e)]
    | _ => []
    };
  let mark_doms = doms_of_marks(top_marks(e'));
  let mark_dom =
    List.length(mark_doms) > 0 ? [Node.br()] @ mark_doms @ [Node.hr()] : [];

  Node.div(
    ~attr=Attr.create("class", "code-display"),
    [
      Node.div(~attr=Attr.create("class", "exp-div"), [e_dom]),
      Node.div(
        ~attr=Attr.create("class", "context-display"),
        [
          Node.div(
            ~attr=Attr.create("class", "context-div"),
            mark_dom
            @ [
              Node.text("Goal"),
              Node.hr(),
              goal_dom,
              Node.hr(),
              Node.text("Found"),
              Node.hr(),
              syn_dom,
              Node.hr(),
              Node.text("Context"),
              Node.hr(),
            ]
            @ ctx_dom,
          ),
        ],
      ),
    ],
  );
};
