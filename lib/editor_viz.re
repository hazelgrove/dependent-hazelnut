open Incr_dom;
open Vdom;

open Terms;
open Lang;
open Lang_viz;

let dom_of_state = (z: zexp) => {
  // let typ = type_of(exp_of_zexp(z));
  // let type_info =
  //   switch (typ) {
  //   | None => Node.text(":(")
  //   | Some(t) => dom_of_typ(t)
  //   };
  // let complete_info =
  //   switch (complete_zexp(z), typ) {
  //   | (true, Some(_)) => Node.text("🟩")
  //   | (false, Some(t)) =>
  //     complete_typ(t) ? Node.text("❓") : Node.text("")
  //   | _ => Node.text("")
  //   };
  let (marked_e, _) = syn([], exp_of_zexp(z));
  let merged_z = mark_merge(z, marked_e);
  let exp_info = dom_of_zexp([], [], merged_z);
  let context_info = doms_of_context(local_context([], z));
  let goal_info = dom_of_typ(local_goal([], Hole, z));
  let mark_info = doms_of_marks(local_marks(merged_z));
  let mark_info =
    List.length(mark_info) > 0 ? [Node.br()] @ mark_info @ [Node.hr()] : [];
  Node.div(
    ~attr=Attr.create("class", "code-display"),
    [
      Node.div(
        ~attr=Attr.create("class", "context-display"),
        [
          Node.div([
            // type_info, complete_info, Node.hr(),
            exp_info,
          ]),
          Node.div(
            ~attr=Attr.create("class", "context-div"),
            mark_info
            @ [
              Node.text("Goal"),
              Node.hr(),
              goal_info,
              Node.hr(),
              Node.text("Context"),
              Node.hr(),
            ]
            @ context_info,
          ),
        ],
      ),
    ],
  );
};
