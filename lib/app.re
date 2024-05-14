// open Core;
open Incr_dom;
open Js_of_ocaml;

open Terms;
open Lang;
open Editor;
open Editor_viz;

open Actions;
open Keys;

module State = {
  type t = unit;
};

module Action = {
  type t = action;

  let sexp_of_t = (a: t): Sexplib0.Sexp.t =>
    switch (a) {
    // | Inc(b) => Sexplib0.Sexp.Atom(string_of_bool(b))
    | Edit(_) => Sexplib0.Sexp.Atom("edit action")
    | Shift(_) => Sexplib0.Sexp.Atom("shift")
    | Control(_) => Sexplib0.Sexp.Atom("control")
    // | UpdateInput(_) => Sexplib0.Sexp.Atom("update text")
    };
};

module Model = {
  type t = {
    exp: state,
    term,
    term_at_cursor: term,
    shift: bool,
    control: bool,
  };
  let cutoff = (_: t, _: t) => false; //t1.state == t2.state;
};

let on_startup = (~schedule_action as _, _) => Async_kernel.return();

let (let+) = (x, f) => {
  Incr.map(x, ~f);
};

let ( let* ) = (x, f) => {
  Incr.bind(x, ~f);
};

let process_state = (exp: state) => {
  let initial_contexts: contexts = {c: [], en: []};
  let p = pterm_of_zterm(exp);
  let term = syn(initial_contexts, term_of_pterm(p));
  let term_at_cursor = term_at_cursor(exp, term);
  (term, term_at_cursor);
};

let apply_action =
    (model: Model.t, action: action, _, ~schedule_action as _): Model.t => {
  switch (action) {
  | Edit(action) =>
    let exp = apply(action, model.exp, model.term_at_cursor);
    let (term, term_at_cursor) = process_state(exp);
    {...model, exp, term, term_at_cursor};
  | Shift(b) => {...model, shift: b}
  | Control(b) => {...model, control: b}
  };
};

let view =
    (model: Incr.t(Model.t), ~inject: action => Ui_effect.t(Base.unit))
    : Ui_incr.t(Vdom.Node.t) => {
  open Vdom;
  let+ model = model;
  Node.div(
    ~attr=
      Attr.many_without_merge([
        Attr.on_keydown(_ev => {
          switch (
            action_of_key(
              model.shift,
              model.control,
              Dom_html.Keyboard_code.of_event(_ev),
            )
          ) {
          | Some(action) => inject(action)
          | None => Ui_effect.Ignore
          }
        }),
        Attr.on_keyup(_ev => {
          switch (Dom_html.Keyboard_code.of_event(_ev)) {
          | ShiftLeft
          | ShiftRight => inject(Shift(false))
          | ControlLeft
          | ControlRight => inject(Control(false))
          | _ => Ui_effect.Ignore
          }
        }),
      ]),
    [
      // counter,
      // Node.div(buttons),
      // input,
      // exp,
      // test,
      dom_of_state(model.exp, model.term, model.term_at_cursor),
    ],
  );
};

let create =
    (
      model: Incr.t(Model.t),
      ~old_model as _,
      ~inject: action => Virtual_dom.Vdom.Effect.t(unit),
    )
    : Incr.t(Component.t(action, Model.t, State.t)) => {
  let* view: Vdom.Node.t = view(model, ~inject);
  let+ model: Model.t = model;
  let apply_action = apply_action(model);
  Component.create(~apply_action, model, view);
};

let initial_model: Model.t = {
  let (term, term_at_cursor) = process_state(initial_state);
  {exp: initial_state, term, term_at_cursor, shift: false, control: false};
};
