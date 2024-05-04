open Incr_dom;
open Vdom;

let hole: Node.t = {
  Node.div(~attr=Attr.create("class", "hole-div"), []);
};

let cursor_hole: Node.t = {
  Node.div(~attr=Attr.create("class", "cursor-hole-div"), []);
};

let text = (x: string): Node.t => {
  Node.div([Node.text(x)]);
};

let cursor = (a: list(Node.t)): Node.t => {
  Node.div(~attr=Attr.create("class", "cursor-div"), a);
};

let mark = (a: list(Node.t)): Node.t => {
  Node.div(~attr=Attr.create("class", "mark-div"), a);
};

let oneline = (a: list(Node.t)): Node.t => {
  Node.div(~attr=Attr.create("class", "oneline"), a);
};

let block_indent = (a: list(Node.t), b: Node.t): Node.t => {
  Node.div(
    ~attr=Attr.create("class", "block-indent"),
    [
      Node.div(~attr=Attr.create("class", "block-indent-topline"), a),
      Node.div(~attr=Attr.create("class", "block-indent-sideline"), []),
      Node.div(~attr=Attr.create("class", "grid-item"), [b]),
    ],
  );
};

let sub_block = (a: list(Node.t), b: Node.t, c: Node.t): Node.t => {
  Node.div(
    ~attr=Attr.create("class", "sub-block"),
    [
      Node.div(~attr=Attr.create("class", "sub-block-top"), a),
      Node.div(
        ~attr=Attr.create("class", "sub-block-middle"),
        [
          Node.div(~attr=Attr.create("class", "sub-block-sideline"), []),
          Node.div(~attr=Attr.create("class", "sub-block-block"), [b]),
        ],
      ),
      Node.div(~attr=Attr.create("class", "sub-block-bottom"), [c]),
    ],
  );
};
