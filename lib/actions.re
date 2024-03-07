open Editor;

type action =
  | Edit(edit_action)
  | Shift(bool)
  | Control(bool);
