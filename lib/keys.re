open Js_of_ocaml.Dom_html;

open Actions;

let action_of_key =
    (shift: bool, control: bool, k: Keyboard_code.t): option(action) => {
  switch (k) {
  | Tab => Some(Edit(NextHole))
  | ArrowUp => Some(Edit(GoUp))
  | ArrowDown => Some(Edit(GoDown))
  | ArrowRight => Some(Edit(GoRight))
  | ArrowLeft => Some(Edit(GoLeft))
  | Backspace => Some(Edit(Backspace))
  | Delete => Some(Edit(Delete))
  | Space => Some(Edit(TextAction))
  | Period => Some(Edit(MakeFun))
  | Equal => Some(Edit(MakeArrow))
  | ShiftLeft
  | ShiftRight => Some(Shift(true))
  | ControlLeft
  | ControlRight => Some(Control(true))
  | KeyA =>
    control
      ? shift ? Some(Edit(FullAuto)) : Some(Edit(Auto))
      : Some(Edit(AddString(shift ? "A" : "a")))
  | KeyB => Some(Edit(AddString(shift ? "B" : "b")))
  | KeyC => Some(Edit(AddString(shift ? "C" : "c")))
  | KeyD => Some(Edit(AddString(shift ? "D" : "d")))
  | KeyE => Some(Edit(AddString(shift ? "E" : "e")))
  | KeyF =>
    control
      ? Some(Edit(FillVar)) : Some(Edit(AddString(shift ? "F" : "f")))
  | KeyG => Some(Edit(AddString(shift ? "G" : "g")))
  | KeyH => Some(Edit(AddString(shift ? "H" : "h")))
  | KeyI => Some(Edit(AddString(shift ? "I" : "i")))
  | KeyJ => Some(Edit(AddString(shift ? "J" : "j")))
  | KeyK => Some(Edit(AddString(shift ? "K" : "k")))
  | KeyL =>
    control
      ? Some(Edit(MakeLemma)) : Some(Edit(AddString(shift ? "L" : "l")))
  | KeyM => Some(Edit(AddString(shift ? "M" : "m")))
  | KeyN => Some(Edit(AddString(shift ? "N" : "n")))
  | KeyO => Some(Edit(AddString(shift ? "O" : "o")))
  | KeyP => Some(Edit(AddString(shift ? "P" : "p")))
  | KeyQ => Some(Edit(AddString(shift ? "Q" : "q")))
  | KeyR =>
    control
      ? Some(Edit(Refine)) : Some(Edit(AddString(shift ? "R" : "r")))
  | KeyS => Some(Edit(AddString(shift ? "S" : "s")))
  | KeyT => Some(Edit(AddString(shift ? "T" : "t")))
  | KeyU => Some(Edit(AddString(shift ? "U" : "u")))
  | KeyV => Some(Edit(AddString(shift ? "V" : "v")))
  | KeyW => Some(Edit(AddString(shift ? "W" : "w")))
  | KeyX => Some(Edit(AddString(shift ? "X" : "x")))
  | KeyY => Some(Edit(AddString(shift ? "Y" : "y")))
  | KeyZ => Some(Edit(AddString(shift ? "Z" : "z")))
  | Digit0 => shift ? Some(Edit(GoUp)) : Some(Edit(AddString("0")))
  | Digit1 => Some(Edit(AddString(shift ? "1" : "1")))
  | Digit2 => Some(Edit(AddString(shift ? "2" : "2")))
  | Digit3 => Some(Edit(AddString(shift ? "3" : "3")))
  | Digit4 => Some(Edit(AddString(shift ? "4" : "4")))
  | Digit5 => Some(Edit(AddString(shift ? "5" : "5")))
  | Digit6 => Some(Edit(AddString(shift ? "6" : "6")))
  | Digit7 => Some(Edit(AddString(shift ? "7" : "7")))
  | Digit8 => Some(Edit(AddString(shift ? "8" : "8")))
  | Digit9 => Some(Edit(AddString(shift ? "9" : "9")))
  | Minus => Some(Edit(AddString(shift ? "_" : "-")))
  | Comma
  | Enter
  | Escape
  | Insert
  | CapsLock
  | BracketLeft
  | BracketRight
  | Semicolon
  | Quote
  | Backquote
  | Backslash
  | Slash
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | Numpad0
  | Numpad1
  | Numpad2
  | Numpad3
  | Numpad4
  | Numpad5
  | Numpad6
  | Numpad7
  | Numpad8
  | Numpad9
  | NumpadMultiply
  | NumpadSubtract
  | NumpadAdd
  | NumpadDecimal
  | NumpadEqual
  | NumpadEnter
  | NumpadDivide
  | NumLock
  | MetaLeft
  | MetaRight
  | AltLeft
  | AltRight
  | PageUp
  | PageDown
  | Home
  | End
  | VolumeMute
  | VolumeDown
  | VolumeUp
  | MediaTrackPrevious
  | MediaTrackNext
  | MediaPlayPause
  | MediaStop
  | ContextMenu
  | BrowserSearch
  | BrowserHome
  | BrowserFavorites
  | BrowserRefresh
  | BrowserStop
  | BrowserForward
  | BrowserBack
  | OSLeft
  | OSRight
  | ScrollLock
  | PrintScreen
  | IntlBackslash
  | IntlYen
  | Pause
  | Unidentified => None
  };
};
