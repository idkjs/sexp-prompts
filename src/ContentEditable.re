[@bs.module "./contenteditable.js"] [@react.component]
external make:
  (
    ~html: string,
    ~autofocus: bool,
    ~className: string,
    ~onChange: ReactEvent.Form.t => unit,
    ~onKeyUp: ReactEvent.Keyboard.t => unit,
    ~onKeyDown: ReactEvent.Keyboard.t => unit,
    ~onBlur: ReactEvent.Focus.t => unit,
    ~onFocus: ReactEvent.Focus.t => unit
  ) =>
  React.element =
  "default";
