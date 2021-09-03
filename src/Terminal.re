type line = {
  data: SExp.t,
  source: string,
  time: Js.Date.t,
};

type prompt = {
  indicator: string,
  handler: SExp.t => unit,
};

module StringMap = Map.Make(String);

type state = {
  buffer: list(line),
  mods: StringMap.t(SExp.t),
  minibuffer: SExp.t,
  prompt: option(prompt),
};

type action =
  | ClearBuffer
  | AppendBuffer(SExp.t, string)
  | Prompt(string, SExp.t => unit)
  | Define(string, SExp.t)
  | Execute
  | Update(SExp.t);
// module EvelInstance =
//   Eval.Make(
//     {
//       type t = self(state, noRetainedProps, action);
//       let clear = self => ClearBuffer |> self.send;
//       let (<<) = (self, text) => AppendBuffer(text, "output") |> self.send;
//       let (>>) = (Eval.Prompt(self, prompt), callback) =>
//         Prompt(prompt, callback) |> self.send;
//       let (<~) = (self, (name, body)) => Define(name, body) |> self.send;
//       let (%) = (self, name) =>  self.state.mods |> StringMap.find(name);
//       let has = (self, name) =>  self.state.mods |> StringMap.mem(name);
//     },
//   );
module EvalInstance =
  Eval.Make({
    type t = ReactCompat.self(state, action);
    let clear = (self: t) => ClearBuffer |> self.send;
    let (<<) = (self: t, text) => AppendBuffer(text, "output") |> self.send;
    let (>>) = (Eval.Prompt(self: t, prompt), callback) =>
      Prompt(prompt, callback) |> self.send;
    let (<~) = (self: t, (name, body)) => Define(name, body) |> self.send;
    let (%) = (self: t, name) => self.state.mods |> StringMap.find(name);
    let has = (self: t, name) => self.state.mods |> StringMap.mem(name);
  });

module Label = {
  [@react.component]
  let make = (~value, ~clazz, ()) =>
    ReactCompat.useRecordApi({
      ...ReactCompat.component,

      render: _self =>
        <div className={clazz |> String.concat("")}>
          {value |> React.string}
        </div>,
    });
};

[@react.component]
let make = () =>
  ReactCompat.useRecordApi({
    ...ReactCompat.component,

    initialState: () => {
      buffer: [],
      mods: StringMap.empty,
      minibuffer: SExp.empty,
      prompt: None,
    },
    reducer: (action, state) =>
      switch (action) {
      | ClearBuffer => Update({...state, buffer: []})
      | AppendBuffer(data, source) =>
        Update({
          ...state,
          buffer: [{data, source, time: Js.Date.make()}, ...state.buffer],
        })
      | Update(minibuffer) => Update({...state, minibuffer})
      | Execute =>
        UpdateWithSideEffects(
          {
            ...state,
            buffer: [
              {data: state.minibuffer, source: "input", time: Js.Date.make()},
              ...state.buffer,
            ],
            minibuffer: SExp.empty,
            prompt: None,
          },
          self =>
            switch (state.prompt) {
            | None =>
              switch (EvalInstance.eval(self, [], state.minibuffer)) {
              | Eval.Result(exp) => AppendBuffer(exp, "result") |> self.send
              | Eval.Error(exp) => AppendBuffer(exp, "error") |> self.send
              }
            | Some({handler}) => handler(state.minibuffer)
            },
        )
      | Prompt(indicator, handler) =>
        Update({...state, prompt: Some({indicator, handler})})
      | Define(name, body) =>
        Update({...state, mods: state.mods |> StringMap.add(name, body)})
      },
    render: self => {
      let {buffer, minibuffer, prompt} = self.state;
      let length = buffer |> List.length;
      <div className="terminal">
        <div className="buffer">
          {buffer
           |> List.mapi((i, {data: datax, source, time}) =>
                <div
                  className={"log " ++ source}
                  key={length - i |> string_of_int}>
                  <Label
                    clazz=["time"]
                    value={time |> Js.Date.toLocaleString}
                  />
                  <Label clazz=["source"] value={datax |> SExp.toString} />
                  <SExpViewer data=datax />
                </div>
              )
           |> Array.of_list
           |> React.array}
        </div>
        <div className="mini-buffer">
          {switch (prompt) {
           | Some({indicator}) =>
             <Label clazz=["indicator"] value=indicator />
           | _ => React.null
           }}
          <SExpEditor
            data=minibuffer
            onUpdate={data => Update(data) |> self.send}
          />
          <button onClick={_ => Execute |> self.send}>
            {"eval" |> React.string}
          </button>
        </div>
      </div>;
    },
  });
