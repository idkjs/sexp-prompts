// open ReasonReact;

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

// let component = reducerComponent("Terminal");
// [@react.component]
module EvelInstance =
  Eval.Make({
    type t = (state, action);
    let clear = dispatch => ClearBuffer |> dispatch;
    let (<<) = (dispatch, text) => AppendBuffer(text, "output") |> dispatch;
    let (>>) = (Eval.Prompt(dispatch, prompt), callback) =>
      Prompt(prompt, callback) |> dispatch;
    let (<~) = (dispatch, (name, body)) => Define(name, body) |> dispatch;
    let (%) = (dispatch, name) => state.mods |> StringMap.find(name);
    let has = (dispatch, name) => state.mods |> StringMap.mem(name);
  });

module Label = {
  [@react.component]
  let make = (~value, ~clazz) => {
    <div className={clazz |> String.concat("")}>
      {value |> React.string}
    </div>;
  };
};
  let execute = (state,dispatch) =>{
    switch (state.prompt) {
    | None =>
      switch (EvelInstance.eval(state, [], state.minibuffer)) {
      | Eval.Result(exp) => AppendBuffer(exp, "result") |> dispatch
      | Eval.Error(exp) => AppendBuffer(exp, "error") |> dispatch
      }
    | Some({handler}) => handler(state.minibuffer)
    };
    dispatch(Execute);};
let reducer = (state, action) =>
  switch (action) {
  | ClearBuffer => {...state, buffer: []}
  | AppendBuffer(data, source) => {
      ...state,
      buffer: [{data, source, time: Js.Date.make()}, ...state.buffer],
    }
  | Update(minibuffer) => {...state, minibuffer}
  | Execute =>{
        ...state,
        buffer: [
          {data: state.minibuffer, source: "input", time: Js.Date.make()},
          ...state.buffer,
        ],
        minibuffer: SExp.empty,
        prompt: None,
      }
    // UpdateWithSideEffects(
    //   {
    //     ...state,
    //     buffer: [
    //       {data: state.minibuffer, source: "input", time: Js.Date.make()},
    //       ...state.buffer,
    //     ],
    //     minibuffer: SExp.empty,
    //     prompt: None,
    //   },
    //   self =>
    //     switch (state.prompt) {
    //     | None =>
    //       switch (EvelInstance.eval(self, [], state.minibuffer)) {
    //       | Eval.Result(exp) => AppendBuffer(exp, "result") |> self.send
    //       | Eval.Error(exp) => AppendBuffer(exp, "error") |> self.send
    //       }
    //     | Some({handler}) => handler(state.minibuffer)
    //     },
    // )
  | Prompt(indicator, handler) => {
      ...state,
      prompt: Some({indicator, handler}),
    }
  | Define(name, body) => {
      ...state,
      mods: state.mods |> StringMap.add(name, body),
    }
  };
let initialState = {
  buffer: [],
  mods: StringMap.empty,
  minibuffer: SExp.empty,
  prompt: None,
};
[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState);
  let execute = () => execute(state, dispatch)
  // let execute = () =>{
  //   switch (state.prompt) {
  //   | None =>
  //     switch (EvelInstance.eval(state, [], state.minibuffer)) {
  //     | Eval.Result(exp) => AppendBuffer(exp, "result") |> dispatch
  //     | Eval.Error(exp) => AppendBuffer(exp, "error") |> dispatch
  //     }
  //   | Some({handler}) => handler(state.minibuffer)
  //   };
  //   dispatch(Execute);};
  let {buffer, minibuffer, prompt} = state;
  let length = buffer |> List.length;
  <div className="terminal">
    <div className="buffer">
      {buffer
       |> List.mapi((i, {data: datax, source, time}) =>
            <div
              className={"log " ++ source} key={length - i |> string_of_int}>
              <Label clazz=["time"] value={time |> Js.Date.toLocaleString} />
              <Label clazz=["source"] value={datax |> SExp.toString} />
              <SExpViewer data=datax />
            </div>
          )
       |> Array.of_list
       |> React.array}
    </div>
    <div className="mini-buffer">
      {switch (prompt) {
       | Some({indicator}) => <Label clazz=["indicator"] value=indicator />
       | _ => React.null
       }}
      <SExpEditor
        data=minibuffer
        onUpdate={data => Update(data) |> dispatch}
      />
      <button onClick={_ => execute()}>
        {"eval" |> React.string}
      </button>
    </div>
  </div>;
};
