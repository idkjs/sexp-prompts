let expr =
  SExp.parse(
    "(module test (define (quote main) (println (string (Hello, () world)))))",
  );

module SyncView = {
  open ReasonReact;
  type state = {exp: SExp.t};
  type action =
    | Update(SExp.t);
  let reducer = (_state, action) =>
    switch (action) {
    | Update(exp) => {exp: exp}
    };
  [@react.component]
  let make = (~data: SExp.t) => {
    let (state, dispatch) = React.useReducer(reducer, {exp: data});

    <div className="SyncView">
      <SExpEditor data={state.exp} onUpdate={x => Update(x) |> dispatch} />
      <pre> {state.exp |> SExp.toString |> React.string} </pre>
    </div>;
  };
};

/* ReactDOMRe.renderToElementWithId(<SyncView data=expr />, "app"); */
ReactDOMRe.renderToElementWithId(<Terminal />, "app");
