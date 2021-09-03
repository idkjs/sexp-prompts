// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Bytes = require("bs-platform/lib/js/bytes.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_bytes = require("bs-platform/lib/js/caml_bytes.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

function toString(str) {
  if (str.TAG === /* Atom */0) {
    return str._0;
  }
  var join = function (param) {
    if (!param) {
      return "";
    }
    var tail = param.tl;
    var only = param.hd;
    if (tail) {
      return toString(only) + (" " + join(tail));
    } else {
      return toString(only);
    }
  };
  return "(" + (join(str._0) + ")");
}

function to_list(s) {
  var _acc = /* [] */0;
  var _i = s.length - 1 | 0;
  while(true) {
    var i = _i;
    var acc = _acc;
    if (i === -1) {
      return acc;
    }
    _i = i - 1 | 0;
    _acc = {
      hd: Caml_string.get(s, i),
      tl: acc
    };
    continue ;
  };
}

function iteri(f, l) {
  var _i = 0;
  var _param = l;
  while(true) {
    var param = _param;
    var i = _i;
    if (!param) {
      return ;
    }
    Curry._2(f, i, param.hd);
    _param = param.tl;
    _i = i + 1 | 0;
    continue ;
  };
}

function of_list(xs) {
  var l = List.length(xs);
  var s = Caml_bytes.caml_create_bytes(l);
  iteri((function (i, c) {
          return Caml_bytes.set(s, i, c);
        }), xs);
  return Bytes.unsafe_to_string(s);
}

function getCharCategory(ch) {
  if (!(ch > 13 || ch < 9)) {
    if (ch === 12 || ch === 11) {
      return /* Normal */{
              _0: ch
            };
    } else {
      return /* Space */2;
    }
  }
  switch (ch) {
    case 32 :
        return /* Space */2;
    case 33 :
    case 34 :
    case 35 :
    case 36 :
    case 37 :
    case 38 :
    case 39 :
        return /* Normal */{
                _0: ch
              };
    case 40 :
        return /* LeftBracket */0;
    case 41 :
        return /* RightBracket */1;
    default:
      return /* Normal */{
              _0: ch
            };
  }
}

function tokenizer(input) {
  var list = List.map(getCharCategory, to_list(input));
  var proc = function (param) {
    if (!param) {
      return /* [] */0;
    }
    var ch = param.hd;
    if (typeof ch === "number") {
      switch (ch) {
        case /* LeftBracket */0 :
            return {
                    hd: {
                      TAG: /* Bracket */0,
                      _0: true
                    },
                    tl: proc(param.tl)
                  };
        case /* RightBracket */1 :
            return {
                    hd: {
                      TAG: /* Bracket */0,
                      _0: false
                    },
                    tl: proc(param.tl)
                  };
        case /* Space */2 :
            var eat = function (_list) {
              while(true) {
                var list = _list;
                if (!list) {
                  return list;
                }
                var match = list.hd;
                if (typeof match !== "number") {
                  return list;
                }
                if (match < 2) {
                  return list;
                }
                _list = list.tl;
                continue ;
              };
            };
            return {
                    hd: /* Spaces */0,
                    tl: proc(eat(param.tl))
                  };
        
      }
    } else {
      var feed = function (_text, _next) {
        while(true) {
          var next = _next;
          var text = _text;
          if (!next) {
            return [
                    List.rev(text),
                    next
                  ];
          }
          var ch = next.hd;
          if (typeof ch === "number") {
            return [
                    List.rev(text),
                    next
                  ];
          }
          _next = next.tl;
          _text = {
            hd: ch._0,
            tl: text
          };
          continue ;
        };
      };
      var match = feed({
            hd: ch._0,
            tl: /* [] */0
          }, param.tl);
      return {
              hd: {
                TAG: /* Entity */1,
                _0: of_list(match[0])
              },
              tl: proc(match[1])
            };
    }
  };
  return proc(list);
}

var ParseFailed = /* @__PURE__ */Caml_exceptions.create("SExp.ParseFailed");

function parseList(input) {
  var proc = function (_prev, _param) {
    while(true) {
      var param = _param;
      var prev = _prev;
      if (param) {
        var str = param.hd;
        if (typeof str === "number") {
          _param = param.tl;
          continue ;
        }
        if (str.TAG === /* Bracket */0) {
          if (!str._0) {
            return [
                    List.rev(prev),
                    param.tl
                  ];
          }
          var match = proc(/* [] */0, param.tl);
          _param = match[1];
          _prev = {
            hd: {
              TAG: /* List */1,
              _0: match[0]
            },
            tl: prev
          };
          continue ;
        }
        _param = param.tl;
        _prev = {
          hd: {
            TAG: /* Atom */0,
            _0: str._0
          },
          tl: prev
        };
        continue ;
      } else {
        throw {
              RE_EXN_ID: ParseFailed,
              Error: new Error()
            };
      }
    };
  };
  return proc(/* [] */0, input);
}

function parse(input) {
  var valid = function (_param) {
    while(true) {
      var param = _param;
      if (!param) {
        return /* [] */0;
      }
      var match = param.hd;
      if (typeof match === "number") {
        _param = param.tl;
        continue ;
      }
      if (match.TAG === /* Bracket */0) {
        if (match._0) {
          return param.tl;
        }
        _param = param.tl;
        continue ;
      }
      _param = param.tl;
      continue ;
    };
  };
  var match = parseList(valid(tokenizer($$String.trim(input))));
  return {
          TAG: /* List */1,
          _0: match[0]
        };
}

var empty = {
  TAG: /* List */1,
  _0: /* [] */0
};

exports.empty = empty;
exports.toString = toString;
exports.parse = parse;
/* No side effect */