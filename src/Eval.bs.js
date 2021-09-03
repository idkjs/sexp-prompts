// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var $$Map = require("bs-platform/lib/js/map.js");
var List = require("bs-platform/lib/js/list.js");
var SExp = require("./SExp.bs.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

var Imposible = /* @__PURE__ */Caml_exceptions.create("Eval.Imposible");

var Invalid = /* @__PURE__ */Caml_exceptions.create("Eval.Invalid");

var $great$eq$great = List.rev_append;

function partitionList(num, env) {
  var _prev = /* [] */0;
  var _param = [
    num,
    List.rev(env)
  ];
  while(true) {
    var param = _param;
    var prev = _prev;
    var n = param[0];
    if (n === 0) {
      return [
              prev,
              List.rev(param[1])
            ];
    }
    var match = param[1];
    if (match) {
      _param = [
        n - 1 | 0,
        match.tl
      ];
      _prev = {
        hd: match.hd,
        tl: prev
      };
      continue ;
    }
    throw {
          RE_EXN_ID: Invalid,
          Error: new Error()
        };
  };
}

function isValid(text) {
  return /(?:-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?)|(?:true|false)|null/g.test(text);
}

function isOperator(text) {
  return /\+|-|\*|\/|<|>|&&|\|\|/g.test(text);
}

var jseval = (return eval(a+op+b)+'');

var isTrue = (return eval(x) ? a : b;);

function Make(Ctx) {
  $$Map.Make({
        compare: $$String.compare
      });
  var evalList = function (ctx, env, list) {
    var err = List.fold_left((function (p, a) {
            if (p.TAG !== /* Result */0) {
              return p;
            }
            var list = p._0;
            if (list.TAG !== /* Atom */0 && a.TAG === /* Result */0) {
              return {
                      TAG: /* Result */0,
                      _0: {
                        TAG: /* List */1,
                        _0: {
                          hd: a._0,
                          tl: list._0
                        }
                      }
                    };
            }
            if (a.TAG === /* Result */0) {
              return {
                      TAG: /* Error */1,
                      _0: {
                        TAG: /* Atom */0,
                        _0: "InvalidEval"
                      }
                    };
            } else {
              return a;
            }
          }), {
          TAG: /* Result */0,
          _0: {
            TAG: /* List */1,
            _0: /* [] */0
          }
        }, List.map((function (param) {
                return $$eval(ctx, env, param);
              }), list));
    if (err.TAG !== /* Result */0) {
      return err;
    }
    var list$1 = err._0;
    if (list$1.TAG !== /* Atom */0) {
      return {
              TAG: /* Result */0,
              _0: {
                TAG: /* List */1,
                _0: List.rev(list$1._0)
              }
            };
    }
    throw {
          RE_EXN_ID: Invalid,
          Error: new Error()
        };
  };
  var $$eval = function (ctx, env, _src) {
    while(true) {
      var src = _src;
      if (src.TAG === /* Atom */0) {
        var name = src._0;
        if (isValid(name)) {
          return {
                  TAG: /* Result */0,
                  _0: src
                };
        } else if (List.mem_assoc(name, env)) {
          return {
                  TAG: /* Result */0,
                  _0: List.assoc(name, env)
                };
        } else if (Curry._2(Ctx.has, ctx, name)) {
          return {
                  TAG: /* Result */0,
                  _0: Curry._2(Ctx.$percent, ctx, name)
                };
        } else {
          return {
                  TAG: /* Error */1,
                  _0: {
                    TAG: /* List */1,
                    _0: {
                      hd: {
                        TAG: /* Atom */0,
                        _0: "SymbolNotFound"
                      },
                      tl: {
                        hd: {
                          TAG: /* Atom */0,
                          _0: name
                        },
                        tl: /* [] */0
                      }
                    }
                  }
                };
        }
      }
      var match = src._0;
      if (!match) {
        return {
                TAG: /* Result */0,
                _0: src
              };
      }
      var sp = match.hd;
      if (sp.TAG === /* Atom */0) {
        var sp$1 = sp._0;
        var exit = 0;
        var exit$1 = 0;
        switch (sp$1) {
          case "clear" :
              if (match.tl) {
                exit$1 = 3;
              } else {
                Curry._1(Ctx.clear, ctx);
                return {
                        TAG: /* Result */0,
                        _0: SExp.empty
                      };
              }
              break;
          case "debug" :
              var rst = evalList(ctx, env, match.tl);
              if (rst.TAG !== /* Result */0) {
                return rst;
              }
              Curry._2(Ctx.$less$less, ctx, rst._0);
              return {
                      TAG: /* Result */0,
                      _0: SExp.empty
                    };
          case "dump" :
              if (!match.tl) {
                return {
                        TAG: /* Result */0,
                        _0: {
                          TAG: /* List */1,
                          _0: List.map((function (param) {
                                  return {
                                          TAG: /* List */1,
                                          _0: {
                                            hd: {
                                              TAG: /* Atom */0,
                                              _0: param[0]
                                            },
                                            tl: {
                                              hd: param[1],
                                              tl: /* [] */0
                                            }
                                          }
                                        };
                                }), env)
                        }
                      };
              }
              exit$1 = 3;
              break;
          case "eval" :
              return evalList(ctx, env, match.tl);
          case "if" :
              var match$1 = match.tl;
              if (match$1) {
                var match$2 = match$1.tl;
                if (match$2) {
                  var match$3 = match$2.tl;
                  if (match$3) {
                    if (!match$3.tl) {
                      var cond = match$1.hd;
                      var match$4 = $$eval(ctx, env, cond);
                      var exit$2 = 0;
                      if (match$4.TAG === /* Result */0) {
                        var rst$1 = match$4._0;
                        if (rst$1.TAG === /* Atom */0) {
                          var rst$2 = rst$1._0;
                          if (isValid(rst$2)) {
                            _src = Curry._3(isTrue, rst$2, match$2.hd, match$3.hd);
                            continue ;
                          }
                          exit$2 = 4;
                        } else {
                          exit$2 = 4;
                        }
                      } else {
                        exit$2 = 4;
                      }
                      if (exit$2 === 4) {
                        return {
                                TAG: /* Error */1,
                                _0: {
                                  TAG: /* List */1,
                                  _0: {
                                    hd: {
                                      TAG: /* Atom */0,
                                      _0: "InvalidCond"
                                    },
                                    tl: {
                                      hd: cond,
                                      tl: /* [] */0
                                    }
                                  }
                                }
                              };
                      }
                      
                    }
                    
                  } else {
                    exit$1 = 3;
                  }
                }
                
              }
              break;
          case "let" :
              var match$5 = match.tl;
              if (match$5) {
                var vars = match$5.hd;
                if (vars.TAG === /* Atom */0) {
                  exit$1 = 3;
                } else {
                  var loop = function (_prev, _param) {
                    while(true) {
                      var param = _param;
                      var prev = _prev;
                      if (!param) {
                        return prev;
                      }
                      var match = param.hd;
                      if (match.TAG === /* Atom */0) {
                        throw {
                              RE_EXN_ID: Invalid,
                              Error: new Error()
                            };
                      }
                      var match$1 = match._0;
                      if (match$1) {
                        var key = match$1.hd;
                        if (key.TAG === /* Atom */0) {
                          var match$2 = match$1.tl;
                          if (match$2) {
                            if (match$2.tl) {
                              throw {
                                    RE_EXN_ID: Invalid,
                                    Error: new Error()
                                  };
                            }
                            var nval = $$eval(ctx, env, match$2.hd);
                            if (nval.TAG === /* Result */0) {
                              _param = param.tl;
                              _prev = {
                                hd: [
                                  key._0,
                                  nval._0
                                ],
                                tl: prev
                              };
                              continue ;
                            }
                            throw {
                                  RE_EXN_ID: Invalid,
                                  Error: new Error()
                                };
                          }
                          throw {
                                RE_EXN_ID: Invalid,
                                Error: new Error()
                              };
                        }
                        throw {
                              RE_EXN_ID: Invalid,
                              Error: new Error()
                            };
                      }
                      throw {
                            RE_EXN_ID: Invalid,
                            Error: new Error()
                          };
                    };
                  };
                  var exit$3 = 0;
                  var nenv;
                  try {
                    nenv = loop(env, vars._0);
                    exit$3 = 4;
                  }
                  catch (raw_exn){
                    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                    if (exn.RE_EXN_ID === Invalid) {
                      return {
                              TAG: /* Error */1,
                              _0: {
                                TAG: /* Atom */0,
                                _0: "InvalidLet"
                              }
                            };
                    }
                    throw exn;
                  }
                  if (exit$3 === 4) {
                    var _param = match$5.tl;
                    while(true) {
                      var param = _param;
                      if (!param) {
                        return {
                                TAG: /* Error */1,
                                _0: {
                                  TAG: /* Atom */0,
                                  _0: "InvalidLetBody"
                                }
                              };
                      }
                      var tl = param.tl;
                      var only = param.hd;
                      if (!tl) {
                        return $$eval(ctx, nenv, only);
                      }
                      $$eval(ctx, nenv, only);
                      _param = tl;
                      continue ;
                    };
                  }
                  
                }
              }
              break;
          case "quote" :
              var match$6 = match.tl;
              if (match$6) {
                if (!match$6.tl) {
                  return {
                          TAG: /* Result */0,
                          _0: match$6.hd
                        };
                }
                exit$1 = 3;
              }
              break;
          case "string" :
              var match$7 = match.tl;
              if (match$7) {
                var list = match$7.hd;
                if (list.TAG === /* Atom */0) {
                  exit$1 = 3;
                } else {
                  if (!match$7.tl) {
                    return {
                            TAG: /* Result */0,
                            _0: {
                              TAG: /* Atom */0,
                              _0: $$String.concat("", List.map((function (value) {
                                          if (value.TAG === /* Atom */0) {
                                            return value._0;
                                          } else if (value._0) {
                                            return "";
                                          } else {
                                            return " ";
                                          }
                                        }), list._0))
                            }
                          };
                  }
                  exit$1 = 3;
                }
              }
              break;
          default:
            exit$1 = 3;
        }
        if (exit$1 === 3) {
          var match$8 = match.tl;
          if (match$8) {
            var match$9 = match$8.tl;
            if (match$9 && !match$9.tl) {
              var b = match$9.hd;
              var a = match$8.hd;
              if (isOperator(sp$1)) {
                var proc = (function(a){
                return function proc(fn, x) {
                  var err = $$eval(ctx, env, x);
                  if (err.TAG !== /* Result */0) {
                    return {
                            TAG: /* Error */1,
                            _0: {
                              TAG: /* List */1,
                              _0: {
                                hd: {
                                  TAG: /* Atom */0,
                                  _0: "ErrorInsideOp"
                                },
                                tl: {
                                  hd: err._0,
                                  tl: /* [] */0
                                }
                              }
                            }
                          };
                  }
                  var xv = err._0;
                  if (xv.TAG === /* Atom */0) {
                    var xv$1 = xv._0;
                    if (isValid(xv$1)) {
                      return Curry._1(fn, xv$1);
                    }
                    
                  }
                  return {
                          TAG: /* Error */1,
                          _0: {
                            TAG: /* List */1,
                            _0: {
                              hd: {
                                TAG: /* Atom */0,
                                _0: "FailedToConvert"
                              },
                              tl: {
                                hd: a,
                                tl: /* [] */0
                              }
                            }
                          }
                        };
                }
                }(a));
                return proc((function(sp$1,b){
                          return function (av) {
                            return proc((function (bv) {
                                          return {
                                                  TAG: /* Result */0,
                                                  _0: {
                                                    TAG: /* Atom */0,
                                                    _0: Curry._3(jseval, sp$1, av, bv)
                                                  }
                                                };
                                        }), b);
                          }
                          }(sp$1,b)), a);
              }
              exit = 2;
            } else {
              exit = 2;
            }
          }
          
        }
        if (exit === 2) {
          switch (sp$1) {
            case "define" :
                var match$10 = match.tl;
                var name$1 = match$10.hd;
                if (name$1.TAG === /* Atom */0) {
                  var match$11 = match$10.tl;
                  if (match$11 && !match$11.tl) {
                    var name$2 = name$1._0;
                    var rst$3 = $$eval(ctx, env, match$11.hd);
                    if (rst$3.TAG !== /* Result */0) {
                      return rst$3;
                    }
                    var rst$4 = rst$3._0;
                    Curry._2(Ctx.$less$tilde, ctx, [
                          name$2,
                          rst$4
                        ]);
                    return {
                            TAG: /* Result */0,
                            _0: {
                              TAG: /* List */1,
                              _0: {
                                hd: {
                                  TAG: /* Atom */0,
                                  _0: "defined"
                                },
                                tl: {
                                  hd: {
                                    TAG: /* List */1,
                                    _0: {
                                      hd: {
                                        TAG: /* Atom */0,
                                        _0: "quote"
                                      },
                                      tl: {
                                        hd: {
                                          TAG: /* Atom */0,
                                          _0: name$2
                                        },
                                        tl: /* [] */0
                                      }
                                    }
                                  },
                                  tl: {
                                    hd: rst$4,
                                    tl: /* [] */0
                                  }
                                }
                              }
                            }
                          };
                  }
                  
                }
                break;
            case "defun" :
                var match$12 = match.tl;
                var name$3 = match$12.hd;
                if (name$3.TAG === /* Atom */0) {
                  var match$13 = match$12.tl;
                  if (match$13) {
                    var params = match$13.hd;
                    if (params.TAG !== /* Atom */0) {
                      _src = {
                        TAG: /* List */1,
                        _0: {
                          hd: {
                            TAG: /* Atom */0,
                            _0: "define"
                          },
                          tl: {
                            hd: {
                              TAG: /* Atom */0,
                              _0: name$3._0
                            },
                            tl: {
                              hd: {
                                TAG: /* List */1,
                                _0: {
                                  hd: {
                                    TAG: /* Atom */0,
                                    _0: "fun"
                                  },
                                  tl: {
                                    hd: params,
                                    tl: match$13.tl
                                  }
                                }
                              },
                              tl: /* [] */0
                            }
                          }
                        }
                      };
                      continue ;
                    }
                    
                  }
                  
                }
                break;
            case "fun" :
                var match$14 = match.tl;
                var params$1 = match$14.hd;
                if (params$1.TAG !== /* Atom */0) {
                  var body = match$14.tl;
                  var exit$4 = 0;
                  if (body) {
                    var match$15 = body.hd;
                    if (match$15.TAG === /* Atom */0) {
                      exit$4 = 3;
                    } else {
                      var match$16 = match$15._0;
                      if (match$16) {
                        var match$17 = match$16.hd;
                        if (match$17.TAG === /* Atom */0 && match$17._0 === "let") {
                          var match$18 = match$16.tl;
                          if (match$18 && match$18.hd.TAG !== /* Atom */0) {
                            if (!body.tl) {
                              return {
                                      TAG: /* Result */0,
                                      _0: src
                                    };
                            }
                            exit$4 = 3;
                          } else {
                            exit$4 = 3;
                          }
                        } else {
                          exit$4 = 3;
                        }
                      } else {
                        exit$4 = 3;
                      }
                    }
                  } else {
                    exit$4 = 3;
                  }
                  if (exit$4 === 3) {
                    return {
                            TAG: /* Result */0,
                            _0: {
                              TAG: /* List */1,
                              _0: {
                                hd: {
                                  TAG: /* Atom */0,
                                  _0: "fun"
                                },
                                tl: {
                                  hd: {
                                    TAG: /* List */1,
                                    _0: params$1._0
                                  },
                                  tl: {
                                    hd: {
                                      TAG: /* List */1,
                                      _0: {
                                        hd: {
                                          TAG: /* Atom */0,
                                          _0: "let"
                                        },
                                        tl: {
                                          hd: {
                                            TAG: /* List */1,
                                            _0: List.map((function (param) {
                                                    return {
                                                            TAG: /* List */1,
                                                            _0: {
                                                              hd: {
                                                                TAG: /* Atom */0,
                                                                _0: param[0]
                                                              },
                                                              tl: {
                                                                hd: param[1],
                                                                tl: /* [] */0
                                                              }
                                                            }
                                                          };
                                                  }), env)
                                          },
                                          tl: body
                                        }
                                      }
                                    },
                                    tl: /* [] */0
                                  }
                                }
                              }
                            }
                          };
                  }
                  
                }
                break;
            default:
              
          }
        }
        var next = match.tl;
        if (List.mem_assoc(sp$1, env)) {
          _src = {
            TAG: /* List */1,
            _0: {
              hd: List.assoc(sp$1, env),
              tl: next
            }
          };
          continue ;
        }
        if (!Curry._2(Ctx.has, ctx, sp$1)) {
          return {
                  TAG: /* Error */1,
                  _0: {
                    TAG: /* List */1,
                    _0: {
                      hd: {
                        TAG: /* Atom */0,
                        _0: "UnknownCommand"
                      },
                      tl: {
                        hd: src,
                        tl: /* [] */0
                      }
                    }
                  }
                };
        }
        _src = {
          TAG: /* List */1,
          _0: {
            hd: Curry._2(Ctx.$percent, ctx, sp$1),
            tl: next
          }
        };
        continue ;
      }
      var match$19 = sp._0;
      if (match$19) {
        var match$20 = match$19.hd;
        if (match$20.TAG === /* Atom */0 && match$20._0 === "fun") {
          var match$21 = match$19.tl;
          if (match$21) {
            var params$2 = match$21.hd;
            if (params$2.TAG !== /* Atom */0) {
              var real = match.tl;
              var body$1 = match$21.tl;
              var params$3 = params$2._0;
              if (List.length(real) > List.length(params$3)) {
                var match$22 = partitionList(List.length(real) - List.length(params$3) | 0, real);
                _src = {
                  TAG: /* List */1,
                  _0: {
                    hd: {
                      TAG: /* List */1,
                      _0: {
                        hd: sp,
                        tl: match$22[1]
                      }
                    },
                    tl: match$22[0]
                  }
                };
                continue ;
              }
              var mkCore = (function(body$1){
              return function mkCore(prev) {
                if (body$1) {
                  var match = body$1.hd;
                  if (match.TAG !== /* Atom */0) {
                    var match$1 = match._0;
                    if (match$1) {
                      var match$2 = match$1.hd;
                      if (match$2.TAG === /* Atom */0 && match$2._0 === "let") {
                        var match$3 = match$1.tl;
                        if (match$3) {
                          var list = match$3.hd;
                          if (list.TAG !== /* Atom */0 && !body$1.tl) {
                            return {
                                    TAG: /* List */1,
                                    _0: {
                                      hd: {
                                        TAG: /* Atom */0,
                                        _0: "let"
                                      },
                                      tl: {
                                        hd: {
                                          TAG: /* List */1,
                                          _0: List.rev_append(List.map((function (param) {
                                                      return {
                                                              TAG: /* List */1,
                                                              _0: {
                                                                hd: {
                                                                  TAG: /* Atom */0,
                                                                  _0: param[0]
                                                                },
                                                                tl: {
                                                                  hd: param[1],
                                                                  tl: /* [] */0
                                                                }
                                                              }
                                                            };
                                                    }), prev), list._0)
                                        },
                                        tl: match$3.tl
                                      }
                                    }
                                  };
                          }
                          
                        }
                        
                      }
                      
                    }
                    
                  }
                  
                }
                return {
                        TAG: /* List */1,
                        _0: {
                          hd: {
                            TAG: /* Atom */0,
                            _0: "let"
                          },
                          tl: {
                            hd: {
                              TAG: /* List */1,
                              _0: List.map((function (param) {
                                      return {
                                              TAG: /* List */1,
                                              _0: {
                                                hd: {
                                                  TAG: /* Atom */0,
                                                  _0: param[0]
                                                },
                                                tl: {
                                                  hd: param[1],
                                                  tl: /* [] */0
                                                }
                                              }
                                            };
                                    }), prev)
                            },
                            tl: body$1
                          }
                        }
                      };
              }
              }(body$1));
              var _prev = /* [] */0;
              var _param$1 = [
                params$3,
                real
              ];
              while(true) {
                var param$1 = _param$1;
                var prev = _prev;
                var list$1 = param$1[0];
                if (!list$1) {
                  if (param$1[1]) {
                    return {
                            TAG: /* Error */1,
                            _0: {
                              TAG: /* Atom */0,
                              _0: "InvalidFunction"
                            }
                          };
                  } else {
                    return $$eval(ctx, env, mkCore(prev));
                  }
                }
                var name$4 = list$1.hd;
                if (name$4.TAG === /* Atom */0) {
                  var match$23 = param$1[1];
                  if (match$23) {
                    _param$1 = [
                      list$1.tl,
                      match$23.tl
                    ];
                    _prev = {
                      hd: [
                        name$4._0,
                        match$23.hd
                      ],
                      tl: prev
                    };
                    continue ;
                  }
                  
                }
                if (param$1[1]) {
                  return {
                          TAG: /* Error */1,
                          _0: {
                            TAG: /* Atom */0,
                            _0: "InvalidFunction"
                          }
                        };
                } else {
                  return {
                          TAG: /* Result */0,
                          _0: {
                            TAG: /* List */1,
                            _0: {
                              hd: {
                                TAG: /* Atom */0,
                                _0: "fun"
                              },
                              tl: {
                                hd: {
                                  TAG: /* List */1,
                                  _0: list$1
                                },
                                tl: {
                                  hd: mkCore(prev),
                                  tl: /* [] */0
                                }
                              }
                            }
                          }
                        };
                }
              };
            }
            
          }
          
        }
        
      }
      var rst$5 = $$eval(ctx, env, sp);
      if (rst$5.TAG !== /* Result */0) {
        return rst$5;
      }
      _src = {
        TAG: /* List */1,
        _0: {
          hd: rst$5._0,
          tl: match.tl
        }
      };
      continue ;
    };
  };
  return {
          $$eval: $$eval
        };
}

exports.Imposible = Imposible;
exports.Invalid = Invalid;
exports.$great$eq$great = $great$eq$great;
exports.partitionList = partitionList;
exports.isValid = isValid;
exports.isOperator = isOperator;
exports.jseval = jseval;
exports.isTrue = isTrue;
exports.Make = Make;
/* jseval Not a pure module */