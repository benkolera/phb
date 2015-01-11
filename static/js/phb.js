// Generated by psc version 0.5.6.2
var PS = PS || {};
PS.Prelude = (function () {
    "use strict";
    function Functor($less$dollar$greater) {
        this["<$>"] = $less$dollar$greater;
    };
    function Apply($less$times$greater, __superclass_Prelude$dotFunctor_0) {
        this["<*>"] = $less$times$greater;
        this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    };
    function Applicative(__superclass_Prelude$dotApply_0, pure) {
        this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
        this.pure = pure;
    };
    function Bind($greater$greater$eq, __superclass_Prelude$dotApply_0) {
        this[">>="] = $greater$greater$eq;
        this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    };
    function Monad(__superclass_Prelude$dotApplicative_0, __superclass_Prelude$dotBind_1) {
        this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
        this["__superclass_Prelude.Bind_1"] = __superclass_Prelude$dotBind_1;
    };
    function cons(e) {  return function(l) {    return [e].concat(l);  };};
    var $greater$greater$eq = function (dict) {
        return dict[">>="];
    };
    var $less$greater = function (dict) {
        return dict["<>"];
    };
    var $less$times$greater = function (dict) {
        return dict["<*>"];
    };
    var $less$dollar$greater = function (dict) {
        return dict["<$>"];
    };
    var $colon = cons;
    var $dollar = function (f) {
        return function (x) {
            return f(x);
        };
    };
    var pure = function (dict) {
        return dict.pure;
    };
    var $$return = function (__dict_Monad_4) {
        return pure(__dict_Monad_4["__superclass_Prelude.Applicative_0"]());
    };
    var liftA1 = function (__dict_Applicative_6) {
        return function (f) {
            return function (a) {
                return $less$times$greater(__dict_Applicative_6["__superclass_Prelude.Apply_0"]())(pure(__dict_Applicative_6)(f))(a);
            };
        };
    };
    var ap = function (__dict_Monad_14) {
        return function (f) {
            return function (a) {
                return $greater$greater$eq(__dict_Monad_14["__superclass_Prelude.Bind_1"]())(f)(function (_2) {
                    return $greater$greater$eq(__dict_Monad_14["__superclass_Prelude.Bind_1"]())(a)(function (_1) {
                        return $$return(__dict_Monad_14)(_2(_1));
                    });
                });
            };
        };
    };
    return {
        Monad: Monad, 
        Bind: Bind, 
        Applicative: Applicative, 
        Apply: Apply, 
        Functor: Functor, 
        "<>": $less$greater, 
        ap: ap, 
        "return": $$return, 
        ">>=": $greater$greater$eq, 
        liftA1: liftA1, 
        pure: pure, 
        "<*>": $less$times$greater, 
        "<$>": $less$dollar$greater, 
        cons: cons, 
        ":": $colon, 
        "$": $dollar
    };
})();
var PS = PS || {};
PS.Data_Function = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function runFn3(fn) {  return function(a) {    return function(b) {      return function(c) {        return fn(a, b, c);      };    };  };};
    return {
        runFn3: runFn3
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function returnE(a) {  return function() {    return a;  };};
    function bindE(a) {  return function(f) {    return function() {      return f(a())();    };  };};
    var applicativeEff = function () {
        return new Prelude.Applicative(applyEff, returnE);
    };
    var applyEff = function () {
        return new Prelude.Apply(Prelude.ap(monadEff()), functorEff);
    };
    var monadEff = function () {
        return new Prelude.Monad(applicativeEff, bindEff);
    };
    var bindEff = function () {
        return new Prelude.Bind(bindE, applyEff);
    };
    var functorEff = function () {
        return new Prelude.Functor(Prelude.liftA1(applicativeEff()));
    };
    return {
        bindE: bindE, 
        returnE: returnE, 
        functorEff: functorEff, 
        applyEff: applyEff, 
        applicativeEff: applicativeEff, 
        bindEff: bindEff, 
        monadEff: monadEff
    };
})();
var PS = PS || {};
PS.PleaseJs = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function makeColor() {
    return Please.make_color()[0];
  };;
    return {
        makeColor: makeColor
    };
})();
var PS = PS || {};
PS.Data_Maybe = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function Nothing() {

    };
    Nothing.value = new Nothing();
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    return {
        Nothing: Nothing, 
        Just: Just
    };
})();
var PS = PS || {};
PS.Data_Array = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function map (f) {  return function (arr) {    var l = arr.length;    var result = new Array(l);    for (var i = 0; i < l; i++) {      result[i] = f(arr[i]);    }    return result;  };};
    var functorArray = function () {
        return new Prelude.Functor(map);
    };
    return {
        map: map, 
        functorArray: functorArray
    };
})();
var PS = PS || {};
PS.Data_Monoid = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var mempty = function (dict) {
        return dict.mempty;
    };
    return {
        mempty: mempty
    };
})();
var PS = PS || {};
PS.Data_Foldable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Monoid = PS.Data_Monoid;
    function Foldable(foldMap, foldl, foldr) {
        this.foldMap = foldMap;
        this.foldl = foldl;
        this.foldr = foldr;
    };
    
  function foldrArray(f) {
    return function(z) {
      return function(xs) {
        var acc = z;
        for (var i = xs.length - 1; i >= 0; --i) {
          acc = f(xs[i])(acc);
        }
        return acc;
      }
    }
  };
    
  function foldlArray(f) {
    return function(z) {
      return function(xs) {
        var acc = z;
        for (var i = 0, len = xs.length; i < len; ++i) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      }
    }
  };
    var foldr = function (dict) {
        return dict.foldr;
    };
    var foldableArray = function () {
        return new Foldable(function (__dict_Monoid_110) {
            return function (f) {
                return function (xs) {
                    return foldr(foldableArray())(function (x) {
                        return function (acc) {
                            return Prelude["<>"](__dict_Monoid_110["__superclass_Prelude.Semigroup_0"]())(f(x))(acc);
                        };
                    })(Data_Monoid.mempty(__dict_Monoid_110))(xs);
                };
            };
        }, function (f) {
            return function (z) {
                return function (xs) {
                    return foldlArray(f)(z)(xs);
                };
            };
        }, function (f) {
            return function (z) {
                return function (xs) {
                    return foldrArray(f)(z)(xs);
                };
            };
        });
    };
    return {
        Foldable: Foldable, 
        foldlArray: foldlArray, 
        foldrArray: foldrArray, 
        foldr: foldr, 
        foldableArray: foldableArray
    };
})();
var PS = PS || {};
PS.Data_Traversable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Array = PS.Data_Array;
    var Data_Foldable = PS.Data_Foldable;
    function Traversable(__superclass_Data$dotFoldable$dotFoldable_1, __superclass_Prelude$dotFunctor_0, sequence, traverse) {
        this["__superclass_Data.Foldable.Foldable_1"] = __superclass_Data$dotFoldable$dotFoldable_1;
        this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
        this.sequence = sequence;
        this.traverse = traverse;
    };
    var traverse = function (dict) {
        return dict.traverse;
    };
    var sequence = function (dict) {
        return dict.sequence;
    };
    var traversableArray = function () {
        return new Traversable(Data_Foldable.foldableArray, Data_Array.functorArray, function (__dict_Applicative_133) {
            return function (_266) {
                if (_266.length === 0) {
                    return Prelude.pure(__dict_Applicative_133)([  ]);
                };
                if (_266.length >= 1) {
                    var _284 = _266.slice(1);
                    return Prelude["<*>"](__dict_Applicative_133["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((__dict_Applicative_133["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude[":"])(_266[0]))(sequence(traversableArray())(__dict_Applicative_133)(_284));
                };
                throw new Error("Failed pattern match");
            };
        }, function (__dict_Applicative_132) {
            return function (_264) {
                return function (_265) {
                    if (_265.length === 0) {
                        return Prelude.pure(__dict_Applicative_132)([  ]);
                    };
                    if (_265.length >= 1) {
                        var _288 = _265.slice(1);
                        return Prelude["<*>"](__dict_Applicative_132["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((__dict_Applicative_132["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude[":"])(_264(_265[0])))(traverse(traversableArray())(__dict_Applicative_132)(_264)(_288));
                    };
                    throw new Error("Failed pattern match");
                };
            };
        });
    };
    return {
        Traversable: Traversable, 
        sequence: sequence, 
        traverse: traverse, 
        traversableArray: traversableArray
    };
})();
var PS = PS || {};
PS.Graphics_Canvas = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Function = PS.Data_Function;
    var Data_Maybe = PS.Data_Maybe;
    function getCanvasElementByIdImpl(id, Just, Nothing) {
    return function() {
      var el = document.getElementById(id);
      if (el && el instanceof HTMLCanvasElement) {
        return Just(el);
      } else {
        return Nothing;
      }
    };
  };
    function getContext2D(c) {  return function() {    return c.getContext('2d');  };};
    var getCanvasElementById = function (elId) {
        return getCanvasElementByIdImpl(elId, Data_Maybe.Just.create, Data_Maybe.Nothing.value);
    };
    return {
        getContext2D: getContext2D, 
        getCanvasElementById: getCanvasElementById
    };
})();
var PS = PS || {};
PS.ChartJs = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    
  function newChart (ctx) {
    return function () {
      return new Chart(ctx);
    }
  }
  ;
    
  function doughnutChart (chart) {
    return function (data) {
      return function (config) {
        return function () {
          return chart.Doughnut(data,config);
        }
      }
    }
  }
  ;
    return {
        doughnutChart: doughnutChart, 
        newChart: newChart
    };
})();
var PS = PS || {};
PS.Phb = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Control_Monad_Eff = PS.Control_Monad_Eff;
    var PleaseJs = PS.PleaseJs;
    var Graphics_Canvas = PS.Graphics_Canvas;
    var Data_Maybe = PS.Data_Maybe;
    var ChartJs = PS.ChartJs;
    var Data_Traversable = PS.Data_Traversable;
    var heartbeatTimebreakdown = function (canvasId) {
        return function (dataz) {
            return function (config) {
                var fillInColor = function (d) {
                    return function __do() {
                        var _15 = PleaseJs.makeColor();
                        return {
                            label: d.label, 
                            value: d.value, 
                            color: _15, 
                            highlight: _15
                        };
                    };
                };
                return Prelude[">>="](Control_Monad_Eff.bindEff())(Graphics_Canvas.getCanvasElementById(canvasId))(function (_19) {
                    if (_19 instanceof Data_Maybe.Just) {
                        return function __do() {
                            var _18 = Graphics_Canvas.getContext2D(_19.value0)();
                            var _17 = ChartJs.newChart(_18)();
                            var _16 = Data_Traversable.traverse(Data_Traversable.traversableArray())(Control_Monad_Eff.applicativeEff())(fillInColor)(dataz)();
                            return ChartJs.doughnutChart(_17)(_16)(config)();
                        };
                    };
                    throw new Error("Failed pattern match");
                });
            };
        };
    };
    return {
        heartbeatTimebreakdown: heartbeatTimebreakdown
    };
})();
