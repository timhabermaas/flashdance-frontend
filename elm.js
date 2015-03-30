var Elm = Elm || { Native: {} };
Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values)
   return _elm.Array.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Array",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Array = Elm.Native.Array.make(_elm);
   var append = $Native$Array.append;
   var length = $Native$Array.length;
   var isEmpty = function (array) {
      return _U.eq(length(array),
      0);
   };
   var slice = $Native$Array.slice;
   var set = $Native$Array.set;
   var get = F2(function (i,
   array) {
      return _U.cmp(0,
      i) < 1 && _U.cmp(i,
      $Native$Array.length(array)) < 0 ? $Maybe.Just(A2($Native$Array.get,
      i,
      array)) : $Maybe.Nothing;
   });
   var push = $Native$Array.push;
   var empty = $Native$Array.empty;
   var filter = F2(function (isOkay,
   arr) {
      return function () {
         var update = F2(function (x,
         xs) {
            return isOkay(x) ? A2($Native$Array.push,
            x,
            xs) : xs;
         });
         return A3($Native$Array.foldl,
         update,
         $Native$Array.empty,
         arr);
      }();
   });
   var foldr = $Native$Array.foldr;
   var foldl = $Native$Array.foldl;
   var indexedMap = $Native$Array.indexedMap;
   var map = $Native$Array.map;
   var toIndexedList = function (array) {
      return A3($List.map2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      _L.range(0,
      $Native$Array.length(array) - 1),
      $Native$Array.toList(array));
   };
   var toList = $Native$Array.toList;
   var fromList = $Native$Array.fromList;
   var initialize = $Native$Array.initialize;
   var repeat = F2(function (n,e) {
      return A2(initialize,
      n,
      $Basics.always(e));
   });
   var Array = {ctor: "Array"};
   _elm.Array.values = {_op: _op
                       ,empty: empty
                       ,repeat: repeat
                       ,initialize: initialize
                       ,fromList: fromList
                       ,isEmpty: isEmpty
                       ,length: length
                       ,push: push
                       ,append: append
                       ,get: get
                       ,set: set
                       ,slice: slice
                       ,toList: toList
                       ,toIndexedList: toIndexedList
                       ,map: map
                       ,indexedMap: indexedMap
                       ,filter: filter
                       ,foldl: foldl
                       ,foldr: foldr};
   return _elm.Array.values;
};
Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values)
   return _elm.Basics.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Basics",
   $Native$Basics = Elm.Native.Basics.make(_elm),
   $Native$Show = Elm.Native.Show.make(_elm),
   $Native$Utils = Elm.Native.Utils.make(_elm);
   var uncurry = F2(function (f,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2": return A2(f,
              _v0._0,
              _v0._1);}
         _U.badCase($moduleName,
         "on line 595, column 3 to 8");
      }();
   });
   var curry = F3(function (f,
   a,
   b) {
      return f({ctor: "_Tuple2"
               ,_0: a
               ,_1: b});
   });
   var flip = F3(function (f,b,a) {
      return A2(f,a,b);
   });
   var snd = function (_v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2": return _v4._1;}
         _U.badCase($moduleName,
         "on line 573, column 3 to 4");
      }();
   };
   var fst = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2": return _v8._0;}
         _U.badCase($moduleName,
         "on line 567, column 3 to 4");
      }();
   };
   var always = F2(function (a,
   _v12) {
      return function () {
         return a;
      }();
   });
   var identity = function (x) {
      return x;
   };
   _op["<|"] = F2(function (f,x) {
      return f(x);
   });
   _op["|>"] = F2(function (x,f) {
      return f(x);
   });
   _op[">>"] = F3(function (f,
   g,
   x) {
      return g(f(x));
   });
   _op["<<"] = F3(function (g,
   f,
   x) {
      return g(f(x));
   });
   _op["++"] = $Native$Utils.append;
   var toString = $Native$Show.toString;
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var otherwise = true;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {
      return t;
   };
   _elm.Basics.values = {_op: _op
                        ,max: max
                        ,min: min
                        ,compare: compare
                        ,not: not
                        ,xor: xor
                        ,otherwise: otherwise
                        ,rem: rem
                        ,negate: negate
                        ,abs: abs
                        ,sqrt: sqrt
                        ,clamp: clamp
                        ,logBase: logBase
                        ,e: e
                        ,pi: pi
                        ,cos: cos
                        ,sin: sin
                        ,tan: tan
                        ,acos: acos
                        ,asin: asin
                        ,atan: atan
                        ,atan2: atan2
                        ,round: round
                        ,floor: floor
                        ,ceiling: ceiling
                        ,truncate: truncate
                        ,toFloat: toFloat
                        ,degrees: degrees
                        ,radians: radians
                        ,turns: turns
                        ,toPolar: toPolar
                        ,fromPolar: fromPolar
                        ,isNaN: isNaN
                        ,isInfinite: isInfinite
                        ,toString: toString
                        ,fst: fst
                        ,snd: snd
                        ,identity: identity
                        ,always: always
                        ,flip: flip
                        ,curry: curry
                        ,uncurry: uncurry
                        ,LT: LT
                        ,EQ: EQ
                        ,GT: GT};
   return _elm.Basics.values;
};
Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values)
   return _elm.Char.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Char",
   $Basics = Elm.Basics.make(_elm),
   $Native$Char = Elm.Native.Char.make(_elm);
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isBetween = F3(function (low,
   high,
   $char) {
      return function () {
         var code = toCode($char);
         return _U.cmp(code,
         toCode(low)) > -1 && _U.cmp(code,
         toCode(high)) < 1;
      }();
   });
   var isUpper = A2(isBetween,
   _U.chr("A"),
   _U.chr("Z"));
   var isLower = A2(isBetween,
   _U.chr("a"),
   _U.chr("z"));
   var isDigit = A2(isBetween,
   _U.chr("0"),
   _U.chr("9"));
   var isOctDigit = A2(isBetween,
   _U.chr("0"),
   _U.chr("7"));
   var isHexDigit = function ($char) {
      return isDigit($char) || (A3(isBetween,
      _U.chr("a"),
      _U.chr("f"),
      $char) || A3(isBetween,
      _U.chr("A"),
      _U.chr("F"),
      $char));
   };
   _elm.Char.values = {_op: _op
                      ,isUpper: isUpper
                      ,isLower: isLower
                      ,isDigit: isDigit
                      ,isOctDigit: isOctDigit
                      ,isHexDigit: isHexDigit
                      ,toUpper: toUpper
                      ,toLower: toLower
                      ,toLocaleUpper: toLocaleUpper
                      ,toLocaleLower: toLocaleLower
                      ,toCode: toCode
                      ,fromCode: fromCode};
   return _elm.Char.values;
};
Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values)
   return _elm.Color.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Color",
   $Basics = Elm.Basics.make(_elm);
   var Radial = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "Radial"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var radial = Radial;
   var Linear = F3(function (a,
   b,
   c) {
      return {ctor: "Linear"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var linear = Linear;
   var fmod = F2(function (f,n) {
      return function () {
         var integer = $Basics.floor(f);
         return $Basics.toFloat(A2($Basics._op["%"],
         integer,
         n)) + f - $Basics.toFloat(integer);
      }();
   });
   var rgbToHsl = F3(function (red,
   green,
   blue) {
      return function () {
         var b = $Basics.toFloat(blue) / 255;
         var g = $Basics.toFloat(green) / 255;
         var r = $Basics.toFloat(red) / 255;
         var cMax = A2($Basics.max,
         A2($Basics.max,r,g),
         b);
         var cMin = A2($Basics.min,
         A2($Basics.min,r,g),
         b);
         var c = cMax - cMin;
         var lightness = (cMax + cMin) / 2;
         var saturation = _U.eq(lightness,
         0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
         var hue = $Basics.degrees(60) * (_U.eq(cMax,
         r) ? A2(fmod,
         (g - b) / c,
         6) : _U.eq(cMax,
         g) ? (b - r) / c + 2 : _U.eq(cMax,
         b) ? (r - g) / c + 4 : _U.badIf($moduleName,
         "between lines 150 and 152"));
         return {ctor: "_Tuple3"
                ,_0: hue
                ,_1: saturation
                ,_2: lightness};
      }();
   });
   var hslToRgb = F3(function (hue,
   saturation,
   lightness) {
      return function () {
         var hue$ = hue / $Basics.degrees(60);
         var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
         var x = chroma * (1 - $Basics.abs(A2(fmod,
         hue$,
         2) - 1));
         var $ = _U.cmp(hue$,
         0) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: 0
                  ,_2: 0} : _U.cmp(hue$,
         1) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: x
                  ,_2: 0} : _U.cmp(hue$,
         2) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: chroma
                  ,_2: 0} : _U.cmp(hue$,
         3) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: chroma
                  ,_2: x} : _U.cmp(hue$,
         4) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: x
                  ,_2: chroma} : _U.cmp(hue$,
         5) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: 0
                  ,_2: chroma} : _U.cmp(hue$,
         6) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: 0
                  ,_2: x} : {ctor: "_Tuple3"
                            ,_0: 0
                            ,_1: 0
                            ,_2: 0},
         r = $._0,
         g = $._1,
         b = $._2;
         var m = lightness - chroma / 2;
         return {ctor: "_Tuple3"
                ,_0: r + m
                ,_1: g + m
                ,_2: b + m};
      }();
   });
   var toRgb = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA":
            return function () {
                 var $ = A3(hslToRgb,
                 color._0,
                 color._1,
                 color._2),
                 r = $._0,
                 g = $._1,
                 b = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,blue: $Basics.round(255 * b)
                        ,green: $Basics.round(255 * g)
                        ,red: $Basics.round(255 * r)};
              }();
            case "RGBA": return {_: {}
                                ,alpha: color._3
                                ,blue: color._2
                                ,green: color._1
                                ,red: color._0};}
         _U.badCase($moduleName,
         "between lines 124 and 132");
      }();
   };
   var toHsl = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return {_: {}
                              ,alpha: color._3
                              ,hue: color._0
                              ,lightness: color._2
                              ,saturation: color._1};
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,hue: h
                        ,lightness: l
                        ,saturation: s};
              }();}
         _U.badCase($moduleName,
         "between lines 114 and 118");
      }();
   };
   var HSLA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "HSLA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var hsla = F4(function (hue,
   saturation,
   lightness,
   alpha) {
      return A4(HSLA,
      hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),
      saturation,
      lightness,
      alpha);
   });
   var hsl = F3(function (hue,
   saturation,
   lightness) {
      return A4(hsla,
      hue,
      saturation,
      lightness,
      1);
   });
   var complement = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return A4(hsla,
              color._0 + $Basics.degrees(180),
              color._1,
              color._2,
              color._3);
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return A4(hsla,
                 h + $Basics.degrees(180),
                 s,
                 l,
                 color._3);
              }();}
         _U.badCase($moduleName,
         "between lines 105 and 108");
      }();
   };
   var grayscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var greyscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var RGBA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "RGBA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {
      return A4(RGBA,r,g,b,1);
   });
   var lightRed = A4(RGBA,
   239,
   41,
   41,
   1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,
   164,
   0,
   0,
   1);
   var lightOrange = A4(RGBA,
   252,
   175,
   62,
   1);
   var orange = A4(RGBA,
   245,
   121,
   0,
   1);
   var darkOrange = A4(RGBA,
   206,
   92,
   0,
   1);
   var lightYellow = A4(RGBA,
   255,
   233,
   79,
   1);
   var yellow = A4(RGBA,
   237,
   212,
   0,
   1);
   var darkYellow = A4(RGBA,
   196,
   160,
   0,
   1);
   var lightGreen = A4(RGBA,
   138,
   226,
   52,
   1);
   var green = A4(RGBA,
   115,
   210,
   22,
   1);
   var darkGreen = A4(RGBA,
   78,
   154,
   6,
   1);
   var lightBlue = A4(RGBA,
   114,
   159,
   207,
   1);
   var blue = A4(RGBA,
   52,
   101,
   164,
   1);
   var darkBlue = A4(RGBA,
   32,
   74,
   135,
   1);
   var lightPurple = A4(RGBA,
   173,
   127,
   168,
   1);
   var purple = A4(RGBA,
   117,
   80,
   123,
   1);
   var darkPurple = A4(RGBA,
   92,
   53,
   102,
   1);
   var lightBrown = A4(RGBA,
   233,
   185,
   110,
   1);
   var brown = A4(RGBA,
   193,
   125,
   17,
   1);
   var darkBrown = A4(RGBA,
   143,
   89,
   2,
   1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,
   255,
   255,
   255,
   1);
   var lightGrey = A4(RGBA,
   238,
   238,
   236,
   1);
   var grey = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGrey = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightGray = A4(RGBA,
   238,
   238,
   236,
   1);
   var gray = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGray = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightCharcoal = A4(RGBA,
   136,
   138,
   133,
   1);
   var charcoal = A4(RGBA,
   85,
   87,
   83,
   1);
   var darkCharcoal = A4(RGBA,
   46,
   52,
   54,
   1);
   _elm.Color.values = {_op: _op
                       ,rgb: rgb
                       ,rgba: rgba
                       ,hsl: hsl
                       ,hsla: hsla
                       ,greyscale: greyscale
                       ,grayscale: grayscale
                       ,complement: complement
                       ,linear: linear
                       ,radial: radial
                       ,toRgb: toRgb
                       ,toHsl: toHsl
                       ,red: red
                       ,orange: orange
                       ,yellow: yellow
                       ,green: green
                       ,blue: blue
                       ,purple: purple
                       ,brown: brown
                       ,lightRed: lightRed
                       ,lightOrange: lightOrange
                       ,lightYellow: lightYellow
                       ,lightGreen: lightGreen
                       ,lightBlue: lightBlue
                       ,lightPurple: lightPurple
                       ,lightBrown: lightBrown
                       ,darkRed: darkRed
                       ,darkOrange: darkOrange
                       ,darkYellow: darkYellow
                       ,darkGreen: darkGreen
                       ,darkBlue: darkBlue
                       ,darkPurple: darkPurple
                       ,darkBrown: darkBrown
                       ,white: white
                       ,lightGrey: lightGrey
                       ,grey: grey
                       ,darkGrey: darkGrey
                       ,lightCharcoal: lightCharcoal
                       ,charcoal: charcoal
                       ,darkCharcoal: darkCharcoal
                       ,black: black
                       ,lightGray: lightGray
                       ,gray: gray
                       ,darkGray: darkGray};
   return _elm.Color.values;
};
Elm.Date = Elm.Date || {};
Elm.Date.make = function (_elm) {
   "use strict";
   _elm.Date = _elm.Date || {};
   if (_elm.Date.values)
   return _elm.Date.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Date",
   $Native$Date = Elm.Native.Date.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Time = Elm.Time.make(_elm);
   var millisecond = $Native$Date.millisecond;
   var second = $Native$Date.second;
   var minute = $Native$Date.minute;
   var hour = $Native$Date.hour;
   var dayOfWeek = $Native$Date.dayOfWeek;
   var day = $Native$Date.day;
   var month = $Native$Date.month;
   var year = $Native$Date.year;
   var fromTime = $Native$Date.fromTime;
   var toTime = $Native$Date.toTime;
   var fromString = $Native$Date.read;
   var Dec = {ctor: "Dec"};
   var Nov = {ctor: "Nov"};
   var Oct = {ctor: "Oct"};
   var Sep = {ctor: "Sep"};
   var Aug = {ctor: "Aug"};
   var Jul = {ctor: "Jul"};
   var Jun = {ctor: "Jun"};
   var May = {ctor: "May"};
   var Apr = {ctor: "Apr"};
   var Mar = {ctor: "Mar"};
   var Feb = {ctor: "Feb"};
   var Jan = {ctor: "Jan"};
   var Sun = {ctor: "Sun"};
   var Sat = {ctor: "Sat"};
   var Fri = {ctor: "Fri"};
   var Thu = {ctor: "Thu"};
   var Wed = {ctor: "Wed"};
   var Tue = {ctor: "Tue"};
   var Mon = {ctor: "Mon"};
   var Date = {ctor: "Date"};
   _elm.Date.values = {_op: _op
                      ,fromString: fromString
                      ,toTime: toTime
                      ,fromTime: fromTime
                      ,year: year
                      ,month: month
                      ,day: day
                      ,dayOfWeek: dayOfWeek
                      ,hour: hour
                      ,minute: minute
                      ,second: second
                      ,millisecond: millisecond
                      ,Jan: Jan
                      ,Feb: Feb
                      ,Mar: Mar
                      ,Apr: Apr
                      ,May: May
                      ,Jun: Jun
                      ,Jul: Jul
                      ,Aug: Aug
                      ,Sep: Sep
                      ,Oct: Oct
                      ,Nov: Nov
                      ,Dec: Dec
                      ,Mon: Mon
                      ,Tue: Tue
                      ,Wed: Wed
                      ,Thu: Thu
                      ,Fri: Fri
                      ,Sat: Sat
                      ,Sun: Sun};
   return _elm.Date.values;
};
Elm.Date = Elm.Date || {};
Elm.Date.Format = Elm.Date.Format || {};
Elm.Date.Format.make = function (_elm) {
   "use strict";
   _elm.Date = _elm.Date || {};
   _elm.Date.Format = _elm.Date.Format || {};
   if (_elm.Date.Format.values)
   return _elm.Date.Format.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Date.Format",
   $Basics = Elm.Basics.make(_elm),
   $Date = Elm.Date.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Regex = Elm.Regex.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var padWith = function (c) {
      return function ($) {
         return A2($String.padLeft,
         2,
         c)($Basics.toString($));
      };
   };
   var mod12 = function (h) {
      return A2($Basics._op["%"],
      h,
      12);
   };
   var fullDayOfWeek = function (dow) {
      return function () {
         switch (dow.ctor)
         {case "Fri": return "Friday";
            case "Mon": return "Monday";
            case "Sat": return "Saturday";
            case "Sun": return "Sunday";
            case "Thu": return "Thursday";
            case "Tue": return "Tuesday";
            case "Wed": return "Wednesday";}
         _U.badCase($moduleName,
         "between lines 76 and 83");
      }();
   };
   var monthToFullName = function (m) {
      return function () {
         switch (m.ctor)
         {case "Apr": return "April";
            case "Aug": return "August";
            case "Dec": return "December";
            case "Feb": return "February";
            case "Jan": return "January";
            case "Jul": return "July";
            case "Jun": return "June";
            case "Mar": return "March";
            case "May": return "May";
            case "Nov": return "November";
            case "Oct": return "October";
            case "Sep": return "September";}
         _U.badCase($moduleName,
         "between lines 62 and 74");
      }();
   };
   var monthToInt = function (m) {
      return function () {
         switch (m.ctor)
         {case "Apr": return 4;
            case "Aug": return 8;
            case "Dec": return 12;
            case "Feb": return 2;
            case "Jan": return 1;
            case "Jul": return 7;
            case "Jun": return 6;
            case "Mar": return 3;
            case "May": return 5;
            case "Nov": return 11;
            case "Oct": return 10;
            case "Sep": return 9;}
         _U.badCase($moduleName,
         "between lines 48 and 60");
      }();
   };
   var collapse = function (m) {
      return A2($Maybe.andThen,
      m,
      $Basics.identity);
   };
   var formatToken = F2(function (d,
   m) {
      return function () {
         var symbol = $Maybe.withDefault(" ")(collapse(A2($Maybe.andThen,
         $List.tail(m.submatches),
         $List.head)));
         var prefix = $Maybe.withDefault(" ")(collapse($List.head(m.submatches)));
         return A2($Basics._op["++"],
         prefix,
         function () {
            switch (symbol)
            {case "A":
               return fullDayOfWeek($Date.dayOfWeek(d));
               case "B":
               return monthToFullName($Date.month(d));
               case "H":
               return padWith(_U.chr("0"))($Date.hour(d));
               case "I":
               return padWith(_U.chr("0"))(mod12($Date.hour(d)));
               case "M":
               return padWith(_U.chr("0"))($Date.minute(d));
               case "P":
               return _U.cmp($Date.hour(d),
                 13) < 0 ? "am" : "pm";
               case "S":
               return padWith(_U.chr("0"))($Date.second(d));
               case "Y":
               return $Basics.toString($Date.year(d));
               case "a":
               return $Basics.toString($Date.dayOfWeek(d));
               case "b":
               return $Basics.toString($Date.month(d));
               case "d":
               return padWith(_U.chr("0"))($Date.day(d));
               case "e":
               return padWith(_U.chr(" "))($Date.day(d));
               case "k":
               return padWith(_U.chr(" "))($Date.hour(d));
               case "l":
               return padWith(_U.chr(" "))(mod12($Date.hour(d)));
               case "m":
               return A2($String.padLeft,
                 2,
                 _U.chr("0"))($Basics.toString(monthToInt($Date.month(d))));
               case "p":
               return _U.cmp($Date.hour(d),
                 13) < 0 ? "AM" : "PM";}
            return "";
         }());
      }();
   });
   var re = $Regex.regex("(^|[^%])%(Y|m|B|b|d|e|a|A|H|k|I|l|p|P|M|S)");
   var format = F2(function (s,d) {
      return A4($Regex.replace,
      $Regex.All,
      re,
      formatToken(d),
      s);
   });
   _elm.Date.Format.values = {_op: _op
                             ,format: format};
   return _elm.Date.Format.values;
};
Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values)
   return _elm.Debug.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Debug",
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm);
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Debug.crash;
   var log = $Native$Debug.log;
   _elm.Debug.values = {_op: _op
                       ,log: log
                       ,crash: crash
                       ,watch: watch
                       ,watchSummary: watchSummary
                       ,trace: trace};
   return _elm.Debug.values;
};
Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values)
   return _elm.Dict.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Dict",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $String = Elm.String.make(_elm);
   var foldr = F3(function (f,
   acc,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldr,
              f,
              A3(f,
              t._1,
              t._2,
              A3(foldr,f,acc,t._4)),
              t._3);}
         _U.badCase($moduleName,
         "between lines 417 and 421");
      }();
   });
   var keys = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      keyList) {
         return A2($List._op["::"],
         key,
         keyList);
      }),
      _L.fromArray([]),
      dict);
   };
   var values = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      valueList) {
         return A2($List._op["::"],
         value,
         valueList);
      }),
      _L.fromArray([]),
      dict);
   };
   var toList = function (dict) {
      return A3(foldr,
      F3(function (key,value,list) {
         return A2($List._op["::"],
         {ctor: "_Tuple2"
         ,_0: key
         ,_1: value},
         list);
      }),
      _L.fromArray([]),
      dict);
   };
   var foldl = F3(function (f,
   acc,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldl,
              f,
              A3(f,
              dict._1,
              dict._2,
              A3(foldl,f,acc,dict._3)),
              dict._4);}
         _U.badCase($moduleName,
         "between lines 406 and 410");
      }();
   });
   var isBBlack = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack": return true;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "BBlack": return true;}
              break;}
         return false;
      }();
   };
   var showFlag = function (f) {
      return function () {
         switch (f.ctor)
         {case "Insert": return "Insert";
            case "Remove": return "Remove";
            case "Same": return "Same";}
         _U.badCase($moduleName,
         "between lines 182 and 185");
      }();
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var get = F2(function (targetKey,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Maybe.Nothing;}
              break;
            case "RBNode":
            return function () {
                 var _v29 = A2($Basics.compare,
                 targetKey,
                 dict._1);
                 switch (_v29.ctor)
                 {case "EQ":
                    return $Maybe.Just(dict._2);
                    case "GT": return A2(get,
                      targetKey,
                      dict._4);
                    case "LT": return A2(get,
                      targetKey,
                      dict._3);}
                 _U.badCase($moduleName,
                 "between lines 129 and 132");
              }();}
         _U.badCase($moduleName,
         "between lines 124 and 132");
      }();
   });
   var member = F2(function (key,
   dict) {
      return function () {
         var _v30 = A2(get,key,dict);
         switch (_v30.ctor)
         {case "Just": return true;
            case "Nothing": return false;}
         _U.badCase($moduleName,
         "between lines 138 and 140");
      }();
   });
   var max = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("(max Empty) is not defined");
            case "RBNode":
            switch (dict._4.ctor)
              {case "RBEmpty":
                 return {ctor: "_Tuple2"
                        ,_0: dict._1
                        ,_1: dict._2};}
              return max(dict._4);}
         _U.badCase($moduleName,
         "between lines 100 and 108");
      }();
   };
   var min = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Native$Debug.crash("(min Empty) is not defined");}
              break;
            case "RBNode":
            switch (dict._3.ctor)
              {case "RBEmpty":
                 switch (dict._3._0.ctor)
                   {case "LBlack":
                      return {ctor: "_Tuple2"
                             ,_0: dict._1
                             ,_1: dict._2};}
                   break;}
              return min(dict._3);}
         _U.badCase($moduleName,
         "between lines 87 and 95");
      }();
   };
   var RBEmpty = function (a) {
      return {ctor: "RBEmpty"
             ,_0: a};
   };
   var RBNode = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "RBNode"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var showLColor = function (color) {
      return function () {
         switch (color.ctor)
         {case "LBBlack":
            return "LBBlack";
            case "LBlack": return "LBlack";}
         _U.badCase($moduleName,
         "between lines 70 and 72");
      }();
   };
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty(LBlack);
   var isEmpty = function (dict) {
      return _U.eq(dict,empty);
   };
   var map = F2(function (f,dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              dict._0,
              dict._1,
              A2(f,dict._1,dict._2),
              A2(map,f,dict._3),
              A2(map,f,dict._4));}
         _U.badCase($moduleName,
         "between lines 394 and 399");
      }();
   });
   var showNColor = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack": return "BBlack";
            case "Black": return "Black";
            case "NBlack": return "NBlack";
            case "Red": return "Red";}
         _U.badCase($moduleName,
         "between lines 56 and 60");
      }();
   };
   var reportRemBug = F4(function (msg,
   c,
   lgot,
   rgot) {
      return $Native$Debug.crash($String.concat(_L.fromArray(["Internal red-black tree invariant violated, expected "
                                                             ,msg
                                                             ," and got "
                                                             ,showNColor(c)
                                                             ,"/"
                                                             ,lgot
                                                             ,"/"
                                                             ,rgot
                                                             ,"\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"])));
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return dict;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "Black": return dict;
                 case "Red": return A5(RBNode,
                   Black,
                   dict._1,
                   dict._2,
                   dict._3,
                   dict._4);}
              break;}
         _U.badCase($moduleName,
         "between lines 154 and 162");
      }();
   };
   var blackish = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty": return true;
            case "RBNode":
            return _U.eq(t._0,
              Black) || _U.eq(t._0,BBlack);}
         _U.badCase($moduleName,
         "between lines 339 and 341");
      }();
   };
   var blacken = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return RBEmpty(LBlack);
            case "RBNode": return A5(RBNode,
              Black,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 378 and 380");
      }();
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack":
            return $Native$Debug.crash("Can\'t make a double black node more black!");
            case "Black": return BBlack;
            case "NBlack": return Red;
            case "Red": return Black;}
         _U.badCase($moduleName,
         "between lines 244 and 248");
      }();
   };
   var lessBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack": return Black;
            case "Black": return Red;
            case "NBlack":
            return $Native$Debug.crash("Can\'t make a negative black node less black!");
            case "Red": return NBlack;}
         _U.badCase($moduleName,
         "between lines 253 and 257");
      }();
   };
   var lessBlackTree = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              lessBlack(dict._0),
              dict._1,
              dict._2,
              dict._3,
              dict._4);}
         _U.badCase($moduleName,
         "between lines 262 and 264");
      }();
   };
   var redden = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("can\'t make a Leaf red");
            case "RBNode": return A5(RBNode,
              Red,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 386 and 388");
      }();
   };
   var balance_node = function (t) {
      return function () {
         var assemble = function (col) {
            return function (xk) {
               return function (xv) {
                  return function (yk) {
                     return function (yv) {
                        return function (zk) {
                           return function (zv) {
                              return function (a) {
                                 return function (b) {
                                    return function (c) {
                                       return function (d) {
                                          return A5(RBNode,
                                          lessBlack(col),
                                          yk,
                                          yv,
                                          A5(RBNode,Black,xk,xv,a,b),
                                          A5(RBNode,Black,zk,zv,c,d));
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
         return blackish(t) ? function () {
            switch (t.ctor)
            {case "RBNode":
               switch (t._3.ctor)
                 {case "RBNode":
                    switch (t._3._0.ctor)
                      {case "Red":
                         switch (t._3._3.ctor)
                           {case "RBNode":
                              switch (t._3._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._3._1)(t._3._3._2)(t._3._1)(t._3._2)(t._1)(t._2)(t._3._3._3)(t._3._3._4)(t._3._4)(t._4);}
                                break;}
                           switch (t._3._4.ctor)
                           {case "RBNode":
                              switch (t._3._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._1)(t._3._2)(t._3._4._1)(t._3._4._2)(t._1)(t._2)(t._3._3)(t._3._4._3)(t._3._4._4)(t._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._4.ctor)
                 {case "RBNode":
                    switch (t._4._0.ctor)
                      {case "Red":
                         switch (t._4._3.ctor)
                           {case "RBNode":
                              switch (t._4._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._3._1)(t._4._3._2)(t._4._1)(t._4._2)(t._3)(t._4._3._3)(t._4._3._4)(t._4._4);}
                                break;}
                           switch (t._4._4.ctor)
                           {case "RBNode":
                              switch (t._4._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._1)(t._4._2)(t._4._4._1)(t._4._4._2)(t._3)(t._4._3)(t._4._4._3)(t._4._4._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._0.ctor)
                 {case "BBlack":
                    switch (t._4.ctor)
                      {case "RBNode":
                         switch (t._4._0.ctor)
                           {case "NBlack":
                              switch (t._4._3.ctor)
                                {case "RBNode":
                                   switch (t._4._3._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._4._4.ctor)
                                             {case "RBNode":
                                                switch (t._4._4._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._4._3._1,
                                                       t._4._3._2,
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3,
                                                       t._4._3._3),
                                                       A5(balance,
                                                       Black,
                                                       t._4._1,
                                                       t._4._2,
                                                       t._4._3._4,
                                                       redden(t._4._4)));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      switch (t._3.ctor)
                      {case "RBNode":
                         switch (t._3._0.ctor)
                           {case "NBlack":
                              switch (t._3._4.ctor)
                                {case "RBNode":
                                   switch (t._3._4._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._3._3.ctor)
                                             {case "RBNode":
                                                switch (t._3._3._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._3._4._1,
                                                       t._3._4._2,
                                                       A5(balance,
                                                       Black,
                                                       t._3._1,
                                                       t._3._2,
                                                       redden(t._3._3),
                                                       t._3._4._3),
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3._4._4,
                                                       t._4));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      break;}
                 break;}
            return t;
         }() : t;
      }();
   };
   var balance = F5(function (c,
   k,
   v,
   l,
   r) {
      return balance_node(A5(RBNode,
      c,
      k,
      v,
      l,
      r));
   });
   var bubble = F5(function (c,
   k,
   v,
   l,
   r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,
      moreBlack(c),
      k,
      v,
      lessBlackTree(l),
      lessBlackTree(r)) : A5(RBNode,
      c,
      k,
      v,
      l,
      r);
   });
   var remove_max = F5(function (c,
   k,
   v,
   l,
   r) {
      return function () {
         switch (r.ctor)
         {case "RBEmpty": return A3(rem,
              c,
              l,
              r);
            case "RBNode": return A5(bubble,
              c,
              k,
              v,
              l,
              A5(remove_max,
              r._0,
              r._1,
              r._2,
              r._3,
              r._4));}
         _U.badCase($moduleName,
         "between lines 323 and 328");
      }();
   });
   var rem = F3(function (c,l,r) {
      return function () {
         var _v169 = {ctor: "_Tuple2"
                     ,_0: l
                     ,_1: r};
         switch (_v169.ctor)
         {case "_Tuple2":
            switch (_v169._0.ctor)
              {case "RBEmpty":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           switch (c.ctor)
                           {case "Black":
                              return RBEmpty(LBBlack);
                              case "Red":
                              return RBEmpty(LBlack);}
                           _U.badCase($moduleName,
                           "between lines 282 and 286");
                        }();
                      case "RBNode":
                      return function () {
                           var _v191 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v191.ctor)
                           {case "_Tuple3":
                              switch (_v191._0.ctor)
                                {case "Black":
                                   switch (_v191._1.ctor)
                                     {case "LBlack":
                                        switch (_v191._2.ctor)
                                          {case "Red": return A5(RBNode,
                                               Black,
                                               _v169._1._1,
                                               _v169._1._2,
                                               _v169._1._3,
                                               _v169._1._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/LBlack/Red",
                           c,
                           showLColor(_v169._0._0),
                           showNColor(_v169._1._0));
                        }();}
                   break;
                 case "RBNode":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           var _v195 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v195.ctor)
                           {case "_Tuple3":
                              switch (_v195._0.ctor)
                                {case "Black":
                                   switch (_v195._1.ctor)
                                     {case "Red":
                                        switch (_v195._2.ctor)
                                          {case "LBlack":
                                             return A5(RBNode,
                                               Black,
                                               _v169._0._1,
                                               _v169._0._2,
                                               _v169._0._3,
                                               _v169._0._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/Red/LBlack",
                           c,
                           showNColor(_v169._0._0),
                           showLColor(_v169._1._0));
                        }();
                      case "RBNode":
                      return function () {
                           var l$ = A5(remove_max,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var r = A5(RBNode,
                           _v169._1._0,
                           _v169._1._1,
                           _v169._1._2,
                           _v169._1._3,
                           _v169._1._4);
                           var l = A5(RBNode,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var $ = max(l),
                           k = $._0,
                           v = $._1;
                           return A5(bubble,c,k,v,l$,r);
                        }();}
                   break;}
              break;}
         _U.badCase($moduleName,
         "between lines 280 and 309");
      }();
   });
   var update = F3(function (k,
   alter,
   dict) {
      return function () {
         var up = function (dict) {
            return function () {
               switch (dict.ctor)
               {case "RBEmpty":
                  switch (dict._0.ctor)
                    {case "LBlack":
                       return function () {
                            var _v206 = alter($Maybe.Nothing);
                            switch (_v206.ctor)
                            {case "Just":
                               return {ctor: "_Tuple2"
                                      ,_0: Insert
                                      ,_1: A5(RBNode,
                                      Red,
                                      k,
                                      _v206._0,
                                      empty,
                                      empty)};
                               case "Nothing":
                               return {ctor: "_Tuple2"
                                      ,_0: Same
                                      ,_1: empty};}
                            _U.badCase($moduleName,
                            "between lines 194 and 198");
                         }();}
                    break;
                  case "RBNode":
                  return function () {
                       var _v208 = A2($Basics.compare,
                       k,
                       dict._1);
                       switch (_v208.ctor)
                       {case "EQ": return function () {
                               var _v209 = alter($Maybe.Just(dict._2));
                               switch (_v209.ctor)
                               {case "Just":
                                  return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode,
                                         dict._0,
                                         dict._1,
                                         _v209._0,
                                         dict._3,
                                         dict._4)};
                                  case "Nothing":
                                  return {ctor: "_Tuple2"
                                         ,_0: Remove
                                         ,_1: A3(rem,
                                         dict._0,
                                         dict._3,
                                         dict._4)};}
                               _U.badCase($moduleName,
                               "between lines 201 and 206");
                            }();
                          case "GT": return function () {
                               var $ = up(dict._4),
                               flag = $._0,
                               newRight = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};}
                                  _U.badCase($moduleName,
                                  "between lines 215 and 220");
                               }();
                            }();
                          case "LT": return function () {
                               var $ = up(dict._3),
                               flag = $._0,
                               newLeft = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};}
                                  _U.badCase($moduleName,
                                  "between lines 208 and 213");
                               }();
                            }();}
                       _U.badCase($moduleName,
                       "between lines 199 and 220");
                    }();}
               _U.badCase($moduleName,
               "between lines 192 and 220");
            }();
         };
         var $ = up(dict),
         flag = $._0,
         updatedDict = $._1;
         return function () {
            switch (flag.ctor)
            {case "Insert":
               return ensureBlackRoot(updatedDict);
               case "Remove":
               return blacken(updatedDict);
               case "Same":
               return updatedDict;}
            _U.badCase($moduleName,
            "between lines 222 and 225");
         }();
      }();
   });
   var insert = F3(function (key,
   value,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Just(value)),
      dict);
   });
   var singleton = F2(function (key,
   value) {
      return A3(insert,
      key,
      value,
      empty);
   });
   var union = F2(function (t1,
   t2) {
      return A3(foldl,
      insert,
      t2,
      t1);
   });
   var fromList = function (assocs) {
      return A3($List.foldl,
      F2(function (_v214,dict) {
         return function () {
            switch (_v214.ctor)
            {case "_Tuple2":
               return A3(insert,
                 _v214._0,
                 _v214._1,
                 dict);}
            _U.badCase($moduleName,
            "on line 466, column 38 to 59");
         }();
      }),
      empty,
      assocs);
   };
   var filter = F2(function (predicate,
   dictionary) {
      return function () {
         var add = F3(function (key,
         value,
         dict) {
            return A2(predicate,
            key,
            value) ? A3(insert,
            key,
            value,
            dict) : dict;
         });
         return A3(foldl,
         add,
         empty,
         dictionary);
      }();
   });
   var intersect = F2(function (t1,
   t2) {
      return A2(filter,
      F2(function (k,_v218) {
         return function () {
            return A2(member,k,t2);
         }();
      }),
      t1);
   });
   var partition = F2(function (predicate,
   dict) {
      return function () {
         var add = F3(function (key,
         value,
         _v220) {
            return function () {
               switch (_v220.ctor)
               {case "_Tuple2":
                  return A2(predicate,
                    key,
                    value) ? {ctor: "_Tuple2"
                             ,_0: A3(insert,
                             key,
                             value,
                             _v220._0)
                             ,_1: _v220._1} : {ctor: "_Tuple2"
                                              ,_0: _v220._0
                                              ,_1: A3(insert,
                                              key,
                                              value,
                                              _v220._1)};}
               _U.badCase($moduleName,
               "between lines 487 and 489");
            }();
         });
         return A3(foldl,
         add,
         {ctor: "_Tuple2"
         ,_0: empty
         ,_1: empty},
         dict);
      }();
   });
   var remove = F2(function (key,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Nothing),
      dict);
   });
   var diff = F2(function (t1,t2) {
      return A3(foldl,
      F3(function (k,v,t) {
         return A2(remove,k,t);
      }),
      t1,
      t2);
   });
   _elm.Dict.values = {_op: _op
                      ,empty: empty
                      ,singleton: singleton
                      ,insert: insert
                      ,update: update
                      ,isEmpty: isEmpty
                      ,get: get
                      ,remove: remove
                      ,member: member
                      ,filter: filter
                      ,partition: partition
                      ,foldl: foldl
                      ,foldr: foldr
                      ,map: map
                      ,union: union
                      ,intersect: intersect
                      ,diff: diff
                      ,keys: keys
                      ,values: values
                      ,toList: toList
                      ,fromList: fromList};
   return _elm.Dict.values;
};
Elm.Flashdance = Elm.Flashdance || {};
Elm.Flashdance.make = function (_elm) {
   "use strict";
   _elm.Flashdance = _elm.Flashdance || {};
   if (_elm.Flashdance.values)
   return _elm.Flashdance.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Flashdance",
   $Basics = Elm.Basics.make(_elm),
   $Date = Elm.Date.make(_elm),
   $Date$Format = Elm.Date.Format.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $Http = Elm.Http.make(_elm),
   $HttpRequests = Elm.HttpRequests.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Model = Elm.Model.make(_elm),
   $Price = Elm.Price.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Svg = Elm.Svg.make(_elm),
   $Svg$Attributes = Elm.Svg.Attributes.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Task$Extra = Elm.Task.Extra.make(_elm);
   var seatColor = F2(function (model,
   seat) {
      return A2($Model.isSelected,
      model,
      seat) ? "#0f0" : A2($Model.isReserved,
      model,
      seat) ? "#f00" : "rgba(0,0,0,0)";
   });
   var isJust = function (m) {
      return function () {
         switch (m.ctor)
         {case "Just": return true;
            case "Nothing": return false;}
         _U.badCase($moduleName,
         "between lines 963 and 965");
      }();
   };
   var radioInputs = F5(function (address,
   action,
   name,
   labels,
   checked) {
      return function () {
         var labelFor = function (option) {
            return function () {
               switch (option.ctor)
               {case "Delivery":
                  return "Karten per Post zusenden lassen (Aufpreis von € 3,-)";
                  case "PickUpBeforehand":
                  return "Abholung vorab an der HGR (Mo. – Fr., 13.00 – 14.30 Uhr, Raum 234)";
                  case "PickUpBoxOffice":
                  return "Abholung an der Abendkasse";}
               _U.badCase($moduleName,
               "between lines 932 and 936");
            }();
         };
         var radioField = F2(function (option,
         checked) {
            return A2($Html.div,
            _L.fromArray([$Html$Attributes.$class("radio")]),
            _L.fromArray([A2($Html.label,
            _L.fromArray([]),
            _L.fromArray([A2($Html.input,
                         _L.fromArray([A2($Html$Events.onClick,
                                      address,
                                      action(option))
                                      ,$Html$Attributes.type$("radio")
                                      ,$Html$Attributes.name(name)
                                      ,$Html$Attributes.checked(_U.eq(option,
                                      checked))]),
                         _L.fromArray([]))
                         ,$Html.text(labelFor(option))]))]));
         });
         return A2($Html.div,
         _L.fromArray([$Html$Attributes.$class("form-group")]),
         A2($List.map,
         function (l) {
            return A2(radioField,
            l,
            checked);
         },
         labels));
      }();
   });
   var formInput = F6(function (type$,
   address,
   action,
   name,
   text,
   value) {
      return A2($Html.div,
      _L.fromArray([$Html$Attributes.$class("form-group")]),
      _L.fromArray([A2($Html.label,
                   _L.fromArray([$Html$Attributes.$for(name)]),
                   _L.fromArray([$Html.text(text)]))
                   ,A2($Html.input,
                   _L.fromArray([$Html$Attributes.type$(type$)
                                ,$Html$Attributes.$class("form-control")
                                ,$Html$Attributes.id(name)
                                ,$Html$Attributes.value(value)
                                ,A2($Html$Events.on,
                                "input",
                                $Html$Events.targetValue)(function ($) {
                                   return $Signal.message(address)(action($));
                                })]),
                   _L.fromArray([]))]));
   });
   var textInput = formInput("text");
   var passwordInput = formInput("password");
   var emailInput = formInput("email");
   var numberInput = formInput("number");
   var orderTableFromOrder = function (order) {
      return function () {
         var deliveryPrice = isJust(order.address) ? $Maybe.Just($Price.fromInt(300)) : $Maybe.Nothing;
         var reducedPrice = $Price.fromInt(1200 * order.reducedCount);
         var fullCount = $List.length(order.seatIds) - order.reducedCount;
         var fullPrice = $Price.fromInt(1600 * fullCount);
         return {_: {}
                ,deliveryPrice: deliveryPrice
                ,fullCount: fullCount
                ,fullPrice: fullPrice
                ,reducedCount: order.reducedCount
                ,reducedPrice: reducedPrice
                ,totalPrice: function () {
                   switch (deliveryPrice.ctor)
                   {case "Just":
                      return A2($Price.add,
                        deliveryPrice._0,
                        A2($Price.add,
                        fullPrice,
                        reducedPrice));
                      case "Nothing":
                      return A2($Price.add,
                        fullPrice,
                        reducedPrice);}
                   _U.badCase($moduleName,
                   "between lines 862 and 864");
                }()};
      }();
   };
   var OrderTable = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,deliveryPrice: e
             ,fullCount: a
             ,fullPrice: b
             ,reducedCount: c
             ,reducedPrice: d
             ,totalPrice: f};
   });
   var mapWithDefault = F3(function (f,
   d,
   x) {
      return $Maybe.withDefault(d)(A2($Maybe.map,
      f,
      x));
   });
   var deliveryCosts = function (model) {
      return function () {
         var _v6 = model.orderState;
         switch (_v6.ctor)
         {case "Ordering":
            return function () {
                 var _v8 = _v6._0.deliveryOption;
                 switch (_v8.ctor)
                 {case "Delivery":
                    return $Price.fromInt(300);}
                 return $Price.fromInt(0);
              }();}
         return $Price.fromInt(0);
      }();
   };
   var combine = F3(function (f,
   a,
   b) {
      return function () {
         switch (a.ctor)
         {case "Just":
            return function () {
                 switch (b.ctor)
                 {case "Just":
                    return $Maybe.Just(A2(f,
                      a._0,
                      b._0));
                    case "Nothing":
                    return $Maybe.Nothing;}
                 _U.badCase($moduleName,
                 "between lines 781 and 784");
              }();
            case "Nothing":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 780 and 784");
      }();
   });
   var addPrice = $Price.add;
   var reducedCountValid = F2(function (reduced,
   totalCount) {
      return function () {
         var _v14 = $String.toInt(reduced);
         switch (_v14.ctor)
         {case "Err": return false;
            case "Ok":
            return _U.cmp(totalCount - _v14._0,
              0) > -1 && (_U.cmp(_v14._0,
              0) > -1 && _U.cmp(totalCount,
              0) > 0);}
         _U.badCase($moduleName,
         "between lines 760 and 762");
      }();
   });
   var reducedCount = function (model) {
      return $Result.toMaybe($String.toInt(model.formInput.reduced));
   };
   var fullCount = function (model) {
      return A2($Maybe.map,
      function (r) {
         return $List.length(model.stand.selections) - r;
      },
      reducedCount(model));
   };
   var fullPrice = function (model) {
      return $Maybe.map(function (n) {
         return $Price.fromInt(n * 1600);
      })(fullCount(model));
   };
   var reducedPrice = function (model) {
      return $Maybe.map(function (n) {
         return $Price.fromInt(n * 1200);
      })(reducedCount(model));
   };
   var totalPrice = function (model) {
      return A2($Maybe.map,
      function (price) {
         return A2($Price.add,
         price,
         deliveryCosts(model));
      },
      A3(combine,
      addPrice,
      fullPrice(model),
      reducedPrice(model)));
   };
   var viewOrderTable = function (model) {
      return function () {
         var optionalDeliveryCosts = function () {
            var _v17 = model.orderState;
            switch (_v17.ctor)
            {case "Ordering":
               return function () {
                    var _v19 = _v17._0.deliveryOption;
                    switch (_v19.ctor)
                    {case "Delivery":
                       return _L.fromArray([A2($Html.tr,
                         _L.fromArray([]),
                         _L.fromArray([A2($Html.td,
                                      _L.fromArray([]),
                                      _L.fromArray([$Html.text("Versandkosten")]))
                                      ,A2($Html.td,
                                      _L.fromArray([$Html$Attributes.$class("text-right")]),
                                      _L.fromArray([$Html.text($Price.format(deliveryCosts(model)))]))]))]);}
                    return _L.fromArray([]);
                 }();}
            return _L.fromArray([]);
         }();
         return A2($Html.div,
         _L.fromArray([]),
         _L.fromArray([A2($Html.table,
         _L.fromArray([$Html$Attributes.$class("table")]),
         _L.fromArray([A2($Html.tbody,
                      _L.fromArray([]),
                      A2($Basics._op["++"],
                      _L.fromArray([A2($Html.tr,
                                   _L.fromArray([]),
                                   _L.fromArray([A2($Html.td,
                                                _L.fromArray([]),
                                                _L.fromArray([$Html.text(A2($Basics._op["++"],
                                                A2(mapWithDefault,
                                                $Basics.toString,
                                                "-")(fullCount(model)),
                                                " reguläre Karten"))]))
                                                ,A2($Html.td,
                                                _L.fromArray([$Html$Attributes.$class("text-right")]),
                                                _L.fromArray([$Html.text(A2(mapWithDefault,
                                                $Price.format,
                                                "-")(fullPrice(model)))]))]))
                                   ,A2($Html.tr,
                                   _L.fromArray([]),
                                   _L.fromArray([A2($Html.td,
                                                _L.fromArray([]),
                                                _L.fromArray([$Html.text(A2($Basics._op["++"],
                                                A2(mapWithDefault,
                                                $Basics.toString,
                                                "-")(reducedCount(model)),
                                                " ermäßigte Karten"))]))
                                                ,A2($Html.td,
                                                _L.fromArray([$Html$Attributes.$class("text-right")]),
                                                _L.fromArray([$Html.text(A2(mapWithDefault,
                                                $Price.format,
                                                "-")(reducedPrice(model)))]))]))]),
                      optionalDeliveryCosts))
                      ,A2($Html.tfoot,
                      _L.fromArray([]),
                      _L.fromArray([A2($Html.tr,
                      _L.fromArray([]),
                      _L.fromArray([A2($Html.th,
                                   _L.fromArray([]),
                                   _L.fromArray([A2($Html.strong,
                                   _L.fromArray([]),
                                   _L.fromArray([$Html.text("Gesamtkosten")]))]))
                                   ,A2($Html.th,
                                   _L.fromArray([$Html$Attributes.$class("text-right")]),
                                   _L.fromArray([A2($Html.strong,
                                   _L.fromArray([]),
                                   _L.fromArray([$Html.text(A2(mapWithDefault,
                                   $Price.format,
                                   "-")(totalPrice(model)))]))]))]))]))]))]));
      }();
   };
   var unwrapMaybe = function (m) {
      return function () {
         switch (m.ctor)
         {case "Just": return m._0;}
         _U.badCase($moduleName,
         "between lines 751 and 752");
      }();
   };
   var viewOrderTable$ = function (order) {
      return function () {
         var orderTable = orderTableFromOrder(order);
         var optionalDeliveryCosts = function () {
            var _v23 = order.address;
            switch (_v23.ctor)
            {case "Just":
               return _L.fromArray([A2($Html.tr,
                 _L.fromArray([]),
                 _L.fromArray([A2($Html.td,
                              _L.fromArray([]),
                              _L.fromArray([$Html.text("Versandkosten")]))
                              ,A2($Html.td,
                              _L.fromArray([$Html$Attributes.$class("text-right")]),
                              _L.fromArray([$Html.text($Price.format(unwrapMaybe(orderTable.deliveryPrice)))]))]))]);}
            return _L.fromArray([]);
         }();
         return A2($Html.div,
         _L.fromArray([]),
         _L.fromArray([A2($Html.table,
         _L.fromArray([$Html$Attributes.$class("table")]),
         _L.fromArray([A2($Html.tbody,
                      _L.fromArray([]),
                      A2($Basics._op["++"],
                      _L.fromArray([A2($Html.tr,
                                   _L.fromArray([]),
                                   _L.fromArray([A2($Html.td,
                                                _L.fromArray([]),
                                                _L.fromArray([$Html.text(A2($Basics._op["++"],
                                                $Basics.toString(orderTable.fullCount),
                                                " reguläre Karten"))]))
                                                ,A2($Html.td,
                                                _L.fromArray([$Html$Attributes.$class("text-right")]),
                                                _L.fromArray([$Html.text($Price.format(orderTable.fullPrice))]))]))
                                   ,A2($Html.tr,
                                   _L.fromArray([]),
                                   _L.fromArray([A2($Html.td,
                                                _L.fromArray([]),
                                                _L.fromArray([$Html.text(A2($Basics._op["++"],
                                                $Basics.toString(orderTable.reducedCount),
                                                " ermäßigte Karten"))]))
                                                ,A2($Html.td,
                                                _L.fromArray([$Html$Attributes.$class("text-right")]),
                                                _L.fromArray([$Html.text($Price.format(orderTable.reducedPrice))]))]))]),
                      optionalDeliveryCosts))
                      ,A2($Html.tfoot,
                      _L.fromArray([]),
                      _L.fromArray([A2($Html.tr,
                      _L.fromArray([]),
                      _L.fromArray([A2($Html.th,
                                   _L.fromArray([]),
                                   _L.fromArray([A2($Html.strong,
                                   _L.fromArray([]),
                                   _L.fromArray([$Html.text("Gesamtkosten")]))]))
                                   ,A2($Html.th,
                                   _L.fromArray([$Html$Attributes.$class("text-right")]),
                                   _L.fromArray([A2($Html.strong,
                                   _L.fromArray([]),
                                   _L.fromArray([$Html.text($Price.format(orderTable.totalPrice))]))]))]))]))]))]));
      }();
   };
   var isDelivery = function ($do) {
      return function () {
         switch ($do.ctor)
         {case "Delivery": return true;}
         return false;
      }();
   };
   var viewOrderInfos = function (order) {
      return A2($Html.h1,
      _L.fromArray([]),
      _L.fromArray([$Html.text(A2($Basics._op["++"],
                   "Karten für \'",
                   A2($Basics._op["++"],
                   order.name,
                   "\' ")))
                   ,A2($Html.small,
                   _L.fromArray([]),
                   _L.fromArray([$Html.text(order.email)]))]));
   };
   var isAdmin = function (s) {
      return function () {
         switch (s.ctor)
         {case "Admin": return true;}
         return false;
      }();
   };
   var formatShortDate = $Date$Format.format("%d.%m.%Y");
   var formatDate = function (date) {
      return function () {
         var year = $Date$Format.format("%Y");
         var day = $Date$Format.format("%d");
         var monthName = "Juli";
         return A2($Basics._op["++"],
         day(date),
         A2($Basics._op["++"],
         ". ",
         A2($Basics._op["++"],
         monthName,
         A2($Basics._op["++"],
         " ",
         year(date)))));
      }();
   };
   var formatDateTime = $Date$Format.format("%d.%m.%Y um %H:%M Uhr");
   var viewFlashMessage = F3(function (address,
   action,
   message) {
      return function () {
         var alert = F2(function (text,
         $class) {
            return A2($Html.div,
            _L.fromArray([$Html$Attributes.$class(A2($Basics._op["++"],
                         "alert alert-",
                         A2($Basics._op["++"],
                         $class,
                         " alert-dismissible")))
                         ,A2($Html$Attributes.stringProperty,
                         "role",
                         "alert")]),
            _L.fromArray([A2($Html.button,
                         _L.fromArray([A2($Html$Events.onClick,
                                      address,
                                      action)
                                      ,$Html$Attributes.type$("button")
                                      ,$Html$Attributes.$class("close")
                                      ,A2($Html$Attributes.stringProperty,
                                      "aria-label",
                                      "Close")]),
                         _L.fromArray([A2($Html.span,
                         _L.fromArray([]),
                         _L.fromArray([$Html.text("×")]))]))
                         ,$Html.text(text)]));
         });
         var empty = A2($Html.div,
         _L.fromArray([]),
         _L.fromArray([]));
         return function () {
            switch (message.ctor)
            {case "Error": return A2(alert,
                 message._0,
                 "danger");
               case "Hidden": return empty;
               case "Info": return A2(alert,
                 message._0,
                 "info");
               case "Success": return A2(alert,
                 message._0,
                 "success");}
            _U.badCase($moduleName,
            "between lines 426 and 430");
         }();
      }();
   });
   var emptyHref = $Html$Attributes.href("javascript:void(0)");
   var typeFromDeliveryOption = function ($do) {
      return function () {
         switch ($do.ctor)
         {case "PickUpBeforehand":
            return "pickUpBeforehand";
            case "PickUpBoxOffice":
            return "pickUpBoxOffice";}
         _U.badCase($moduleName,
         "between lines 344 and 346");
      }();
   };
   var httpErrorToMessage = function (error) {
      return function () {
         switch (error.ctor)
         {case "BadResponse":
            return error._1;
            case "NetworkError":
            return "Server nicht erreichbar.";
            case "Timeout":
            return "Server nicht erreichbar.";
            case "UnexpectedPayload":
            return "Error";}
         _U.badCase($moduleName,
         "between lines 313 and 317");
      }();
   };
   var ordersWithoutEmail = $List.filter(function ($) {
      return $String.isEmpty(function (_) {
         return _.email;
      }($));
   });
   var ordersWithEmail = $List.filter(function ($) {
      return $Basics.not($String.isEmpty(function (_) {
         return _.email;
      }($)));
   });
   var ordersWithDelivery = $List.filter(function ($) {
      return isJust(function (_) {
         return _.address;
      }($));
   });
   var filterOrders = F2(function (orders,
   search) {
      return $String.isEmpty(search) ? orders : function () {
         var _v38 = $String.toInt(search);
         switch (_v38.ctor)
         {case "Err":
            return A2($List.filter,
              function (o) {
                 return A2($String.contains,
                 $String.toLower(search),
                 $String.toLower(o.name));
              },
              orders);
            case "Ok":
            return A2($List.filter,
              function (o) {
                 return A2($String.contains,
                 $Basics.toString(_v38._0),
                 $Basics.toString(o.number));
              },
              orders);}
         _U.badCase($moduleName,
         "between lines 248 and 250");
      }();
   });
   var ordersWithUnpaid = F2(function (orders,
   order) {
      return A2($List.map,
      function (o) {
         return _U.eq(o.id,
         order.id) ? _U.replace([["paid"
                                 ,false]],
         o) : o;
      },
      orders);
   });
   var ordersWithPaid = F2(function (orders,
   order) {
      return A2($List.map,
      function (o) {
         return _U.eq(o.id,
         order.id) ? _U.replace([["paid"
                                 ,true]],
         o) : o;
      },
      orders);
   });
   var startOrderValid = F3(function (model,
   name,
   email) {
      return function () {
         var _v41 = model.session;
         switch (_v41.ctor)
         {case "Anonymous":
            return A2($String.contains,
              "@",
              email) && $Basics.not($String.isEmpty(name));}
         return $Basics.not($String.isEmpty(name));
      }();
   });
   var updatePostalCode = F2(function (a,
   p) {
      return _U.replace([["postalCode"
                         ,p]],
      a);
   });
   var updateStreet = F2(function (a,
   s) {
      return _U.replace([["street"
                         ,s]],
      a);
   });
   var updateCity = F2(function (a,
   c) {
      return _U.replace([["city"
                         ,c]],
      a);
   });
   var forcefullyExtractAddress = function (model) {
      return function () {
         var _v42 = model.orderState;
         switch (_v42.ctor)
         {case "Ordering":
            return function () {
                 var _v44 = _v42._0.deliveryOption;
                 switch (_v44.ctor)
                 {case "Delivery":
                    return _v44._0;}
                 _U.badCase($moduleName,
                 "between lines 125 and 126");
              }();}
         _U.badCase($moduleName,
         "between lines 123 and 126");
      }();
   };
   var forcefullyExtractOrderInfo = function (model) {
      return function () {
         var _v46 = model.orderState;
         switch (_v46.ctor)
         {case "Ordering":
            return _v46._0;}
         _U.badCase($moduleName,
         "between lines 118 and 119");
      }();
   };
   var forcefullyExtractDeliveryOption = function (model) {
      return function () {
         var _v48 = model.orderState;
         switch (_v48.ctor)
         {case "Ordering":
            return _v48._0.deliveryOption;}
         _U.badCase($moduleName,
         "between lines 113 and 114");
      }();
   };
   var emptyAddress = {_: {}
                      ,city: ""
                      ,postalCode: ""
                      ,street: ""};
   var Model = function (a) {
      return function (b) {
         return function (c) {
            return function (d) {
               return function (e) {
                  return function (f) {
                     return function (g) {
                        return function (h) {
                           return function (i) {
                              return function (j) {
                                 return function (k) {
                                    return function (l) {
                                       return {_: {}
                                              ,currentOrder: k
                                              ,flashMessage: f
                                              ,formInput: a
                                              ,gigs: c
                                              ,innerFlashMessage: g
                                              ,loginFields: h
                                              ,orderState: d
                                              ,orders: j
                                              ,page: e
                                              ,searchField: l
                                              ,session: i
                                              ,stand: b};
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   var Admin = function (a) {
      return {ctor: "Admin",_0: a};
   };
   var User = function (a) {
      return {ctor: "User",_0: a};
   };
   var Anonymous = {ctor: "Anonymous"};
   var Credentials = F2(function (a,
   b) {
      return {_: {}
             ,name: a
             ,password: b};
   });
   var Order = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,address: i
             ,createdAt: b
             ,email: d
             ,id: a
             ,name: c
             ,number: h
             ,paid: e
             ,reducedCount: f
             ,seatIds: g};
   });
   var Gig = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,date: b
             ,freeSeats: d
             ,id: a
             ,title: c};
   });
   var CurrentFormInput = F3(function (a,
   b,
   c) {
      return {_: {}
             ,email: b
             ,name: a
             ,reduced: c};
   });
   var Address$ = F3(function (a,
   b,
   c) {
      return {_: {}
             ,city: c
             ,postalCode: b
             ,street: a};
   });
   var Ordered = function (a) {
      return {ctor: "Ordered"
             ,_0: a};
   };
   var Browsing = {ctor: "Browsing"};
   var Ordering = function (a) {
      return {ctor: "Ordering"
             ,_0: a};
   };
   var OrderInfo = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,deliveryOption: d
             ,email: b
             ,id: c
             ,name: a};
   });
   var Delivery = function (a) {
      return {ctor: "Delivery"
             ,_0: a};
   };
   var updateAddress = F2(function (model,
   address) {
      return function () {
         var _v50 = model.orderState;
         switch (_v50.ctor)
         {case "Ordering":
            return function () {
                 var _v52 = _v50._0.deliveryOption;
                 switch (_v52.ctor)
                 {case "Delivery":
                    return _U.replace([["orderState"
                                       ,Ordering(_U.replace([["deliveryOption"
                                                             ,Delivery(address)]],
                                       _v50._0))]],
                      model);}
                 return model;
              }();}
         return model;
      }();
   });
   var PickUpBeforehand = {ctor: "PickUpBeforehand"};
   var PickUpBoxOffice = {ctor: "PickUpBoxOffice"};
   var Hidden = {ctor: "Hidden"};
   var Error = function (a) {
      return {ctor: "Error",_0: a};
   };
   var Info = function (a) {
      return {ctor: "Info",_0: a};
   };
   var Success = function (a) {
      return {ctor: "Success"
             ,_0: a};
   };
   var GigView = function (a) {
      return {ctor: "GigView"
             ,_0: a};
   };
   var GigIndex = {ctor: "GigIndex"};
   var initialModel = {_: {}
                      ,currentOrder: $Maybe.Nothing
                      ,flashMessage: Hidden
                      ,formInput: {_: {}
                                  ,email: ""
                                  ,name: ""
                                  ,reduced: "0"}
                      ,gigs: _L.fromArray([])
                      ,innerFlashMessage: Hidden
                      ,loginFields: $Maybe.Nothing
                      ,orderState: Browsing
                      ,orders: _L.fromArray([])
                      ,page: GigIndex
                      ,searchField: ""
                      ,session: Anonymous
                      ,stand: $Model.initialModel};
   var CancelOrderRequest = function (a) {
      return {ctor: "CancelOrderRequest"
             ,_0: a};
   };
   var ListOrderRequest = function (a) {
      return {ctor: "ListOrderRequest"
             ,_0: a};
   };
   var UnpaidRequest = F2(function (a,
   b) {
      return {ctor: "UnpaidRequest"
             ,_0: a
             ,_1: b};
   });
   var PaidRequest = F2(function (a,
   b) {
      return {ctor: "PaidRequest"
             ,_0: a
             ,_1: b};
   });
   var LoginRequest = function (a) {
      return {ctor: "LoginRequest"
             ,_0: a};
   };
   var FinishOrderWithAddressRequest = F3(function (a,
   b,
   c) {
      return {ctor: "FinishOrderWithAddressRequest"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var FinishOrderRequest = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "FinishOrderRequest"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var FreeSeatRequest = F2(function (a,
   b) {
      return {ctor: "FreeSeatRequest"
             ,_0: a
             ,_1: b};
   });
   var ReserveSeatRequest = F2(function (a,
   b) {
      return {ctor: "ReserveSeatRequest"
             ,_0: a
             ,_1: b};
   });
   var StartOrderRequest = F2(function (a,
   b) {
      return {ctor: "StartOrderRequest"
             ,_0: a
             ,_1: b};
   });
   var SubmitOrder = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "SubmitOrder"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var FetchSeats = function (a) {
      return {ctor: "FetchSeats"
             ,_0: a};
   };
   var update = F2(function (action,
   _v54) {
      return function () {
         switch (_v54.ctor)
         {case "_Tuple2":
            return function () {
                 switch (action.ctor)
                 {case "CancelOrderSucceeded":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["stand"
                                            ,A2($Model.freeSeats,
                                            $Model.clearSelections(_v54._0.stand),
                                            action._0.seatIds)]
                                           ,["orders"
                                            ,A2($List.filter,
                                            function (o) {
                                               return !_U.eq(o.id,
                                               action._0.id);
                                            },
                                            _v54._0.orders)]
                                           ,["currentOrder"
                                            ,$Maybe.Nothing]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "ClickAdmin":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["loginFields"
                                            ,$Maybe.Just({_: {}
                                                         ,name: ""
                                                         ,password: ""})]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "ClickCancelOrder":
                    return {ctor: "_Tuple2"
                           ,_0: _v54._0
                           ,_1: $Maybe.Just(CancelOrderRequest(action._0))};
                    case "ClickGig":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["page"
                                            ,GigView(action._0)]],
                           _v54._0)
                           ,_1: $Maybe.Just(FetchSeats(action._0.id))};
                    case "ClickOrder":
                    return function () {
                         var _v102 = _v54._0.orderState;
                         switch (_v102.ctor)
                         {case "Ordering":
                            return {ctor: "_Tuple2"
                                   ,_0: _U.replace([["currentOrder"
                                                    ,$Maybe.Just(action._0)]],
                                   _v54._0)
                                   ,_1: $Maybe.Nothing};}
                         return {ctor: "_Tuple2"
                                ,_0: _U.replace([["currentOrder"
                                                 ,$Maybe.Just(action._0)]
                                                ,["stand"
                                                 ,A2($Model.selectSeatIds,
                                                 $Model.clearSelections(_v54._0.stand),
                                                 action._0.seatIds)]],
                                _v54._0)
                                ,_1: $Maybe.Nothing};
                      }();
                    case "ClickPaid":
                    return function () {
                         var _v104 = _v54._0.session;
                         switch (_v104.ctor)
                         {case "Admin":
                            return {ctor: "_Tuple2"
                                   ,_0: _v54._0
                                   ,_1: $Maybe.Just(A2(PaidRequest,
                                   _v104._0,
                                   action._0))};}
                         return {ctor: "_Tuple2"
                                ,_0: _v54._0
                                ,_1: $Maybe.Nothing};
                      }();
                    case "ClickSeat":
                    return function () {
                         var _v106 = _v54._0.orderState;
                         switch (_v106.ctor)
                         {case "Ordering":
                            return A2($Model.isSelected,
                              _v54._0.stand,
                              action._0) ? {ctor: "_Tuple2"
                                           ,_0: _v54._0
                                           ,_1: $Maybe.Just(A2(FreeSeatRequest,
                                           _v106._0.id,
                                           action._0.id))} : {ctor: "_Tuple2"
                                                             ,_0: _v54._0
                                                             ,_1: $Maybe.Just(A2(ReserveSeatRequest,
                                                             _v106._0.id,
                                                             action._0.id))};}
                         return {ctor: "_Tuple2"
                                ,_0: _v54._0
                                ,_1: $Maybe.Nothing};
                      }();
                    case "ClickUnpaid":
                    return function () {
                         var _v108 = _v54._0.session;
                         switch (_v108.ctor)
                         {case "Admin":
                            return {ctor: "_Tuple2"
                                   ,_0: _v54._0
                                   ,_1: $Maybe.Just(A2(UnpaidRequest,
                                   _v108._0,
                                   action._0))};}
                         return {ctor: "_Tuple2"
                                ,_0: _v54._0
                                ,_1: $Maybe.Nothing};
                      }();
                    case "CloseFlashMessage":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["flashMessage"
                                            ,Hidden]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "FinishOrderTicket":
                    return function () {
                         var request = function () {
                            var _v110 = forcefullyExtractDeliveryOption(_v54._0);
                            switch (_v110.ctor)
                            {case "Delivery":
                               return A3(FinishOrderWithAddressRequest,
                                 action._0,
                                 unwrapMaybe(reducedCount(_v54._0)),
                                 forcefullyExtractAddress(_v54._0));}
                            return A4(FinishOrderRequest,
                            action._0,
                            forcefullyExtractDeliveryOption(_v54._0),
                            unwrapMaybe(reducedCount(_v54._0)),
                            typeFromDeliveryOption(forcefullyExtractDeliveryOption(_v54._0)));
                         }();
                         return {ctor: "_Tuple2"
                                ,_0: _v54._0
                                ,_1: $Maybe.Just(request)};
                      }();
                    case "GigsReceived":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["gigs"
                                            ,action._0]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "HttpOrderFailed":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["flashMessage"
                                            ,Error(action._0)]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "Login":
                    return {ctor: "_Tuple2"
                           ,_0: _v54._0
                           ,_1: $Maybe.Just(LoginRequest(action._0))};
                    case "LoginAsAdminSucceeded":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["session"
                                            ,Admin(action._0)]],
                           _v54._0)
                           ,_1: $Maybe.Just(ListOrderRequest(action._0))};
                    case "LoginAsUserSucceeded":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["session"
                                            ,User(action._0)]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "NoOp":
                    return {ctor: "_Tuple2"
                           ,_0: _v54._0
                           ,_1: $Maybe.Nothing};
                    case "OrderStarted":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["innerFlashMessage"
                                            ,Hidden]
                                           ,["orderState"
                                            ,Ordering({_: {}
                                                      ,deliveryOption: PickUpBoxOffice
                                                      ,email: _v54._0.formInput.email
                                                      ,id: action._0
                                                      ,name: _v54._0.formInput.name})]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "OrderSucceeded":
                    return function () {
                         var _v112 = _v54._0.session;
                         switch (_v112.ctor)
                         {case "Admin":
                            return {ctor: "_Tuple2"
                                   ,_0: _U.replace([["orderState"
                                                    ,Ordered(forcefullyExtractOrderInfo(_v54._0))]],
                                   _v54._0)
                                   ,_1: $Maybe.Just(ListOrderRequest(_v112._0))};}
                         return {ctor: "_Tuple2"
                                ,_0: _U.replace([["orderState"
                                                 ,Ordered(forcefullyExtractOrderInfo(_v54._0))]],
                                _v54._0)
                                ,_1: $Maybe.Nothing};
                      }();
                    case "OrderTicket":
                    return function () {
                         var _v114 = $String.toInt(action._4);
                         switch (_v114.ctor)
                         {case "Err":
                            return {ctor: "_Tuple2"
                                   ,_0: _v54._0
                                   ,_1: $Maybe.Nothing};
                            case "Ok":
                            return {ctor: "_Tuple2"
                                   ,_0: _U.replace([["stand"
                                                    ,$Model.clearSelections(A2($Model.reserveSeats,
                                                    _v54._0.stand,
                                                    action._3))]],
                                   _v54._0)
                                   ,_1: $Maybe.Just(A5(SubmitOrder,
                                   action._0.id,
                                   action._1,
                                   action._2,
                                   action._3,
                                   _v114._0))};}
                         _U.badCase($moduleName,
                         "between lines 173 and 176");
                      }();
                    case "OrdersReceived":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["orders"
                                            ,action._0]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "PayOrder":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["orders"
                                            ,A2(ordersWithPaid,
                                            _v54._0.orders,
                                            action._0)]
                                           ,["currentOrder"
                                            ,$Maybe.Just(_U.replace([["paid"
                                                                     ,true]],
                                            action._0))]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "ReservationsReceived":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["stand"
                                            ,A2($Model.updateReservations,
                                            _v54._0.stand,
                                            action._0)]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "ResetApp":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["orderState"
                                            ,Browsing]
                                           ,["stand"
                                            ,$Model.clearSelections(A2($Model.reserveSeats,
                                            _v54._0.stand,
                                            _v54._0.stand.selections))]
                                           ,["flashMessage",Hidden]
                                           ,["formInput"
                                            ,{_: {}
                                             ,email: ""
                                             ,name: ""
                                             ,reduced: "0"}]
                                           ,["innerFlashMessage",Hidden]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "SeatReserved":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["stand"
                                            ,A2($Model.reserveSeats,
                                            _v54._0.stand,
                                            _L.fromArray([unwrapMaybe(A2($Model.findSeat,
                                            _v54._0.stand,
                                            action._0))]))]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "SeatSelected":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["stand"
                                            ,$Model.selectSeat(_v54._0.stand)(unwrapMaybe(A2($Model.findSeat,
                                            _v54._0.stand,
                                            action._0)))]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "SeatUnselected":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["stand"
                                            ,A2($Model.freeSeats,
                                            $Model.unselectSeat(_v54._0.stand)(unwrapMaybe(A2($Model.findSeat,
                                            _v54._0.stand,
                                            action._0))),
                                            _L.fromArray([action._0]))]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "SeatsReceived":
                    switch (action._0.ctor)
                      {case "_Tuple2":
                         return function () {
                              var _v117 = _v54._0.orderState;
                              switch (_v117.ctor)
                              {case "Ordering":
                                 return {ctor: "_Tuple2"
                                        ,_0: _U.replace([["stand"
                                                         ,A3($Model.updateSeats,
                                                         _v54._0.stand,
                                                         action._0._0,
                                                         action._0._1)]],
                                        _v54._0)
                                        ,_1: $Maybe.Nothing};}
                              return function () {
                                 var _v119 = _v54._0.currentOrder;
                                 switch (_v119.ctor)
                                 {case "Just":
                                    return {ctor: "_Tuple2"
                                           ,_0: _U.replace([["stand"
                                                            ,A2($Model.selectSeatIds,
                                                            A3($Model.updateSeats,
                                                            _v54._0.stand,
                                                            action._0._0,
                                                            action._0._1),
                                                            _v119._0.seatIds)]],
                                           _v54._0)
                                           ,_1: $Maybe.Nothing};
                                    case "Nothing":
                                    return {ctor: "_Tuple2"
                                           ,_0: _U.replace([["stand"
                                                            ,A3($Model.updateSeats,
                                                            _v54._0.stand,
                                                            action._0._0,
                                                            action._0._1)]],
                                           _v54._0)
                                           ,_1: $Maybe.Nothing};}
                                 _U.badCase($moduleName,
                                 "between lines 159 and 162");
                              }();
                           }();}
                      break;
                    case "ShowErrorFlashMessage":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["flashMessage"
                                            ,Error(action._0)]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "StartOrder":
                    return A3(startOrderValid,
                      _v54._0,
                      action._0,
                      action._1) ? {ctor: "_Tuple2"
                                   ,_0: _U.replace([["stand"
                                                    ,$Model.clearSelections(_v54._0.stand)]],
                                   _v54._0)
                                   ,_1: $Maybe.Just(A2(StartOrderRequest,
                                   action._0,
                                   action._1))} : {ctor: "_Tuple2"
                                                  ,_0: _U.replace([["innerFlashMessage"
                                                                   ,Error("Name oder E-Mail nicht vorhanden.")]],
                                                  _v54._0)
                                                  ,_1: $Maybe.Nothing};
                    case "TypeSearch":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["searchField"
                                            ,action._0]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "UnpayOrder":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["orders"
                                            ,A2(ordersWithUnpaid,
                                            _v54._0.orders,
                                            action._0)]
                                           ,["currentOrder"
                                            ,$Maybe.Just(_U.replace([["paid"
                                                                     ,false]],
                                            action._0))]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "UpdateCity":
                    return {ctor: "_Tuple2"
                           ,_0: A2(updateAddress,
                           _v54._0,
                           A2(updateCity,
                           forcefullyExtractAddress(_v54._0),
                           action._0))
                           ,_1: $Maybe.Nothing};
                    case "UpdateDeliveryOption":
                    return function () {
                         var _v121 = _v54._0.orderState;
                         switch (_v121.ctor)
                         {case "Browsing":
                            return {ctor: "_Tuple2"
                                   ,_0: _v54._0
                                   ,_1: $Maybe.Nothing};
                            case "Ordering":
                            return {ctor: "_Tuple2"
                                   ,_0: _U.replace([["orderState"
                                                    ,Ordering({_: {}
                                                              ,deliveryOption: action._0
                                                              ,email: _v121._0.email
                                                              ,id: _v121._0.id
                                                              ,name: _v121._0.name})]],
                                   _v54._0)
                                   ,_1: $Maybe.Nothing};}
                         _U.badCase($moduleName,
                         "between lines 181 and 184");
                      }();
                    case "UpdateEmail":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["formInput"
                                            ,{_: {}
                                             ,email: action._0
                                             ,name: _v54._0.formInput.name
                                             ,reduced: _v54._0.formInput.reduced}]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "UpdateLoginName":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["loginFields"
                                            ,$Maybe.Just({_: {}
                                                         ,name: action._0
                                                         ,password: function (_) {
                                                            return _.password;
                                                         }(unwrapMaybe(_v54._0.loginFields))})]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "UpdateLoginPassword":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["loginFields"
                                            ,$Maybe.Just({_: {}
                                                         ,name: function (_) {
                                                            return _.name;
                                                         }(unwrapMaybe(_v54._0.loginFields))
                                                         ,password: action._0})]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "UpdateName":
                    return {ctor: "_Tuple2"
                           ,_0: _U.replace([["formInput"
                                            ,{_: {}
                                             ,email: _v54._0.formInput.email
                                             ,name: action._0
                                             ,reduced: _v54._0.formInput.reduced}]],
                           _v54._0)
                           ,_1: $Maybe.Nothing};
                    case "UpdatePostalCode":
                    return {ctor: "_Tuple2"
                           ,_0: A2(updateAddress,
                           _v54._0,
                           A2(updatePostalCode,
                           forcefullyExtractAddress(_v54._0),
                           action._0))
                           ,_1: $Maybe.Nothing};
                    case "UpdateReducedCount":
                    return A2(reducedCountValid,
                      action._0,
                      $List.length(_v54._0.stand.selections)) ? {ctor: "_Tuple2"
                                                                ,_0: _U.replace([["innerFlashMessage"
                                                                                 ,Hidden]
                                                                                ,["formInput"
                                                                                 ,{_: {}
                                                                                  ,email: _v54._0.formInput.email
                                                                                  ,name: _v54._0.formInput.name
                                                                                  ,reduced: action._0}]],
                                                                _v54._0)
                                                                ,_1: $Maybe.Nothing} : {ctor: "_Tuple2"
                                                                                       ,_0: _U.replace([["innerFlashMessage"
                                                                                                        ,Error("Bitte gültige Anzahl an ermäßigten Karten angeben.")]
                                                                                                       ,["formInput"
                                                                                                        ,{_: {}
                                                                                                         ,email: _v54._0.formInput.email
                                                                                                         ,name: _v54._0.formInput.name
                                                                                                         ,reduced: action._0}]],
                                                                                       _v54._0)
                                                                                       ,_1: $Maybe.Nothing};
                    case "UpdateStreet":
                    return {ctor: "_Tuple2"
                           ,_0: A2(updateAddress,
                           _v54._0,
                           A2(updateStreet,
                           forcefullyExtractAddress(_v54._0),
                           action._0))
                           ,_1: $Maybe.Nothing};}
                 _U.badCase($moduleName,
                 "between lines 139 and 226");
              }();}
         _U.badCase($moduleName,
         "between lines 139 and 226");
      }();
   });
   var LoginAsAdminSucceeded = function (a) {
      return {ctor: "LoginAsAdminSucceeded"
             ,_0: a};
   };
   var LoginAsUserSucceeded = function (a) {
      return {ctor: "LoginAsUserSucceeded"
             ,_0: a};
   };
   var Login = function (a) {
      return {ctor: "Login",_0: a};
   };
   var UpdateLoginName = function (a) {
      return {ctor: "UpdateLoginName"
             ,_0: a};
   };
   var UpdateLoginPassword = function (a) {
      return {ctor: "UpdateLoginPassword"
             ,_0: a};
   };
   var ClickAdmin = {ctor: "ClickAdmin"};
   var FinishOrderTicket = F2(function (a,
   b) {
      return {ctor: "FinishOrderTicket"
             ,_0: a
             ,_1: b};
   });
   var UpdateDeliveryOption = function (a) {
      return {ctor: "UpdateDeliveryOption"
             ,_0: a};
   };
   var ShowErrorFlashMessage = function (a) {
      return {ctor: "ShowErrorFlashMessage"
             ,_0: a};
   };
   var TypeSearch = function (a) {
      return {ctor: "TypeSearch"
             ,_0: a};
   };
   var viewOrderFilterTextField = F2(function (address,
   model) {
      return A2($Html.div,
      _L.fromArray([$Html$Attributes.$class("row")]),
      _L.fromArray([A2($Html.div,
      _L.fromArray([$Html$Attributes.$class("form-group")]),
      _L.fromArray([A2($Html.div,
      _L.fromArray([$Html$Attributes.$class("input-group")]),
      _L.fromArray([A2($Html.div,
                   _L.fromArray([$Html$Attributes.$class("input-group-addon")]),
                   _L.fromArray([A2($Html.span,
                   _L.fromArray([$Html$Attributes.$class("glyphicon glyphicon-search")]),
                   _L.fromArray([]))]))
                   ,A2($Html.input,
                   _L.fromArray([A2($Html$Events.on,
                                "input",
                                $Html$Events.targetValue)(function ($) {
                                   return $Signal.message(address)(TypeSearch($));
                                })
                                ,$Html$Attributes.type$("text")
                                ,$Html$Attributes.$class("form-control")
                                ,$Html$Attributes.placeholder("Suche...")]),
                   _L.fromArray([]))]))]))]));
   });
   var SeatUnselected = function (a) {
      return {ctor: "SeatUnselected"
             ,_0: a};
   };
   var SeatSelected = function (a) {
      return {ctor: "SeatSelected"
             ,_0: a};
   };
   var SeatReserved = function (a) {
      return {ctor: "SeatReserved"
             ,_0: a};
   };
   var OrderStarted = function (a) {
      return {ctor: "OrderStarted"
             ,_0: a};
   };
   var StartOrder = F2(function (a,
   b) {
      return {ctor: "StartOrder"
             ,_0: a
             ,_1: b};
   });
   var CloseFlashMessage = {ctor: "CloseFlashMessage"};
   var HttpOrderFailed = function (a) {
      return {ctor: "HttpOrderFailed"
             ,_0: a};
   };
   var UpdateCity = function (a) {
      return {ctor: "UpdateCity"
             ,_0: a};
   };
   var UpdatePostalCode = function (a) {
      return {ctor: "UpdatePostalCode"
             ,_0: a};
   };
   var UpdateStreet = function (a) {
      return {ctor: "UpdateStreet"
             ,_0: a};
   };
   var UpdateReducedCount = function (a) {
      return {ctor: "UpdateReducedCount"
             ,_0: a};
   };
   var viewOrderFinishForm = F3(function (address,
   order,
   model) {
      return function () {
         var addressForm = F2(function (address,
         model) {
            return function () {
               var _v123 = model.orderState;
               switch (_v123.ctor)
               {case "Ordering":
                  return function () {
                       var _v125 = _v123._0.deliveryOption;
                       switch (_v125.ctor)
                       {case "Delivery":
                          return A2($Html.div,
                            _L.fromArray([]),
                            _L.fromArray([A2($Html.div,
                                         _L.fromArray([$Html$Attributes.$class("row")]),
                                         _L.fromArray([A2($Html.div,
                                         _L.fromArray([$Html$Attributes.$class("col-md-12")]),
                                         _L.fromArray([A5(textInput,
                                         address,
                                         UpdateStreet,
                                         "street",
                                         "Straße",
                                         _v125._0.street)]))]))
                                         ,A2($Html.div,
                                         _L.fromArray([$Html$Attributes.$class("row")]),
                                         _L.fromArray([A2($Html.div,
                                                      _L.fromArray([$Html$Attributes.$class("col-md-4")]),
                                                      _L.fromArray([A5(textInput,
                                                      address,
                                                      UpdatePostalCode,
                                                      "postalcode",
                                                      "PLZ",
                                                      _v125._0.postalCode)]))
                                                      ,A2($Html.div,
                                                      _L.fromArray([$Html$Attributes.$class("col-md-8")]),
                                                      _L.fromArray([A5(textInput,
                                                      address,
                                                      UpdateCity,
                                                      "city",
                                                      "Stadt",
                                                      _v125._0.city)]))]))]));}
                       return A2($Html.div,
                       _L.fromArray([]),
                       _L.fromArray([]));
                    }();}
               return A2($Html.div,
               _L.fromArray([]),
               _L.fromArray([]));
            }();
         });
         return A2($Html.div,
         _L.fromArray([]),
         _L.fromArray([A2($Html.div,
                      _L.fromArray([$Html$Attributes.$class("row")]),
                      _L.fromArray([A2($Html.div,
                      _L.fromArray([$Html$Attributes.$class("col-md-6")]),
                      _L.fromArray([A5(numberInput,
                                   address,
                                   UpdateReducedCount,
                                   "reduced",
                                   "davon ermäßigte Karten",
                                   model.formInput.reduced)
                                   ,A5(radioInputs,
                                   address,
                                   UpdateDeliveryOption,
                                   "deliveryOption",
                                   _L.fromArray([PickUpBoxOffice
                                                ,PickUpBeforehand]),
                                   PickUpBoxOffice)
                                   ,A2(addressForm,
                                   address,
                                   model)]))]))
                      ,A2($Html.div,
                      _L.fromArray([$Html$Attributes.$class("row")]),
                      _L.fromArray([A2($Html.div,
                      _L.fromArray([$Html$Attributes.$class("col-md-12")]),
                      _L.fromArray([A2($Html.button,
                      _L.fromArray([$Html$Attributes.disabled($Basics.not(A2(reducedCountValid,
                                   model.formInput.reduced,
                                   $List.length(model.stand.selections))))
                                   ,A2($Html$Events.onClick,
                                   address,
                                   A2(FinishOrderTicket,
                                   order.id,
                                   model.formInput.reduced))
                                   ,$Html$Attributes.$class("btn btn-primary")]),
                      _L.fromArray([$Html.text("Bestellen")]))]))]))]));
      }();
   });
   var UpdateName = function (a) {
      return {ctor: "UpdateName"
             ,_0: a};
   };
   var UpdateEmail = function (a) {
      return {ctor: "UpdateEmail"
             ,_0: a};
   };
   var viewRegisterForm = F2(function (address,
   form) {
      return A2($Html.div,
      _L.fromArray([]),
      _L.fromArray([A5(textInput,
                   address,
                   UpdateName,
                   "name",
                   "Name, Vorname",
                   form.name)
                   ,A5(emailInput,
                   address,
                   UpdateEmail,
                   "email",
                   "E-Mail-Adresse",
                   form.email)
                   ,A2($Html.button,
                   _L.fromArray([A2($Html$Events.onClick,
                                address,
                                A2(StartOrder,
                                form.name,
                                form.email))
                                ,$Html$Attributes.$class("btn btn-primary")]),
                   _L.fromArray([$Html.text("Anmelden")]))]));
   });
   var OrderTicket = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "OrderTicket"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var ReservationsReceived = function (a) {
      return {ctor: "ReservationsReceived"
             ,_0: a};
   };
   var CancelOrderSucceeded = function (a) {
      return {ctor: "CancelOrderSucceeded"
             ,_0: a};
   };
   var ClickCancelOrder = function (a) {
      return {ctor: "ClickCancelOrder"
             ,_0: a};
   };
   var ClickUnpaid = function (a) {
      return {ctor: "ClickUnpaid"
             ,_0: a};
   };
   var ClickPaid = function (a) {
      return {ctor: "ClickPaid"
             ,_0: a};
   };
   var viewOrderDetail = F2(function (address,
   model) {
      return function () {
         var addressList = function (address) {
            return function () {
               switch (address.ctor)
               {case "Just":
                  return A2($Html.dl,
                    _L.fromArray([]),
                    _L.fromArray([A2($Html.dt,
                                 _L.fromArray([]),
                                 _L.fromArray([$Html.text("Straße")]))
                                 ,A2($Html.dd,
                                 _L.fromArray([]),
                                 _L.fromArray([$Html.text(address._0.street)]))
                                 ,A2($Html.dt,
                                 _L.fromArray([]),
                                 _L.fromArray([$Html.text("PLZ")]))
                                 ,A2($Html.dd,
                                 _L.fromArray([]),
                                 _L.fromArray([$Html.text(address._0.postalCode)]))
                                 ,A2($Html.dt,
                                 _L.fromArray([]),
                                 _L.fromArray([$Html.text("Stadt")]))
                                 ,A2($Html.dd,
                                 _L.fromArray([]),
                                 _L.fromArray([$Html.text(address._0.city)]))]));
                  case "Nothing":
                  return $Html.text("");}
               _U.badCase($moduleName,
               "between lines 495 and 507");
            }();
         };
         var mailTo = function (email) {
            return A2($Basics._op["++"],
            "mailto:",
            email);
         };
         var cancelButton = function (order) {
            return A2($Html.button,
            _L.fromArray([A2($Html$Events.onClick,
                         address,
                         ClickCancelOrder(order))
                         ,$Html$Attributes.$class("btn btn-danger")]),
            _L.fromArray([$Html.text("Bestellung stornieren")]));
         };
         var paidButton = function (order) {
            return $Basics.not(order.paid) ? A2($Html.button,
            _L.fromArray([A2($Html$Events.onClick,
                         address,
                         ClickPaid(order))
                         ,$Html$Attributes.$class("btn btn-primary")]),
            _L.fromArray([$Html.text("Bezahlt!")])) : A2($Html.button,
            _L.fromArray([A2($Html$Events.onClick,
                         address,
                         ClickUnpaid(order))
                         ,$Html$Attributes.$class("btn btn-warning")]),
            _L.fromArray([$Html.text("Bezahlung widerrufen!")]));
         };
         return function () {
            var _v129 = model.currentOrder;
            switch (_v129.ctor)
            {case "Just":
               return A2($Html.div,
                 _L.fromArray([$Html$Attributes.$class("panel panel-default")]),
                 _L.fromArray([A2($Html.div,
                              _L.fromArray([$Html$Attributes.$class("panel-heading")]),
                              _L.fromArray([A2($Html.h3,
                              _L.fromArray([$Html$Attributes.$class("panel-title")]),
                              _L.fromArray([$Html.text("Bestellungsdetail")]))]))
                              ,A2($Html.div,
                              _L.fromArray([$Html$Attributes.$class("panel-body")]),
                              _L.fromArray([A2($Html.h2,
                                           _L.fromArray([]),
                                           _L.fromArray([$Html.text(_v129._0.name)
                                                        ,A2($Html.small,
                                                        _L.fromArray([]),
                                                        _L.fromArray([$Html.text(A2($Basics._op["++"],
                                                        " #",
                                                        $Basics.toString(_v129._0.number)))]))]))
                                           ,A2($Html.p,
                                           _L.fromArray([]),
                                           _L.fromArray([A2($Html.a,
                                           _L.fromArray([$Html$Attributes.href(mailTo(_v129._0.email))]),
                                           _L.fromArray([$Html.text(_v129._0.email)]))]))
                                           ,viewOrderTable$(_v129._0)
                                           ,paidButton(_v129._0)
                                           ,cancelButton(_v129._0)
                                           ,addressList(_v129._0.address)]))]));
               case "Nothing":
               return $Html.text("");}
            _U.badCase($moduleName,
            "between lines 508 and 528");
         }();
      }();
   });
   var ClickOrder = function (a) {
      return {ctor: "ClickOrder"
             ,_0: a};
   };
   var viewOrderList = F3(function (address,
   orders,
   currentOrder) {
      return function () {
         var countLabel = function (order) {
            return A2($Html.span,
            _L.fromArray([$Html$Attributes.$class("label label-success")]),
            _L.fromArray([$Html.text($Basics.toString($List.length(order.seatIds)))]));
         };
         var paidLabel = function (order) {
            return A2($Html.span,
            _L.fromArray([$Html$Attributes.$class(A2($Basics._op["++"],
            "label ",
            order.paid ? "label-success" : "label-warning"))]),
            _L.fromArray([$Html.text(order.paid ? "Bezahlt" : "Nicht bezahlt")]));
         };
         var orderItem = F2(function (currentOrder,
         order) {
            return A2($Html.li,
            _L.fromArray([A2($Html$Events.onClick,
                         address,
                         ClickOrder(order))
                         ,$Html$Attributes.style(_L.fromArray([{ctor: "_Tuple2"
                                                               ,_0: "cursor"
                                                               ,_1: "pointer"}]))
                         ,$Html$Attributes.$class(A2($Basics._op["++"],
                         "list-group-item",
                         _U.eq($Maybe.Just(order),
                         currentOrder) ? " active" : ""))]),
            _L.fromArray([A2($Html.span,
                         _L.fromArray([$Html$Attributes.$class("badge")]),
                         _L.fromArray([$Html.text(A2($Basics._op["++"],
                         "am ",
                         formatShortDate(order.createdAt)))]))
                         ,$Html.text(A2($Basics._op["++"],
                         "#",
                         A2($Basics._op["++"],
                         $Basics.toString(order.number),
                         A2($Basics._op["++"],
                         " ",
                         order.name))))
                         ,paidLabel(order)
                         ,countLabel(order)]));
         });
         return A2($Html.div,
         _L.fromArray([$Html$Attributes.$class("row")]),
         _L.fromArray([A2($Html.ul,
         _L.fromArray([$Html$Attributes.$class("list-group")]),
         A2($List.map,
         orderItem(currentOrder),
         orders))]));
      }();
   });
   var ClickGig = function (a) {
      return {ctor: "ClickGig"
             ,_0: a};
   };
   var drawGigEntry = F2(function (address,
   gig) {
      return A2($Html.li,
      _L.fromArray([]),
      _L.fromArray([A2($Html.a,
      _L.fromArray([emptyHref
                   ,A2($Html$Events.onClick,
                   address,
                   ClickGig(gig))]),
      _L.fromArray([$Html.text(A2($Basics._op["++"],
                   formatDate(gig.date),
                   " "))
                   ,A2($Html.span,
                   _L.fromArray([$Html$Attributes.$class("badge")]),
                   _L.fromArray([$Html.text(A2($Basics._op["++"],
                   $Basics.toString(gig.freeSeats),
                   " freie Plätze"))]))]))]));
   });
   var UnpayOrder = function (a) {
      return {ctor: "UnpayOrder"
             ,_0: a};
   };
   var PayOrder = function (a) {
      return {ctor: "PayOrder"
             ,_0: a};
   };
   var OrdersReceived = function (a) {
      return {ctor: "OrdersReceived"
             ,_0: a};
   };
   var GigsReceived = function (a) {
      return {ctor: "GigsReceived"
             ,_0: a};
   };
   var SeatsReceived = function (a) {
      return {ctor: "SeatsReceived"
             ,_0: a};
   };
   var ClickSeat = function (a) {
      return {ctor: "ClickSeat"
             ,_0: a};
   };
   var drawSeat = F4(function (address,
   model,
   rowNumber,
   seat) {
      return function () {
         var text = $Model.isUsable(seat) ? $Basics.toString(seat.number) : "";
         return A2($Svg.g,
         _L.fromArray([A2($Html$Events.onClick,
                      address,
                      ClickSeat(seat))
                      ,$Svg$Attributes.transform(A2($Basics._op["++"],
                      "translate(",
                      A2($Basics._op["++"],
                      $Basics.toString(seat.x * 18),
                      ", 0)")))]),
         _L.fromArray([A2($Svg.rect,
                      _L.fromArray([$Svg$Attributes.cursor("pointer")
                                   ,$Svg$Attributes.style(A2($Basics._op["++"],
                                   "fill:",
                                   A2($Basics._op["++"],
                                   A2(seatColor,model,seat),
                                   "; stroke-width: 1; stroke: rgb(0, 0, 0)")))
                                   ,$Svg$Attributes.width("18")
                                   ,$Svg$Attributes.height("18")]),
                      _L.fromArray([]))
                      ,A2($Svg.text,
                      _L.fromArray([$Svg$Attributes.x("9")
                                   ,$Svg$Attributes.y("13")
                                   ,$Svg$Attributes.cursor("pointer")
                                   ,$Svg$Attributes.textAnchor("middle")
                                   ,$Svg$Attributes.fontSize("11")]),
                      _L.fromArray([$Html.text(text)]))]));
      }();
   });
   var drawRow = F3(function (address,
   model,
   row) {
      return A2($Svg.g,
      _L.fromArray([$Svg$Attributes.transform(A2($Basics._op["++"],
      "translate(0,",
      A2($Basics._op["++"],
      $Basics.toString(row.y * 25),
      ")")))]),
      _L.fromArray([A2($Svg.g,
                   _L.fromArray([$Svg$Attributes.transform("translate(20, 0)")]),
                   A2($List.map,
                   A3(drawSeat,
                   address,
                   model,
                   row.number),
                   A2($Model.seatsInRow,
                   model,
                   row)))
                   ,A2($Svg.text,
                   _L.fromArray([$Svg$Attributes.x("5")
                                ,$Svg$Attributes.y("14")
                                ,$Svg$Attributes.textAnchor("end")]),
                   _L.fromArray([$Html.text($Basics.toString(row.number))]))
                   ,A2($Svg.text,
                   _L.fromArray([$Svg$Attributes.x("880")
                                ,$Svg$Attributes.y("14")
                                ,$Svg$Attributes.textAnchor("end")]),
                   _L.fromArray([$Html.text($Basics.toString(row.number))]))]));
   });
   var drawStand = F2(function (address,
   model) {
      return A2($Svg.svg,
      _L.fromArray([$Svg$Attributes.version("1.1")
                   ,$Svg$Attributes.x("0")
                   ,$Svg$Attributes.y("0")
                   ,$Svg$Attributes.height("500")
                   ,$Svg$Attributes.width("910")
                   ,$Svg$Attributes.style("display: block; margin: 0 auto;")]),
      _L.fromArray([A2($Svg.g,
                   _L.fromArray([$Svg$Attributes.transform("translate(18, 1)")]),
                   A2($List.map,
                   A2(drawRow,address,model),
                   $Model.rows(model)))
                   ,A2($Svg.line,
                   _L.fromArray([$Svg$Attributes.x1("200")
                                ,$Svg$Attributes.y1("450")
                                ,$Svg$Attributes.x2("750")
                                ,$Svg$Attributes.y2("450")
                                ,$Svg$Attributes.style("stroke:rgb(0,0,0);stroke-width:2")]),
                   _L.fromArray([]))
                   ,A2($Svg.text,
                   _L.fromArray([$Svg$Attributes.x("475")
                                ,$Svg$Attributes.y("475")
                                ,$Svg$Attributes.textAnchor("middle")
                                ,$Svg$Attributes.style("font-size: 16px")]),
                   _L.fromArray([$Html.text("BÜHNE")]))]));
   });
   var ResetApp = {ctor: "ResetApp"};
   var OrderSucceeded = {ctor: "OrderSucceeded"};
   var NoOp = {ctor: "NoOp"};
   var viewOrderPanel = F3(function (address,
   gig,
   model) {
      return function () {
         var _v131 = model.orderState;
         switch (_v131.ctor)
         {case "Browsing":
            return A2($Html.div,
              _L.fromArray([$Html$Attributes.$class("row")]),
              _L.fromArray([A2($Html.div,
              _L.fromArray([$Html$Attributes.$class("col-md-12")]),
              _L.fromArray([A2($Html.p,
                           _L.fromArray([]),
                           _L.fromArray([A2($Html.br,
                                        _L.fromArray([]),
                                        _L.fromArray([]))
                                        ,$Html.text("Bitte geben Sie Ihren Namen und eine gültige E-Mail-Adresse ein, um Sitzplätze per Mausklick auszuwählen.")
                                        ,A2($Html.br,
                                        _L.fromArray([]),
                                        _L.fromArray([]))
                                        ,$Html.text("Nachdem Sie sich angemeldet haben, können Sie zudem weitere Veranstaltungstage zu Ihrer Bestellung hinzufügen.")]))
                           ,A2($Html.br,
                           _L.fromArray([]),
                           _L.fromArray([]))
                           ,A2(viewRegisterForm,
                           address,
                           model.formInput)]))]));
            case "Ordered":
            return A2($Html.div,
              _L.fromArray([$Html$Attributes.$class("row")]),
              _L.fromArray([A2($Html.div,
              _L.fromArray([$Html$Attributes.$class("col-md-12")]),
              _L.fromArray([A3(viewFlashMessage,
                           address,
                           NoOp,
                           Success("Karten erfolgreich bestellt."))
                           ,A2($Html.button,
                           _L.fromArray([A2($Html$Events.onClick,
                                        address,
                                        ResetApp)
                                        ,$Html$Attributes.$class("btn btn-primary")]),
                           _L.fromArray([$Html.text("Weitere Karten bestellen")]))]))]));
            case "Ordering":
            return A2($Html.div,
              _L.fromArray([]),
              _L.fromArray([viewOrderInfos(_v131._0)
                           ,A2($Html.div,
                           _L.fromArray([]),
                           _L.fromArray([A2($Html.br,
                                        _L.fromArray([]),
                                        _L.fromArray([]))
                                        ,A2($Html.p,
                                        _L.fromArray([]),
                                        _L.fromArray([$Html.text("Nachdem Sie alle Sitzplätze ausgewählt und die Anzahl der ermäßigten Karten (Schüler und Studenten mit gültigem Ausweis) angegeben haben,\nklicken Sie zum Abschließen Ihrer Bestellung auf „Absenden“. Sie erhalten dann in Kürze eine Bestätigungs-E-Mail mit den Zahlungsinformationen.")]))
                                        ,A2($Html.br,
                                        _L.fromArray([]),
                                        _L.fromArray([]))]))
                           ,viewOrderTable(model)
                           ,A3(viewOrderFinishForm,
                           address,
                           _v131._0,
                           model)]));}
         _U.badCase($moduleName,
         "between lines 710 and 746");
      }();
   });
   var view = F2(function (address,
   model) {
      return function () {
         var loginForm = function (model) {
            return function () {
               var _v134 = model.session;
               switch (_v134.ctor)
               {case "Admin":
                  return A2($Html.p,
                    _L.fromArray([$Html$Attributes.$class("navbar-text navbar-right")]),
                    _L.fromArray([$Html.text("Eingeloggt als Admin")]));
                  case "Anonymous":
                  return function () {
                       var _v137 = model.loginFields;
                       switch (_v137.ctor)
                       {case "Just":
                          return A2($Html.form,
                            _L.fromArray([$Html$Attributes.$class("navbar-form navbar-right")]),
                            _L.fromArray([A5(textInput,
                                         address,
                                         UpdateLoginName,
                                         "user",
                                         "Username",
                                         _v137._0.name)
                                         ,A5(passwordInput,
                                         address,
                                         UpdateLoginPassword,
                                         "password",
                                         "Passwort",
                                         _v137._0.password)
                                         ,A2($Html.button,
                                         _L.fromArray([A2($Html$Events.onClick,
                                                      address,
                                                      Login(_v137._0))
                                                      ,$Html$Attributes.type$("button")
                                                      ,$Html$Attributes.$class("btn btn-primary")]),
                                         _L.fromArray([$Html.text("Login")]))]));
                          case "Nothing":
                          return A2($Html.form,
                            _L.fromArray([$Html$Attributes.$class("nav navbar-form navbar-right")]),
                            _L.fromArray([A2($Html.button,
                            _L.fromArray([A2($Html$Events.onClick,
                                         address,
                                         ClickAdmin)
                                         ,$Html$Attributes.type$("button")
                                         ,$Html$Attributes.$class("btn btn-default")]),
                            _L.fromArray([$Html.text("Admin")]))]));}
                       _U.badCase($moduleName,
                       "between lines 620 and 631");
                    }();
                  case "User": return A2($Html.p,
                    _L.fromArray([$Html$Attributes.$class("navbar-text navbar-right")]),
                    _L.fromArray([$Html.text("Eingeloggt als Benutzer")]));}
               _U.badCase($moduleName,
               "between lines 618 and 638");
            }();
         };
         var nav = function (model) {
            return A2($Html.nav,
            _L.fromArray([$Html$Attributes.$class("navbar navbar-default")]),
            _L.fromArray([A2($Html.div,
            _L.fromArray([$Html$Attributes.$class("container-fluid")]),
            _L.fromArray([A2($Html.div,
                         _L.fromArray([$Html$Attributes.$class("navbar-header")]),
                         _L.fromArray([A2($Html.a,
                         _L.fromArray([$Html$Attributes.$class("navbar-brand")]),
                         _L.fromArray([$Html.text("FLASHDANCE")]))]))
                         ,A2($Html.div,
                         _L.fromArray([$Html$Attributes.$class("collapse navbar-collapse")]),
                         _L.fromArray([loginForm(model)]))]))]));
         };
         var gigNavItem = F3(function (address,
         currentGig,
         gig) {
            return A2($Html.li,
            _U.eq(gig,
            currentGig) ? _L.fromArray([$Html$Attributes.$class("disabled")]) : _L.fromArray([]),
            _L.fromArray([A2($Html.a,
            _L.fromArray([emptyHref
                         ,A2($Html$Events.onClick,
                         address,
                         ClickGig(gig))]),
            _L.fromArray([$Html.text(formatDate(gig.date))]))]));
         });
         var innerHeader = A3(viewFlashMessage,
         address,
         CloseFlashMessage,
         model.innerFlashMessage);
         var header = A3(viewFlashMessage,
         address,
         CloseFlashMessage,
         model.flashMessage);
         var body = function (model) {
            return function () {
               var _v139 = model.page;
               switch (_v139.ctor)
               {case "GigIndex":
                  return _L.fromArray([A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("row")]),
                                      _L.fromArray([A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("col-md-12")]),
                                      _L.fromArray([A2($Html.h1,
                                                   _L.fromArray([]),
                                                   _L.fromArray([$Html.text("FLASHDANCE – The Musical | Tickets")]))
                                                   ,A2($Html.br,
                                                   _L.fromArray([]),
                                                   _L.fromArray([]))
                                                   ,A2($Html.p,
                                                   _L.fromArray([]),
                                                   _L.fromArray([$Html.text("Herzlich Willkommen beim Online-Ticketservice der HGR Musical AG!")
                                                                ,A2($Html.br,
                                                                _L.fromArray([]),
                                                                _L.fromArray([]))
                                                                ,$Html.text("Bitte wählen Sie einen Veranstaltungstag, um zu beginnen.")]))]))]))
                                      ,A2($Html.br,
                                      _L.fromArray([]),
                                      _L.fromArray([]))
                                      ,A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("row")]),
                                      _L.fromArray([A2($Html.div,
                                                   _L.fromArray([$Html$Attributes.$class("col-md-4")]),
                                                   _L.fromArray([A2($Html.ul,
                                                   _L.fromArray([$Html$Attributes.$class("nav nav-pills nav-stacked")]),
                                                   A2($List.map,
                                                   drawGigEntry(address),
                                                   model.gigs))]))
                                                   ,A2($Html.div,
                                                   _L.fromArray([$Html$Attributes.$class("col-md-8")]),
                                                   _L.fromArray([A2($Html.img,
                                                   _L.fromArray([$Html$Attributes.src("logo.png")]),
                                                   _L.fromArray([]))]))]))
                                      ,A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("row")]),
                                      _L.fromArray([A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("col-md-12")]),
                                      _L.fromArray([A2($Html.br,
                                                   _L.fromArray([]),
                                                   _L.fromArray([]))
                                                   ,A2($Html.p,
                                                   _L.fromArray([]),
                                                   _L.fromArray([$Html.text("What a feeling: Wir haben unsere Zuschauertribüne überarbeitet und bieten ab sofort 20% mehr Sitzfreiheit je Platz!")]))]))]))]);
                  case "GigView":
                  return _L.fromArray([A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("row")]),
                                      _L.fromArray([header]))
                                      ,A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("row")]),
                                      _L.fromArray([A2($Html.nav,
                                      _L.fromArray([]),
                                      _L.fromArray([A2($Html.ul,
                                      _L.fromArray([$Html$Attributes.$class("pager")]),
                                      A2($List.map,
                                      A2(gigNavItem,address,_v139._0),
                                      model.gigs))]))]))
                                      ,A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("row")]),
                                      _L.fromArray([A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("col-md-12")]),
                                      _L.fromArray([A2($Html.h1,
                                      _L.fromArray([]),
                                      _L.fromArray([$Html.text(A2($Basics._op["++"],
                                                   _v139._0.title,
                                                   " "))
                                                   ,A2($Html.small,
                                                   _L.fromArray([]),
                                                   _L.fromArray([$Html.text(formatDateTime(_v139._0.date))]))]))]))]))
                                      ,A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("row")]),
                                      _L.fromArray([A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("col-md-12")]),
                                      _L.fromArray([A2(drawStand,
                                      address,
                                      model.stand)]))]))
                                      ,A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("row")]),
                                      _L.fromArray([A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("col-md-12")]),
                                      _L.fromArray([A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("panel panel-default")]),
                                      _L.fromArray([A2($Html.div,
                                                   _L.fromArray([$Html$Attributes.$class("panel-heading")]),
                                                   _L.fromArray([A2($Html.h3,
                                                   _L.fromArray([$Html$Attributes.$class("panel-title")]),
                                                   _L.fromArray([$Html.text("Karten bestellen")]))]))
                                                   ,A2($Html.div,
                                                   _L.fromArray([$Html$Attributes.$class("panel-body")]),
                                                   _L.fromArray([innerHeader
                                                                ,A3(viewOrderPanel,
                                                                address,
                                                                _v139._0,
                                                                model)]))]))]))]))
                                      ,isAdmin(model.session) ? A2($Html.div,
                                      _L.fromArray([$Html$Attributes.$class("row")]),
                                      _L.fromArray([A2($Html.div,
                                                   _L.fromArray([$Html$Attributes.$class("col-md-4")]),
                                                   _L.fromArray([A2(viewOrderFilterTextField,
                                                                address,
                                                                model)
                                                                ,A2($Html.div,
                                                                _L.fromArray([$Html$Attributes.$class("row")]),
                                                                _L.fromArray([A2($Html.h3,
                                                                _L.fromArray([]),
                                                                _L.fromArray([$Html.text("Online-Bestellungen")
                                                                             ,A2($Html.small,
                                                                             _L.fromArray([]),
                                                                             _L.fromArray([$Html.text(" mit E-Mail")]))]))]))
                                                                ,A3(viewOrderList,
                                                                address,
                                                                A2(filterOrders,
                                                                ordersWithEmail(model.orders),
                                                                model.searchField),
                                                                model.currentOrder)
                                                                ,A2($Html.div,
                                                                _L.fromArray([$Html$Attributes.$class("row")]),
                                                                _L.fromArray([A2($Html.h3,
                                                                _L.fromArray([]),
                                                                _L.fromArray([$Html.text("Interne Bestellungen")
                                                                             ,A2($Html.small,
                                                                             _L.fromArray([]),
                                                                             _L.fromArray([$Html.text(" keine E-Mail")]))]))]))
                                                                ,A3(viewOrderList,
                                                                address,
                                                                A2(filterOrders,
                                                                ordersWithoutEmail(model.orders),
                                                                model.searchField),
                                                                model.currentOrder)
                                                                ,A2($Html.div,
                                                                _L.fromArray([$Html$Attributes.$class("row")]),
                                                                _L.fromArray([A2($Html.h3,
                                                                _L.fromArray([]),
                                                                _L.fromArray([$Html.text("Zu versendende Bestellungen")]))]))
                                                                ,A3(viewOrderList,
                                                                address,
                                                                A2(filterOrders,
                                                                ordersWithDelivery(model.orders),
                                                                model.searchField),
                                                                model.currentOrder)]))
                                                   ,A2($Html.div,
                                                   _L.fromArray([$Html$Attributes.$class("col-md-8")]),
                                                   _L.fromArray([A2(viewOrderDetail,
                                                   address,
                                                   model)]))])) : $Html.text("")]);}
               _U.badCase($moduleName,
               "between lines 544 and 617");
            }();
         };
         return A2($Html.div,
         _L.fromArray([$Html$Attributes.$class("container")]),
         A2($Basics._op["++"],
         _L.fromArray([nav(model)]),
         body(model)));
      }();
   });
   var actions = $Signal.mailbox(NoOp);
   var fetchGigs = Elm.Native.Task.make(_elm).perform(A2($Task.andThen,
   $HttpRequests.fetchGigs,
   function (gigs) {
      return A2($Signal.send,
      actions.address,
      GigsReceived(gigs));
   }));
   var input = actions.signal;
   var updatesWithEffect = A3($Signal.foldp,
   update,
   {ctor: "_Tuple2"
   ,_0: initialModel
   ,_1: $Maybe.Nothing},
   input);
   var effects = A3($Signal.filter,
   isJust,
   $Maybe.Nothing,
   A2($Signal.map,
   $Basics.snd,
   updatesWithEffect));
   var fetchOrders = Elm.Native.Task.make(_elm).performSignal("fetchOrders",
   function () {
      var maybeRequest = function (s) {
         return function () {
            switch (s.ctor)
            {case "Just": switch (s._0.ctor)
                 {case "ListOrderRequest":
                    return A2($Task.onError,
                      A2($Task.andThen,
                      $HttpRequests.orders,
                      function (orders) {
                         return A2($Signal.send,
                         actions.address,
                         OrdersReceived(orders));
                      }),
                      function (error) {
                         return A2($Signal.send,
                         actions.address,
                         ShowErrorFlashMessage($Basics.toString(error)));
                      });}
                 break;}
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      };
      return A2($Signal.map,
      maybeRequest,
      effects);
   }());
   var cancelOrder = Elm.Native.Task.make(_elm).performSignal("cancelOrder",
   function () {
      var maybeRequest = function (s) {
         return function () {
            switch (s.ctor)
            {case "Just": switch (s._0.ctor)
                 {case "CancelOrderRequest":
                    return A2($Task.onError,
                      A2($Task.andThen,
                      $HttpRequests.cancelOrder(s._0._0.id),
                      function (_v147) {
                         return function () {
                            return A2($Signal.send,
                            actions.address,
                            CancelOrderSucceeded(s._0._0));
                         }();
                      }),
                      function (error) {
                         return A2($Signal.send,
                         actions.address,
                         ShowErrorFlashMessage($Basics.toString(error)));
                      });}
                 break;}
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      };
      return A2($Signal.map,
      maybeRequest,
      effects);
   }());
   var seatsRequestGigIdSignal = function () {
      var toId = function (effect) {
         return function () {
            switch (effect.ctor)
            {case "Just":
               switch (effect._0.ctor)
                 {case "FetchSeats":
                    return $Maybe.Just(effect._0._0);}
                 break;}
            return $Maybe.Nothing;
         }();
      };
      return A2($Signal.map,
      toId,
      effects);
   }();
   var seatsRequest = Elm.Native.Task.make(_elm).performSignal("seatsRequest",
   function () {
      var send = function (gigId) {
         return function () {
            switch (gigId.ctor)
            {case "Just":
               return $Task$Extra.parallel(_L.fromArray([A2($Task.andThen,
                                                        $HttpRequests.fetchSeats(gigId._0),
                                                        function (r) {
                                                           return A2($Signal.send,
                                                           actions.address,
                                                           SeatsReceived(r));
                                                        })
                                                        ,A2($Task.andThen,
                                                        $HttpRequests.fetchReservations(gigId._0),
                                                        function (r) {
                                                           return A2($Signal.send,
                                                           actions.address,
                                                           ReservationsReceived(r));
                                                        })]));
               case "Nothing":
               return $Task.sequence(_L.fromArray([$Task.spawn($Task.succeed(_L.fromArray([{ctor: "_Tuple0"}])))]));}
            _U.badCase($moduleName,
            "between lines 300 and 308");
         }();
      };
      return A2($Signal.map,
      send,
      seatsRequestGigIdSignal);
   }());
   var orderRequest = Elm.Native.Task.make(_elm).performSignal("orderRequest",
   function () {
      var maybeRequest = function (s) {
         return function () {
            switch (s.ctor)
            {case "Just": switch (s._0.ctor)
                 {case "SubmitOrder":
                    return A2($Task.onError,
                      A2($Task.andThen,
                      A5($HttpRequests.submitOrder,
                      s._0._0,
                      s._0._1,
                      s._0._2,
                      A2($List.map,
                      function (_) {
                         return _.id;
                      },
                      s._0._3),
                      s._0._4),
                      function (result) {
                         return A2($Signal.send,
                         actions.address,
                         NoOp);
                      }),
                      function (error) {
                         return A2($Signal.send,
                         actions.address,
                         HttpOrderFailed(httpErrorToMessage(error)));
                      });}
                 break;}
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      };
      return A2($Signal.map,
      maybeRequest,
      effects);
   }());
   var orderStartRequest = Elm.Native.Task.make(_elm).performSignal("orderStartRequest",
   function () {
      var maybeRequest = function (s) {
         return function () {
            switch (s.ctor)
            {case "Just": switch (s._0.ctor)
                 {case "StartOrderRequest":
                    return A2($Task.andThen,
                      A2($HttpRequests.startOrder,
                      s._0._0,
                      s._0._1),
                      function (orderId) {
                         return A2($Signal.send,
                         actions.address,
                         OrderStarted(orderId));
                      });}
                 break;}
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      };
      return A2($Signal.map,
      maybeRequest,
      effects);
   }());
   var finishOrderRequest = Elm.Native.Task.make(_elm).performSignal("finishOrderRequest",
   function () {
      var maybeRequest = function (s) {
         return function () {
            switch (s.ctor)
            {case "Just": switch (s._0.ctor)
                 {case "FinishOrderRequest":
                    return A2($Task.andThen,
                      A3($HttpRequests.finishOrder,
                      s._0._0,
                      s._0._2,
                      typeFromDeliveryOption(s._0._1)),
                      function (_v174) {
                         return function () {
                            return A2($Signal.send,
                            actions.address,
                            OrderSucceeded);
                         }();
                      });
                    case "FinishOrderWithAddressRequest":
                    return A2($Task.andThen,
                      A5($HttpRequests.finishOrderWithAddress,
                      s._0._0,
                      s._0._1,
                      s._0._2.street,
                      s._0._2.postalCode,
                      s._0._2.city),
                      function (_v176) {
                         return function () {
                            return A2($Signal.send,
                            actions.address,
                            OrderSucceeded);
                         }();
                      });}
                 break;}
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      };
      return A2($Signal.map,
      maybeRequest,
      effects);
   }());
   var reserveSeatRequest = Elm.Native.Task.make(_elm).performSignal("reserveSeatRequest",
   function () {
      var maybeRequest = function (s) {
         return function () {
            switch (s.ctor)
            {case "Just": switch (s._0.ctor)
                 {case "ReserveSeatRequest":
                    return A2($Task.onError,
                      A2($Task.andThen,
                      A2($HttpRequests.reserveSeat,
                      s._0._0,
                      s._0._1),
                      function (result) {
                         return A2($Signal.send,
                         actions.address,
                         SeatSelected(s._0._1));
                      }),
                      function (result) {
                         return A2($Task.andThen,
                         A2($Signal.send,
                         actions.address,
                         ShowErrorFlashMessage("Sitz ist schon reserviert.")),
                         function (foo) {
                            return A2($Signal.send,
                            actions.address,
                            SeatReserved(s._0._1));
                         });
                      });}
                 break;}
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      };
      return A2($Signal.map,
      maybeRequest,
      effects);
   }());
   var freeSeatRequest = Elm.Native.Task.make(_elm).performSignal("freeSeatRequest",
   function () {
      var maybeRequest = function (s) {
         return function () {
            switch (s.ctor)
            {case "Just": switch (s._0.ctor)
                 {case "FreeSeatRequest":
                    return A2($Task.onError,
                      A2($Task.andThen,
                      A2($HttpRequests.freeSeat,
                      s._0._0,
                      s._0._1),
                      function (result) {
                         return A2($Signal.send,
                         actions.address,
                         SeatUnselected(s._0._1));
                      }),
                      function (result) {
                         return A2($Signal.send,
                         actions.address,
                         ShowErrorFlashMessage("Sitz konnte nicht entfernt werden."));
                      });}
                 break;}
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      };
      return A2($Signal.map,
      maybeRequest,
      effects);
   }());
   var payRequests = Elm.Native.Task.make(_elm).performSignal("payRequests",
   function () {
      var maybeRequest = function (s) {
         return function () {
            switch (s.ctor)
            {case "Just": switch (s._0.ctor)
                 {case "PaidRequest":
                    return A2($Task.andThen,
                      A3($HttpRequests.paid,
                      s._0._1.id,
                      s._0._0.name,
                      s._0._0.password),
                      function (_v192) {
                         return function () {
                            return A2($Signal.send,
                            actions.address,
                            PayOrder(s._0._1));
                         }();
                      });
                    case "UnpaidRequest":
                    return A2($Task.andThen,
                      A3($HttpRequests.unpaid,
                      s._0._1.id,
                      s._0._0.name,
                      s._0._0.password),
                      function (_v194) {
                         return function () {
                            return A2($Signal.send,
                            actions.address,
                            UnpayOrder(s._0._1));
                         }();
                      });}
                 break;}
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      };
      return A2($Signal.map,
      maybeRequest,
      effects);
   }());
   var loginRequest = Elm.Native.Task.make(_elm).performSignal("loginRequest",
   function () {
      var maybeRequest = function (s) {
         return function () {
            switch (s.ctor)
            {case "Just": switch (s._0.ctor)
                 {case "LoginRequest":
                    return A2($Task.andThen,
                      A2($HttpRequests.login,
                      s._0._0.name,
                      s._0._0.password),
                      function (result) {
                         return A2($Signal.send,
                         actions.address,
                         _U.eq(result,
                         "admin") ? LoginAsAdminSucceeded(s._0._0) : LoginAsUserSucceeded(s._0._0));
                      });}
                 break;}
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      };
      return A2($Signal.map,
      maybeRequest,
      effects);
   }());
   var model = A2($Signal.map,
   $Basics.fst,
   updatesWithEffect);
   var main = A2($Signal.map,
   view(actions.address),
   model);
   _elm.Flashdance.values = {_op: _op
                            ,NoOp: NoOp
                            ,OrderSucceeded: OrderSucceeded
                            ,ResetApp: ResetApp
                            ,ClickSeat: ClickSeat
                            ,SeatsReceived: SeatsReceived
                            ,GigsReceived: GigsReceived
                            ,OrdersReceived: OrdersReceived
                            ,PayOrder: PayOrder
                            ,UnpayOrder: UnpayOrder
                            ,ClickGig: ClickGig
                            ,ClickOrder: ClickOrder
                            ,ClickPaid: ClickPaid
                            ,ClickUnpaid: ClickUnpaid
                            ,ClickCancelOrder: ClickCancelOrder
                            ,CancelOrderSucceeded: CancelOrderSucceeded
                            ,ReservationsReceived: ReservationsReceived
                            ,OrderTicket: OrderTicket
                            ,UpdateEmail: UpdateEmail
                            ,UpdateName: UpdateName
                            ,UpdateReducedCount: UpdateReducedCount
                            ,UpdateStreet: UpdateStreet
                            ,UpdatePostalCode: UpdatePostalCode
                            ,UpdateCity: UpdateCity
                            ,HttpOrderFailed: HttpOrderFailed
                            ,CloseFlashMessage: CloseFlashMessage
                            ,StartOrder: StartOrder
                            ,OrderStarted: OrderStarted
                            ,SeatReserved: SeatReserved
                            ,SeatSelected: SeatSelected
                            ,SeatUnselected: SeatUnselected
                            ,TypeSearch: TypeSearch
                            ,ShowErrorFlashMessage: ShowErrorFlashMessage
                            ,UpdateDeliveryOption: UpdateDeliveryOption
                            ,FinishOrderTicket: FinishOrderTicket
                            ,ClickAdmin: ClickAdmin
                            ,UpdateLoginPassword: UpdateLoginPassword
                            ,UpdateLoginName: UpdateLoginName
                            ,Login: Login
                            ,LoginAsUserSucceeded: LoginAsUserSucceeded
                            ,LoginAsAdminSucceeded: LoginAsAdminSucceeded
                            ,FetchSeats: FetchSeats
                            ,SubmitOrder: SubmitOrder
                            ,StartOrderRequest: StartOrderRequest
                            ,ReserveSeatRequest: ReserveSeatRequest
                            ,FreeSeatRequest: FreeSeatRequest
                            ,FinishOrderRequest: FinishOrderRequest
                            ,FinishOrderWithAddressRequest: FinishOrderWithAddressRequest
                            ,LoginRequest: LoginRequest
                            ,PaidRequest: PaidRequest
                            ,UnpaidRequest: UnpaidRequest
                            ,ListOrderRequest: ListOrderRequest
                            ,CancelOrderRequest: CancelOrderRequest
                            ,GigIndex: GigIndex
                            ,GigView: GigView
                            ,Success: Success
                            ,Info: Info
                            ,Error: Error
                            ,Hidden: Hidden
                            ,PickUpBoxOffice: PickUpBoxOffice
                            ,PickUpBeforehand: PickUpBeforehand
                            ,Delivery: Delivery
                            ,OrderInfo: OrderInfo
                            ,Ordering: Ordering
                            ,Browsing: Browsing
                            ,Ordered: Ordered
                            ,Address$: Address$
                            ,CurrentFormInput: CurrentFormInput
                            ,Gig: Gig
                            ,Order: Order
                            ,Credentials: Credentials
                            ,Anonymous: Anonymous
                            ,User: User
                            ,Admin: Admin
                            ,Model: Model
                            ,emptyAddress: emptyAddress
                            ,initialModel: initialModel
                            ,updateAddress: updateAddress
                            ,forcefullyExtractDeliveryOption: forcefullyExtractDeliveryOption
                            ,forcefullyExtractOrderInfo: forcefullyExtractOrderInfo
                            ,forcefullyExtractAddress: forcefullyExtractAddress
                            ,updateCity: updateCity
                            ,updateStreet: updateStreet
                            ,updatePostalCode: updatePostalCode
                            ,update: update
                            ,startOrderValid: startOrderValid
                            ,ordersWithPaid: ordersWithPaid
                            ,ordersWithUnpaid: ordersWithUnpaid
                            ,filterOrders: filterOrders
                            ,ordersWithDelivery: ordersWithDelivery
                            ,ordersWithEmail: ordersWithEmail
                            ,ordersWithoutEmail: ordersWithoutEmail
                            ,seatsRequestGigIdSignal: seatsRequestGigIdSignal
                            ,httpErrorToMessage: httpErrorToMessage
                            ,typeFromDeliveryOption: typeFromDeliveryOption
                            ,emptyHref: emptyHref
                            ,drawGigEntry: drawGigEntry
                            ,viewFlashMessage: viewFlashMessage
                            ,formatDateTime: formatDateTime
                            ,formatDate: formatDate
                            ,formatShortDate: formatShortDate
                            ,viewOrderFilterTextField: viewOrderFilterTextField
                            ,viewOrderList: viewOrderList
                            ,viewOrderDetail: viewOrderDetail
                            ,isAdmin: isAdmin
                            ,view: view
                            ,viewOrderInfos: viewOrderInfos
                            ,viewOrderFinishForm: viewOrderFinishForm
                            ,isDelivery: isDelivery
                            ,viewOrderPanel: viewOrderPanel
                            ,unwrapMaybe: unwrapMaybe
                            ,reducedCount: reducedCount
                            ,reducedCountValid: reducedCountValid
                            ,fullCount: fullCount
                            ,reducedPrice: reducedPrice
                            ,fullPrice: fullPrice
                            ,addPrice: addPrice
                            ,combine: combine
                            ,deliveryCosts: deliveryCosts
                            ,totalPrice: totalPrice
                            ,mapWithDefault: mapWithDefault
                            ,viewOrderTable: viewOrderTable
                            ,OrderTable: OrderTable
                            ,orderTableFromOrder: orderTableFromOrder
                            ,viewOrderTable$: viewOrderTable$
                            ,formInput: formInput
                            ,textInput: textInput
                            ,passwordInput: passwordInput
                            ,emailInput: emailInput
                            ,numberInput: numberInput
                            ,radioInputs: radioInputs
                            ,viewRegisterForm: viewRegisterForm
                            ,main: main
                            ,input: input
                            ,updatesWithEffect: updatesWithEffect
                            ,isJust: isJust
                            ,effects: effects
                            ,model: model
                            ,actions: actions
                            ,seatColor: seatColor
                            ,drawSeat: drawSeat
                            ,drawRow: drawRow
                            ,drawStand: drawStand};
   return _elm.Flashdance.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values)
   return _elm.Graphics.Collage.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Collage",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var ngon = F2(function (n,r) {
      return function () {
         var m = $Basics.toFloat(n);
         var t = 2 * $Basics.pi / m;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: r * $Basics.cos(t * i)
                   ,_1: r * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,m - 1));
      }();
   });
   var oval = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         var n = 50;
         var t = 2 * $Basics.pi / n;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: hw * $Basics.cos(t * i)
                   ,_1: hh * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,n - 1));
      }();
   });
   var circle = function (r) {
      return A2(oval,2 * r,2 * r);
   };
   var rect = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         return _L.fromArray([{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: 0 - hh}
                             ,{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: hh}
                             ,{ctor: "_Tuple2",_0: hw,_1: hh}
                             ,{ctor: "_Tuple2"
                              ,_0: hw
                              ,_1: 0 - hh}]);
      }();
   });
   var square = function (n) {
      return A2(rect,n,n);
   };
   var polygon = function (points) {
      return points;
   };
   var segment = F2(function (p1,
   p2) {
      return _L.fromArray([p1,p2]);
   });
   var path = function (ps) {
      return ps;
   };
   var collage = $Native$Graphics$Collage.collage;
   var alpha = F2(function (a,f) {
      return _U.replace([["alpha"
                         ,a]],
      f);
   });
   var rotate = F2(function (t,f) {
      return _U.replace([["theta"
                         ,f.theta + t]],
      f);
   });
   var scale = F2(function (s,f) {
      return _U.replace([["scale"
                         ,f.scale * s]],
      f);
   });
   var moveY = F2(function (y,f) {
      return _U.replace([["y"
                         ,f.y + y]],
      f);
   });
   var moveX = F2(function (x,f) {
      return _U.replace([["x"
                         ,f.x + x]],
      f);
   });
   var move = F2(function (_v0,f) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return _U.replace([["x"
                               ,f.x + _v0._0]
                              ,["y",f.y + _v0._1]],
              f);}
         _U.badCase($moduleName,
         "on line 226, column 3 to 37");
      }();
   });
   var form = function (f) {
      return {_: {}
             ,alpha: 1
             ,form: f
             ,scale: 1
             ,theta: 0
             ,x: 0
             ,y: 0};
   };
   var Fill = function (a) {
      return {ctor: "Fill",_0: a};
   };
   var Line = function (a) {
      return {ctor: "Line",_0: a};
   };
   var FGroup = F2(function (a,b) {
      return {ctor: "FGroup"
             ,_0: a
             ,_1: b};
   });
   var group = function (fs) {
      return form(A2(FGroup,
      $Transform2D.identity,
      fs));
   };
   var groupTransform = F2(function (matrix,
   fs) {
      return form(A2(FGroup,
      matrix,
      fs));
   });
   var FElement = function (a) {
      return {ctor: "FElement"
             ,_0: a};
   };
   var toForm = function (e) {
      return form(FElement(e));
   };
   var FImage = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "FImage"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var sprite = F4(function (w,
   h,
   pos,
   src) {
      return form(A4(FImage,
      w,
      h,
      pos,
      src));
   });
   var FText = function (a) {
      return {ctor: "FText",_0: a};
   };
   var text = function (t) {
      return form(FText(t));
   };
   var FOutlinedText = F2(function (a,
   b) {
      return {ctor: "FOutlinedText"
             ,_0: a
             ,_1: b};
   });
   var outlinedText = F2(function (ls,
   t) {
      return form(A2(FOutlinedText,
      ls,
      t));
   });
   var FShape = F2(function (a,b) {
      return {ctor: "FShape"
             ,_0: a
             ,_1: b};
   });
   var fill = F2(function (style,
   shape) {
      return form(A2(FShape,
      Fill(style),
      shape));
   });
   var outlined = F2(function (style,
   shape) {
      return form(A2(FShape,
      Line(style),
      shape));
   });
   var FPath = F2(function (a,b) {
      return {ctor: "FPath"
             ,_0: a
             ,_1: b};
   });
   var traced = F2(function (style,
   path) {
      return form(A2(FPath,
      style,
      path));
   });
   var LineStyle = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,cap: c
             ,color: a
             ,dashOffset: f
             ,dashing: e
             ,join: d
             ,width: b};
   });
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {
      return {ctor: "Sharp",_0: a};
   };
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {_: {}
                     ,cap: Flat
                     ,color: $Color.black
                     ,dashOffset: 0
                     ,dashing: _L.fromArray([])
                     ,join: Sharp(10)
                     ,width: 1};
   var solid = function (clr) {
      return _U.replace([["color"
                         ,clr]],
      defaultLine);
   };
   var dashed = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([8,4])]],
      defaultLine);
   };
   var dotted = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([3,3])]],
      defaultLine);
   };
   var Grad = function (a) {
      return {ctor: "Grad",_0: a};
   };
   var gradient = F2(function (grad,
   shape) {
      return A2(fill,
      Grad(grad),
      shape);
   });
   var Texture = function (a) {
      return {ctor: "Texture"
             ,_0: a};
   };
   var textured = F2(function (src,
   shape) {
      return A2(fill,
      Texture(src),
      shape);
   });
   var Solid = function (a) {
      return {ctor: "Solid",_0: a};
   };
   var filled = F2(function (color,
   shape) {
      return A2(fill,
      Solid(color),
      shape);
   });
   var Form = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,alpha: e
             ,form: f
             ,scale: b
             ,theta: a
             ,x: c
             ,y: d};
   });
   _elm.Graphics.Collage.values = {_op: _op
                                  ,collage: collage
                                  ,toForm: toForm
                                  ,filled: filled
                                  ,textured: textured
                                  ,gradient: gradient
                                  ,outlined: outlined
                                  ,traced: traced
                                  ,text: text
                                  ,outlinedText: outlinedText
                                  ,move: move
                                  ,moveX: moveX
                                  ,moveY: moveY
                                  ,scale: scale
                                  ,rotate: rotate
                                  ,alpha: alpha
                                  ,group: group
                                  ,groupTransform: groupTransform
                                  ,rect: rect
                                  ,oval: oval
                                  ,square: square
                                  ,circle: circle
                                  ,ngon: ngon
                                  ,polygon: polygon
                                  ,segment: segment
                                  ,path: path
                                  ,solid: solid
                                  ,dashed: dashed
                                  ,dotted: dotted
                                  ,defaultLine: defaultLine
                                  ,Form: Form
                                  ,LineStyle: LineStyle
                                  ,Flat: Flat
                                  ,Round: Round
                                  ,Padded: Padded
                                  ,Smooth: Smooth
                                  ,Sharp: Sharp
                                  ,Clipped: Clipped};
   return _elm.Graphics.Collage.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values)
   return _elm.Graphics.Element.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Element",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Graphics$Element = Elm.Native.Graphics.Element.make(_elm),
   $Text = Elm.Text.make(_elm);
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var Position = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,horizontal: a
             ,vertical: b
             ,x: c
             ,y: d};
   });
   var Relative = function (a) {
      return {ctor: "Relative"
             ,_0: a};
   };
   var relative = Relative;
   var Absolute = function (a) {
      return {ctor: "Absolute"
             ,_0: a};
   };
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var Z = {ctor: "Z"};
   var middle = {_: {}
                ,horizontal: Z
                ,vertical: Z
                ,x: Relative(0.5)
                ,y: Relative(0.5)};
   var midLeft = _U.replace([["horizontal"
                             ,N]
                            ,["x",Absolute(0)]],
   middle);
   var middleAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midBottomAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var P = {ctor: "P"};
   var topLeft = {_: {}
                 ,horizontal: N
                 ,vertical: P
                 ,x: Absolute(0)
                 ,y: Absolute(0)};
   var bottomLeft = _U.replace([["vertical"
                                ,N]],
   topLeft);
   var topRight = _U.replace([["horizontal"
                              ,P]],
   topLeft);
   var bottomRight = _U.replace([["horizontal"
                                 ,P]],
   bottomLeft);
   var midRight = _U.replace([["horizontal"
                              ,P]],
   midLeft);
   var midTop = _U.replace([["vertical"
                            ,P]
                           ,["y",Absolute(0)]],
   middle);
   var midBottom = _U.replace([["vertical"
                               ,N]],
   midTop);
   var topLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var topRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var bottomRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var midRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midTopAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var justified = $Native$Graphics$Element.block("justify");
   var centered = $Native$Graphics$Element.block("center");
   var rightAligned = $Native$Graphics$Element.block("right");
   var leftAligned = $Native$Graphics$Element.block("left");
   var show = function (value) {
      return leftAligned($Text.monospace($Text.fromString($Basics.toString(value))));
   };
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {
      return {ctor: "Cropped"
             ,_0: a};
   };
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {
      return {ctor: "Flow"
             ,_0: a
             ,_1: b};
   });
   var Container = F2(function (a,
   b) {
      return {ctor: "Container"
             ,_0: a
             ,_1: b};
   });
   var Image = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "Image"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var newElement = $Native$Graphics$Element.newElement;
   var image = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Plain,w,h,src));
   });
   var fittedImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Fitted,w,h,src));
   });
   var croppedImage = F4(function (pos,
   w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Cropped(pos),w,h,src));
   });
   var tiledImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Tiled,w,h,src));
   });
   var container = F4(function (w,
   h,
   pos,
   e) {
      return A3(newElement,
      w,
      h,
      A2(Container,pos,e));
   });
   var spacer = F2(function (w,h) {
      return A3(newElement,
      w,
      h,
      Spacer);
   });
   var link = F2(function (href,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["href"
                                    ,href]],
                p)};
      }();
   });
   var tag = F2(function (name,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["tag"
                                    ,name]],
                p)};
      }();
   });
   var color = F2(function (c,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["color"
                                    ,$Maybe.Just(c)]],
                p)};
      }();
   });
   var opacity = F2(function (o,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["opacity"
                                    ,o]],
                p)};
      }();
   });
   var height = F2(function (nh,
   e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v0 = e.element;
            switch (_v0.ctor)
            {case "Image":
               return _U.replace([["width"
                                  ,$Basics.round($Basics.toFloat(_v0._1) / $Basics.toFloat(_v0._2) * $Basics.toFloat(nh))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["height"
                                    ,nh]],
                p)};
      }();
   });
   var width = F2(function (nw,e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v5 = e.element;
            switch (_v5.ctor)
            {case "Image":
               return _U.replace([["height"
                                  ,$Basics.round($Basics.toFloat(_v5._2) / $Basics.toFloat(_v5._1) * $Basics.toFloat(nw))]],
                 p);
               case "RawHtml":
               return _U.replace([["height"
                                  ,$Basics.snd(A2($Native$Graphics$Element.htmlHeight,
                                  nw,
                                  e.element))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["width"
                                    ,nw]],
                props)};
      }();
   });
   var size = F3(function (w,h,e) {
      return A2(height,
      h,
      A2(width,w,e));
   });
   var sizeOf = function (e) {
      return {ctor: "_Tuple2"
             ,_0: e.props.width
             ,_1: e.props.height};
   };
   var heightOf = function (e) {
      return e.props.height;
   };
   var widthOf = function (e) {
      return e.props.width;
   };
   var above = F2(function (hi,
   lo) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var below = F2(function (lo,
   hi) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var beside = F2(function (lft,
   rht) {
      return A3(newElement,
      widthOf(lft) + widthOf(rht),
      A2($Basics.max,
      heightOf(lft),
      heightOf(rht)),
      A2(Flow,
      right,
      _L.fromArray([lft,rht])));
   });
   var layers = function (es) {
      return function () {
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return A3(newElement,
         A2($Maybe.withDefault,
         0,
         $List.maximum(ws)),
         A2($Maybe.withDefault,
         0,
         $List.maximum(hs)),
         A2(Flow,DOut,es));
      }();
   };
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,
   es) {
      return function () {
         var newFlow = F2(function (w,
         h) {
            return A3(newElement,
            w,
            h,
            A2(Flow,dir,es));
         });
         var maxOrZero = function (list) {
            return A2($Maybe.withDefault,
            0,
            $List.maximum(list));
         };
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return _U.eq(es,
         _L.fromArray([])) ? empty : function () {
            switch (dir.ctor)
            {case "DDown":
               return A2(newFlow,
                 maxOrZero(ws),
                 $List.sum(hs));
               case "DIn": return A2(newFlow,
                 maxOrZero(ws),
                 maxOrZero(hs));
               case "DLeft": return A2(newFlow,
                 $List.sum(ws),
                 maxOrZero(hs));
               case "DOut": return A2(newFlow,
                 maxOrZero(ws),
                 maxOrZero(hs));
               case "DRight":
               return A2(newFlow,
                 $List.sum(ws),
                 maxOrZero(hs));
               case "DUp": return A2(newFlow,
                 maxOrZero(ws),
                 $List.sum(hs));}
            _U.badCase($moduleName,
            "between lines 362 and 368");
         }();
      }();
   });
   var Properties = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,click: i
             ,color: e
             ,height: c
             ,hover: h
             ,href: f
             ,id: a
             ,opacity: d
             ,tag: g
             ,width: b};
   });
   var Element = F2(function (a,
   b) {
      return {_: {}
             ,element: b
             ,props: a};
   });
   _elm.Graphics.Element.values = {_op: _op
                                  ,image: image
                                  ,fittedImage: fittedImage
                                  ,croppedImage: croppedImage
                                  ,tiledImage: tiledImage
                                  ,leftAligned: leftAligned
                                  ,rightAligned: rightAligned
                                  ,centered: centered
                                  ,justified: justified
                                  ,show: show
                                  ,width: width
                                  ,height: height
                                  ,size: size
                                  ,color: color
                                  ,opacity: opacity
                                  ,link: link
                                  ,tag: tag
                                  ,widthOf: widthOf
                                  ,heightOf: heightOf
                                  ,sizeOf: sizeOf
                                  ,flow: flow
                                  ,up: up
                                  ,down: down
                                  ,left: left
                                  ,right: right
                                  ,inward: inward
                                  ,outward: outward
                                  ,layers: layers
                                  ,above: above
                                  ,below: below
                                  ,beside: beside
                                  ,empty: empty
                                  ,spacer: spacer
                                  ,container: container
                                  ,middle: middle
                                  ,midTop: midTop
                                  ,midBottom: midBottom
                                  ,midLeft: midLeft
                                  ,midRight: midRight
                                  ,topLeft: topLeft
                                  ,topRight: topRight
                                  ,bottomLeft: bottomLeft
                                  ,bottomRight: bottomRight
                                  ,absolute: absolute
                                  ,relative: relative
                                  ,middleAt: middleAt
                                  ,midTopAt: midTopAt
                                  ,midBottomAt: midBottomAt
                                  ,midLeftAt: midLeftAt
                                  ,midRightAt: midRightAt
                                  ,topLeftAt: topLeftAt
                                  ,topRightAt: topRightAt
                                  ,bottomLeftAt: bottomLeftAt
                                  ,bottomRightAt: bottomRightAt
                                  ,Element: Element
                                  ,Position: Position};
   return _elm.Graphics.Element.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   if (_elm.Html.values)
   return _elm.Html.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html",
   $Basics = Elm.Basics.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var fromElement = $VirtualDom.fromElement;
   var toElement = $VirtualDom.toElement;
   var text = $VirtualDom.text;
   var node = $VirtualDom.node;
   var body = node("body");
   var section = node("section");
   var nav = node("nav");
   var article = node("article");
   var aside = node("aside");
   var h1 = node("h1");
   var h2 = node("h2");
   var h3 = node("h3");
   var h4 = node("h4");
   var h5 = node("h5");
   var h6 = node("h6");
   var header = node("header");
   var footer = node("footer");
   var address = node("address");
   var main$ = node("main");
   var p = node("p");
   var hr = node("hr");
   var pre = node("pre");
   var blockquote = node("blockquote");
   var ol = node("ol");
   var ul = node("ul");
   var li = node("li");
   var dl = node("dl");
   var dt = node("dt");
   var dd = node("dd");
   var figure = node("figure");
   var figcaption = node("figcaption");
   var div = node("div");
   var a = node("a");
   var em = node("em");
   var strong = node("strong");
   var small = node("small");
   var s = node("s");
   var cite = node("cite");
   var q = node("q");
   var dfn = node("dfn");
   var abbr = node("abbr");
   var time = node("time");
   var code = node("code");
   var $var = node("var");
   var samp = node("samp");
   var kbd = node("kbd");
   var sub = node("sub");
   var sup = node("sup");
   var i = node("i");
   var b = node("b");
   var u = node("u");
   var mark = node("mark");
   var ruby = node("ruby");
   var rt = node("rt");
   var rp = node("rp");
   var bdi = node("bdi");
   var bdo = node("bdo");
   var span = node("span");
   var br = node("br");
   var wbr = node("wbr");
   var ins = node("ins");
   var del = node("del");
   var img = node("img");
   var iframe = node("iframe");
   var embed = node("embed");
   var object = node("object");
   var param = node("param");
   var video = node("video");
   var audio = node("audio");
   var source = node("source");
   var track = node("track");
   var canvas = node("canvas");
   var svg = node("svg");
   var math = node("math");
   var table = node("table");
   var caption = node("caption");
   var colgroup = node("colgroup");
   var col = node("col");
   var tbody = node("tbody");
   var thead = node("thead");
   var tfoot = node("tfoot");
   var tr = node("tr");
   var td = node("td");
   var th = node("th");
   var form = node("form");
   var fieldset = node("fieldset");
   var legend = node("legend");
   var label = node("label");
   var input = node("input");
   var button = node("button");
   var select = node("select");
   var datalist = node("datalist");
   var optgroup = node("optgroup");
   var option = node("option");
   var textarea = node("textarea");
   var keygen = node("keygen");
   var output = node("output");
   var progress = node("progress");
   var meter = node("meter");
   var details = node("details");
   var summary = node("summary");
   var menuitem = node("menuitem");
   var menu = node("menu");
   _elm.Html.values = {_op: _op
                      ,node: node
                      ,text: text
                      ,toElement: toElement
                      ,fromElement: fromElement
                      ,body: body
                      ,section: section
                      ,nav: nav
                      ,article: article
                      ,aside: aside
                      ,h1: h1
                      ,h2: h2
                      ,h3: h3
                      ,h4: h4
                      ,h5: h5
                      ,h6: h6
                      ,header: header
                      ,footer: footer
                      ,address: address
                      ,main$: main$
                      ,p: p
                      ,hr: hr
                      ,pre: pre
                      ,blockquote: blockquote
                      ,ol: ol
                      ,ul: ul
                      ,li: li
                      ,dl: dl
                      ,dt: dt
                      ,dd: dd
                      ,figure: figure
                      ,figcaption: figcaption
                      ,div: div
                      ,a: a
                      ,em: em
                      ,strong: strong
                      ,small: small
                      ,s: s
                      ,cite: cite
                      ,q: q
                      ,dfn: dfn
                      ,abbr: abbr
                      ,time: time
                      ,code: code
                      ,$var: $var
                      ,samp: samp
                      ,kbd: kbd
                      ,sub: sub
                      ,sup: sup
                      ,i: i
                      ,b: b
                      ,u: u
                      ,mark: mark
                      ,ruby: ruby
                      ,rt: rt
                      ,rp: rp
                      ,bdi: bdi
                      ,bdo: bdo
                      ,span: span
                      ,br: br
                      ,wbr: wbr
                      ,ins: ins
                      ,del: del
                      ,img: img
                      ,iframe: iframe
                      ,embed: embed
                      ,object: object
                      ,param: param
                      ,video: video
                      ,audio: audio
                      ,source: source
                      ,track: track
                      ,canvas: canvas
                      ,svg: svg
                      ,math: math
                      ,table: table
                      ,caption: caption
                      ,colgroup: colgroup
                      ,col: col
                      ,tbody: tbody
                      ,thead: thead
                      ,tfoot: tfoot
                      ,tr: tr
                      ,td: td
                      ,th: th
                      ,form: form
                      ,fieldset: fieldset
                      ,legend: legend
                      ,label: label
                      ,input: input
                      ,button: button
                      ,select: select
                      ,datalist: datalist
                      ,optgroup: optgroup
                      ,option: option
                      ,textarea: textarea
                      ,keygen: keygen
                      ,output: output
                      ,progress: progress
                      ,meter: meter
                      ,details: details
                      ,summary: summary
                      ,menuitem: menuitem
                      ,menu: menu};
   return _elm.Html.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Attributes = Elm.Html.Attributes || {};
Elm.Html.Attributes.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Attributes = _elm.Html.Attributes || {};
   if (_elm.Html.Attributes.values)
   return _elm.Html.Attributes.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Attributes",
   $Basics = Elm.Basics.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var attribute = $VirtualDom.attribute;
   var property = $VirtualDom.property;
   var stringProperty = F2(function (name,
   string) {
      return A2(property,
      name,
      $Json$Encode.string(string));
   });
   var $class = function (name) {
      return A2(stringProperty,
      "className",
      name);
   };
   var id = function (name) {
      return A2(stringProperty,
      "id",
      name);
   };
   var title = function (name) {
      return A2(stringProperty,
      "title",
      name);
   };
   var accesskey = function ($char) {
      return A2(stringProperty,
      "accesskey",
      $String.fromList(_L.fromArray([$char])));
   };
   var contextmenu = function (value) {
      return A2(stringProperty,
      "contextmenu",
      value);
   };
   var dir = function (value) {
      return A2(stringProperty,
      "dir",
      value);
   };
   var draggable = function (value) {
      return A2(stringProperty,
      "draggable",
      value);
   };
   var dropzone = function (value) {
      return A2(stringProperty,
      "dropzone",
      value);
   };
   var itemprop = function (value) {
      return A2(stringProperty,
      "itemprop",
      value);
   };
   var lang = function (value) {
      return A2(stringProperty,
      "lang",
      value);
   };
   var tabindex = function (n) {
      return A2(stringProperty,
      "tabIndex",
      $Basics.toString(n));
   };
   var charset = function (value) {
      return A2(stringProperty,
      "charset",
      value);
   };
   var content = function (value) {
      return A2(stringProperty,
      "content",
      value);
   };
   var httpEquiv = function (value) {
      return A2(stringProperty,
      "httpEquiv",
      value);
   };
   var language = function (value) {
      return A2(stringProperty,
      "language",
      value);
   };
   var src = function (value) {
      return A2(stringProperty,
      "src",
      value);
   };
   var height = function (value) {
      return A2(stringProperty,
      "height",
      $Basics.toString(value));
   };
   var width = function (value) {
      return A2(stringProperty,
      "width",
      $Basics.toString(value));
   };
   var alt = function (value) {
      return A2(stringProperty,
      "alt",
      value);
   };
   var preload = function (value) {
      return A2(stringProperty,
      "preload",
      value);
   };
   var poster = function (value) {
      return A2(stringProperty,
      "poster",
      value);
   };
   var kind = function (value) {
      return A2(stringProperty,
      "kind",
      value);
   };
   var srclang = function (value) {
      return A2(stringProperty,
      "srclang",
      value);
   };
   var sandbox = function (value) {
      return A2(stringProperty,
      "sandbox",
      value);
   };
   var srcdoc = function (value) {
      return A2(stringProperty,
      "srcdoc",
      value);
   };
   var type$ = function (value) {
      return A2(stringProperty,
      "type",
      value);
   };
   var value = function (value) {
      return A2(stringProperty,
      "value",
      value);
   };
   var placeholder = function (value) {
      return A2(stringProperty,
      "placeholder",
      value);
   };
   var accept = function (value) {
      return A2(stringProperty,
      "accept",
      value);
   };
   var acceptCharset = function (value) {
      return A2(stringProperty,
      "acceptCharset",
      value);
   };
   var action = function (value) {
      return A2(stringProperty,
      "action",
      value);
   };
   var autocomplete = function (bool) {
      return A2(stringProperty,
      "autocomplete",
      bool ? "on" : "off");
   };
   var autosave = function (value) {
      return A2(stringProperty,
      "autosave",
      value);
   };
   var enctype = function (value) {
      return A2(stringProperty,
      "enctype",
      value);
   };
   var formaction = function (value) {
      return A2(stringProperty,
      "formaction",
      value);
   };
   var list = function (value) {
      return A2(stringProperty,
      "list",
      value);
   };
   var minlength = function (n) {
      return A2(stringProperty,
      "minLength",
      $Basics.toString(n));
   };
   var maxlength = function (n) {
      return A2(stringProperty,
      "maxLength",
      $Basics.toString(n));
   };
   var method = function (value) {
      return A2(stringProperty,
      "method",
      value);
   };
   var name = function (value) {
      return A2(stringProperty,
      "name",
      value);
   };
   var pattern = function (value) {
      return A2(stringProperty,
      "pattern",
      value);
   };
   var size = function (n) {
      return A2(stringProperty,
      "size",
      $Basics.toString(n));
   };
   var $for = function (value) {
      return A2(stringProperty,
      "htmlFor",
      value);
   };
   var form = function (value) {
      return A2(stringProperty,
      "form",
      value);
   };
   var max = function (value) {
      return A2(stringProperty,
      "max",
      value);
   };
   var min = function (value) {
      return A2(stringProperty,
      "min",
      value);
   };
   var step = function (n) {
      return A2(stringProperty,
      "step",
      n);
   };
   var cols = function (n) {
      return A2(stringProperty,
      "cols",
      $Basics.toString(n));
   };
   var rows = function (n) {
      return A2(stringProperty,
      "rows",
      $Basics.toString(n));
   };
   var wrap = function (value) {
      return A2(stringProperty,
      "wrap",
      value);
   };
   var usemap = function (value) {
      return A2(stringProperty,
      "useMap",
      value);
   };
   var shape = function (value) {
      return A2(stringProperty,
      "shape",
      value);
   };
   var coords = function (value) {
      return A2(stringProperty,
      "coords",
      value);
   };
   var challenge = function (value) {
      return A2(stringProperty,
      "challenge",
      value);
   };
   var keytype = function (value) {
      return A2(stringProperty,
      "keytype",
      value);
   };
   var align = function (value) {
      return A2(stringProperty,
      "align",
      value);
   };
   var cite = function (value) {
      return A2(stringProperty,
      "cite",
      value);
   };
   var href = function (value) {
      return A2(stringProperty,
      "href",
      value);
   };
   var target = function (value) {
      return A2(stringProperty,
      "target",
      value);
   };
   var downloadAs = function (value) {
      return A2(stringProperty,
      "download",
      value);
   };
   var hreflang = function (value) {
      return A2(stringProperty,
      "hreflang",
      value);
   };
   var media = function (value) {
      return A2(stringProperty,
      "media",
      value);
   };
   var ping = function (value) {
      return A2(stringProperty,
      "ping",
      value);
   };
   var rel = function (value) {
      return A2(stringProperty,
      "rel",
      value);
   };
   var datetime = function (value) {
      return A2(stringProperty,
      "datetime",
      value);
   };
   var pubdate = function (value) {
      return A2(stringProperty,
      "pubdate",
      value);
   };
   var start = function (n) {
      return A2(stringProperty,
      "start",
      $Basics.toString(n));
   };
   var colspan = function (n) {
      return A2(stringProperty,
      "colSpan",
      $Basics.toString(n));
   };
   var headers = function (value) {
      return A2(stringProperty,
      "headers",
      value);
   };
   var rowspan = function (n) {
      return A2(stringProperty,
      "rowSpan",
      $Basics.toString(n));
   };
   var scope = function (value) {
      return A2(stringProperty,
      "scope",
      value);
   };
   var manifest = function (value) {
      return A2(stringProperty,
      "manifest",
      value);
   };
   var boolProperty = F2(function (name,
   bool) {
      return A2(property,
      name,
      $Json$Encode.bool(bool));
   });
   var hidden = function (bool) {
      return A2(boolProperty,
      "hidden",
      bool);
   };
   var contenteditable = function (bool) {
      return A2(boolProperty,
      "contentEditable",
      bool);
   };
   var spellcheck = function (bool) {
      return A2(boolProperty,
      "spellcheck",
      bool);
   };
   var async = function (bool) {
      return A2(boolProperty,
      "async",
      bool);
   };
   var defer = function (bool) {
      return A2(boolProperty,
      "defer",
      bool);
   };
   var scoped = function (bool) {
      return A2(boolProperty,
      "scoped",
      bool);
   };
   var autoplay = function (bool) {
      return A2(boolProperty,
      "autoplay",
      bool);
   };
   var controls = function (bool) {
      return A2(boolProperty,
      "controls",
      bool);
   };
   var loop = function (bool) {
      return A2(boolProperty,
      "loop",
      bool);
   };
   var $default = function (bool) {
      return A2(boolProperty,
      "default",
      bool);
   };
   var seamless = function (bool) {
      return A2(boolProperty,
      "seamless",
      bool);
   };
   var checked = function (bool) {
      return A2(boolProperty,
      "checked",
      bool);
   };
   var selected = function (bool) {
      return A2(boolProperty,
      "selected",
      bool);
   };
   var autofocus = function (bool) {
      return A2(boolProperty,
      "autofocus",
      bool);
   };
   var disabled = function (bool) {
      return A2(boolProperty,
      "disabled",
      bool);
   };
   var multiple = function (bool) {
      return A2(boolProperty,
      "multiple",
      bool);
   };
   var novalidate = function (bool) {
      return A2(boolProperty,
      "noValidate",
      bool);
   };
   var readonly = function (bool) {
      return A2(boolProperty,
      "readOnly",
      bool);
   };
   var required = function (bool) {
      return A2(boolProperty,
      "required",
      bool);
   };
   var ismap = function (value) {
      return A2(boolProperty,
      "isMap",
      value);
   };
   var download = function (bool) {
      return A2(boolProperty,
      "download",
      bool);
   };
   var reversed = function (bool) {
      return A2(boolProperty,
      "reversed",
      bool);
   };
   var classList = function (list) {
      return $class($String.join(" ")($List.map($Basics.fst)($List.filter($Basics.snd)(list))));
   };
   var style = function (props) {
      return property("style")($Json$Encode.object($List.map(function (_v0) {
         return function () {
            switch (_v0.ctor)
            {case "_Tuple2":
               return {ctor: "_Tuple2"
                      ,_0: _v0._0
                      ,_1: $Json$Encode.string(_v0._1)};}
            _U.badCase($moduleName,
            "on line 133, column 35 to 57");
         }();
      })(props)));
   };
   var key = function (k) {
      return A2(stringProperty,
      "key",
      k);
   };
   _elm.Html.Attributes.values = {_op: _op
                                 ,key: key
                                 ,style: style
                                 ,classList: classList
                                 ,property: property
                                 ,stringProperty: stringProperty
                                 ,boolProperty: boolProperty
                                 ,attribute: attribute
                                 ,$class: $class
                                 ,hidden: hidden
                                 ,id: id
                                 ,title: title
                                 ,accesskey: accesskey
                                 ,contenteditable: contenteditable
                                 ,contextmenu: contextmenu
                                 ,dir: dir
                                 ,draggable: draggable
                                 ,dropzone: dropzone
                                 ,itemprop: itemprop
                                 ,lang: lang
                                 ,spellcheck: spellcheck
                                 ,tabindex: tabindex
                                 ,async: async
                                 ,charset: charset
                                 ,content: content
                                 ,defer: defer
                                 ,httpEquiv: httpEquiv
                                 ,language: language
                                 ,scoped: scoped
                                 ,src: src
                                 ,height: height
                                 ,width: width
                                 ,alt: alt
                                 ,autoplay: autoplay
                                 ,controls: controls
                                 ,loop: loop
                                 ,preload: preload
                                 ,poster: poster
                                 ,$default: $default
                                 ,kind: kind
                                 ,srclang: srclang
                                 ,sandbox: sandbox
                                 ,seamless: seamless
                                 ,srcdoc: srcdoc
                                 ,type$: type$
                                 ,value: value
                                 ,checked: checked
                                 ,placeholder: placeholder
                                 ,selected: selected
                                 ,accept: accept
                                 ,acceptCharset: acceptCharset
                                 ,action: action
                                 ,autocomplete: autocomplete
                                 ,autofocus: autofocus
                                 ,autosave: autosave
                                 ,disabled: disabled
                                 ,enctype: enctype
                                 ,formaction: formaction
                                 ,list: list
                                 ,minlength: minlength
                                 ,maxlength: maxlength
                                 ,method: method
                                 ,multiple: multiple
                                 ,name: name
                                 ,novalidate: novalidate
                                 ,pattern: pattern
                                 ,readonly: readonly
                                 ,required: required
                                 ,size: size
                                 ,$for: $for
                                 ,form: form
                                 ,max: max
                                 ,min: min
                                 ,step: step
                                 ,cols: cols
                                 ,rows: rows
                                 ,wrap: wrap
                                 ,ismap: ismap
                                 ,usemap: usemap
                                 ,shape: shape
                                 ,coords: coords
                                 ,challenge: challenge
                                 ,keytype: keytype
                                 ,align: align
                                 ,cite: cite
                                 ,href: href
                                 ,target: target
                                 ,download: download
                                 ,downloadAs: downloadAs
                                 ,hreflang: hreflang
                                 ,media: media
                                 ,ping: ping
                                 ,rel: rel
                                 ,datetime: datetime
                                 ,pubdate: pubdate
                                 ,reversed: reversed
                                 ,start: start
                                 ,colspan: colspan
                                 ,headers: headers
                                 ,rowspan: rowspan
                                 ,scope: scope
                                 ,manifest: manifest};
   return _elm.Html.Attributes.values;
};
Elm.Html = Elm.Html || {};
Elm.Html.Events = Elm.Html.Events || {};
Elm.Html.Events.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Events = _elm.Html.Events || {};
   if (_elm.Html.Events.values)
   return _elm.Html.Events.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Html.Events",
   $Basics = Elm.Basics.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var keyCode = A2($Json$Decode._op[":="],
   "keyCode",
   $Json$Decode.$int);
   var targetChecked = A2($Json$Decode.at,
   _L.fromArray(["target"
                ,"checked"]),
   $Json$Decode.bool);
   var targetValue = A2($Json$Decode.at,
   _L.fromArray(["target"
                ,"value"]),
   $Json$Decode.string);
   var on = $VirtualDom.on;
   var messageOn = F3(function (name,
   addr,
   msg) {
      return A3(on,
      name,
      $Json$Decode.value,
      function (_v0) {
         return function () {
            return A2($Signal.message,
            addr,
            msg);
         }();
      });
   });
   var onClick = messageOn("click");
   var onDoubleClick = messageOn("dblclick");
   var onMouseMove = messageOn("mousemove");
   var onMouseDown = messageOn("mousedown");
   var onMouseUp = messageOn("mouseup");
   var onMouseEnter = messageOn("mouseenter");
   var onMouseLeave = messageOn("mouseleave");
   var onMouseOver = messageOn("mouseover");
   var onMouseOut = messageOn("mouseout");
   var onBlur = messageOn("blur");
   var onFocus = messageOn("focus");
   var onSubmit = messageOn("submit");
   var onKey = F3(function (name,
   addr,
   handler) {
      return A3(on,
      name,
      keyCode,
      function (code) {
         return A2($Signal.message,
         addr,
         handler(code));
      });
   });
   var onKeyUp = onKey("keyup");
   var onKeyDown = onKey("keydown");
   var onKeyPress = onKey("keypress");
   _elm.Html.Events.values = {_op: _op
                             ,onBlur: onBlur
                             ,onFocus: onFocus
                             ,onSubmit: onSubmit
                             ,onKeyUp: onKeyUp
                             ,onKeyDown: onKeyDown
                             ,onKeyPress: onKeyPress
                             ,onClick: onClick
                             ,onDoubleClick: onDoubleClick
                             ,onMouseMove: onMouseMove
                             ,onMouseDown: onMouseDown
                             ,onMouseUp: onMouseUp
                             ,onMouseEnter: onMouseEnter
                             ,onMouseLeave: onMouseLeave
                             ,onMouseOver: onMouseOver
                             ,onMouseOut: onMouseOut
                             ,on: on
                             ,targetValue: targetValue
                             ,targetChecked: targetChecked
                             ,keyCode: keyCode};
   return _elm.Html.Events.values;
};
Elm.Http = Elm.Http || {};
Elm.Http.make = function (_elm) {
   "use strict";
   _elm.Http = _elm.Http || {};
   if (_elm.Http.values)
   return _elm.Http.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Http",
   $Basics = Elm.Basics.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Http = Elm.Native.Http.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Time = Elm.Time.make(_elm);
   var send = $Native$Http.send;
   var BadResponse = F2(function (a,
   b) {
      return {ctor: "BadResponse"
             ,_0: a
             ,_1: b};
   });
   var UnexpectedPayload = function (a) {
      return {ctor: "UnexpectedPayload"
             ,_0: a};
   };
   var handleResponse = F2(function (handle,
   response) {
      return function () {
         var _v0 = _U.cmp(200,
         response.status) < 1 && _U.cmp(response.status,
         300) < 0;
         switch (_v0)
         {case false:
            return $Task.fail(A2(BadResponse,
              response.status,
              response.statusText));
            case true: return function () {
                 var _v1 = response.value;
                 switch (_v1.ctor)
                 {case "Text":
                    return handle(_v1._0);}
                 return $Task.fail(UnexpectedPayload("Response body is a blob, expecting a string."));
              }();}
         _U.badCase($moduleName,
         "between lines 419 and 426");
      }();
   });
   var NetworkError = {ctor: "NetworkError"};
   var Timeout = {ctor: "Timeout"};
   var promoteError = function (rawError) {
      return function () {
         switch (rawError.ctor)
         {case "RawNetworkError":
            return NetworkError;
            case "RawTimeout":
            return Timeout;}
         _U.badCase($moduleName,
         "between lines 431 and 433");
      }();
   };
   var fromJson = F2(function (decoder,
   response) {
      return function () {
         var decode = function (str) {
            return function () {
               var _v4 = A2($Json$Decode.decodeString,
               decoder,
               str);
               switch (_v4.ctor)
               {case "Err":
                  return $Task.fail(UnexpectedPayload(_v4._0));
                  case "Ok":
                  return $Task.succeed(_v4._0);}
               _U.badCase($moduleName,
               "between lines 409 and 412");
            }();
         };
         return A2($Task.andThen,
         A2($Task.mapError,
         promoteError,
         response),
         handleResponse(decode));
      }();
   });
   var RawNetworkError = {ctor: "RawNetworkError"};
   var RawTimeout = {ctor: "RawTimeout"};
   var Blob = function (a) {
      return {ctor: "Blob",_0: a};
   };
   var Text = function (a) {
      return {ctor: "Text",_0: a};
   };
   var Response = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,headers: c
             ,status: a
             ,statusText: b
             ,url: d
             ,value: e};
   });
   var defaultSettings = {_: {}
                         ,desiredResponseType: $Maybe.Nothing
                         ,onProgress: $Maybe.Nothing
                         ,onStart: $Maybe.Nothing
                         ,timeout: 0};
   var post = F3(function (decoder,
   url,
   body) {
      return function () {
         var request = {_: {}
                       ,body: body
                       ,headers: _L.fromArray([])
                       ,url: url
                       ,verb: "POST"};
         return A2(fromJson,
         decoder,
         A2(send,
         defaultSettings,
         request));
      }();
   });
   var Settings = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,desiredResponseType: d
             ,onProgress: c
             ,onStart: b
             ,timeout: a};
   });
   var multipart = $Native$Http.multipart;
   var FileData = F3(function (a,
   b,
   c) {
      return {ctor: "FileData"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var BlobData = F3(function (a,
   b,
   c) {
      return {ctor: "BlobData"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var blobData = BlobData;
   var StringData = F2(function (a,
   b) {
      return {ctor: "StringData"
             ,_0: a
             ,_1: b};
   });
   var stringData = StringData;
   var BodyBlob = function (a) {
      return {ctor: "BodyBlob"
             ,_0: a};
   };
   var BodyFormData = {ctor: "BodyFormData"};
   var ArrayBuffer = {ctor: "ArrayBuffer"};
   var BodyString = function (a) {
      return {ctor: "BodyString"
             ,_0: a};
   };
   var string = BodyString;
   var Empty = {ctor: "Empty"};
   var empty = Empty;
   var getString = function (url) {
      return function () {
         var request = {_: {}
                       ,body: empty
                       ,headers: _L.fromArray([])
                       ,url: url
                       ,verb: "GET"};
         return A2($Task.andThen,
         A2($Task.mapError,
         promoteError,
         A2(send,
         defaultSettings,
         request)),
         handleResponse($Task.succeed));
      }();
   };
   var get = F2(function (decoder,
   url) {
      return function () {
         var request = {_: {}
                       ,body: empty
                       ,headers: _L.fromArray([])
                       ,url: url
                       ,verb: "GET"};
         return A2(fromJson,
         decoder,
         A2(send,
         defaultSettings,
         request));
      }();
   });
   var Request = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,body: d
             ,headers: b
             ,url: c
             ,verb: a};
   });
   var uriDecode = $Native$Http.uriDecode;
   var uriEncode = $Native$Http.uriEncode;
   var queryEscape = function (string) {
      return A2($String.join,
      "+",
      A2($String.split,
      "%20",
      uriEncode(string)));
   };
   var queryPair = function (_v7) {
      return function () {
         switch (_v7.ctor)
         {case "_Tuple2":
            return A2($Basics._op["++"],
              queryEscape(_v7._0),
              A2($Basics._op["++"],
              "=",
              queryEscape(_v7._1)));}
         _U.badCase($moduleName,
         "on line 63, column 3 to 46");
      }();
   };
   var url = F2(function (domain,
   args) {
      return function () {
         switch (args.ctor)
         {case "[]": return domain;}
         return A2($Basics._op["++"],
         domain,
         A2($Basics._op["++"],
         "?",
         A2($String.join,
         "&",
         A2($List.map,queryPair,args))));
      }();
   });
   var TODO_implement_file_in_another_library = {ctor: "TODO_implement_file_in_another_library"};
   var TODO_implement_blob_in_another_library = {ctor: "TODO_implement_blob_in_another_library"};
   _elm.Http.values = {_op: _op
                      ,getString: getString
                      ,get: get
                      ,post: post
                      ,send: send
                      ,url: url
                      ,uriEncode: uriEncode
                      ,uriDecode: uriDecode
                      ,empty: empty
                      ,string: string
                      ,multipart: multipart
                      ,stringData: stringData
                      ,blobData: blobData
                      ,defaultSettings: defaultSettings
                      ,fromJson: fromJson
                      ,Request: Request
                      ,Settings: Settings
                      ,Response: Response
                      ,Text: Text
                      ,Blob: Blob
                      ,Timeout: Timeout
                      ,NetworkError: NetworkError
                      ,UnexpectedPayload: UnexpectedPayload
                      ,BadResponse: BadResponse
                      ,RawTimeout: RawTimeout
                      ,RawNetworkError: RawNetworkError};
   return _elm.Http.values;
};
Elm.HttpRequests = Elm.HttpRequests || {};
Elm.HttpRequests.make = function (_elm) {
   "use strict";
   _elm.HttpRequests = _elm.HttpRequests || {};
   if (_elm.HttpRequests.values)
   return _elm.HttpRequests.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "HttpRequests",
   $Basics = Elm.Basics.make(_elm),
   $Date = Elm.Date.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Http = Elm.Http.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Model = Elm.Model.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var unwrapResult = function (r) {
      return function () {
         switch (r.ctor)
         {case "Ok": return r._0;}
         _U.badCase($moduleName,
         "between lines 119 and 120");
      }();
   };
   var unwrap = function (x) {
      return function () {
         switch (x.ctor)
         {case "Just": return x._0;}
         _U.badCase($moduleName,
         "between lines 114 and 115");
      }();
   };
   var requestWithCredentials = F6(function (user,
   pw,
   verb,
   decoder,
   url,
   body) {
      return function () {
         var headers = !_U.eq(user,
         "") ? _L.fromArray([{ctor: "_Tuple2"
                             ,_0: "X-User"
                             ,_1: user}
                            ,{ctor: "_Tuple2"
                             ,_0: "X-Password"
                             ,_1: pw}]) : _L.fromArray([]);
         var request = {_: {}
                       ,body: body
                       ,headers: headers
                       ,url: url
                       ,verb: verb};
         return A2($Http.fromJson,
         decoder,
         A2($Http.send,
         $Http.defaultSettings,
         request));
      }();
   });
   var post = A3(requestWithCredentials,
   "",
   "",
   "POST");
   var put = A3(requestWithCredentials,
   "",
   "",
   "PUT");
   var $delete = A3(requestWithCredentials,
   "",
   "",
   "DELETE");
   var get = F2(function (decoder,
   url) {
      return A6(requestWithCredentials,
      "",
      "",
      "GET",
      decoder,
      url,
      $Http.empty);
   });
   var finishOrderWithAddressEncoder = F4(function (reducedCount,
   street,
   postalCode,
   city) {
      return A2($Json$Encode.encode,
      0,
      $Json$Encode.object(_L.fromArray([{ctor: "_Tuple2"
                                        ,_0: "reducedCount"
                                        ,_1: $Json$Encode.$int(reducedCount)}
                                       ,{ctor: "_Tuple2"
                                        ,_0: "address"
                                        ,_1: $Json$Encode.object(_L.fromArray([{ctor: "_Tuple2"
                                                                               ,_0: "street"
                                                                               ,_1: $Json$Encode.string(street)}
                                                                              ,{ctor: "_Tuple2"
                                                                               ,_0: "postalCode"
                                                                               ,_1: $Json$Encode.string(postalCode)}
                                                                              ,{ctor: "_Tuple2"
                                                                               ,_0: "city"
                                                                               ,_1: $Json$Encode.string(city)}]))}])));
   });
   var finishOrderEncoder = F2(function (reducedCount,
   type$) {
      return A2($Json$Encode.encode,
      0,
      $Json$Encode.object(_L.fromArray([{ctor: "_Tuple2"
                                        ,_0: "reducedCount"
                                        ,_1: $Json$Encode.$int(reducedCount)}
                                       ,{ctor: "_Tuple2"
                                        ,_0: "type"
                                        ,_1: $Json$Encode.string(type$)}])));
   });
   var orderEncoder = F4(function (name,
   email,
   seatIds,
   reducedCount) {
      return A2($Json$Encode.encode,
      0,
      $Json$Encode.object(_L.fromArray([{ctor: "_Tuple2"
                                        ,_0: "name"
                                        ,_1: $Json$Encode.string(name)}
                                       ,{ctor: "_Tuple2"
                                        ,_0: "reducedCount"
                                        ,_1: $Json$Encode.$int(reducedCount)}
                                       ,{ctor: "_Tuple2"
                                        ,_0: "email"
                                        ,_1: $Json$Encode.string(email)}
                                       ,{ctor: "_Tuple2"
                                        ,_0: "seatIds"
                                        ,_1: $Json$Encode.list(A2($List.map,
                                        $Json$Encode.string,
                                        seatIds))}])));
   });
   var dateDecoder = A2($Json$Decode.customDecoder,
   $Json$Decode.string,
   $Date.fromString);
   var rowDecoder = A3($Json$Decode.object2,
   $Model.Row,
   A2($Json$Decode._op[":="],
   "number",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "y",
   $Json$Decode.$int));
   var seatDecoder = A6($Json$Decode.object5,
   $Model.Seat,
   A2($Json$Decode._op[":="],
   "id",
   $Json$Decode.string),
   A2($Json$Decode._op[":="],
   "x",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "number",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "row",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "usable",
   $Json$Decode.bool));
   var seatsDecoder = A3($Json$Decode.object2,
   F2(function (v0,v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   }),
   A2($Json$Decode._op[":="],
   "seats",
   $Json$Decode.list(seatDecoder)),
   A2($Json$Decode._op[":="],
   "rows",
   $Json$Decode.list(rowDecoder)));
   var reservationDecoder = A2($Json$Decode.object1,
   $Model.Reservation,
   A2($Json$Decode._op[":="],
   "seatId",
   $Json$Decode.string));
   var reservationsDecoder = $Json$Decode.list(reservationDecoder);
   var Order = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,address: i
             ,createdAt: b
             ,email: d
             ,id: a
             ,name: c
             ,number: h
             ,paid: e
             ,reducedCount: f
             ,seatIds: g};
   });
   var Gig = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,date: b
             ,freeSeats: d
             ,id: a
             ,title: c};
   });
   var gigDecoder = $Json$Decode.list(A5($Json$Decode.object4,
   Gig,
   A2($Json$Decode._op[":="],
   "id",
   $Json$Decode.string),
   A2($Json$Decode._op[":="],
   "date",
   dateDecoder),
   A2($Json$Decode._op[":="],
   "title",
   $Json$Decode.string),
   A2($Json$Decode._op[":="],
   "freeSeats",
   $Json$Decode.$int)));
   var Address$ = F3(function (a,
   b,
   c) {
      return {_: {}
             ,city: c
             ,postalCode: b
             ,street: a};
   });
   var addressDecoder = A4($Json$Decode.object3,
   Address$,
   A2($Json$Decode._op[":="],
   "street",
   $Json$Decode.string),
   A2($Json$Decode._op[":="],
   "postalCode",
   $Json$Decode.string),
   A2($Json$Decode._op[":="],
   "city",
   $Json$Decode.string));
   var orderDecoder = function () {
      var foo = F3(function (key,
      decoder,
      v) {
         return unwrapResult(A2($Json$Decode.decodeValue,
         decoder,
         unwrap(A2($Dict.get,key,v))));
      });
      return A2($Json$Decode.customDecoder,
      $Json$Decode.dict($Json$Decode.value),
      function (v) {
         return $Result.Ok({_: {}
                           ,address: function () {
                              var _v4 = A2($Dict.get,
                              "address",
                              v);
                              switch (_v4.ctor)
                              {case "Just":
                                 return function () {
                                      var _v6 = A2($Json$Decode.decodeValue,
                                      addressDecoder,
                                      _v4._0);
                                      switch (_v6.ctor)
                                      {case "Err":
                                         return $Maybe.Nothing;
                                         case "Ok":
                                         return $Maybe.Just(_v6._0);}
                                      _U.badCase($moduleName,
                                      "between lines 137 and 140");
                                   }();
                                 case "Nothing":
                                 return $Maybe.Nothing;}
                              _U.badCase($moduleName,
                              "between lines 135 and 141");
                           }()
                           ,createdAt: A3(foo,
                           "createdAt",
                           dateDecoder,
                           v)
                           ,email: A3(foo,
                           "email",
                           $Json$Decode.string,
                           v)
                           ,id: A3(foo,
                           "id",
                           $Json$Decode.string,
                           v)
                           ,name: A3(foo,
                           "name",
                           $Json$Decode.string,
                           v)
                           ,number: A3(foo,
                           "number",
                           $Json$Decode.$int,
                           v)
                           ,paid: A3(foo,
                           "paid",
                           $Json$Decode.bool,
                           v)
                           ,reducedCount: A3(foo,
                           "reducedCount",
                           $Json$Decode.$int,
                           v)
                           ,seatIds: A3(foo,
                           "seatIds",
                           $Json$Decode.list($Json$Decode.string),
                           v)});
      });
   }();
   var ordersDecoder = $Json$Decode.list(orderDecoder);
   var baseApiEndpoint = "https://tickets-backend-ruby.herokuapp.com";
   var fetchGigs = A2(get,
   gigDecoder,
   A2($Basics._op["++"],
   baseApiEndpoint,
   "/gigs"));
   var fetchSeats = function (id) {
      return A2(get,
      seatsDecoder,
      A2($Basics._op["++"],
      baseApiEndpoint,
      A2($Basics._op["++"],
      "/gigs/",
      A2($Basics._op["++"],
      id,
      "/seats"))));
   };
   var fetchReservations = function (id) {
      return A2(get,
      reservationsDecoder,
      A2($Basics._op["++"],
      baseApiEndpoint,
      A2($Basics._op["++"],
      "/gigs/",
      A2($Basics._op["++"],
      id,
      "/reservations"))));
   };
   var submitOrder = F5(function (gigId,
   name,
   email,
   seatIds,
   reducedCount) {
      return A3(post,
      $Json$Decode.succeed(""),
      A2($Basics._op["++"],
      baseApiEndpoint,
      A2($Basics._op["++"],
      "/gigs/",
      A2($Basics._op["++"],
      gigId,
      "/orders"))),
      $Http.string(A4(orderEncoder,
      name,
      email,
      seatIds,
      reducedCount)));
   });
   var startOrder = F2(function (name,
   email) {
      return A3(post,
      A2($Json$Decode.at,
      _L.fromArray(["orderId"]),
      $Json$Decode.string),
      A2($Basics._op["++"],
      baseApiEndpoint,
      "/orders"),
      $Http.string(A4(orderEncoder,
      name,
      email,
      _L.fromArray([]),
      0)));
   });
   var finishOrder = F3(function (orderId,
   reducedCount,
   type$) {
      return A3(put,
      $Json$Decode.succeed(""),
      A2($Basics._op["++"],
      baseApiEndpoint,
      A2($Basics._op["++"],
      "/orders/",
      A2($Basics._op["++"],
      orderId,
      "/finish"))),
      $Http.string(A2(finishOrderEncoder,
      reducedCount,
      type$)));
   });
   var finishOrderWithAddress = F5(function (orderId,
   reducedCount,
   street,
   postalCode,
   city) {
      return A3(put,
      $Json$Decode.succeed(""),
      A2($Basics._op["++"],
      baseApiEndpoint,
      A2($Basics._op["++"],
      "/orders/",
      A2($Basics._op["++"],
      orderId,
      "/finish"))),
      $Http.string(A4(finishOrderWithAddressEncoder,
      reducedCount,
      street,
      postalCode,
      city)));
   });
   var orders = A2(get,
   ordersDecoder,
   A2($Basics._op["++"],
   baseApiEndpoint,
   "/orders"));
   var paid = F3(function (orderId,
   name,
   password) {
      return A3(put,
      $Json$Decode.succeed(""),
      A2($Basics._op["++"],
      baseApiEndpoint,
      A2($Basics._op["++"],
      "/orders/",
      A2($Basics._op["++"],
      orderId,
      "/pay"))),
      $Http.empty);
   });
   var unpaid = F3(function (orderId,
   name,
   password) {
      return A3(put,
      $Json$Decode.succeed(""),
      A2($Basics._op["++"],
      baseApiEndpoint,
      A2($Basics._op["++"],
      "/orders/",
      A2($Basics._op["++"],
      orderId,
      "/unpay"))),
      $Http.empty);
   });
   var reserveSeat = F2(function (orderId,
   seatId) {
      return A3(put,
      $Json$Decode.succeed(""),
      A2($Basics._op["++"],
      baseApiEndpoint,
      A2($Basics._op["++"],
      "/orders/",
      A2($Basics._op["++"],
      orderId,
      A2($Basics._op["++"],
      "/reservations/",
      seatId)))),
      $Http.empty);
   });
   var freeSeat = F2(function (orderId,
   seatId) {
      return A3($delete,
      $Json$Decode.succeed(""),
      A2($Basics._op["++"],
      baseApiEndpoint,
      A2($Basics._op["++"],
      "/orders/",
      A2($Basics._op["++"],
      orderId,
      A2($Basics._op["++"],
      "/reservations/",
      seatId)))),
      $Http.empty);
   });
   var login = F2(function (user,
   password) {
      return A3(post,
      A2($Json$Decode.at,
      _L.fromArray(["role"]),
      $Json$Decode.string),
      A2($Basics._op["++"],
      baseApiEndpoint,
      "/login"),
      $Http.string(A2($Json$Encode.encode,
      0,
      $Json$Encode.object(_L.fromArray([{ctor: "_Tuple2"
                                        ,_0: "user"
                                        ,_1: $Json$Encode.string(user)}
                                       ,{ctor: "_Tuple2"
                                        ,_0: "password"
                                        ,_1: $Json$Encode.string(password)}])))));
   });
   var cancelOrder = function (orderId) {
      return A3($delete,
      $Json$Decode.succeed(""),
      A2($Basics._op["++"],
      baseApiEndpoint,
      A2($Basics._op["++"],
      "/orders/",
      orderId)),
      $Http.empty);
   };
   _elm.HttpRequests.values = {_op: _op
                              ,fetchGigs: fetchGigs
                              ,fetchSeats: fetchSeats
                              ,fetchReservations: fetchReservations
                              ,submitOrder: submitOrder
                              ,startOrder: startOrder
                              ,reserveSeat: reserveSeat
                              ,freeSeat: freeSeat
                              ,finishOrder: finishOrder
                              ,finishOrderWithAddress: finishOrderWithAddress
                              ,login: login
                              ,orders: orders
                              ,paid: paid
                              ,unpaid: unpaid
                              ,cancelOrder: cancelOrder};
   return _elm.HttpRequests.values;
};
Elm.Json = Elm.Json || {};
Elm.Json.Decode = Elm.Json.Decode || {};
Elm.Json.Decode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Decode = _elm.Json.Decode || {};
   if (_elm.Json.Decode.values)
   return _elm.Json.Decode.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Json.Decode",
   $Array = Elm.Array.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Result = Elm.Result.make(_elm);
   var tuple8 = $Native$Json.decodeTuple8;
   var tuple7 = $Native$Json.decodeTuple7;
   var tuple6 = $Native$Json.decodeTuple6;
   var tuple5 = $Native$Json.decodeTuple5;
   var tuple4 = $Native$Json.decodeTuple4;
   var tuple3 = $Native$Json.decodeTuple3;
   var tuple2 = $Native$Json.decodeTuple2;
   var tuple1 = $Native$Json.decodeTuple1;
   var succeed = $Native$Json.succeed;
   var fail = $Native$Json.fail;
   var andThen = $Native$Json.andThen;
   var customDecoder = $Native$Json.customDecoder;
   var decodeValue = $Native$Json.runDecoderValue;
   var value = $Native$Json.decodeValue;
   var maybe = $Native$Json.decodeMaybe;
   var $null = $Native$Json.decodeNull;
   var array = $Native$Json.decodeArray;
   var list = $Native$Json.decodeList;
   var bool = $Native$Json.decodeBool;
   var $int = $Native$Json.decodeInt;
   var $float = $Native$Json.decodeFloat;
   var string = $Native$Json.decodeString;
   var oneOf = $Native$Json.oneOf;
   var keyValuePairs = $Native$Json.decodeKeyValuePairs;
   var object8 = $Native$Json.decodeObject8;
   var object7 = $Native$Json.decodeObject7;
   var object6 = $Native$Json.decodeObject6;
   var object5 = $Native$Json.decodeObject5;
   var object4 = $Native$Json.decodeObject4;
   var object3 = $Native$Json.decodeObject3;
   var object2 = $Native$Json.decodeObject2;
   var object1 = $Native$Json.decodeObject1;
   _op[":="] = $Native$Json.decodeField;
   var at = F2(function (fields,
   decoder) {
      return A3($List.foldr,
      F2(function (x,y) {
         return A2(_op[":="],x,y);
      }),
      decoder,
      fields);
   });
   var decodeString = $Native$Json.runDecoderString;
   var map = $Native$Json.decodeObject1;
   var dict = function (decoder) {
      return A2(map,
      $Dict.fromList,
      keyValuePairs(decoder));
   };
   var Decoder = {ctor: "Decoder"};
   _elm.Json.Decode.values = {_op: _op
                             ,decodeString: decodeString
                             ,decodeValue: decodeValue
                             ,string: string
                             ,$int: $int
                             ,$float: $float
                             ,bool: bool
                             ,$null: $null
                             ,list: list
                             ,array: array
                             ,tuple1: tuple1
                             ,tuple2: tuple2
                             ,tuple3: tuple3
                             ,tuple4: tuple4
                             ,tuple5: tuple5
                             ,tuple6: tuple6
                             ,tuple7: tuple7
                             ,tuple8: tuple8
                             ,at: at
                             ,object1: object1
                             ,object2: object2
                             ,object3: object3
                             ,object4: object4
                             ,object5: object5
                             ,object6: object6
                             ,object7: object7
                             ,object8: object8
                             ,keyValuePairs: keyValuePairs
                             ,dict: dict
                             ,maybe: maybe
                             ,oneOf: oneOf
                             ,map: map
                             ,fail: fail
                             ,succeed: succeed
                             ,andThen: andThen
                             ,value: value
                             ,customDecoder: customDecoder
                             ,Decoder: Decoder};
   return _elm.Json.Decode.values;
};
Elm.Json = Elm.Json || {};
Elm.Json.Encode = Elm.Json.Encode || {};
Elm.Json.Encode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Encode = _elm.Json.Encode || {};
   if (_elm.Json.Encode.values)
   return _elm.Json.Encode.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Json.Encode",
   $Array = Elm.Array.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm);
   var list = $Native$Json.encodeList;
   var array = $Native$Json.encodeArray;
   var object = $Native$Json.encodeObject;
   var $null = $Native$Json.encodeNull;
   var bool = $Native$Json.identity;
   var $float = $Native$Json.identity;
   var $int = $Native$Json.identity;
   var string = $Native$Json.identity;
   var encode = $Native$Json.encode;
   var Value = {ctor: "Value"};
   _elm.Json.Encode.values = {_op: _op
                             ,encode: encode
                             ,string: string
                             ,$int: $int
                             ,$float: $float
                             ,bool: bool
                             ,$null: $null
                             ,list: list
                             ,array: array
                             ,object: object
                             ,Value: Value};
   return _elm.Json.Encode.values;
};
Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values)
   return _elm.List.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "List",
   $Basics = Elm.Basics.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$List = Elm.Native.List.make(_elm);
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = function (xs) {
      return A2(sortBy,
      $Basics.identity,
      xs);
   };
   var repeat = $Native$List.repeat;
   var drop = $Native$List.drop;
   var take = $Native$List.take;
   var map5 = $Native$List.map5;
   var map4 = $Native$List.map4;
   var map3 = $Native$List.map3;
   var map2 = $Native$List.map2;
   var any = $Native$List.any;
   var all = F2(function (pred,
   xs) {
      return $Basics.not(A2(any,
      function ($) {
         return $Basics.not(pred($));
      },
      xs));
   });
   var foldr = $Native$List.foldr;
   var foldl = $Native$List.foldl;
   var length = function (xs) {
      return A3(foldl,
      F2(function (_v0,i) {
         return function () {
            return i + 1;
         }();
      }),
      0,
      xs);
   };
   var sum = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x + y;
      }),
      0,
      numbers);
   };
   var product = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x * y;
      }),
      1,
      numbers);
   };
   var maximum = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(A3(foldl,
              $Basics.max,
              list._0,
              list._1));}
         return $Maybe.Nothing;
      }();
   };
   var minimum = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(A3(foldl,
              $Basics.min,
              list._0,
              list._1));}
         return $Maybe.Nothing;
      }();
   };
   var indexedMap = F2(function (f,
   xs) {
      return A3(map2,
      f,
      _L.range(0,length(xs) - 1),
      xs);
   });
   var member = F2(function (x,
   xs) {
      return A2(any,
      function (a) {
         return _U.eq(a,x);
      },
      xs);
   });
   var isEmpty = function (xs) {
      return function () {
         switch (xs.ctor)
         {case "[]": return true;}
         return false;
      }();
   };
   var tail = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(list._1);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 87 and 89");
      }();
   };
   var head = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(list._0);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 75 and 77");
      }();
   };
   _op["::"] = $Native$List.cons;
   var map = F2(function (f,xs) {
      return A3(foldr,
      F2(function (x,acc) {
         return A2(_op["::"],
         f(x),
         acc);
      }),
      _L.fromArray([]),
      xs);
   });
   var filter = F2(function (pred,
   xs) {
      return function () {
         var conditionalCons = F2(function (x,
         xs$) {
            return pred(x) ? A2(_op["::"],
            x,
            xs$) : xs$;
         });
         return A3(foldr,
         conditionalCons,
         _L.fromArray([]),
         xs);
      }();
   });
   var maybeCons = F3(function (f,
   mx,
   xs) {
      return function () {
         var _v15 = f(mx);
         switch (_v15.ctor)
         {case "Just":
            return A2(_op["::"],_v15._0,xs);
            case "Nothing": return xs;}
         _U.badCase($moduleName,
         "between lines 179 and 181");
      }();
   });
   var filterMap = F2(function (f,
   xs) {
      return A3(foldr,
      maybeCons(f),
      _L.fromArray([]),
      xs);
   });
   var reverse = function (list) {
      return A3(foldl,
      F2(function (x,y) {
         return A2(_op["::"],x,y);
      }),
      _L.fromArray([]),
      list);
   };
   var scanl = F3(function (f,
   b,
   xs) {
      return function () {
         var scan1 = F2(function (x,
         accAcc) {
            return function () {
               switch (accAcc.ctor)
               {case "::": return A2(_op["::"],
                    A2(f,x,accAcc._0),
                    accAcc);
                  case "[]":
                  return _L.fromArray([]);}
               _U.badCase($moduleName,
               "between lines 148 and 151");
            }();
         });
         return reverse(A3(foldl,
         scan1,
         _L.fromArray([b]),
         xs));
      }();
   });
   var append = F2(function (xs,
   ys) {
      return function () {
         switch (ys.ctor)
         {case "[]": return xs;}
         return A3(foldr,
         F2(function (x,y) {
            return A2(_op["::"],x,y);
         }),
         ys,
         xs);
      }();
   });
   var concat = function (lists) {
      return A3(foldr,
      append,
      _L.fromArray([]),
      lists);
   };
   var concatMap = F2(function (f,
   list) {
      return concat(A2(map,
      f,
      list));
   });
   var partition = F2(function (pred,
   list) {
      return function () {
         var step = F2(function (x,
         _v21) {
            return function () {
               switch (_v21.ctor)
               {case "_Tuple2":
                  return pred(x) ? {ctor: "_Tuple2"
                                   ,_0: A2(_op["::"],x,_v21._0)
                                   ,_1: _v21._1} : {ctor: "_Tuple2"
                                                   ,_0: _v21._0
                                                   ,_1: A2(_op["::"],
                                                   x,
                                                   _v21._1)};}
               _U.badCase($moduleName,
               "between lines 301 and 303");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         list);
      }();
   });
   var unzip = function (pairs) {
      return function () {
         var step = F2(function (_v25,
         _v26) {
            return function () {
               switch (_v26.ctor)
               {case "_Tuple2":
                  return function () {
                       switch (_v25.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(_op["::"],
                                 _v25._0,
                                 _v26._0)
                                 ,_1: A2(_op["::"],
                                 _v25._1,
                                 _v26._1)};}
                       _U.badCase($moduleName,
                       "on line 339, column 12 to 28");
                    }();}
               _U.badCase($moduleName,
               "on line 339, column 12 to 28");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         pairs);
      }();
   };
   var intersperse = F2(function (sep,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "::": return function () {
                 var step = F2(function (x,
                 rest) {
                    return A2(_op["::"],
                    sep,
                    A2(_op["::"],x,rest));
                 });
                 var spersed = A3(foldr,
                 step,
                 _L.fromArray([]),
                 xs._1);
                 return A2(_op["::"],
                 xs._0,
                 spersed);
              }();
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 350 and 356");
      }();
   });
   _elm.List.values = {_op: _op
                      ,isEmpty: isEmpty
                      ,length: length
                      ,reverse: reverse
                      ,member: member
                      ,head: head
                      ,tail: tail
                      ,filter: filter
                      ,take: take
                      ,drop: drop
                      ,repeat: repeat
                      ,append: append
                      ,concat: concat
                      ,intersperse: intersperse
                      ,partition: partition
                      ,unzip: unzip
                      ,map: map
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,filterMap: filterMap
                      ,concatMap: concatMap
                      ,indexedMap: indexedMap
                      ,foldr: foldr
                      ,foldl: foldl
                      ,sum: sum
                      ,product: product
                      ,maximum: maximum
                      ,minimum: minimum
                      ,all: all
                      ,any: any
                      ,scanl: scanl
                      ,sort: sort
                      ,sortBy: sortBy
                      ,sortWith: sortWith};
   return _elm.List.values;
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values)
   return _elm.Maybe.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Maybe";
   var withDefault = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just": return maybe._0;
            case "Nothing":
            return $default;}
         _U.badCase($moduleName,
         "between lines 45 and 47");
      }();
   });
   var Nothing = {ctor: "Nothing"};
   var oneOf = function (maybes) {
      return function () {
         switch (maybes.ctor)
         {case "::": return function () {
                 switch (maybes._0.ctor)
                 {case "Just": return maybes._0;
                    case "Nothing":
                    return oneOf(maybes._1);}
                 _U.badCase($moduleName,
                 "between lines 64 and 66");
              }();
            case "[]": return Nothing;}
         _U.badCase($moduleName,
         "between lines 59 and 66");
      }();
   };
   var andThen = F2(function (maybeValue,
   callback) {
      return function () {
         switch (maybeValue.ctor)
         {case "Just":
            return callback(maybeValue._0);
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 110 and 112");
      }();
   });
   var Just = function (a) {
      return {ctor: "Just",_0: a};
   };
   var map = F2(function (f,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Just(f(maybe._0));
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 76 and 78");
      }();
   });
   _elm.Maybe.values = {_op: _op
                       ,andThen: andThen
                       ,map: map
                       ,withDefault: withDefault
                       ,oneOf: oneOf
                       ,Just: Just
                       ,Nothing: Nothing};
   return _elm.Maybe.values;
};
Elm.Model = Elm.Model || {};
Elm.Model.make = function (_elm) {
   "use strict";
   _elm.Model = _elm.Model || {};
   if (_elm.Model.values)
   return _elm.Model.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Model",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var clearSelections = function (m) {
      return _U.replace([["selections"
                         ,_L.fromArray([])]],
      m);
   };
   var selectionsAsText = function (selections) {
      return $String.concat($List.intersperse(", ")(A2($List.map,
      function (s) {
         return A2($Basics._op["++"],
         "(",
         A2($Basics._op["++"],
         $Basics.toString(s.row),
         A2($Basics._op["++"],
         ", ",
         A2($Basics._op["++"],
         $Basics.toString(s.number),
         ")"))));
      },
      selections)));
   };
   var reserveSeats = F2(function (model,
   seats) {
      return function () {
         var newReservations = A2($List.map,
         function (s) {
            return {_: {},seatId: s.id};
         },
         seats);
         return _U.replace([["reservations"
                            ,A2($Basics._op["++"],
                            model.reservations,
                            newReservations)]],
         model);
      }();
   });
   var updateReservations = F2(function (model,
   reservations) {
      return _U.replace([["reservations"
                         ,reservations]],
      model);
   });
   var updateSeats = F3(function (model,
   seats,
   rows) {
      return _U.replace([["rows"
                         ,rows]
                        ,["seats",seats]],
      model);
   });
   var seatsInRow = F2(function (model,
   row) {
      return A2($List.filter,
      function (s) {
         return _U.eq(s.row,
         row.number);
      },
      model.seats);
   });
   var rows = function (model) {
      return model.rows;
   };
   var findSeat = F2(function (model,
   seatId) {
      return function () {
         var _v0 = A2($List.filter,
         function (s) {
            return _U.eq(s.id,seatId);
         },
         model.seats);
         switch (_v0.ctor)
         {case "::":
            return $Maybe.Just(_v0._0);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 69 and 71");
      }();
   });
   var isReserved = F2(function (m,
   s) {
      return $List.member(s.id)(A2($List.map,
      function (_) {
         return _.seatId;
      },
      m.reservations));
   });
   var isUsable = function (_) {
      return _.usable;
   };
   var isSelected = F2(function (m,
   s) {
      return A2($List.member,
      s,
      m.selections);
   });
   var initialModel = {_: {}
                      ,reservations: _L.fromArray([])
                      ,rows: _L.fromArray([])
                      ,seats: _L.fromArray([])
                      ,selections: _L.fromArray([])};
   var unselectSeat = F2(function (model,
   seat) {
      return function () {
         var _v3 = seat.usable;
         switch (_v3)
         {case false: return model;
            case true:
            return _U.replace([["selections"
                               ,A2($List.filter,
                               F2(function (x,y) {
                                  return !_U.eq(x,y);
                               })(seat),
                               model.selections)]],
              model);}
         _U.badCase($moduleName,
         "between lines 50 and 52");
      }();
   });
   var freeSeats = F2(function (model,
   seatIds) {
      return _U.replace([["reservations"
                         ,A2($List.filter,
                         function (r) {
                            return $Basics.not(A2($List.member,
                            r.seatId,
                            seatIds));
                         },
                         model.reservations)]],
      model);
   });
   var unwrap = function (m) {
      return function () {
         switch (m.ctor)
         {case "Just": return m._0;}
         _U.badCase($moduleName,
         "between lines 28 and 29");
      }();
   };
   var compact = function ($) {
      return $List.map(unwrap)($List.filter(function (e) {
         return function () {
            switch (e.ctor)
            {case "Just": return true;
               case "Nothing": return false;}
            _U.badCase($moduleName,
            "between lines 37 and 40");
         }();
      })($));
   };
   var alwaysSelectSeat = F2(function (model,
   seat) {
      return _U.replace([["selections"
                         ,A2($List._op["::"],
                         seat,
                         model.selections)]],
      model);
   });
   var selectSeatIds = F2(function (model,
   seatIds) {
      return function () {
         var selectSeats = F2(function (model,
         seats) {
            return A3($List.foldl,
            F2(function (s,m) {
               return A2(alwaysSelectSeat,
               m,
               s);
            }),
            model,
            seats);
         });
         return A2(selectSeats,
         model,
         compact(A2($List.map,
         function (s) {
            return A2(findSeat,model,s);
         },
         seatIds)));
      }();
   });
   var selectSeat = F2(function (model,
   seat) {
      return function () {
         var _v8 = seat.usable;
         switch (_v8)
         {case false: return model;
            case true: return A2(isReserved,
              model,
              seat) ? model : A2(alwaysSelectSeat,
              model,
              seat);}
         _U.badCase($moduleName,
         "between lines 17 and 20");
      }();
   });
   var Model = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,reservations: a
             ,rows: c
             ,seats: b
             ,selections: d};
   });
   var Seat = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,id: a
             ,number: c
             ,row: d
             ,usable: e
             ,x: b};
   });
   var Row = F2(function (a,b) {
      return {_: {}
             ,number: a
             ,y: b};
   });
   var Reservation = function (a) {
      return {_: {},seatId: a};
   };
   _elm.Model.values = {_op: _op
                       ,selectionsAsText: selectionsAsText
                       ,isUsable: isUsable
                       ,updateSeats: updateSeats
                       ,updateReservations: updateReservations
                       ,reserveSeats: reserveSeats
                       ,freeSeats: freeSeats
                       ,rows: rows
                       ,seatsInRow: seatsInRow
                       ,initialModel: initialModel
                       ,isSelected: isSelected
                       ,isReserved: isReserved
                       ,selectSeat: selectSeat
                       ,unselectSeat: unselectSeat
                       ,clearSelections: clearSelections
                       ,findSeat: findSeat
                       ,selectSeatIds: selectSeatIds
                       ,Model: Model
                       ,Seat: Seat
                       ,Row: Row
                       ,Reservation: Reservation};
   return _elm.Model.values;
};
Elm.Native.Array = {};
Elm.Native.Array.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Array = localRuntime.Native.Array || {};
	if (localRuntime.Native.Array.values)
	{
		return localRuntime.Native.Array.values;
	}
	if ('values' in Elm.Native.Array)
	{
		return localRuntime.Native.Array.values = Elm.Native.Array.values;
	}

	var List = Elm.Native.List.make(localRuntime);

	// A RRB-Tree has two distinct data types.
	// Leaf -> "height"  is always 0
	//         "table"   is an array of elements
	// Node -> "height"  is always greater than 0
	//         "table"   is an array of child nodes
	//         "lengths" is an array of accumulated lengths of the child nodes

	// M is the maximal table size. 32 seems fast. E is the allowed increase
	// of search steps when concatting to find an index. Lower values will
	// decrease balancing, but will increase search steps.
	var M = 32;
	var E = 2;

	// An empty array.
	var empty = {
		ctor: "_Array",
		height: 0,
		table: new Array()
	};


	function get(i, array)
	{
		if (i < 0 || i >= length(array))
		{
			throw new Error(
				"Index " + i + " is out of range. Check the length of " +
				"your array first or use getMaybe or getWithDefault.");
		}
		return unsafeGet(i, array);
	}


	function unsafeGet(i, array)
	{
		for (var x = array.height; x > 0; x--)
		{
			var slot = i >> (x * 5);
			while (array.lengths[slot] <= i)
			{
				slot++;
			}
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array = array.table[slot];
		}
		return array.table[i];
	}


	// Sets the value at the index i. Only the nodes leading to i will get
	// copied and updated.
	function set(i, item, array)
	{
		if (i < 0 || length(array) <= i)
		{
			return array;
		}
		return unsafeSet(i, item, array);
	}


	function unsafeSet(i, item, array)
	{
		array = nodeCopy(array);

		if (array.height == 0)
		{
			array.table[i] = item;
		}
		else
		{
			var slot = getSlot(i, array);
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array.table[slot] = unsafeSet(i, item, array.table[slot]);
		}
		return array;
	}


	function initialize(len, f)
	{
		if (len == 0)
		{
			return empty;
		}
		var h = Math.floor( Math.log(len) / Math.log(M) );
		return initialize_(f, h, 0, len);
	}

	function initialize_(f, h, from, to)
	{
		if (h == 0)
		{
			var table = new Array((to - from) % (M + 1));
			for (var i = 0; i < table.length; i++)
			{
			  table[i] = f(from + i);
			}
			return {
				ctor: "_Array",
				height: 0,
				table: table
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: "_Array",
			height: h,
			table: table,
			lengths: lengths
		};
	}

	function fromList(list)
	{
		if (list == List.Nil)
		{
			return empty;
		}

		// Allocate M sized blocks (table) and write list elements to it.
		var table = new Array(M);
		var nodes = new Array();
		var i = 0;

		while (list.ctor !== '[]')
		{
			table[i] = list._0;
			list = list._1;
			i++;

			// table is full, so we can push a leaf containing it into the
			// next node.
			if (i == M)
			{
				var leaf = {
					ctor: "_Array",
					height: 0,
					table: table
				};
				fromListPush(leaf, nodes);
				table = new Array(M);
				i = 0;
			}
		}

		// Maybe there is something left on the table.
		if (i > 0)
		{
			var leaf = {
				ctor: "_Array",
				height: 0,
				table: table.splice(0,i)
			};
			fromListPush(leaf, nodes);
		}

		// Go through all of the nodes and eventually push them into higher nodes.
		for (var h = 0; h < nodes.length - 1; h++)
		{
			if (nodes[h].table.length > 0)
			{
				fromListPush(nodes[h], nodes);
			}
		}

		var head = nodes[nodes.length - 1];
		if (head.height > 0 && head.table.length == 1)
		{
			return head.table[0];
		}
		else
		{
			return head;
		}
	}

	// Push a node into a higher node as a child.
	function fromListPush(toPush, nodes)
	{
		var h = toPush.height;

		// Maybe the node on this height does not exist.
		if (nodes.length == h)
		{
			var node = {
				ctor: "_Array",
				height: h + 1,
				table: new Array(),
				lengths: new Array()
			};
			nodes.push(node);
		}

		nodes[h].table.push(toPush);
		var len = length(toPush);
		if (nodes[h].lengths.length > 0)
		{
			len += nodes[h].lengths[nodes[h].lengths.length - 1];
		}
		nodes[h].lengths.push(len);

		if (nodes[h].table.length == M)
		{
			fromListPush(nodes[h], nodes);
			nodes[h] = {
				ctor: "_Array",
				height: h + 1,
				table: new Array(),
				lengths: new Array()
			};
		}
	}

	// Pushes an item via push_ to the bottom right of a tree.
	function push(item, a)
	{
		var pushed = push_(item, a);
		if (pushed !== null)
		{
			return pushed;
		}

		var newTree = create(item, a.height);
		return siblise(a, newTree);
	}

	// Recursively tries to push an item to the bottom-right most
	// tree possible. If there is no space left for the item,
	// null will be returned.
	function push_(item, a)
	{
		// Handle resursion stop at leaf level.
		if (a.height == 0)
		{
			if (a.table.length < M)
			{
				var newA = {
					ctor: "_Array",
					height: 0,
					table: a.table.slice()
				};
				newA.table.push(item);
				return newA;
			}
			else
			{
			  return null;
			}
		}

		// Recursively push
		var pushed = push_(item, botRight(a));

		// There was space in the bottom right tree, so the slot will
		// be updated.
		if (pushed != null)
		{
			var newA = nodeCopy(a);
			newA.table[newA.table.length - 1] = pushed;
			newA.lengths[newA.lengths.length - 1]++;
			return newA;
		}

		// When there was no space left, check if there is space left
		// for a new slot with a tree which contains only the item
		// at the bottom.
		if (a.table.length < M)
		{
			var newSlot = create(item, a.height - 1);
			var newA = nodeCopy(a);
			newA.table.push(newSlot);
			newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
			return newA;
		}
		else
		{
			return null;
		}
	}

	// Converts an array into a list of elements.
	function toList(a)
	{
		return toList_(List.Nil, a);
	}

	function toList_(list, a)
	{
		for (var i = a.table.length - 1; i >= 0; i--)
		{
			list =
				a.height == 0
					? List.Cons(a.table[i], list)
					: toList_(list, a.table[i]);
		}
		return list;
	}

	// Maps a function over the elements of an array.
	function map(f, a)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height == 0
					? f(a.table[i])
					: map(f, a.table[i]);
		}
		return newA;
	}

	// Maps a function over the elements with their index as first argument.
	function indexedMap(f, a)
	{
		return indexedMap_(f, a, 0);
	}

	function indexedMap_(f, a, from)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height == 0
					? A2(f, from + i, a.table[i])
					: indexedMap_(f, a.table[i], i == 0 ? 0 : a.lengths[i - 1]);
		}
		return newA;
	}

	function foldl(f, b, a)
	{
		if (a.height == 0)
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = foldl(f, b, a.table[i]);
			}
		}
		return b;
	}

	function foldr(f, b, a)
	{
		if (a.height == 0)
		{
			for (var i = a.table.length; i--; )
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = a.table.length; i--; )
			{
				b = foldr(f, b, a.table[i]);
			}
		}
		return b;
	}

	// TODO: currently, it slices the right, then the left. This can be
	// optimized.
	function slice(from, to, a)
	{
		if (from < 0)
		{
			from += length(a);
		}
		if (to < 0)
		{
			to += length(a);
		}
		return sliceLeft(from, sliceRight(to, a));
	}

	function sliceRight(to, a)
	{
		if (to == length(a))
		{
			return a;
		}

		// Handle leaf level.
		if (a.height == 0)
		{
			var newA = { ctor:"_Array", height:0 };
			newA.table = a.table.slice(0, to);
			return newA;
		}

		// Slice the right recursively.
		var right = getSlot(to, a);
		var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (right == 0)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice(0, right),
			lengths: a.lengths.slice(0, right)
		};
		if (sliced.table.length > 0)
		{
			newA.table[right] = sliced;
			newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
		}
		return newA;
	}

	function sliceLeft(from, a)
	{
		if (from == 0)
		{
			return a;
		}

		// Handle leaf level.
		if (a.height == 0)
		{
			var newA = { ctor:"_Array", height:0 };
			newA.table = a.table.slice(from, a.table.length + 1);
			return newA;
		}

		// Slice the left recursively.
		var left = getSlot(from, a);
		var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (left == a.table.length - 1)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice(left, a.table.length + 1),
			lengths: new Array(a.table.length - left)
		};
		newA.table[0] = sliced;
		var len = 0;
		for (var i = 0; i < newA.table.length; i++)
		{
			len += length(newA.table[i]);
			newA.lengths[i] = len;
		}

		return newA;
	}

	// Appends two trees.
	function append(a,b)
	{
		if (a.table.length === 0)
		{
			return b;
		}
		if (b.table.length === 0)
		{
			return a;
		}

		var c = append_(a, b);

		// Check if both nodes can be crunshed together.
		if (c[0].table.length + c[1].table.length <= M)
		{
			if (c[0].table.length === 0)
			{
				return c[1];
			}
			if (c[1].table.length === 0)
			{
				return c[0];
			}

			// Adjust .table and .lengths
			c[0].table = c[0].table.concat(c[1].table);
			if (c[0].height > 0)
			{
				var len = length(c[0]);
				for (var i = 0; i < c[1].lengths.length; i++)
				{
					c[1].lengths[i] += len;
				}
				c[0].lengths = c[0].lengths.concat(c[1].lengths);
			}

			return c[0];
		}

		if (c[0].height > 0)
		{
			var toRemove = calcToRemove(a, b);
			if (toRemove > E)
			{
				c = shuffle(c[0], c[1], toRemove);
			}
		}

		return siblise(c[0], c[1]);
	}

	// Returns an array of two nodes; right and left. One node _may_ be empty.
	function append_(a, b)
	{
		if (a.height === 0 && b.height === 0)
		{
			return [a, b];
		}

		if (a.height !== 1 || b.height !== 1)
		{
			if (a.height === b.height)
			{
				a = nodeCopy(a);
				b = nodeCopy(b);
				var appended = append_(botRight(a), botLeft(b));

				insertRight(a, appended[1]);
				insertLeft(b, appended[0]);
			}
			else if (a.height > b.height)
			{
				a = nodeCopy(a);
				var appended = append_(botRight(a), b);

				insertRight(a, appended[0]);
				b = parentise(appended[1], appended[1].height + 1);
			}
			else
			{
				b = nodeCopy(b);
				var appended = append_(a, botLeft(b));

				var left = appended[0].table.length === 0 ? 0 : 1;
				var right = left === 0 ? 1 : 0;
				insertLeft(b, appended[left]);
				a = parentise(appended[right], appended[right].height + 1);
			}
		}

		// Check if balancing is needed and return based on that.
		if (a.table.length === 0 || b.table.length === 0)
		{
			return [a,b];
		}

		var toRemove = calcToRemove(a, b);
		if (toRemove <= E)
		{
			return [a,b];
		}
		return shuffle(a, b, toRemove);
	}

	// Helperfunctions for append_. Replaces a child node at the side of the parent.
	function insertRight(parent, node)
	{
		var index = parent.table.length - 1;
		parent.table[index] = node;
		parent.lengths[index] = length(node)
		parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
	}

	function insertLeft(parent, node)
	{
		if (node.table.length > 0)
		{
			parent.table[0] = node;
			parent.lengths[0] = length(node);

			var len = length(parent.table[0]);
			for (var i = 1; i < parent.lengths.length; i++)
			{
				len += length(parent.table[i]);
				parent.lengths[i] = len;
			}
		}
		else
		{
			parent.table.shift();
			for (var i = 1; i < parent.lengths.length; i++)
			{
				parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
			}
			parent.lengths.shift();
		}
	}

	// Returns the extra search steps for E. Refer to the paper.
	function calcToRemove(a, b)
	{
		var subLengths = 0;
		for (var i = 0; i < a.table.length; i++)
		{
			subLengths += a.table[i].table.length;
		}
		for (var i = 0; i < b.table.length; i++)
		{
			subLengths += b.table[i].table.length;
		}

		var toRemove = a.table.length + b.table.length
		return toRemove - (Math.floor((subLengths - 1) / M) + 1);
	}

	// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
	function get2(a, b, index)
	{
		return index < a.length
			? a[index]
			: b[index - a.length];
	}

	function set2(a, b, index, value)
	{
		if (index < a.length)
		{
			a[index] = value;
		}
		else
		{
			b[index - a.length] = value;
		}
	}

	function saveSlot(a, b, index, slot)
	{
		set2(a.table, b.table, index, slot);

		var l = (index == 0 || index == a.lengths.length)
			? 0
			: get2(a.lengths, a.lengths, index - 1);

		set2(a.lengths, b.lengths, index, l + length(slot));
	}

	// Creates a node or leaf with a given length at their arrays for perfomance.
	// Is only used by shuffle.
	function createNode(h, length)
	{
		if (length < 0)
		{
			length = 0;
		}
		var a = {
			ctor: "_Array",
			height: h,
			table: new Array(length)
		};
		if (h > 0)
		{
			a.lengths = new Array(length);
		}
		return a;
	}

	// Returns an array of two balanced nodes.
	function shuffle(a, b, toRemove)
	{
		var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
		var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

		// Skip the slots with size M. More precise: copy the slot references
		// to the new node
		var read = 0;
		while (get2(a.table, b.table, read).table.length % M == 0)
		{
			set2(newA.table, newB.table, read, get2(a.table, b.table, read));
			set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
			read++;
		}

		// Pulling items from left to right, caching in a slot before writing
		// it into the new nodes.
		var write = read;
		var slot = new createNode(a.height - 1, 0);
		var from = 0;

		// If the current slot is still containing data, then there will be at
		// least one more write, so we do not break this loop yet.
		while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
		{
			// Find out the max possible items for copying.
			var source = get2(a.table, b.table, read);
			var to = Math.min(M - slot.table.length, source.table.length)

			// Copy and adjust size table.
			slot.table = slot.table.concat(source.table.slice(from, to));
			if (slot.height > 0)
			{
				var len = slot.lengths.length;
				for (var i = len; i < len + to - from; i++)
				{
					slot.lengths[i] = length(slot.table[i]);
					slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
				}
			}

			from += to;

			// Only proceed to next slots[i] if the current one was
			// fully copied.
			if (source.table.length <= to)
			{
				read++; from = 0;
			}

			// Only create a new slot if the current one is filled up.
			if (slot.table.length == M)
			{
				saveSlot(newA, newB, write, slot);
				slot = createNode(a.height - 1,0);
				write++;
			}
		}

		// Cleanup after the loop. Copy the last slot into the new nodes.
		if (slot.table.length > 0)
		{
			saveSlot(newA, newB, write, slot);
			write++;
		}

		// Shift the untouched slots to the left
		while (read < a.table.length + b.table.length )
		{
			saveSlot(newA, newB, write, get2(a.table, b.table, read));
			read++;
			write++;
		}

		return [newA, newB];
	}

	// Navigation functions
	function botRight(a)
	{
		return a.table[a.table.length - 1];
	}
	function botLeft(a)
	{
		return a.table[0];
	}

	// Copies a node for updating. Note that you should not use this if
	// only updating only one of "table" or "lengths" for performance reasons.
	function nodeCopy(a)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice()
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths.slice();
		}
		return newA;
	}

	// Returns how many items are in the tree.
	function length(array)
	{
		if (array.height == 0)
		{
			return array.table.length;
		}
		else
		{
			return array.lengths[array.lengths.length - 1];
		}
	}

	// Calculates in which slot of "table" the item probably is, then
	// find the exact slot via forward searching in  "lengths". Returns the index.
	function getSlot(i, a)
	{
		var slot = i >> (5 * a.height);
		while (a.lengths[slot] <= i)
		{
			slot++;
		}
		return slot;
	}

	// Recursively creates a tree with a given height containing
	// only the given item.
	function create(item, h)
	{
		if (h == 0)
		{
			return {
				ctor: "_Array",
				height: 0,
				table: [item]
			};
		}
		return {
			ctor: "_Array",
			height: h,
			table: [create(item, h - 1)],
			lengths: [1]
		};
	}

	// Recursively creates a tree that contains the given tree.
	function parentise(tree, h)
	{
		if (h == tree.height)
		{
			return tree;
		}

		return {
			ctor: "_Array",
			height: h,
			table: [parentise(tree, h - 1)],
			lengths: [length(tree)]
		};
	}

	// Emphasizes blood brotherhood beneath two trees.
	function siblise(a, b)
	{
		return {
			ctor: "_Array",
			height: a.height + 1,
			table: [a, b],
			lengths: [length(a), length(a) + length(b)]
		};
	}

	function toJSArray(a)
	{
		var jsArray = new Array(length(a));
		toJSArray_(jsArray, 0, a);
		return jsArray;
	}

	function toJSArray_(jsArray, i, a)
	{
		for (var t = 0; t < a.table.length; t++)
		{
			if (a.height == 0)
			{
				jsArray[i + t] = a.table[t];
			}
			else
			{
				var inc = t == 0 ? 0 : a.lengths[t - 1];
				toJSArray_(jsArray, i + inc, a.table[t]);
			}
		}
	}

	function fromJSArray(jsArray)
	{
		if (jsArray.length == 0)
		{
			return empty;
		}
		var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
		return fromJSArray_(jsArray, h, 0, jsArray.length);
	}

	function fromJSArray_(jsArray, h, from, to)
	{
		if (h == 0)
		{
			return {
				ctor: "_Array",
				height: 0,
				table: jsArray.slice(from, to)
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: "_Array",
			height: h,
			table: table,
			lengths: lengths
		};
	}

	Elm.Native.Array.values = {
		empty: empty,
		fromList: fromList,
		toList: toList,
		initialize: F2(initialize),
		append: F2(append),
		push: F2(push),
		slice: F3(slice),
		get: F2(get),
		set: F3(set),
		map: F2(map),
		indexedMap: F2(indexedMap),
		foldl: F3(foldl),
		foldr: F3(foldr),
		length: length,

		toJSArray:toJSArray,
		fromJSArray:fromJSArray
	};

	return localRuntime.Native.Array.values = Elm.Native.Array.values;

}

Elm.Native.Basics = {};
Elm.Native.Basics.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Basics = localRuntime.Native.Basics || {};
	if (localRuntime.Native.Basics.values)
	{
		return localRuntime.Native.Basics.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	function div(a, b)
	{
		return (a/b)|0;
	}
	function rem(a, b)
	{
		return a % b;
	}
	function mod(a, b)
	{
		if (b === 0)
		{
			throw new Error("Cannot perform mod 0. Division by zero error.");
		}
		var r = a % b;
		var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

		return m === b ? 0 : m;
	}
	function logBase(base, n)
	{
		return Math.log(n) / Math.log(base);
	}
	function negate(n)
	{
		return -n;
	}
	function abs(n)
	{
		return n < 0 ? -n : n;
	}

	function min(a, b)
	{
		return Utils.cmp(a,b) < 0 ? a : b;
	}
	function max(a, b)
	{
		return Utils.cmp(a,b) > 0 ? a : b;
	}
	function clamp(lo, hi, n)
	{
		return Utils.cmp(n,lo) < 0 ? lo : Utils.cmp(n,hi) > 0 ? hi : n;
	}

	function xor(a, b)
	{
		return a !== b;
	}
	function not(b)
	{
		return !b;
	}
	function isInfinite(n)
	{
		return n === Infinity || n === -Infinity
	}

	function truncate(n)
	{
		return n|0;
	}

	function degrees(d)
	{
		return d * Math.PI / 180;
	}
	function turns(t)
	{
		return 2 * Math.PI * t;
	}
	function fromPolar(point)
	{
		var r = point._0;
		var t = point._1;
		return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
	}
	function toPolar(point)
	{
		var x = point._0;
		var y = point._1;
		return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y,x));
	}

	return localRuntime.Native.Basics.values = {
		div: F2(div),
		rem: F2(rem),
		mod: F2(mod),

		pi: Math.PI,
		e: Math.E,
		cos: Math.cos,
		sin: Math.sin,
		tan: Math.tan,
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: F2(Math.atan2),

		degrees:  degrees,
		turns:  turns,
		fromPolar:  fromPolar,
		toPolar:  toPolar,

		sqrt: Math.sqrt,
		logBase: F2(logBase),
		negate: negate,
		abs: abs,
		min: F2(min),
		max: F2(max),
		clamp: F3(clamp),
		compare: Utils.compare,

		xor: F2(xor),
		not: not,

		truncate: truncate,
		ceiling: Math.ceil,
		floor: Math.floor,
		round: Math.round,
		toFloat: function(x) { return x; },
		isNaN: isNaN,
		isInfinite: isInfinite
	};
};

Elm.Native.Char = {};
Elm.Native.Char.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Char = localRuntime.Native.Char || {};
	if (localRuntime.Native.Char.values)
	{
		return localRuntime.Native.Char.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	return localRuntime.Native.Char.values = {
		fromCode : function(c) { return Utils.chr(String.fromCharCode(c)); },
		toCode   : function(c) { return c.charCodeAt(0); },
		toUpper  : function(c) { return Utils.chr(c.toUpperCase()); },
		toLower  : function(c) { return Utils.chr(c.toLowerCase()); },
		toLocaleUpper : function(c) { return Utils.chr(c.toLocaleUpperCase()); },
		toLocaleLower : function(c) { return Utils.chr(c.toLocaleLowerCase()); },
	};
};

Elm.Native.Color = {};
Elm.Native.Color.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Color = localRuntime.Native.Color || {};
	if (localRuntime.Native.Color.values)
	{
		return localRuntime.Native.Color.values;
	}

	function toCss(c)
	{
		var format = '';
		var colors = '';
		if (c.ctor === 'RGBA')
		{
			format = 'rgb';
			colors = c._0 + ', ' + c._1 + ', ' + c._2;
		}
		else
		{
			format = 'hsl';
			colors = (c._0 * 180 / Math.PI) + ', ' +
					 (c._1 * 100) + '%, ' +
					 (c._2 * 100) + '%';
		}
		if (c._3 === 1)
		{
			return format + '(' + colors + ')';
		}
		else
		{
			return format + 'a(' + colors + ', ' + c._3 + ')';
		}
	}

	return localRuntime.Native.Color.values = {
		toCss: toCss
	};

};

Elm.Native.Date = {};
Elm.Native.Date.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Date = localRuntime.Native.Date || {};
	if (localRuntime.Native.Date.values)
	{
		return localRuntime.Native.Date.values;
	}

	var Result = Elm.Result.make(localRuntime);

	function dateNow()
	{
		return new window.Date;
	}

	function readDate(str)
	{
		var date = new window.Date(str);
		return isNaN(date.getTime())
			? Result.Err("unable to parse '" + str + "' as a date")
			: Result.Ok(date);
	}

	var dayTable = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
	var monthTable =
		["Jan", "Feb", "Mar", "Apr", "May", "Jun",
		 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];


	return localRuntime.Native.Date.values = {
		read    : readDate,
		year    : function(d) { return d.getFullYear(); },
		month   : function(d) { return { ctor:monthTable[d.getMonth()] }; },
		day     : function(d) { return d.getDate(); },
		hour    : function(d) { return d.getHours(); },
		minute  : function(d) { return d.getMinutes(); },
		second  : function(d) { return d.getSeconds(); },
		millisecond: function (d) { return d.getMilliseconds(); },
		toTime  : function(d) { return d.getTime(); },
		fromTime: function(t) { return new window.Date(t); },
		dayOfWeek : function(d) { return { ctor:dayTable[d.getDay()] }; }
	};

};

Elm.Native.Debug = {};
Elm.Native.Debug.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debug = localRuntime.Native.Debug || {};
	if (localRuntime.Native.Debug.values)
	{
		return localRuntime.Native.Debug.values;
	}

	var toString = Elm.Native.Show.make(localRuntime).toString;

	function log(tag, value)
	{
		var msg = tag + ': ' + toString(value);
		var process = process || {};
		if (process.stdout)
		{
			process.stdout.write(msg);
		}
		else
		{
			console.log(msg);
		}
		return value;
	}

	function crash(message)
	{
		throw new Error(message);
	}

	function tracePath(tag, form)
	{
		if (localRuntime.debug)
		{
			return localRuntime.debug.trace(tag, form);
		}
		return form;
	}

	function watch(tag, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, value);
		}
		return value;
	}

	function watchSummary(tag, summarize, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, summarize(value));
		}
		return value;
	}

	return localRuntime.Native.Debug.values = {
		crash: crash,
		tracePath: F2(tracePath),
		log: F2(log),
		watch: F2(watch),
		watchSummary:F3(watchSummary),
	};
};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Collage = Elm.Native.Graphics.Collage || {};

// definition
Elm.Native.Graphics.Collage.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Collage = localRuntime.Native.Graphics.Collage || {};
	if ('values' in localRuntime.Native.Graphics.Collage)
	{
		return localRuntime.Native.Graphics.Collage.values;
	}

	// okay, we cannot short-ciruit, so now we define everything
	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);
	var Transform = Elm.Transform2D.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function setStrokeStyle(ctx, style)
	{
		ctx.lineWidth = style.width;

		var cap = style.cap.ctor;
		ctx.lineCap = cap === 'Flat'
			? 'butt'
			: cap === 'Round'
				? 'round'
				: 'square';

		var join = style.join.ctor;
		ctx.lineJoin = join === 'Smooth'
			? 'round'
			: join === 'Sharp'
				? 'miter'
				: 'bevel';

		ctx.miterLimit = style.join._0 || 10;
		ctx.strokeStyle = Color.toCss(style.color);
	}

	function setFillStyle(ctx, style)
	{
		var sty = style.ctor;
		ctx.fillStyle = sty === 'Solid'
			? Color.toCss(style._0)
			: sty === 'Texture'
				? texture(redo, ctx, style._0)
				: gradient(ctx, style._0);
	}

	function trace(ctx, path)
	{
		var points = List.toArray(path);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		ctx.moveTo(points[i]._0, points[i]._1);
		while (i--)
		{
			ctx.lineTo(points[i]._0, points[i]._1);
		}
		if (path.closed)
		{
			i = points.length - 1;
			ctx.lineTo(points[i]._0, points[i]._1);
		}
	}

	function line(ctx,style,path)
	{
		(style.dashing.ctor === '[]')
			? trace(ctx, path)
			: customLineHelp(ctx, style, path);
		ctx.scale(1,-1);
		ctx.stroke();
	}

	function customLineHelp(ctx, style, path)
	{
		var points = List.toArray(path);
		if (path.closed)
		{
			points.push(points[0]);
		}
		var pattern = List.toArray(style.dashing);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		var x0 = points[i]._0, y0 = points[i]._1;
		var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
		var pindex = 0, plen = pattern.length;
		var draw = true, segmentLength = pattern[0];
		ctx.moveTo(x0,y0);
		while (i--)
		{
			x1 = points[i]._0;
			y1 = points[i]._1;
			dx = x1 - x0;
			dy = y1 - y0;
			remaining = Math.sqrt(dx * dx + dy * dy);
			while (segmentLength <= remaining)
			{
				x0 += dx * segmentLength / remaining;
				y0 += dy * segmentLength / remaining;
				ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
				// update starting position
				dx = x1 - x0;
				dy = y1 - y0;
				remaining = Math.sqrt(dx * dx + dy * dy);
				// update pattern
				draw = !draw;
				pindex = (pindex + 1) % plen;
				segmentLength = pattern[pindex];
			}
			if (remaining > 0)
			{
				ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
				segmentLength -= remaining;
			}
			x0 = x1;
			y0 = y1;
		}
	}

	function drawLine(ctx, style, path)
	{
		setStrokeStyle(ctx, style);
		return line(ctx, style, path);
	}

	function texture(redo, ctx, src)
	{
		var img = new Image();
		img.src = src;
		img.onload = redo;
		return ctx.createPattern(img, 'repeat');
	}

	function gradient(ctx, grad)
	{
		var g;
		var stops = [];
		if (grad.ctor === 'Linear')
		{
			var p0 = grad._0, p1 = grad._1;
			g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
			stops = List.toArray(grad._2);
		}
		else
		{
			var p0 = grad._0, p2 = grad._2;
			g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
			stops = List.toArray(grad._4);
		}
		var len = stops.length;
		for (var i = 0; i < len; ++i)
		{
			var stop = stops[i];
			g.addColorStop(stop._0, Color.toCss(stop._1));
		}
		return g;
	}

	function drawShape(redo, ctx, style, path)
	{
		trace(ctx, path);
		setFillStyle(ctx, style);
		ctx.scale(1,-1);
		ctx.fill();
	}


	// TEXT RENDERING

	function fillText(redo, ctx, text)
	{
		drawText(ctx, text, ctx.fillText);
	}

	function strokeText(redo, ctx, style, text)
	{
		setStrokeStyle(ctx, style);
		// Use native canvas API for dashes only for text for now
		// Degrades to non-dashed on IE 9 + 10
		if (style.dashing.ctor !== '[]' && ctx.setLineDash)
		{
			var pattern = List.toArray(style.dashing);
			ctx.setLineDash(pattern);
		}
		drawText(ctx, text, ctx.strokeText);
	}

	function drawText(ctx, text, canvasDrawFn)
	{
		var textChunks = chunkText(defaultContext, text);

		var totalWidth = 0;
		var maxHeight = 0;
		var numChunks = textChunks.length;

		ctx.scale(1,-1);

		for (var i = numChunks; i--; )
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			var metrics = ctx.measureText(chunk.text);
			chunk.width = metrics.width;
			totalWidth += chunk.width;
			if (chunk.height > maxHeight)
			{
				maxHeight = chunk.height;
			}
		}

		var x = -totalWidth / 2.0;
		for (var i = 0; i < numChunks; ++i)
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			ctx.fillStyle = chunk.color;
			canvasDrawFn.call(ctx, chunk.text, x, maxHeight / 2);
			x += chunk.width;
		}
	}

	function toFont(props)
	{
		return [
			props['font-style'],
			props['font-variant'],
			props['font-weight'],
			props['font-size'],
			props['font-family']
		].join(' ');
	}


	// Convert the object returned by the text module
	// into something we can use for styling canvas text
	function chunkText(context, text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			var leftChunks = chunkText(context, text._0);
			var rightChunks = chunkText(context, text._1);
			return leftChunks.concat(rightChunks);
		}
		if (tag === 'Text:Text')
		{
			return [{
				text: text._0,
				color: context.color,
				height: context['font-size'].slice(0,-2) | 0,
				font: toFont(context)
			}];
		}
		if (tag === 'Text:Meta')
		{
			var newContext = freshContext(text._0, context);
			return chunkText(newContext, text._1);
		}
	}

	function freshContext(props, ctx)
	{
		return {
			'font-style': props['font-style'] || ctx['font-style'],
			'font-variant': props['font-variant'] || ctx['font-variant'],
			'font-weight': props['font-weight'] || ctx['font-weight'],
			'font-size': props['font-size'] || ctx['font-size'],
			'font-family': props['font-family'] || ctx['font-family'],
			'color': props['color'] || ctx['color']
		};
	}

	var defaultContext = {
		'font-style': 'normal',
		'font-variant': 'normal',
		'font-weight': 'normal',
		'font-size': '12px',
		'font-family': 'sans-serif',
		'color': 'black'
	};


	// IMAGES

	function drawImage(redo, ctx, form)
	{
		var img = new Image();
		img.onload = redo;
		img.src = form._3;
		var w = form._0,
			h = form._1,
			pos = form._2,
			srcX = pos._0,
			srcY = pos._1,
			srcW = w,
			srcH = h,
			destX = -w/2,
			destY = -h/2,
			destW = w,
			destH = h;

		ctx.scale(1,-1);
		ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
	}

	function renderForm(redo, ctx, form)
	{
		ctx.save();

		var x = form.x,
			y = form.y,
			theta = form.theta,
			scale = form.scale;

		if (x !== 0 || y !== 0)
		{
			ctx.translate(x, y);
		}
		if (theta !== 0)
		{
			ctx.rotate(theta);
		}
		if (scale !== 1)
		{
			ctx.scale(scale,scale);
		}
		if (form.alpha !== 1)
		{
			ctx.globalAlpha = ctx.globalAlpha * form.alpha;
		}

		ctx.beginPath();
		var f = form.form;
		switch (f.ctor)
		{
			case 'FPath':
				drawLine(ctx, f._0, f._1);
				break;

			case 'FImage':
				drawImage(redo, ctx, f);
				break;

			case 'FShape':
				if (f._0.ctor === 'Line')
				{
					f._1.closed = true;
					drawLine(ctx, f._0._0, f._1);
				}
				else
				{
					drawShape(redo, ctx, f._0._0, f._1);
				}
				break;

			case 'FText':
				fillText(redo, ctx, f._0);
				break;

			case 'FOutlinedText':
				strokeText(redo, ctx, f._0, f._1);
				break;
		}
		ctx.restore();
	}

	function formToMatrix(form)
	{
	   var scale = form.scale;
	   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

	   var theta = form.theta
	   if (theta !== 0)
	   {
		   matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
	   }

	   return matrix;
	}

	function str(n)
	{
		if (n < 0.00001 && n > -0.00001)
		{
			return 0;
		}
		return n;
	}

	function makeTransform(w, h, form, matrices)
	{
		var props = form.form._0.props;
		var m = A6( Transform.matrix, 1, 0, 0, -1,
					(w - props.width ) / 2,
					(h - props.height) / 2 );
		var len = matrices.length;
		for (var i = 0; i < len; ++i)
		{
			m = A2( Transform.multiply, m, matrices[i] );
		}
		m = A2( Transform.multiply, m, formToMatrix(form) );

		return 'matrix(' +
			str( m[0]) + ', ' + str( m[3]) + ', ' +
			str(-m[1]) + ', ' + str(-m[4]) + ', ' +
			str( m[2]) + ', ' + str( m[5]) + ')';
	}

	function stepperHelp(list)
	{
		var arr = List.toArray(list);
		var i = 0;
		function peekNext()
		{
			return i < arr.length ? arr[i].form.ctor : '';
		}
		// assumes that there is a next element
		function next()
		{
			var out = arr[i];
			++i;
			return out;
		}
		return {
			peekNext: peekNext,
			next: next
		};
	}

	function formStepper(forms)
	{
		var ps = [stepperHelp(forms)];
		var matrices = [];
		var alphas = [];
		function peekNext()
		{
			var len = ps.length;
			var formType = '';
			for (var i = 0; i < len; ++i )
			{
				if (formType = ps[i].peekNext()) return formType;
			}
			return '';
		}
		// assumes that there is a next element
		function next(ctx)
		{
			while (!ps[0].peekNext())
			{
				ps.shift();
				matrices.pop();
				alphas.shift();
				if (ctx)
				{
					ctx.restore();
				}
			}
			var out = ps[0].next();
			var f = out.form;
			if (f.ctor === 'FGroup')
			{
				ps.unshift(stepperHelp(f._1));
				var m = A2(Transform.multiply, f._0, formToMatrix(out));
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
				matrices.push(m);

				var alpha = (alphas[0] || 1) * out.alpha;
				alphas.unshift(alpha);
				ctx.globalAlpha = alpha;
			}
			return out;
		}
		function transforms()
		{
			return matrices;
		}
		function alpha()
		{
			return alphas[0] || 1;
		}
		return {
			peekNext: peekNext,
			next: next,
			transforms: transforms,
			alpha: alpha
		};
	}

	function makeCanvas(w,h)
	{
		var canvas = NativeElement.createNode('canvas');
		canvas.style.width  = w + 'px';
		canvas.style.height = h + 'px';
		canvas.style.display = "block";
		canvas.style.position = "absolute";
		var ratio = window.devicePixelRatio || 1;
		canvas.width  = w * ratio;
		canvas.height = h * ratio;
		return canvas;
	}

	function render(model)
	{
		var div = NativeElement.createNode('div');
		div.style.overflow = 'hidden';
		div.style.position = 'relative';
		update(div, model, model);
		return div;
	}

	function nodeStepper(w,h,div)
	{
		var kids = div.childNodes;
		var i = 0;
		var ratio = window.devicePixelRatio || 1;

		function transform(transforms, ctx)
		{
			ctx.translate( w / 2 * ratio, h / 2 * ratio );
			ctx.scale( ratio, -ratio );
			var len = transforms.length;
			for (var i = 0; i < len; ++i)
			{
				var m = transforms[i];
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
			}
			return ctx;
		}
		function nextContext(transforms)
		{
			while (i < kids.length)
			{
				var node = kids[i];
				if (node.getContext)
				{
					node.width = w * ratio;
					node.height = h * ratio;
					node.style.width = w + 'px';
					node.style.height = h + 'px';
					++i;
					return transform(transforms, node.getContext('2d'));
				}
				div.removeChild(node);
			}
			var canvas = makeCanvas(w,h);
			div.appendChild(canvas);
			// we have added a new node, so we must step our position
			++i;
			return transform(transforms, canvas.getContext('2d'));
		}
		function addElement(matrices, alpha, form)
		{
			var kid = kids[i];
			var elem = form.form._0;

			var node = (!kid || kid.getContext)
				? NativeElement.render(elem)
				: NativeElement.update(kid, kid.oldElement, elem);

			node.style.position = 'absolute';
			node.style.opacity = alpha * form.alpha * elem.props.opacity;
			NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
			node.oldElement = elem;
			++i;
			if (!kid)
			{
				div.appendChild(node);
			}
			else
			{
				div.insertBefore(node, kid);
			}
		}
		function clearRest()
		{
			while (i < kids.length)
			{
				div.removeChild(kids[i]);
			}
		}
		return {
			nextContext: nextContext,
			addElement: addElement,
			clearRest: clearRest
		};
	}


	function update(div, _, model)
	{
		var w = model.w;
		var h = model.h;

		var forms = formStepper(model.forms);
		var nodes = nodeStepper(w,h,div);
		var ctx = null;
		var formType = '';

		while (formType = forms.peekNext())
		{
			// make sure we have context if we need it
			if (ctx === null && formType !== 'FElement')
			{
				ctx = nodes.nextContext(forms.transforms());
				ctx.globalAlpha = forms.alpha();
			}

			var form = forms.next(ctx);
			// if it is FGroup, all updates are made within formStepper when next is called.
			if (formType === 'FElement')
			{
				// update or insert an element, get a new context
				nodes.addElement(forms.transforms(), forms.alpha(), form);
				ctx = null;
			}
			else if (formType !== 'FGroup')
			{
				renderForm(function() { update(div, model, model); }, ctx, form);
			}
		}
		nodes.clearRest();
		return div;
	}


	function collage(w,h,forms)
	{
		return A3(NativeElement.newElement, w, h, {
			ctor: 'Custom',
			type: 'Collage',
			render: render,
			update: update,
			model: {w:w, h:h, forms:forms}
		});
	}

	return localRuntime.Native.Graphics.Collage.values = {
		collage: F3(collage)
	};

};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Element = Elm.Native.Graphics.Element || {};

// definition
Elm.Native.Graphics.Element.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Element = localRuntime.Native.Graphics.Element || {};
	if ('values' in localRuntime.Native.Graphics.Element)
	{
		return localRuntime.Native.Graphics.Element.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CREATION

	function createNode(elementType)
	{
		var node = document.createElement(elementType);
		node.style.padding = "0";
		node.style.margin = "0";
		return node;
	}


	function newElement(width, height, elementPrim)
	{
		return {
			_: {},
			element: elementPrim,
			props: {
				_: {},
				id: Utils.guid(),
				width: width,
				height: height,
				opacity: 1,
				color: Maybe.Nothing,
				href: "",
				tag: "",
				hover: Utils.Tuple0,
				click: Utils.Tuple0
			}
		};
	}


	// PROPERTIES

	function setProps(elem, node)
	{
		var props = elem.props;

		var element = elem.element;
		var width = props.width - (element.adjustWidth || 0);
		var height = props.height - (element.adjustHeight || 0);
		node.style.width  = (width |0) + 'px';
		node.style.height = (height|0) + 'px';

		if (props.opacity !== 1)
		{
			node.style.opacity = props.opacity;
		}

		if (props.color.ctor === 'Just')
		{
			node.style.backgroundColor = Color.toCss(props.color._0);
		}

		if (props.tag !== '')
		{
			node.id = props.tag;
		}

		if (props.hover.ctor !== '_Tuple0')
		{
			addHover(node, props.hover);
		}

		if (props.click.ctor !== '_Tuple0')
		{
			addClick(node, props.click);
		}

		if (props.href !== '')
		{
			var anchor = createNode('a');
			anchor.href = props.href;
			anchor.style.display = 'block';
			anchor.style.pointerEvents = 'auto';
			anchor.appendChild(node);
			node = anchor;
		}

		return node;
	}

	function addClick(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_click_handler = handler;
		function trigger(ev)
		{
			e.elm_click_handler(Utils.Tuple0);
			ev.stopPropagation();
		}
		e.elm_click_trigger = trigger;
		e.addEventListener('click', trigger);
	}

	function removeClick(e, handler)
	{
		if (e.elm_click_trigger)
		{
			e.removeEventListener('click', e.elm_click_trigger);
			e.elm_click_trigger = null;
			e.elm_click_handler = null;
		}
	}

	function addHover(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_hover_handler = handler;
		e.elm_hover_count = 0;

		function over(evt)
		{
			if (e.elm_hover_count++ > 0) return;
			e.elm_hover_handler(true);
			evt.stopPropagation();
		}
		function out(evt)
		{
			if (e.contains(evt.toElement || evt.relatedTarget)) return;
			e.elm_hover_count = 0;
			e.elm_hover_handler(false);
			evt.stopPropagation();
		}
		e.elm_hover_over = over;
		e.elm_hover_out = out;
		e.addEventListener('mouseover', over);
		e.addEventListener('mouseout', out);
	}

	function removeHover(e)
	{
		e.elm_hover_handler = null;
		if (e.elm_hover_over)
		{
			e.removeEventListener('mouseover', e.elm_hover_over);
			e.elm_hover_over = null;
		}
		if (e.elm_hover_out)
		{
			e.removeEventListener('mouseout', e.elm_hover_out);
			e.elm_hover_out = null;
		}
	}


	// IMAGES

	function image(props, img)
	{
		switch (img._0.ctor)
		{
			case 'Plain':
				return plainImage(img._3);

			case 'Fitted':
				return fittedImage(props.width, props.height, img._3);

			case 'Cropped':
				return croppedImage(img,props.width,props.height,img._3);

			case 'Tiled':
				return tiledImage(img._3);
		}
	}

	function plainImage(src)
	{
		var img = createNode('img');
		img.src = src;
		img.name = src;
		img.style.display = "block";
		return img;
	}

	function tiledImage(src)
	{
		var div = createNode('div');
		div.style.backgroundImage = 'url(' + src + ')';
		return div;
	}

	function fittedImage(w, h, src)
	{
		var div = createNode('div');
		div.style.background = 'url(' + src + ') no-repeat center';
		div.style.webkitBackgroundSize = 'cover';
		div.style.MozBackgroundSize = 'cover';
		div.style.OBackgroundSize = 'cover';
		div.style.backgroundSize = 'cover';
		return div;
	}

	function croppedImage(elem, w, h, src)
	{
		var pos = elem._0._0;
		var e = createNode('div');
		e.style.overflow = "hidden";

		var img = createNode('img');
		img.onload = function() {
			var sw = w / elem._1, sh = h / elem._2;
			img.style.width = ((this.width * sw)|0) + 'px';
			img.style.height = ((this.height * sh)|0) + 'px';
			img.style.marginLeft = ((- pos._0 * sw)|0) + 'px';
			img.style.marginTop = ((- pos._1 * sh)|0) + 'px';
		};
		img.src = src;
		img.name = src;
		e.appendChild(img);
		return e;
	}


	// FLOW

	function goOut(node)
	{
		node.style.position = 'absolute';
		return node;
	}
	function goDown(node)
	{
		return node;
	}
	function goRight(node)
	{
		node.style.styleFloat = 'left';
		node.style.cssFloat = 'left';
		return node;
	}

	var directionTable = {
		DUp    : goDown,
		DDown  : goDown,
		DLeft  : goRight,
		DRight : goRight,
		DIn    : goOut,
		DOut   : goOut
	};
	function needsReversal(dir)
	{
		return dir == 'DUp' || dir == 'DLeft' || dir == 'DIn';
	}

	function flow(dir,elist)
	{
		var array = List.toArray(elist);
		var container = createNode('div');
		var goDir = directionTable[dir];
		if (goDir == goOut)
		{
			container.style.pointerEvents = 'none';
		}
		if (needsReversal(dir))
		{
			array.reverse();
		}
		var len = array.length;
		for (var i = 0; i < len; ++i)
		{
			container.appendChild(goDir(render(array[i])));
		}
		return container;
	}


	// CONTAINER

	function toPos(pos)
	{
		return pos.ctor === "Absolute"
			? pos._0 + "px"
			: (pos._0 * 100) + "%";
	}

	// must clear right, left, top, bottom, and transform
	// before calling this function
	function setPos(pos,elem,e)
	{
		var element = elem.element;
		var props = elem.props;
		var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
		var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

		e.style.position = 'absolute';
		e.style.margin = 'auto';
		var transform = '';

		switch (pos.horizontal.ctor)
		{
			case 'P':
				e.style.right = toPos(pos.x);
				e.style.removeProperty('left');
				break;

			case 'Z':
				transform = 'translateX(' + ((-w/2)|0) + 'px) ';

			case 'N':
				e.style.left = toPos(pos.x);
				e.style.removeProperty('right');
				break;
		}
		switch (pos.vertical.ctor)
		{
			case 'N':
				e.style.bottom = toPos(pos.y);
				e.style.removeProperty('top');
				break;

			case 'Z':
				transform += 'translateY(' + ((-h/2)|0) + 'px)';

			case 'P':
				e.style.top = toPos(pos.y);
				e.style.removeProperty('bottom');
				break;
		}
		if (transform !== '')
		{
			addTransform(e.style, transform);
		}
		return e;
	}

	function addTransform(style, transform)
	{
		style.transform       = transform;
		style.msTransform     = transform;
		style.MozTransform    = transform;
		style.webkitTransform = transform;
		style.OTransform      = transform;
	}

	function container(pos,elem)
	{
		var e = render(elem);
		setPos(pos, elem, e);
		var div = createNode('div');
		div.style.position = 'relative';
		div.style.overflow = 'hidden';
		div.appendChild(e);
		return div;
	}


	function rawHtml(elem)
	{
		var html = elem.html;
		var guid = elem.guid;
		var align = elem.align;

		var div = createNode('div');
		div.innerHTML = html;
		div.style.visibility = "hidden";
		if (align)
		{
			div.style.textAlign = align;
		}
		div.style.visibility = 'visible';
		div.style.pointerEvents = 'auto';
		return div;
	}


	// RENDER

	function render(elem)
	{
		return setProps(elem, makeElement(elem));
	}
	function makeElement(e)
	{
		var elem = e.element;
		switch(elem.ctor)
		{
			case 'Image':
				return image(e.props, elem);

			case 'Flow':
				return flow(elem._0.ctor, elem._1);

			case 'Container':
				return container(elem._0, elem._1);

			case 'Spacer':
				return createNode('div');

			case 'RawHtml':
				return rawHtml(elem);

			case 'Custom':
				return elem.render(elem.model);
		}
	}

	function updateAndReplace(node, curr, next)
	{
		var newNode = update(node, curr, next);
		if (newNode !== node)
		{
			node.parentNode.replaceChild(newNode, node);
		}
		return newNode;
	}


	// UPDATE

	function update(node, curr, next)
	{
		var rootNode = node;
		if (node.tagName === 'A')
		{
			node = node.firstChild;
		}
		if (curr.props.id === next.props.id)
		{
			updateProps(node, curr, next);
			return rootNode;
		}
		if (curr.element.ctor !== next.element.ctor)
		{
			return render(next);
		}
		var nextE = next.element;
		var currE = curr.element;
		switch(nextE.ctor)
		{
			case "Spacer":
				updateProps(node, curr, next);
				return rootNode;

			case "RawHtml":
				if(currE.html.valueOf() !== nextE.html.valueOf())
				{
					node.innerHTML = nextE.html;
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Image":
				if (nextE._0.ctor === 'Plain')
				{
					if (nextE._3 !== currE._3)
					{
						node.src = nextE._3;
					}
				}
				else if (!Utils.eq(nextE,currE)
					|| next.props.width !== curr.props.width
					|| next.props.height !== curr.props.height)
				{
					return render(next);
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Flow":
				var arr = List.toArray(nextE._1);
				for (var i = arr.length; i--; )
				{
					arr[i] = arr[i].element.ctor;
				}
				if (nextE._0.ctor !== currE._0.ctor)
				{
					return render(next);
				}
				var nexts = List.toArray(nextE._1);
				var kids = node.childNodes;
				if (nexts.length !== kids.length)
				{
					return render(next);
				}
				var currs = List.toArray(currE._1);
				var dir = nextE._0.ctor;
				var goDir = directionTable[dir];
				var toReverse = needsReversal(dir);
				var len = kids.length;
				for (var i = len; i-- ;)
				{
					var subNode = kids[toReverse ? len - i - 1 : i];
					goDir(updateAndReplace(subNode, currs[i], nexts[i]));
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Container":
				var subNode = node.firstChild;
				var newSubNode = updateAndReplace(subNode, currE._1, nextE._1);
				setPos(nextE._0, nextE._1, newSubNode);
				updateProps(node, curr, next);
				return rootNode;

			case "Custom":
				if (currE.type === nextE.type)
				{
					var updatedNode = nextE.update(node, currE.model, nextE.model);
					updateProps(updatedNode, curr, next);
					return updatedNode;
				}
				return render(next);
		}
	}

	function updateProps(node, curr, next)
	{
		var nextProps = next.props;
		var currProps = curr.props;

		var element = next.element;
		var width = nextProps.width - (element.adjustWidth || 0);
		var height = nextProps.height - (element.adjustHeight || 0);
		if (width !== currProps.width)
		{
			node.style.width = (width|0) + 'px';
		}
		if (height !== currProps.height)
		{
			node.style.height = (height|0) + 'px';
		}

		if (nextProps.opacity !== currProps.opacity)
		{
			node.style.opacity = nextProps.opacity;
		}

		var nextColor = nextProps.color.ctor === 'Just'
			? Color.toCss(nextProps.color._0)
			: '';
		if (node.style.backgroundColor !== nextColor)
		{
			node.style.backgroundColor = nextColor;
		}

		if (nextProps.tag !== currProps.tag)
		{
			node.id = nextProps.tag;
		}

		if (nextProps.href !== currProps.href)
		{
			if (currProps.href === '')
			{
				// add a surrounding href
				var anchor = createNode('a');
				anchor.href = nextProps.href;
				anchor.style.display = 'block';
				anchor.style.pointerEvents = 'auto';

				node.parentNode.replaceChild(anchor, node);
				anchor.appendChild(node);
			}
			else if (nextProps.href === '')
			{
				// remove the surrounding href
				var anchor = node.parentNode;
				anchor.parentNode.replaceChild(node, anchor);
			}
			else
			{
				// just update the link
				node.parentNode.href = nextProps.href;
			}
		}

		// update click and hover handlers
		var removed = false;

		// update hover handlers
		if (currProps.hover.ctor === '_Tuple0')
		{
			if (nextProps.hover.ctor !== '_Tuple0')
			{
				addHover(node, nextProps.hover);
			}
		}
		else
		{
			if (nextProps.hover.ctor === '_Tuple0')
			{
				removed = true;
				removeHover(node);
			}
			else
			{
				node.elm_hover_handler = nextProps.hover;
			}
		}

		// update click handlers
		if (currProps.click.ctor === '_Tuple0')
		{
			if (nextProps.click.ctor !== '_Tuple0')
			{
				addClick(node, nextProps.click);
			}
		}
		else
		{
			if (nextProps.click.ctor === '_Tuple0')
			{
				removed = true;
				removeClick(node);
			}
			else
			{
				node.elm_click_handler = nextProps.click;
			}
		}

		// stop capturing clicks if
		if (removed
			&& nextProps.hover.ctor === '_Tuple0'
			&& nextProps.click.ctor === '_Tuple0')
		{
			node.style.pointerEvents = 'none';
		}
	}


	// TEXT

	function block(align)
	{
		return function(text)
		{
			var raw = {
				ctor :'RawHtml',
				html : Text.renderHtml(text),
				align: align
			};
			var pos = htmlHeight(0, raw);
			return newElement(pos._0, pos._1, raw);
		}
	}

	function markdown(text)
	{
		var raw = {
			ctor:'RawHtml',
			html: text,
			align: null
		};
		var pos = htmlHeight(0, raw);
		return newElement(pos._0, pos._1, raw);
	}

	function htmlHeight(width, rawHtml)
	{
		// create dummy node
		var temp = document.createElement('div');
		temp.innerHTML = rawHtml.html;
		if (width > 0)
		{
			temp.style.width = width + "px";
		}
		temp.style.visibility = "hidden";
		temp.style.styleFloat = "left";
		temp.style.cssFloat   = "left";

		document.body.appendChild(temp);

		// get dimensions
		var style = window.getComputedStyle(temp, null);
		var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
		var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
		document.body.removeChild(temp);
		return Utils.Tuple2(w,h);
	}


	return localRuntime.Native.Graphics.Element.values = {
		render: render,
		update: update,
		updateAndReplace: updateAndReplace,

		createNode: createNode,
		newElement: F3(newElement),
		addTransform: addTransform,
		htmlHeight: F2(htmlHeight),
		guid: Utils.guid,

		block: block,
		markdown: markdown
	};

};

Elm.Native.Http = {};
Elm.Native.Http.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Http = localRuntime.Native.Http || {};
	if (localRuntime.Native.Http.values)
	{
		return localRuntime.Native.Http.values;
	}

	var Dict = Elm.Dict.make(localRuntime);
	var List = Elm.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Task = Elm.Native.Task.make(localRuntime);


	function send(settings, request)
	{
		return Task.asyncFunction(function(callback) {
			var req = new XMLHttpRequest();

			// start
			if (settings.onStart.ctor === 'Just')
			{
				req.addEventListener('loadStart', function() {
					var task = settings.onStart._0;
					Task.spawn(task);
				});
			}

			// progress
			if (settings.onProgress.ctor === 'Just')
			{
				req.addEventListener('progress', function(event) {
					var progress = !event.lengthComputable
						? Maybe.Nothing
						: Maybe.Just({
							_: {},
							loaded: event.loaded,
							total: event.total
						});
					var task = settings.onProgress._0(progress);
					Task.spawn(task);
				});
			}

			// end
			req.addEventListener('error', function() {
				return callback(Task.fail({ ctor: 'RawNetworkError' }));
			});

			req.addEventListener('timeout', function() {
				return callback(Task.fail({ ctor: 'RawTimeout' }));
			});

			req.addEventListener('load', function() {
				return callback(Task.succeed(toResponse(req)));
			});

			req.open(request.verb, request.url, true);

			// set all the headers
			function setHeader(pair) {
				req.setRequestHeader(pair._0, pair._1);
			}
			A2(List.map, setHeader, request.headers);

			// set the timeout
			req.timeout = settings.timeout;

			// ask for a specific MIME type for the response
			if (settings.desiredResponseType.ctor === 'Just')
			{
				req.overrideMimeType(settings.desiredResponseType._0);
			}

			req.send(request.body._0);
		});
	}


	// deal with responses

	function toResponse(req)
	{
		var tag = typeof req.response === 'string' ? 'Text' : 'Blob';
		return {
			_: {},
			status: req.status,
			statusText: req.statusText,
			headers: parseHeaders(req.getAllResponseHeaders()),
			url: req.responseURL,
			value: { ctor: tag, _0: req.response }
		};
	}


	function parseHeaders(rawHeaders)
	{
		var headers = Dict.empty;

		if (!rawHeaders)
		{
			return headers;
		}

		var headerPairs = rawHeaders.split('\u000d\u000a');
		for (var i = headerPairs.length; i--; )
		{
			var headerPair = headerPairs[i];
			var index = headerPair.indexOf('\u003a\u0020');
			if (index > 0)
			{
				var key = headerPair.substring(0, index);
				var value = headerPair.substring(index + 2);

				headers = A3(Dict.update, key, function(oldValue) {
					if (oldValue.ctor === 'Just')
					{
						return Maybe.Just(value + ', ' + oldValue._0);
					}
					return Maybe.Just(value);
				}, headers);
			}
		}

		return headers;
	}


	function multipart(dataList)
	{
		var formData = new FormData();

		while (dataList.ctor !== '[]')
		{
			var data = dataList._0;
			if (type === 'StringData')
			{
				formData.append(data._0, data._1);
			}
			else
			{
				var fileName = data._1.ctor === 'Nothing'
					? undefined
					: data._1._0;
				formData.append(data._0, data._2, fileName);
			}
			dataList = dataList._1;
		}

		return { ctor: 'FormData', formData: formData };
	}


	function uriEncode(string)
	{
		return encodeURIComponent(string);
	}

	function uriDecode(string)
	{
		return decodeURIComponent(string);
	}

	return localRuntime.Native.Http.values = {
		send: F2(send),
		multipart: multipart,
		uriEncode: uriEncode,
		uriDecode: uriDecode
	};
};

Elm.Native.Json = {};
Elm.Native.Json.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Json = localRuntime.Native.Json || {};
	if (localRuntime.Native.Json.values) {
		return localRuntime.Native.Json.values;
	}

	var ElmArray = Elm.Native.Array.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function crash(expected, actual) {
		throw new Error(
			'expecting ' + expected + ' but got ' + JSON.stringify(actual)
		);
	}


	// PRIMITIVE VALUES

	function decodeNull(successValue) {
		return function(value) {
			if (value === null) {
				return successValue;
			}
			crash('null', value);
		};
	}


	function decodeString(value) {
		if (typeof value === 'string' || value instanceof String) {
			return value;
		}
		crash('a String', value);
	}


	function decodeFloat(value) {
		if (typeof value === 'number') {
			return value;
		}
		crash('a Float', value);
	}


	function decodeInt(value) {
		if (typeof value === 'number' && (value|0) === value) {
			return value;
		}
		crash('an Int', value);
	}


	function decodeBool(value) {
		if (typeof value === 'boolean') {
			return value;
		}
		crash('a Bool', value);
	}


	// ARRAY

	function decodeArray(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var array = new Array(len);
				for (var i = len; i-- ; ) {
					array[i] = decoder(value[i]);
				}
				return ElmArray.fromJSArray(array);
			}
			crash('an Array', value);
		};
	}


	// LIST

	function decodeList(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var list = List.Nil;
				for (var i = len; i-- ; ) {
					list = List.Cons( decoder(value[i]), list );
				}
				return list;
			}
			crash('a List', value);
		};
	}


	// MAYBE

	function decodeMaybe(decoder) {
		return function(value) {
			try {
				return Maybe.Just(decoder(value));
			} catch(e) {
				return Maybe.Nothing;
			}
		};
	}


	// FIELDS

	function decodeField(field, decoder) {
		return function(value) {
			var subValue = value[field];
			if (subValue !== undefined) {
				return decoder(subValue);
			}
			crash("an object with field '" + field + "'", value);
		};
	}


	// OBJECTS

	function decodeKeyValuePairs(decoder) {
		return function(value) {
			var isObject =
				typeof value === 'object'
					&& value !== null
					&& !(value instanceof Array);

			if (isObject) {
				var keyValuePairs = List.Nil;
				for (var key in value) {
					var elmValue = decoder(value[key]);
					var pair = Utils.Tuple2(key, elmValue);
					keyValuePairs = List.Cons(pair, keyValuePairs);
				}
				return keyValuePairs;
			}

			crash("an object", value);
		};
	}

	function decodeObject1(f, d1) {
		return function(value) {
			return f(d1(value));
		};
	}

	function decodeObject2(f, d1, d2) {
		return function(value) {
			return A2( f, d1(value), d2(value) );
		};
	}

	function decodeObject3(f, d1, d2, d3) {
		return function(value) {
			return A3( f, d1(value), d2(value), d3(value) );
		};
	}

	function decodeObject4(f, d1, d2, d3, d4) {
		return function(value) {
			return A4( f, d1(value), d2(value), d3(value), d4(value) );
		};
	}

	function decodeObject5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			return A5( f, d1(value), d2(value), d3(value), d4(value), d5(value) );
		};
	}

	function decodeObject6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			return A6( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value)
			);
		};
	}

	function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			return A7( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value)
			);
		};
	}

	function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			return A8( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value),
				d8(value)
			);
		};
	}


	// TUPLES

	function decodeTuple1(f, d1) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 1 ) {
				crash('a Tuple of length 1', value);
			}
			return f( d1(value[0]) );
		};
	}

	function decodeTuple2(f, d1, d2) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 2 ) {
				crash('a Tuple of length 2', value);
			}
			return A2( f, d1(value[0]), d2(value[1]) );
		};
	}

	function decodeTuple3(f, d1, d2, d3) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 3 ) {
				crash('a Tuple of length 3', value);
			}
			return A3( f, d1(value[0]), d2(value[1]), d3(value[2]) );
		};
	}


	function decodeTuple4(f, d1, d2, d3, d4) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 4 ) {
				crash('a Tuple of length 4', value);
			}
			return A4( f, d1(value[0]), d2(value[1]), d3(value[2]), d4(value[3]) );
		};
	}


	function decodeTuple5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 5 ) {
				crash('a Tuple of length 5', value);
			}
			return A5( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4])
			);
		};
	}


	function decodeTuple6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 6 ) {
				crash('a Tuple of length 6', value);
			}
			return A6( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5])
			);
		};
	}

	function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 7 ) {
				crash('a Tuple of length 7', value);
			}
			return A7( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6])
			);
		};
	}


	function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 8 ) {
				crash('a Tuple of length 8', value);
			}
			return A8( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6]),
				d8(value[7])
			);
		};
	}


	// CUSTOM DECODERS

	function decodeValue(value) {
		return value;
	}

	function runDecoderValue(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function customDecoder(decoder, callback) {
		return function(value) {
			var result = callback(decoder(value));
			if (result.ctor === 'Err') {
				throw new Error('custom decoder failed: ' + result._0);
			}
			return result._0;
		}
	}

	function andThen(decode, callback) {
		return function(value) {
			var result = decode(value);
			return callback(result)(value);
		}
	}

	function fail(msg) {
		return function(value) {
			throw new Error(msg);
		}
	}

	function succeed(successValue) {
		return function(value) {
			return successValue;
		}
	}


	// ONE OF MANY

	function oneOf(decoders) {
		return function(value) {
			var errors = [];
			var temp = decoders;
			while (temp.ctor !== '[]') {
				try {
					return temp._0(value);
				} catch(e) {
					errors.push(e.message);
				}
				temp = temp._1;
			}
			throw new Error('expecting one of the following:\n    ' + errors.join('\n    '));
		}
	}

	function get(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}


	// ENCODE / DECODE

	function runDecoderString(decoder, string) {
		try {
			return Result.Ok(decoder(JSON.parse(string)));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function encode(indentLevel, value) {
		return JSON.stringify(value, null, indentLevel);
	}

	function identity(value) {
		return value;
	}

	function encodeObject(keyValuePairs) {
		var obj = {};
		while (keyValuePairs.ctor !== '[]') {
			var pair = keyValuePairs._0;
			obj[pair._0] = pair._1;
			keyValuePairs = keyValuePairs._1;
		}
		return obj;
	}

	return localRuntime.Native.Json.values = {
		encode: F2(encode),
		runDecoderString: F2(runDecoderString),
		runDecoderValue: F2(runDecoderValue),

		get: F2(get),
		oneOf: oneOf,

		decodeNull: decodeNull,
		decodeInt: decodeInt,
		decodeFloat: decodeFloat,
		decodeString: decodeString,
		decodeBool: decodeBool,

		decodeMaybe: decodeMaybe,

		decodeList: decodeList,
		decodeArray: decodeArray,

		decodeField: F2(decodeField),

		decodeObject1: F2(decodeObject1),
		decodeObject2: F3(decodeObject2),
		decodeObject3: F4(decodeObject3),
		decodeObject4: F5(decodeObject4),
		decodeObject5: F6(decodeObject5),
		decodeObject6: F7(decodeObject6),
		decodeObject7: F8(decodeObject7),
		decodeObject8: F9(decodeObject8),
		decodeKeyValuePairs: decodeKeyValuePairs,

		decodeTuple1: F2(decodeTuple1),
		decodeTuple2: F3(decodeTuple2),
		decodeTuple3: F4(decodeTuple3),
		decodeTuple4: F5(decodeTuple4),
		decodeTuple5: F6(decodeTuple5),
		decodeTuple6: F7(decodeTuple6),
		decodeTuple7: F8(decodeTuple7),
		decodeTuple8: F9(decodeTuple8),

		andThen: F2(andThen),
		decodeValue: decodeValue,
		customDecoder: F2(customDecoder),
		fail: fail,
		succeed: succeed,

		identity: identity,
		encodeNull: null,
		encodeArray: ElmArray.toJSArray,
		encodeList: List.toArray,
		encodeObject: encodeObject

	};

};

Elm.Native.List = {};
Elm.Native.List.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.List = localRuntime.Native.List || {};
	if (localRuntime.Native.List.values)
	{
		return localRuntime.Native.List.values;
	}
	if ('values' in Elm.Native.List)
	{
		return localRuntime.Native.List.values = Elm.Native.List.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	var Nil = Utils.Nil;
	var Cons = Utils.Cons;

	function toArray(xs)
	{
		var out = [];
		while (xs.ctor !== '[]')
		{
			out.push(xs._0);
			xs = xs._1;
		}
		return out;
	}

	function fromArray(arr)
	{
		var out = Nil;
		for (var i = arr.length; i--; )
		{
			out = Cons(arr[i], out);
		}
		return out;
	}

	function range(lo,hi)
	{
		var lst = Nil;
		if (lo <= hi)
		{
			do { lst = Cons(hi,lst) } while (hi-->lo);
		}
		return lst
	}

	// f defined similarly for both foldl and foldr (NB: different from Haskell)
	// ie, foldl : (a -> b -> b) -> b -> [a] -> b
	function foldl(f, b, xs)
	{
		var acc = b;
		while (xs.ctor !== '[]')
		{
			acc = A2(f, xs._0, acc);
			xs = xs._1;
		}
		return acc;
	}

	function foldr(f, b, xs)
	{
		var arr = toArray(xs);
		var acc = b;
		for (var i = arr.length; i--; )
		{
			acc = A2(f, arr[i], acc);
		}
		return acc;
	}

	function any(pred, xs)
	{
		while (xs.ctor !== '[]')
		{
			if (pred(xs._0))
			{
				return true;
			}
			xs = xs._1;
		}
		return false;
	}

	function map2(f, xs, ys)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]')
		{
			arr.push(A2(f, xs._0, ys._0));
			xs = xs._1;
			ys = ys._1;
		}
		return fromArray(arr);
	}

	function map3(f, xs, ys, zs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
		{
			arr.push(A3(f, xs._0, ys._0, zs._0));
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map4(f, ws, xs, ys, zs)
	{
		var arr = [];
		while (   ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map5(f, vs, ws, xs, ys, zs)
	{
		var arr = [];
		while (   vs.ctor !== '[]'
			   && ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
			vs = vs._1;
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function sortBy(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a,b){
			return Utils.cmp(f(a), f(b));
		}));
	}

	function sortWith(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a,b){
			var ord = f(a)(b).ctor;
			return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
		}));
	}

	function take(n, xs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && n > 0)
		{
			arr.push(xs._0);
			xs = xs._1;
			--n;
		}
		return fromArray(arr);
	}

	function drop(n, xs)
	{
		while (xs.ctor !== '[]' && n > 0)
		{
			xs = xs._1;
			--n;
		}
		return xs;
	}

	function repeat(n, x)
	{
		var arr = [];
		var pattern = [x];
		while (n > 0)
		{
			if (n & 1)
			{
				arr = arr.concat(pattern);
			}
			n >>= 1, pattern = pattern.concat(pattern);
		}
		return fromArray(arr);
	}


	Elm.Native.List.values = {
		Nil:Nil,
		Cons:Cons,
		cons:F2(Cons),
		toArray:toArray,
		fromArray:fromArray,
		range:range,

		foldl:F3(foldl),
		foldr:F3(foldr),

		any:F2(any),
		map2:F3(map2),
		map3:F4(map3),
		map4:F5(map4),
		map5:F6(map5),
		sortBy:F2(sortBy),
		sortWith:F2(sortWith),
		take:F2(take),
		drop:F2(drop),
		repeat:F2(repeat)
	};
	return localRuntime.Native.List.values = Elm.Native.List.values;

};

Elm.Native.Port = {};
Elm.Native.Port.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Port = localRuntime.Native.Port || {};
	if (localRuntime.Native.Port.values)
	{
		return localRuntime.Native.Port.values;
	}

	var NS;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// INBOUND

	function inbound(name, type, converter)
	{
		if (!localRuntime.argsTracker[name])
		{
			throw new Error(
				"Port Error:\n" +
				"No argument was given for the port named '" + name + "' with type:\n\n" +
				"    " + type.split('\n').join('\n        ') + "\n\n" +
				"You need to provide an initial value!\n\n" +
				"Find out more about ports here <http://elm-lang.org/learn/Ports.elm>"
			);
		}
		var arg = localRuntime.argsTracker[name];
		arg.used = true;

		return jsToElm(name, type, converter, arg.value);
	}


	function inboundSignal(name, type, converter)
	{
		var initialValue = inbound(name, type, converter);

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		var signal = NS.input('inbound-port-' + name, initialValue);

		function send(jsValue)
		{
			var elmValue = jsToElm(name, type, converter, jsValue);
			setTimeout(function() {
				localRuntime.notify(signal.id, elmValue);
			}, 0);
		}

		localRuntime.ports[name] = { send: send };

		return signal;
	}


	function jsToElm(name, type, converter, value)
	{
		try
		{
			return converter(value);
		}
		catch(e)
		{
			throw new Error(
				"Port Error:\n" +
				"Regarding the port named '" + name + "' with type:\n\n" +
				"    " + type.split('\n').join('\n        ') + "\n\n" +
				"You just sent the value:\n\n" +
				"    " + JSON.stringify(value) + "\n\n" +
				"but it cannot be converted to the necessary type.\n" +
				e.message
			);
		}
	}


	// OUTBOUND

	function outbound(name, converter, elmValue)
	{
		localRuntime.ports[name] = converter(elmValue);
	}


	function outboundSignal(name, converter, signal)
	{
		var subscribers = [];

		function subscribe(handler)
		{
			subscribers.push(handler);
		}
		function unsubscribe(handler)
		{
			subscribers.pop(subscribers.indexOf(handler));
		}

		function notify(elmValue)
		{
			var jsValue = converter(elmValue);
			var len = subscribers.length;
			for (var i = 0; i < len; ++i)
			{
				subscribers[i](jsValue);
			}
		}

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		NS.output('outbound-port-' + name, notify, signal);

		localRuntime.ports[name] = {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		};

		return signal;
	}


	return localRuntime.Native.Port.values = {
		inbound: inbound,
		outbound: outbound,
		inboundSignal: inboundSignal,
		outboundSignal: outboundSignal
	};
};

Elm.Native.Regex = {};
Elm.Native.Regex.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Regex = localRuntime.Native.Regex || {};
	if (localRuntime.Native.Regex.values)
	{
		return localRuntime.Native.Regex.values;
	}
	if ('values' in Elm.Native.Regex)
	{
		return localRuntime.Native.Regex.values = Elm.Native.Regex.values;
	}

	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);

	function escape(str)
	{
		return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
	}
	function caseInsensitive(re)
	{
		return new RegExp(re.source, 'gi');
	}
	function regex(raw)
	{
		return new RegExp(raw, 'g');
	}

	function contains(re, string)
	{
		return string.match(re) !== null;
	}

	function find(n, re, str)
	{
		n = n.ctor === "All" ? Infinity : n._0;
		var out = [];
		var number = 0;
		var string = str;
		var lastIndex = re.lastIndex;
		var prevLastIndex = -1;
		var result;
		while (number++ < n && (result = re.exec(string)))
		{
			if (prevLastIndex === re.lastIndex) break;
			var i = result.length - 1;
			var subs = new Array(i);
			while (i > 0)
			{
				var submatch = result[i];
				subs[--i] = submatch === undefined
					? Maybe.Nothing
					: Maybe.Just(submatch);
			}
			out.push({
				_:{},
				match: result[0],
				submatches: List.fromArray(subs),
				index: result.index,
				number: number
			});
			prevLastIndex = re.lastIndex;
		}
		re.lastIndex = lastIndex;
		return List.fromArray(out);
	}

	function replace(n, re, replacer, string)
	{
		n = n.ctor === "All" ? Infinity : n._0;
		var count = 0;
		function jsReplacer(match)
		{
			if (count++ > n)
			{
				return match;
			}
			var i = arguments.length-3;
			var submatches = new Array(i);
			while (i > 0)
			{
				var submatch = arguments[i];
				submatches[--i] = submatch === undefined
					? Maybe.Nothing
					: Maybe.Just(submatch);
			}
			return replacer({
				_:{},
				match:match,
				submatches:List.fromArray(submatches),
				index:arguments[i-1],
				number:count
			});
		}
		return string.replace(re, jsReplacer);
	}

	function split(n, re, str)
	{
		n = n.ctor === "All" ? Infinity : n._0;
		if (n === Infinity)
		{
			return List.fromArray(str.split(re));
		}
		var string = str;
		var result;
		var out = [];
		var start = re.lastIndex;
		while (n--)
		{
			if (!(result = re.exec(string))) break;
			out.push(string.slice(start, result.index));
			start = re.lastIndex;
		}
		out.push(string.slice(start));
		return List.fromArray(out);
	}

	return Elm.Native.Regex.values = {
		regex: regex,
		caseInsensitive: caseInsensitive,
		escape: escape,

		contains: F2(contains),
		find: F3(find),
		replace: F4(replace),
		split: F3(split)
	};
};


if (!Elm.fullscreen) {

	(function() {
		'use strict';

		var Display = {
			FULLSCREEN: 0,
			COMPONENT: 1,
			NONE: 2
		};

		Elm.fullscreen = function(module, args)
		{
			var container = document.createElement('div');
			document.body.appendChild(container);
			return init(Display.FULLSCREEN, container, module, args || {});
		};

		Elm.embed = function(module, container, args)
		{
			var tag = container.tagName;
			if (tag !== 'DIV')
			{
				throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
			}
			return init(Display.COMPONENT, container, module, args || {});
		};

		Elm.worker = function(module, args)
		{
			return init(Display.NONE, {}, module, args || {});
		};

		function init(display, container, module, args, moduleToReplace)
		{
			// defining state needed for an instance of the Elm RTS
			var inputs = [];

			/* OFFSET
			 * Elm's time traveling debugger lets you pause time. This means
			 * "now" may be shifted a bit into the past. By wrapping Date.now()
			 * we can manage this.
			 */
			var timer = {
				programStart: Date.now(),
				now: function()
				{
					return Date.now();
				}
			};

			var updateInProgress = false;
			function notify(id, v)
			{
				if (updateInProgress)
				{
					throw new Error(
						'The notify function has been called synchronously!\n' +
						'This can lead to frames being dropped.\n' +
						'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
				}
				updateInProgress = true;
				var timestep = timer.now();
				for (var i = inputs.length; i--; )
				{
					inputs[i].notify(timestep, id, v);
				}
				updateInProgress = false;
			}
			function setTimeout(func, delay)
			{
				return window.setTimeout(func, delay);
			}

			var listeners = [];
			function addListener(relevantInputs, domNode, eventName, func)
			{
				domNode.addEventListener(eventName, func);
				var listener = {
					relevantInputs: relevantInputs,
					domNode: domNode,
					eventName: eventName,
					func: func
				};
				listeners.push(listener);
			}

			var argsTracker = {};
			for (var name in args)
			{
				argsTracker[name] = {
					value: args[name],
					used: false
				};
			}

			// create the actual RTS. Any impure modules will attach themselves to this
			// object. This permits many Elm programs to be embedded per document.
			var elm = {
				notify: notify,
				setTimeout: setTimeout,
				node: container,
				addListener: addListener,
				inputs: inputs,
				timer: timer,
				argsTracker: argsTracker,
				ports: {},

				isFullscreen: function() { return display === Display.FULLSCREEN; },
				isEmbed: function() { return display === Display.COMPONENT; },
				isWorker: function() { return display === Display.NONE; }
			};

			function swap(newModule)
			{
				removeListeners(listeners);
				var div = document.createElement('div');
				var newElm = init(display, div, newModule, args, elm);
				inputs = [];
				// elm.swap = newElm.swap;
				return newElm;
			}

			function dispose()
			{
				removeListeners(listeners);
				inputs = [];
			}

			var Module = {};
			try
			{
				Module = module.make(elm);
				checkInputs(elm);
			}
			catch (error)
			{
				if (typeof container.appendChild == 'undefined')
				{
					console.log(error.message);
				}
				else
				{
					container.appendChild(errorNode(error.message));
				}
				throw error;
			}

			if (display !== Display.NONE)
			{
				var graphicsNode = initGraphics(elm, Module);
			}

			var rootNode = { kids: inputs };
			trimDeadNodes(rootNode);
			inputs = rootNode.kids;
			filterListeners(inputs, listeners);

			addReceivers(elm.ports);

			if (typeof moduleToReplace !== 'undefined')
			{
				hotSwap(moduleToReplace, elm);

				// rerender scene if graphics are enabled.
				if (typeof graphicsNode !== 'undefined')
				{
					graphicsNode.notify(0, true, 0);
				}
			}

			return {
				swap: swap,
				ports: elm.ports,
				dispose: dispose
			};
		};

		function checkInputs(elm)
		{
			var argsTracker = elm.argsTracker;
			for (var name in argsTracker)
			{
				if (!argsTracker[name].used)
				{
					throw new Error(
						"Port Error:\nYou provided an argument named '" + name +
						"' but there is no corresponding port!\n\n" +
						"Maybe add a port '" + name + "' to your Elm module?\n" +
						"Maybe remove the '" + name + "' argument from your initialization code in JS?"
					);
				}
			}
		}

		function errorNode(message)
		{
			var code = document.createElement('code');

			var lines = message.split('\n');
			code.appendChild(document.createTextNode(lines[0]));
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createElement('br'));
			for (var i = 1; i < lines.length; ++i)
			{
				code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i].replace(/  /g, '\u00A0 ')));
				code.appendChild(document.createElement('br'));
			}
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createTextNode("Open the developer console for more details."));
			return code;
		}


		//// FILTER SIGNALS ////

		// TODO: move this code into the signal module and create a function
		// Signal.initializeGraph that actually instantiates everything.

		function filterListeners(inputs, listeners)
		{
			loop:
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				for (var j = inputs.length; j--; )
				{
					if (listener.relevantInputs.indexOf(inputs[j].id) >= 0)
					{
						continue loop;
					}
				}
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		function removeListeners(listeners)
		{
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		// add receivers for built-in ports if they are defined
		function addReceivers(ports)
		{
			if ('title' in ports)
			{
				if (typeof ports.title === 'string')
				{
					document.title = ports.title;
				}
				else
				{
					ports.title.subscribe(function(v) { document.title = v; });
				}
			}
			if ('redirect' in ports)
			{
				ports.redirect.subscribe(function(v) {
					if (v.length > 0)
					{
						window.location = v;
					}
				});
			}
		}


		// returns a boolean representing whether the node is alive or not.
		function trimDeadNodes(node)
		{
			if (node.isOutput)
			{
				return true;
			}

			var liveKids = [];
			for (var i = node.kids.length; i--; )
			{
				var kid = node.kids[i];
				if (trimDeadNodes(kid))
				{
					liveKids.push(kid);
				}
			}
			node.kids = liveKids;

			return liveKids.length > 0;
		}


		////  RENDERING  ////

		function initGraphics(elm, Module)
		{
			if (!('main' in Module))
			{
				throw new Error("'main' is missing! What do I display?!");
			}

			var signalGraph = Module.main;

			// make sure the signal graph is actually a signal & extract the visual model
			if (!('notify' in signalGraph))
			{
				signalGraph = Elm.Signal.make(elm).constant(signalGraph);
			}
			var initialScene = signalGraph.value;

			// Figure out what the render functions should be
			var render;
			var update;
			if (initialScene.props)
			{
				var Element = Elm.Native.Graphics.Element.make(elm);
				render = Element.render;
				update = Element.updateAndReplace;
			}
			else
			{
				var VirtualDom = Elm.Native.VirtualDom.make(elm);
				render = VirtualDom.render;
				update = VirtualDom.updateAndReplace;
			}

			// Add the initialScene to the DOM
			var container = elm.node;
			var node = render(initialScene);
			while (container.firstChild)
			{
				container.removeChild(container.firstChild);
			}
			container.appendChild(node);

			var _requestAnimationFrame =
				typeof requestAnimationFrame !== 'undefined'
					? requestAnimationFrame
					: function(cb) { setTimeout(cb, 1000/60); }
					;

			// domUpdate is called whenever the main Signal changes.
			//
			// domUpdate and drawCallback implement a small state machine in order
			// to schedule only 1 draw per animation frame. This enforces that
			// once draw has been called, it will not be called again until the
			// next frame.
			//
			// drawCallback is scheduled whenever
			// 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
			// 2. The state transitions from NO_REQUEST to PENDING_REQUEST
			//
			// Invariants:
			// 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
			// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
			//    scheduled drawCallback.
			var NO_REQUEST = 0;
			var PENDING_REQUEST = 1;
			var EXTRA_REQUEST = 2;
			var state = NO_REQUEST;
			var savedScene = initialScene;
			var scheduledScene = initialScene;

			function domUpdate(newScene)
			{
				scheduledScene = newScene;

				switch (state)
				{
					case NO_REQUEST:
						_requestAnimationFrame(drawCallback);
						state = PENDING_REQUEST;
						return;
					case PENDING_REQUEST:
						state = PENDING_REQUEST;
						return;
					case EXTRA_REQUEST:
						state = PENDING_REQUEST;
						return;
				}
			}

			function drawCallback()
			{
				switch (state)
				{
					case NO_REQUEST:
						// This state should not be possible. How can there be no
						// request, yet somehow we are actively fulfilling a
						// request?
						throw new Error(
							"Unexpected draw callback.\n" +
							"Please report this to <https://github.com/elm-lang/core/issues>."
						);

					case PENDING_REQUEST:
						// At this point, we do not *know* that another frame is
						// needed, but we make an extra request to rAF just in
						// case. It's possible to drop a frame if rAF is called
						// too late, so we just do it preemptively.
						_requestAnimationFrame(drawCallback);
						state = EXTRA_REQUEST;

						// There's also stuff we definitely need to draw.
						draw();
						return;

					case EXTRA_REQUEST:
						// Turns out the extra request was not needed, so we will
						// stop calling rAF. No reason to call it all the time if
						// no one needs it.
						state = NO_REQUEST;
						return;
				}
			}

			function draw()
			{
				update(elm.node.firstChild, savedScene, scheduledScene);
				if (elm.Native.Window)
				{
					elm.Native.Window.values.resizeIfNeeded();
				}
				savedScene = scheduledScene;
			}

			var renderer = Elm.Native.Signal.make(elm).output('main', domUpdate, signalGraph);

			// must check for resize after 'renderer' is created so
			// that changes show up.
			if (elm.Native.Window)
			{
				elm.Native.Window.values.resizeIfNeeded();
			}

			return renderer;
		}

		//// HOT SWAPPING ////

		// Returns boolean indicating if the swap was successful.
		// Requires that the two signal graphs have exactly the same
		// structure.
		function hotSwap(from, to)
		{
			function similar(nodeOld,nodeNew)
			{
				if (nodeOld.id !== nodeNew.id)
				{
					return false;
				}
				if (nodeOld.isOutput)
				{
					return nodeNew.isOutput;
				}
				return nodeOld.kids.length === nodeNew.kids.length;
			}
			function swap(nodeOld,nodeNew)
			{
				nodeNew.value = nodeOld.value;
				return true;
			}
			var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
			if (canSwap)
			{
				depthFirstTraversals(swap, from.inputs, to.inputs);
			}
			from.node.parentNode.replaceChild(to.node, from.node);

			return canSwap;
		}

		// Returns false if the node operation f ever fails.
		function depthFirstTraversals(f, queueOld, queueNew)
		{
			if (queueOld.length !== queueNew.length)
			{
				return false;
			}
			queueOld = queueOld.slice(0);
			queueNew = queueNew.slice(0);

			var seen = [];
			while (queueOld.length > 0 && queueNew.length > 0)
			{
				var nodeOld = queueOld.pop();
				var nodeNew = queueNew.pop();
				if (seen.indexOf(nodeOld.id) < 0)
				{
					if (!f(nodeOld, nodeNew))
					{
						return false;
					}
					queueOld = queueOld.concat(nodeOld.kids || []);
					queueNew = queueNew.concat(nodeNew.kids || []);
					seen.push(nodeOld.id);
				}
			}
			return true;
		}
	}());

	function F2(fun)
	{
		function wrapper(a) { return function(b) { return fun(a,b) } }
		wrapper.arity = 2;
		wrapper.func = fun;
		return wrapper;
	}

	function F3(fun)
	{
		function wrapper(a) {
			return function(b) { return function(c) { return fun(a,b,c) }}
		}
		wrapper.arity = 3;
		wrapper.func = fun;
		return wrapper;
	}

	function F4(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return fun(a,b,c,d) }}}
		}
		wrapper.arity = 4;
		wrapper.func = fun;
		return wrapper;
	}

	function F5(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return fun(a,b,c,d,e) }}}}
		}
		wrapper.arity = 5;
		wrapper.func = fun;
		return wrapper;
	}

	function F6(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return fun(a,b,c,d,e,f) }}}}}
		}
		wrapper.arity = 6;
		wrapper.func = fun;
		return wrapper;
	}

	function F7(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return fun(a,b,c,d,e,f,g) }}}}}}
		}
		wrapper.arity = 7;
		wrapper.func = fun;
		return wrapper;
	}

	function F8(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) {
			return fun(a,b,c,d,e,f,g,h)}}}}}}}
		}
		wrapper.arity = 8;
		wrapper.func = fun;
		return wrapper;
	}

	function F9(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) { return function(i) {
			return fun(a,b,c,d,e,f,g,h,i) }}}}}}}}
		}
		wrapper.arity = 9;
		wrapper.func = fun;
		return wrapper;
	}

	function A2(fun,a,b)
	{
		return fun.arity === 2
			? fun.func(a,b)
			: fun(a)(b);
	}
	function A3(fun,a,b,c)
	{
		return fun.arity === 3
			? fun.func(a,b,c)
			: fun(a)(b)(c);
	}
	function A4(fun,a,b,c,d)
	{
		return fun.arity === 4
			? fun.func(a,b,c,d)
			: fun(a)(b)(c)(d);
	}
	function A5(fun,a,b,c,d,e)
	{
		return fun.arity === 5
			? fun.func(a,b,c,d,e)
			: fun(a)(b)(c)(d)(e);
	}
	function A6(fun,a,b,c,d,e,f)
	{
		return fun.arity === 6
			? fun.func(a,b,c,d,e,f)
			: fun(a)(b)(c)(d)(e)(f);
	}
	function A7(fun,a,b,c,d,e,f,g)
	{
		return fun.arity === 7
			? fun.func(a,b,c,d,e,f,g)
			: fun(a)(b)(c)(d)(e)(f)(g);
	}
	function A8(fun,a,b,c,d,e,f,g,h)
	{
		return fun.arity === 8
			? fun.func(a,b,c,d,e,f,g,h)
			: fun(a)(b)(c)(d)(e)(f)(g)(h);
	}
	function A9(fun,a,b,c,d,e,f,g,h,i)
	{
		return fun.arity === 9
			? fun.func(a,b,c,d,e,f,g,h,i)
			: fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	}
}

Elm.Native.Show = {};
Elm.Native.Show.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Show = localRuntime.Native.Show || {};
	if (localRuntime.Native.Show.values)
	{
		return localRuntime.Native.Show.values;
	}

	var _Array;
	var Dict;
	var List;
	var Utils = Elm.Native.Utils.make(localRuntime);

	var toString = function(v)
	{
		var type = typeof v;
		if (type === "function")
		{
			var name = v.func ? v.func.name : v.name;
			return '<function' + (name === '' ? '' : ': ') + name + '>';
		}
		else if (type === "boolean")
		{
			return v ? "True" : "False";
		}
		else if (type === "number")
		{
			return v + "";
		}
		else if ((v instanceof String) && v.isChar)
		{
			return "'" + addSlashes(v, true) + "'";
		}
		else if (type === "string")
		{
			return '"' + addSlashes(v, false) + '"';
		}
		else if (type === "object" && '_' in v && probablyPublic(v))
		{
			var output = [];
			for (var k in v._)
			{
				for (var i = v._[k].length; i--; )
				{
					output.push(k + " = " + toString(v._[k][i]));
				}
			}
			for (var k in v)
			{
				if (k === '_') continue;
				output.push(k + " = " + toString(v[k]));
			}
			if (output.length === 0)
			{
				return "{}";
			}
			return "{ " + output.join(", ") + " }";
		}
		else if (type === "object" && 'ctor' in v)
		{
			if (v.ctor.substring(0,6) === "_Tuple")
			{
				var output = [];
				for (var k in v)
				{
					if (k === 'ctor') continue;
					output.push(toString(v[k]));
				}
				return "(" + output.join(",") + ")";
			}
			else if (v.ctor === "_Array")
			{
				if (!_Array)
				{
					_Array = Elm.Array.make(localRuntime);
				}
				var list = _Array.toList(v);
				return "Array.fromList " + toString(list);
			}
			else if (v.ctor === "::")
			{
				var output = '[' + toString(v._0);
				v = v._1;
				while (v.ctor === "::")
				{
					output += "," + toString(v._0);
					v = v._1;
				}
				return output + ']';
			}
			else if (v.ctor === "[]")
			{
				return "[]";
			}
			else if (v.ctor === "RBNode" || v.ctor === "RBEmpty")
			{
				if (!Dict)
				{
					Dict = Elm.Dict.make(localRuntime);
				}
				if (!List)
				{
					List = Elm.List.make(localRuntime);
				}
				var list = Dict.toList(v);
				var name = "Dict";
				if (list.ctor === "::" && list._0._1.ctor === "_Tuple0")
				{
					name = "Set";
					list = A2(List.map, function(x){return x._0}, list);
				}
				return name + ".fromList " + toString(list);
			}
			else if (v.ctor.slice(0,5) === "Text:")
			{
				return '<text>'
			}
			else
			{
				var output = "";
				for (var i in v)
				{
					if (i === 'ctor') continue;
					var str = toString(v[i]);
					var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
					output += ' ' + (parenless ? str : '(' + str + ')');
				}
				return v.ctor + output;
			}
		}
		if (type === 'object' && 'notify' in v && 'id' in v)
		{
			return '<Signal>';
		}
		return "<internal structure>";
	};

	function addSlashes(str, isChar)
	{
		var s = str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0');
		if (isChar)
		{
			return s.replace(/\'/g, "\\'")
		}
		else
		{
			return s.replace(/\"/g, '\\"');
		}
	}

	function probablyPublic(v)
	{
		var keys = Object.keys(v);
		var len = keys.length;
		if (len === 3
			&& 'props' in v
			&& 'element' in v)
		{
			return false;
		}
		else if (len === 5
			&& 'horizontal' in v
			&& 'vertical' in v
			&& 'x' in v
			&& 'y' in v)
		{
			return false;
		}
		else if (len === 7
			&& 'theta' in v
			&& 'scale' in v
			&& 'x' in v
			&& 'y' in v
			&& 'alpha' in v
			&& 'form' in v)
		{
			return false;
		}
		return true;
	}

	return localRuntime.Native.Show.values = {
		toString: toString
	};
};

Elm.Native.Signal = {};
Elm.Native.Signal.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Signal = localRuntime.Native.Signal || {};
	if (localRuntime.Native.Signal.values)
	{
		return localRuntime.Native.Signal.values;
	}


	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function broadcastToKids(node, timestamp, update)
	{
		var kids = node.kids;
		for (var i = kids.length; i--; )
		{
			kids[i].notify(timestamp, update, node.id);
		}
	}


	// INPUT

	function input(name, base)
	{
		var node = {
			id: Utils.guid(),
			name: 'input-' + name,
			value: base,
			parents: [],
			kids: []
		};

		node.notify = function(timestamp, targetId, value) {
			var update = targetId === node.id;
			if (update)
			{
				node.value = value;
			}
			broadcastToKids(node, timestamp, update);
			return update;
		};

		localRuntime.inputs.push(node);

		return node;
	}

	function constant(value)
	{
		return input('constant', value);
	}


	// MAILBOX

	function mailbox(base)
	{
		var signal = input('mailbox', base);

		function send(value) {
			return Task.asyncFunction(function(callback) {
				localRuntime.setTimeout(function() {
					localRuntime.notify(signal.id, value);
				}, 0);
				callback(Task.succeed(Utils.Tuple0));
			});
		}

		return {
			_: {},
			signal: signal,
			address: {
				ctor: 'Address',
				_0: send
			}
		};
	}

	function sendMessage(message)
	{
		Task.perform(message._0);
	}


	// OUTPUT

	function output(name, handler, parent)
	{
		var node = {
			id: Utils.guid(),
			name: 'output-' + name,
			parents: [parent],
			isOutput: true
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				handler(parent.value);
			}
		};

		parent.kids.push(node);

		return node;
	}


	// MAP

	function mapMany(refreshValue, args)
	{
		var node = {
			id: Utils.guid(),
			name: 'map' + args.length,
			value: refreshValue(),
			parents: args,
			kids: []
		};

		var numberOfParents = args.length;
		var count = 0;
		var update = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			++count;

			update = update || parentUpdate;

			if (count === numberOfParents)
			{
				if (update)
				{
					node.value = refreshValue();
				}
				broadcastToKids(node, timestamp, update);
				update = false;
				count = 0;
			}
		};

		for (var i = numberOfParents; i--; )
		{
			args[i].kids.push(node);
		}

		return node;
	}


	function map(func, a)
	{
		function refreshValue()
		{
			return func(a.value);
		}
		return mapMany(refreshValue, [a]);
	}


	function map2(func, a, b)
	{
		function refreshValue()
		{
			return A2( func, a.value, b.value );
		}
		return mapMany(refreshValue, [a,b]);
	}


	function map3(func, a, b, c)
	{
		function refreshValue()
		{
			return A3( func, a.value, b.value, c.value );
		}
		return mapMany(refreshValue, [a,b,c]);
	}


	function map4(func, a, b, c, d)
	{
		function refreshValue()
		{
			return A4( func, a.value, b.value, c.value, d.value );
		}
		return mapMany(refreshValue, [a,b,c,d]);
	}


	function map5(func, a, b, c, d, e)
	{
		function refreshValue()
		{
			return A5( func, a.value, b.value, c.value, d.value, e.value );
		}
		return mapMany(refreshValue, [a,b,c,d,e]);
	}



	// FOLD

	function foldp(update, state, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'foldp',
			parents: [signal],
			kids: [],
			value: state
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = A2( update, signal.value, node.value );
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	// TIME

	function timestamp(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'timestamp',
			value: Utils.Tuple2(localRuntime.timer.programStart, signal.value),
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = Utils.Tuple2(timestamp, signal.value);
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	function delay(time, signal)
	{
		var delayed = input('delay-input-' + time, signal.value);

		function handler(value)
		{
			setTimeout(function() {
				localRuntime.notify(delayed.id, value);
			}, time);
		}

		output('delay-output-' + time, handler, signal);

		return delayed;
	}


	// MERGING

	function genericMerge(tieBreaker, leftStream, rightStream)
	{
		var node = {
			id: Utils.guid(),
			name: 'merge',
			value: A2(tieBreaker, leftStream.value, rightStream.value),
			parents: [leftStream, rightStream],
			kids: []
		};

		var left = { touched: false, update: false, value: null };
		var right = { touched: false, update: false, value: null };

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === leftStream.id)
			{
				left.touched = true;
				left.update = parentUpdate;
				left.value = leftStream.value;
			}
			if (parentID === rightStream.id)
			{
				right.touched = true;
				right.update = parentUpdate;
				right.value = rightStream.value;
			}

			if (left.touched && right.touched)
			{
				var update = false;
				if (left.update && right.update)
				{
					node.value = A2(tieBreaker, left.value, right.value);
					update = true;
				}
				else if (left.update)
				{
					node.value = left.value;
					update = true;
				}
				else if (right.update)
				{
					node.value = right.value;
					update = true;
				}
				left.touched = false;
				right.touched = false;

				broadcastToKids(node, timestamp, update);
			}
		};

		leftStream.kids.push(node);
		rightStream.kids.push(node);

		return node;
	}


	// FILTERING

	function filterMap(toMaybe, base, signal)
	{
		var maybe = toMaybe(signal.value);
		var node = {
			id: Utils.guid(),
			name: 'filterMap',
			value: maybe.ctor === 'Nothing' ? base : maybe._0,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate)
			{
				var maybe = toMaybe(signal.value);
				if (maybe.ctor === 'Just')
				{
					update = true;
					node.value = maybe._0;
				}
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	// SAMPLING

	function sampleOn(ticker, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'sampleOn',
			value: signal.value,
			parents: [ticker, signal],
			kids: []
		};

		var signalTouch = false;
		var tickerTouch = false;
		var tickerUpdate = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === ticker.id)
			{
				tickerTouch = true;
				tickerUpdate = parentUpdate;
			}
			if (parentID === signal.id)
			{
				signalTouch = true;
			}

			if (tickerTouch && signalTouch)
			{
				if (tickerUpdate)
				{
					node.value = signal.value;
				}
				tickerTouch = false;
				signalTouch = false;

				broadcastToKids(node, timestamp, tickerUpdate);
			}
		};

		ticker.kids.push(node);
		signal.kids.push(node);

		return node;
	}


	// DROP REPEATS

	function dropRepeats(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'dropRepeats',
			value: signal.value,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate && !Utils.eq(node.value, signal.value))
			{
				node.value = signal.value;
				update = true;
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	return localRuntime.Native.Signal.values = {
		input: input,
		constant: constant,
		mailbox: mailbox,
		sendMessage: sendMessage,
		output: output,
		map: F2(map),
		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		foldp: F3(foldp),
		genericMerge: F3(genericMerge),
		filterMap: F3(filterMap),
		sampleOn: F2(sampleOn),
		dropRepeats: dropRepeats,
		timestamp: timestamp,
		delay: F2(delay)
	};
};

Elm.Native.String = {};
Elm.Native.String.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.String = localRuntime.Native.String || {};
	if (localRuntime.Native.String.values)
	{
		return localRuntime.Native.String.values;
	}
	if ('values' in Elm.Native.String)
	{
		return localRuntime.Native.String.values = Elm.Native.String.values;
	}


	var Char = Elm.Char.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function isEmpty(str)
	{
		return str.length === 0;
	}
	function cons(chr,str)
	{
		return chr + str;
	}
	function uncons(str)
	{
		var hd;
		return (hd = str[0])
			? Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)))
			: Maybe.Nothing;
	}
	function append(a,b)
	{
		return a + b;
	}
	function concat(strs)
	{
		return List.toArray(strs).join('');
	}
	function length(str)
	{
		return str.length;
	}
	function map(f,str)
	{
		var out = str.split('');
		for (var i = out.length; i--; )
		{
			out[i] = f(Utils.chr(out[i]));
		}
		return out.join('');
	}
	function filter(pred,str)
	{
		return str.split('').map(Utils.chr).filter(pred).join('');
	}
	function reverse(str)
	{
		return str.split('').reverse().join('');
	}
	function foldl(f,b,str)
	{
		var len = str.length;
		for (var i = 0; i < len; ++i)
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function foldr(f,b,str)
	{
		for (var i = str.length; i--; )
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}

	function split(sep, str)
	{
		return List.fromArray(str.split(sep));
	}
	function join(sep, strs)
	{
		return List.toArray(strs).join(sep);
	}
	function repeat(n, str)
	{
		var result = '';
		while (n > 0)
		{
			if (n & 1)
			{
				result += str;
			}
			n >>= 1, str += str;
		}
		return result;
	}

	function slice(start, end, str)
	{
		return str.slice(start,end);
	}
	function left(n, str)
	{
		return n < 1 ? "" : str.slice(0,n);
	}
	function right(n, str)
	{
		return n < 1 ? "" : str.slice(-n);
	}
	function dropLeft(n, str)
	{
		return n < 1 ? str : str.slice(n);
	}
	function dropRight(n, str)
	{
		return n < 1 ? str : str.slice(0,-n);
	}

	function pad(n,chr,str)
	{
		var half = (n - str.length) / 2;
		return repeat(Math.ceil(half),chr) + str + repeat(half|0,chr);
	}
	function padRight(n,chr,str)
	{
		return str + repeat(n - str.length, chr);
	}
	function padLeft(n,chr,str)
	{
		return repeat(n - str.length, chr) + str;
	}

	function trim(str)
	{
		return str.trim();
	}
	function trimLeft(str)
	{
		return str.trimLeft();
	}
	function trimRight(str)
	{
		return str.trimRight();
	}

	function words(str)
	{
		return List.fromArray(str.trim().split(/\s+/g));
	}
	function lines(str)
	{
		return List.fromArray(str.split(/\r\n|\r|\n/g));
	}

	function toUpper(str)
	{
		return str.toUpperCase();
	}
	function toLower(str)
	{
		return str.toLowerCase();
	}

	function any(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (pred(Utils.chr(str[i])))
			{
				return true;
			}
		}
		return false;
	}
	function all(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (!pred(Utils.chr(str[i])))
			{
				return false;
			}
		}
		return true;
	}

	function contains(sub, str)
	{
		return str.indexOf(sub) > -1;
	}
	function startsWith(sub, str)
	{
		return str.indexOf(sub) === 0;
	}
	function endsWith(sub, str)
	{
		return str.length >= sub.length &&
			str.lastIndexOf(sub) === str.length - sub.length;
	}
	function indexes(sub, str)
	{
		var subLen = sub.length;
		var i = 0;
		var is = [];
		while ((i = str.indexOf(sub, i)) > -1)
		{
			is.push(i);
			i = i + subLen;
		}
		return List.fromArray(is);
	}

	function toInt(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to an Int" );
		}
		var start = 0;
		if (s[0] == '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
			start = 1;
		}
		for (var i = start; i < len; ++i)
		{
			if (!Char.isDigit(s[i]))
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
		}
		return Result.Ok(parseInt(s, 10));
	}

	function toFloat(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		var start = 0;
		if (s[0] == '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to a Float" );
			}
			start = 1;
		}
		var dotCount = 0;
		for (var i = start; i < len; ++i)
		{
			if (Char.isDigit(s[i]))
			{
				continue;
			}
			if (s[i] === '.')
			{
				dotCount += 1;
				if (dotCount <= 1)
				{
					continue;
				}
			}
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		return Result.Ok(parseFloat(s));
	}

	function toList(str)
	{
		return List.fromArray(str.split('').map(Utils.chr));
	}
	function fromList(chars)
	{
		return List.toArray(chars).join('');
	}

	return Elm.Native.String.values = {
		isEmpty: isEmpty,
		cons: F2(cons),
		uncons: uncons,
		append: F2(append),
		concat: concat,
		length: length,
		map: F2(map),
		filter: F2(filter),
		reverse: reverse,
		foldl: F3(foldl),
		foldr: F3(foldr),

		split: F2(split),
		join: F2(join),
		repeat: F2(repeat),

		slice: F3(slice),
		left: F2(left),
		right: F2(right),
		dropLeft: F2(dropLeft),
		dropRight: F2(dropRight),

		pad: F3(pad),
		padLeft: F3(padLeft),
		padRight: F3(padRight),

		trim: trim,
		trimLeft: trimLeft,
		trimRight: trimRight,

		words: words,
		lines: lines,

		toUpper: toUpper,
		toLower: toLower,

		any: F2(any),
		all: F2(all),

		contains: F2(contains),
		startsWith: F2(startsWith),
		endsWith: F2(endsWith),
		indexes: F2(indexes),

		toInt: toInt,
		toFloat: toFloat,
		toList: toList,
		fromList: fromList
	};
};

Elm.Native.Task = {};
Elm.Native.Task.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Task = localRuntime.Native.Task || {};
	if (localRuntime.Native.Task.values)
	{
		return localRuntime.Native.Task.values;
	}

	var Result = Elm.Result.make(localRuntime);
	var Signal;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CONSTRUCTORS

	function succeed(value)
	{
		return {
			tag: 'Succeed',
			value: value
		};
	}

	function fail(error)
	{
		return {
			tag: 'Fail',
			value: error
		};
	}

	function asyncFunction(func)
	{
		return {
			tag: 'Async',
			asyncFunction: func
		};
	}

	function andThen(task, callback)
	{
		return {
			tag: 'AndThen',
			task: task,
			callback: callback
		};
	}

	function catch_(task, callback)
	{
		return {
			tag: 'Catch',
			task: task,
			callback: callback
		};
	}


	// RUNNER

	function perform(task) {
		runTask({ task: task }, function() {});
	}

	function performSignal(name, signal)
	{
		var workQueue = [];

		function onComplete()
		{
			workQueue.shift();

			setTimeout(function() {
				if (workQueue.length > 0)
				{
					runTask(workQueue[0], onComplete);
				}
			}, 0);
		}

		function register(task)
		{
			var root = { task: task };
			workQueue.push(root);
			if (workQueue.length === 1)
			{
				runTask(root, onComplete);
			}
		}

		if (!Signal)
		{
			Signal = Elm.Native.Signal.make(localRuntime);
		}
		Signal.output('perform-tasks-' + name, register, signal);

		register(signal.value);

		return signal;
	}

	function mark(status, task)
	{
		return { status: status, task: task };
	}

	function runTask(root, onComplete)
	{
		var result = mark('runnable', root.task);
		while (result.status === 'runnable')
		{
			result = stepTask(onComplete, root, result.task);
		}

		if (result.status === 'done')
		{
			root.task = result.task;
			onComplete();
		}

		if (result.status === 'blocked')
		{
			root.task = result.task;
		}
	}

	function stepTask(onComplete, root, task)
	{
		var tag = task.tag;

		if (tag === 'Succeed' || tag === 'Fail')
		{
			return mark('done', task);
		}

		if (tag === 'Async')
		{
			var placeHolder = {};
			var couldBeSync = true;
			var wasSync = false;

			task.asyncFunction(function(result) {
				placeHolder.tag = result.tag;
				placeHolder.value = result.value;
				if (couldBeSync)
				{
					wasSync = true;
				}
				else
				{
					runTask(root, onComplete);
				}
			});
			couldBeSync = false;
			return mark(wasSync ? 'done' : 'blocked', placeHolder);
		}

		if (tag === 'AndThen' || tag === 'Catch')
		{
			var result = mark('runnable', task.task);
			while (result.status === 'runnable')
			{
				result = stepTask(onComplete, root, result.task);
			}

			if (result.status === 'done')
			{
				var activeTask = result.task;
				var activeTag = activeTask.tag;

				var succeedChain = activeTag === 'Succeed' && tag === 'AndThen';
				var failChain = activeTag === 'Fail' && tag === 'Catch';

				return (succeedChain || failChain)
					? mark('runnable', task.callback(activeTask.value))
					: mark('runnable', activeTask);
			}
			if (result.status === 'blocked')
			{
				return mark('blocked', {
					tag: tag,
					task: result.task,
					callback: task.callback
				});
			}
		}
	}


	// THREADS

	function sleep(time) {
		return asyncFunction(function(callback) {
			setTimeout(function() {
				callback(succeed(Utils.Tuple0));
			}, time);
		});
	}

	function spawn(task) {
		return asyncFunction(function(callback) {
			var id = setTimeout(function() {
				perform(task);
			}, 0);
			callback(succeed(id));
		});
	}


	return localRuntime.Native.Task.values = {
		succeed: succeed,
		fail: fail,
		asyncFunction: asyncFunction,
		andThen: F2(andThen),
		catch_: F2(catch_),
		perform: perform,
		performSignal: performSignal,
		spawn: spawn,
		sleep: sleep
	};
};

Elm.Native.Text = {};
Elm.Native.Text.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Text = localRuntime.Native.Text || {};
	if (localRuntime.Native.Text.values)
	{
		return localRuntime.Native.Text.values;
	}

	var toCss = Elm.Native.Color.make(localRuntime).toCss;
	var List = Elm.Native.List.make(localRuntime);


	// CONSTRUCTORS

	function fromString(str)
	{
		return {
			ctor: 'Text:Text',
			_0: str
		};
	}

	function append(a, b)
	{
		return {
			ctor: 'Text:Append',
			_0: a,
			_1: b
		};
	}

	function addMeta(field, value, text)
	{
		var newProps = {};
		var newText = {
			ctor: 'Text:Meta',
			_0: newProps,
			_1: text
		};

		if (text.ctor === 'Text:Meta')
		{
			newText._1 = text._1;
			var props = text._0;
			for (var i = metaKeys.length; i--; )
			{
				var key = metaKeys[i];
				var val = props[key];
				if (val)
				{
					newProps[key] = val;
				}
			}
		}
		newProps[field] = value;
		return newText;
	}

	var metaKeys = [
		'font-size',
		'font-family',
		'font-style',
		'font-weight',
		'href',
		'text-decoration',
		'color'
	];


	// conversions from Elm values to CSS

	function toTypefaces(list)
	{
		var typefaces = List.toArray(list);
		for (var i = typefaces.length; i--; )
		{
			var typeface = typefaces[i];
			if (typeface.indexOf(' ') > -1)
			{
				typefaces[i] = "'" + typeface + "'";
			}
		}
		return typefaces.join(',');
	}

	function toLine(line)
	{
		var ctor = line.ctor;
		return ctor === 'Under'
			? 'underline'
			: ctor === 'Over'
				? 'overline'
				: 'line-through';
	}

	// setting styles of Text

	function style(style, text)
	{
		var newText = addMeta('color', toCss(style.color), text);
		var props = newText._0;

		if (style.typeface.ctor !== '[]')
		{
			props['font-family'] = toTypefaces(style.typeface);
		}
		if (style.height.ctor !== "Nothing")
		{
			props['font-size'] = style.height._0 + 'px';
		}
		if (style.bold)
		{
			props['font-weight'] = 'bold';
		}
		if (style.italic)
		{
			props['font-style'] = 'italic';
		}
		if (style.line.ctor !== 'Nothing')
		{
			props['text-decoration'] = toLine(style.line._0);
		}
		return newText;
	}

	function height(px, text)
	{
		return addMeta('font-size', px + 'px', text);
	}

	function typeface(names, text)
	{
		return addMeta('font-family', toTypefaces(names), text);
	}

	function monospace(text)
	{
		return addMeta('font-family', 'monospace', text);
	}

	function italic(text)
	{
		return addMeta('font-style', 'italic', text);
	}

	function bold(text)
	{
		return addMeta('font-weight', 'bold', text);
	}

	function link(href, text)
	{
		return addMeta('href', href, text);
	}

	function line(line, text)
	{
		return addMeta('text-decoration', toLine(line), text);
	}

	function color(color, text)
	{
		return addMeta('color', toCss(color), text);;
	}


	// RENDER

	function renderHtml(text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			return renderHtml(text._0) + renderHtml(text._1);
		}
		if (tag === 'Text:Text')
		{
			return properEscape(text._0);
		}
		if (tag === 'Text:Meta')
		{
			return renderMeta(text._0, renderHtml(text._1));
		}
	}

	function renderMeta(metas, string)
	{
		var href = metas['href'];
		if (href)
		{
			string = '<a href="' + href + '">' + string + '</a>';
		}
		var styles = '';
		for (var key in metas)
		{
			if (key === 'href')
			{
				continue;
			}
			styles += key + ':' + metas[key] + ';';
		}
		if (styles)
		{
			string = '<span style="' + styles + '">' + string + '</span>';
		}
		return string;
	}

	function properEscape(str)
	{
		if (str.length == 0)
		{
			return str;
		}
		str = str //.replace(/&/g,  "&#38;")
			.replace(/"/g,  '&#34;')
			.replace(/'/g,  "&#39;")
			.replace(/</g,  "&#60;")
			.replace(/>/g,  "&#62;");
		var arr = str.split('\n');
		for (var i = arr.length; i--; )
		{
			arr[i] = makeSpaces(arr[i]);
		}
		return arr.join('<br/>');
	}

	function makeSpaces(s)
	{
		if (s.length == 0)
		{
			return s;
		}
		var arr = s.split('');
		if (arr[0] == ' ')
		{
			arr[0] = "&nbsp;"
		}
		for (var i = arr.length; --i; )
		{
			if (arr[i][0] == ' ' && arr[i-1] == ' ')
			{
				arr[i-1] = arr[i-1] + arr[i];
				arr[i] = '';
			}
		}
		for (var i = arr.length; i--; )
		{
			if (arr[i].length > 1 && arr[i][0] == ' ')
			{
				var spaces = arr[i].split('');
				for (var j = spaces.length - 2; j >= 0; j -= 2)
				{
					spaces[j] = '&nbsp;';
				}
				arr[i] = spaces.join('');
			}
		}
		arr = arr.join('');
		if (arr[arr.length-1] === " ")
		{
			return arr.slice(0,-1) + '&nbsp;';
		}
		return arr;
	}


	return localRuntime.Native.Text.values = {
		fromString: fromString,
		append: F2(append),

		height: F2(height),
		italic: italic,
		bold: bold,
		line: F2(line),
		monospace: monospace,
		typeface: F2(typeface),
		color: F2(color),
		link: F2(link),
		style: F2(style),

		toTypefaces: toTypefaces,
		toLine: toLine,
		renderHtml: renderHtml
	};
};

Elm.Native.Time = {};
Elm.Native.Time.make = function(localRuntime)
{

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Time = localRuntime.Native.Time || {};
	if (localRuntime.Native.Time.values)
	{
		return localRuntime.Native.Time.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);


	// FRAMES PER SECOND

	function fpsWhen(desiredFPS, isOn)
	{
		var msPerFrame = 1000 / desiredFPS;
		var ticker = NS.input('fps-' + desiredFPS, null);

		function notifyTicker()
		{
			localRuntime.notify(ticker.id, null);
		}

		function firstArg(x, y)
		{
			return x;
		}

		// input fires either when isOn changes, or when ticker fires.
		// Its value is a tuple with the current timestamp, and the state of isOn
		var input = NS.timestamp(A3(NS.map2, F2(firstArg), NS.dropRepeats(isOn), ticker));

		var initialState = {
			isOn: false,
			time: localRuntime.timer.programStart,
			delta: 0
		};

		var timeoutId;

		function update(input,state)
		{
			var currentTime = input._0;
			var isOn = input._1;
			var wasOn = state.isOn;
			var previousTime = state.time;

			if (isOn)
			{
				timeoutId = localRuntime.setTimeout(notifyTicker, msPerFrame);
			}
			else if (wasOn)
			{
				clearTimeout(timeoutId);
			}

			return {
				isOn: isOn,
				time: currentTime,
				delta: (isOn && !wasOn) ? 0 : currentTime - previousTime
			};
		}

		return A2(
			NS.map,
			function(state) { return state.delta; },
			A3(NS.foldp, F2(update), update(input.value,initialState), input)
		);
	}


	// EVERY

	function every(t)
	{
		var ticker = NS.input('every-' + t, null);
		function tellTime()
		{
			localRuntime.notify(ticker.id, null);
		}
		var clock = A2( NS.map, fst, NS.timestamp(ticker) );
		setInterval(tellTime, t);
		return clock;
	}


	function fst(pair)
	{
		return pair._0;
	}


	function read(s)
	{
		var t = Date.parse(s);
		return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
	}

	return localRuntime.Native.Time.values = {
		fpsWhen: F2(fpsWhen),
		every: every,
		toDate: function(t) { return new window.Date(t); },
		read: read
	};

};

Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Transform2D = localRuntime.Native.Transform2D || {};
	if (localRuntime.Native.Transform2D.values)
	{
		return localRuntime.Native.Transform2D.values;
	}

	var A;
	if (typeof Float32Array === 'undefined')
	{
		A = function(arr)
		{
			this.length = arr.length;
			this[0] = arr[0];
			this[1] = arr[1];
			this[2] = arr[2];
			this[3] = arr[3];
			this[4] = arr[4];
			this[5] = arr[5];
		};
	}
	else
	{
		A = Float32Array;
	}

	// layout of matrix in an array is
	//
	//   | m11 m12 dx |
	//   | m21 m22 dy |
	//   |  0   0   1 |
	//
	//  new A([ m11, m12, dx, m21, m22, dy ])

	var identity = new A([1,0,0,0,1,0]);
	function matrix(m11, m12, m21, m22, dx, dy)
	{
		return new A([m11, m12, dx, m21, m22, dy]);
	}

	function rotation(t)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		return new A([c, -s, 0, s, c, 0]);
	}

	function rotate(t,m)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11*c + m12*s, -m11*s + m12*c, m[2],
					  m21*c + m22*s, -m21*s + m22*c, m[5]]);
	}
	/*
	function move(xy,m) {
		var x = xy._0;
		var y = xy._1;
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11, m12, m11*x + m12*y + m[2],
					  m21, m22, m21*x + m22*y + m[5]]);
	}
	function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
	function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
	function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
	function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
	function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

	function transform(m11, m21, m12, m22, mdx, mdy, n) {
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}
	*/
	function multiply(m, n)
	{
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}

	return localRuntime.Native.Transform2D.values = {
		identity:identity,
		matrix:F6(matrix),
		rotation:rotation,
		multiply:F2(multiply)
		/*
		transform:F7(transform),
		rotate:F2(rotate),
		move:F2(move),
		scale:F2(scale),
		scaleX:F2(scaleX),
		scaleY:F2(scaleY),
		reflectX:reflectX,
		reflectY:reflectY
		*/
	};

};

Elm.Native = Elm.Native || {};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Utils = localRuntime.Native.Utils || {};
	if (localRuntime.Native.Utils.values)
	{
		return localRuntime.Native.Utils.values;
	}

	function eq(l,r)
	{
		var stack = [{'x': l, 'y': r}]
		while (stack.length > 0)
		{
			var front = stack.pop();
			var x = front.x;
			var y = front.y;
			if (x === y)
			{
				continue;
			}
			if (typeof x === "object")
			{
				var c = 0;
				for (var i in x)
				{
					++c;
					if (i in y)
					{
						if (i !== 'ctor')
						{
							stack.push({ 'x': x[i], 'y': y[i] });
						}
					}
					else
					{
						return false;
					}
				}
				if ('ctor' in x)
				{
					stack.push({'x': x.ctor, 'y': y.ctor});
				}
				if (c !== Object.keys(y).length)
				{
					return false;
				}
			}
			else if (typeof x === 'function')
			{
				throw new Error('Equality error: general function equality is ' +
								'undecidable, and therefore, unsupported');
			}
			else
			{
				return false;
			}
		}
		return true;
	}

	// code in Generate/JavaScript.hs depends on the particular
	// integer values assigned to LT, EQ, and GT
	var LT = -1, EQ = 0, GT = 1, ord = ['LT','EQ','GT'];

	function compare(x,y)
	{
		return {
			ctor: ord[cmp(x,y)+1]
		};
	}

	function cmp(x,y) {
		var ord;
		if (typeof x !== 'object')
		{
			return x === y ? EQ : x < y ? LT : GT;
		}
		else if (x.isChar)
		{
			var a = x.toString();
			var b = y.toString();
			return a === b
				? EQ
				: a < b
					? LT
					: GT;
		}
		else if (x.ctor === "::" || x.ctor === "[]")
		{
			while (true)
			{
				if (x.ctor === "[]" && y.ctor === "[]")
				{
					return EQ;
				}
				if (x.ctor !== y.ctor)
				{
					return x.ctor === '[]' ? LT : GT;
				}
				ord = cmp(x._0, y._0);
				if (ord !== EQ)
				{
					return ord;
				}
				x = x._1;
				y = y._1;
			}
		}
		else if (x.ctor.slice(0,6) === '_Tuple')
		{
			var n = x.ctor.slice(6) - 0;
			var err = 'cannot compare tuples with more than 6 elements.';
			if (n === 0) return EQ;
			if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
			if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
			if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
			if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
			if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
			if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
			if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
			return EQ;
		}
		else
		{
			throw new Error('Comparison error: comparison is only defined on ints, ' +
							'floats, times, chars, strings, lists of comparable values, ' +
							'and tuples of comparable values.');
		}
	}


	var Tuple0 = {
		ctor: "_Tuple0"
	};

	function Tuple2(x,y)
	{
		return {
			ctor: "_Tuple2",
			_0: x,
			_1: y
		};
	}

	function chr(c)
	{
		var x = new String(c);
		x.isChar = true;
		return x;
	}

	function txt(str)
	{
		var t = new String(str);
		t.text = true;
		return t;
	}

	var count = 0;
	function guid(_)
	{
		return count++
	}

	function copy(oldRecord)
	{
		var newRecord = {};
		for (var key in oldRecord)
		{
			var value = key === '_'
				? copy(oldRecord._)
				: oldRecord[key];
			newRecord[key] = value;
		}
		return newRecord;
	}

	function remove(key, oldRecord)
	{
		var record = copy(oldRecord);
		if (key in record._)
		{
			record[key] = record._[key][0];
			record._[key] = record._[key].slice(1);
			if (record._[key].length === 0)
			{
				delete record._[key];
			}
		}
		else
		{
			delete record[key];
		}
		return record;
	}

	function replace(keyValuePairs, oldRecord)
	{
		var record = copy(oldRecord);
		for (var i = keyValuePairs.length; i--; )
		{
			var pair = keyValuePairs[i];
			record[pair[0]] = pair[1];
		}
		return record;
	}

	function insert(key, value, oldRecord)
	{
		var newRecord = copy(oldRecord);
		if (key in newRecord)
		{
			var values = newRecord._[key];
			var copiedValues = values ? values.slice(0) : [];
			newRecord._[key] = [newRecord[key]].concat(copiedValues);
		}
		newRecord[key] = value;
		return newRecord;
	}

	function getXY(e)
	{
		var posx = 0;
		var posy = 0;
		if (e.pageX || e.pageY)
		{
			posx = e.pageX;
			posy = e.pageY;
		}
		else if (e.clientX || e.clientY)
		{
			posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
			posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
		}

		if (localRuntime.isEmbed())
		{
			var rect = localRuntime.node.getBoundingClientRect();
			var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
			var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
			// TODO: figure out if there is a way to avoid rounding here
			posx = posx - Math.round(relx) - localRuntime.node.clientLeft;
			posy = posy - Math.round(rely) - localRuntime.node.clientTop;
		}
		return Tuple2(posx, posy);
	}


	//// LIST STUFF ////

	var Nil = { ctor:'[]' };

	function Cons(hd,tl)
	{
		return {
			ctor: "::",
			_0: hd,
			_1: tl
		};
	}

	function append(xs,ys)
	{
		// append Strings
		if (typeof xs === "string")
		{
			return xs + ys;
		}

		// append Text
		if (xs.ctor.slice(0,5) === 'Text:')
		{
			return {
				ctor: 'Text:Append',
				_0: xs,
				_1: ys
			};
		}



		// append Lists
		if (xs.ctor === '[]')
		{
			return ys;
		}
		var root = Cons(xs._0, Nil);
		var curr = root;
		xs = xs._1;
		while (xs.ctor !== '[]')
		{
			curr._1 = Cons(xs._0, Nil);
			xs = xs._1;
			curr = curr._1;
		}
		curr._1 = ys;
		return root;
	}

	//// RUNTIME ERRORS ////

	function indent(lines)
	{
		return '\n' + lines.join('\n');
	}

	function badCase(moduleName, span)
	{
		var msg = indent([
			'Non-exhaustive pattern match in case-expression.',
			'Make sure your patterns cover every case!'
		]);
		throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
	}

	function badIf(moduleName, span)
	{
		var msg = indent([
			'Non-exhaustive pattern match in multi-way-if expression.',
			'It is best to use \'otherwise\' as the last branch of multi-way-if.'
		]);
		throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
	}


	function badPort(expected, received)
	{
		var msg = indent([
			'Expecting ' + expected + ' but was given ',
			JSON.stringify(received)
		]);
		throw new Error('Runtime error when sending values through a port.' + msg);
	}


	return localRuntime.Native.Utils.values = {
		eq: eq,
		cmp: cmp,
		compare: F2(compare),
		Tuple0: Tuple0,
		Tuple2: Tuple2,
		chr: chr,
		txt: txt,
		copy: copy,
		remove: remove,
		replace: replace,
		insert: insert,
		guid: guid,
		getXY: getXY,

		Nil: Nil,
		Cons: Cons,
		append: F2(append),

		badCase: badCase,
		badIf: badIf,
		badPort: badPort
	};
};

(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
module.exports = createHash

function createHash(elem) {
    var attributes = elem.attributes
    var hash = {}

    if (attributes === null || attributes === undefined) {
        return hash
    }

    for (var i = 0; i < attributes.length; i++) {
        var attr = attributes[i]

        if (attr.name.substr(0,5) !== "data-") {
            continue
        }

        hash[attr.name.substr(5)] = attr.value
    }

    return hash
}

},{}],2:[function(require,module,exports){
var createStore = require("weakmap-shim/create-store")
var Individual = require("individual")

var createHash = require("./create-hash.js")

var hashStore = Individual("__DATA_SET_WEAKMAP@3", createStore())

module.exports = DataSet

function DataSet(elem) {
    var store = hashStore(elem)

    if (!store.hash) {
        store.hash = createHash(elem)
    }

    return store.hash
}

},{"./create-hash.js":1,"individual":3,"weakmap-shim/create-store":4}],3:[function(require,module,exports){
(function (global){
var root = typeof window !== 'undefined' ?
    window : typeof global !== 'undefined' ?
    global : {};

module.exports = Individual

function Individual(key, value) {
    if (root[key]) {
        return root[key]
    }

    Object.defineProperty(root, key, {
        value: value
        , configurable: true
    })

    return value
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],4:[function(require,module,exports){
var hiddenStore = require('./hidden-store.js');

module.exports = createStore;

function createStore() {
    var key = {};

    return function (obj) {
        if (typeof obj !== 'object' || obj === null) {
            throw new Error('Weakmap-shim: Key must be object')
        }

        var store = obj.valueOf(key);
        return store && store.identity === key ?
            store : hiddenStore(obj, key);
    };
}

},{"./hidden-store.js":5}],5:[function(require,module,exports){
module.exports = hiddenStore;

function hiddenStore(obj, key) {
    var store = { identity: key };
    var valueOf = obj.valueOf;

    Object.defineProperty(obj, "valueOf", {
        value: function (value) {
            return value !== key ?
                valueOf.apply(this, arguments) : store;
        },
        writable: true
    });

    return store;
}

},{}],6:[function(require,module,exports){
var DataSet = require("data-set")

module.exports = addEvent

function addEvent(target, type, handler) {
    var ds = DataSet(target)
    var events = ds[type]

    if (!events) {
        ds[type] = handler
    } else if (Array.isArray(events)) {
        if (events.indexOf(handler) === -1) {
            events.push(handler)
        }
    } else if (events !== handler) {
        ds[type] = [events, handler]
    }
}

},{"data-set":2}],7:[function(require,module,exports){
var globalDocument = require("global/document")
var DataSet = require("data-set")
var createStore = require("weakmap-shim/create-store")

var addEvent = require("./add-event.js")
var removeEvent = require("./remove-event.js")
var ProxyEvent = require("./proxy-event.js")

var HANDLER_STORE = createStore()

module.exports = DOMDelegator

function DOMDelegator(document) {
    document = document || globalDocument

    this.target = document.documentElement
    this.events = {}
    this.rawEventListeners = {}
    this.globalListeners = {}
}

DOMDelegator.prototype.addEventListener = addEvent
DOMDelegator.prototype.removeEventListener = removeEvent

DOMDelegator.prototype.allocateHandle =
    function allocateHandle(func) {
        var handle = new Handle()

        HANDLER_STORE(handle).func = func;

        return handle
    }

DOMDelegator.prototype.transformHandle =
    function transformHandle(handle, lambda) {
        var func = HANDLER_STORE(handle).func

        return this.allocateHandle(function (ev) {
            var result = lambda(ev)
            if (result) {
                func(result)
            }
        })
    }

DOMDelegator.prototype.addGlobalEventListener =
    function addGlobalEventListener(eventName, fn) {
        var listeners = this.globalListeners[eventName] || [];
        if (listeners.indexOf(fn) === -1) {
            listeners.push(fn)
        }

        this.globalListeners[eventName] = listeners;
    }

DOMDelegator.prototype.removeGlobalEventListener =
    function removeGlobalEventListener(eventName, fn) {
        var listeners = this.globalListeners[eventName] || [];

        var index = listeners.indexOf(fn)
        if (index !== -1) {
            listeners.splice(index, 1)
        }
    }

DOMDelegator.prototype.listenTo = function listenTo(eventName) {
    if (this.events[eventName]) {
        return
    }

    this.events[eventName] = true

    var listener = this.rawEventListeners[eventName]
    if (!listener) {
        listener = this.rawEventListeners[eventName] =
            createHandler(eventName, this)
    }

    this.target.addEventListener(eventName, listener, true)
}

DOMDelegator.prototype.unlistenTo = function unlistenTo(eventName) {
    if (!this.events[eventName]) {
        return
    }

    this.events[eventName] = false
    var listener = this.rawEventListeners[eventName]

    if (!listener) {
        throw new Error("dom-delegator#unlistenTo: cannot " +
            "unlisten to " + eventName)
    }

    this.target.removeEventListener(eventName, listener, true)
}

function createHandler(eventName, delegator) {
    var globalListeners = delegator.globalListeners;
    var delegatorTarget = delegator.target;

    return handler

    function handler(ev) {
        var globalHandlers = globalListeners[eventName] || []

        if (globalHandlers.length > 0) {
            var globalEvent = new ProxyEvent(ev);
            globalEvent.currentTarget = delegatorTarget;
            callListeners(globalHandlers, globalEvent)
        }

        findAndInvokeListeners(ev.target, ev, eventName)
    }
}

function findAndInvokeListeners(elem, ev, eventName) {
    var listener = getListener(elem, eventName)

    if (listener && listener.handlers.length > 0) {
        var listenerEvent = new ProxyEvent(ev);
        listenerEvent.currentTarget = listener.currentTarget
        callListeners(listener.handlers, listenerEvent)

        if (listenerEvent._bubbles) {
            var nextTarget = listener.currentTarget.parentNode
            findAndInvokeListeners(nextTarget, ev, eventName)
        }
    }
}

function getListener(target, type) {
    // terminate recursion if parent is `null`
    if (target === null) {
        return null
    }

    var ds = DataSet(target)
    // fetch list of handler fns for this event
    var handler = ds[type]
    var allHandler = ds.event

    if (!handler && !allHandler) {
        return getListener(target.parentNode, type)
    }

    var handlers = [].concat(handler || [], allHandler || [])
    return new Listener(target, handlers)
}

function callListeners(handlers, ev) {
    handlers.forEach(function (handler) {
        if (typeof handler === "function") {
            handler(ev)
        } else if (typeof handler.handleEvent === "function") {
            handler.handleEvent(ev)
        } else if (handler.type === "dom-delegator-handle") {
            HANDLER_STORE(handler).func(ev)
        } else {
            throw new Error("dom-delegator: unknown handler " +
                "found: " + JSON.stringify(handlers));
        }
    })
}

function Listener(target, handlers) {
    this.currentTarget = target
    this.handlers = handlers
}

function Handle() {
    this.type = "dom-delegator-handle"
}

},{"./add-event.js":6,"./proxy-event.js":15,"./remove-event.js":16,"data-set":2,"global/document":10,"weakmap-shim/create-store":13}],8:[function(require,module,exports){
var Individual = require("individual")
var cuid = require("cuid")
var globalDocument = require("global/document")

var DOMDelegator = require("./dom-delegator.js")

var delegatorCache = Individual("__DOM_DELEGATOR_CACHE@9", {
    delegators: {}
})
var commonEvents = [
    "blur", "change", "click",  "contextmenu", "dblclick",
    "error","focus", "focusin", "focusout", "input", "keydown",
    "keypress", "keyup", "load", "mousedown", "mouseup",
    "resize", "scroll", "select", "submit", "touchcancel",
    "touchend", "touchstart", "unload"
]

/*  Delegator is a thin wrapper around a singleton `DOMDelegator`
        instance.

    Only one DOMDelegator should exist because we do not want
        duplicate event listeners bound to the DOM.

    `Delegator` will also `listenTo()` all events unless 
        every caller opts out of it
*/
module.exports = Delegator

function Delegator(opts) {
    opts = opts || {}
    var document = opts.document || globalDocument

    var cacheKey = document["__DOM_DELEGATOR_CACHE_TOKEN@9"]

    if (!cacheKey) {
        cacheKey =
            document["__DOM_DELEGATOR_CACHE_TOKEN@9"] = cuid()
    }

    var delegator = delegatorCache.delegators[cacheKey]

    if (!delegator) {
        delegator = delegatorCache.delegators[cacheKey] =
            new DOMDelegator(document)
    }

    if (opts.defaultEvents !== false) {
        for (var i = 0; i < commonEvents.length; i++) {
            delegator.listenTo(commonEvents[i])
        }
    }

    return delegator
}



},{"./dom-delegator.js":7,"cuid":9,"global/document":10,"individual":11}],9:[function(require,module,exports){
/**
 * cuid.js
 * Collision-resistant UID generator for browsers and node.
 * Sequential for fast db lookups and recency sorting.
 * Safe for element IDs and server-side lookups.
 *
 * Extracted from CLCTR
 * 
 * Copyright (c) Eric Elliott 2012
 * MIT License
 */

/*global window, navigator, document, require, process, module */
(function (app) {
  'use strict';
  var namespace = 'cuid',
    c = 0,
    blockSize = 4,
    base = 36,
    discreteValues = Math.pow(base, blockSize),

    pad = function pad(num, size) {
      var s = "000000000" + num;
      return s.substr(s.length-size);
    },

    randomBlock = function randomBlock() {
      return pad((Math.random() *
            discreteValues << 0)
            .toString(base), blockSize);
    },

    safeCounter = function () {
      c = (c < discreteValues) ? c : 0;
      c++; // this is not subliminal
      return c - 1;
    },

    api = function cuid() {
      // Starting with a lowercase letter makes
      // it HTML element ID friendly.
      var letter = 'c', // hard-coded allows for sequential access

        // timestamp
        // warning: this exposes the exact date and time
        // that the uid was created.
        timestamp = (new Date().getTime()).toString(base),

        // Prevent same-machine collisions.
        counter,

        // A few chars to generate distinct ids for different
        // clients (so different computers are far less
        // likely to generate the same id)
        fingerprint = api.fingerprint(),

        // Grab some more chars from Math.random()
        random = randomBlock() + randomBlock();

        counter = pad(safeCounter().toString(base), blockSize);

      return  (letter + timestamp + counter + fingerprint + random);
    };

  api.slug = function slug() {
    var date = new Date().getTime().toString(36),
      counter,
      print = api.fingerprint().slice(0,1) +
        api.fingerprint().slice(-1),
      random = randomBlock().slice(-2);

      counter = safeCounter().toString(36).slice(-4);

    return date.slice(-2) + 
      counter + print + random;
  };

  api.globalCount = function globalCount() {
    // We want to cache the results of this
    var cache = (function calc() {
        var i,
          count = 0;

        for (i in window) {
          count++;
        }

        return count;
      }());

    api.globalCount = function () { return cache; };
    return cache;
  };

  api.fingerprint = function browserPrint() {
    return pad((navigator.mimeTypes.length +
      navigator.userAgent.length).toString(36) +
      api.globalCount().toString(36), 4);
  };

  // don't change anything from here down.
  if (app.register) {
    app.register(namespace, api);
  } else if (typeof module !== 'undefined') {
    module.exports = api;
  } else {
    app[namespace] = api;
  }

}(this.applitude || this));

},{}],10:[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":40}],11:[function(require,module,exports){
module.exports=require(3)
},{}],12:[function(require,module,exports){
if (typeof Object.create === 'function') {
  // implementation from standard node.js 'util' module
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  };
} else {
  // old school shim for old browsers
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    var TempCtor = function () {}
    TempCtor.prototype = superCtor.prototype
    ctor.prototype = new TempCtor()
    ctor.prototype.constructor = ctor
  }
}

},{}],13:[function(require,module,exports){
module.exports=require(4)
},{"./hidden-store.js":14}],14:[function(require,module,exports){
module.exports=require(5)
},{}],15:[function(require,module,exports){
var inherits = require("inherits")

var ALL_PROPS = [
    "altKey", "bubbles", "cancelable", "ctrlKey",
    "eventPhase", "metaKey", "relatedTarget", "shiftKey",
    "target", "timeStamp", "type", "view", "which"
]
var KEY_PROPS = ["char", "charCode", "key", "keyCode"]
var MOUSE_PROPS = [
    "button", "buttons", "clientX", "clientY", "layerX",
    "layerY", "offsetX", "offsetY", "pageX", "pageY",
    "screenX", "screenY", "toElement"
]

var rkeyEvent = /^key|input/
var rmouseEvent = /^(?:mouse|pointer|contextmenu)|click/

module.exports = ProxyEvent

function ProxyEvent(ev) {
    if (!(this instanceof ProxyEvent)) {
        return new ProxyEvent(ev)
    }

    if (rkeyEvent.test(ev.type)) {
        return new KeyEvent(ev)
    } else if (rmouseEvent.test(ev.type)) {
        return new MouseEvent(ev)
    }

    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    this._rawEvent = ev
    this._bubbles = false;
}

ProxyEvent.prototype.preventDefault = function () {
    this._rawEvent.preventDefault()
}

ProxyEvent.prototype.startPropagation = function () {
    this._bubbles = true;
}

function MouseEvent(ev) {
    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    for (var j = 0; j < MOUSE_PROPS.length; j++) {
        var mousePropKey = MOUSE_PROPS[j]
        this[mousePropKey] = ev[mousePropKey]
    }

    this._rawEvent = ev
}

inherits(MouseEvent, ProxyEvent)

function KeyEvent(ev) {
    for (var i = 0; i < ALL_PROPS.length; i++) {
        var propKey = ALL_PROPS[i]
        this[propKey] = ev[propKey]
    }

    for (var j = 0; j < KEY_PROPS.length; j++) {
        var keyPropKey = KEY_PROPS[j]
        this[keyPropKey] = ev[keyPropKey]
    }

    this._rawEvent = ev
}

inherits(KeyEvent, ProxyEvent)

},{"inherits":12}],16:[function(require,module,exports){
var DataSet = require("data-set")

module.exports = removeEvent

function removeEvent(target, type, handler) {
    var ds = DataSet(target)
    var events = ds[type]

    if (!events) {
        return
    } else if (Array.isArray(events)) {
        var index = events.indexOf(handler)
        if (index !== -1) {
            events.splice(index, 1)
        }
    } else if (events === handler) {
        ds[type] = null
    }
}

},{"data-set":2}],17:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("vtree/is-vhook")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (propValue === undefined) {
            removeProperty(node, props, previous, propName);
        } else if (isHook(propValue)) {
            propValue.hook(node,
                propName,
                previous ? previous[propName] : undefined)
        } else {
            if (isObject(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else if (propValue !== undefined) {
                node[propName] = propValue
            }
        }
    }
}

function removeProperty(node, props, previous, propName) {
    if (previous) {
        var previousValue = previous[propName]

        if (!isHook(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName)
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = ""
                }
            } else if (typeof previousValue === "string") {
                node[propName] = ""
            } else {
                node[propName] = null
            }
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName]

            if (attrValue === undefined) {
                node.removeAttribute(attrName)
            } else {
                node.setAttribute(attrName, attrValue)
            }
        }

        return
    }

    if(previousValue && isObject(previousValue) &&
        getPrototype(previousValue) !== getPrototype(propValue)) {
        node[propName] = propValue
        return
    }

    if (!isObject(node[propName])) {
        node[propName] = {}
    }

    var replacer = propName === "style" ? "" : undefined

    for (var k in propValue) {
        var value = propValue[k]
        node[propName][k] = (value === undefined) ? replacer : value
    }
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

},{"is-object":21,"vtree/is-vhook":29}],18:[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("vtree/is-vnode")
var isVText = require("vtree/is-vtext")
var isWidget = require("vtree/is-widget")
var handleThunk = require("vtree/handle-thunk")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    vnode = handleThunk(vnode).a

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"./apply-properties":17,"global/document":20,"vtree/handle-thunk":27,"vtree/is-vnode":30,"vtree/is-vtext":31,"vtree/is-widget":32}],19:[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],20:[function(require,module,exports){
module.exports=require(10)
},{"min-document":40}],21:[function(require,module,exports){
module.exports = isObject

function isObject(x) {
    return typeof x === "object" && x !== null
}

},{}],22:[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],23:[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("vtree/is-widget")
var VPatch = require("vtree/vpatch")

var render = require("./create-element")
var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.properties)
            return domNode
        case VPatch.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = render(vText, renderOptions)

        if (parentNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    destroyWidget(domNode, leftVNode)

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    if (updateWidget(leftVNode, widget)) {
        return widget.update(leftVNode, domNode) || domNode
    }

    var parentNode = domNode.parentNode
    var newWidget = render(widget, renderOptions)

    if (parentNode) {
        parentNode.replaceChild(newWidget, domNode)
    }

    destroyWidget(domNode, leftVNode)

    return newWidget
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    destroyWidget(domNode, leftVNode)

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, bIndex) {
    var children = []
    var childNodes = domNode.childNodes
    var len = childNodes.length
    var i
    var reverseIndex = bIndex.reverse

    for (i = 0; i < len; i++) {
        children.push(domNode.childNodes[i])
    }

    var insertOffset = 0
    var move
    var node
    var insertNode
    for (i = 0; i < len; i++) {
        move = bIndex[i]
        if (move !== undefined && move !== i) {
            // the element currently at this index will be moved later so increase the insert offset
            if (reverseIndex[i] > i) {
                insertOffset++
            }

            node = children[move]
            insertNode = childNodes[i + insertOffset] || null
            if (node !== insertNode) {
                domNode.insertBefore(node, insertNode)
            }

            // the moved element came from the front of the array so reduce the insert offset
            if (move < i) {
                insertOffset--
            }
        }

        // element at this index is scheduled to be removed so increase insert offset
        if (i in bIndex.removes) {
            insertOffset++
        }
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        console.log(oldRoot)
        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }

    return newRoot;
}

},{"./apply-properties":17,"./create-element":18,"./update-widget":25,"vtree/is-widget":32,"vtree/vpatch":37}],24:[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var domIndex = require("./dom-index")
var patchOp = require("./patch-op")
module.exports = patch

function patch(rootNode, patches) {
    return patchRecursive(rootNode, patches)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument

    if (!renderOptions) {
        renderOptions = { patch: patchRecursive }
        if (ownerDocument !== document) {
            renderOptions.document = ownerDocument
        }
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./dom-index":19,"./patch-op":23,"global/document":20,"x-is-array":22}],25:[function(require,module,exports){
var isWidget = require("vtree/is-widget")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"vtree/is-widget":32}],26:[function(require,module,exports){
var isArray = require("x-is-array")
var isObject = require("is-object")

var VPatch = require("./vpatch")
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")
var handleThunk = require("./handle-thunk")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        if (isThunk(a) || isThunk(b)) {
            thunks(a, b, patch, index)
        } else {
            hooks(b, patch, index)
        }
        return
    }

    var apply = patch[index]

    if (b == null) {
        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
        destroyWidgets(a, patch, index)
    } else if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index)
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties, b.hooks)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
                apply = diffChildren(a, b, patch, apply, index)
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                destroyWidgets(a, patch, index)
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            destroyWidgets(a, patch, index)
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            destroyWidgets(a, patch, index)
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isWidget(b)) {
        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))

        if (!isWidget(a)) {
            destroyWidgets(a, patch, index)
        }
    }

    if (apply) {
        patch[index] = apply
    }
}

function diffProps(a, b, hooks) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {}
            diff[aKey] = undefined
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (hooks && aKey in hooks) {
            diff = diff || {}
            diff[aKey] = bValue
        } else {
            if (isObject(aValue) && isObject(bValue)) {
                if (getPrototype(bValue) !== getPrototype(aValue)) {
                    diff = diff || {}
                    diff[aKey] = bValue
                } else {
                    var objectDiff = diffProps(aValue, bValue)
                    if (objectDiff) {
                        diff = diff || {}
                        diff[aKey] = objectDiff
                    }
                }
            } else if (aValue !== bValue) {
                diff = diff || {}
                diff[aKey] = bValue
            }
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var bChildren = reorder(aChildren, b.children)

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else if (!rightNode) {
            if (leftNode) {
                // Excess nodes in a need to be removed
                patch[index] = new VPatch(VPatch.REMOVE, leftNode, null)
                destroyWidgets(leftNode, patch, index)
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (bChildren.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(VPatch.ORDER, a, bChildren.moves))
    }

    return apply
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = new VPatch(VPatch.REMOVE, vNode, null)
        }
    } else if (isVNode(vNode) && vNode.hasWidgets) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b);
    var thunkPatch = diff(nodes.a, nodes.b)
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true;
        }
    }

    return false;
}

// Execute hooks when two nodes are identical
function hooks(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = new VPatch(VPatch.PROPS, vNode.hooks, vNode.hooks)
        }

        if (vNode.descendantHooks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                hooks(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    }
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {

    var bKeys = keyIndex(bChildren)

    if (!bKeys) {
        return bChildren
    }

    var aKeys = keyIndex(aChildren)

    if (!aKeys) {
        return bChildren
    }

    var bMatch = {}, aMatch = {}

    for (var key in bKeys) {
        bMatch[bKeys[key]] = aKeys[key]
    }

    for (var key in aKeys) {
        aMatch[aKeys[key]] = bKeys[key]
    }

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen
    var shuffle = []
    var freeIndex = 0
    var i = 0
    var moveIndex = 0
    var moves = {}
    var removes = moves.removes = {}
    var reverse = moves.reverse = {}
    var hasMoves = false

    while (freeIndex < len) {
        var move = aMatch[i]
        if (move !== undefined) {
            shuffle[i] = bChildren[move]
            if (move !== moveIndex) {
                moves[move] = moveIndex
                reverse[moveIndex] = move
                hasMoves = true
            }
            moveIndex++
        } else if (i in aMatch) {
            shuffle[i] = undefined
            removes[i] = moveIndex++
            hasMoves = true
        } else {
            while (bMatch[freeIndex] !== undefined) {
                freeIndex++
            }

            if (freeIndex < len) {
                var freeChild = bChildren[freeIndex]
                if (freeChild) {
                    shuffle[i] = freeChild
                    if (freeIndex !== moveIndex) {
                        hasMoves = true
                        moves[freeIndex] = moveIndex
                        reverse[moveIndex] = freeIndex
                    }
                    moveIndex++
                }
                freeIndex++
            }
        }
        i++
    }

    if (hasMoves) {
        shuffle.moves = moves
    }

    return shuffle
}

function keyIndex(children) {
    var i, keys

    for (i = 0; i < children.length; i++) {
        var child = children[i]

        if (child.key !== undefined) {
            keys = keys || {}
            keys[child.key] = i
        }
    }

    return keys
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"./handle-thunk":27,"./is-thunk":28,"./is-vnode":30,"./is-vtext":31,"./is-widget":32,"./vpatch":37,"is-object":33,"x-is-array":34}],27:[function(require,module,exports){
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    var renderedA = a
    var renderedB = b

    if (isThunk(b)) {
        renderedB = renderThunk(b, a)
    }

    if (isThunk(a)) {
        renderedA = renderThunk(a, null)
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous)
    }

    if (!(isVNode(renderedThunk) ||
            isVText(renderedThunk) ||
            isWidget(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

},{"./is-thunk":28,"./is-vnode":30,"./is-vtext":31,"./is-widget":32}],28:[function(require,module,exports){
module.exports = isThunk

function isThunk(t) {
    return t && t.type === "Thunk"
}

},{}],29:[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook && typeof hook.hook === "function" &&
        !hook.hasOwnProperty("hook")
}

},{}],30:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":35}],31:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version
}

},{"./version":35}],32:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],33:[function(require,module,exports){
module.exports=require(21)
},{}],34:[function(require,module,exports){
module.exports=require(22)
},{}],35:[function(require,module,exports){
module.exports = "1"

},{}],36:[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property)) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-vhook":29,"./is-vnode":30,"./is-widget":32,"./version":35}],37:[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7
VirtualPatch.THUNK = 8

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":35}],38:[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":35}],39:[function(require,module,exports){

var VNode = require('vtree/vnode');
var VText = require('vtree/vtext');
var diff = require('vtree/diff');
var patch = require('vdom/patch');
var createElement = require('vdom/create-element');
var DataSet = require("data-set");
var Delegator = require("dom-delegator");
var isHook = require("vtree/is-vhook");

Elm.Native.VirtualDom = {};
Elm.Native.VirtualDom.make = function(elm)
{
	elm.Native = elm.Native || {};
	elm.Native.VirtualDom = elm.Native.VirtualDom || {};
	if (elm.Native.VirtualDom.values)
	{
		return elm.Native.VirtualDom.values;
	}

	// This manages event listeners. Somehow...
	// Save a reference for use in on(...)
	var delegator = Delegator();

	var Element = Elm.Native.Graphics.Element.make(elm);
	var Json = Elm.Native.Json.make(elm);
	var List = Elm.Native.List.make(elm);
	var Signal = Elm.Native.Signal.make(elm);
	var Utils = Elm.Native.Utils.make(elm);

	var ATTRIBUTE_KEY = 'UniqueNameThatOthersAreVeryUnlikelyToUse';

	function listToProperties(list)
	{
		var object = {};
		while (list.ctor !== '[]')
		{
			var entry = list._0;
			if (entry.key === ATTRIBUTE_KEY)
			{
				object.attributes = object.attributes || {};
				object.attributes[entry.value.attrKey] = entry.value.attrValue;
			}
			else
			{
				object[entry.key] = entry.value;
			}
			list = list._1;
		}
		return object;
	}

	function node(name, propertyList, contents)
	{
		var props = listToProperties(propertyList);

		var key, namespace;
		// support keys
		if (props.key !== undefined)
		{
			key = props.key;
			props.key = undefined;
		}

		// support namespace
		if (props.namespace !== undefined)
		{
			namespace = props.namespace;
			props.namespace = undefined;
		}

		// ensure that setting text of an input does not move the cursor
		var useSoftSet =
			name === 'input'
			&& props.value !== undefined
			&& !isHook(props.value);

		if (useSoftSet)
		{
			props.value = SoftSetHook(props.value);
		}

		return new VNode(name, props, List.toArray(contents), key, namespace);
	}

	function property(key, value)
	{
		return {
			key: key,
			value: value
		};
	}

	function attribute(key, value)
	{
		return {
			key: ATTRIBUTE_KEY,
			value: {
				attrKey: key,
				attrValue: value
			}
		};
	}

	function on(name, decoder, createMessage)
	{
		// Ensure we're listening for this type of event
		delegator.listenTo(name);
		function eventHandler(event)
		{
			var value = A2(Json.runDecoderValue, decoder, event);
			if (value.ctor === 'Ok')
			{
				Signal.sendMessage(createMessage(value._0));
			}
		}
		return property(name, DataSetHook(eventHandler));
	}

	function DataSetHook(value)
	{
		if (!(this instanceof DataSetHook))
		{
			return new DataSetHook(value);
		}

		this.value = value;
	}

	DataSetHook.prototype.hook = function (node, propertyName) {
		var ds = DataSet(node);
		ds[propertyName] = this.value;
	};


	function SoftSetHook(value)
	{
		if (!(this instanceof SoftSetHook))
		{
			return new SoftSetHook(value);
		}

		this.value = value;
	}

	SoftSetHook.prototype.hook = function (node, propertyName)
	{
		if (node[propertyName] !== this.value)
		{
			node[propertyName] = this.value;
		}
	};

	function text(string)
	{
		return new VText(string);
	}

	function fromElement(element)
	{
		return {
			type: "Widget",

			element: element,

			init: function () {
				return Element.render(element);
			},

			update: function (previous, node) {
				return Element.update(node, previous.element, element);
			}
		};
	}

	function toElement(width, height, html)
	{
		return A3(Element.newElement, width, height, {
			ctor: 'Custom',
			type: 'evancz/elm-html',
			render: render,
			update: update,
			model: html
		});
	}

	function render(model)
	{
		var element = Element.createNode('div');
		element.appendChild(createElement(model));
		return element;
	}

	function update(node, oldModel, newModel)
	{
		updateAndReplace(node.firstChild, oldModel, newModel);
		return node;
	}

	function updateAndReplace(node, oldModel, newModel)
	{
		var patches = diff(oldModel, newModel);
		var newNode = patch(node, patches);
		return newNode;
	}

	function lazyRef(fn, a)
	{
		function thunk()
		{
			return fn(a);
		}
		return new Thunk(fn, [a], thunk);
	}

	function lazyRef2(fn, a, b)
	{
		function thunk()
		{
			return A2(fn, a, b);
		}
		return new Thunk(fn, [a,b], thunk);
	}

	function lazyRef3(fn, a, b, c)
	{
		function thunk()
		{
			return A3(fn, a, b, c);
		}
		return new Thunk(fn, [a,b,c], thunk);
	}

	function Thunk(fn, args, thunk)
	{
		this.fn = fn;
		this.args = args;
		this.vnode = null;
		this.key = undefined;
		this.thunk = thunk;
	}

	Thunk.prototype.type = "Thunk";
	Thunk.prototype.update = updateThunk;
	Thunk.prototype.render = renderThunk;

	function shouldUpdate(current, previous)
	{
		if (current.fn !== previous.fn)
		{
			return true;
		}

		// if it's the same function, we know the number of args must match
		var cargs = current.args;
		var pargs = previous.args;

		for (var i = cargs.length; i--; )
		{
			if (cargs[i] !== pargs[i])
			{
				return true;
			}
		}

		return false;
	}

	function updateThunk(previous, domNode)
	{
		if (!shouldUpdate(this, previous))
		{
			this.vnode = previous.vnode;
			return;
		}

		if (!this.vnode)
		{
			this.vnode = this.thunk();
		}

		var patches = diff(previous.vnode, this.vnode);
		patch(domNode, patches);
	}

	function renderThunk()
	{
		return this.thunk();
	}

	return Elm.Native.VirtualDom.values = {
		node: F3(node),
		text: text,
		on: F3(on),

		property: F2(property),
		attribute: F2(attribute),

		lazy: F2(lazyRef),
		lazy2: F3(lazyRef2),
		lazy3: F4(lazyRef3),

		toElement: F3(toElement),
		fromElement: fromElement,

		render: createElement,
		updateAndReplace: updateAndReplace
	};
};

},{"data-set":2,"dom-delegator":8,"vdom/create-element":18,"vdom/patch":24,"vtree/diff":26,"vtree/is-vhook":29,"vtree/vnode":36,"vtree/vtext":38}],40:[function(require,module,exports){

},{}]},{},[39]);

Elm.Price = Elm.Price || {};
Elm.Price.make = function (_elm) {
   "use strict";
   _elm.Price = _elm.Price || {};
   if (_elm.Price.values)
   return _elm.Price.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Price",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var format = function (p) {
      return function () {
         switch (p.ctor)
         {case "EUR":
            return A2($Basics._op["++"],
              "€ ",
              $Basics.toString($Basics.toFloat(p._0) / 100));}
         _U.badCase($moduleName,
         "between lines 13 and 14");
      }();
   };
   var EUR = function (a) {
      return {ctor: "EUR",_0: a};
   };
   var add = F2(function (_v2,
   _v3) {
      return function () {
         switch (_v3.ctor)
         {case "EUR":
            return function () {
                 switch (_v2.ctor)
                 {case "EUR":
                    return EUR(_v2._0 + _v3._0);}
                 _U.badCase($moduleName,
                 "on line 6, column 23 to 35");
              }();}
         _U.badCase($moduleName,
         "on line 6, column 23 to 35");
      }();
   });
   var fromInt = function (x) {
      return EUR(x);
   };
   _elm.Price.values = {_op: _op
                       ,format: format
                       ,fromInt: fromInt
                       ,add: add};
   return _elm.Price.values;
};
Elm.Regex = Elm.Regex || {};
Elm.Regex.make = function (_elm) {
   "use strict";
   _elm.Regex = _elm.Regex || {};
   if (_elm.Regex.values)
   return _elm.Regex.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Regex",
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Regex = Elm.Native.Regex.make(_elm);
   var split = $Native$Regex.split;
   var replace = $Native$Regex.replace;
   var find = $Native$Regex.find;
   var AtMost = function (a) {
      return {ctor: "AtMost"
             ,_0: a};
   };
   var All = {ctor: "All"};
   var Match = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,index: c
             ,match: a
             ,number: d
             ,submatches: b};
   });
   var contains = $Native$Regex.contains;
   var caseInsensitive = $Native$Regex.caseInsensitive;
   var regex = $Native$Regex.regex;
   var escape = $Native$Regex.escape;
   var Regex = {ctor: "Regex"};
   _elm.Regex.values = {_op: _op
                       ,regex: regex
                       ,escape: escape
                       ,caseInsensitive: caseInsensitive
                       ,contains: contains
                       ,find: find
                       ,replace: replace
                       ,split: split
                       ,Match: Match
                       ,All: All
                       ,AtMost: AtMost};
   return _elm.Regex.values;
};
Elm.Result = Elm.Result || {};
Elm.Result.make = function (_elm) {
   "use strict";
   _elm.Result = _elm.Result || {};
   if (_elm.Result.values)
   return _elm.Result.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Result",
   $Maybe = Elm.Maybe.make(_elm);
   var toMaybe = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return $Maybe.Nothing;
            case "Ok":
            return $Maybe.Just(result._0);}
         _U.badCase($moduleName,
         "between lines 164 and 166");
      }();
   };
   var Err = function (a) {
      return {ctor: "Err",_0: a};
   };
   var andThen = F2(function (result,
   callback) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(result._0);
            case "Ok":
            return callback(result._0);}
         _U.badCase($moduleName,
         "between lines 126 and 128");
      }();
   });
   var Ok = function (a) {
      return {ctor: "Ok",_0: a};
   };
   var map = F2(function (func,
   ra) {
      return function () {
         switch (ra.ctor)
         {case "Err": return Err(ra._0);
            case "Ok":
            return Ok(func(ra._0));}
         _U.badCase($moduleName,
         "between lines 41 and 43");
      }();
   });
   var map2 = F3(function (func,
   ra,
   rb) {
      return function () {
         var _v9 = {ctor: "_Tuple2"
                   ,_0: ra
                   ,_1: rb};
         switch (_v9.ctor)
         {case "_Tuple2":
            switch (_v9._0.ctor)
              {case "Err":
                 return Err(_v9._0._0);
                 case "Ok": switch (_v9._1.ctor)
                   {case "Ok": return Ok(A2(func,
                        _v9._0._0,
                        _v9._1._0));}
                   break;}
              switch (_v9._1.ctor)
              {case "Err":
                 return Err(_v9._1._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 55 and 58");
      }();
   });
   var map3 = F4(function (func,
   ra,
   rb,
   rc) {
      return function () {
         var _v16 = {ctor: "_Tuple3"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc};
         switch (_v16.ctor)
         {case "_Tuple3":
            switch (_v16._0.ctor)
              {case "Err":
                 return Err(_v16._0._0);
                 case "Ok": switch (_v16._1.ctor)
                   {case "Ok":
                      switch (_v16._2.ctor)
                        {case "Ok": return Ok(A3(func,
                             _v16._0._0,
                             _v16._1._0,
                             _v16._2._0));}
                        break;}
                   break;}
              switch (_v16._1.ctor)
              {case "Err":
                 return Err(_v16._1._0);}
              switch (_v16._2.ctor)
              {case "Err":
                 return Err(_v16._2._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 63 and 67");
      }();
   });
   var map4 = F5(function (func,
   ra,
   rb,
   rc,
   rd) {
      return function () {
         var _v26 = {ctor: "_Tuple4"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd};
         switch (_v26.ctor)
         {case "_Tuple4":
            switch (_v26._0.ctor)
              {case "Err":
                 return Err(_v26._0._0);
                 case "Ok": switch (_v26._1.ctor)
                   {case "Ok":
                      switch (_v26._2.ctor)
                        {case "Ok":
                           switch (_v26._3.ctor)
                             {case "Ok": return Ok(A4(func,
                                  _v26._0._0,
                                  _v26._1._0,
                                  _v26._2._0,
                                  _v26._3._0));}
                             break;}
                        break;}
                   break;}
              switch (_v26._1.ctor)
              {case "Err":
                 return Err(_v26._1._0);}
              switch (_v26._2.ctor)
              {case "Err":
                 return Err(_v26._2._0);}
              switch (_v26._3.ctor)
              {case "Err":
                 return Err(_v26._3._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 72 and 77");
      }();
   });
   var map5 = F6(function (func,
   ra,
   rb,
   rc,
   rd,
   re) {
      return function () {
         var _v39 = {ctor: "_Tuple5"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd
                    ,_4: re};
         switch (_v39.ctor)
         {case "_Tuple5":
            switch (_v39._0.ctor)
              {case "Err":
                 return Err(_v39._0._0);
                 case "Ok": switch (_v39._1.ctor)
                   {case "Ok":
                      switch (_v39._2.ctor)
                        {case "Ok":
                           switch (_v39._3.ctor)
                             {case "Ok":
                                switch (_v39._4.ctor)
                                  {case "Ok": return Ok(A5(func,
                                       _v39._0._0,
                                       _v39._1._0,
                                       _v39._2._0,
                                       _v39._3._0,
                                       _v39._4._0));}
                                  break;}
                             break;}
                        break;}
                   break;}
              switch (_v39._1.ctor)
              {case "Err":
                 return Err(_v39._1._0);}
              switch (_v39._2.ctor)
              {case "Err":
                 return Err(_v39._2._0);}
              switch (_v39._3.ctor)
              {case "Err":
                 return Err(_v39._3._0);}
              switch (_v39._4.ctor)
              {case "Err":
                 return Err(_v39._4._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 82 and 88");
      }();
   });
   var formatError = F2(function (f,
   result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(f(result._0));
            case "Ok":
            return Ok(result._0);}
         _U.badCase($moduleName,
         "between lines 148 and 150");
      }();
   });
   var fromMaybe = F2(function (err,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Ok(maybe._0);
            case "Nothing":
            return Err(err);}
         _U.badCase($moduleName,
         "between lines 180 and 182");
      }();
   });
   _elm.Result.values = {_op: _op
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,andThen: andThen
                        ,toMaybe: toMaybe
                        ,fromMaybe: fromMaybe
                        ,formatError: formatError
                        ,Ok: Ok
                        ,Err: Err};
   return _elm.Result.values;
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values)
   return _elm.Signal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Signal",
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var send = F2(function (_v0,
   value) {
      return function () {
         switch (_v0.ctor)
         {case "Address":
            return A2($Task.onError,
              _v0._0(value),
              function (_v3) {
                 return function () {
                    return $Task.succeed({ctor: "_Tuple0"});
                 }();
              });}
         _U.badCase($moduleName,
         "between lines 370 and 371");
      }();
   });
   var Message = function (a) {
      return {ctor: "Message"
             ,_0: a};
   };
   var message = F2(function (_v5,
   value) {
      return function () {
         switch (_v5.ctor)
         {case "Address":
            return Message(_v5._0(value));}
         _U.badCase($moduleName,
         "on line 352, column 5 to 24");
      }();
   });
   var mailbox = $Native$Signal.mailbox;
   var Address = function (a) {
      return {ctor: "Address"
             ,_0: a};
   };
   var forwardTo = F2(function (_v8,
   f) {
      return function () {
         switch (_v8.ctor)
         {case "Address":
            return Address(function (x) {
                 return _v8._0(f(x));
              });}
         _U.badCase($moduleName,
         "on line 339, column 5 to 29");
      }();
   });
   var Mailbox = F2(function (a,
   b) {
      return {_: {}
             ,address: a
             ,signal: b};
   });
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var filterMap = $Native$Signal.filterMap;
   var filter = F3(function (isOk,
   base,
   signal) {
      return A3(filterMap,
      function (value) {
         return isOk(value) ? $Maybe.Just(value) : $Maybe.Nothing;
      },
      base,
      signal);
   });
   var merge = F2(function (left,
   right) {
      return A3($Native$Signal.genericMerge,
      $Basics.always,
      left,
      right);
   });
   var mergeMany = function (signalList) {
      return function () {
         var _v11 = $List.reverse(signalList);
         switch (_v11.ctor)
         {case "::":
            return A3($List.foldl,
              merge,
              _v11._0,
              _v11._1);
            case "[]":
            return $Debug.crash("mergeMany was given an empty list!");}
         _U.badCase($moduleName,
         "between lines 177 and 182");
      }();
   };
   var foldp = $Native$Signal.foldp;
   var map5 = $Native$Signal.map5;
   var map4 = $Native$Signal.map4;
   var map3 = $Native$Signal.map3;
   var map2 = $Native$Signal.map2;
   _op["~"] = F2(function (funcs,
   args) {
      return A3(map2,
      F2(function (f,v) {
         return f(v);
      }),
      funcs,
      args);
   });
   var map = $Native$Signal.map;
   _op["<~"] = map;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   _elm.Signal.values = {_op: _op
                        ,merge: merge
                        ,mergeMany: mergeMany
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,constant: constant
                        ,dropRepeats: dropRepeats
                        ,filter: filter
                        ,filterMap: filterMap
                        ,sampleOn: sampleOn
                        ,foldp: foldp
                        ,mailbox: mailbox
                        ,send: send
                        ,message: message
                        ,forwardTo: forwardTo
                        ,Mailbox: Mailbox};
   return _elm.Signal.values;
};
Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values)
   return _elm.String.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "String",
   $Maybe = Elm.Maybe.make(_elm),
   $Native$String = Elm.Native.String.make(_elm),
   $Result = Elm.Result.make(_elm);
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var fromChar = function ($char) {
      return A2(cons,$char,"");
   };
   var isEmpty = $Native$String.isEmpty;
   _elm.String.values = {_op: _op
                        ,isEmpty: isEmpty
                        ,length: length
                        ,reverse: reverse
                        ,repeat: repeat
                        ,cons: cons
                        ,uncons: uncons
                        ,fromChar: fromChar
                        ,append: append
                        ,concat: concat
                        ,split: split
                        ,join: join
                        ,words: words
                        ,lines: lines
                        ,slice: slice
                        ,left: left
                        ,right: right
                        ,dropLeft: dropLeft
                        ,dropRight: dropRight
                        ,contains: contains
                        ,startsWith: startsWith
                        ,endsWith: endsWith
                        ,indexes: indexes
                        ,indices: indices
                        ,toInt: toInt
                        ,toFloat: toFloat
                        ,toList: toList
                        ,fromList: fromList
                        ,toUpper: toUpper
                        ,toLower: toLower
                        ,pad: pad
                        ,padLeft: padLeft
                        ,padRight: padRight
                        ,trim: trim
                        ,trimLeft: trimLeft
                        ,trimRight: trimRight
                        ,map: map
                        ,filter: filter
                        ,foldl: foldl
                        ,foldr: foldr
                        ,any: any
                        ,all: all};
   return _elm.String.values;
};
Elm.Svg = Elm.Svg || {};
Elm.Svg.make = function (_elm) {
   "use strict";
   _elm.Svg = _elm.Svg || {};
   if (_elm.Svg.values)
   return _elm.Svg.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Svg",
   $Basics = Elm.Basics.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var svgNamespace = A2($VirtualDom.property,
   "namespace",
   $Json$Encode.string("http://www.w3.org/2000/svg"));
   var svgNode = F3(function (name,
   attributes,
   children) {
      return A3($VirtualDom.node,
      name,
      A2($List._op["::"],
      svgNamespace,
      attributes),
      children);
   });
   var svg = F2(function (attributes,
   children) {
      return A3(svgNode,
      "svg",
      attributes,
      children);
   });
   var foreignObject = F2(function (attributes,
   children) {
      return A3(svgNode,
      "foreignObject",
      attributes,
      children);
   });
   var animate = F2(function (attributes,
   children) {
      return A3(svgNode,
      "animate",
      attributes,
      children);
   });
   var animateColor = F2(function (attributes,
   children) {
      return A3(svgNode,
      "animateColor",
      attributes,
      children);
   });
   var animateMotion = F2(function (attributes,
   children) {
      return A3(svgNode,
      "animateMotion",
      attributes,
      children);
   });
   var animateTransform = F2(function (attributes,
   children) {
      return A3(svgNode,
      "animateTransform",
      attributes,
      children);
   });
   var mpath = F2(function (attributes,
   children) {
      return A3(svgNode,
      "mpath",
      attributes,
      children);
   });
   var set = F2(function (attributes,
   children) {
      return A3(svgNode,
      "set",
      attributes,
      children);
   });
   var a = F2(function (attributes,
   children) {
      return A3(svgNode,
      "a",
      attributes,
      children);
   });
   var defs = F2(function (attributes,
   children) {
      return A3(svgNode,
      "defs",
      attributes,
      children);
   });
   var g = F2(function (attributes,
   children) {
      return A3(svgNode,
      "g",
      attributes,
      children);
   });
   var marker = F2(function (attributes,
   children) {
      return A3(svgNode,
      "marker",
      attributes,
      children);
   });
   var mask = F2(function (attributes,
   children) {
      return A3(svgNode,
      "mask",
      attributes,
      children);
   });
   var missingGlyph = F2(function (attributes,
   children) {
      return A3(svgNode,
      "missingGlyph",
      attributes,
      children);
   });
   var pattern = F2(function (attributes,
   children) {
      return A3(svgNode,
      "pattern",
      attributes,
      children);
   });
   var $switch = F2(function (attributes,
   children) {
      return A3(svgNode,
      "switch",
      attributes,
      children);
   });
   var symbol = F2(function (attributes,
   children) {
      return A3(svgNode,
      "symbol",
      attributes,
      children);
   });
   var desc = F2(function (attributes,
   children) {
      return A3(svgNode,
      "desc",
      attributes,
      children);
   });
   var metadata = F2(function (attributes,
   children) {
      return A3(svgNode,
      "metadata",
      attributes,
      children);
   });
   var title = F2(function (attributes,
   children) {
      return A3(svgNode,
      "title",
      attributes,
      children);
   });
   var feBlend = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feBlend",
      attributes,
      children);
   });
   var feColorMatrix = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feColorMatrix",
      attributes,
      children);
   });
   var feComponentTransfer = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feComponentTransfer",
      attributes,
      children);
   });
   var feComposite = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feComposite",
      attributes,
      children);
   });
   var feConvolveMatrix = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feConvolveMatrix",
      attributes,
      children);
   });
   var feDiffuseLighting = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feDiffuseLighting",
      attributes,
      children);
   });
   var feDisplacementMap = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feDisplacementMap",
      attributes,
      children);
   });
   var feFlood = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feFlood",
      attributes,
      children);
   });
   var feFuncA = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feFuncA",
      attributes,
      children);
   });
   var feFuncB = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feFuncB",
      attributes,
      children);
   });
   var feFuncG = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feFuncG",
      attributes,
      children);
   });
   var feFuncR = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feFuncR",
      attributes,
      children);
   });
   var feGaussianBlur = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feGaussianBlur",
      attributes,
      children);
   });
   var feImage = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feImage",
      attributes,
      children);
   });
   var feMerge = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feMerge",
      attributes,
      children);
   });
   var feMergeNode = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feMergeNode",
      attributes,
      children);
   });
   var feMorphology = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feMorphology",
      attributes,
      children);
   });
   var feOffset = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feOffset",
      attributes,
      children);
   });
   var feSpecularLighting = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feSpecularLighting",
      attributes,
      children);
   });
   var feTile = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feTile",
      attributes,
      children);
   });
   var feTurbulence = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feTurbulence",
      attributes,
      children);
   });
   var font = F2(function (attributes,
   children) {
      return A3(svgNode,
      "font",
      attributes,
      children);
   });
   var fontFace = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fontFace",
      attributes,
      children);
   });
   var fontFaceFormat = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fontFaceFormat",
      attributes,
      children);
   });
   var fontFaceName = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fontFaceName",
      attributes,
      children);
   });
   var fontFaceSrc = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fontFaceSrc",
      attributes,
      children);
   });
   var fontFaceUri = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fontFaceUri",
      attributes,
      children);
   });
   var hkern = F2(function (attributes,
   children) {
      return A3(svgNode,
      "hkern",
      attributes,
      children);
   });
   var vkern = F2(function (attributes,
   children) {
      return A3(svgNode,
      "vkern",
      attributes,
      children);
   });
   var linearGradient = F2(function (attributes,
   children) {
      return A3(svgNode,
      "linearGradient",
      attributes,
      children);
   });
   var radialGradient = F2(function (attributes,
   children) {
      return A3(svgNode,
      "radialGradient",
      attributes,
      children);
   });
   var stop = F2(function (attributes,
   children) {
      return A3(svgNode,
      "stop",
      attributes,
      children);
   });
   var circle = F2(function (attributes,
   children) {
      return A3(svgNode,
      "circle",
      attributes,
      children);
   });
   var ellipse = F2(function (attributes,
   children) {
      return A3(svgNode,
      "ellipse",
      attributes,
      children);
   });
   var image = F2(function (attributes,
   children) {
      return A3(svgNode,
      "image",
      attributes,
      children);
   });
   var line = F2(function (attributes,
   children) {
      return A3(svgNode,
      "line",
      attributes,
      children);
   });
   var path = F2(function (attributes,
   children) {
      return A3(svgNode,
      "path",
      attributes,
      children);
   });
   var polygon = F2(function (attributes,
   children) {
      return A3(svgNode,
      "polygon",
      attributes,
      children);
   });
   var polyline = F2(function (attributes,
   children) {
      return A3(svgNode,
      "polyline",
      attributes,
      children);
   });
   var rect = F2(function (attributes,
   children) {
      return A3(svgNode,
      "rect",
      attributes,
      children);
   });
   var use = F2(function (attributes,
   children) {
      return A3(svgNode,
      "use",
      attributes,
      children);
   });
   var feDistantLight = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feDistantLight",
      attributes,
      children);
   });
   var fePointLight = F2(function (attributes,
   children) {
      return A3(svgNode,
      "fePointLight",
      attributes,
      children);
   });
   var feSpotLight = F2(function (attributes,
   children) {
      return A3(svgNode,
      "feSpotLight",
      attributes,
      children);
   });
   var altGlyph = F2(function (attributes,
   children) {
      return A3(svgNode,
      "altGlyph",
      attributes,
      children);
   });
   var altGlyphDef = F2(function (attributes,
   children) {
      return A3(svgNode,
      "altGlyphDef",
      attributes,
      children);
   });
   var altGlyphItem = F2(function (attributes,
   children) {
      return A3(svgNode,
      "altGlyphItem",
      attributes,
      children);
   });
   var glyph = F2(function (attributes,
   children) {
      return A3(svgNode,
      "glyph",
      attributes,
      children);
   });
   var glyphRef = F2(function (attributes,
   children) {
      return A3(svgNode,
      "glyphRef",
      attributes,
      children);
   });
   var textPath = F2(function (attributes,
   children) {
      return A3(svgNode,
      "textPath",
      attributes,
      children);
   });
   var text = F2(function (attributes,
   children) {
      return A3(svgNode,
      "text",
      attributes,
      children);
   });
   var tref = F2(function (attributes,
   children) {
      return A3(svgNode,
      "tref",
      attributes,
      children);
   });
   var tspan = F2(function (attributes,
   children) {
      return A3(svgNode,
      "tspan",
      attributes,
      children);
   });
   var clipPath = F2(function (attributes,
   children) {
      return A3(svgNode,
      "clipPath",
      attributes,
      children);
   });
   var colorProfile = F2(function (attributes,
   children) {
      return A3(svgNode,
      "colorProfile",
      attributes,
      children);
   });
   var cursor = F2(function (attributes,
   children) {
      return A3(svgNode,
      "cursor",
      attributes,
      children);
   });
   var filter = F2(function (attributes,
   children) {
      return A3(svgNode,
      "filter",
      attributes,
      children);
   });
   var script = F2(function (attributes,
   children) {
      return A3(svgNode,
      "script",
      attributes,
      children);
   });
   var style = F2(function (attributes,
   children) {
      return A3(svgNode,
      "style",
      attributes,
      children);
   });
   var view = F2(function (attributes,
   children) {
      return A3(svgNode,
      "view",
      attributes,
      children);
   });
   _elm.Svg.values = {_op: _op
                     ,svgNamespace: svgNamespace
                     ,svgNode: svgNode
                     ,svg: svg
                     ,foreignObject: foreignObject
                     ,animate: animate
                     ,animateColor: animateColor
                     ,animateMotion: animateMotion
                     ,animateTransform: animateTransform
                     ,mpath: mpath
                     ,set: set
                     ,a: a
                     ,defs: defs
                     ,g: g
                     ,marker: marker
                     ,mask: mask
                     ,missingGlyph: missingGlyph
                     ,pattern: pattern
                     ,$switch: $switch
                     ,symbol: symbol
                     ,desc: desc
                     ,metadata: metadata
                     ,title: title
                     ,feBlend: feBlend
                     ,feColorMatrix: feColorMatrix
                     ,feComponentTransfer: feComponentTransfer
                     ,feComposite: feComposite
                     ,feConvolveMatrix: feConvolveMatrix
                     ,feDiffuseLighting: feDiffuseLighting
                     ,feDisplacementMap: feDisplacementMap
                     ,feFlood: feFlood
                     ,feFuncA: feFuncA
                     ,feFuncB: feFuncB
                     ,feFuncG: feFuncG
                     ,feFuncR: feFuncR
                     ,feGaussianBlur: feGaussianBlur
                     ,feImage: feImage
                     ,feMerge: feMerge
                     ,feMergeNode: feMergeNode
                     ,feMorphology: feMorphology
                     ,feOffset: feOffset
                     ,feSpecularLighting: feSpecularLighting
                     ,feTile: feTile
                     ,feTurbulence: feTurbulence
                     ,font: font
                     ,fontFace: fontFace
                     ,fontFaceFormat: fontFaceFormat
                     ,fontFaceName: fontFaceName
                     ,fontFaceSrc: fontFaceSrc
                     ,fontFaceUri: fontFaceUri
                     ,hkern: hkern
                     ,vkern: vkern
                     ,linearGradient: linearGradient
                     ,radialGradient: radialGradient
                     ,stop: stop
                     ,circle: circle
                     ,ellipse: ellipse
                     ,image: image
                     ,line: line
                     ,path: path
                     ,polygon: polygon
                     ,polyline: polyline
                     ,rect: rect
                     ,use: use
                     ,feDistantLight: feDistantLight
                     ,fePointLight: fePointLight
                     ,feSpotLight: feSpotLight
                     ,altGlyph: altGlyph
                     ,altGlyphDef: altGlyphDef
                     ,altGlyphItem: altGlyphItem
                     ,glyph: glyph
                     ,glyphRef: glyphRef
                     ,textPath: textPath
                     ,text: text
                     ,tref: tref
                     ,tspan: tspan
                     ,clipPath: clipPath
                     ,colorProfile: colorProfile
                     ,cursor: cursor
                     ,filter: filter
                     ,script: script
                     ,style: style
                     ,view: view};
   return _elm.Svg.values;
};
Elm.Svg = Elm.Svg || {};
Elm.Svg.Attributes = Elm.Svg.Attributes || {};
Elm.Svg.Attributes.make = function (_elm) {
   "use strict";
   _elm.Svg = _elm.Svg || {};
   _elm.Svg.Attributes = _elm.Svg.Attributes || {};
   if (_elm.Svg.Attributes.values)
   return _elm.Svg.Attributes.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Svg.Attributes",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Svg = Elm.Svg.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var writingMode = function (value) {
      return A2($VirtualDom.attribute,
      "writing-mode",
      value);
   };
   var wordSpacing = function (value) {
      return A2($VirtualDom.attribute,
      "word-spacing",
      value);
   };
   var visibility = function (value) {
      return A2($VirtualDom.attribute,
      "visibility",
      value);
   };
   var unicodeBidi = function (value) {
      return A2($VirtualDom.attribute,
      "unicode-bidi",
      value);
   };
   var textRendering = function (value) {
      return A2($VirtualDom.attribute,
      "text-rendering",
      value);
   };
   var textDecoration = function (value) {
      return A2($VirtualDom.attribute,
      "text-decoration",
      value);
   };
   var textAnchor = function (value) {
      return A2($VirtualDom.attribute,
      "text-anchor",
      value);
   };
   var stroke = function (value) {
      return A2($VirtualDom.attribute,
      "stroke",
      value);
   };
   var strokeWidth = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-width",
      value);
   };
   var strokeOpacity = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-opacity",
      value);
   };
   var strokeMiterlimit = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-miterlimit",
      value);
   };
   var strokeLinejoin = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-linejoin",
      value);
   };
   var strokeLinecap = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-linecap",
      value);
   };
   var strokeDashoffset = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-dashoffset",
      value);
   };
   var strokeDasharray = function (value) {
      return A2($VirtualDom.attribute,
      "stroke-dasharray",
      value);
   };
   var stopOpacity = function (value) {
      return A2($VirtualDom.attribute,
      "stop-opacity",
      value);
   };
   var stopColor = function (value) {
      return A2($VirtualDom.attribute,
      "stop-color",
      value);
   };
   var shapeRendering = function (value) {
      return A2($VirtualDom.attribute,
      "shape-rendering",
      value);
   };
   var pointerEvents = function (value) {
      return A2($VirtualDom.attribute,
      "pointer-events",
      value);
   };
   var overflow = function (value) {
      return A2($VirtualDom.attribute,
      "overflow",
      value);
   };
   var opacity = function (value) {
      return A2($VirtualDom.attribute,
      "opacity",
      value);
   };
   var mask = function (value) {
      return A2($VirtualDom.attribute,
      "mask",
      value);
   };
   var markerStart = function (value) {
      return A2($VirtualDom.attribute,
      "marker-start",
      value);
   };
   var markerMid = function (value) {
      return A2($VirtualDom.attribute,
      "marker-mid",
      value);
   };
   var markerEnd = function (value) {
      return A2($VirtualDom.attribute,
      "marker-end",
      value);
   };
   var lightingColor = function (value) {
      return A2($VirtualDom.attribute,
      "lighting-color",
      value);
   };
   var letterSpacing = function (value) {
      return A2($VirtualDom.attribute,
      "letter-spacing",
      value);
   };
   var kerning = function (value) {
      return A2($VirtualDom.attribute,
      "kerning",
      value);
   };
   var imageRendering = function (value) {
      return A2($VirtualDom.attribute,
      "image-rendering",
      value);
   };
   var glyphOrientationVertical = function (value) {
      return A2($VirtualDom.attribute,
      "glyph-orientation-vertical",
      value);
   };
   var glyphOrientationHorizontal = function (value) {
      return A2($VirtualDom.attribute,
      "glyph-orientation-horizontal",
      value);
   };
   var fontWeight = function (value) {
      return A2($VirtualDom.attribute,
      "font-weight",
      value);
   };
   var fontVariant = function (value) {
      return A2($VirtualDom.attribute,
      "font-variant",
      value);
   };
   var fontStyle = function (value) {
      return A2($VirtualDom.attribute,
      "font-style",
      value);
   };
   var fontStretch = function (value) {
      return A2($VirtualDom.attribute,
      "font-stretch",
      value);
   };
   var fontSize = function (value) {
      return A2($VirtualDom.attribute,
      "font-size",
      value);
   };
   var fontSizeAdjust = function (value) {
      return A2($VirtualDom.attribute,
      "font-size-adjust",
      value);
   };
   var fontFamily = function (value) {
      return A2($VirtualDom.attribute,
      "font-family",
      value);
   };
   var floodOpacity = function (value) {
      return A2($VirtualDom.attribute,
      "flood-opacity",
      value);
   };
   var floodColor = function (value) {
      return A2($VirtualDom.attribute,
      "flood-color",
      value);
   };
   var filter = function (value) {
      return A2($VirtualDom.attribute,
      "filter",
      value);
   };
   var fill = function (value) {
      return A2($VirtualDom.attribute,
      "fill",
      value);
   };
   var fillRule = function (value) {
      return A2($VirtualDom.attribute,
      "fill-rule",
      value);
   };
   var fillOpacity = function (value) {
      return A2($VirtualDom.attribute,
      "fill-opacity",
      value);
   };
   var enableBackground = function (value) {
      return A2($VirtualDom.attribute,
      "enable-background",
      value);
   };
   var dominantBaseline = function (value) {
      return A2($VirtualDom.attribute,
      "dominant-baseline",
      value);
   };
   var display = function (value) {
      return A2($VirtualDom.attribute,
      "display",
      value);
   };
   var direction = function (value) {
      return A2($VirtualDom.attribute,
      "direction",
      value);
   };
   var cursor = function (value) {
      return A2($VirtualDom.attribute,
      "cursor",
      value);
   };
   var color = function (value) {
      return A2($VirtualDom.attribute,
      "color",
      value);
   };
   var colorRendering = function (value) {
      return A2($VirtualDom.attribute,
      "color-rendering",
      value);
   };
   var colorProfile = function (value) {
      return A2($VirtualDom.attribute,
      "color-profile",
      value);
   };
   var colorInterpolation = function (value) {
      return A2($VirtualDom.attribute,
      "color-interpolation",
      value);
   };
   var colorInterpolationFilters = function (value) {
      return A2($VirtualDom.attribute,
      "color-interpolation-filters",
      value);
   };
   var clip = function (value) {
      return A2($VirtualDom.attribute,
      "clip",
      value);
   };
   var clipRule = function (value) {
      return A2($VirtualDom.attribute,
      "clip-rule",
      value);
   };
   var clipPath = function (value) {
      return A2($VirtualDom.attribute,
      "clip-path",
      value);
   };
   var baselineShift = function (value) {
      return A2($VirtualDom.attribute,
      "baseline-shift",
      value);
   };
   var alignmentBaseline = function (value) {
      return A2($VirtualDom.attribute,
      "alignment-baseline",
      value);
   };
   var zoomAndPan = function (value) {
      return A2($VirtualDom.attribute,
      "zoomAndPan",
      value);
   };
   var z = function (value) {
      return A2($VirtualDom.attribute,
      "z",
      value);
   };
   var yChannelSelector = function (value) {
      return A2($VirtualDom.attribute,
      "yChannelSelector",
      value);
   };
   var y2 = function (value) {
      return A2($VirtualDom.attribute,
      "y2",
      value);
   };
   var y1 = function (value) {
      return A2($VirtualDom.attribute,
      "y1",
      value);
   };
   var y = function (value) {
      return A2($VirtualDom.attribute,
      "y",
      value);
   };
   var xmlSpace = function (value) {
      return A2($VirtualDom.attribute,
      "xml:space",
      value);
   };
   var xmlLang = function (value) {
      return A2($VirtualDom.attribute,
      "xml:lang",
      value);
   };
   var xmlBase = function (value) {
      return A2($VirtualDom.attribute,
      "xml:base",
      value);
   };
   var xlinkType = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:type",
      value);
   };
   var xlinkTitle = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:title",
      value);
   };
   var xlinkShow = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:show",
      value);
   };
   var xlinkRole = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:role",
      value);
   };
   var xlinkHref = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:href",
      value);
   };
   var xlinkArcrole = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:arcrole",
      value);
   };
   var xlinkActuate = function (value) {
      return A2($VirtualDom.attribute,
      "xlink:actuate",
      value);
   };
   var xChannelSelector = function (value) {
      return A2($VirtualDom.attribute,
      "xChannelSelector",
      value);
   };
   var x2 = function (value) {
      return A2($VirtualDom.attribute,
      "x2",
      value);
   };
   var x1 = function (value) {
      return A2($VirtualDom.attribute,
      "x1",
      value);
   };
   var xHeight = function (value) {
      return A2($VirtualDom.attribute,
      "x-height",
      value);
   };
   var x = function (value) {
      return A2($VirtualDom.attribute,
      "x",
      value);
   };
   var widths = function (value) {
      return A2($VirtualDom.attribute,
      "widths",
      value);
   };
   var width = function (value) {
      return A2($VirtualDom.attribute,
      "width",
      value);
   };
   var viewTarget = function (value) {
      return A2($VirtualDom.attribute,
      "viewTarget",
      value);
   };
   var viewBox = function (value) {
      return A2($VirtualDom.attribute,
      "viewBox",
      value);
   };
   var vertOriginY = function (value) {
      return A2($VirtualDom.attribute,
      "vert-origin-y",
      value);
   };
   var vertOriginX = function (value) {
      return A2($VirtualDom.attribute,
      "vert-origin-x",
      value);
   };
   var vertAdvY = function (value) {
      return A2($VirtualDom.attribute,
      "vert-adv-y",
      value);
   };
   var version = function (value) {
      return A2($VirtualDom.attribute,
      "version",
      value);
   };
   var values = function (value) {
      return A2($VirtualDom.attribute,
      "values",
      value);
   };
   var vMathematical = function (value) {
      return A2($VirtualDom.attribute,
      "v-mathematical",
      value);
   };
   var vIdeographic = function (value) {
      return A2($VirtualDom.attribute,
      "v-ideographic",
      value);
   };
   var vHanging = function (value) {
      return A2($VirtualDom.attribute,
      "v-hanging",
      value);
   };
   var vAlphabetic = function (value) {
      return A2($VirtualDom.attribute,
      "v-alphabetic",
      value);
   };
   var unitsPerEm = function (value) {
      return A2($VirtualDom.attribute,
      "units-per-em",
      value);
   };
   var unicodeRange = function (value) {
      return A2($VirtualDom.attribute,
      "unicode-range",
      value);
   };
   var unicode = function (value) {
      return A2($VirtualDom.attribute,
      "unicode",
      value);
   };
   var underlineThickness = function (value) {
      return A2($VirtualDom.attribute,
      "underline-thickness",
      value);
   };
   var underlinePosition = function (value) {
      return A2($VirtualDom.attribute,
      "underline-position",
      value);
   };
   var u2 = function (value) {
      return A2($VirtualDom.attribute,
      "u2",
      value);
   };
   var u1 = function (value) {
      return A2($VirtualDom.attribute,
      "u1",
      value);
   };
   var type$ = function (value) {
      return A2($VirtualDom.attribute,
      "type",
      value);
   };
   var transform = function (value) {
      return A2($VirtualDom.attribute,
      "transform",
      value);
   };
   var to = function (value) {
      return A2($VirtualDom.attribute,
      "to",
      value);
   };
   var title = function (value) {
      return A2($VirtualDom.attribute,
      "title",
      value);
   };
   var textLength = function (value) {
      return A2($VirtualDom.attribute,
      "textLength",
      value);
   };
   var targetY = function (value) {
      return A2($VirtualDom.attribute,
      "targetY",
      value);
   };
   var targetX = function (value) {
      return A2($VirtualDom.attribute,
      "targetX",
      value);
   };
   var target = function (value) {
      return A2($VirtualDom.attribute,
      "target",
      value);
   };
   var tableValues = function (value) {
      return A2($VirtualDom.attribute,
      "tableValues",
      value);
   };
   var systemLanguage = function (value) {
      return A2($VirtualDom.attribute,
      "systemLanguage",
      value);
   };
   var surfaceScale = function (value) {
      return A2($VirtualDom.attribute,
      "surfaceScale",
      value);
   };
   var style = function (value) {
      return A2($VirtualDom.attribute,
      "style",
      value);
   };
   var string = function (value) {
      return A2($VirtualDom.attribute,
      "string",
      value);
   };
   var strikethroughThickness = function (value) {
      return A2($VirtualDom.attribute,
      "strikethrough-thickness",
      value);
   };
   var strikethroughPosition = function (value) {
      return A2($VirtualDom.attribute,
      "strikethrough-position",
      value);
   };
   var stitchTiles = function (value) {
      return A2($VirtualDom.attribute,
      "stitchTiles",
      value);
   };
   var stemv = function (value) {
      return A2($VirtualDom.attribute,
      "stemv",
      value);
   };
   var stemh = function (value) {
      return A2($VirtualDom.attribute,
      "stemh",
      value);
   };
   var stdDeviation = function (value) {
      return A2($VirtualDom.attribute,
      "stdDeviation",
      value);
   };
   var startOffset = function (value) {
      return A2($VirtualDom.attribute,
      "startOffset",
      value);
   };
   var spreadMethod = function (value) {
      return A2($VirtualDom.attribute,
      "spreadMethod",
      value);
   };
   var speed = function (value) {
      return A2($VirtualDom.attribute,
      "speed",
      value);
   };
   var specularExponent = function (value) {
      return A2($VirtualDom.attribute,
      "specularExponent",
      value);
   };
   var specularConstant = function (value) {
      return A2($VirtualDom.attribute,
      "specularConstant",
      value);
   };
   var spacing = function (value) {
      return A2($VirtualDom.attribute,
      "spacing",
      value);
   };
   var slope = function (value) {
      return A2($VirtualDom.attribute,
      "slope",
      value);
   };
   var seed = function (value) {
      return A2($VirtualDom.attribute,
      "seed",
      value);
   };
   var scale = function (value) {
      return A2($VirtualDom.attribute,
      "scale",
      value);
   };
   var ry = function (value) {
      return A2($VirtualDom.attribute,
      "ry",
      value);
   };
   var rx = function (value) {
      return A2($VirtualDom.attribute,
      "rx",
      value);
   };
   var rotate = function (value) {
      return A2($VirtualDom.attribute,
      "rotate",
      value);
   };
   var result = function (value) {
      return A2($VirtualDom.attribute,
      "result",
      value);
   };
   var restart = function (value) {
      return A2($VirtualDom.attribute,
      "restart",
      value);
   };
   var requiredFeatures = function (value) {
      return A2($VirtualDom.attribute,
      "requiredFeatures",
      value);
   };
   var requiredExtensions = function (value) {
      return A2($VirtualDom.attribute,
      "requiredExtensions",
      value);
   };
   var repeatDur = function (value) {
      return A2($VirtualDom.attribute,
      "repeatDur",
      value);
   };
   var repeatCount = function (value) {
      return A2($VirtualDom.attribute,
      "repeatCount",
      value);
   };
   var renderingIntent = function (value) {
      return A2($VirtualDom.attribute,
      "rendering-intent",
      value);
   };
   var refY = function (value) {
      return A2($VirtualDom.attribute,
      "refY",
      value);
   };
   var refX = function (value) {
      return A2($VirtualDom.attribute,
      "refX",
      value);
   };
   var radius = function (value) {
      return A2($VirtualDom.attribute,
      "radius",
      value);
   };
   var r = function (value) {
      return A2($VirtualDom.attribute,
      "r",
      value);
   };
   var primitiveUnits = function (value) {
      return A2($VirtualDom.attribute,
      "primitiveUnits",
      value);
   };
   var preserveAspectRatio = function (value) {
      return A2($VirtualDom.attribute,
      "preserveAspectRatio",
      value);
   };
   var preserveAlpha = function (value) {
      return A2($VirtualDom.attribute,
      "preserveAlpha",
      value);
   };
   var pointsAtZ = function (value) {
      return A2($VirtualDom.attribute,
      "pointsAtZ",
      value);
   };
   var pointsAtY = function (value) {
      return A2($VirtualDom.attribute,
      "pointsAtY",
      value);
   };
   var pointsAtX = function (value) {
      return A2($VirtualDom.attribute,
      "pointsAtX",
      value);
   };
   var points = function (value) {
      return A2($VirtualDom.attribute,
      "points",
      value);
   };
   var pointOrder = function (value) {
      return A2($VirtualDom.attribute,
      "point-order",
      value);
   };
   var patternUnits = function (value) {
      return A2($VirtualDom.attribute,
      "patternUnits",
      value);
   };
   var patternTransform = function (value) {
      return A2($VirtualDom.attribute,
      "patternTransform",
      value);
   };
   var patternContentUnits = function (value) {
      return A2($VirtualDom.attribute,
      "patternContentUnits",
      value);
   };
   var pathLength = function (value) {
      return A2($VirtualDom.attribute,
      "pathLength",
      value);
   };
   var path = function (value) {
      return A2($VirtualDom.attribute,
      "path",
      value);
   };
   var panose1 = function (value) {
      return A2($VirtualDom.attribute,
      "panose-1",
      value);
   };
   var overlineThickness = function (value) {
      return A2($VirtualDom.attribute,
      "overline-thickness",
      value);
   };
   var overlinePosition = function (value) {
      return A2($VirtualDom.attribute,
      "overline-position",
      value);
   };
   var origin = function (value) {
      return A2($VirtualDom.attribute,
      "origin",
      value);
   };
   var orientation = function (value) {
      return A2($VirtualDom.attribute,
      "orientation",
      value);
   };
   var orient = function (value) {
      return A2($VirtualDom.attribute,
      "orient",
      value);
   };
   var order = function (value) {
      return A2($VirtualDom.attribute,
      "order",
      value);
   };
   var operator = function (value) {
      return A2($VirtualDom.attribute,
      "operator",
      value);
   };
   var offset = function (value) {
      return A2($VirtualDom.attribute,
      "offset",
      value);
   };
   var numOctaves = function (value) {
      return A2($VirtualDom.attribute,
      "numOctaves",
      value);
   };
   var name = function (value) {
      return A2($VirtualDom.attribute,
      "name",
      value);
   };
   var mode = function (value) {
      return A2($VirtualDom.attribute,
      "mode",
      value);
   };
   var min = function (value) {
      return A2($VirtualDom.attribute,
      "min",
      value);
   };
   var method = function (value) {
      return A2($VirtualDom.attribute,
      "method",
      value);
   };
   var media = function (value) {
      return A2($VirtualDom.attribute,
      "media",
      value);
   };
   var max = function (value) {
      return A2($VirtualDom.attribute,
      "max",
      value);
   };
   var mathematical = function (value) {
      return A2($VirtualDom.attribute,
      "mathematical",
      value);
   };
   var maskUnits = function (value) {
      return A2($VirtualDom.attribute,
      "maskUnits",
      value);
   };
   var maskContentUnits = function (value) {
      return A2($VirtualDom.attribute,
      "maskContentUnits",
      value);
   };
   var markerWidth = function (value) {
      return A2($VirtualDom.attribute,
      "markerWidth",
      value);
   };
   var markerUnits = function (value) {
      return A2($VirtualDom.attribute,
      "markerUnits",
      value);
   };
   var markerHeight = function (value) {
      return A2($VirtualDom.attribute,
      "markerHeight",
      value);
   };
   var local = function (value) {
      return A2($VirtualDom.attribute,
      "local",
      value);
   };
   var limitingConeAngle = function (value) {
      return A2($VirtualDom.attribute,
      "limitingConeAngle",
      value);
   };
   var lengthAdjust = function (value) {
      return A2($VirtualDom.attribute,
      "lengthAdjust",
      value);
   };
   var lang = function (value) {
      return A2($VirtualDom.attribute,
      "lang",
      value);
   };
   var keyTimes = function (value) {
      return A2($VirtualDom.attribute,
      "keyTimes",
      value);
   };
   var keySplines = function (value) {
      return A2($VirtualDom.attribute,
      "keySplines",
      value);
   };
   var keyPoints = function (value) {
      return A2($VirtualDom.attribute,
      "keyPoints",
      value);
   };
   var kernelUnitLength = function (value) {
      return A2($VirtualDom.attribute,
      "kernelUnitLength",
      value);
   };
   var kernelMatrix = function (value) {
      return A2($VirtualDom.attribute,
      "kernelMatrix",
      value);
   };
   var k4 = function (value) {
      return A2($VirtualDom.attribute,
      "k4",
      value);
   };
   var k3 = function (value) {
      return A2($VirtualDom.attribute,
      "k3",
      value);
   };
   var k2 = function (value) {
      return A2($VirtualDom.attribute,
      "k2",
      value);
   };
   var k1 = function (value) {
      return A2($VirtualDom.attribute,
      "k1",
      value);
   };
   var k = function (value) {
      return A2($VirtualDom.attribute,
      "k",
      value);
   };
   var intercept = function (value) {
      return A2($VirtualDom.attribute,
      "intercept",
      value);
   };
   var in2 = function (value) {
      return A2($VirtualDom.attribute,
      "in2",
      value);
   };
   var in$ = function (value) {
      return A2($VirtualDom.attribute,
      "in",
      value);
   };
   var ideographic = function (value) {
      return A2($VirtualDom.attribute,
      "ideographic",
      value);
   };
   var id = function (value) {
      return A2($VirtualDom.attribute,
      "id",
      value);
   };
   var horizOriginY = function (value) {
      return A2($VirtualDom.attribute,
      "horiz-origin-y",
      value);
   };
   var horizOriginX = function (value) {
      return A2($VirtualDom.attribute,
      "horiz-origin-x",
      value);
   };
   var horizAdvX = function (value) {
      return A2($VirtualDom.attribute,
      "horiz-adv-x",
      value);
   };
   var height = function (value) {
      return A2($VirtualDom.attribute,
      "height",
      value);
   };
   var hanging = function (value) {
      return A2($VirtualDom.attribute,
      "hanging",
      value);
   };
   var gradientUnits = function (value) {
      return A2($VirtualDom.attribute,
      "gradientUnits",
      value);
   };
   var gradientTransform = function (value) {
      return A2($VirtualDom.attribute,
      "gradientTransform",
      value);
   };
   var glyphRef = function (value) {
      return A2($VirtualDom.attribute,
      "glyphRef",
      value);
   };
   var glyphName = function (value) {
      return A2($VirtualDom.attribute,
      "glyph-name",
      value);
   };
   var g2 = function (value) {
      return A2($VirtualDom.attribute,
      "g2",
      value);
   };
   var g1 = function (value) {
      return A2($VirtualDom.attribute,
      "g1",
      value);
   };
   var fy = function (value) {
      return A2($VirtualDom.attribute,
      "fy",
      value);
   };
   var fx = function (value) {
      return A2($VirtualDom.attribute,
      "fx",
      value);
   };
   var from = function (value) {
      return A2($VirtualDom.attribute,
      "from",
      value);
   };
   var format = function (value) {
      return A2($VirtualDom.attribute,
      "format",
      value);
   };
   var filterUnits = function (value) {
      return A2($VirtualDom.attribute,
      "filterUnits",
      value);
   };
   var filterRes = function (value) {
      return A2($VirtualDom.attribute,
      "filterRes",
      value);
   };
   var externalResourcesRequired = function (value) {
      return A2($VirtualDom.attribute,
      "externalResourcesRequired",
      value);
   };
   var exponent = function (value) {
      return A2($VirtualDom.attribute,
      "exponent",
      value);
   };
   var end = function (value) {
      return A2($VirtualDom.attribute,
      "end",
      value);
   };
   var elevation = function (value) {
      return A2($VirtualDom.attribute,
      "elevation",
      value);
   };
   var edgeMode = function (value) {
      return A2($VirtualDom.attribute,
      "edgeMode",
      value);
   };
   var dy = function (value) {
      return A2($VirtualDom.attribute,
      "dy",
      value);
   };
   var dx = function (value) {
      return A2($VirtualDom.attribute,
      "dx",
      value);
   };
   var dur = function (value) {
      return A2($VirtualDom.attribute,
      "dur",
      value);
   };
   var divisor = function (value) {
      return A2($VirtualDom.attribute,
      "divisor",
      value);
   };
   var diffuseConstant = function (value) {
      return A2($VirtualDom.attribute,
      "diffuseConstant",
      value);
   };
   var descent = function (value) {
      return A2($VirtualDom.attribute,
      "descent",
      value);
   };
   var decelerate = function (value) {
      return A2($VirtualDom.attribute,
      "decelerate",
      value);
   };
   var d = function (value) {
      return A2($VirtualDom.attribute,
      "d",
      value);
   };
   var cy = function (value) {
      return A2($VirtualDom.attribute,
      "cy",
      value);
   };
   var cx = function (value) {
      return A2($VirtualDom.attribute,
      "cx",
      value);
   };
   var contentStyleType = function (value) {
      return A2($VirtualDom.attribute,
      "contentStyleType",
      value);
   };
   var contentScriptType = function (value) {
      return A2($VirtualDom.attribute,
      "contentScriptType",
      value);
   };
   var clipPathUnits = function (value) {
      return A2($VirtualDom.attribute,
      "clipPathUnits",
      value);
   };
   var $class = function (value) {
      return A2($VirtualDom.attribute,
      "class",
      value);
   };
   var capHeight = function (value) {
      return A2($VirtualDom.attribute,
      "cap-height",
      value);
   };
   var calcMode = function (value) {
      return A2($VirtualDom.attribute,
      "calcMode",
      value);
   };
   var by = function (value) {
      return A2($VirtualDom.attribute,
      "by",
      value);
   };
   var bias = function (value) {
      return A2($VirtualDom.attribute,
      "bias",
      value);
   };
   var begin = function (value) {
      return A2($VirtualDom.attribute,
      "begin",
      value);
   };
   var bbox = function (value) {
      return A2($VirtualDom.attribute,
      "bbox",
      value);
   };
   var baseProfile = function (value) {
      return A2($VirtualDom.attribute,
      "baseProfile",
      value);
   };
   var baseFrequency = function (value) {
      return A2($VirtualDom.attribute,
      "baseFrequency",
      value);
   };
   var azimuth = function (value) {
      return A2($VirtualDom.attribute,
      "azimuth",
      value);
   };
   var autoReverse = function (value) {
      return A2($VirtualDom.attribute,
      "autoReverse",
      value);
   };
   var attributeType = function (value) {
      return A2($VirtualDom.attribute,
      "attributeType",
      value);
   };
   var attributeName = function (value) {
      return A2($VirtualDom.attribute,
      "attributeName",
      value);
   };
   var ascent = function (value) {
      return A2($VirtualDom.attribute,
      "ascent",
      value);
   };
   var arabicForm = function (value) {
      return A2($VirtualDom.attribute,
      "arabic-form",
      value);
   };
   var amplitude = function (value) {
      return A2($VirtualDom.attribute,
      "amplitude",
      value);
   };
   var allowReorder = function (value) {
      return A2($VirtualDom.attribute,
      "allowReorder",
      value);
   };
   var alphabetic = function (value) {
      return A2($VirtualDom.attribute,
      "alphabetic",
      value);
   };
   var additive = function (value) {
      return A2($VirtualDom.attribute,
      "additive",
      value);
   };
   var accumulate = function (value) {
      return A2($VirtualDom.attribute,
      "accumulate",
      value);
   };
   var accelerate = function (value) {
      return A2($VirtualDom.attribute,
      "accelerate",
      value);
   };
   var accentHeight = function (value) {
      return A2($VirtualDom.attribute,
      "accent-height",
      value);
   };
   _elm.Svg.Attributes.values = {_op: _op
                                ,accentHeight: accentHeight
                                ,accelerate: accelerate
                                ,accumulate: accumulate
                                ,additive: additive
                                ,alphabetic: alphabetic
                                ,allowReorder: allowReorder
                                ,amplitude: amplitude
                                ,arabicForm: arabicForm
                                ,ascent: ascent
                                ,attributeName: attributeName
                                ,attributeType: attributeType
                                ,autoReverse: autoReverse
                                ,azimuth: azimuth
                                ,baseFrequency: baseFrequency
                                ,baseProfile: baseProfile
                                ,bbox: bbox
                                ,begin: begin
                                ,bias: bias
                                ,by: by
                                ,calcMode: calcMode
                                ,capHeight: capHeight
                                ,$class: $class
                                ,clipPathUnits: clipPathUnits
                                ,contentScriptType: contentScriptType
                                ,contentStyleType: contentStyleType
                                ,cx: cx
                                ,cy: cy
                                ,d: d
                                ,decelerate: decelerate
                                ,descent: descent
                                ,diffuseConstant: diffuseConstant
                                ,divisor: divisor
                                ,dur: dur
                                ,dx: dx
                                ,dy: dy
                                ,edgeMode: edgeMode
                                ,elevation: elevation
                                ,end: end
                                ,exponent: exponent
                                ,externalResourcesRequired: externalResourcesRequired
                                ,filterRes: filterRes
                                ,filterUnits: filterUnits
                                ,format: format
                                ,from: from
                                ,fx: fx
                                ,fy: fy
                                ,g1: g1
                                ,g2: g2
                                ,glyphName: glyphName
                                ,glyphRef: glyphRef
                                ,gradientTransform: gradientTransform
                                ,gradientUnits: gradientUnits
                                ,hanging: hanging
                                ,height: height
                                ,horizAdvX: horizAdvX
                                ,horizOriginX: horizOriginX
                                ,horizOriginY: horizOriginY
                                ,id: id
                                ,ideographic: ideographic
                                ,in$: in$
                                ,in2: in2
                                ,intercept: intercept
                                ,k: k
                                ,k1: k1
                                ,k2: k2
                                ,k3: k3
                                ,k4: k4
                                ,kernelMatrix: kernelMatrix
                                ,kernelUnitLength: kernelUnitLength
                                ,keyPoints: keyPoints
                                ,keySplines: keySplines
                                ,keyTimes: keyTimes
                                ,lang: lang
                                ,lengthAdjust: lengthAdjust
                                ,limitingConeAngle: limitingConeAngle
                                ,local: local
                                ,markerHeight: markerHeight
                                ,markerUnits: markerUnits
                                ,markerWidth: markerWidth
                                ,maskContentUnits: maskContentUnits
                                ,maskUnits: maskUnits
                                ,mathematical: mathematical
                                ,max: max
                                ,media: media
                                ,method: method
                                ,min: min
                                ,mode: mode
                                ,name: name
                                ,numOctaves: numOctaves
                                ,offset: offset
                                ,operator: operator
                                ,order: order
                                ,orient: orient
                                ,orientation: orientation
                                ,origin: origin
                                ,overlinePosition: overlinePosition
                                ,overlineThickness: overlineThickness
                                ,panose1: panose1
                                ,path: path
                                ,pathLength: pathLength
                                ,patternContentUnits: patternContentUnits
                                ,patternTransform: patternTransform
                                ,patternUnits: patternUnits
                                ,pointOrder: pointOrder
                                ,points: points
                                ,pointsAtX: pointsAtX
                                ,pointsAtY: pointsAtY
                                ,pointsAtZ: pointsAtZ
                                ,preserveAlpha: preserveAlpha
                                ,preserveAspectRatio: preserveAspectRatio
                                ,primitiveUnits: primitiveUnits
                                ,r: r
                                ,radius: radius
                                ,refX: refX
                                ,refY: refY
                                ,renderingIntent: renderingIntent
                                ,repeatCount: repeatCount
                                ,repeatDur: repeatDur
                                ,requiredExtensions: requiredExtensions
                                ,requiredFeatures: requiredFeatures
                                ,restart: restart
                                ,result: result
                                ,rotate: rotate
                                ,rx: rx
                                ,ry: ry
                                ,scale: scale
                                ,seed: seed
                                ,slope: slope
                                ,spacing: spacing
                                ,specularConstant: specularConstant
                                ,specularExponent: specularExponent
                                ,speed: speed
                                ,spreadMethod: spreadMethod
                                ,startOffset: startOffset
                                ,stdDeviation: stdDeviation
                                ,stemh: stemh
                                ,stemv: stemv
                                ,stitchTiles: stitchTiles
                                ,strikethroughPosition: strikethroughPosition
                                ,strikethroughThickness: strikethroughThickness
                                ,string: string
                                ,style: style
                                ,surfaceScale: surfaceScale
                                ,systemLanguage: systemLanguage
                                ,tableValues: tableValues
                                ,target: target
                                ,targetX: targetX
                                ,targetY: targetY
                                ,textLength: textLength
                                ,title: title
                                ,to: to
                                ,transform: transform
                                ,type$: type$
                                ,u1: u1
                                ,u2: u2
                                ,underlinePosition: underlinePosition
                                ,underlineThickness: underlineThickness
                                ,unicode: unicode
                                ,unicodeRange: unicodeRange
                                ,unitsPerEm: unitsPerEm
                                ,vAlphabetic: vAlphabetic
                                ,vHanging: vHanging
                                ,vIdeographic: vIdeographic
                                ,vMathematical: vMathematical
                                ,values: values
                                ,version: version
                                ,vertAdvY: vertAdvY
                                ,vertOriginX: vertOriginX
                                ,vertOriginY: vertOriginY
                                ,viewBox: viewBox
                                ,viewTarget: viewTarget
                                ,width: width
                                ,widths: widths
                                ,x: x
                                ,xHeight: xHeight
                                ,x1: x1
                                ,x2: x2
                                ,xChannelSelector: xChannelSelector
                                ,xlinkActuate: xlinkActuate
                                ,xlinkArcrole: xlinkArcrole
                                ,xlinkHref: xlinkHref
                                ,xlinkRole: xlinkRole
                                ,xlinkShow: xlinkShow
                                ,xlinkTitle: xlinkTitle
                                ,xlinkType: xlinkType
                                ,xmlBase: xmlBase
                                ,xmlLang: xmlLang
                                ,xmlSpace: xmlSpace
                                ,y: y
                                ,y1: y1
                                ,y2: y2
                                ,yChannelSelector: yChannelSelector
                                ,z: z
                                ,zoomAndPan: zoomAndPan
                                ,alignmentBaseline: alignmentBaseline
                                ,baselineShift: baselineShift
                                ,clipPath: clipPath
                                ,clipRule: clipRule
                                ,clip: clip
                                ,colorInterpolationFilters: colorInterpolationFilters
                                ,colorInterpolation: colorInterpolation
                                ,colorProfile: colorProfile
                                ,colorRendering: colorRendering
                                ,color: color
                                ,cursor: cursor
                                ,direction: direction
                                ,display: display
                                ,dominantBaseline: dominantBaseline
                                ,enableBackground: enableBackground
                                ,fillOpacity: fillOpacity
                                ,fillRule: fillRule
                                ,fill: fill
                                ,filter: filter
                                ,floodColor: floodColor
                                ,floodOpacity: floodOpacity
                                ,fontFamily: fontFamily
                                ,fontSizeAdjust: fontSizeAdjust
                                ,fontSize: fontSize
                                ,fontStretch: fontStretch
                                ,fontStyle: fontStyle
                                ,fontVariant: fontVariant
                                ,fontWeight: fontWeight
                                ,glyphOrientationHorizontal: glyphOrientationHorizontal
                                ,glyphOrientationVertical: glyphOrientationVertical
                                ,imageRendering: imageRendering
                                ,kerning: kerning
                                ,letterSpacing: letterSpacing
                                ,lightingColor: lightingColor
                                ,markerEnd: markerEnd
                                ,markerMid: markerMid
                                ,markerStart: markerStart
                                ,mask: mask
                                ,opacity: opacity
                                ,overflow: overflow
                                ,pointerEvents: pointerEvents
                                ,shapeRendering: shapeRendering
                                ,stopColor: stopColor
                                ,stopOpacity: stopOpacity
                                ,strokeDasharray: strokeDasharray
                                ,strokeDashoffset: strokeDashoffset
                                ,strokeLinecap: strokeLinecap
                                ,strokeLinejoin: strokeLinejoin
                                ,strokeMiterlimit: strokeMiterlimit
                                ,strokeOpacity: strokeOpacity
                                ,strokeWidth: strokeWidth
                                ,stroke: stroke
                                ,textAnchor: textAnchor
                                ,textDecoration: textDecoration
                                ,textRendering: textRendering
                                ,unicodeBidi: unicodeBidi
                                ,visibility: visibility
                                ,wordSpacing: wordSpacing
                                ,writingMode: writingMode};
   return _elm.Svg.Attributes.values;
};
Elm.Task = Elm.Task || {};
Elm.Task.make = function (_elm) {
   "use strict";
   _elm.Task = _elm.Task || {};
   if (_elm.Task.values)
   return _elm.Task.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Task",
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Task = Elm.Native.Task.make(_elm),
   $Result = Elm.Result.make(_elm);
   var sleep = $Native$Task.sleep;
   var spawn = $Native$Task.spawn;
   var ThreadID = function (a) {
      return {ctor: "ThreadID"
             ,_0: a};
   };
   var onError = $Native$Task.catch_;
   var andThen = $Native$Task.andThen;
   var fail = $Native$Task.fail;
   var mapError = F2(function (f,
   promise) {
      return A2(onError,
      promise,
      function (err) {
         return fail(f(err));
      });
   });
   var succeed = $Native$Task.succeed;
   var map = F2(function (func,
   promiseA) {
      return A2(andThen,
      promiseA,
      function (a) {
         return succeed(func(a));
      });
   });
   var map2 = F3(function (func,
   promiseA,
   promiseB) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return succeed(A2(func,a,b));
         });
      });
   });
   var map3 = F4(function (func,
   promiseA,
   promiseB,
   promiseC) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return succeed(A3(func,
               a,
               b,
               c));
            });
         });
      });
   });
   var map4 = F5(function (func,
   promiseA,
   promiseB,
   promiseC,
   promiseD) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return A2(andThen,
               promiseD,
               function (d) {
                  return succeed(A4(func,
                  a,
                  b,
                  c,
                  d));
               });
            });
         });
      });
   });
   var map5 = F6(function (func,
   promiseA,
   promiseB,
   promiseC,
   promiseD,
   promiseE) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return A2(andThen,
               promiseD,
               function (d) {
                  return A2(andThen,
                  promiseE,
                  function (e) {
                     return succeed(A5(func,
                     a,
                     b,
                     c,
                     d,
                     e));
                  });
               });
            });
         });
      });
   });
   var andMap = F2(function (promiseFunc,
   promiseValue) {
      return A2(andThen,
      promiseFunc,
      function (func) {
         return A2(andThen,
         promiseValue,
         function (value) {
            return succeed(func(value));
         });
      });
   });
   var sequence = function (promises) {
      return function () {
         switch (promises.ctor)
         {case "::": return A3(map2,
              F2(function (x,y) {
                 return A2($List._op["::"],
                 x,
                 y);
              }),
              promises._0,
              sequence(promises._1));
            case "[]":
            return succeed(_L.fromArray([]));}
         _U.badCase($moduleName,
         "between lines 101 and 106");
      }();
   };
   var toMaybe = function (task) {
      return A2(onError,
      A2(map,$Maybe.Just,task),
      function (_v3) {
         return function () {
            return succeed($Maybe.Nothing);
         }();
      });
   };
   var fromMaybe = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return succeed(maybe._0);
            case "Nothing":
            return fail($default);}
         _U.badCase($moduleName,
         "between lines 139 and 141");
      }();
   });
   var toResult = function (task) {
      return A2(onError,
      A2(map,$Result.Ok,task),
      function (msg) {
         return succeed($Result.Err(msg));
      });
   };
   var fromResult = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return fail(result._0);
            case "Ok":
            return succeed(result._0);}
         _U.badCase($moduleName,
         "between lines 151 and 153");
      }();
   };
   var Task = {ctor: "Task"};
   _elm.Task.values = {_op: _op
                      ,succeed: succeed
                      ,fail: fail
                      ,map: map
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,andMap: andMap
                      ,sequence: sequence
                      ,andThen: andThen
                      ,onError: onError
                      ,mapError: mapError
                      ,toMaybe: toMaybe
                      ,fromMaybe: fromMaybe
                      ,toResult: toResult
                      ,fromResult: fromResult
                      ,spawn: spawn
                      ,sleep: sleep};
   return _elm.Task.values;
};
Elm.Task = Elm.Task || {};
Elm.Task.Extra = Elm.Task.Extra || {};
Elm.Task.Extra.make = function (_elm) {
   "use strict";
   _elm.Task = _elm.Task || {};
   _elm.Task.Extra = _elm.Task.Extra || {};
   if (_elm.Task.Extra.values)
   return _elm.Task.Extra.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Task.Extra",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Time = Elm.Time.make(_elm);
   var computeLazyAsync = F2(function (address,
   lazy) {
      return A2($Task.andThen,
      $Task.spawn(A2($Task.andThen,
      $Task.succeed(lazy),
      function (f) {
         return A2($Task.andThen,
         $Task.succeed(f({ctor: "_Tuple0"})),
         function (value) {
            return A2($Signal.send,
            address,
            value);
         });
      })),
      function (_v0) {
         return function () {
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      });
   });
   var interceptError = F2(function (failAddress,
   task) {
      return A2($Task.onError,
      task,
      function (error) {
         return A2($Task.andThen,
         A2($Signal.send,
         failAddress,
         error),
         function (_v2) {
            return function () {
               return $Task.fail(error);
            }();
         });
      });
   });
   var interceptSuccess = F2(function (successAddress,
   task) {
      return A2($Task.andThen,
      task,
      function (value) {
         return A2($Task.andThen,
         A2($Signal.send,
         successAddress,
         value),
         function (_v4) {
            return function () {
               return $Task.succeed(value);
            }();
         });
      });
   });
   var intercept = F2(function (address,
   task) {
      return A2($Task.andThen,
      A2($Task.onError,
      task,
      function (error) {
         return A2($Task.andThen,
         A2($Signal.send,
         address,
         $Result.Err(error)),
         function (_v6) {
            return function () {
               return $Task.fail(error);
            }();
         });
      }),
      function (value) {
         return A2($Task.andThen,
         A2($Signal.send,
         address,
         $Result.Ok(value)),
         function (_v8) {
            return function () {
               return $Task.succeed(value);
            }();
         });
      });
   });
   var delay = F2(function (time,
   task) {
      return A2($Task.andThen,
      $Task.sleep(time),
      function (_v10) {
         return function () {
            return task;
         }();
      });
   });
   var loop = F2(function (every,
   task) {
      return A2($Task.andThen,
      task,
      function (_v12) {
         return function () {
            return A2($Task.andThen,
            $Task.sleep(every),
            function (_v14) {
               return function () {
                  return A2(loop,every,task);
               }();
            });
         }();
      });
   });
   var optional = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return A2($Task.onError,
              A2($Task.andThen,
              list._0,
              function (value) {
                 return A2($Task.map,
                 F2(function (x,y) {
                    return A2($List._op["::"],
                    x,
                    y);
                 })(value),
                 optional(list._1));
              }),
              function (_v19) {
                 return function () {
                    return optional(list._1);
                 }();
              });
            case "[]":
            return $Task.succeed(_L.fromArray([]));}
         _U.badCase($moduleName,
         "between lines 53 and 57");
      }();
   };
   var parallel = function (tasks) {
      return $Task.sequence(A2($List.map,
      $Task.spawn,
      tasks));
   };
   var broadcast = F2(function (addresses,
   value) {
      return A2($Task.andThen,
      parallel(A2($List.map,
      function (address) {
         return A2($Signal.send,
         address,
         value);
      },
      addresses)),
      function (_v21) {
         return function () {
            return $Task.succeed({ctor: "_Tuple0"});
         }();
      });
   });
   _elm.Task.Extra.values = {_op: _op
                            ,parallel: parallel
                            ,broadcast: broadcast
                            ,optional: optional
                            ,loop: loop
                            ,delay: delay
                            ,intercept: intercept
                            ,interceptSuccess: interceptSuccess
                            ,interceptError: interceptError
                            ,computeLazyAsync: computeLazyAsync};
   return _elm.Task.Extra.values;
};
Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values)
   return _elm.Text.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Text",
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var append = $Native$Text.append;
   var fromString = $Native$Text.fromString;
   var empty = fromString("");
   var concat = function (texts) {
      return A3($List.foldr,
      append,
      empty,
      texts);
   };
   var join = F2(function (seperator,
   texts) {
      return concat(A2($List.intersperse,
      seperator,
      texts));
   });
   var defaultStyle = {_: {}
                      ,bold: false
                      ,color: $Color.black
                      ,height: $Maybe.Nothing
                      ,italic: false
                      ,line: $Maybe.Nothing
                      ,typeface: _L.fromArray([])};
   var Style = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,bold: d
             ,color: c
             ,height: b
             ,italic: e
             ,line: f
             ,typeface: a};
   });
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   _elm.Text.values = {_op: _op
                      ,fromString: fromString
                      ,empty: empty
                      ,append: append
                      ,concat: concat
                      ,join: join
                      ,link: link
                      ,style: style
                      ,defaultStyle: defaultStyle
                      ,typeface: typeface
                      ,monospace: monospace
                      ,height: height
                      ,color: color
                      ,bold: bold
                      ,italic: italic
                      ,line: line
                      ,Style: Style
                      ,Under: Under
                      ,Over: Over
                      ,Through: Through};
   return _elm.Text.values;
};
Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values)
   return _elm.Time.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Time",
   $Basics = Elm.Basics.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var delay = $Native$Signal.delay;
   var since = F2(function (time,
   signal) {
      return function () {
         var stop = A2($Signal.map,
         $Basics.always(-1),
         A2(delay,time,signal));
         var start = A2($Signal.map,
         $Basics.always(1),
         signal);
         var delaydiff = A3($Signal.foldp,
         F2(function (x,y) {
            return x + y;
         }),
         0,
         A2($Signal.merge,start,stop));
         return A2($Signal.map,
         F2(function (x,y) {
            return !_U.eq(x,y);
         })(0),
         delaydiff);
      }();
   });
   var timestamp = $Native$Signal.timestamp;
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = function (targetFrames) {
      return A2(fpsWhen,
      targetFrames,
      $Signal.constant(true));
   };
   var inMilliseconds = function (t) {
      return t;
   };
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {
      return t / hour;
   };
   var inMinutes = function (t) {
      return t / minute;
   };
   var inSeconds = function (t) {
      return t / second;
   };
   _elm.Time.values = {_op: _op
                      ,millisecond: millisecond
                      ,second: second
                      ,minute: minute
                      ,hour: hour
                      ,inMilliseconds: inMilliseconds
                      ,inSeconds: inSeconds
                      ,inMinutes: inMinutes
                      ,inHours: inHours
                      ,fps: fps
                      ,fpsWhen: fpsWhen
                      ,every: every
                      ,timestamp: timestamp
                      ,delay: delay
                      ,since: since};
   return _elm.Time.values;
};
Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values)
   return _elm.Transform2D.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Transform2D",
   $Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,
   y) {
      return A6(matrix,
      1,
      0,
      0,
      1,
      x,
      y);
   });
   var scale = function (s) {
      return A6(matrix,
      s,
      0,
      0,
      s,
      0,
      0);
   };
   var scaleX = function (x) {
      return A6(matrix,
      x,
      0,
      0,
      1,
      0,
      0);
   };
   var scaleY = function (y) {
      return A6(matrix,
      1,
      0,
      0,
      y,
      0,
      0);
   };
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   _elm.Transform2D.values = {_op: _op
                             ,identity: identity
                             ,matrix: matrix
                             ,multiply: multiply
                             ,rotation: rotation
                             ,translation: translation
                             ,scale: scale
                             ,scaleX: scaleX
                             ,scaleY: scaleY};
   return _elm.Transform2D.values;
};
Elm.VirtualDom = Elm.VirtualDom || {};
Elm.VirtualDom.make = function (_elm) {
   "use strict";
   _elm.VirtualDom = _elm.VirtualDom || {};
   if (_elm.VirtualDom.values)
   return _elm.VirtualDom.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "VirtualDom",
   $Basics = Elm.Basics.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$VirtualDom = Elm.Native.VirtualDom.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var lazy3 = $Native$VirtualDom.lazy3;
   var lazy2 = $Native$VirtualDom.lazy2;
   var lazy = $Native$VirtualDom.lazy;
   var on = $Native$VirtualDom.on;
   var attribute = $Native$VirtualDom.attribute;
   var property = $Native$VirtualDom.property;
   var Property = {ctor: "Property"};
   var fromElement = $Native$VirtualDom.fromElement;
   var toElement = $Native$VirtualDom.toElement;
   var text = $Native$VirtualDom.text;
   var node = $Native$VirtualDom.node;
   var Node = {ctor: "Node"};
   _elm.VirtualDom.values = {_op: _op
                            ,Node: Node
                            ,node: node
                            ,text: text
                            ,toElement: toElement
                            ,fromElement: fromElement
                            ,Property: Property
                            ,property: property
                            ,attribute: attribute
                            ,on: on
                            ,lazy: lazy
                            ,lazy2: lazy2
                            ,lazy3: lazy3};
   return _elm.VirtualDom.values;
};