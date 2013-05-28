"use strict"
/*  JSLib functions to handle Moscow ML situations like words and overflow,
    where defined exceptions of Moscow ML should be thrown.*/

var $_mosmllib = (function() {

/*********************/
/* PRIVATE FUNCTIONS */
/*********************/

/*  Overflow exception as defined in Moscow ML */
function OverflowException() {
   this.message = "! Uncaught exception:\n! Overflow";
   this.name = "OverflowException";
}

/*  Div (division) exception as defined in Moscow ML */
function DivException() {
    this.message = "! Uncaught exception:\n! Div";
    this.name = "DivException";
}

/* Object definition of the sml constructor */
function Constructor(tag, args){
    var instance = {}
    instance.tag = tag;
    instance.args = args;
    return instance;
}

/* Exception of division */
var exn_div = Constructor(250, ["exn_div"]);
var exn_match = Constructor(250, ["exn_match"]);
var exn_overflow = Constructor(250, ["exn_overflow"]);

/********************/
/* PUBLIC FUNCTIONS */
/********************/
return {

/* Object definition of the sml constructor */
Constructor : function(tag,args){return Constructor(tag, args)},

/* Some SML standard exceptions. */
exn_div : exn_div,
exn_match : exn_match,
exn_overflow : exn_overflow,


/*  Overflow check. Takes signed 32-bit integer.
    Throws OverflowException on overflow, else given integer. */
overflowCheck32 : function(x) {
    if(x > 1073741823 || x < -1073741824) {
        throw new Constructor(0, [exn_overflow, Constructor(0)]);
    }
    else return x;
},

/*  Overflow check. Takes signed 64-bit integer.
    Throws OverflowException on overflow, else given integer. */
overflowCheck64 : function(x) {
    if(x > 4611686018427387903 || x < -4611686018427387904) {
        throw new Constructor(0, [exn_overflow, Constructor(0)]);
    }
    else return x;
},

/*  Takes sml word on form "0wx..."
    Returns word converted to integer */
wordSmlToJS : function(x) {
    var re = /^(0wx)[0-9,A-F]+$/;
    if (re.test(x)) {
        var JSWord = x.substring(3);
        return parseInt(JSWord, 16);
    }
    else throw "NotWord";
},

/*  Takes JS word of int
    Returns 32-bit sml word on form "0wx..." */
wordJSToSml32 : function(x) {
    var JSWord = x > 2147483647 ? x-2147483648 : x
    var SmlWord = JSWord.toString(16);
    return "0wx"+SmlWord;
},

/*  Takes JS word of int
    Returns 64-bit sml word on form "0wx..." */
wordJSToSml64 : function(x) {
    var JSWord = x > 9223372036854775807 ? x-9223372036854775808 : x
    var SmlWord = JSWord.toString(16);
    return "0wx"+SmlWord;
},

/*  Takes 8-bit sml word8  on form "0wx..."
    Returns word converted to integer */
word8SmlToJS : function(x) {
    var re = /^(0wx)[0-9,A-F]+$/;
    if (re.test(x)) {
        var JSWord = x.substring(3);
        return parseInt(JSWord, 16);
    }
    else throw "NotWord8";
},

/*  Takes JS word of int
    Returns 8-bit sml word8 on form "0wx..." */
word8JSToSml : function(x) {
    var SmlWord8 = x.toString(16).substring(0,2);
    return "0wx"+SmlWord8
},

/*  Throws exception on division by 0, i.e. y==0.
    Returns integer x divided by y. */
divInt : function(x,y) {
    return (y === 0 ? (function(){throw Constructor(0, [exn_div, Constructor(0)])}()) : Math.floor(y/x));
},

division : function(x,y) {
    return (y === 0 ? (function(){throw Constructor(0, [exn_div, Constructor(0)])}()) : y/x);
},

sml_equal : function(x,y) {
    function eqArgs(x,y) {
        if(x === y){return true}
        if((typeof x === "undefined" || x.length == 0) && 
           (typeof y === "undefined" || y.length == 0)) {return true}
        if(x.length !== y.length){return false}
        for(var i = 0; i < x.length; i++){
           if(!eqTag(x[i], y[i])){return false}
        }
        return true;
    }
    function eqTag(x,y) {
        if(typeof(x) !== typeof(y)){return false}
        if(typeof(x) !== typeof(Constructor(0))){return (x===y)}
        if(x.tag === y.tag){return eqArgs(x.args,y.args)} else {return false}

    }
        return (eqTag(x,y) ? Constructor(1, []) : Constructor(0, []));
}

}}());

var ctimeBegin$1 = function($_var1) {
    return console.time($_var1);
};
var ctimeEnd$2 = function($_var5) {
    return console.timeEnd($_var5);
};
var repeat$3 = function($_var9) {
    return function($_var10) {
        return function($_var11) {
            var $_var13 = function($_var14) {
                return (function() {
                    switch ($_var14) {
                        case 0:
                            return $_var10($_var11)
                        default:
                            return ($_var10($_var11), $_var13($_var14 - 1))
                    }
                }());
            };
            return $_var13($_var9 - 1);
        };
    };
};
var timerep$4 = function($_var35) {
    return function($_var36) {
        return function($_var37) {
            return repeat$3($_var35)(function($_var41) {
                return (ctimeBegin$1($_var37), ($_var36($_var41), ctimeEnd$2($_var37)));
            });
        };
    };
};
var recAdd$5 = function($_var54) {
    var $_var56 = $_mosmllib.Constructor(250, [0.0]);
    return (function() {
        while ($_mosmllib.Constructor($_var56.args[0] < $_var54 ? 1 : 0).tag) {
            ((function() {
                $_var56.args[0] = $_var56.args[0] + 1.0
            }()), 10 + 10)
        }
    }());
};
var recSub$6 = function($_var73) {
    var $_var75 = $_mosmllib.Constructor(250, [0.0]);
    return (function() {
        while ($_mosmllib.Constructor($_var75.args[0] < $_var73 ? 1 : 0).tag) {
            ((function() {
                $_var75.args[0] = $_var75.args[0] + 1.0
            }()), 100000 - 10002)
        }
    }());
};
var it$7 = timerep$4(10)(recAdd$5)("addInt")(10000000.0);
var it$8 = timerep$4(10)(recAdd$5)("subInt")(10000000.0);

