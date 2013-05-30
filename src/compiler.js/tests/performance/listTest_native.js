"use strict"
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

var listPrepend$5 = function($_var54) {
    var i = 0;
    var listRef = [];
    while (i++ < $_var54) {
        listRef.unshift(1);
    }
};
var listAppend$10 = function($_var54) {
    var i = 0;
    var listRef = [];
    while (i++ < $_var54) {
        listRef.push(1);
    }
};

var it$6 = timerep$4(10)(listPrepend$5)("listPrepend")(100000.0);
var it$12 = timerep$4(10)(listAppend$10)("listAppend")(1000.0);
