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
var realAdd = function($_var54) {
    var i = 0;
    while (i++ < $_var54) {
        33.3334+9.992;
    }
};
var realSub = function($_var73) {
    var i = 0;
    while (i++ < $_var73) {
        33.3334-9.992;
    }
};
var it$7 = timerep$4(10)(realAdd)("realAdd_native")(10000000.0);
var it$8 = timerep$4(10)(realSub)("realSub_native")(10000000.0);


var realSubTest0 = function($_var73) {
    var i = 0;
    (function() {
        while (i++ < $_var73) {
            33.3334-9.992;
        }
    }())
};
var it$9 = timerep$4(10)(recSubTest)("realSubTest0")(10000000.0);

var realSubTest1 = function($_var73) {
    var i = 0;
    while (i++ < $_var73) {
        (function(){33.3334-9.992}())
    }
};
var it$9 = timerep$4(10)(recSubTest1)("realSubTest1")(10000000.0);

var realSubTest2 = function($_var73) {
    var i = 0;
    (function() {
        while (i++ < $_var73) {
            (function(){33.3334-9.992}())
        }
    }())
};
var it$9 = timerep$4(10)(recSubTest2)("realSubTest2")(10000000.0);






