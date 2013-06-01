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
var intAdd = function($_var54) {
    var $_var56 = $_mosmllib.Constructor(250, [0.0]);
    return (function() {
        while ($_mosmllib.Constructor($_var56.args[0] < $_var54 ? 1 : 0).tag) {
            ((function() {
                $_var56.args[0] = $_var56.args[0] + 1.0
            }()), 10 + 10)
        }
    }());
};
var intSub = function($_var73) {
    var $_var75 = $_mosmllib.Constructor(250, [0.0]);
    return (function() {
        while ($_mosmllib.Constructor($_var75.args[0] < $_var73 ? 1 : 0).tag) {
            ((function() {
                $_var75.args[0] = $_var75.args[0] + 1.0
            }()), 10- 1)
        }
    }());
};
var it$7 = timerep$4(100)(intAdd)("intAdd_native_ref")(1000000.0);
var it$8 = timerep$4(100)(intSub)("intSub_native_ref")(1000000.0);

