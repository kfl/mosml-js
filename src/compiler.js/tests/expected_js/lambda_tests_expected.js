/* Lvar */
var hello = "goodbye";
var Lvar_concat = (function(){
    var hello = "hello";
    var world = "world";
    var hw = hello+world;
    return hw;
}());

/* Lconst */
var Lconst_ATOMsc_INTscon = 15;
var Lconst_ATOMsc_WORDscon = 0x0F;
var Lconst_ATOMsc_CHARscon = "F";
var Lconst_ATOMsc_REALscon = 15.0;
var Lconst_ATOMsc_STRINGscon = "fifteen";

var Lconst_BLOCKsc_list = $_mosmllib.Constructor(1, [1, $_mosmllib.Constructor(1, [2, $_mosmllib.Constructor(1, [3, $_mosmllib.Constructor(1, [4, $_mosmllib.Constructor(1, [5, $_mosmllib.Constructor(0)])])])])]);
var Lconst_BLOCKsc_tuple = $_mosmllib.Constructor(0, [1, 2, 3, 4, 5]);
var Lconst_BLOCKsc_false = $_mosmllib.Constructor(0);
var Lconst_BLOCKsc_true = $_mosmllib.Constructor(1);

var Lconst_BLOCKsc_datatype_test = $_mosmllib.Constructor(0, [1, $_mosmllib.Constructor(0, [2, $_mosmllib.Constructor(0, [3, $_mosmllib.Constructor(2)])])]);

/* Lfn + Lapply */
var Lfn_normal = function(xy) {
    return xy.args[0] + 1 + xy.args[1];
};
var Lapply_normal = Lfn_normal($_mosmllib.Constructor(0, [1, 2]));

var Lfn_curried = function(x) {
    return function(y) {
        x + y;
    };
};
var Lapply_closure1 = Lfn_curried(2);
var Lapply_closure2 = Lapply_closure1(3);
var Lapply_closure3 = Lfn_curried(2)(3);

/* Llet */
var a = 2;
var Llet_test = (function(){
    var a = 1;
    var b = 1;
    return a+b === 2;
}());

/* Lletrec */
var Lletrec_test1 = function(x) {
    return x+x;
};
var Lletrec_test2 = function(y) {
    var Lletrec_test1 = function(x) {
        x;
    };
    return Lletrec_test1(y);
};
var Lletrec_test3 = Lletrec_test2(2) === 2;

/* Lprim */
/* TODO: Lprim */

/* Lhandle */
/* TODO: Lhandle */

/* Lstatichandle -> Lcase */
var Lcase_fib_rec = function(n) {
    return (function(){
        switch(n) {
            case 0: return 1;
            case 1: return 1;
            default: return Lcase_fib_rec(n-1) + Lcase_fib_rec(n-2);
        }());
    };
};

var Lcase_fib_case = function(n) {
    return (function(){
        switch(n) {
            case 0: return 1;
            case 1: return 1;
            default: return Lcase_fib_case(n-1) + Lcase_fib_case(n-2);
        }());
    };
};

var Lcase_not_exhaustive = function(var0) {
    return (function(){
        switch(var0) {
            case 1: return 1;
            case 2: return 2;
            default: /* TODO: Match error */
        }());
    };
};

/* Lstatichandle -> Lswitch */
var Lcase_list = function(var0) {
    return (function(){
        switch(var0.tag) {
            case 0: return 0;
            default: return 1 + Lcase_list(var0.args[1]);
        }());
    };
};

/* Lif */
var Lif_a = 63;
var Lif_b = if Lif_a === 63 ? 10 : 20;
var Lif_c = if Lif_b === 20 ? "a" : "b";

var Lif_d = $_mosmllib.Constructor(0);
var Lif_d = $_mosmllib.Constructor(1);
var Lif_e = Lif_d.tag ? 1 : 2;
var Lif_f = !Lif_d.tag ? 1 : 2;

var Lif_test = function(x) {
    return x > 0 ? 1 : 2;
};

/* Lseq */
/* TODO: Lseq */
var Lseq_discard_evaluation = (1, 2);

var Lseq_func1 = function(x) {
    return (function(){
        switch(x) {
            case 0: return 0;
            default: return 1 + Lseq_func2(x-1);
        }());
    };
};

var Lseq_func2 = function(x) {
    return (function(){
        switch(x) {
            case 0: return 0;
            default: return 2 * Lseq_func1(x-1);
        }());
    };
};

/* Lwhile */
var Lwhile_infinite = while(true) {1};

/* Landsalso + Lorelse */
var Landsalso_test = ($_mosmllib.Constructor(1).tag && $_mosmllib.Constructor(0).tag) === $_mosmllib.Constructor(0).tag;
var Lorelse_test = ($_mosmllib.Constructor(0).tag || $_mosmllib.Constructor(1).tag) === $_mosmllib.Constructor(1).tag;

/* Lunspec */
(function(){var var0 = "Lunspec"})();