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

var Lconst_BLOCKsc_list = new $_mosmllib.constructor(1, [1, new $_mosmllib.constructor(1, [2, new $_mosmllib.constructor(1, [3, new $_mosmllib.constructor(1, [4, new $_mosmllib.constructor(1, [5, new $_mosmllib.constructor(0, [])])])])])]);
var Lconst_BLOCKsc_tuple = new $_mosmllib.constructor(0, [1, 2, 3, 4, 5]);
var Lconst_BLOCKsc_false = new $_mosmllib.constructor(0, []);
var Lconst_BLOCKsc_true = new $_mosmllib.constructor(1, []);

var Lconst_BLOCKsc_datatype_test = new $_mosmllib.constructor(0, [1, new $_mosmllib.constructor(0, [2, new $_mosmllib.constructor(0, [3, new $_mosmllib.constructor(2, [])])])]);

/* Lfn + Lapply */
var Lfn_normal = function(xy) {
    return xy.args[0] + 1 + xy.args[1];
};
var Lapply_normal = Lfn_normal(new $_mosmllib.constructor(0, [1, 2]));

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

/* Lhandle */
/* TODO: Lhandle */

/* Lstatichandle -> Lcase */
var Lcase_fib_rec = function(n) {
    return switch(n) {
        case 0: 1;
        case 1: 1;
        default: Lcase_fib_rec(n-1) + Lcase_fib_rec(n-2);
    };
};

var Lcase_fib_case = function(n) {
    return switch(n) {
        case 0: 1;
        case 1: 1;
        default: Lcase_fib_case(n-1) + Lcase_fib_case(n-2);
    };
};

var Lcase_not_exhaustive = function(var0) {
    return switch(var0) {
        case 1: 1;
        case 2: 2;
        default: /* TODO: Match error */
    };
};

/* Lstatichandle -> Lswitch */
/* TODO: Lswitch */

/* Lif */
var Lif_a = 63;
var Lif_b = if Lif_a === 63 {10} else {20};
var Lif_c = if Lif_b === 20 {"a"} else {"b"};

var Lif_d = new $_mosmllib.constructor(0, []);
var Lif_d = new $_mosmllib.constructor(1, []);
var Lif_e = if Lif_d.tag {1} else {2};
var Lif_f = if !Lif_d.tag {1} else {2};

var Lif_test = function(x) {
    return if x > 0 {1} else {2};
};

/* Lseq */
/* TODO: Lseq */

/* Lwhile */
var Lwhile_infinite = while(true) {1};

/* Landsalso + Lorelse */
var Landsalso_test = (new $_mosmllib.constructor(1, []).tag && new $_mosmllib.constructor(0, []).tag) === new $_mosmllib.constructor(0, []).tag;
var Lorelse_test = (new $_mosmllib.constructor(0, []).tag || new $_mosmllib.constructor(1, []).tag) === new $_mosmllib.constructor(1, []).tag;

/* Lunspec */
(function(){var var0 = "Lunspec"})();