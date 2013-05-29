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

wordOverflow32 : function(x) {
    return x > 2147483647 ? x-2147483648 : x;
},

wordOverflow64 : function(x) {
    return x > 9223372036854775807 ? x-9223372036854775808 : x;
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
