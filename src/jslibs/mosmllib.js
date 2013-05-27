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
        if((typeof x === "undefined" || x.length == []) && 
           (typeof y === "undefined" || y.length == [])) {return true}
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
