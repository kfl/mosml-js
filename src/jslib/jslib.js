/*  JSLib functions to handle Moscow ML situations like words and overflow,
    where defined exceptions of Moscow ML should be thrown.*/

/* Object definition of the sml constructor */
function constructor(tag, args){
    var instance = {}
    instance.tag = tag;
    instance.args = args;
    return instance;
}

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

/*  Overflow check. Takes signed 32-bit integer.
    Throws OverflowException on overflow, else given integer. */
function overflowCheck32(x) {
    if(x > 1073741823 || x < -1073741824) {
        throw new OverflowException();
    }
    else return x;
}
/*  Overflow check. Takes signed 64-bit integer.
    Throws OverflowException on overflow, else given integer. */
function overflowCheck64(x) {
    if(x > 4611686018427387903 || x < -4611686018427387904) {
        throw new OverflowException();
    }
    else return x;
}
/*  Takes sml word on form "0wx..."
    Returns word converted to integer */
function wordSmlToJS(x) {
    var re = /^(0wx)[0-9,A-F]+$/;
    if (re.test(x)) {
        var JSWord = x.substring(3);
        return parseInt(JSWord, 16);
    }
    else throw "NotWord";
}
/*  Takes JS word of int
    Returns 32-bit sml word on form "0wx..." */
function wordJSToSml32(x) {
    var JSWord = x > 2147483647 ? x-2147483648 : x
    var SmlWord = JSWord.toString(16);
    return "0wx"+SmlWord;
}
/*  Takes JS word of int
    Returns 64-bit sml word on form "0wx..." */
function wordJSToSml64(x) {
    var JSWord = x > 9223372036854775807 ? x-9223372036854775808 : x
    var SmlWord = JSWord.toString(16);
    return "0wx"+SmlWord;
}
/*  Takes 8-bit sml word8  on form "0wx..."
    Returns word converted to integer */
function word8SmlToJS(x) {
    var re = /^(0wx)[0-9,A-F]+$/;
    if (re.test(x)) {
        var JSWord = x.substring(3);
        return parseInt(JSWord, 16);
    }
    else throw "NotWord8";
}
/*  Takes JS word of int
    Returns 8-bit sml word8 on form "0wx..." */
function word8JSToSml(x) {
    var SmlWord8 = x.toString(16).substring(0,2);
    return "0wx"+SmlWord8
}
/*  Throws exception on division by 0, i.e. y==0.
    Returns integer x divided by y. */
function divInt(x,y) {
    y == 0 ? throw new DivException() : return Math.floor(y/x);
}
function division(x,y) {
    y == 0 ? throw new DivException() : return y/x;
}
