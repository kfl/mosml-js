var hello = "goodbye";
var Lvar_concat = (function(){
var var_3 = "hello";
var var_5 = "world";
var var_7 = var_3+var_5;
return var_7;
}());
var Lconst_ATOMsc_INTscon = 15;
var Lconst_ATOMsc_WORDscon = F;
var Lconst_ATOMsc_CHARscon = "F";
var Lconst_ATOMsc_REALscon = 15.0;
var Lconst_ATOMsc_STRINGscon = "fifteen";
var Lconst_BLOCKsc_list = Constructor(1,[1, Constructor(1,[2, Constructor(1,[3, Constructor(1,[4, Constructor(1,[5, Constructor(0)])])])])]);
var Lconst_BLOCKsc_tuple = Constructor(0,[1, 2, 3, 4, 5]);
var Lconst_BLOCKsc_false = Constructor(0);
var Lconst_BLOCKsc_true = Constructor(1);
var Lconst_BLOCKsc_datatype_test = Constructor(0,[1, Constructor(0,[2, Constructor(0,[3, Constructor(2)])])]);
var Lfn_normal = function(var_34)
{return overflowCheck64(overflowCheck64(var_34.args[0]+1)+var_34.args[1]);};
var Lapply_normal = Lfn_normal(Constructor(0,[1, 2]));
var Lfn_curried = function(var_45)
{return function(var_46)
{return overflowCheck64(var_45+var_46);};};
var Lapply_closure1 = Lfn_curried(2);
var Lapply_closure2 = Lapply_closure1(3);
var Lapply_closure3 = Lfn_curried(2)(3);
var a = 2;
var Llet_test = (function(){
var var_66 = 1;
var var_68 = 1;
return (overflowCheck64(var_66+var_68) === 2);
}());
var Lletrec_test1 = function(var_77)
{return overflowCheck64(var_77+var_77);};
var Lletrec_test2 = function(var_82){
var var_83 = function(var_84)
{return var_84;};
return var_83(var_82);};
var Lletrec_test3 = (Lletrec_test2(2) === 2);
var Lhandle_test =  Error! ;
(function(){
var var_98 = Constructor(250,["Lhandle_exception1"]);
return var Lhandle_exception1 = var_98;
}());
(function(){
var var_104 = Constructor(250,["Lhandle_exception2"]);
return var Lhandle_exception2 = var_104;
}());
var Lhandle_fun1 = function(var_111)
{return (function(){switch(var_111.args[0]){
case 0:
return (function(){switch(var_111.args[1]){
case 0:
return  Error! 
default:
return  Error! 
}}())
default:
return  Error! 
}}());};
var Lhandle_fun2 = function(var_122)
{return  Error! ;};
var Lcase_fib_rec = function(var_125)
{return (function(){switch(var_125){
case 0:
return 1
case 1:
return 1
default:
return overflowCheck64(overflowCheck64(overflowCheck64(Lcase_fib_rec(var_125)-1)+Lcase_fib_rec(var_125))-2)
}}());};
var Lcase_fib_case = function(var_142){
var var_143 = var_142;
return (function(){switch(var_143){
case 0:
return 1
case 1:
return 1
default:
return overflowCheck64(overflowCheck64(overflowCheck64(Lcase_fib_case(var_143)-1)+Lcase_fib_case(var_143))-2)
}}());};
var Lcase_not_exhaustive = function(var_162)
{return (function(){switch(var_162){
case 1:
return 1
case 2:
return 2
default:
return  Error! 
}}());};
var Lcase_curried = function(var_169)
{return function(var_170)
{return (function(){switch(var_169){
case 1:
return (function(){switch(var_170){
case 1:
return 2
default:
return Lcase_curried(overflowCheck64(var_170-1))(1)
}}())
default:
return (function(){switch(var_170){
case 1:
return overflowCheck64(Lcase_curried(overflowCheck64(var_169-1))(1)+Lcase_curried(1)(1))
default:
return  Error! 
}}())
}}());};};
var Lcase_list = function(var_197)
{return (function(){switch(var_197.tag){
case 0:
return 0
default:
return overflowCheck64(1+Lcase_list(var_197.args[1]))
}}());};
var Lif_a = 63;
var Lif_b = (function(){ return ((Lif_a === 63) ? 10 : 20)}());
var Lif_c = (function(){ return ((Lif_b === 20) ? "a" : "b")}());
var Lif_d = Constructor(0);
var Lif_d = Constructor(1);
var Lif_e = (function(){ return (Lif_d ? 1 : 2)}());
var Lif_f = (function(){ return (!Lif_d ? 1 : 2)}());
var Lif_test = function(var_238)
{return (function(){ return ( Error!  ? 1 : 0)}());};
var Lseq_discard_evaluation = (1, 2);
var Lseq_func1 = function(var_249)
{return (function(){switch(var_249){
case 0:
return 0
default:
return overflowCheck64(overflowCheck64(1+Lseq_func2(var_249))-1)
}}());};
var Lseq_func2 = function(var_261)
{return (function(){switch(var_261){
case 0:
return 0
default:
return overflowCheck64(overflowCheck64(2*Lseq_func1(var_261))-1)
}}());};
var Lwhile_infinite = while (Constructor(1)){
1
};
var Landalso_test =  Error! ;
var Lorelse_test =  Error! ;
(function(){
var var_280 = "Lunspec";
return ;
}());
var Lshared_test = function(var_285){
var var_286 = var_285;
return (function(){switch(var_286.args[0].tag){
case 0:
return (function(){switch(var_286.args[1].tag){
case 0:
return 0
default:
return (function(){switch(var_286.args[2].tag){
case 0:
return (function(){switch(var_286.args[3].tag){
case 0:
return 1
default:
return (function(){switch(var_286.args[4].tag){
case 0:
return (function(){switch(var_286.args[5].tag){
case 0:
return 2
default:
return (function(){switch(var_286.args[6].tag){
case 0:
return (function(){switch(var_286.args[7].tag){
case 0:
return 3
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return ~1
}}())
default:
return  Error! 
}}())
}}())
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
}}())
default:
return (function(){switch(var_286.args[6].tag){
case 0:
return (function(){switch(var_286.args[7].tag){
case 0:
return 3
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
}}())
}}())
default:
return (function(){switch(var_286.args[4].tag){
case 0:
return (function(){switch(var_286.args[5].tag){
case 0:
return 2
default:
return (function(){switch(var_286.args[6].tag){
case 0:
return (function(){switch(var_286.args[7].tag){
case 0:
return 3
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
}}())
default:
return (function(){switch(var_286.args[6].tag){
case 0:
return (function(){switch(var_286.args[7].tag){
case 0:
return 3
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
}}())
}}())
}}())
default:
return (function(){switch(var_286.args[2].tag){
case 0:
return (function(){switch(var_286.args[3].tag){
case 0:
return 1
default:
return (function(){switch(var_286.args[4].tag){
case 0:
return (function(){switch(var_286.args[5].tag){
case 0:
return 2
default:
return (function(){switch(var_286.args[6].tag){
case 0:
return (function(){switch(var_286.args[7].tag){
case 0:
return 3
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
}}())
default:
return (function(){switch(var_286.args[6].tag){
case 0:
return (function(){switch(var_286.args[7].tag){
case 0:
return 3
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
}}())
}}())
default:
return (function(){switch(var_286.args[4].tag){
case 0:
return (function(){switch(var_286.args[5].tag){
case 0:
return 2
default:
return (function(){switch(var_286.args[6].tag){
case 0:
return (function(){switch(var_286.args[7].tag){
case 0:
return 3
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
}}())
default:
return (function(){switch(var_286.args[6].tag){
case 0:
return (function(){switch(var_286.args[7].tag){
case 0:
return 3
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
default:
return (function(){switch(var_286.args[8].tag){
case 0:
return (function(){switch(var_286.args[9].tag){
case 0:
return 4
default:
return  Error! 
}}())
default:
return  Error! 
}}())
}}())
}}())
}}())
}}());};
