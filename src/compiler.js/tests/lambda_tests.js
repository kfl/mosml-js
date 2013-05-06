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
var Lconst_BLOCKsc_list = [1,2,3,4,5];
var Lconst_BLOCKsc_tuple = [1,2,3,4,5];
var Lconst_BLOCKsc_false = false;
var Lconst_BLOCKsc_true = true;
var Lfn_normal = function(var_32)
{return overflowCheck64(overflowCheck64(var_32[0]+1)+var_32[1]);};
var Lapply_normal = Lfn_normal([1,2]);
var Lfn_curried = function(var_43)
{return function(var_44)
{return overflowCheck64(var_43+var_44);};};
var Lapply_closure1 = Lfn_curried(2);
var Lapply_closure2 = Lapply_closure1(3);
var Lapply_closure3 = Lfn_curried(2)(3);
var a = 2;
var Llet_test = (function(){
var var_64 = 1;
var var_66 = 1;
return (overflowCheck64(var_64+var_66) === 2);
}());
var Lletrec_test1 = function(var_75)
{return overflowCheck64(var_75+var_75);};
var Lletrec_test2 = function(var_80){
var var_81 = function(var_82)
{return var_82;};
return var_81(var_80);};
var Lletrec_test3 = (Lletrec_test2(2) === 2);
var Lhandle_test =  Error! ;
var Lcase_fib_rec = function(var_97)
{return switch(var_97){
case 0:
1
case 1:
1
default:overflowCheck64(overflowCheck64(overflowCheck64(Lcase_fib_rec(var_97)-1)+Lcase_fib_rec(var_97))-2)
};};
var Lcase_fib_case = function(var_114){
var var_115 = var_114;
return switch(var_115){
case 0:
1
case 1:
1
default:overflowCheck64(overflowCheck64(overflowCheck64(Lcase_fib_case(var_115)-1)+Lcase_fib_case(var_115))-2)
};};
var Lcase_not_exhaustive = function(var_134)
{return switch(var_134){
case 1:
1
case 2:
2
default: Error! 
};};
var Lcase_curried = function(var_141)
{return function(var_142)
{return switch(var_141){
case 1:
switch(var_142){
case 1:
2
default:Lcase_curried(overflowCheck64(var_142-1))(1)
}
default:switch(var_142){
case 1:
overflowCheck64(Lcase_curried(overflowCheck64(var_141-1))(1)+Lcase_curried(1)(1))
default: Error! 
}
};};};
var Lcase_list = function(var_169)
{return  Error! ;};
var Lif_a = 63;
var Lif_b = (function(){ return ((Lif_a === 63) ? 10 : 20)}());
var Lif_c = (function(){ return ((Lif_b === 20) ? "a" : "b")}());
var Lif_d = false;
var Lif_d = true;
var Lif_e = (function(){ return (Lif_d ? 1 : 2)}());
var Lif_f = (function(){ return (!Lif_d ? 1 : 2)}());
var Lif_test = function(var_203)
{return (function(){ return ( Error!  ? 1 : 0)}());};
var Lseq_discard_evaluation = (1,2);
(var Lseq_func1 = function(var_214)
{return switch(var_214){
case 0:
0
default:overflowCheck64(overflowCheck64(1+Lseq_func2(var_214))-1)
};},var Lseq_func2 = function(var_226)
{return switch(var_226){
case 0:
0
default:overflowCheck64(overflowCheck64(2*Lseq_func1(var_226))-1)
};});
var Lwhile_infinite = while (true){
1
};
var Landalso_test1 =  Error! ;
var Lorelse_test =  Error! ;
(function(){
var var_245 = "Lunspec";
return ;
}());
