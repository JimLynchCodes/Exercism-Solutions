// Compiled by ClojureScript 1.10.773 {:target :nodejs}
goog.provide('two_fer');
goog.require('cljs.core');
two_fer.line_beginning = "One for ";
two_fer.line_ending = ", one for me.";
two_fer.default_name = "you";
two_fer.two_fer = (function two_fer$two_fer(var_args){
var G__4094 = arguments.length;
switch (G__4094) {
case 1:
return two_fer.two_fer.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 0:
return two_fer.two_fer.cljs$core$IFn$_invoke$arity$0();

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(two_fer.two_fer.cljs$core$IFn$_invoke$arity$1 = (function (name){
return [two_fer.line_beginning,cljs.core.str.cljs$core$IFn$_invoke$arity$1(name),two_fer.line_ending].join('');
}));

(two_fer.two_fer.cljs$core$IFn$_invoke$arity$0 = (function (){
return [two_fer.line_beginning,two_fer.default_name,two_fer.line_ending].join('');
}));

(two_fer.two_fer.cljs$lang$maxFixedArity = 1);


//# sourceMappingURL=two_fer.js.map
