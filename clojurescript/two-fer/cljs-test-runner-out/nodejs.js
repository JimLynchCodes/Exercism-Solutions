// Compiled by ClojureScript 1.10.773 {:target :nodejs}
goog.provide('cljs.nodejs');
goog.require('cljs.core');
cljs.nodejs.require = require;
cljs.nodejs.process = process;
cljs.nodejs.enable_util_print_BANG_ = (function cljs$nodejs$enable_util_print_BANG_(){
(cljs.core._STAR_print_newline_STAR_ = false);

cljs.core.set_print_fn_BANG_.call(null,(function() { 
var G__4278__delegate = function (args){
return console.log.apply(console,cljs.core.into_array.call(null,args));
};
var G__4278 = function (var_args){
var args = null;
if (arguments.length > 0) {
var G__4279__i = 0, G__4279__a = new Array(arguments.length -  0);
while (G__4279__i < G__4279__a.length) {G__4279__a[G__4279__i] = arguments[G__4279__i + 0]; ++G__4279__i;}
  args = new cljs.core.IndexedSeq(G__4279__a,0,null);
} 
return G__4278__delegate.call(this,args);};
G__4278.cljs$lang$maxFixedArity = 0;
G__4278.cljs$lang$applyTo = (function (arglist__4280){
var args = cljs.core.seq(arglist__4280);
return G__4278__delegate(args);
});
G__4278.cljs$core$IFn$_invoke$arity$variadic = G__4278__delegate;
return G__4278;
})()
);

cljs.core.set_print_err_fn_BANG_.call(null,(function() { 
var G__4281__delegate = function (args){
return console.error.apply(console,cljs.core.into_array.call(null,args));
};
var G__4281 = function (var_args){
var args = null;
if (arguments.length > 0) {
var G__4282__i = 0, G__4282__a = new Array(arguments.length -  0);
while (G__4282__i < G__4282__a.length) {G__4282__a[G__4282__i] = arguments[G__4282__i + 0]; ++G__4282__i;}
  args = new cljs.core.IndexedSeq(G__4282__a,0,null);
} 
return G__4281__delegate.call(this,args);};
G__4281.cljs$lang$maxFixedArity = 0;
G__4281.cljs$lang$applyTo = (function (arglist__4283){
var args = cljs.core.seq(arglist__4283);
return G__4281__delegate(args);
});
G__4281.cljs$core$IFn$_invoke$arity$variadic = G__4281__delegate;
return G__4281;
})()
);

return null;
});

//# sourceMappingURL=nodejs.js.map
