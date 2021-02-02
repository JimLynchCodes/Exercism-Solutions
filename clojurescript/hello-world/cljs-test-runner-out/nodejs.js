// Compiled by ClojureScript 1.10.773 {:target :nodejs}
goog.provide('cljs.nodejs');
goog.require('cljs.core');
cljs.nodejs.require = require;
cljs.nodejs.process = process;
cljs.nodejs.enable_util_print_BANG_ = (function cljs$nodejs$enable_util_print_BANG_(){
(cljs.core._STAR_print_newline_STAR_ = false);

cljs.core.set_print_fn_BANG_.call(null,(function() { 
var G__4271__delegate = function (args){
return console.log.apply(console,cljs.core.into_array.call(null,args));
};
var G__4271 = function (var_args){
var args = null;
if (arguments.length > 0) {
var G__4272__i = 0, G__4272__a = new Array(arguments.length -  0);
while (G__4272__i < G__4272__a.length) {G__4272__a[G__4272__i] = arguments[G__4272__i + 0]; ++G__4272__i;}
  args = new cljs.core.IndexedSeq(G__4272__a,0,null);
} 
return G__4271__delegate.call(this,args);};
G__4271.cljs$lang$maxFixedArity = 0;
G__4271.cljs$lang$applyTo = (function (arglist__4273){
var args = cljs.core.seq(arglist__4273);
return G__4271__delegate(args);
});
G__4271.cljs$core$IFn$_invoke$arity$variadic = G__4271__delegate;
return G__4271;
})()
);

cljs.core.set_print_err_fn_BANG_.call(null,(function() { 
var G__4274__delegate = function (args){
return console.error.apply(console,cljs.core.into_array.call(null,args));
};
var G__4274 = function (var_args){
var args = null;
if (arguments.length > 0) {
var G__4275__i = 0, G__4275__a = new Array(arguments.length -  0);
while (G__4275__i < G__4275__a.length) {G__4275__a[G__4275__i] = arguments[G__4275__i + 0]; ++G__4275__i;}
  args = new cljs.core.IndexedSeq(G__4275__a,0,null);
} 
return G__4274__delegate.call(this,args);};
G__4274.cljs$lang$maxFixedArity = 0;
G__4274.cljs$lang$applyTo = (function (arglist__4276){
var args = cljs.core.seq(arglist__4276);
return G__4274__delegate(args);
});
G__4274.cljs$core$IFn$_invoke$arity$variadic = G__4274__delegate;
return G__4274;
})()
);

return null;
});

//# sourceMappingURL=nodejs.js.map
