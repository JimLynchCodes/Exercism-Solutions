// Compiled by ClojureScript 1.10.773 {:target :nodejs}
goog.provide('clojure.core.rrb_vector.protocols');
goog.require('cljs.core');

/**
 * @interface
 */
clojure.core.rrb_vector.protocols.PSpliceableVector = function(){};

var clojure$core$rrb_vector$protocols$PSpliceableVector$_splicev$dyn_3303 = (function (v1,v2){
var x__4428__auto__ = (((v1 == null))?null:v1);
var m__4429__auto__ = (clojure.core.rrb_vector.protocols._splicev[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,v1,v2);
} else {
var m__4426__auto__ = (clojure.core.rrb_vector.protocols._splicev["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,v1,v2);
} else {
throw cljs.core.missing_protocol.call(null,"PSpliceableVector.-splicev",v1);
}
}
});
clojure.core.rrb_vector.protocols._splicev = (function clojure$core$rrb_vector$protocols$_splicev(v1,v2){
if((((!((v1 == null)))) && ((!((v1.clojure$core$rrb_vector$protocols$PSpliceableVector$_splicev$arity$2 == null)))))){
return v1.clojure$core$rrb_vector$protocols$PSpliceableVector$_splicev$arity$2(v1,v2);
} else {
return clojure$core$rrb_vector$protocols$PSpliceableVector$_splicev$dyn_3303.call(null,v1,v2);
}
});


/**
 * @interface
 */
clojure.core.rrb_vector.protocols.PSliceableVector = function(){};

var clojure$core$rrb_vector$protocols$PSliceableVector$_slicev$dyn_3304 = (function (v,start,end){
var x__4428__auto__ = (((v == null))?null:v);
var m__4429__auto__ = (clojure.core.rrb_vector.protocols._slicev[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,v,start,end);
} else {
var m__4426__auto__ = (clojure.core.rrb_vector.protocols._slicev["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,v,start,end);
} else {
throw cljs.core.missing_protocol.call(null,"PSliceableVector.-slicev",v);
}
}
});
clojure.core.rrb_vector.protocols._slicev = (function clojure$core$rrb_vector$protocols$_slicev(v,start,end){
if((((!((v == null)))) && ((!((v.clojure$core$rrb_vector$protocols$PSliceableVector$_slicev$arity$3 == null)))))){
return v.clojure$core$rrb_vector$protocols$PSliceableVector$_slicev$arity$3(v,start,end);
} else {
return clojure$core$rrb_vector$protocols$PSliceableVector$_slicev$dyn_3304.call(null,v,start,end);
}
});


//# sourceMappingURL=protocols.js.map
