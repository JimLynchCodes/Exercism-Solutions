// Compiled by ClojureScript 1.10.773 {:target :nodejs}
goog.provide('fipp.visit');
goog.require('cljs.core');
goog.require('fipp.util');
goog.require('fipp.ednize');

/**
 * @interface
 */
fipp.visit.IVisitor = function(){};

var fipp$visit$IVisitor$visit_unknown$dyn_3660 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_unknown[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_unknown["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-unknown",this$);
}
}
});
fipp.visit.visit_unknown = (function fipp$visit$visit_unknown(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_unknown$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_unknown$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_unknown$dyn_3660.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_nil$dyn_3661 = (function (this$){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_nil[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$);
} else {
var m__4426__auto__ = (fipp.visit.visit_nil["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-nil",this$);
}
}
});
fipp.visit.visit_nil = (function fipp$visit$visit_nil(this$){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_nil$arity$1 == null)))))){
return this$.fipp$visit$IVisitor$visit_nil$arity$1(this$);
} else {
return fipp$visit$IVisitor$visit_nil$dyn_3661.call(null,this$);
}
});

var fipp$visit$IVisitor$visit_boolean$dyn_3662 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_boolean[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_boolean["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-boolean",this$);
}
}
});
fipp.visit.visit_boolean = (function fipp$visit$visit_boolean(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_boolean$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_boolean$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_boolean$dyn_3662.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_string$dyn_3663 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_string[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_string["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-string",this$);
}
}
});
fipp.visit.visit_string = (function fipp$visit$visit_string(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_string$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_string$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_string$dyn_3663.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_character$dyn_3664 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_character[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_character["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-character",this$);
}
}
});
fipp.visit.visit_character = (function fipp$visit$visit_character(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_character$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_character$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_character$dyn_3664.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_symbol$dyn_3665 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_symbol[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_symbol["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-symbol",this$);
}
}
});
fipp.visit.visit_symbol = (function fipp$visit$visit_symbol(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_symbol$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_symbol$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_symbol$dyn_3665.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_keyword$dyn_3666 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_keyword[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_keyword["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-keyword",this$);
}
}
});
fipp.visit.visit_keyword = (function fipp$visit$visit_keyword(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_keyword$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_keyword$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_keyword$dyn_3666.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_number$dyn_3667 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_number[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_number["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-number",this$);
}
}
});
fipp.visit.visit_number = (function fipp$visit$visit_number(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_number$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_number$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_number$dyn_3667.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_seq$dyn_3668 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_seq[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_seq["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-seq",this$);
}
}
});
fipp.visit.visit_seq = (function fipp$visit$visit_seq(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_seq$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_seq$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_seq$dyn_3668.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_vector$dyn_3669 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_vector[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_vector["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-vector",this$);
}
}
});
fipp.visit.visit_vector = (function fipp$visit$visit_vector(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_vector$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_vector$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_vector$dyn_3669.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_map$dyn_3670 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_map[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_map["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-map",this$);
}
}
});
fipp.visit.visit_map = (function fipp$visit$visit_map(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_map$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_map$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_map$dyn_3670.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_set$dyn_3671 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_set[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_set["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-set",this$);
}
}
});
fipp.visit.visit_set = (function fipp$visit$visit_set(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_set$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_set$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_set$dyn_3671.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_tagged$dyn_3672 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_tagged[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_tagged["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-tagged",this$);
}
}
});
fipp.visit.visit_tagged = (function fipp$visit$visit_tagged(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_tagged$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_tagged$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_tagged$dyn_3672.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_meta$dyn_3673 = (function (this$,meta,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_meta[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,meta,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_meta["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,meta,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-meta",this$);
}
}
});
fipp.visit.visit_meta = (function fipp$visit$visit_meta(this$,meta,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_meta$arity$3 == null)))))){
return this$.fipp$visit$IVisitor$visit_meta$arity$3(this$,meta,x);
} else {
return fipp$visit$IVisitor$visit_meta$dyn_3673.call(null,this$,meta,x);
}
});

var fipp$visit$IVisitor$visit_var$dyn_3674 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_var[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_var["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-var",this$);
}
}
});
fipp.visit.visit_var = (function fipp$visit$visit_var(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_var$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_var$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_var$dyn_3674.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_pattern$dyn_3675 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_pattern[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_pattern["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-pattern",this$);
}
}
});
fipp.visit.visit_pattern = (function fipp$visit$visit_pattern(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_pattern$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_pattern$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_pattern$dyn_3675.call(null,this$,x);
}
});

var fipp$visit$IVisitor$visit_record$dyn_3676 = (function (this$,x){
var x__4428__auto__ = (((this$ == null))?null:this$);
var m__4429__auto__ = (fipp.visit.visit_record[goog.typeOf(x__4428__auto__)]);
if((!((m__4429__auto__ == null)))){
return m__4429__auto__.call(null,this$,x);
} else {
var m__4426__auto__ = (fipp.visit.visit_record["_"]);
if((!((m__4426__auto__ == null)))){
return m__4426__auto__.call(null,this$,x);
} else {
throw cljs.core.missing_protocol.call(null,"IVisitor.visit-record",this$);
}
}
});
fipp.visit.visit_record = (function fipp$visit$visit_record(this$,x){
if((((!((this$ == null)))) && ((!((this$.fipp$visit$IVisitor$visit_record$arity$2 == null)))))){
return this$.fipp$visit$IVisitor$visit_record$arity$2(this$,x);
} else {
return fipp$visit$IVisitor$visit_record$dyn_3676.call(null,this$,x);
}
});

/**
 * Visits objects, ignoring metadata.
 */
fipp.visit.visit_STAR_ = (function fipp$visit$visit_STAR_(visitor,x){
if((x == null)){
return fipp.visit.visit_nil.call(null,visitor);
} else {
if(fipp.ednize.override_QMARK_.call(null,x)){
return fipp.visit.visit_unknown.call(null,visitor,x);
} else {
if(fipp.util.boolean_QMARK_.call(null,x)){
return fipp.visit.visit_boolean.call(null,visitor,x);
} else {
if(typeof x === 'string'){
return fipp.visit.visit_string.call(null,visitor,x);
} else {
if(fipp.util.char_QMARK_.call(null,x)){
return fipp.visit.visit_character.call(null,visitor,x);
} else {
if((x instanceof cljs.core.Symbol)){
return fipp.visit.visit_symbol.call(null,visitor,x);
} else {
if((x instanceof cljs.core.Keyword)){
return fipp.visit.visit_keyword.call(null,visitor,x);
} else {
if(typeof x === 'number'){
return fipp.visit.visit_number.call(null,visitor,x);
} else {
if(cljs.core.seq_QMARK_.call(null,x)){
return fipp.visit.visit_seq.call(null,visitor,x);
} else {
if(cljs.core.vector_QMARK_.call(null,x)){
return fipp.visit.visit_vector.call(null,visitor,x);
} else {
if(cljs.core.record_QMARK_.call(null,x)){
return fipp.visit.visit_record.call(null,visitor,x);
} else {
if(cljs.core.map_QMARK_.call(null,x)){
return fipp.visit.visit_map.call(null,visitor,x);
} else {
if(cljs.core.set_QMARK_.call(null,x)){
return fipp.visit.visit_set.call(null,visitor,x);
} else {
if(cljs.core.tagged_literal_QMARK_.call(null,x)){
return fipp.visit.visit_tagged.call(null,visitor,x);
} else {
if(cljs.core.var_QMARK_.call(null,x)){
return fipp.visit.visit_var.call(null,visitor,x);
} else {
if(cljs.core.truth_(fipp.util.regexp_QMARK_.call(null,x))){
return fipp.visit.visit_pattern.call(null,visitor,x);
} else {
return fipp.visit.visit_unknown.call(null,visitor,x);

}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
fipp.visit.value_meta = (function fipp$visit$value_meta(x){
if(fipp.util.value_obj_QMARK_.call(null,x)){
return cljs.core.meta.call(null,x);
} else {
return null;
}
});
fipp.visit.visit = (function fipp$visit$visit(visitor,x){
var temp__5733__auto__ = fipp.visit.value_meta.call(null,x);
if(cljs.core.truth_(temp__5733__auto__)){
var m = temp__5733__auto__;
return fipp.visit.visit_meta.call(null,visitor,m,x);
} else {
return fipp.visit.visit_STAR_.call(null,visitor,x);
}
});

//# sourceMappingURL=visit.js.map
