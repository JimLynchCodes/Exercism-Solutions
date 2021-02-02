// Compiled by ClojureScript 1.10.773 {:target :nodejs}
goog.provide('fipp.clojure');
goog.require('cljs.core');
goog.require('clojure.walk');
goog.require('fipp.visit');
goog.require('fipp.edn');
fipp.clojure.block = (function fipp$clojure$block(nodes){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"nest","nest",-314993663),(2),cljs.core.interpose.call(null,new cljs.core.Keyword(null,"line","line",212345235),nodes)], null);
});
fipp.clojure.list_group = (function fipp$clojure$list_group(var_args){
var args__4742__auto__ = [];
var len__4736__auto___3797 = arguments.length;
var i__4737__auto___3798 = (0);
while(true){
if((i__4737__auto___3798 < len__4736__auto___3797)){
args__4742__auto__.push((arguments[i__4737__auto___3798]));

var G__3799 = (i__4737__auto___3798 + (1));
i__4737__auto___3798 = G__3799;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((0) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((0)),(0),null)):null);
return fipp.clojure.list_group.cljs$core$IFn$_invoke$arity$variadic(argseq__4743__auto__);
});

(fipp.clojure.list_group.cljs$core$IFn$_invoke$arity$variadic = (function (nodes){
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"group","group",582596132),"(",nodes,")"], null);
}));

(fipp.clojure.list_group.cljs$lang$maxFixedArity = (0));

/** @this {Function} */
(fipp.clojure.list_group.cljs$lang$applyTo = (function (seq3796){
var self__4724__auto__ = this;
return self__4724__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq3796));
}));

fipp.clojure.maybe_a = (function fipp$clojure$maybe_a(pred,xs){
var x = cljs.core.first.call(null,xs);
if(cljs.core.truth_(pred.call(null,x))){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [x,cljs.core.rest.call(null,xs)], null);
} else {
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [null,xs], null);
}
});
fipp.clojure.pretty_cond_clause = (function fipp$clojure$pretty_cond_clause(p,p__3800){
var vec__3801 = p__3800;
var test = cljs.core.nth.call(null,vec__3801,(0),null);
var result = cljs.core.nth.call(null,vec__3801,(1),null);
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"group","group",582596132),fipp.visit.visit.call(null,p,test),new cljs.core.Keyword(null,"line","line",212345235),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"nest","nest",-314993663),(2),fipp.visit.visit.call(null,p,result)], null)], null);
});
fipp.clojure.pretty_case = (function fipp$clojure$pretty_case(p,p__3805){
var vec__3806 = p__3805;
var seq__3807 = cljs.core.seq.call(null,vec__3806);
var first__3808 = cljs.core.first.call(null,seq__3807);
var seq__3807__$1 = cljs.core.next.call(null,seq__3807);
var head = first__3808;
var first__3808__$1 = cljs.core.first.call(null,seq__3807__$1);
var seq__3807__$2 = cljs.core.next.call(null,seq__3807__$1);
var expr = first__3808__$1;
var more = seq__3807__$2;
var clauses = cljs.core.partition.call(null,(2),more);
var default$ = ((cljs.core.odd_QMARK_.call(null,cljs.core.count.call(null,more)))?cljs.core.last.call(null,more):null);
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,expr),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.concat.call(null,cljs.core.map.call(null,(function (p1__3804_SHARP_){
return fipp.clojure.pretty_cond_clause.call(null,p,p1__3804_SHARP_);
}),clauses),(cljs.core.truth_(default$)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,default$)], null):null))));
});
fipp.clojure.pretty_cond = (function fipp$clojure$pretty_cond(p,p__3810){
var vec__3811 = p__3810;
var seq__3812 = cljs.core.seq.call(null,vec__3811);
var first__3813 = cljs.core.first.call(null,seq__3812);
var seq__3812__$1 = cljs.core.next.call(null,seq__3812);
var head = first__3813;
var more = seq__3812__$1;
var clauses = cljs.core.partition.call(null,(2),more);
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.map.call(null,(function (p1__3809_SHARP_){
return fipp.clojure.pretty_cond_clause.call(null,p,p1__3809_SHARP_);
}),clauses)));
});
fipp.clojure.pretty_condp = (function fipp$clojure$pretty_condp(p,p__3815){
var vec__3816 = p__3815;
var seq__3817 = cljs.core.seq.call(null,vec__3816);
var first__3818 = cljs.core.first.call(null,seq__3817);
var seq__3817__$1 = cljs.core.next.call(null,seq__3817);
var head = first__3818;
var first__3818__$1 = cljs.core.first.call(null,seq__3817__$1);
var seq__3817__$2 = cljs.core.next.call(null,seq__3817__$1);
var pred = first__3818__$1;
var first__3818__$2 = cljs.core.first.call(null,seq__3817__$2);
var seq__3817__$3 = cljs.core.next.call(null,seq__3817__$2);
var expr = first__3818__$2;
var more = seq__3817__$3;
var clauses = cljs.core.partition.call(null,(2),more);
var default$ = ((cljs.core.odd_QMARK_.call(null,cljs.core.count.call(null,more)))?cljs.core.last.call(null,more):null);
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,pred)," ",fipp.visit.visit.call(null,p,expr),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.concat.call(null,cljs.core.map.call(null,(function (p1__3814_SHARP_){
return fipp.clojure.pretty_cond_clause.call(null,p,p1__3814_SHARP_);
}),clauses),(cljs.core.truth_(default$)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,default$)], null):null))));
});
fipp.clojure.pretty_arrow = (function fipp$clojure$pretty_arrow(p,p__3820){
var vec__3821 = p__3820;
var seq__3822 = cljs.core.seq.call(null,vec__3821);
var first__3823 = cljs.core.first.call(null,seq__3822);
var seq__3822__$1 = cljs.core.next.call(null,seq__3822);
var head = first__3823;
var stmts = seq__3822__$1;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),cljs.core.interpose.call(null,new cljs.core.Keyword(null,"line","line",212345235),cljs.core.map.call(null,(function (p1__3819_SHARP_){
return fipp.visit.visit.call(null,p,p1__3819_SHARP_);
}),stmts))], null));
});
fipp.clojure.pretty_if = (function fipp$clojure$pretty_if(p,p__3825){
var vec__3826 = p__3825;
var seq__3827 = cljs.core.seq.call(null,vec__3826);
var first__3828 = cljs.core.first.call(null,seq__3827);
var seq__3827__$1 = cljs.core.next.call(null,seq__3827);
var head = first__3828;
var first__3828__$1 = cljs.core.first.call(null,seq__3827__$1);
var seq__3827__$2 = cljs.core.next.call(null,seq__3827__$1);
var test = first__3828__$1;
var more = seq__3827__$2;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,test),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.map.call(null,(function (p1__3824_SHARP_){
return fipp.visit.visit.call(null,p,p1__3824_SHARP_);
}),more)));
});
fipp.clojure.pretty_method = (function fipp$clojure$pretty_method(p,p__3830){
var vec__3831 = p__3830;
var seq__3832 = cljs.core.seq.call(null,vec__3831);
var first__3833 = cljs.core.first.call(null,seq__3832);
var seq__3832__$1 = cljs.core.next.call(null,seq__3832);
var params = first__3833;
var body = seq__3832__$1;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,params),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.map.call(null,(function (p1__3829_SHARP_){
return fipp.visit.visit.call(null,p,p1__3829_SHARP_);
}),body)));
});
fipp.clojure.pretty_defn = (function fipp$clojure$pretty_defn(p,p__3836){
var vec__3837 = p__3836;
var seq__3838 = cljs.core.seq.call(null,vec__3837);
var first__3839 = cljs.core.first.call(null,seq__3838);
var seq__3838__$1 = cljs.core.next.call(null,seq__3838);
var head = first__3839;
var first__3839__$1 = cljs.core.first.call(null,seq__3838__$1);
var seq__3838__$2 = cljs.core.next.call(null,seq__3838__$1);
var fn_name = first__3839__$1;
var more = seq__3838__$2;
var vec__3840 = fipp.clojure.maybe_a.call(null,cljs.core.string_QMARK_,more);
var docstring = cljs.core.nth.call(null,vec__3840,(0),null);
var more__$1 = cljs.core.nth.call(null,vec__3840,(1),null);
var vec__3843 = fipp.clojure.maybe_a.call(null,cljs.core.map_QMARK_,more__$1);
var attr_map = cljs.core.nth.call(null,vec__3843,(0),null);
var more__$2 = cljs.core.nth.call(null,vec__3843,(1),null);
var vec__3846 = fipp.clojure.maybe_a.call(null,cljs.core.vector_QMARK_,more__$2);
var params = cljs.core.nth.call(null,vec__3846,(0),null);
var body = cljs.core.nth.call(null,vec__3846,(1),null);
var params_on_first_line_QMARK_ = (function (){var and__4115__auto__ = params;
if(cljs.core.truth_(and__4115__auto__)){
return (((docstring == null)) && ((attr_map == null)));
} else {
return and__4115__auto__;
}
})();
var params_after_attr_map_QMARK_ = (function (){var and__4115__auto__ = params;
if(cljs.core.truth_(and__4115__auto__)){
return cljs.core.not.call(null,params_on_first_line_QMARK_);
} else {
return and__4115__auto__;
}
})();
return fipp.clojure.list_group.call(null,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,fn_name)], null),(cljs.core.truth_(params_on_first_line_QMARK_)?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [" ",fipp.visit.visit.call(null,p,params)], null):null)),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.concat.call(null,(cljs.core.truth_(docstring)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,docstring)], null):null),(cljs.core.truth_(attr_map)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,attr_map)], null):null),(cljs.core.truth_(params_after_attr_map_QMARK_)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,params)], null):null),(cljs.core.truth_(body)?cljs.core.map.call(null,(function (p1__3834_SHARP_){
return fipp.visit.visit.call(null,p,p1__3834_SHARP_);
}),body):cljs.core.map.call(null,(function (p1__3835_SHARP_){
return fipp.clojure.pretty_method.call(null,p,p1__3835_SHARP_);
}),more__$2)))));
});
fipp.clojure.pretty_fn = (function fipp$clojure$pretty_fn(p,p__3851){
var vec__3852 = p__3851;
var seq__3853 = cljs.core.seq.call(null,vec__3852);
var first__3854 = cljs.core.first.call(null,seq__3853);
var seq__3853__$1 = cljs.core.next.call(null,seq__3853);
var head = first__3854;
var more = seq__3853__$1;
var vec__3855 = fipp.clojure.maybe_a.call(null,cljs.core.symbol_QMARK_,more);
var fn_name = cljs.core.nth.call(null,vec__3855,(0),null);
var more__$1 = cljs.core.nth.call(null,vec__3855,(1),null);
var vec__3858 = fipp.clojure.maybe_a.call(null,cljs.core.vector_QMARK_,more__$1);
var params = cljs.core.nth.call(null,vec__3858,(0),null);
var body = cljs.core.nth.call(null,vec__3858,(1),null);
return fipp.clojure.list_group.call(null,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,head)], null),(cljs.core.truth_(fn_name)?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [" ",fipp.visit.visit.call(null,p,fn_name)], null):null),(cljs.core.truth_(params)?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [" ",fipp.visit.visit.call(null,p,params)], null):null)),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,(cljs.core.truth_(body)?cljs.core.map.call(null,(function (p1__3849_SHARP_){
return fipp.visit.visit.call(null,p,p1__3849_SHARP_);
}),body):cljs.core.map.call(null,(function (p1__3850_SHARP_){
return fipp.clojure.pretty_method.call(null,p,p1__3850_SHARP_);
}),more__$1))));
});
fipp.clojure.pretty_fn_STAR_ = (function fipp$clojure$pretty_fn_STAR_(p,p__3864){
var vec__3865 = p__3864;
var _ = cljs.core.nth.call(null,vec__3865,(0),null);
var params = cljs.core.nth.call(null,vec__3865,(1),null);
var body = cljs.core.nth.call(null,vec__3865,(2),null);
var form = vec__3865;
if(((cljs.core.vector_QMARK_.call(null,params)) && (cljs.core.seq_QMARK_.call(null,body)))){
var vec__3868 = cljs.core.split_with.call(null,(function (p1__3861_SHARP_){
return cljs.core.not_EQ_.call(null,p1__3861_SHARP_,new cljs.core.Symbol(null,"&","&",-2144855648,null));
}),params);
var inits = cljs.core.nth.call(null,vec__3868,(0),null);
var rests = cljs.core.nth.call(null,vec__3868,(1),null);
var params_STAR_ = cljs.core.merge.call(null,((cljs.core._EQ_.call(null,cljs.core.count.call(null,inits),(1)))?cljs.core.PersistentArrayMap.createAsIfByAssoc([cljs.core.first.call(null,inits),new cljs.core.Symbol(null,"%","%",-950237169,null)]):cljs.core.zipmap.call(null,inits,cljs.core.map.call(null,(function (p1__3862_SHARP_){
return cljs.core.symbol.call(null,["%",cljs.core.str.cljs$core$IFn$_invoke$arity$1((p1__3862_SHARP_ + (1)))].join(''));
}),cljs.core.range.call(null)))),((cljs.core.seq.call(null,rests))?cljs.core.PersistentArrayMap.createAsIfByAssoc([cljs.core.second.call(null,rests),new cljs.core.Symbol(null,"%&","%&",-728707069,null)]):null));
var body_STAR_ = clojure.walk.prewalk_replace.call(null,params_STAR_,body);
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"group","group",582596132),"#(",new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),(2),cljs.core.interpose.call(null,new cljs.core.Keyword(null,"line","line",212345235),cljs.core.map.call(null,(function (p1__3863_SHARP_){
return fipp.visit.visit.call(null,p,p1__3863_SHARP_);
}),body_STAR_))], null),")"], null);
} else {
return fipp.clojure.pretty_fn.call(null,p,form);
}
});
fipp.clojure.pretty_libspec = (function fipp$clojure$pretty_libspec(p,p__3872){
var vec__3873 = p__3872;
var seq__3874 = cljs.core.seq.call(null,vec__3873);
var first__3875 = cljs.core.first.call(null,seq__3874);
var seq__3874__$1 = cljs.core.next.call(null,seq__3874);
var head = first__3875;
var clauses = seq__3874__$1;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),cljs.core.interpose.call(null,new cljs.core.Keyword(null,"line","line",212345235),cljs.core.map.call(null,(function (p1__3871_SHARP_){
return fipp.visit.visit.call(null,p,p1__3871_SHARP_);
}),clauses))], null));
});
fipp.clojure.pretty_ns = (function fipp$clojure$pretty_ns(p,p__3877){
var vec__3878 = p__3877;
var seq__3879 = cljs.core.seq.call(null,vec__3878);
var first__3880 = cljs.core.first.call(null,seq__3879);
var seq__3879__$1 = cljs.core.next.call(null,seq__3879);
var head = first__3880;
var first__3880__$1 = cljs.core.first.call(null,seq__3879__$1);
var seq__3879__$2 = cljs.core.next.call(null,seq__3879__$1);
var ns_sym = first__3880__$1;
var more = seq__3879__$2;
var vec__3881 = fipp.clojure.maybe_a.call(null,cljs.core.string_QMARK_,more);
var docstring = cljs.core.nth.call(null,vec__3881,(0),null);
var more__$1 = cljs.core.nth.call(null,vec__3881,(1),null);
var vec__3884 = fipp.clojure.maybe_a.call(null,cljs.core.map_QMARK_,more__$1);
var attr_map = cljs.core.nth.call(null,vec__3884,(0),null);
var specs = cljs.core.nth.call(null,vec__3884,(1),null);
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,ns_sym),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.concat.call(null,(cljs.core.truth_(docstring)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,docstring)], null):null),(cljs.core.truth_(attr_map)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,attr_map)], null):null),cljs.core.map.call(null,(function (p1__3876_SHARP_){
return fipp.clojure.pretty_libspec.call(null,p,p1__3876_SHARP_);
}),specs))));
});
fipp.clojure.pretty_quote = (function fipp$clojure$pretty_quote(p,p__3887){
var vec__3888 = p__3887;
var macro = cljs.core.nth.call(null,vec__3888,(0),null);
var arg = cljs.core.nth.call(null,vec__3888,(1),null);
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span","span",1394872991),(function (){var G__3891 = cljs.core.keyword.call(null,cljs.core.name.call(null,macro));
var G__3891__$1 = (((G__3891 instanceof cljs.core.Keyword))?G__3891.fqn:null);
switch (G__3891__$1) {
case "deref":
return "@";

break;
case "quote":
return "'";

break;
case "unquote":
return "~";

break;
case "var":
return "#'";

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__3891__$1)].join('')));

}
})(),fipp.visit.visit.call(null,p,arg)], null);
});
fipp.clojure.pretty_bindings = (function fipp$clojure$pretty_bindings(p,bvec){
var kvps = (function (){var iter__4529__auto__ = (function fipp$clojure$pretty_bindings_$_iter__3893(s__3894){
return (new cljs.core.LazySeq(null,(function (){
var s__3894__$1 = s__3894;
while(true){
var temp__5735__auto__ = cljs.core.seq.call(null,s__3894__$1);
if(temp__5735__auto__){
var s__3894__$2 = temp__5735__auto__;
if(cljs.core.chunked_seq_QMARK_.call(null,s__3894__$2)){
var c__4527__auto__ = cljs.core.chunk_first.call(null,s__3894__$2);
var size__4528__auto__ = cljs.core.count.call(null,c__4527__auto__);
var b__3896 = cljs.core.chunk_buffer.call(null,size__4528__auto__);
if((function (){var i__3895 = (0);
while(true){
if((i__3895 < size__4528__auto__)){
var vec__3897 = cljs.core._nth.call(null,c__4527__auto__,i__3895);
var k = cljs.core.nth.call(null,vec__3897,(0),null);
var v = cljs.core.nth.call(null,vec__3897,(1),null);
cljs.core.chunk_append.call(null,b__3896,new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span","span",1394872991),fipp.visit.visit.call(null,p,k)," ",new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),fipp.visit.visit.call(null,p,v)], null)], null));

var G__3903 = (i__3895 + (1));
i__3895 = G__3903;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons.call(null,cljs.core.chunk.call(null,b__3896),fipp$clojure$pretty_bindings_$_iter__3893.call(null,cljs.core.chunk_rest.call(null,s__3894__$2)));
} else {
return cljs.core.chunk_cons.call(null,cljs.core.chunk.call(null,b__3896),null);
}
} else {
var vec__3900 = cljs.core.first.call(null,s__3894__$2);
var k = cljs.core.nth.call(null,vec__3900,(0),null);
var v = cljs.core.nth.call(null,vec__3900,(1),null);
return cljs.core.cons.call(null,new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span","span",1394872991),fipp.visit.visit.call(null,p,k)," ",new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),fipp.visit.visit.call(null,p,v)], null)], null),fipp$clojure$pretty_bindings_$_iter__3893.call(null,cljs.core.rest.call(null,s__3894__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__4529__auto__.call(null,cljs.core.partition.call(null,(2),bvec));
})();
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"group","group",582596132),"[",new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),cljs.core.interpose.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"line","line",212345235),", "], null),kvps)], null),"]"], null);
});
fipp.clojure.pretty_let = (function fipp$clojure$pretty_let(p,p__3905){
var vec__3906 = p__3905;
var seq__3907 = cljs.core.seq.call(null,vec__3906);
var first__3908 = cljs.core.first.call(null,seq__3907);
var seq__3907__$1 = cljs.core.next.call(null,seq__3907);
var head = first__3908;
var first__3908__$1 = cljs.core.first.call(null,seq__3907__$1);
var seq__3907__$2 = cljs.core.next.call(null,seq__3907__$1);
var bvec = first__3908__$1;
var body = seq__3907__$2;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.clojure.pretty_bindings.call(null,p,bvec),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.map.call(null,(function (p1__3904_SHARP_){
return fipp.visit.visit.call(null,p,p1__3904_SHARP_);
}),body)));
});
fipp.clojure.pretty_impls = (function fipp$clojure$pretty_impls(p,opts_PLUS_specs){
return fipp.clojure.block.call(null,cljs.core.map.call(null,(function (p1__3909_SHARP_){
return fipp.visit.visit.call(null,p,p1__3909_SHARP_);
}),opts_PLUS_specs));
});
fipp.clojure.pretty_type = (function fipp$clojure$pretty_type(p,p__3910){
var vec__3911 = p__3910;
var seq__3912 = cljs.core.seq.call(null,vec__3911);
var first__3913 = cljs.core.first.call(null,seq__3912);
var seq__3912__$1 = cljs.core.next.call(null,seq__3912);
var head = first__3913;
var first__3913__$1 = cljs.core.first.call(null,seq__3912__$1);
var seq__3912__$2 = cljs.core.next.call(null,seq__3912__$1);
var fields = first__3913__$1;
var opts_PLUS_specs = seq__3912__$2;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,fields),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.pretty_impls.call(null,p,opts_PLUS_specs));
});
fipp.clojure.pretty_reify = (function fipp$clojure$pretty_reify(p,p__3914){
var vec__3915 = p__3914;
var seq__3916 = cljs.core.seq.call(null,vec__3915);
var first__3917 = cljs.core.first.call(null,seq__3916);
var seq__3916__$1 = cljs.core.next.call(null,seq__3916);
var head = first__3917;
var opts_PLUS_specs = seq__3916__$1;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.pretty_impls.call(null,p,opts_PLUS_specs));
});
fipp.clojure.build_symbol_map = (function fipp$clojure$build_symbol_map(dispatch){
return cljs.core.into.call(null,cljs.core.PersistentArrayMap.EMPTY,(function (){var iter__4529__auto__ = (function fipp$clojure$build_symbol_map_$_iter__3918(s__3919){
return (new cljs.core.LazySeq(null,(function (){
var s__3919__$1 = s__3919;
while(true){
var temp__5735__auto__ = cljs.core.seq.call(null,s__3919__$1);
if(temp__5735__auto__){
var xs__6292__auto__ = temp__5735__auto__;
var vec__3926 = cljs.core.first.call(null,xs__6292__auto__);
var pretty_fn = cljs.core.nth.call(null,vec__3926,(0),null);
var syms = cljs.core.nth.call(null,vec__3926,(1),null);
var iterys__4525__auto__ = ((function (s__3919__$1,vec__3926,pretty_fn,syms,xs__6292__auto__,temp__5735__auto__){
return (function fipp$clojure$build_symbol_map_$_iter__3918_$_iter__3920(s__3921){
return (new cljs.core.LazySeq(null,((function (s__3919__$1,vec__3926,pretty_fn,syms,xs__6292__auto__,temp__5735__auto__){
return (function (){
var s__3921__$1 = s__3921;
while(true){
var temp__5735__auto____$1 = cljs.core.seq.call(null,s__3921__$1);
if(temp__5735__auto____$1){
var xs__6292__auto____$1 = temp__5735__auto____$1;
var sym = cljs.core.first.call(null,xs__6292__auto____$1);
var iterys__4525__auto__ = ((function (s__3921__$1,s__3919__$1,sym,xs__6292__auto____$1,temp__5735__auto____$1,vec__3926,pretty_fn,syms,xs__6292__auto__,temp__5735__auto__){
return (function fipp$clojure$build_symbol_map_$_iter__3918_$_iter__3920_$_iter__3922(s__3923){
return (new cljs.core.LazySeq(null,((function (s__3921__$1,s__3919__$1,sym,xs__6292__auto____$1,temp__5735__auto____$1,vec__3926,pretty_fn,syms,xs__6292__auto__,temp__5735__auto__){
return (function (){
var s__3923__$1 = s__3923;
while(true){
var temp__5735__auto____$2 = cljs.core.seq.call(null,s__3923__$1);
if(temp__5735__auto____$2){
var s__3923__$2 = temp__5735__auto____$2;
if(cljs.core.chunked_seq_QMARK_.call(null,s__3923__$2)){
var c__4527__auto__ = cljs.core.chunk_first.call(null,s__3923__$2);
var size__4528__auto__ = cljs.core.count.call(null,c__4527__auto__);
var b__3925 = cljs.core.chunk_buffer.call(null,size__4528__auto__);
if((function (){var i__3924 = (0);
while(true){
if((i__3924 < size__4528__auto__)){
var sym__$1 = cljs.core._nth.call(null,c__4527__auto__,i__3924);
cljs.core.chunk_append.call(null,b__3925,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [sym__$1,pretty_fn], null));

var G__3929 = (i__3924 + (1));
i__3924 = G__3929;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons.call(null,cljs.core.chunk.call(null,b__3925),fipp$clojure$build_symbol_map_$_iter__3918_$_iter__3920_$_iter__3922.call(null,cljs.core.chunk_rest.call(null,s__3923__$2)));
} else {
return cljs.core.chunk_cons.call(null,cljs.core.chunk.call(null,b__3925),null);
}
} else {
var sym__$1 = cljs.core.first.call(null,s__3923__$2);
return cljs.core.cons.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [sym__$1,pretty_fn], null),fipp$clojure$build_symbol_map_$_iter__3918_$_iter__3920_$_iter__3922.call(null,cljs.core.rest.call(null,s__3923__$2)));
}
} else {
return null;
}
break;
}
});})(s__3921__$1,s__3919__$1,sym,xs__6292__auto____$1,temp__5735__auto____$1,vec__3926,pretty_fn,syms,xs__6292__auto__,temp__5735__auto__))
,null,null));
});})(s__3921__$1,s__3919__$1,sym,xs__6292__auto____$1,temp__5735__auto____$1,vec__3926,pretty_fn,syms,xs__6292__auto__,temp__5735__auto__))
;
var fs__4526__auto__ = cljs.core.seq.call(null,iterys__4525__auto__.call(null,cljs.core.cons.call(null,sym,((cljs.core.special_symbol_QMARK_.call(null,sym))?null:new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.symbol.call(null,"clojure.core",cljs.core.name.call(null,sym)),cljs.core.symbol.call(null,"cljs.core",cljs.core.name.call(null,sym))], null)))));
if(fs__4526__auto__){
return cljs.core.concat.call(null,fs__4526__auto__,fipp$clojure$build_symbol_map_$_iter__3918_$_iter__3920.call(null,cljs.core.rest.call(null,s__3921__$1)));
} else {
var G__3930 = cljs.core.rest.call(null,s__3921__$1);
s__3921__$1 = G__3930;
continue;
}
} else {
return null;
}
break;
}
});})(s__3919__$1,vec__3926,pretty_fn,syms,xs__6292__auto__,temp__5735__auto__))
,null,null));
});})(s__3919__$1,vec__3926,pretty_fn,syms,xs__6292__auto__,temp__5735__auto__))
;
var fs__4526__auto__ = cljs.core.seq.call(null,iterys__4525__auto__.call(null,syms));
if(fs__4526__auto__){
return cljs.core.concat.call(null,fs__4526__auto__,fipp$clojure$build_symbol_map_$_iter__3918.call(null,cljs.core.rest.call(null,s__3919__$1)));
} else {
var G__3931 = cljs.core.rest.call(null,s__3919__$1);
s__3919__$1 = G__3931;
continue;
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__4529__auto__.call(null,dispatch);
})());
});
fipp.clojure.default_symbols = fipp.clojure.build_symbol_map.call(null,cljs.core.PersistentHashMap.fromArrays([fipp.clojure.pretty_fn_STAR_,fipp.clojure.pretty_condp,fipp.clojure.pretty_quote,fipp.clojure.pretty_cond,fipp.clojure.pretty_fn,fipp.clojure.pretty_arrow,fipp.clojure.pretty_reify,fipp.clojure.pretty_let,fipp.clojure.pretty_type,fipp.clojure.pretty_if,fipp.clojure.pretty_defn,fipp.clojure.pretty_ns,fipp.clojure.pretty_case],[new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"fn*","fn*",-752876845,null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"condp","condp",1054325175,null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"deref","deref",1494944732,null),new cljs.core.Symbol(null,"quote","quote",1377916282,null),new cljs.core.Symbol(null,"unquote","unquote",-1004694737,null),new cljs.core.Symbol(null,"var","var",870848730,null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"cond","cond",1606708055,null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"fn","fn",465265323,null)], null),new cljs.core.PersistentVector(null, 9, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,".",".",1975675962,null),new cljs.core.Symbol(null,"..","..",-300507420,null),new cljs.core.Symbol(null,"->","->",-2139605430,null),new cljs.core.Symbol(null,"->>","->>",-1874332161,null),new cljs.core.Symbol(null,"and","and",668631710,null),new cljs.core.Symbol(null,"doto","doto",1252536074,null),new cljs.core.Symbol(null,"or","or",1876275696,null),new cljs.core.Symbol(null,"some->","some->",-1011172200,null),new cljs.core.Symbol(null,"some->>","some->>",-1499987794,null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"reify","reify",1885539699,null)], null),new cljs.core.PersistentVector(null, 16, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"binding","binding",-2114503176,null),new cljs.core.Symbol(null,"doseq","doseq",221164135,null),new cljs.core.Symbol(null,"dotimes","dotimes",-818708397,null),new cljs.core.Symbol(null,"for","for",316745208,null),new cljs.core.Symbol(null,"if-let","if-let",1803593690,null),new cljs.core.Symbol(null,"if-some","if-some",1960677609,null),new cljs.core.Symbol(null,"let","let",358118826,null),new cljs.core.Symbol(null,"let*","let*",1920721458,null),new cljs.core.Symbol(null,"loop","loop",1244978678,null),new cljs.core.Symbol(null,"loop*","loop*",615029416,null),new cljs.core.Symbol(null,"when-first","when-first",821699168,null),new cljs.core.Symbol(null,"when-let","when-let",-1383043480,null),new cljs.core.Symbol(null,"when-some","when-some",1700415903,null),new cljs.core.Symbol(null,"with-local-vars","with-local-vars",837642072,null),new cljs.core.Symbol(null,"with-open","with-open",172119667,null),new cljs.core.Symbol(null,"with-redefs","with-redefs",-1143728263,null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"deftype","deftype",1980826088,null),new cljs.core.Symbol(null,"defrecord","defrecord",273038109,null)], null),new cljs.core.PersistentVector(null, 6, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"def","def",597100991,null),new cljs.core.Symbol(null,"defonce","defonce",-1681484013,null),new cljs.core.Symbol(null,"if","if",1181717262,null),new cljs.core.Symbol(null,"if-not","if-not",-265415609,null),new cljs.core.Symbol(null,"when","when",1064114221,null),new cljs.core.Symbol(null,"when-not","when-not",-1223136340,null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"defmacro","defmacro",2054157304,null),new cljs.core.Symbol(null,"defmulti","defmulti",1936112154,null),new cljs.core.Symbol(null,"defn","defn",-126010802,null),new cljs.core.Symbol(null,"defn-","defn-",1097765044,null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"ns","ns",2082130287,null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"case","case",-1510733573,null),new cljs.core.Symbol(null,"cond->","cond->",561741875,null),new cljs.core.Symbol(null,"cond->>","cond->>",348844960,null)], null)]));
fipp.clojure.pprint = (function fipp$clojure$pprint(var_args){
var G__3933 = arguments.length;
switch (G__3933) {
case 1:
return fipp.clojure.pprint.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return fipp.clojure.pprint.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(fipp.clojure.pprint.cljs$core$IFn$_invoke$arity$1 = (function (x){
return fipp.clojure.pprint.call(null,x,cljs.core.PersistentArrayMap.EMPTY);
}));

(fipp.clojure.pprint.cljs$core$IFn$_invoke$arity$2 = (function (x,options){
return fipp.edn.pprint.call(null,x,cljs.core.merge.call(null,new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"symbols","symbols",1211743),fipp.clojure.default_symbols], null),options));
}));

(fipp.clojure.pprint.cljs$lang$maxFixedArity = 2);


//# sourceMappingURL=clojure.js.map
