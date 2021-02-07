// Compiled by ClojureScript 1.10.773 {:target :nodejs}
goog.provide('cljs_test_runner.gen');
goog.require('cljs.core');
goog.require('doo.runner');
goog.require('two_fer_test');
cljs_test_runner.gen.var__GT_sym = (function cljs_test_runner$gen$var__GT_sym(var$){
return cljs.core.symbol.call(null,new cljs.core.Keyword(null,"ns","ns",441598760).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var$)),new cljs.core.Keyword(null,"name","name",1843675177).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var$)));
});
cljs_test_runner.gen.var_filter = (function cljs_test_runner$gen$var_filter(p__4139){
var map__4140 = p__4139;
var map__4140__$1 = (((((!((map__4140 == null))))?(((((map__4140.cljs$lang$protocol_mask$partition0$ & (64))) || ((cljs.core.PROTOCOL_SENTINEL === map__4140.cljs$core$ISeq$))))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__4140):map__4140);
var var$ = cljs.core.get.call(null,map__4140__$1,new cljs.core.Keyword(null,"var","var",-769682797));
var include = cljs.core.get.call(null,map__4140__$1,new cljs.core.Keyword(null,"include","include",153360230));
var exclude = cljs.core.get.call(null,map__4140__$1,new cljs.core.Keyword(null,"exclude","exclude",-1230250334));
var test_specific = (cljs.core.truth_(var$)?cljs.core.comp.call(null,var$,cljs_test_runner.gen.var__GT_sym):cljs.core.constantly.call(null,true));
var test_inclusion = (cljs.core.truth_(include)?(function (p1__4136_SHARP_){
return cljs.core.apply.call(null,cljs.core.some_fn,include).call(null,cljs.core.meta.call(null,p1__4136_SHARP_));
}):cljs.core.constantly.call(null,true));
var test_exclusion = (cljs.core.truth_(exclude)?(function (p1__4137_SHARP_){
return cljs.core.complement.call(null,cljs.core.apply.call(null,cljs.core.some_fn,exclude)).call(null,cljs.core.meta.call(null,p1__4137_SHARP_));
}):cljs.core.constantly.call(null,true));
return (function (p1__4138_SHARP_){
var and__4115__auto__ = test_specific.call(null,p1__4138_SHARP_);
if(cljs.core.truth_(and__4115__auto__)){
var and__4115__auto____$1 = test_inclusion.call(null,p1__4138_SHARP_);
if(cljs.core.truth_(and__4115__auto____$1)){
return test_exclusion.call(null,p1__4138_SHARP_);
} else {
return and__4115__auto____$1;
}
} else {
return and__4115__auto__;
}
});
});
cljs_test_runner.gen.filter_vars_BANG_ = (function cljs_test_runner$gen$filter_vars_BANG_(ns_syms,filter_fn){
var seq__4142 = cljs.core.seq.call(null,ns_syms);
var chunk__4143 = null;
var count__4144 = (0);
var i__4145 = (0);
while(true){
if((i__4145 < count__4144)){
var ns_sym = cljs.core._nth.call(null,chunk__4143,i__4145);
var seq__4178_4210 = cljs.core.seq.call(null,ns_sym);
var chunk__4179_4211 = null;
var count__4180_4212 = (0);
var i__4181_4213 = (0);
while(true){
if((i__4181_4213 < count__4180_4212)){
var vec__4188_4214 = cljs.core._nth.call(null,chunk__4179_4211,i__4181_4213);
var __4215 = cljs.core.nth.call(null,vec__4188_4214,(0),null);
var var_4216 = cljs.core.nth.call(null,vec__4188_4214,(1),null);
if(cljs.core.truth_(new cljs.core.Keyword(null,"test","test",577538877).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var_4216)))){
if(cljs.core.not.call(null,filter_fn.call(null,var_4216))){
(cljs.core.deref.call(null,var_4216).cljs$lang$test = null);
} else {
}
} else {
}


var G__4217 = seq__4178_4210;
var G__4218 = chunk__4179_4211;
var G__4219 = count__4180_4212;
var G__4220 = (i__4181_4213 + (1));
seq__4178_4210 = G__4217;
chunk__4179_4211 = G__4218;
count__4180_4212 = G__4219;
i__4181_4213 = G__4220;
continue;
} else {
var temp__5735__auto___4221 = cljs.core.seq.call(null,seq__4178_4210);
if(temp__5735__auto___4221){
var seq__4178_4222__$1 = temp__5735__auto___4221;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__4178_4222__$1)){
var c__4556__auto___4223 = cljs.core.chunk_first.call(null,seq__4178_4222__$1);
var G__4224 = cljs.core.chunk_rest.call(null,seq__4178_4222__$1);
var G__4225 = c__4556__auto___4223;
var G__4226 = cljs.core.count.call(null,c__4556__auto___4223);
var G__4227 = (0);
seq__4178_4210 = G__4224;
chunk__4179_4211 = G__4225;
count__4180_4212 = G__4226;
i__4181_4213 = G__4227;
continue;
} else {
var vec__4191_4228 = cljs.core.first.call(null,seq__4178_4222__$1);
var __4229 = cljs.core.nth.call(null,vec__4191_4228,(0),null);
var var_4230 = cljs.core.nth.call(null,vec__4191_4228,(1),null);
if(cljs.core.truth_(new cljs.core.Keyword(null,"test","test",577538877).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var_4230)))){
if(cljs.core.not.call(null,filter_fn.call(null,var_4230))){
(cljs.core.deref.call(null,var_4230).cljs$lang$test = null);
} else {
}
} else {
}


var G__4231 = cljs.core.next.call(null,seq__4178_4222__$1);
var G__4232 = null;
var G__4233 = (0);
var G__4234 = (0);
seq__4178_4210 = G__4231;
chunk__4179_4211 = G__4232;
count__4180_4212 = G__4233;
i__4181_4213 = G__4234;
continue;
}
} else {
}
}
break;
}


var G__4235 = seq__4142;
var G__4236 = chunk__4143;
var G__4237 = count__4144;
var G__4238 = (i__4145 + (1));
seq__4142 = G__4235;
chunk__4143 = G__4236;
count__4144 = G__4237;
i__4145 = G__4238;
continue;
} else {
var temp__5735__auto__ = cljs.core.seq.call(null,seq__4142);
if(temp__5735__auto__){
var seq__4142__$1 = temp__5735__auto__;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__4142__$1)){
var c__4556__auto__ = cljs.core.chunk_first.call(null,seq__4142__$1);
var G__4239 = cljs.core.chunk_rest.call(null,seq__4142__$1);
var G__4240 = c__4556__auto__;
var G__4241 = cljs.core.count.call(null,c__4556__auto__);
var G__4242 = (0);
seq__4142 = G__4239;
chunk__4143 = G__4240;
count__4144 = G__4241;
i__4145 = G__4242;
continue;
} else {
var ns_sym = cljs.core.first.call(null,seq__4142__$1);
var seq__4194_4243 = cljs.core.seq.call(null,ns_sym);
var chunk__4195_4244 = null;
var count__4196_4245 = (0);
var i__4197_4246 = (0);
while(true){
if((i__4197_4246 < count__4196_4245)){
var vec__4204_4247 = cljs.core._nth.call(null,chunk__4195_4244,i__4197_4246);
var __4248 = cljs.core.nth.call(null,vec__4204_4247,(0),null);
var var_4249 = cljs.core.nth.call(null,vec__4204_4247,(1),null);
if(cljs.core.truth_(new cljs.core.Keyword(null,"test","test",577538877).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var_4249)))){
if(cljs.core.not.call(null,filter_fn.call(null,var_4249))){
(cljs.core.deref.call(null,var_4249).cljs$lang$test = null);
} else {
}
} else {
}


var G__4250 = seq__4194_4243;
var G__4251 = chunk__4195_4244;
var G__4252 = count__4196_4245;
var G__4253 = (i__4197_4246 + (1));
seq__4194_4243 = G__4250;
chunk__4195_4244 = G__4251;
count__4196_4245 = G__4252;
i__4197_4246 = G__4253;
continue;
} else {
var temp__5735__auto___4254__$1 = cljs.core.seq.call(null,seq__4194_4243);
if(temp__5735__auto___4254__$1){
var seq__4194_4255__$1 = temp__5735__auto___4254__$1;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__4194_4255__$1)){
var c__4556__auto___4256 = cljs.core.chunk_first.call(null,seq__4194_4255__$1);
var G__4257 = cljs.core.chunk_rest.call(null,seq__4194_4255__$1);
var G__4258 = c__4556__auto___4256;
var G__4259 = cljs.core.count.call(null,c__4556__auto___4256);
var G__4260 = (0);
seq__4194_4243 = G__4257;
chunk__4195_4244 = G__4258;
count__4196_4245 = G__4259;
i__4197_4246 = G__4260;
continue;
} else {
var vec__4207_4261 = cljs.core.first.call(null,seq__4194_4255__$1);
var __4262 = cljs.core.nth.call(null,vec__4207_4261,(0),null);
var var_4263 = cljs.core.nth.call(null,vec__4207_4261,(1),null);
if(cljs.core.truth_(new cljs.core.Keyword(null,"test","test",577538877).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var_4263)))){
if(cljs.core.not.call(null,filter_fn.call(null,var_4263))){
(cljs.core.deref.call(null,var_4263).cljs$lang$test = null);
} else {
}
} else {
}


var G__4264 = cljs.core.next.call(null,seq__4194_4255__$1);
var G__4265 = null;
var G__4266 = (0);
var G__4267 = (0);
seq__4194_4243 = G__4264;
chunk__4195_4244 = G__4265;
count__4196_4245 = G__4266;
i__4197_4246 = G__4267;
continue;
}
} else {
}
}
break;
}


var G__4268 = cljs.core.next.call(null,seq__4142__$1);
var G__4269 = null;
var G__4270 = (0);
var G__4271 = (0);
seq__4142 = G__4268;
chunk__4143 = G__4269;
count__4144 = G__4270;
i__4145 = G__4271;
continue;
}
} else {
return null;
}
}
break;
}
});
cljs_test_runner.gen.filter_vars_BANG_.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.into.call(null,cljs.core.PersistentArrayMap.EMPTY,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.symbol.call(null,"two-fer-no-args-test"),new cljs.core.Var(function(){return two_fer_test.two_fer_no_args_test;},new cljs.core.Symbol("two-fer-test","two-fer-no-args-test","two-fer-test/two-fer-no-args-test",1191062948,null),cljs.core.PersistentHashMap.fromArrays([new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Keyword(null,"name","name",1843675177),new cljs.core.Keyword(null,"file","file",-1269645878),new cljs.core.Keyword(null,"end-column","end-column",1425389514),new cljs.core.Keyword(null,"column","column",2078222095),new cljs.core.Keyword(null,"line","line",212345235),new cljs.core.Keyword(null,"end-line","end-line",1837326455),new cljs.core.Keyword(null,"arglists","arglists",1661989754),new cljs.core.Keyword(null,"doc","doc",1913296891),new cljs.core.Keyword(null,"test","test",577538877)],[new cljs.core.Symbol(null,"two-fer-test","two-fer-test",-567472516,null),new cljs.core.Symbol(null,"two-fer-no-args-test","two-fer-no-args-test",812276401,null),"/Users/jlynch/Exercism/clojurescript/two-fer/test/two_fer_test.cljs",30,1,5,5,cljs.core.List.EMPTY,null,(cljs.core.truth_(two_fer_test.two_fer_no_args_test)?two_fer_test.two_fer_no_args_test.cljs$lang$test:null)]))], null)], null))], null),cljs_test_runner.gen.var_filter.call(null,new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"var","var",-769682797),null,new cljs.core.Keyword(null,"include","include",153360230),null,new cljs.core.Keyword(null,"exclude","exclude",-1230250334),null], null)));
doo.runner.set_entry_point_BANG_.call(null,((doo.runner.karma_QMARK_.call(null))?(function (tc__4128__auto__){
jx.reporter.karma.start.call(null,tc__4128__auto__,1);

return cljs.test.run_block.call(null,(function (){var env4272 = cljs.test.empty_env.call(null,new cljs.core.Keyword("jx.reporter.karma","karma","jx.reporter.karma/karma",404831826));
var summary4273 = cljs.core.volatile_BANG_.call(null,new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"summary","summary",380847952),new cljs.core.Keyword(null,"fail","fail",1706214930),(0),new cljs.core.Keyword(null,"error","error",-978969032),(0),new cljs.core.Keyword(null,"pass","pass",1574159993),(0),new cljs.core.Keyword(null,"test","test",577538877),(0)], null));
return cljs.core.concat.call(null,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
cljs.test.set_env_BANG_.call(null,env4272);

cljs.test.do_report.call(null,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Symbol(null,"two-fer-test","two-fer-test",-567472516,null),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"begin-test-ns","begin-test-ns",-1701237033)], null));

return cljs.test.block.call(null,(function (){var env__3166__auto__ = cljs.test.get_current_env.call(null);
return cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
if((env__3166__auto__ == null)){
cljs.test.set_env_BANG_.call(null,cljs.test.empty_env.call(null));
} else {
}


return null;
})], null),cljs.test.test_vars_block.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Var(function(){return two_fer_test.two_fer_no_args_test;},new cljs.core.Symbol("two-fer-test","two-fer-no-args-test","two-fer-test/two-fer-no-args-test",1191062948,null),cljs.core.PersistentHashMap.fromArrays([new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Keyword(null,"name","name",1843675177),new cljs.core.Keyword(null,"file","file",-1269645878),new cljs.core.Keyword(null,"end-column","end-column",1425389514),new cljs.core.Keyword(null,"column","column",2078222095),new cljs.core.Keyword(null,"line","line",212345235),new cljs.core.Keyword(null,"end-line","end-line",1837326455),new cljs.core.Keyword(null,"arglists","arglists",1661989754),new cljs.core.Keyword(null,"doc","doc",1913296891),new cljs.core.Keyword(null,"test","test",577538877)],[new cljs.core.Symbol(null,"two-fer-test","two-fer-test",-567472516,null),new cljs.core.Symbol(null,"two-fer-no-args-test","two-fer-no-args-test",812276401,null),"/Users/jlynch/Exercism/clojurescript/two-fer/test/two_fer_test.cljs",30,1,5,5,cljs.core.List.EMPTY,null,(cljs.core.truth_(two_fer_test.two_fer_no_args_test)?two_fer_test.two_fer_no_args_test.cljs$lang$test:null)]))], null)),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
if((env__3166__auto__ == null)){
return cljs.test.clear_env_BANG_.call(null);
} else {
return null;
}
})], null));
})());
}),(function (){
return cljs.test.do_report.call(null,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Symbol(null,"two-fer-test","two-fer-test",-567472516,null),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"end-test-ns","end-test-ns",1620675645)], null));
})], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
return cljs.core._vreset_BANG_.call(null,summary4273,cljs.core.partial.call(null,cljs.core.merge_with,cljs.core._PLUS_).call(null,cljs.core._deref.call(null,summary4273),new cljs.core.Keyword(null,"report-counters","report-counters",-1702609242).cljs$core$IFn$_invoke$arity$1(cljs.test.get_and_clear_env_BANG_.call(null))));
})], null)),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
cljs.test.set_env_BANG_.call(null,env4272);

cljs.test.do_report.call(null,cljs.core.deref.call(null,summary4273));

cljs.test.report.call(null,cljs.core.assoc.call(null,cljs.core.deref.call(null,summary4273),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"end-run-tests","end-run-tests",267300563)));

return cljs.test.clear_env_BANG_.call(null);
})], null));
})());
}):(function (){
return cljs.test.run_block.call(null,(function (){var env4274 = cljs.test.empty_env.call(null);
var summary4275 = cljs.core.volatile_BANG_.call(null,new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"summary","summary",380847952),new cljs.core.Keyword(null,"fail","fail",1706214930),(0),new cljs.core.Keyword(null,"error","error",-978969032),(0),new cljs.core.Keyword(null,"pass","pass",1574159993),(0),new cljs.core.Keyword(null,"test","test",577538877),(0)], null));
return cljs.core.concat.call(null,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
cljs.test.set_env_BANG_.call(null,env4274);

cljs.test.do_report.call(null,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Symbol(null,"two-fer-test","two-fer-test",-567472516,null),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"begin-test-ns","begin-test-ns",-1701237033)], null));

return cljs.test.block.call(null,(function (){var env__3166__auto__ = cljs.test.get_current_env.call(null);
return cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
if((env__3166__auto__ == null)){
cljs.test.set_env_BANG_.call(null,cljs.test.empty_env.call(null));
} else {
}


return null;
})], null),cljs.test.test_vars_block.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Var(function(){return two_fer_test.two_fer_no_args_test;},new cljs.core.Symbol("two-fer-test","two-fer-no-args-test","two-fer-test/two-fer-no-args-test",1191062948,null),cljs.core.PersistentHashMap.fromArrays([new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Keyword(null,"name","name",1843675177),new cljs.core.Keyword(null,"file","file",-1269645878),new cljs.core.Keyword(null,"end-column","end-column",1425389514),new cljs.core.Keyword(null,"column","column",2078222095),new cljs.core.Keyword(null,"line","line",212345235),new cljs.core.Keyword(null,"end-line","end-line",1837326455),new cljs.core.Keyword(null,"arglists","arglists",1661989754),new cljs.core.Keyword(null,"doc","doc",1913296891),new cljs.core.Keyword(null,"test","test",577538877)],[new cljs.core.Symbol(null,"two-fer-test","two-fer-test",-567472516,null),new cljs.core.Symbol(null,"two-fer-no-args-test","two-fer-no-args-test",812276401,null),"/Users/jlynch/Exercism/clojurescript/two-fer/test/two_fer_test.cljs",30,1,5,5,cljs.core.List.EMPTY,null,(cljs.core.truth_(two_fer_test.two_fer_no_args_test)?two_fer_test.two_fer_no_args_test.cljs$lang$test:null)]))], null)),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
if((env__3166__auto__ == null)){
return cljs.test.clear_env_BANG_.call(null);
} else {
return null;
}
})], null));
})());
}),(function (){
return cljs.test.do_report.call(null,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Symbol(null,"two-fer-test","two-fer-test",-567472516,null),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"end-test-ns","end-test-ns",1620675645)], null));
})], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
return cljs.core._vreset_BANG_.call(null,summary4275,cljs.core.partial.call(null,cljs.core.merge_with,cljs.core._PLUS_).call(null,cljs.core._deref.call(null,summary4275),new cljs.core.Keyword(null,"report-counters","report-counters",-1702609242).cljs$core$IFn$_invoke$arity$1(cljs.test.get_and_clear_env_BANG_.call(null))));
})], null)),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
cljs.test.set_env_BANG_.call(null,env4274);

cljs.test.do_report.call(null,cljs.core.deref.call(null,summary4275));

cljs.test.report.call(null,cljs.core.assoc.call(null,cljs.core.deref.call(null,summary4275),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"end-run-tests","end-run-tests",267300563)));

return cljs.test.clear_env_BANG_.call(null);
})], null));
})());
})));

//# sourceMappingURL=gen.js.map
