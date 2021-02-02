// Compiled by ClojureScript 1.10.773 {:target :nodejs}
goog.provide('cljs_test_runner.gen');
goog.require('cljs.core');
goog.require('doo.runner');
goog.require('hello_world_test');
cljs_test_runner.gen.var__GT_sym = (function cljs_test_runner$gen$var__GT_sym(var$){
return cljs.core.symbol.call(null,new cljs.core.Keyword(null,"ns","ns",441598760).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var$)),new cljs.core.Keyword(null,"name","name",1843675177).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var$)));
});
cljs_test_runner.gen.var_filter = (function cljs_test_runner$gen$var_filter(p__2134){
var map__2135 = p__2134;
var map__2135__$1 = (((((!((map__2135 == null))))?(((((map__2135.cljs$lang$protocol_mask$partition0$ & (64))) || ((cljs.core.PROTOCOL_SENTINEL === map__2135.cljs$core$ISeq$))))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__2135):map__2135);
var var$ = cljs.core.get.call(null,map__2135__$1,new cljs.core.Keyword(null,"var","var",-769682797));
var include = cljs.core.get.call(null,map__2135__$1,new cljs.core.Keyword(null,"include","include",153360230));
var exclude = cljs.core.get.call(null,map__2135__$1,new cljs.core.Keyword(null,"exclude","exclude",-1230250334));
var test_specific = (cljs.core.truth_(var$)?cljs.core.comp.call(null,var$,cljs_test_runner.gen.var__GT_sym):cljs.core.constantly.call(null,true));
var test_inclusion = (cljs.core.truth_(include)?(function (p1__2131_SHARP_){
return cljs.core.apply.call(null,cljs.core.some_fn,include).call(null,cljs.core.meta.call(null,p1__2131_SHARP_));
}):cljs.core.constantly.call(null,true));
var test_exclusion = (cljs.core.truth_(exclude)?(function (p1__2132_SHARP_){
return cljs.core.complement.call(null,cljs.core.apply.call(null,cljs.core.some_fn,exclude)).call(null,cljs.core.meta.call(null,p1__2132_SHARP_));
}):cljs.core.constantly.call(null,true));
return (function (p1__2133_SHARP_){
var and__4115__auto__ = test_specific.call(null,p1__2133_SHARP_);
if(cljs.core.truth_(and__4115__auto__)){
var and__4115__auto____$1 = test_inclusion.call(null,p1__2133_SHARP_);
if(cljs.core.truth_(and__4115__auto____$1)){
return test_exclusion.call(null,p1__2133_SHARP_);
} else {
return and__4115__auto____$1;
}
} else {
return and__4115__auto__;
}
});
});
cljs_test_runner.gen.filter_vars_BANG_ = (function cljs_test_runner$gen$filter_vars_BANG_(ns_syms,filter_fn){
var seq__2137 = cljs.core.seq.call(null,ns_syms);
var chunk__2138 = null;
var count__2139 = (0);
var i__2140 = (0);
while(true){
if((i__2140 < count__2139)){
var ns_sym = cljs.core._nth.call(null,chunk__2138,i__2140);
var seq__2173_2205 = cljs.core.seq.call(null,ns_sym);
var chunk__2174_2206 = null;
var count__2175_2207 = (0);
var i__2176_2208 = (0);
while(true){
if((i__2176_2208 < count__2175_2207)){
var vec__2183_2209 = cljs.core._nth.call(null,chunk__2174_2206,i__2176_2208);
var __2210 = cljs.core.nth.call(null,vec__2183_2209,(0),null);
var var_2211 = cljs.core.nth.call(null,vec__2183_2209,(1),null);
if(cljs.core.truth_(new cljs.core.Keyword(null,"test","test",577538877).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var_2211)))){
if(cljs.core.not.call(null,filter_fn.call(null,var_2211))){
(cljs.core.deref.call(null,var_2211).cljs$lang$test = null);
} else {
}
} else {
}


var G__2212 = seq__2173_2205;
var G__2213 = chunk__2174_2206;
var G__2214 = count__2175_2207;
var G__2215 = (i__2176_2208 + (1));
seq__2173_2205 = G__2212;
chunk__2174_2206 = G__2213;
count__2175_2207 = G__2214;
i__2176_2208 = G__2215;
continue;
} else {
var temp__5735__auto___2216 = cljs.core.seq.call(null,seq__2173_2205);
if(temp__5735__auto___2216){
var seq__2173_2217__$1 = temp__5735__auto___2216;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__2173_2217__$1)){
var c__4556__auto___2218 = cljs.core.chunk_first.call(null,seq__2173_2217__$1);
var G__2219 = cljs.core.chunk_rest.call(null,seq__2173_2217__$1);
var G__2220 = c__4556__auto___2218;
var G__2221 = cljs.core.count.call(null,c__4556__auto___2218);
var G__2222 = (0);
seq__2173_2205 = G__2219;
chunk__2174_2206 = G__2220;
count__2175_2207 = G__2221;
i__2176_2208 = G__2222;
continue;
} else {
var vec__2186_2223 = cljs.core.first.call(null,seq__2173_2217__$1);
var __2224 = cljs.core.nth.call(null,vec__2186_2223,(0),null);
var var_2225 = cljs.core.nth.call(null,vec__2186_2223,(1),null);
if(cljs.core.truth_(new cljs.core.Keyword(null,"test","test",577538877).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var_2225)))){
if(cljs.core.not.call(null,filter_fn.call(null,var_2225))){
(cljs.core.deref.call(null,var_2225).cljs$lang$test = null);
} else {
}
} else {
}


var G__2226 = cljs.core.next.call(null,seq__2173_2217__$1);
var G__2227 = null;
var G__2228 = (0);
var G__2229 = (0);
seq__2173_2205 = G__2226;
chunk__2174_2206 = G__2227;
count__2175_2207 = G__2228;
i__2176_2208 = G__2229;
continue;
}
} else {
}
}
break;
}


var G__2230 = seq__2137;
var G__2231 = chunk__2138;
var G__2232 = count__2139;
var G__2233 = (i__2140 + (1));
seq__2137 = G__2230;
chunk__2138 = G__2231;
count__2139 = G__2232;
i__2140 = G__2233;
continue;
} else {
var temp__5735__auto__ = cljs.core.seq.call(null,seq__2137);
if(temp__5735__auto__){
var seq__2137__$1 = temp__5735__auto__;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__2137__$1)){
var c__4556__auto__ = cljs.core.chunk_first.call(null,seq__2137__$1);
var G__2234 = cljs.core.chunk_rest.call(null,seq__2137__$1);
var G__2235 = c__4556__auto__;
var G__2236 = cljs.core.count.call(null,c__4556__auto__);
var G__2237 = (0);
seq__2137 = G__2234;
chunk__2138 = G__2235;
count__2139 = G__2236;
i__2140 = G__2237;
continue;
} else {
var ns_sym = cljs.core.first.call(null,seq__2137__$1);
var seq__2189_2238 = cljs.core.seq.call(null,ns_sym);
var chunk__2190_2239 = null;
var count__2191_2240 = (0);
var i__2192_2241 = (0);
while(true){
if((i__2192_2241 < count__2191_2240)){
var vec__2199_2242 = cljs.core._nth.call(null,chunk__2190_2239,i__2192_2241);
var __2243 = cljs.core.nth.call(null,vec__2199_2242,(0),null);
var var_2244 = cljs.core.nth.call(null,vec__2199_2242,(1),null);
if(cljs.core.truth_(new cljs.core.Keyword(null,"test","test",577538877).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var_2244)))){
if(cljs.core.not.call(null,filter_fn.call(null,var_2244))){
(cljs.core.deref.call(null,var_2244).cljs$lang$test = null);
} else {
}
} else {
}


var G__2245 = seq__2189_2238;
var G__2246 = chunk__2190_2239;
var G__2247 = count__2191_2240;
var G__2248 = (i__2192_2241 + (1));
seq__2189_2238 = G__2245;
chunk__2190_2239 = G__2246;
count__2191_2240 = G__2247;
i__2192_2241 = G__2248;
continue;
} else {
var temp__5735__auto___2249__$1 = cljs.core.seq.call(null,seq__2189_2238);
if(temp__5735__auto___2249__$1){
var seq__2189_2250__$1 = temp__5735__auto___2249__$1;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__2189_2250__$1)){
var c__4556__auto___2251 = cljs.core.chunk_first.call(null,seq__2189_2250__$1);
var G__2252 = cljs.core.chunk_rest.call(null,seq__2189_2250__$1);
var G__2253 = c__4556__auto___2251;
var G__2254 = cljs.core.count.call(null,c__4556__auto___2251);
var G__2255 = (0);
seq__2189_2238 = G__2252;
chunk__2190_2239 = G__2253;
count__2191_2240 = G__2254;
i__2192_2241 = G__2255;
continue;
} else {
var vec__2202_2256 = cljs.core.first.call(null,seq__2189_2250__$1);
var __2257 = cljs.core.nth.call(null,vec__2202_2256,(0),null);
var var_2258 = cljs.core.nth.call(null,vec__2202_2256,(1),null);
if(cljs.core.truth_(new cljs.core.Keyword(null,"test","test",577538877).cljs$core$IFn$_invoke$arity$1(cljs.core.meta.call(null,var_2258)))){
if(cljs.core.not.call(null,filter_fn.call(null,var_2258))){
(cljs.core.deref.call(null,var_2258).cljs$lang$test = null);
} else {
}
} else {
}


var G__2259 = cljs.core.next.call(null,seq__2189_2250__$1);
var G__2260 = null;
var G__2261 = (0);
var G__2262 = (0);
seq__2189_2238 = G__2259;
chunk__2190_2239 = G__2260;
count__2191_2240 = G__2261;
i__2192_2241 = G__2262;
continue;
}
} else {
}
}
break;
}


var G__2263 = cljs.core.next.call(null,seq__2137__$1);
var G__2264 = null;
var G__2265 = (0);
var G__2266 = (0);
seq__2137 = G__2263;
chunk__2138 = G__2264;
count__2139 = G__2265;
i__2140 = G__2266;
continue;
}
} else {
return null;
}
}
break;
}
});
cljs_test_runner.gen.filter_vars_BANG_.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.into.call(null,cljs.core.PersistentArrayMap.EMPTY,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.symbol.call(null,"test-hello-world"),new cljs.core.Var(function(){return hello_world_test.test_hello_world;},new cljs.core.Symbol("hello-world-test","test-hello-world","hello-world-test/test-hello-world",71552555,null),cljs.core.PersistentHashMap.fromArrays([new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Keyword(null,"name","name",1843675177),new cljs.core.Keyword(null,"file","file",-1269645878),new cljs.core.Keyword(null,"end-column","end-column",1425389514),new cljs.core.Keyword(null,"column","column",2078222095),new cljs.core.Keyword(null,"line","line",212345235),new cljs.core.Keyword(null,"end-line","end-line",1837326455),new cljs.core.Keyword(null,"arglists","arglists",1661989754),new cljs.core.Keyword(null,"doc","doc",1913296891),new cljs.core.Keyword(null,"test","test",577538877)],[new cljs.core.Symbol(null,"hello-world-test","hello-world-test",399832352,null),new cljs.core.Symbol(null,"test-hello-world","test-hello-world",-1577943981,null),"/Users/jlynch/Exercism/clojurescript/hello-world/test/hello_world_test.cljs",26,1,5,5,cljs.core.List.EMPTY,null,(cljs.core.truth_(hello_world_test.test_hello_world)?hello_world_test.test_hello_world.cljs$lang$test:null)]))], null)], null))], null),cljs_test_runner.gen.var_filter.call(null,new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"var","var",-769682797),null,new cljs.core.Keyword(null,"include","include",153360230),null,new cljs.core.Keyword(null,"exclude","exclude",-1230250334),null], null)));
doo.runner.set_entry_point_BANG_.call(null,((doo.runner.karma_QMARK_.call(null))?(function (tc__2123__auto__){
jx.reporter.karma.start.call(null,tc__2123__auto__,1);

return cljs.test.run_block.call(null,(function (){var env2267 = cljs.test.empty_env.call(null,new cljs.core.Keyword("jx.reporter.karma","karma","jx.reporter.karma/karma",404831826));
var summary2268 = cljs.core.volatile_BANG_.call(null,new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"summary","summary",380847952),new cljs.core.Keyword(null,"fail","fail",1706214930),(0),new cljs.core.Keyword(null,"error","error",-978969032),(0),new cljs.core.Keyword(null,"pass","pass",1574159993),(0),new cljs.core.Keyword(null,"test","test",577538877),(0)], null));
return cljs.core.concat.call(null,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
cljs.test.set_env_BANG_.call(null,env2267);

cljs.test.do_report.call(null,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Symbol(null,"hello-world-test","hello-world-test",399832352,null),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"begin-test-ns","begin-test-ns",-1701237033)], null));

return cljs.test.block.call(null,(function (){var env__1960__auto__ = cljs.test.get_current_env.call(null);
return cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
if((env__1960__auto__ == null)){
cljs.test.set_env_BANG_.call(null,cljs.test.empty_env.call(null));
} else {
}


return null;
})], null),cljs.test.test_vars_block.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Var(function(){return hello_world_test.test_hello_world;},new cljs.core.Symbol("hello-world-test","test-hello-world","hello-world-test/test-hello-world",71552555,null),cljs.core.PersistentHashMap.fromArrays([new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Keyword(null,"name","name",1843675177),new cljs.core.Keyword(null,"file","file",-1269645878),new cljs.core.Keyword(null,"end-column","end-column",1425389514),new cljs.core.Keyword(null,"column","column",2078222095),new cljs.core.Keyword(null,"line","line",212345235),new cljs.core.Keyword(null,"end-line","end-line",1837326455),new cljs.core.Keyword(null,"arglists","arglists",1661989754),new cljs.core.Keyword(null,"doc","doc",1913296891),new cljs.core.Keyword(null,"test","test",577538877)],[new cljs.core.Symbol(null,"hello-world-test","hello-world-test",399832352,null),new cljs.core.Symbol(null,"test-hello-world","test-hello-world",-1577943981,null),"/Users/jlynch/Exercism/clojurescript/hello-world/test/hello_world_test.cljs",26,1,5,5,cljs.core.List.EMPTY,null,(cljs.core.truth_(hello_world_test.test_hello_world)?hello_world_test.test_hello_world.cljs$lang$test:null)]))], null)),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
if((env__1960__auto__ == null)){
return cljs.test.clear_env_BANG_.call(null);
} else {
return null;
}
})], null));
})());
}),(function (){
return cljs.test.do_report.call(null,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Symbol(null,"hello-world-test","hello-world-test",399832352,null),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"end-test-ns","end-test-ns",1620675645)], null));
})], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
return cljs.core._vreset_BANG_.call(null,summary2268,cljs.core.partial.call(null,cljs.core.merge_with,cljs.core._PLUS_).call(null,cljs.core._deref.call(null,summary2268),new cljs.core.Keyword(null,"report-counters","report-counters",-1702609242).cljs$core$IFn$_invoke$arity$1(cljs.test.get_and_clear_env_BANG_.call(null))));
})], null)),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
cljs.test.set_env_BANG_.call(null,env2267);

cljs.test.do_report.call(null,cljs.core.deref.call(null,summary2268));

cljs.test.report.call(null,cljs.core.assoc.call(null,cljs.core.deref.call(null,summary2268),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"end-run-tests","end-run-tests",267300563)));

return cljs.test.clear_env_BANG_.call(null);
})], null));
})());
}):(function (){
return cljs.test.run_block.call(null,(function (){var env2269 = cljs.test.empty_env.call(null);
var summary2270 = cljs.core.volatile_BANG_.call(null,new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"summary","summary",380847952),new cljs.core.Keyword(null,"fail","fail",1706214930),(0),new cljs.core.Keyword(null,"error","error",-978969032),(0),new cljs.core.Keyword(null,"pass","pass",1574159993),(0),new cljs.core.Keyword(null,"test","test",577538877),(0)], null));
return cljs.core.concat.call(null,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
cljs.test.set_env_BANG_.call(null,env2269);

cljs.test.do_report.call(null,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Symbol(null,"hello-world-test","hello-world-test",399832352,null),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"begin-test-ns","begin-test-ns",-1701237033)], null));

return cljs.test.block.call(null,(function (){var env__1960__auto__ = cljs.test.get_current_env.call(null);
return cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
if((env__1960__auto__ == null)){
cljs.test.set_env_BANG_.call(null,cljs.test.empty_env.call(null));
} else {
}


return null;
})], null),cljs.test.test_vars_block.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Var(function(){return hello_world_test.test_hello_world;},new cljs.core.Symbol("hello-world-test","test-hello-world","hello-world-test/test-hello-world",71552555,null),cljs.core.PersistentHashMap.fromArrays([new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Keyword(null,"name","name",1843675177),new cljs.core.Keyword(null,"file","file",-1269645878),new cljs.core.Keyword(null,"end-column","end-column",1425389514),new cljs.core.Keyword(null,"column","column",2078222095),new cljs.core.Keyword(null,"line","line",212345235),new cljs.core.Keyword(null,"end-line","end-line",1837326455),new cljs.core.Keyword(null,"arglists","arglists",1661989754),new cljs.core.Keyword(null,"doc","doc",1913296891),new cljs.core.Keyword(null,"test","test",577538877)],[new cljs.core.Symbol(null,"hello-world-test","hello-world-test",399832352,null),new cljs.core.Symbol(null,"test-hello-world","test-hello-world",-1577943981,null),"/Users/jlynch/Exercism/clojurescript/hello-world/test/hello_world_test.cljs",26,1,5,5,cljs.core.List.EMPTY,null,(cljs.core.truth_(hello_world_test.test_hello_world)?hello_world_test.test_hello_world.cljs$lang$test:null)]))], null)),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
if((env__1960__auto__ == null)){
return cljs.test.clear_env_BANG_.call(null);
} else {
return null;
}
})], null));
})());
}),(function (){
return cljs.test.do_report.call(null,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"ns","ns",441598760),new cljs.core.Symbol(null,"hello-world-test","hello-world-test",399832352,null),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"end-test-ns","end-test-ns",1620675645)], null));
})], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
return cljs.core._vreset_BANG_.call(null,summary2270,cljs.core.partial.call(null,cljs.core.merge_with,cljs.core._PLUS_).call(null,cljs.core._deref.call(null,summary2270),new cljs.core.Keyword(null,"report-counters","report-counters",-1702609242).cljs$core$IFn$_invoke$arity$1(cljs.test.get_and_clear_env_BANG_.call(null))));
})], null)),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){
cljs.test.set_env_BANG_.call(null,env2269);

cljs.test.do_report.call(null,cljs.core.deref.call(null,summary2270));

cljs.test.report.call(null,cljs.core.assoc.call(null,cljs.core.deref.call(null,summary2270),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"end-run-tests","end-run-tests",267300563)));

return cljs.test.clear_env_BANG_.call(null);
})], null));
})());
})));

//# sourceMappingURL=gen.js.map
