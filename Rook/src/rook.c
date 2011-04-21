#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP rawmatch( SEXP needle, SEXP haystack, SEXP allMatches){
    int i, j, k, i1, n1, n2;
    Rbyte *x1, *x2;
    Rboolean all;
    SEXP ans, newans;

    if (TYPEOF(haystack) == RAWSXP){
	n1 = LENGTH(haystack);
	x1 = RAW(haystack);
    } else
	error_return("haystack must be a raw vector");

    if (isString(needle)){
	n2 = LENGTH(STRING_ELT(needle,0));
	x2 = (Rbyte *) CHAR(STRING_ELT(needle,0));
    } else if (TYPEOF(needle) == RAWSXP){
	n2 = LENGTH(needle);
	x2 = RAW(needle);
    } else
	error_return("needle must be a character or raw vector");

    if (!isLogical(allMatches))
	error_return("all must be a logical vector")
    all = LOGICAL(allMatches)[0];

    if (n2 > n1) return allocVector(INTSXP,0);

    k = 0;
    ans = allocVector(INTSXP, all? (int)(n1 / n2) : 1);
    
    for (i = 0; i < n1; i++){
	if (x1[i] == x2[0]){
	    for (j = 0; j < n2; j++){
		i1 = i+1;
		if (x1[i+j] != x2[j])
		    break;
	    }
	    if (j==n2){
		INTEGER(ans)[k++] = i + 1;
		if (!all) return ans;
	    }
	}
    }
    if (k == LENGTH(ans)) return ans;

    newans = allocVector(INTSXP,k);
    while(k) INTEGER(newans)[--k] = INTEGER(ans)[k];
    return newans;
}

R_CallMethodDef callMethods[]  = {
	{"rawmatch", (DL_FUNC) &rawmatch, 3},
	{NULL, NULL, 0}
};

void R_init_Rook(DllInfo *info) {
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);
/*	R_useDynamicSymbols(info, FALSE);*/
}
