*** ASSOCIATION OF HOME AND NEIGHBOURHOOD CONDITIONS DURING LOCKDOWN WITH ANXIETY AND DEPRESSION 

* MULTIPLE IMPUTATION

* making vars binary
replace kz021 = kz021-1
replace c804  = c804-1
replace covid2yp_6000 = covid2yp_6000-1

* restricting to analytic sample
capture drop if a551 ==.  |  a636==. | ypresptoany==0  | ypresptoany==. | anyanxdata==0 | anydepdata==0
count

* mice model
mi set mlong

mi register imputed covid1yp_4065 covid2yp_4065  covid1yp_4080 covid2yp_4080  sdimd1 sdpopden1  sdgreenspace1 sdsf1 covid1yp_5025b covid1yp_3052b yphousetypebin yphouseholdbin fampsy3bin c804 b_sc_m covid2yp_6090 covid2yp_6000 FJCI1001 FJCI501b  FKDQ1030 FKDQ1010

mi impute chained (logit) covid1yp_5025b covid1yp_3052b yphousetypebin yphouseholdbin fampsy3bin c804 covid2yp_6000 FJCI1001 FJCI501b  FKDQ1030 FKDQ1010 (regress) sdimd1 sdpopden1  sdgreenspace1 sdsf1 (truncreg, ll(0) ul(26)) covid1yp_4065 covid2yp_4065 (truncreg, ll(0) ul(21)) covid1yp_4080 covid2yp_4080 (ologit)  b_sc_m covid2yp_6090  = a551 a636,  add(5) rseed(1234) force augment 

mi xeq 1 2 3: summarize covid1yp_4065 covid2yp_4065 covid1yp_4080 covid2yp_4080 sdimd1 sdpopden1  sdgreenspace1 sdsf1 covid1yp_3052b covid1yp_5025b yphousetypebin yphouseholdbin fampsy3bin c804 b_sc_m covid2yp_6090 covid2yp_6000


* standardizing home variables

* nature access
capture drop sdnature
egen sdnature=std(covid1yp_3052b)
sum sdnature
hist sdnature

* garden access
capture drop sdgarden
egen sdgarden=std(covid1yp_5025b)
sum sdgarden
hist sdgarden

*house type
capture drop sdhouse
egen sdhouse=std(yphousetypebin)
sum sdhouse
hist sdhouse

*living alone
capture drop sdalone
egen sdalone=std(yphouseholdbin)
sum sdalone
hist sdalone


*** MAIN ANALYSIS

*** home vars and anxiety and depression at Covid 1 and Covid 2
postutil clear
postfile mice str50(outcome exposure confounders) n effect pval lci1 uci1 str30(CI)  using "C:\pathname", every(1) replace

local conf1 ""
local conf2 "c804 fampsy3bin i.b_sc_m FJCI1001 FJCI501b  FKDQ1030 FKDQ1010 i.covid2yp_6090 covid2yp_6000"

foreach var of varlist covid1yp_4080 covid2yp_4080 covid1yp_4065 covid2yp_4065 {
	foreach var1 of varlist sdnature sdgarden sdhouse sdalone {
		forvalues i=1(1)2 {
mi estimate, dots: regress `var' `var1' `conf`i'' 
	
matrix define A=r(table)
local n=e(N)
local b=A[1,1]
local b: di %3.2f `b'
local pval=A[4,1]
local pval: di %7.3g `pval'
local lci=A[5,1]
local lci: di %3.2f `lci'
local uci=A[6,1]
local uci: di %3.2f `uci'


post mice ("`var'") ("`var1'") ("`conf`i''") (`n') (`b') (`pval')  (`lci') (`uci') ("`b' (`lci'-`uci')") 
}	
}
}


*** neighbourhood vars and anxiety and depression at Covid 1 and Covid 2
postutil clear
postfile mice str50(outcome exposure confounders) n effect pval lci1 uci1 str30(CI)  using "C:\pathname", every(1) replace

local conf1 ""
local conf2 "c804 fampsy3bin i.b_sc_m FJCI1001 FJCI501b  FKDQ1030 FKDQ1010 i.covid2yp_6090 covid2yp_6000"

foreach var of varlist covid1yp_4080 covid2yp_4080 covid1yp_4065 covid2yp_4065 {
	foreach var1 of varlist sdimd1 sdpopden1  sdgreenspace1 sdsf1 {
		forvalues i=1(1)2 {
mi estimate, dots: regress `var' `var1' `conf`i'' 
	
matrix define A=r(table)
local n=e(N)
local b=A[1,1]
local b: di %3.2f `b'
local pval=A[4,1]
local pval: di %7.3g `pval'
local lci=A[5,1]
local lci: di %3.2f `lci'
local uci=A[6,1]
local uci: di %3.2f `uci'


post mice ("`var'") ("`var1'") ("`conf`i''") (`n') (`b') (`pval') (`lci') (`uci') ("`b' (`lci'-`uci')") 
}	
}
}

