*Figure 2
*****************************************
******* bid distribution by gender

twoway (histogram s01 if female_bid==0 & consentauction==1, width(200) start(0) color(navy%50) disc freq) (histogram s01 if  female_bid==1 & consentauction==1, width(200) start(0) color(maroon%50) disc freq), legend(order(1 "Men" 2 "Women")) xtitle("Willingness to pay for induction stoves (in Rupees)")



************************* cumulative distribution

cumul s01 if female_bid==0 & consentauction==1, gen(male_all) equal
cumul s01 if female_bid==1 & consentauction==1, gen(female_all)  equal

cumul s01 if treatment==0 & female_bid==0 & consentauction==1, gen(male_c) equal
cumul s01 if treatment==1 & female_bid==0 & consentauction==1, gen(male_t)  equal
cumul s01 if treatment==0 & female_bid==1 & consentauction==1, gen(female_c)  equal
cumul s01 if treatment==1 & female_bid==1 & consentauction==1, gen(female_t)  equal

 
*Figure 3
*******************************************
* by gender

twoway (line male_all s01 if consentauction==1, sort lcolor(blue)) (line female_all s01 if consentauction==1, sort lcolor(red)) , ytitle(Cumulative distribution) xtitle(Willingness to pay for induction stoves (in Rupees)) legend(order(1 "Men" 2 "Women" )) 


*Figure 4
*******************************************
* by gender and treatment

twoway (line male_c s01 if consentauction==1, sort lcolor(blue)) (line female_c s01 if consentauction==1, sort lcolor(red)) (line male_t s01 if consentauction==1, sort lcolor(blue) lpattern(dash)) (line female_t s01 if consentauction==1, sort lcolor(red) lpattern(dash)), ytitle(Cumulative distribution) xtitle(Willingness to pay for induction stoves (in Rupees)) legend(order(1 "Men - No sanitation intervention" 2 "Women - No sanitation intervention" 3 "Men - Sanitation intervention" 4 "Women - Sanitation intervention")) 


*Table 2
**********
bysort treatment: tab ever_latrine 
bysort treatment: tab hh_abandonment_all 

*Table 3
**********
g relation_to_head  = bidder_rth
replace relation_to_head = 3 if bidder_rth>2

label define rtoh 1 "Household-head" 2 "Spouse" 3 "Others"
label values relation_to_head rtoh

eststo a: estpost tab relation_to_head if consentauction==1 & female_bid==0, nototal
eststo b: estpost tab relation_to_head if consentauction==1 & female_bid==1, nototal

esttab a b, replace


*Table 4
**********

tab female_bid if consentauction==1
bysort female_bid: sum s01 
ttest s01, by(female_bid)

tabstat wealthindex fem_head  hh_size bidder_age bidder_10  salaried_bidder head_age  head_10 salaried_head  treatment if consentauction==1, by( female_bid) col(stat) long stat(mean sd)


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*********************************************************************** Regressions **************************************************************
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

*Table 5
******************OLS

reg s01 i.female_bid i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2  if consentauction==1, cluster(village_no)
eststo ols1a
estadd local fixed "Yes" , replace

reg s01 i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2  if consentauction==1, cluster(village_no)
eststo ols1b
estadd local fixed "Yes" , replace

reg s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2  if consentauction==1, cluster(village_no)
eststo ols1c
estadd local fixed "Yes" , replace

esttab ols1a ols1b ols1c, replace  b(%9.2f) se(%9.2f) keep( 1.female_bid 1.treatment 1.female_bid#1.treatment) star(* 0.10  ** 0.05 *** 0.01) coeflabels(1.female_bid "Bidder is a woman (d)" 1.treatment "Sanitation campaign (d)" 1.female_bid#1.treatment "Bidder is a woman (d) \times Sanitation campaign (d)")  s(fixed N r2_a rmse, label("HH control variables" "N" "Adj R-squared" "RMSE" )) nonotes


*Table 6
****************** PSM 

*** psm -- radius

	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2   if consentauction==1
		more
		margins, dydx(_all)
		more
		predict double score if consentauction==1
		more	
		sum score
		
		density2 score, group(female_bid)
		psgraph, treated(female_bid) pscore(score)

	psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) caliper(.1) radius common 
		more
		summarize _support if female_bid

	pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head , sum graph
	gen psm_wt0 = _weight 
	egen psm_wt = max(psm_wt0), by(qreid)
	
** Hotelling's T-test

hotelling head_10 head_age salaried_head fem_head wealthindex hh_size  size2 [aw=_weight], by(_treated)
			
regress s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1, cluster(village_no)

********** checks
regress s01 i.female_bid##i.hh_abandonment_all i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1, cluster(village_no)


	gen delta = s01 - _s01 if _treat==1 & _support==1 & consentauction==1
	rbounds delta, gamma(1 (0.5) 3) 


drop score _pscore _treated _suppor$xvarst _weight _s01 psm_wt0 psm_wt 
 
 
 
 *** psm -- kernel
 
	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2   if consentauction==1
	more
	margins, dydx(_all)
	more
	predict double score if consentauction==1
	more
	
	density2 score, group(female_bid)
	psgraph, treated(female_bid) pscore(score)
	
psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) bw(0.07) kernel k(normal) common 

	more
	summarize _support if female_bid
	
pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head  , sum graph
gen psm_wt0 = _weight 
egen psm_wt = max(psm_wt0), by(qreid)


hotelling head_10 head_age salaried_head fem_head wealthindex hh_size  size2 [aw=_weight], by(_treated)

regress s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1, cluster(village_no)
 
********** checks
regress s01 i.female_bid##i.hh_abandonment_all i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1, cluster(village_no)

	gen delta1 = s01 - _s01 if _treat==1 & _support==1 & consentauction==1
	rbounds delta1, gamma(1 (0.5) 3) 
  
drop score _pscore _treated _support _weight _s01 psm_wt0 psm_wt 
 
******************IV regressions  

*********** in paper Table 7
  

ivregress 2sls s01 i.female_bid i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2 (i.hh_abandonment_all i.female_bid#i.hh_abandonment_all = i.treatment i.female_bid#i.treatment) if consentauction==1, cluster(village_no) first 

estat endog
 
 
* Appendix
***********************************************

*Table A2 
**************************
*** from PSM estimates

*Table A3
**************************

tab firstprice
bysort r01: tab firstprice

probit r01 treatment
eststo A1: margins, dydx(*)  post
probit r01 treatment i.female_bid i.bidder_10 bidder_age salaried_bidder head_10 head_age salaried_head fem_head wealthindex hh_size  size2
eststo A2: margins, dydx(*) post

esttab A1 A2 , replace  b(%9.2f) se(%9.2f) star(* 0.10  ** 0.05 *** 0.01) coeflabels(treatment "Sanitation campaigns (d)"  1.female_bid "Bidder is a woman (d)" 1.bidder_10 "Bidder completed grade 10" bidder_age "Bidder's age" salaried_bidder "Salaried bidder" head_10 "Household head" head_age "Head's age" salaried_head "Salaried head" fem_head "Household head is a woman" wealthindex "Household wealth index" hh_size "Household size" size2 "Square of household size") 


*Table A4
******************OLS *** non-compliant
reg s01 i.female_bid i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2  if consentauction==1 & r01!=0, cluster(village_no)

reg s01 i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2  if consentauction==1 & r01!=0, cluster(village_no)

reg s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2  if consentauction==1 & r01!=0, cluster(village_no)


*Table A5
******************OLS *** caste
reg s01 i.female_bid i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2 i.caste_code if consentauction==1 , cluster(village_no)
eststo ols2a
estadd local fixed "Yes" , replace
reg s01 i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2 i.caste_code if consentauction==1 , cluster(village_no)
eststo ols2b
estadd local fixed "Yes" , replace
reg s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2 i.caste_code if consentauction==1 , cluster(village_no)
eststo ols2c
estadd local fixed "Yes" , replace

esttab ols2a ols2b ols2c, replace  b(%9.2f) se(%9.2f) keep( 1.female_bid 1.treatment 1.female_bid#1.treatment) star(* 0.10  ** 0.05 *** 0.01) coeflabels(1.female_bid "Bidder is a woman (d)" 1.treatment "Sanitation campaign (d)" 1.female_bid#1.treatment "Bidder is a woman (d) \times Sanitation campaign (d)")  s(fixed N r2 rmse, label("HH control variables" "N" "R-squared" "RMSE" )) nonotes



*Table A6
****************** PSM *** non-compliant

*** psm -- radius

	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2   if consentauction==1 & r01!=0
		more
		margins, dydx(_all)
		more
		predict double score if consentauction==1 & r01!=0
		more	
		sum score
		
		density2 score, group(female_bid)
		psgraph, treated(female_bid) pscore(score)

	psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) caliper(.1) radius common 
		more
		summarize _support if female_bid

	pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head , sum graph
	gen psm_wt0 = _weight 
	egen psm_wt = max(psm_wt0), by(qreid)
	
regress s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1  & r01!=0, cluster(village_no)

********** checks
regress s01 i.female_bid##i.hh_abandonment_all i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1 & r01!=0, cluster(village_no)


drop score _pscore _treated _support _weight _s01 psm_wt0 psm_wt 
 
 
 
 *** psm -- kernel
 
	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2   if consentauction==1 & r01!=0
	more
	margins, dydx(_all)
	more
	predict double score if consentauction==1 & r01!=0
	more
	
	density2 score, group(female_bid)
	psgraph, treated(female_bid) pscore(score)
	
psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) bw(0.07) kernel k(normal) common 

	more
	summarize _support if female_bid
	
pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head  , sum graph
gen psm_wt0 = _weight 
egen psm_wt = max(psm_wt0), by(qreid)

regress s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1 & r01!=0, cluster(village_no)
 
********** checks
regress s01 i.female_bid##i.hh_abandonment_all i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1 & r01!=0, cluster(village_no)
 
 
drop score _pscore _treated _support _weight _s01 psm_wt0 psm_wt 



*Table A7
*** Caste variable PSM

logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2 i.caste_code  if consentauction==1
	more
	margins, dydx(_all)
	more
	predict double score if consentauction==1
	more	
	sum score
		
	density2 score, group(female_bid)
	psgraph, treated(female_bid) pscore(score)

psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) caliper(.1) radius common 
	more
	summarize _support if female_bid

pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head , sum graph
gen psm_wt0 = _weight 
egen psm_wt = max(psm_wt0), by(qreid)
			

regress s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2 i.caste_code [w=psm_wt] if consentauction==1, cluster(village_no)
********** checks
regress s01 i.female_bid##i.hh_abandonment_all i.bidder_10 bidder_age salaried_bidder head_10 head_age salaried_head fem_head wealthindex hh_size size2 i.caste_code [w=psm_wt] if consentauction==1, cluster(village_no)

drop score _pscore _treated _suppor$xvarst _weight _s01 psm_wt0 psm_wt 
 
 
 
 *** psm -- kernel
 
	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2 i.caste_code  if consentauction==1
	more
	margins, dydx(_all)
	more
	predict double score if consentauction==1
	more
	
	density2 score, group(female_bid)
	psgraph, treated(female_bid) pscore(score)
	
psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) bw(0.07) kernel k(normal) common 

	more
	summarize _support if female_bid
	
pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head  , sum graph
gen psm_wt0 = _weight 
egen psm_wt = max(psm_wt0), by(qreid)


regress s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2 i.caste_code [w=psm_wt] if consentauction==1, cluster(village_no)
********** checks
regress s01 i.female_bid##i.hh_abandonment_all i.bidder_10 bidder_age salaried_bidder head_10 head_age salaried_head fem_head wealthindex hh_size size2 i.caste_code [w=psm_wt] if consentauction==1, cluster(village_no)

drop score _pscore _treated _suppor$xvarst _weight _s01 psm_wt0 psm_wt 
 


	 
*Table A8 
*IV *** non-compliant
******************

ivregress 2sls s01 i.female_bid i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2 (i.hh_abandonment_all i.female_bid#i.hh_abandonment_all = i.treatment i.female_bid#i.treatment) if consentauction==1 & r01!=0, cluster(village_no) first 


*Table A9 
*IV *** caste variable
******************

ivregress 2sls s01 i.female_bid i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2 i.caste_code (i.hh_abandonment_all i.female_bid#i.hh_abandonment_all = i.treatment i.female_bid#i.treatment) if consentauction==1,  first 


*** Table A12 *** OLS --no control vars

reg s01 i.female_bid  if consentauction==1, cluster(village_no)

reg s01 i.treatment  if consentauction==1, cluster(village_no)

reg s01 i.female_bid##i.treatment if consentauction==1, cluster(village_no)


*Table A14 -- PSM -- no control vars
****************** PSM 

*** psm -- radius

	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2   if consentauction==1
		more
		margins, dydx(_all)
		more
		predict double score if consentauction==1
		more	
		sum score
		
		density2 score, group(female_bid)
		psgraph, treated(female_bid) pscore(score)

	psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) caliper(.1) radius common 
		more
		summarize _support if female_bid

	pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head , sum graph
	gen psm_wt0 = _weight 
	egen psm_wt = max(psm_wt0), by(qreid)
	
** Hotelling's T-test

hotelling head_10 head_age salaried_head fem_head wealthindex hh_size  size2 [aw=_weight], by(_treated)
			
regress s01 i.female_bid##i.treatment [w=psm_wt] if consentauction==1, cluster(village_no)

********** checks
regress s01 i.female_bid##i.hh_abandonment_all   [w=psm_wt] if consentauction==1, cluster(village_no)


drop score _pscore _treated _support _weight _s01 psm_wt0 psm_wt 
 
 
 
 *** psm -- kernel
 
	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2   if consentauction==1
	more
	margins, dydx(_all)
	more
	predict double score if consentauction==1
	more
	
	density2 score, group(female_bid)
	psgraph, treated(female_bid) pscore(score)
	
psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) bw(0.07) kernel k(normal) common 

	more
	summarize _support if female_bid
	
pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head  , sum graph
gen psm_wt0 = _weight 
egen psm_wt = max(psm_wt0), by(qreid)


hotelling head_10 head_age salaried_head fem_head wealthindex hh_size  size2 [aw=_weight], by(_treated)

regress s01 i.female_bid##i.treatment [w=psm_wt] if consentauction==1, cluster(village_no)
 
********** checks
regress s01 i.female_bid##i.hh_abandonment_all [w=psm_wt] if consentauction==1, cluster(village_no)
  
drop score _pscore _treated _support _weight _s01 psm_wt0 psm_wt 


*Table A15
****************** 

drop village

etregress s01 i.female_bid##i.treatment head_10 head_age salaried_head wealthindex ///
	 hh_size size2 if consentauction==1 ,  /// 
	 treat(female_bid = fem_head head_10 head_age salaried_head wealthindex i.bidder_10 bidder_age salaried_bidder) /// 
	 vce(cluster village_no) 
	 

*Table A16
******************PSM*** for those who ever owned latrines
*** psm -- radius

	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2   if consentauction==1 & ever_latrine==1
		more
		margins, dydx(_all)
		more
		predict double score if consentauction==1
		more	
		sum score
		
		density2 score, group(female_bid)
		psgraph, treated(female_bid) pscore(score)

	psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) caliper(.1) radius common 
		more
		summarize _support if female_bid

	pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head , sum graph
	gen psm_wt0 = _weight 
	egen psm_wt = max(psm_wt0), by(qreid)
	
regress s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1 & ever_latrine==1, cluster(village_no)

********** checks
regress s01 i.female_bid##i.hh_abandonment_all i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1  & ever_latrine==1, cluster(village_no)


drop score _pscore _treated _suppor$xvarst _weight _s01 psm_wt0 psm_wt 
 
 
 
 *** psm -- kernel
 
	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2   if consentauction==1 & ever_latrine==1
	more
	margins, dydx(_all)
	more
	predict double score if consentauction==1
	more
	
	density2 score, group(female_bid)
	psgraph, treated(female_bid) pscore(score)
	
psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) bw(0.07) kernel k(normal) common 

	more
	summarize _support if female_bid
	
pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head  , sum graph
gen psm_wt0 = _weight 
egen psm_wt = max(psm_wt0), by(qreid)


regress s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1 & ever_latrine==1, cluster(village_no)
********** checks

regress s01 i.female_bid##i.hh_abandonment_all i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1  & ever_latrine==1, cluster(village_no)

drop score _pscore _treated _suppor$xvarst _weight _s01 psm_wt0 psm_wt 

*Table A18
******************IV*** for those who ever owned latrines	 
ivregress 2sls s01 i.female_bid i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2 (i.hh_abandonment_all i.female_bid#i.hh_abandonment_all = i.treatment i.female_bid#i.treatment) if consentauction==1  & ever_latrine==1, cluster(village_no) first 
	 
**************************

********** imputing missing values

tab s01
tab s01 if  consentauction==1

foreach i in female_bid bidder_10 bidder_age salaried_bidder head_10 head_age salaried_head fem_head wealthindex hh_size size2 {
	*sum `i', d
	egen v_`i' = median(`i') if consentauction==1, by(village_no) 
	replace `i' = v_`i' if `i' == . & consentauction==1
}

replace bidder_10 = 1 if bidder_10==.5


*Table A10
****************** OLS -- imputed missing obs

reg s01 i.female_bid i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2  if consentauction==1, cluster(village_no)

reg s01 i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2  if consentauction==1, cluster(village_no)

reg s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2  if consentauction==1, cluster(village_no)


*Table 11
****************PSM  -- imputed missing obs

*** psm -- radius

	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2   if consentauction==1
		more
		margins, dydx(_all)
		more
		predict double score if consentauction==1
		more	
		sum score
		
		density2 score, group(female_bid)
		psgraph, treated(female_bid) pscore(score)

	psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) caliper(.1) radius common 
		more
		summarize _support if female_bid

	pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head , sum graph
	gen psm_wt0 = _weight 
	egen psm_wt = max(psm_wt0), by(qreid)
	

regress s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1, cluster(village_no)

********** checks
regress s01 i.female_bid##i.hh_abandonment_all i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1, cluster(village_no)

drop score _pscore _treated _support _weight _s01 psm_wt0 psm_wt 
 
 
 *** psm -- kernel
 
	logit female_bid  head_10 head_age salaried_head fem_head wealthindex hh_size size2   if consentauction==1
	more
	margins, dydx(_all)
	more
	predict double score if consentauction==1
	more
	
	density2 score, group(female_bid)
	psgraph, treated(female_bid) pscore(score)
	

psmatch2 female_bid if consentauction==1, pscore(score) outcome(s01) bw(0.07) kernel k(normal) common 

	more
	summarize _support if female_bid
	
pstest2  wealthindex hh_size size2 fem_head head_age head_10 salaried_head  , sum graph
gen psm_wt0 = _weight 
egen psm_wt = max(psm_wt0), by(qreid)

regress s01 i.female_bid##i.treatment i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1, cluster(village_no)
 
********** checks
regress s01 i.female_bid##i.hh_abandonment_all i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size  size2  [w=psm_wt] if consentauction==1, cluster(village_no)
 
drop score _pscore _treated _support _weight _s01 psm_wt0 psm_wt 
 
*Table A13 -- IV  -- imputed missing obs
   
ivregress 2sls s01 i.female_bid i.bidder_10 bidder_age salaried_bidder  head_10 head_age salaried_head fem_head wealthindex hh_size size2 (i.hh_abandonment_all i.female_bid#i.hh_abandonment_all = i.treatment i.female_bid#i.treatment) if consentauction==1, cluster(village_no) first 

 
 




 