/***********************************
* CHIS pediatric data loading script
* Michael Ching
* 2013
*
**********************************/

* Load data
use "/Users/Chings/Dropbox/Mike/CHIS/chis09_child_stata/CHILD.dta"
svyset [pw = rakedw0], jkrw(rakedw1 - rakedw80, multiplier(1)) vce(jack) mse

* Generate variable pedsHiRisk corresponding to high risk or not on the PEDS
gen pedsHiRisk = .
replace pedsHiRisk = 1 if peds == 4
replace pedsHiRisk = 0 if peds > 0 & peds < 4
tab pedsHiRisk
label variable pedsHiRisk "High Risk classification on PEDS"
label define pedsHiRiskLabel 1 "High Risk" 0 "Not High Risk"
label values pedsHiRisk pedsHiRiskLabel
* need to figure out what to do with the missing variables in the right ages

* Recode referral to developmental specialist to 1-referred, 0-not
generate cf46_2 = 2 - cf46
tab cf46_2
replace cf46_2 = . if cf46_2 == 3
tab cf46_2
* Are there missing variables in this age

* Generate male gender variable 1=male, 0=female
generate male = 2 - srsex

* Generate usual source of care variable
codebook cd1
gen cd1_2 = .
replace cd1_2 = 1 if cd1 == 1 | cd1 == 3 | cd1 == 4
replace cd1_2 = 0 if cd1 == 2 | cd1 ==5


* Exploratory analysis on PEDS and age
codebook peds
codebook srage_p
by peds, sort : summarize srage_p
histogram srage_p, by(peds)
graph save Graph "/Users/Chings/Dropbox/Mike/CHIS/Figures/age by PEDS status.gph"

* Exploratory analysis on referred for developmental evaluation
codebook cf46
histogram srage_p, by(cf46)
graph save Graph "/Users/Chings/Dropbox/Mike/CHIS/Figures/age by refer to devo.gph"

* Exploratory analysis on referred for speech, etc.
codebook cf47

* Comparison of referral for speech vs referral for developmental specialist
tabulate cf46 cf47 if cf46 > 0 & cf47 > 0
tab cf46 peds if cf46 > 0 & peds > 0
tab cf47 peds if cf47 > 0 & peds > 0
tab peds cf46 if cf46 > 0 & peds > 0
tab peds cf47 if cf47 > 0 & peds > 0
tabulate cf46 cf47 if cf46 > 0 & cf47 > 0, row
tab peds cf46 if cf46 > 0 & peds > 0, row
tab peds cf47 if cf47 > 0 & peds > 0, row


logit cf46_2 pedsHiRisk srage_p male
logistic cf46_2 pedsHiRisk srage_p male
codebook brthwk_p
summarize brthwk_p
hist brthwk_p
replace brthwk_p = . if brthwk_p <0
hist brthwk_p
codebook cd33
codebook cd3
codebook cd1
codebook povll
codebook insany
codebook ins
logit cf46_2 srage_p male if pedsHiRisk == 1
logistic cf46_2 srage_p male if pedsHiRisk == 1
tab cd1_2
logistic cf46_2 srage_p male cd1_2 if pedsHiRisk == 1

svy: tabulate cf46 cf47 if cf46 > 0 & cf47 > 0, row col

svy jackknife, subpop(if pedsHiRisk == 1) : logistic cf46_2 srage_p male
svy jackknife, subpop(if pedsHiRisk == 1) : logistic cf46_2 srage_p male cd1_2

svy jackknife, subpop(if pedsHiRisk == 1) : logistic cf47_2 srage_p male
svy jackknife, subpop(if pedsHiRisk == 1) : logistic cf47_2 srage_p male cd1_2
svy jackknife, subpop(if pedsHiRisk == 1) : logistic referred srage_p male
svy jackknife, subpop(if pedsHiRisk == 1) : logistic referred srage_p male cd1_2
svy jackknife, subpop(if pedsHiRisk == 1) : logistic cf47_2 srage_p male brthwk_p
svy jackknife : logistic referred srage_p male brthwk_p
svy jackknife : logistic referred srage_p male brthwk_p cd1_2
svy jackknife, subpop(if srage_p > 0) : logistic referred pedsHiRisk srage_p male brthwk_p cd1_2

svy jackknife, subpop(if srage_p > 0) : logistic referred pedsHiRisk srage_p male cd1_2


