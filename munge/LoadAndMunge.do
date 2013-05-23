/***********************************
* CHIS pediatric data loading and munge script
* Michael Ching
* 2013
*
**********************************/

* Load data
use "/Users/Chings/Dropbox/Mike/CHIS/chis09_child_stata/CHILD.dta"

* Generate variable pedsHiRisk corresponding to high risk or not on the PEDS
gen pedsHiRisk = .
replace pedsHiRisk = 1 if peds == 4
replace pedsHiRisk = 0 if peds > 0 & peds < 4
label variable pedsHiRisk "High Risk classification on PEDS"
label define pedsHiRiskLabel 1 "High Risk" 0 "Not High Risk"
label values pedsHiRisk pedsHiRiskLabel
* tab pedsHiRisk
* need to figure out what to do with the missing variables in the right ages

* Recode referral to developmental specialist to 1-referred, 0-not
codebook cf46
generate cf46_2 = 2 - cf46
tab cf46_2
replace cf46_2 = . if cf46_2 == 3
label variable cf46_2 "Referred to developmental specialist"
label define cf46_2l 1 "Yes" 0 "No"
label values cf46_2 cf46_2l
tab cf46_2
* Are there missing variables in this age

* Generate male gender variable 1=male, 0=female
generate male = 2 - srsex
label variable male "Male gender"
label define malel 1 "Male" 0 "Female"
label values male malel
* tab male

* Generate usual source of care variable
codebook cd1
gen cd1_2 = .
replace cd1_2 = 1 if cd1 == 1 | cd1 == 3 | cd1 == 4
replace cd1_2 = 0 if cd1 == 2 | cd1 ==5
label variable cd1_2 "Has usual place of care"
label define ny 0 "No" 1 "Yes"
label values cd1_2 ny
* tab cd1_2

* Recode referral to speech, hearing, etc. for 1-referred, 0-not
gen cf47_2 = 2 - cf47
replace cf47_2 = . if cf47_2 == 3
label variable cf47_2 "Referred for speech, language, hearing"
label values cf47_2 ny
* tab cf47_2

* Generate referred variable
gen referred = .
replace referred = 1 if cf46_2 == 1 | cf47_2 == 1
replace referred = 0 if cf46_2 == 0 & cf47_2 == 0
label variable referred "Referred for developmental specialist or speech, language, hearing"
label values referred ny
* tab referred

/*****
* Survey set the data
****/
svyset [pw = rakedw0], jkrw(rakedw1 - rakedw80, multiplier(1)) vce(jack) mse
