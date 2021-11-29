
************************
* 					   *
* Microsimulation MDD  *
*				   	   *
************************

********************************
* This do-file consists of     *
* the codes to derive the      *
* transition rates for the     *
* microsimulation MDD paper    *
********************************

use "G:\OV19_0493\Paper Microsimulatie\Datasets\merged_MDD_1.dta", clear

* POPULATION CONSISTS OF ALL 18-65 YEAR OLDS

mi rename EDUCATION1 EDUCATION_1

*************************************************
* 					STEP 1 						*
*************************************************
* Estimate the distribution of MDD in the starting population (e.g. at age 18) *
mi estimate, or level(95): logit MDD1 AGE i.SEX i.EDUCATION_1 if AGE<=27
* Calculate the prevalences for the different sexes and education categories *
mimrgns, at(AGE=18 SEX=1 EDUCATION_1=4) predict(pr) cmdmargins
mimrgns, at(AGE=18 SEX=2 EDUCATION_1=4) predict(pr) cmdmargins
mimrgns, at(AGE=18 SEX=1 EDUCATION_1=8) predict(pr) cmdmargins
mimrgns, at(AGE=18 SEX=2 EDUCATION_1=8) predict(pr) cmdmargins

*************************************************
* 					STEP 2 						*
*************************************************
* Estimate age-specific rates of entering MDD *

* keep only if no MDD at baseline
keep if MDD1 == 0

* distributions for MDD at second assessment and smoking at baseline
tab MDD4
tab SMOKING_1
tab BHLS_dich
tab SOCIALNEED
tab SEX
tab AGEgroup

mi xeq: tab EDUCATION_1 
mi xeq: tab SMOKING_1
mi xeq: tab BHLS_dich
mi xeq: tab SOCIALNEED

* Estimate age-specific rates of entering MDD *
* by using agegroups
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 i.SMOKING_1 i.BHLS_dich i.SOCIALNEED INT_1A1_2A1_com

* Distribution smoking among low and high educated *
tab SMOKING_1 if EDUCATION_1 == 4
tab SMOKING_1 if EDUCATION_1 == 8

* Distribution health literacy among low and high educated *
tab BHLS_dich if EDUCATION_1 == 4
tab BHLS_dich if EDUCATION_1 == 8

* Distribution social contacts among low and high educated *
tab SOCIALNEED if EDUCATION_1 == 4
tab SOCIALNEED if EDUCATION_1 == 8

* Generate smoking dummy's to make it easier to use for the margins *
mi passive: gen SMOKING_form = 0
mi passive: replace SMOKING_form = 1 if SMOKING_1 == 1
mi passive: replace SMOKING_form = . if SMOKING_1 == .
tab SMOKING_form SMOKING_1

mi passive: gen SMOKING_cur = 0
mi passive: replace SMOKING_cur = 1 if SMOKING_1 == 2
mi passive: replace SMOKING_cur = . if SMOKING_1 == .
tab SMOKING_cur SMOKING_1

* Generate health literacy dummy's to make it easier to use for the margins *
mi passive: gen HEALTHLIT_low = 0
mi passive: replace HEALTHLIT_low = 1 if BHLS_dich == 0
mi passive: replace HEALTHLIT_low = . if BHLS_dich == .
tab HEALTHLIT_low BHLS_dich

* Generate quality of social contacts dummy's to make it easier to use for the margins *
tab SOCIALNEED
mi passive: gen SOCIALNEED_low = 0
mi passive: replace SOCIALNEED_low = 1 if SOCIALNEED == 1
mi passive: replace SOCIALNEED_low = . if SOCIALNEED == .
tab SOCIALNEED_low SOCIALNEED

mi passive: gen SOCIALNEED_moderate = 0
mi passive: replace SOCIALNEED_moderate = 1 if SOCIALNEED == 2
mi passive: replace SOCIALNEED_moderate = . if SOCIALNEED == .
tab SOCIALNEED_moderate SOCIALNEED

************************************
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
* Calculate the incidences for the different sexes, education categories, and education-distributions for smoking health literacy social need*
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3815 SMOKING_cur=0.2407 HEALTHLIT_low=0.3101 SOCIALNEED_low=0.3525 SOCIALNEED_moderate=0.3651) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3815 SMOKING_cur=0.2407 HEALTHLIT_low=0.3101 SOCIALNEED_low=0.3525 SOCIALNEED_moderate=0.3651) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=8 SMOKING_form=0.2443 SMOKING_cur=0.1151 HEALTHLIT_low=0.0862 SOCIALNEED_low=0.1884 SOCIALNEED_moderate=0.3916) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=8 SMOKING_form=0.2443 SMOKING_cur=0.1151 HEALTHLIT_low=0.0862 SOCIALNEED_low=0.1884 SOCIALNEED_moderate=0.3916) predict(pr) cmdmargins

* the before mentioned models are without follow-up time (INT_1A1_2A1_com) *
* calculate follow-up time *
gen time = INT_1A1_2A1_com/12
summarize INT_1A1_2A1_com time
tab time // time between baseline and second assessment = 3.96 year
* divide the estimate out of the model by 3,96 to get the MDD incidence per year

************************************************************************
* step 4, estimate counterfactual age-specific rates of developing MDD *
************************************************************************
* Based on the models estimated in step 2 and 3, calculate the age-specific rate of developing and remitting MDD for VMBO (EDUC=10) for *
* men and women if VMBO respondents would have the SAME SMOKING HABITS as university respondents *

* SMOKING *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.2443 SMOKING_cur=0.1151 HEALTHLIT_low=0.3101 SOCIALNEED_low=0.3525 SOCIALNEED_moderate=0.3651) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.2443 SMOKING_cur=0.1151 HEALTHLIT_low=0.3101 SOCIALNEED_low=0.3525 SOCIALNEED_moderate=0.3651) predict(pr) cmdmargins

* HEALTH LITERACY *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3815 SMOKING_cur=0.2407 HEALTHLIT_low=0.0862 SOCIALNEED_low=0.3525 SOCIALNEED_moderate=0.3651) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3815 SMOKING_cur=0.2407 HEALTHLIT_low=0.0862 SOCIALNEED_low=0.3525 SOCIALNEED_moderate=0.3651) predict(pr) cmdmargins

* SOCIAL NEED *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3815 SMOKING_cur=0.2407 HEALTHLIT_low=0.3101 SOCIALNEED_low=0.1884 SOCIALNEED_moderate=0.3916) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3815 SMOKING_cur=0.2407 HEALTHLIT_low=0.3101 SOCIALNEED_low=0.1884 SOCIALNEED_moderate=0.3916) predict(pr) cmdmargins

* SMOKING, HEALTH LITERACY AND SOCIAL NEED *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.2443 SMOKING_cur=0.1151 HEALTHLIT_low=0.0862 SOCIALNEED_low=0.1884 SOCIALNEED_moderate=0.3916) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.2443 SMOKING_cur=0.1151 HEALTHLIT_low=0.0862 SOCIALNEED_low=0.1884 SOCIALNEED_moderate=0.3916) predict(pr) cmdmargins

*************************************************
* 					STEP 3 						*
*************************************************
* Estimate age-specific rates of remitting MDD *

use "G:\OV19_0493\Paper Microsimulatie\Datasets\merged_MDD_1.dta", clear
mi rename EDUCATION1 EDUCATION_1

keep if MDD1 == 1
tab MDD4

* Change reference category of MDD at T4
recode MDD4 (1 = 0) (0 = 1)
label define MDD4_change 0 "Yes" 1 "No"
label value MDD4 MDD4_change
tab MDD4,nolab

* Distribution smoking among low and high educated *
tab SMOKING_1 if EDUCATION_1 == 4
tab SMOKING_1 if EDUCATION_1 == 8

* Distribution health literacy among low and high educated *
tab BHLS_dich if EDUCATION_1 == 4
tab BHLS_dich if EDUCATION_1 == 8

* Distribution social contacts among low and high educated *
tab SOCIALNEED if EDUCATION_1 == 4
tab SOCIALNEED if EDUCATION_1 == 8

* Generate smoking dummy's to make it easier to use for the margins *
mi passive: gen SMOKING_form = 0
mi passive: replace SMOKING_form = 1 if SMOKING_1 == 1
mi passive: replace SMOKING_form = . if SMOKING_1 == .
tab SMOKING_form SMOKING_1

mi passive: gen SMOKING_cur = 0
mi passive: replace SMOKING_cur = 1 if SMOKING_1 == 2
mi passive: replace SMOKING_cur = . if SMOKING_1 == .
tab SMOKING_cur SMOKING_1

* Generate health literacy dummy's to make it easier to use for the margins *
mi passive: gen HEALTHLIT_low = 0
mi passive: replace HEALTHLIT_low = 1 if BHLS_dich == 0
mi passive: replace HEALTHLIT_low = . if BHLS_dich == .
tab HEALTHLIT_low BHLS_dich

* Generate quality of social contacts dummy's to make it easier to use for the margins *
tab SOCIALNEED
mi passive: gen SOCIALNEED_low = 0
mi passive: replace SOCIALNEED_low = 1 if SOCIALNEED == 1
mi passive: replace SOCIALNEED_low = . if SOCIALNEED == .
tab SOCIALNEED_low SOCIALNEED

mi passive: gen SOCIALNEED_moderate = 0
mi passive: replace SOCIALNEED_moderate = 1 if SOCIALNEED == 2
mi passive: replace SOCIALNEED_moderate = . if SOCIALNEED == .
tab SOCIALNEED_moderate SOCIALNEED

mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
* Calculate the incidences for the different sexes, education categories, and education-distributions for smoking health literacy social need*
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3304 SMOKING_cur=0.3217 HEALTHLIT_low=0.4837 SOCIALNEED_low=0.6711 SOCIALNEED_moderate=0.1579) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3304 SMOKING_cur=0.3217 HEALTHLIT_low=0.4837 SOCIALNEED_low=0.6711 SOCIALNEED_moderate=0.1579) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=8 SMOKING_form=0.2703 SMOKING_cur=0.1622 HEALTHLIT_low=0.2069 SOCIALNEED_low=0.4595 SOCIALNEED_moderate=0.2973) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=8 SMOKING_form=0.2703 SMOKING_cur=0.1622 HEALTHLIT_low=0.2069 SOCIALNEED_low=0.4595 SOCIALNEED_moderate=0.2973) predict(pr) cmdmargins

* the before mentioned models are without follow-up time (INT_1A1_2A1_com) *
* calculate follow-up time *
gen time = INT_1A1_2A1_com/12
summarize INT_1A1_2A1_com time
tab time // time between baseline and second assessment = 3.96 year
* divide the estimate out of the model by 3,96 to get the MDD incidence per year

***********************************************************************
* step 4, estimate counterfactual age-specific rates of remitting MDD *
***********************************************************************
* Based on the models estimated in step 2 and 3, calculate the age-specific rate of developing and remitting MDD for VMBO (EDUC=10) for *
* men and women if VMBO respondents would have the SAME SMOKING HABITS as university respondents *

* SMOKING *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.2703 SMOKING_cur=0.1622 HEALTHLIT_low=0.4837 SOCIALNEED_low=0.6711 SOCIALNEED_moderate=0.1579) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.2703 SMOKING_cur=0.1622 HEALTHLIT_low=0.4837 SOCIALNEED_low=0.6711 SOCIALNEED_moderate=0.1579) predict(pr) cmdmargins

* HEALTH LITERACY *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3304 SMOKING_cur=0.3217 HEALTHLIT_low=0.2069 SOCIALNEED_low=0.6711 SOCIALNEED_moderate=0.1579) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3304 SMOKING_cur=0.3217 HEALTHLIT_low=0.2069 SOCIALNEED_low=0.6711 SOCIALNEED_moderate=0.1579) predict(pr) cmdmargins

* SOCIAL NEED *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3304 SMOKING_cur=0.3217 HEALTHLIT_low=0.4837 SOCIALNEED_low=0.4595 SOCIALNEED_moderate=0.2973) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3304 SMOKING_cur=0.3217 HEALTHLIT_low=0.4837 SOCIALNEED_low=0.4595 SOCIALNEED_moderate=0.2973) predict(pr) cmdmargins

* SMOKING, HEALTH LITERACY AND SOCIAL NEED *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.2703 SMOKING_cur=0.1622 HEALTHLIT_low=0.2069 SOCIALNEED_low=0.4595 SOCIALNEED_moderate=0.2973) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.2703 SMOKING_cur=0.1622 HEALTHLIT_low=0.2069 SOCIALNEED_low=0.4595 SOCIALNEED_moderate=0.2973) predict(pr) cmdmargins

******************************************************************************************************************************************************************************************************
* Sensivitity analysis; only participants with depression history
******************************************************************************************************************************************************************************************************

use "G:\OV19_0493\Paper Microsimulatie\Datasets\merged_MDD_1.dta", clear

* POPULATION CONSISTS OF ALL 18-65 YEAR OLDS

mi rename EDUCATION1 EDUCATION_1

* KEEP depression history *
keep if HEALTH72I3 == 1

*************************************************
* 					STEP 1 						*
*************************************************
* Estimate the distribution of MDD in the starting population (e.g. at age 18) *
mi estimate, or level(95): logit MDD1 AGE i.SEX i.EDUCATION_1 if AGE<=27
* Calculate the prevalences for the different sexes and education categories *
mimrgns, at(AGE=18 SEX=1 EDUCATION_1=4) predict(pr) cmdmargins
mimrgns, at(AGE=18 SEX=2 EDUCATION_1=4) predict(pr) cmdmargins
mimrgns, at(AGE=18 SEX=1 EDUCATION_1=8) predict(pr) cmdmargins
mimrgns, at(AGE=18 SEX=2 EDUCATION_1=8) predict(pr) cmdmargins

*************************************************
* 					STEP 2 						*
*************************************************
* Estimate age-specific rates of entering MDD *

* keep only if no MDD at baseline
keep if MDD1 == 0

* distributions for MDD at second assessment and smoking at baseline
tab MDD4
tab SMOKING_1
tab BHLS_dich
tab SOCIALNEED
tab SEX
tab AGEgroup

mi xeq: tab EDUCATION_1 
mi xeq: tab SMOKING_1
mi xeq: tab BHLS_dich
mi xeq: tab SOCIALNEED


* Estimate age-specific rates of entering MDD *
* by using agegroups
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 i.SMOKING_1 i.BHLS_dich i.SOCIALNEED INT_1A1_2A1_com

* Distribution smoking among low and high educated *
tab SMOKING_1 if EDUCATION_1 == 4
tab SMOKING_1 if EDUCATION_1 == 8

* Distribution health literacy among low and high educated *
tab BHLS_dich if EDUCATION_1 == 4
tab BHLS_dich if EDUCATION_1 == 8

* Distribution social contacts among low and high educated *
tab SOCIALNEED if EDUCATION_1 == 4
tab SOCIALNEED if EDUCATION_1 == 8

* Generate smoking dummy's to make it easier to use for the margins *
mi passive: gen SMOKING_form = 0
mi passive: replace SMOKING_form = 1 if SMOKING_1 == 1
mi passive: replace SMOKING_form = . if SMOKING_1 == .
tab SMOKING_form SMOKING_1

mi passive: gen SMOKING_cur = 0
mi passive: replace SMOKING_cur = 1 if SMOKING_1 == 2
mi passive: replace SMOKING_cur = . if SMOKING_1 == .
tab SMOKING_cur SMOKING_1

* Generate health literacy dummy's to make it easier to use for the margins *
mi passive: gen HEALTHLIT_low = 0
mi passive: replace HEALTHLIT_low = 1 if BHLS_dich == 0
mi passive: replace HEALTHLIT_low = . if BHLS_dich == .
tab HEALTHLIT_low BHLS_dich

* Generate quality of social contacts dummy's to make it easier to use for the margins *
tab SOCIALNEED
mi passive: gen SOCIALNEED_low = 0
mi passive: replace SOCIALNEED_low = 1 if SOCIALNEED == 1
mi passive: replace SOCIALNEED_low = . if SOCIALNEED == .
tab SOCIALNEED_low SOCIALNEED

mi passive: gen SOCIALNEED_moderate = 0
mi passive: replace SOCIALNEED_moderate = 1 if SOCIALNEED == 2
mi passive: replace SOCIALNEED_moderate = . if SOCIALNEED == .
tab SOCIALNEED_moderate SOCIALNEED

**************************
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
* Calculate the incidences for the different sexes, education categories, and education-distributions for smoking health literacy social need*
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3646 SMOKING_cur=0.3486 HEALTHLIT_low=0.3342 SOCIALNEED_low=0.4883 SOCIALNEED_moderate=0.3006) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3646 SMOKING_cur=0.3486 HEALTHLIT_low=0.3342 SOCIALNEED_low=0.4883 SOCIALNEED_moderate=0.3006) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=8 SMOKING_form=0.3109 SMOKING_cur=0.1760 HEALTHLIT_low=0.0899 SOCIALNEED_low=0.3447 SOCIALNEED_moderate=0.3732) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=8 SMOKING_form=0.3109 SMOKING_cur=0.1760 HEALTHLIT_low=0.0899 SOCIALNEED_low=0.3447 SOCIALNEED_moderate=0.3732) predict(pr) cmdmargins

* the before mentioned models are without follow-up time (INT_1A1_2A1_com) *
* calculate follow-up time *
gen time = INT_1A1_2A1_com/12
summarize INT_1A1_2A1_com time
tab time // time between baseline and second assessment = 3.96 year
* divide the estimate out of the model by 3,96 to get the MDD incidence per year

************************************************************************
* step 4, estimate counterfactual age-specific rates of developing MDD *
************************************************************************
* Based on the models estimated in step 2 and 3, calculate the age-specific rate of developing and remitting MDD for VMBO (EDUC=10) for *
* men and women if VMBO respondents would have the SAME SMOKING HABITS as university respondents *

* SMOKING *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3109 SMOKING_cur=0.1760 HEALTHLIT_low=0.3342 SOCIALNEED_low=0.4883 SOCIALNEED_moderate=0.3006) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3109 SMOKING_cur=0.1760 HEALTHLIT_low=0.3342 SOCIALNEED_low=0.4883 SOCIALNEED_moderate=0.3006) predict(pr) cmdmargins

* HEALTH LITERACY *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3646 SMOKING_cur=0.3486 HEALTHLIT_low=0.0899 SOCIALNEED_low=0.4883 SOCIALNEED_moderate=0.3006) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3646 SMOKING_cur=0.3486 HEALTHLIT_low=0.0899 SOCIALNEED_low=0.4883 SOCIALNEED_moderate=0.3006) predict(pr) cmdmargins

* SOCIAL NEED *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3646 SMOKING_cur=0.3486 HEALTHLIT_low=0.3342 SOCIALNEED_low=0.3447 SOCIALNEED_moderate=0.3732) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3646 SMOKING_cur=0.3486 HEALTHLIT_low=0.3342 SOCIALNEED_low=0.3447 SOCIALNEED_moderate=0.3732) predict(pr) cmdmargins

* SMOKING, HEALTH LITERACY AND SOCIAL NEED *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3109 SMOKING_cur=0.1760 HEALTHLIT_low=0.0899 SOCIALNEED_low=0.3447 SOCIALNEED_moderate=0.3732) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3109 SMOKING_cur=0.1760 HEALTHLIT_low=0.0899 SOCIALNEED_low=0.3447 SOCIALNEED_moderate=0.3732) predict(pr) cmdmargins

*************************************************
* 					STEP 3 						*
*************************************************
* Estimate age-specific rates of remitting MDD *

use "G:\OV19_0493\Paper Microsimulatie\Datasets\merged_MDD_1.dta", clear
mi rename EDUCATION1 EDUCATION_1

* KEEP depression history *
keep if HEALTH72I3 == 1

keep if MDD1 == 1
tab MDD4

* Change reference category of MDD at T4
recode MDD4 (1 = 0) (0 = 1)
label define MDD4_change 0 "Yes" 1 "No"
label value MDD4 MDD4_change
tab MDD4,nolab

* Distribution smoking among low and high educated *
tab SMOKING_1 if EDUCATION_1 == 4
tab SMOKING_1 if EDUCATION_1 == 8

* Distribution health literacy among low and high educated *
tab BHLS_dich if EDUCATION_1 == 4
tab BHLS_dich if EDUCATION_1 == 8

* Distribution social contacts among low and high educated *
tab SOCIALNEED if EDUCATION_1 == 4
tab SOCIALNEED if EDUCATION_1 == 8

* Generate smoking dummy's to make it easier to use for the margins *
mi passive: gen SMOKING_form = 0
mi passive: replace SMOKING_form = 1 if SMOKING_1 == 1
mi passive: replace SMOKING_form = . if SMOKING_1 == .
tab SMOKING_form SMOKING_1

mi passive: gen SMOKING_cur = 0
mi passive: replace SMOKING_cur = 1 if SMOKING_1 == 2
mi passive: replace SMOKING_cur = . if SMOKING_1 == .
tab SMOKING_cur SMOKING_1

* Generate health literacy dummy's to make it easier to use for the margins *
mi passive: gen HEALTHLIT_low = 0
mi passive: replace HEALTHLIT_low = 1 if BHLS_dich == 0
mi passive: replace HEALTHLIT_low = . if BHLS_dich == .
tab HEALTHLIT_low BHLS_dich

* Generate quality of social contacts dummy's to make it easier to use for the margins *
tab SOCIALNEED
mi passive: gen SOCIALNEED_low = 0
mi passive: replace SOCIALNEED_low = 1 if SOCIALNEED == 1
mi passive: replace SOCIALNEED_low = . if SOCIALNEED == .
tab SOCIALNEED_low SOCIALNEED

mi passive: gen SOCIALNEED_moderate = 0
mi passive: replace SOCIALNEED_moderate = 1 if SOCIALNEED == 2
mi passive: replace SOCIALNEED_moderate = . if SOCIALNEED == .
tab SOCIALNEED_moderate SOCIALNEED

*************************
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
* Calculate the incidences for the different sexes, education categories, and education-distributions for smoking health literacy social need*
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3171 SMOKING_cur=0.2683 HEALTHLIT_low=0.4747 SOCIALNEED_low=0.7541 SOCIALNEED_moderate=0.1311) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3171 SMOKING_cur=0.2683 HEALTHLIT_low=0.4747 SOCIALNEED_low=0.7541 SOCIALNEED_moderate=0.1311) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=8 SMOKING_form=0.1111 SMOKING_cur=0.2778 HEALTHLIT_low=0.1429 SOCIALNEED_low=0.6471 SOCIALNEED_moderate=0.2353) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=8 SMOKING_form=0.1111 SMOKING_cur=0.2778 HEALTHLIT_low=0.1429 SOCIALNEED_low=0.6471 SOCIALNEED_moderate=0.2353) predict(pr) cmdmargins

* the before mentioned models are without follow-up time (INT_1A1_2A1_com) *
* calculate follow-up time *
gen time = INT_1A1_2A1_com/12
summarize INT_1A1_2A1_com time
tab time // time between baseline and second assessment = 3.96 year
* divide the estimate out of the model by 3,96 to get the MDD incidence per year

********************************************************************

***********************************************************************
* step 4, estimate counterfactual age-specific rates of remitting MDD *
***********************************************************************
* Based on the models estimated in step 2 and 3, calculate the age-specific rate of developing and remitting MDD for VMBO (EDUC=10) for *
* men and women if VMBO respondents would have the SAME SMOKING HABITS as university respondents *

* SMOKING *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.1111 SMOKING_cur=0.2778 HEALTHLIT_low=0.4747 SOCIALNEED_low=0.7541 SOCIALNEED_moderate=0.1311) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.1111 SMOKING_cur=0.2778 HEALTHLIT_low=0.4747 SOCIALNEED_low=0.7541 SOCIALNEED_moderate=0.1311) predict(pr) cmdmargins

* HEALTH LITERACY *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3171 SMOKING_cur=0.2683 HEALTHLIT_low=0.1429 SOCIALNEED_low=0.7541 SOCIALNEED_moderate=0.1311) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3171 SMOKING_cur=0.2683 HEALTHLIT_low=0.1429 SOCIALNEED_low=0.7541 SOCIALNEED_moderate=0.1311) predict(pr) cmdmargins

* SOCIAL NEED *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.3171 SMOKING_cur=0.2683 HEALTHLIT_low=0.4747 SOCIALNEED_low=0.6471 SOCIALNEED_moderate=0.2353) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.3171 SMOKING_cur=0.2683 HEALTHLIT_low=0.4747 SOCIALNEED_low=0.6471 SOCIALNEED_moderate=0.2353) predict(pr) cmdmargins

* SMOKING, HEALTH LITERACY AND SOCIAL NEED *
mi estimate, or level(95): logit MDD4 i.AGEgroup i.SEX i.EDUCATION_1 SMOKING_form SMOKING_cur HEALTHLIT_low SOCIALNEED_low SOCIALNEED_moderate
mimrgns, at(AGEgroup=(1(1)10) SEX=1 EDUCATION_1=4 SMOKING_form=0.1111 SMOKING_cur=0.2778 HEALTHLIT_low=0.1429 SOCIALNEED_low=0.6471 SOCIALNEED_moderate=0.2353) predict(pr) cmdmargins
mimrgns, at(AGEgroup=(1(1)10) SEX=2 EDUCATION_1=4 SMOKING_form=0.1111 SMOKING_cur=0.2778 HEALTHLIT_low=0.1429 SOCIALNEED_low=0.6471 SOCIALNEED_moderate=0.2353) predict(pr) cmdmargins

