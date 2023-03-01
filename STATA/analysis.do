********************************************************************************
********************************************************************************
** Sexual Identity, Gender, and Anticipated Discrimination in Prosocial Behavior 
** Authors: Billur Aksoy, Ian Chadd, Boon Han Koh
********************************************************************************
********************************************************************************

* Combined DO file for all analysis reported in paper (including appendix) - Feb 2023

********** CHANGE DIRECTORY HERE **********

// Root folder is your corresponding Dropbox/STATA folder.
// Uncomment corresponding directories below.
// Directory will be used throughout rest of file, so subsequent paths do not need to be updated.

cd "CHANGE DIRECTORY HERE"

********************************************************************************

********** PREAMBLE **********

drop _all
set more off
set seed 12345
set varabbrev off

********************************************************************************

** DATA SETS:
*  "cleaned-recip.dta"
*  "cleaned-dict.dta"
*  "ANES 2020 Data"
*  "survey_responses"

********************************************************************************

** create directory to store graphs, tables, and log files (if not already present)
cap mkdir "Figures"
cap mkdir "Logs"

********************************************************************************

***** Create LOG file for this entire analysis *****
capture log close _all
log using "logs/paper.log", replace

********************************************************************************
********************************************************************************
*********************************** IN TEXT  ***********************************
********************************************************************************
********************************************************************************	


***** SECTION 3.4: Sample Demographics *****

// Joint test for balance of demographics (recipients)
use "Data/cleaned-recip.dta", clear
tab own_educ_recat, generate(own_educ_recat_)
tab own_religion_recat, generate(own_religion_recat_)
tab own_conservative_social_recat, generate(own_conservative_social_recat_)

mvtest means own_age own_male own_female own_trans own_nonhetero_reported own_ethn_white own_ethn_black own_ethn_asian own_ethn_latino own_educ_recat_2 own_educ_recat_3 own_educ_recat_4 own_religion_recat_1 own_religion_recat_2 own_conservative_social_recat_1 own_conservative_social_recat_2 own_conservative_social_recat_4 own_ally_consider, by(informed_choice) // dropped own_religion_recat_3 due to multicollinearity

// Joint test for balance of demographics (decision-makers)
use "Data/cleaned-dict.dta", clear

mvtest means own_age own_male own_female own_trans own_nonhetero_reported own_ethn_white own_ethn_black own_ethn_asian own_ethn_latino own_some_college own_bachelor own_master own_not_religous own_christian own_v_liberal own_liberal own_v_cons own_ally_consider if round==1, by(other_pride) // dropped own_other_religion due to multicollinearity




***** FOOTNOTE 12: Number of color blind participants *****

use "Data/cleaned-recip.dta", clear
tab colorblind

use "Data/cleaned-dict.dta", clear
tab colorblind if round==1




***** SECTION 4.1: Choice of Pride flag by sexual identity *****

use "Data/cleaned-recip.dta", clear
bys own_nonhetero_prolific: sum own_flag_pride
tab own_nonhetero_prolific own_flag_pride, exact




***** SECTION 4.2: Analysis of survey responses *****


import excel "Data/survey_responses.xlsx", sheet("COMBINED") firstrow clear

bys treatment gender_prolific: sum strategic_final
tab treatment strategic_final if gender_prolific == "Female", exact
tab treatment strategic_final if gender_prolific == "Male", exact

bys treatment: sum strategic_final

gen informed_choice = treatment == "Informed-Choice"
gen female = gender_prolific == "Female"
reg strategic_final female##informed_choice, vce(robust)
lincom 1.informed_choice + 1.informed_choice#1.female 




***** SECTION 4.2: ANES data analysis *****


use "Data/anes_timeseries_2020_stata_20220210.dta", clear


** Using weights for Represenativeness
svyset [pweight=V200010b], strata(V200010d) psu(V200010c)

** V201600
** "What is your sex?"


*Expected discrimination againist gay and lesbians?
** V202533
** "Discrimination in the US against Gays and Lesbians"
** valid responses 1 (a great deal) to 5 (none at all)

**higher number means less discrimination 
gen expected_discrim_against_gay = .
replace expected_discrim_against_gay = V202533 if V202533>0

** men think there is less discrimination 
ttest expected_discrim_against_gay  if V201600>0 , by(V201600)
svy: mean expected_discrim_against_gay  if V201600>0, over(V201600)
lincom c.expected_discrim_against_gay@2.V201600 - c.expected_discrim_against_gay@1.V201600


** V202538
** "How much discrimination has R faced because of gender"
** valid responses 1 (a great deal) to 5 (none at all)

**higher number means less discrimination 
gen gender_discrim_faced = .
replace gender_discrim_faced = V202538 if V202538>0

svy: mean gender_discrim_faced  if V201600>0, over(V201600)
lincom c.gender_discrim_faced@2.V201600 - c.gender_discrim_faced@1.V201600		

svy: mean expected_discrim_against_gay  if V201600>0, over(V201600)
lincom c.expected_discrim_against_gay@2.V201600 - c.expected_discrim_against_gay@1.V201600		
				
	
** do those who experience more discrimination also believe there is more discrimination?
svy: reg expected_discrim_against_gay gender_discrim_faced if !missing(V201600) & expected_discrim_against_gay > 0 & gender_discrim_faced > 0


// By political stand:

/* V201200
Where would you place yourself on this scale, or haven't you
thought much about this?
1. Extremely liberal
2. Liberal
3. Slightly liberal
4. Moderate; middle of the road
5. Slightly conservative
6. Conservative
7. Extremely conservative
*/

/*V201414x

Do you favor or oppose laws to protect gays and lesbians against job
discrimination?
1. Favor
2. Oppose
*/

** using weights for representative sample

svyset [pweight=V200010a], strata(V200010d) psu(V200010c)

gen favor_protection_laws = V201414x==1 | V201414x==2

gen political_stand=.
replace political_stand= V201200 if V201200>0 & V201200<90

gen political_stand_recat = .
replace political_stand_recat = 1 if political_stand==1 | political_stand==2 | political_stand==3
replace political_stand_recat = 3 if political_stand==5 | political_stand==6 | political_stand==7
replace political_stand_recat = 2 if political_stand==4 

svy: mean favor_protection_laws, over(political_stand_recat)
lincom c.favor_protection_laws@2.political_stand_recat - c.favor_protection_laws@1.political_stand_recat
lincom c.favor_protection_laws@1.political_stand_recat - c.favor_protection_laws@3.political_stand_recat
lincom c.favor_protection_laws@2.political_stand_recat - c.favor_protection_laws@3.political_stand_recat




********************************************************************************
********************************************************************************
********************************* PAPER TABLES *********************************
********************************************************************************
********************************************************************************


***** TABLE 1: OLS Regressions of Recipients' Choice of Pride Flag *****

use "Data/cleaned-recip.dta", clear

// Define independent variables
global recip_ctrl_demo "c.own_age i.own_educ_recat i.own_ethn_black i.own_ethn_latino i.own_ethn_asian i.own_ethn_other_recat i.own_religion_recat i.own_trans"
global recip_ctrl_ally "i.own_ally_consider i.own_ally_program"
global recip_ctrl_lgbtexpose "i.own_lgbt_interact_recat i.own_lgbt_friend"
global recip_ctrl_views "ib3.own_conservative_social_recat c.own_lgbt_view_index"
global recip_ctrl_prolific_perc "c.prolific_perc_liberal c.prolific_perc_conservative"
global recip_ctrl_belief_amt "c.belief_amt_diff"
global recip_ctrl_additional "i.own_incon_relations_prolific i.own_incon_attraction_prolific i.prolific_perc_female_more i.prolific_perc_female_less i.prolific_perc_lgbt_more i.prolific_perc_lgbt_less"


 reg own_flag_pride i.informed_choice

 reg own_flag_pride i.informed_choice i.own_nonhetero_prolific i.own_female_prolific

 reg own_flag_pride i.informed_choice i.own_nonhetero_prolific i.own_female_prolific $recip_ctrl_demo

 reg own_flag_pride i.informed_choice i.own_nonhetero_prolific i.own_female_prolific $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional

 reg own_flag_pride i.informed_choice##i.own_nonhetero_prolific i.own_female_prolific
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific

 reg own_flag_pride i.informed_choice##i.own_nonhetero_prolific i.own_female_prolific $recip_ctrl_demo
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific

 reg own_flag_pride i.informed_choice##i.own_nonhetero_prolific i.own_female_prolific $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific





***** TABLE 2: OLS Regressions of Recipients' Choice of Pride Flag by Sexual Orientation *****

use "Data/cleaned-recip.dta", clear
// keep if !own_trans

// Define independent variables
global recip_ctrl_demo "c.own_age i.own_educ_recat i.own_ethn_black i.own_ethn_latino i.own_ethn_asian i.own_ethn_other_recat i.own_religion_recat i.own_trans"
global recip_ctrl_ally "i.own_ally_consider i.own_ally_program"
global recip_ctrl_lgbtexpose "i.own_lgbt_interact_recat i.own_lgbt_friend"
global recip_ctrl_views "ib3.own_conservative_social_recat c.own_lgbt_view_index"
global recip_ctrl_prolific_perc "c.prolific_perc_liberal c.prolific_perc_conservative"
global recip_ctrl_belief_amt "c.belief_amt_diff"
global recip_ctrl_additional "i.own_incon_relations_prolific i.own_incon_attraction_prolific i.prolific_perc_female_more i.prolific_perc_female_less i.prolific_perc_lgbt_more i.prolific_perc_lgbt_less"



 reg own_flag_pride i.informed_choice##i.own_female_prolific if own_nonhetero_prolific==0
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific

 reg own_flag_pride i.informed_choice##i.own_female_prolific $recip_ctrl_demo if own_nonhetero_prolific==0
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific

 reg own_flag_pride i.informed_choice##i.own_female_prolific $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional if own_nonhetero_prolific==0
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific


 reg own_flag_pride i.informed_choice##i.own_female_prolific if own_nonhetero_prolific==1
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific

 reg own_flag_pride i.informed_choice##i.own_female_prolific $recip_ctrl_demo if own_nonhetero_prolific==1
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific

 reg own_flag_pride i.informed_choice##i.own_female_prolific $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional if own_nonhetero_prolific==1
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific





**** TABLE 3: OLS Regression Results for Amount Sent -- Perceived Heterosexual vs. Non-Heterosexual Recipients *****

use "Data/cleaned-dict.dta", clear

// create a variable for biased LGBT views
gen biased_lgbt_views_index = -1*own_lgbt_view_index

egen lgbt_bias_mean = mean(biased_lgbt_views_index)
egen lgbt_bias_sd = sd(biased_lgbt_views_index)
replace biased_lgbt_views_index = (biased_lgbt_views_index - lgbt_bias_mean)/lgbt_bias_sd


// Change the labels for tables

label variable own_nonhetero_prolific "DM: Non-Heterosexual"
label variable other_pride "Recip: Pride"
label variable biased_lgbt_views_index "DM: Biased LGBTQ+ Views"
label variable own_conservative_social_3cat "DM: Conservative Political Leaning"
label variable own_female "DM: Female"


// Define independent variables
global dict_ctrl_demo "own_female own_trans c.own_age own_bachelor own_ethn_black own_ethn_latino own_ethn_asian own_ethn_other_recat own_christian "
global dict_ctrl_ally "own_ally_consider "
global dict_ctrl_lgbtexpose "own_lgbt_interact_recat own_lgbt_friend"
global dict_ctrl_other "own_incon_relations_reported own_incon_attraction_reported  recip_ally recip_perc_female recip_perc_trans recip_perc_conservative recip_perc_age "




 reg gave 1.recip_perc_nonhetero if round==1
 reg gave 1.recip_perc_nonhetero   1.own_nonhetero_prolific 1.own_female $dict_ctrl_demo  if round==1  
 reg gave 1.recip_perc_nonhetero   1.own_nonhetero_prolific biased_lgbt_views_index i.own_conservative_social_3cat IAT_score 1.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other if round==1  


** interacting DM's sexual orientation
 reg gave 1.recip_perc_nonhetero    biased_lgbt_views_index i.own_conservative_social_3cat  IAT_score 1.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other 1.recip_perc_nonhetero##1.own_nonhetero_prolific if round==1  
lincom 1.recip_perc_nonhetero + 1.recip_perc_nonhetero#1.own_nonhetero_prolific

** interacting dictator's gender-female
 reg gave 1.recip_perc_nonhetero 1.recip_perc_nonhetero##1.own_female  biased_lgbt_views_index  1.own_nonhetero_prolific i.own_conservative_social_3cat  IAT_score $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other if round==1  
lincom 1.recip_perc_nonhetero  + 1.recip_perc_nonhetero#1.own_female


** interacting stand on biased lgbt views 
 reg gave 1.recip_perc_nonhetero 1.recip_perc_nonhetero##c.biased_lgbt_views_index   i.own_conservative_social_3cat 1.own_nonhetero_prolific IAT_score  1.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other  if round==1  
lincom 1.recip_perc_nonhetero  + 1.recip_perc_nonhetero#c.biased_lgbt_views_index


** interacting IAT
 reg gave 1.recip_perc_nonhetero 1.recip_perc_nonhetero##c.IAT_score  biased_lgbt_views_index i.own_conservative_social_3cat 1.own_nonhetero_prolific 1.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other if round==1  
lincom 1.recip_perc_nonhetero  + 1.recip_perc_nonhetero#c.IAT_score

** interacting stand on social issues 
 reg gave 1.recip_perc_nonhetero 1.recip_perc_nonhetero##i.own_conservative_social_3cat  biased_lgbt_views_index  1.own_nonhetero_prolific  IAT_score 1.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other if round==1  
lincom 1.recip_perc_nonhetero  + 1.recip_perc_nonhetero#3.own_conservative_social_3cat




********************************************************************************
********************************************************************************
************************ PAPER FIGURES *****************************************
********************************************************************************
********************************************************************************


***** FIGURE 1: Recipients' Choice of Pride Flag between Treatments, Pooled and by Sexual Orientation *****


** Panel (a) - overall treatment effect **

local t=1
foreach x of numlist 0/1 {
	use "Data/cleaned-recip.dta", clear
	ci proportions own_flag_pride if informed_choice==`x'
	return list
	svret r, keep(r(lb) r(ub))
	gen informed_choice=`x'
	save "temp`t'.dta", replace
	local t=`t'+1
}

clear
foreach x of numlist 1/2 {
	append using "temp`x'.dta"
	erase "temp`x'.dta"
}
save "temp.dta", replace

use "Data/cleaned-recip.dta", clear
merge m:1 informed_choice using "temp.dta", force

collapse (mean) own_flag_pride r_lb r_ub (sem) own_flag_pride_se=own_flag_pride, by(informed_choice)

gen group=.
replace group=1 if informed_choice==0
replace group=2 if informed_choice==1

foreach y of numlist 1 2 {
	local x=floor(`y')
	cap drop temp1
	cap drop temp2
	sum own_flag_pride if group==`y'
	gen temp1=r(mean)
	tostring temp1, gen(temp2) format("%12.3f") force
	local labmean`x'=temp2
	local labmean`x'position=0.10
	local labse`x'position=0.05

	cap drop temp1
	cap drop temp2
	sum own_flag_pride_se if group==`y'
	gen temp1=r(mean)
	tostring temp1, gen(temp2) format("%12.3f") force
	local labse`x'=temp2
	
	local labmean`x'Xposition=`y'
	local labse`x'Xposition=`y'
}


twoway (bar own_flag_pride group, fcolor(gray) lcolor(gray) barwidth(0.75)) ///
	(rcap r_ub r_lb group, lcolor(black)), ///
	xtitle(" ") xlabel(1 "Uninformed-Choice" 2 "Informed-Choice", noticks) ///
	xscale(range(0.25 2.75)) ///
	yscale(range(0 1)) ylabel(0(.2)1, labsize(medlarge) angle(horizontal)) ///
	ytitle("Proportion of Recipients", size(medlarge)) ///
	legend(off) ///
	graphregion(color(white)) bgcolor(white) ///
	text(`labmean1position' `labmean1Xposition' "`labmean1'", place(c) size(medlarge)) ///
	text(`labse1position' `labse1Xposition' "(`labse1')", place(c) size(medlarge)) ///
	text(`labmean2position' `labmean2Xposition' "`labmean2'", place(c) size(medlarge)) ///
	text(`labse2position' `labse2Xposition' "(`labse2')", place(c) size(medlarge))
graph export "figures/Figure1a.eps", replace
graph export "figures/Figure1a.png", replace
window manage close graph _all

erase temp.dta


// Fisher's exact tests
use "Data/cleaned-recip.dta", clear
tab own_flag_pride informed_choice, exact


** Panel (b) - by treatment **

local t=1
foreach x of numlist 0/1 {
	foreach y of numlist 0/1 {
		use "Data/cleaned-recip.dta", clear
		ci proportions own_flag_pride if informed_choice==`x' & own_nonhetero_prolific==`y'
		return list
		svret r, keep(r(lb) r(ub))
		gen informed_choice=`x'
		gen own_nonhetero_prolific=`y'
		save "temp`t'.dta", replace
		local t=`t'+1
	}
}

clear
foreach x of numlist 1/4 {
	append using "temp`x'.dta"
	erase "temp`x'.dta"
}
save "temp.dta", replace

use "Data/cleaned-recip.dta", clear
merge m:1 informed_choice own_nonhetero_prolific using "temp.dta", force

collapse (mean) own_flag_pride r_lb r_ub (sem) own_flag_pride_se=own_flag_pride, by(informed_choice own_nonhetero_prolific)

gen group=.
replace group=1 if informed_choice==0 & own_nonhetero_prolific==0
replace group=2 if informed_choice==1 & own_nonhetero_prolific==0
replace group=3.5 if informed_choice==0 & own_nonhetero_prolific==1
replace group=4.5 if informed_choice==1 & own_nonhetero_prolific==1

foreach y of numlist 1 2 3.5 4.5 {
	local x=floor(`y')
	cap drop temp1
	cap drop temp2
	sum own_flag_pride if group==`y'
	gen temp1=r(mean)
	tostring temp1, gen(temp2) format("%12.3f") force
	local labmean`x'=temp2
	local labmean`x'position=0.10
	local labse`x'position=0.05

	cap drop temp1
	cap drop temp2
	sum own_flag_pride_se if group==`y'
	gen temp1=r(mean)
	tostring temp1, gen(temp2) format("%12.3f") force
	local labse`x'=temp2
	
	local labmean`x'Xposition=`y'
	local labse`x'Xposition=`y'
}


twoway (bar own_flag_pride group if informed_choice==0, fcolor(white) lcolor(black) barwidth(0.75)) ///
	(bar own_flag_pride group if informed_choice==1, fcolor(gray) lcolor(gray) barwidth(0.75)) ///
	(rcap r_ub r_lb group, lcolor(black)), ///
	xtitle(" ") xlabel(1.5 "Heterosexual" 4 "Gay/Lesbian", noticks) ///
	xscale(range(0.25 5.25)) ///
	yscale(range(0 1)) ylabel(0(.2)1, labsize(medlarge) angle(horizontal)) ///
	ytitle("Proportion of Recipients", size(medlarge)) ///
	legend(order(1 "Uninformed-Choice" 2 "Informed-Choice" ) rows(1)) ///
	graphregion(color(white)) bgcolor(white) ///
	text(`labmean1position' `labmean1Xposition' "`labmean1'", place(c) size(medlarge)) ///
	text(`labse1position' `labse1Xposition' "(`labse1')", place(c) size(medlarge)) ///
	text(`labmean2position' `labmean2Xposition' "`labmean2'", place(c) size(medlarge)) ///
	text(`labse2position' `labse2Xposition' "(`labse2')", place(c) size(medlarge)) ///
	text(`labmean3position' `labmean3Xposition' "`labmean3'", place(c) size(medlarge)) ///
	text(`labse3position' `labse3Xposition' "(`labse3')", place(c) size(medlarge)) ///
	text(`labmean4position' `labmean4Xposition' "`labmean4'", place(c) size(medlarge)) ///
	text(`labse4position' `labse4Xposition' "(`labse4')", place(c) size(medlarge))
	
graph export "figures/Figure1b.eps", replace
graph export "figures/Figure1b.png", replace
window manage close graph _all

erase temp.dta


// Fisher's exact tests
use "Data/cleaned-recip.dta", clear
bys own_nonhetero_prolific: tab own_flag_pride informed_choice, exact




***** FIGURE 2: Recipients' Choice of Pride Flag by Treatment, Sexual Identity, and Gender *****

local t=1
foreach x of numlist 0/1 {
	foreach y of numlist 0/1 {
		foreach z of numlist 0/1 {
			use "Data/cleaned-recip.dta", clear
			ci proportions own_flag_pride if informed_choice==`x' & own_nonhetero_prolific==`y' & own_female_prolific==`z'
			return list
			svret r, keep(r(lb) r(ub))
			gen informed_choice=`x'
			gen own_nonhetero_prolific=`y'
			gen own_female_prolific=`z'
			save "temp`t'.dta", replace
			local t=`t'+1
		}
	}
}

clear
foreach x of numlist 1/8 {
	append using "temp`x'.dta"
	erase "temp`x'.dta"
}
save "temp.dta", replace

use "Data/cleaned-recip.dta", clear
merge m:1 informed_choice own_nonhetero_prolific own_female_prolific using "temp.dta", force

collapse (mean) own_flag_pride r_lb r_ub (sem) own_flag_pride_se=own_flag_pride, by(informed_choice own_nonhetero_prolific own_female_prolific)

gen group=.
replace group=1 if informed_choice==0 & own_nonhetero_prolific==0 & own_female_prolific==1
replace group=2 if informed_choice==1 & own_nonhetero_prolific==0 & own_female_prolific==1
replace group=3.5 if informed_choice==0 & own_nonhetero_prolific==1 & own_female_prolific==1
replace group=4.5 if informed_choice==1 & own_nonhetero_prolific==1 & own_female_prolific==1
replace group=6 if informed_choice==0 & own_nonhetero_prolific==0 & own_female_prolific==0
replace group=7 if informed_choice==1 & own_nonhetero_prolific==0 & own_female_prolific==0
replace group=8.5 if informed_choice==0 & own_nonhetero_prolific==1 & own_female_prolific==0
replace group=9.5 if informed_choice==1 & own_nonhetero_prolific==1 & own_female_prolific==0


foreach y of numlist 1 2 3.5 4.5 6 7 8.5 9.5 {
	local x=floor(`y')
	cap drop temp1
	cap drop temp2
	sum own_flag_pride if group==`y'
	gen temp1=r(mean)
	tostring temp1, gen(temp2) format("%12.3f") force
	local labmean`x'=temp2
	local labmean`x'position=0.10
	local labse`x'position=0.05

	cap drop temp1
	cap drop temp2
	sum own_flag_pride_se if group==`y'
	gen temp1=r(mean)
	tostring temp1, gen(temp2) format("%12.3f") force
	local labse`x'=temp2
	
	local labmean`x'Xposition=`y'
	local labse`x'Xposition=`y'
	
}


twoway (bar own_flag_pride group if informed_choice==0, fcolor(white) lcolor(black)) ///
	(bar own_flag_pride group if informed_choice==1, fcolor(gray) lcolor(gray)) ///
	(rcap r_ub r_lb group, lcolor(black)), ///
	xtitle(" ") xlabel(1.5 `" "Hetero" "Women""' 4 `" "Lesbian" "Women""'  6.5 `" "Hetero" "Men""'  9 `" "Gay" "Men""'  , noticks) ///
	xscale(range(0 10.5)) ///
	yscale(range(0 1)) ylabel(0(.2)1, labsize(medlarge) angle(horizontal)) ///
	ytitle("Proportion of Recipients", size(medlarge)) ///
	legend(order(1 "Uninformed-Choice" 2 "Informed-Choice" ) rows(1)) ///
	graphregion(color(white)) bgcolor(white) ///
	text(`labmean1position' `labmean1Xposition' "`labmean1'", place(c) size(small)) ///
	text(`labse1position' `labse1Xposition' "(`labse1')", place(c) size(small)) ///
	text(`labmean2position' `labmean2Xposition' "`labmean2'", place(c) size(small)) ///
	text(`labse2position' `labse2Xposition' "(`labse2')", place(c) size(small)) ///
	text(`labmean3position' `labmean3Xposition' "`labmean3'", place(c) size(small)) ///
	text(`labse3position' `labse3Xposition' "(`labse3')", place(c) size(small)) ///
	text(`labmean4position' `labmean4Xposition' "`labmean4'", place(c) size(small)) ///
	text(`labse4position' `labse4Xposition' "(`labse4')", place(c) size(small)) ///
	text(`labmean6position' `labmean6Xposition' "`labmean6'", place(c) size(small)) ///
	text(`labse6position' `labse6Xposition' "(`labse6')", place(c) size(small)) ///
	text(`labmean7position' `labmean7Xposition' "`labmean7'", place(c) size(small)) ///
	text(`labse7position' `labse7Xposition' "(`labse7')", place(c) size(small)) ///
	text(`labmean8position' `labmean8Xposition' "`labmean8'", place(c) size(small)) ///
	text(`labse8position' `labse8Xposition' "(`labse8')", place(c) size(small)) ///
	text(`labmean9position' `labmean9Xposition' "`labmean9'", place(c) size(small)) ///
	text(`labse9position' `labse9Xposition' "(`labse9')", place(c) size(small))
	
graph export "figures/Figure2.eps", replace
graph export "figures/Figure2.png", replace
window manage close graph _all
erase temp.dta


// Fisher's exact tests
use "Data/cleaned-recip.dta", clear
bys own_female_prolific own_nonhetero_prolific: tab own_flag_pride informed_choice, exact
bys informed_choice own_nonhetero_prolific: tab own_flag_pride own_female_prolific, exact




***** FIGURE 3: Distributions of and Average Amount Sent by Decision-Makers Based on Recipient's Flag Choice (Endowment = 100 ECU) *****


use "Data/cleaned-dict.dta", clear

qui summarize gave if round==1 &  other_pride==0, detail
local Mean_to_nonpride = r(mean)
qui summarize gave if round==1 &  other_pride==1, detail
local Mean_to_pride = r(mean)	
	
twoway  (histogram gave if round==1 & other_pride==1, width(10) start(-0.5) percent fcolor(gray) fintensity(50) lcolor(none)) ///
	(scatteri 0 `Mean_to_pride' 80 `Mean_to_pride', recast(line) lcolor(gray) lwidth(thick))  ///
	(histogram gave if round==1 & other_pride==0, width(10) start(-0.5) percent fcolor(none) lcolor(black)) ///
	(scatteri 0 `Mean_to_nonpride' 80 `Mean_to_nonpride', recast(line) lcolor(black) lpattern(dash) lwidth(thick) ), ///
	legend(order(1 "Pride" 2 "Pride (mean)" 3 "Non-Pride" 4 "Non-Pride (mean)")) ///
	yscale(range(0 80)) ylabel(0(20)80, labsize(medlarge) angle(horizontal)) ///
	ytitle(% Decision-Makers, size(medium)) xtitle(Amount Sent, size(medium)) ///
	xscale(range(0 100)) xlabel(0(20)100) ///
	graphregion(color(white)) bgcolor(white)
graph export "figures/Figure3.eps", replace
graph export "figures/Figure3.png", replace
window manage close graph _all


// Summary statistics, Kolmogorov-Smirnov, and rank-sum tests
bys other_pride: sum gave if round==1 
ranksum gave if round==1 , by(other_pride)
ksmirnov gave if round==1 , by(other_pride)




********************************************************************************
********************************************************************************
******************************** APPENDIX TABLES *******************************
********************************************************************************
********************************************************************************	


***** TABLE B.1: Sample Demographics *****

// Recipients
use "Data/cleaned-recip.dta", clear
tab own_educ_recat, generate(own_educ_recat_)
tab own_religion_recat, generate(own_religion_recat_)
tab income, generate(income_)

 tabstat own_age ///
	own_male own_female own_trans ///
	own_ethn_white own_ethn_black own_ethn_asian own_ethn_latino ///
	own_educ_recat_2 own_educ_recat_3 own_educ_recat_4 ///
	own_religion_recat_2 own_religion_recat_1 ///
	income_1 income_2 income_3 income_4 income_5 income_6, ///
	statistics(mean) columns(statistics) by(own_nonhetero_prolific)

	
// Decision-Makers
use "Data/cleaned-dict.dta", clear
tab income, generate(income_)

 tabstat own_age ///
	own_male own_female own_trans ///
	own_ethn_white own_ethn_black own_ethn_asian own_ethn_latino ///
	own_some_college own_bachelor own_master ///
	own_christian own_not_religous ///
	income_6 income_2 income_3 income_4 income_5 income_1 if round==1, ///
	statistics(mean) columns(statistics) by(own_nonhetero_prolific)




***** TABLE B.2: Summary Statistics of Recipients' Characteristics by Treatment *****

use "Data/cleaned-recip.dta", clear

tab own_educ_recat, generate(own_educ_recat_)
tab own_religion_recat, generate(own_religion_recat_)
tab own_conservative_social_recat, generate(own_conservative_social_recat_)

 tabstat own_age ///
	own_male own_female own_trans ///
	own_nonhetero_reported ///
	own_ethn_white own_ethn_black own_ethn_asian own_ethn_latino ///
	own_educ_recat_2 own_educ_recat_3 own_educ_recat_4 ///
	own_religion_recat_1 own_religion_recat_2 own_religion_recat_3 ///
	own_conservative_social_recat_1 own_conservative_social_recat_2 own_conservative_social_recat_4 ///
	own_ally_consider, ///
	statistics(mean count sd) columns(statistics) by(informed_choice) nototal

tab informed_choice


// Pairwise tests
foreach x of varlist own_age {
	ranksum `x', by(informed_choice)
}

foreach x of varlist own_male own_female own_trans own_nonhetero_reported own_ethn_white own_ethn_black own_ethn_asian own_ethn_latino own_educ_recat_2 own_educ_recat_3 own_educ_recat_4 own_religion_recat_1 own_religion_recat_2 own_religion_recat_3 own_conservative_social_recat_1 own_conservative_social_recat_2 own_conservative_social_recat_4 own_ally_consider {
	tab `x' informed_choice, exact
}

// Joint test
mvtest means own_age own_male own_female own_trans own_nonhetero_reported own_ethn_white own_ethn_black own_ethn_asian own_ethn_latino own_educ_recat_2 own_educ_recat_3 own_educ_recat_4 own_religion_recat_1 own_religion_recat_2 own_conservative_social_recat_1 own_conservative_social_recat_2 own_conservative_social_recat_4 own_ally_consider, by(informed_choice) // dropped own_religion_recat_3 due to multicollinearity




***** TABLE B.3: Summary Statistics of Decision-Makers' Characteristics by Treatment *****

use "Data/cleaned-dict.dta", clear

 tabstat own_age ///
	own_male own_female own_trans ///
	own_nonhetero_reported ///
	own_ethn_white own_ethn_black own_ethn_asian own_ethn_latino ///
	own_some_college own_bachelor own_master ///
	own_not_religous own_christian own_other_religion ///
	own_v_liberal own_liberal own_v_cons ///
	own_ally_consider if round==1, ///
	statistics(mean count sd) columns(statistics) by(other_pride) nototal

tab other_pride if round==1


// Pairwise tests
foreach x of varlist own_age {
	ranksum `x' if round==1, by(other_pride)
}

foreach x of varlist own_male own_female own_trans own_nonhetero_reported own_ethn_white own_ethn_black own_ethn_asian own_ethn_latino own_some_college own_bachelor own_master own_not_religous own_christian own_other_religion own_v_liberal own_liberal own_v_cons own_ally_consider {
	tab `x' other_pride if round==1, exact
}


// Joint test
mvtest means own_age own_male own_female own_trans own_nonhetero_reported own_ethn_white own_ethn_black own_ethn_asian own_ethn_latino own_some_college own_bachelor own_master own_not_religous own_christian own_v_liberal own_liberal own_v_cons own_ally_consider if round==1, by(other_pride) // dropped own_other_religion due to multicollinearity




***** TABLE B.4: Frequency Table of Recipients' Gender and Sexual Identities (Prolific Profile versus Post-Experimental Questionnaire Responses) *****

use "Data/cleaned-recip.dta", clear
tab treatment_group_reported treatment_group_prolific


// table footnote (a)
tab own_trans




***** TABLE B.5: Frequency Table of Decision-Makers' Sexual Identity (Prolific Profile versus Post-Experimental Questionnaire Responses) *****

use "Data/cleaned-dict.dta", clear
tab own_nonhetero_reported own_nonhetero_prolific if round==1




***** TABLE B.6: Probit Regressions of Recipients' Choice of Pride Flag *****

use "Data/cleaned-recip.dta", clear

// Define independent variables
global recip_ctrl_demo "c.own_age i.own_educ_recat i.own_ethn_black i.own_ethn_latino i.own_ethn_asian i.own_ethn_other_recat i.own_religion_recat i.own_trans"
global recip_ctrl_ally "i.own_ally_consider i.own_ally_program"
global recip_ctrl_lgbtexpose "i.own_lgbt_interact_recat i.own_lgbt_friend"
global recip_ctrl_views "ib3.own_conservative_social_recat c.own_lgbt_view_index"
global recip_ctrl_prolific_perc "c.prolific_perc_liberal c.prolific_perc_conservative"
global recip_ctrl_belief_amt "c.belief_amt_diff"
global recip_ctrl_additional "i.own_incon_relations_prolific i.own_incon_attraction_prolific i.prolific_perc_female_more i.prolific_perc_female_less i.prolific_perc_lgbt_more i.prolific_perc_lgbt_less"


 probit own_flag_pride i.informed_choice

 probit own_flag_pride i.informed_choice i.own_nonhetero_prolific i.own_female_prolific

 probit own_flag_pride i.informed_choice i.own_nonhetero_prolific i.own_female_prolific $recip_ctrl_demo

 probit own_flag_pride i.informed_choice i.own_nonhetero_prolific i.own_female_prolific $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional

 probit own_flag_pride i.informed_choice##i.own_nonhetero_prolific i.own_female_prolific
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific

 probit own_flag_pride i.informed_choice##i.own_nonhetero_prolific i.own_female_prolific $recip_ctrl_demo
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific

 probit own_flag_pride i.informed_choice##i.own_nonhetero_prolific i.own_female_prolific $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific





***** TABLE B.7: Probit Regressions of Recipients' Choice of Pride Flag by Sexual Orientation *****

use "Data/cleaned-recip.dta", clear

// Define independent variables
global recip_ctrl_demo "c.own_age i.own_educ_recat i.own_ethn_black i.own_ethn_latino i.own_ethn_asian i.own_ethn_other_recat i.own_religion_recat i.own_trans"
global recip_ctrl_ally "i.own_ally_consider i.own_ally_program"
global recip_ctrl_lgbtexpose "i.own_lgbt_interact_recat i.own_lgbt_friend"
global recip_ctrl_views "ib3.own_conservative_social_recat c.own_lgbt_view_index"
global recip_ctrl_prolific_perc "c.prolific_perc_liberal c.prolific_perc_conservative"
global recip_ctrl_belief_amt "c.belief_amt_diff"
global recip_ctrl_additional "i.own_incon_relations_prolific i.own_incon_attraction_prolific i.prolific_perc_female_more i.prolific_perc_female_less i.prolific_perc_lgbt_more i.prolific_perc_lgbt_less"


 probit own_flag_pride i.informed_choice##i.own_female_prolific if own_nonhetero_prolific==0
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific

 probit own_flag_pride i.informed_choice##i.own_female_prolific $recip_ctrl_demo if own_nonhetero_prolific==0
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific

 probit own_flag_pride i.informed_choice##i.own_female_prolific $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional if own_nonhetero_prolific==0
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific


 probit own_flag_pride i.informed_choice##i.own_female_prolific if own_nonhetero_prolific==1
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific

 probit own_flag_pride i.informed_choice##i.own_female_prolific $recip_ctrl_demo if own_nonhetero_prolific==1
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific

 probit own_flag_pride i.informed_choice##i.own_female_prolific $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional if own_nonhetero_prolific==1
lincom 1.informed_choice + 1.informed_choice#1.own_female_prolific





***** TABLE B.8: OLS Regressions of Recipients' Choice of Pride Flag *****

use "Data/cleaned-recip.dta", clear

// Define independent variables
global recip_ctrl_demo "c.own_age i.own_educ_recat i.own_ethn_black i.own_ethn_latino i.own_ethn_asian i.own_ethn_other_recat i.own_religion_recat i.own_trans"
global recip_ctrl_ally "i.own_ally_consider i.own_ally_program"
global recip_ctrl_lgbtexpose "i.own_lgbt_interact_recat i.own_lgbt_friend"
global recip_ctrl_views "ib3.own_conservative_social_recat c.own_lgbt_view_index"
global recip_ctrl_prolific_perc "c.prolific_perc_liberal c.prolific_perc_conservative"
global recip_ctrl_belief_amt "c.belief_amt_diff"
global recip_ctrl_additional "i.own_incon_relations_prolific i.own_incon_attraction_prolific i.prolific_perc_female_more i.prolific_perc_female_less i.prolific_perc_lgbt_more i.prolific_perc_lgbt_less"


 reg own_flag_pride i.informed_choice##i.own_nonhetero_prolific if own_female_prolific==0
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific

 reg own_flag_pride i.informed_choice##i.own_nonhetero_prolific $recip_ctrl_demo if own_female_prolific==0
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific

 reg own_flag_pride i.informed_choice##i.own_nonhetero_prolific $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional if own_female_prolific==0
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific


 reg own_flag_pride i.informed_choice##i.own_nonhetero_prolific if own_female_prolific==1
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific

 reg own_flag_pride i.informed_choice##i.own_nonhetero_prolific $recip_ctrl_demo if own_female_prolific==1
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific

 reg own_flag_pride i.informed_choice##i.own_nonhetero_prolific $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional if own_female_prolific==1
lincom 1.informed_choice + 1.informed_choice#1.own_nonhetero_prolific


 reg own_flag_pride i.informed_choice if own_nonhetero_prolific==1 & own_female_prolific==0

 reg own_flag_pride i.informed_choice $recip_ctrl_demo if own_nonhetero_prolific==1 & own_female_prolific==0

 reg own_flag_pride i.informed_choice $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional if own_nonhetero_prolific==1 & own_female_prolific==0


 reg own_flag_pride i.informed_choice if own_nonhetero_prolific==1 & own_female_prolific==1

 reg own_flag_pride i.informed_choice $recip_ctrl_demo if own_nonhetero_prolific==1 & own_female_prolific==1

 reg own_flag_pride i.informed_choice $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional if own_nonhetero_prolific==1 & own_female_prolific==1


 reg own_flag_pride i.informed_choice if own_nonhetero_prolific==0 & own_female_prolific==0

 reg own_flag_pride i.informed_choice $recip_ctrl_demo if own_nonhetero_prolific==0 & own_female_prolific==0

 reg own_flag_pride i.informed_choice $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional if own_nonhetero_prolific==0 & own_female_prolific==0


 reg own_flag_pride i.informed_choice if own_nonhetero_prolific==0 & own_female_prolific==1

 reg own_flag_pride i.informed_choice $recip_ctrl_demo if own_nonhetero_prolific==0 & own_female_prolific==1

 reg own_flag_pride i.informed_choice $recip_ctrl_demo $recip_ctrl_ally $recip_ctrl_views $recip_ctrl_belief_amt $recip_ctrl_prolific_perc $recip_ctrl_lgbtexpose $recip_ctrl_additional if own_nonhetero_prolific==0 & own_female_prolific==1






***** TABLE B.9: OLS Regression Results for Amount Sent -- Pride vs. Non-Pride *****


use "Data/cleaned-dict.dta", clear

// create a variable for biased LGBT views
gen biased_lgbt_views_index = -1*own_lgbt_view_index

egen lgbt_bias_mean = mean(biased_lgbt_views_index)
egen lgbt_bias_sd = sd(biased_lgbt_views_index)
replace biased_lgbt_views_index = (biased_lgbt_views_index - lgbt_bias_mean)/lgbt_bias_sd


// Define independent variables
global dict_ctrl_demo "own_female own_trans c.own_age own_bachelor own_ethn_black own_ethn_latino own_ethn_asian own_ethn_other_recat own_christian "
global dict_ctrl_ally "own_ally_consider "
global dict_ctrl_lgbtexpose "own_lgbt_interact_recat own_lgbt_friend"
global dict_ctrl_other "own_incon_relations_reported own_incon_attraction_reported recip_perc_nonhetero recip_ally recip_perc_female recip_perc_trans recip_perc_conservative recip_perc_age "


// Change the labels for tables

label variable own_nonhetero_prolific "DM: Gay/Lesbian"
label variable other_pride "Recip: Pride"
label variable biased_lgbt_views_index "DM: Biased LGBTQ+ Views"
label variable own_conservative_social_3cat "DM: Conservative Political Leaning"
label variable own_female "DM: Female"



 reg gave other_pride if round==1
 reg gave other_pride   i.own_nonhetero_prolific  i.own_female $dict_ctrl_demo  if round==1  
 reg gave other_pride   i.own_nonhetero_prolific biased_lgbt_views_index i.own_conservative_social_3cat IAT_score i.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other if round==1  

** interacting DM's sexual orientation
 reg gave other_pride   biased_lgbt_views_index i.own_conservative_social_3cat  IAT_score i.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other i.other_pride##i.own_nonhetero_prolific if round==1  
lincom 1.other_pride + 1.other_pride#1.own_nonhetero_prolific

** interacting dictator's gender-female
 reg gave other_pride i.other_pride##i.own_female  biased_lgbt_views_index  i.own_nonhetero_prolific i.own_conservative_social_3cat  IAT_score $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other if round==1  
lincom 1.other_pride  + 1.other_pride#1.own_female


** interacting stand on biased lgbt views 
 reg gave other_pride i.other_pride##c.biased_lgbt_views_index   i.own_conservative_social_3cat i.own_nonhetero_prolific IAT_score i.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other  if round==1  
lincom 1.other_pride  + 1.other_pride#c.biased_lgbt_views_index


** interacting IAT
 reg gave other_pride i.other_pride##c.IAT_score  biased_lgbt_views_index i.own_conservative_social_3cat i.own_nonhetero_prolific i.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other if round==1  
lincom 1.other_pride  + 1.other_pride#c.IAT_score

** interacting stand on social issues 
 reg gave other_pride i.other_pride##i.own_conservative_social_3cat  biased_lgbt_views_index  i.own_nonhetero_prolific  IAT_score i.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other if round==1  
lincom 1.other_pride  + 1.other_pride#3.own_conservative_social_3cat






***** TABLE C.3: Multinomial Probit Regressions of Recipients' Flag Choice *****

use "Data/cleaned-recip.dta", clear

eststo clear

mprobit own_flag i.informed_choice i.own_nonhetero_prolific i.own_female_prolific
eststo: margins, dydx(*) post

mprobit own_flag i.informed_choice i.own_female_prolific if own_nonhetero_prolific==0
eststo: margins, dydx(*) post

mprobit own_flag i.informed_choice i.own_female_prolific if own_nonhetero_prolific==1
eststo: margins, dydx(*) post

mprobit own_flag i.informed_choice i.own_nonhetero_prolific if own_female_prolific==0
eststo: margins, dydx(*) post

mprobit own_flag i.informed_choice i.own_nonhetero_prolific if own_female_prolific==1
eststo: margins, dydx(*) post


eststo clear




***** TABLE C.4: Multinomial Probit Regressions of Recipients' String Choice *****

use "Data/cleaned-recip.dta", clear

eststo clear

mprobit own_string i.informed_choice i.own_nonhetero_prolific i.own_female_prolific
eststo: margins, dydx(*) post

mprobit own_string i.informed_choice i.own_female_prolific if own_nonhetero_prolific==0
eststo: margins, dydx(*) post

mprobit own_string i.informed_choice i.own_female_prolific if own_nonhetero_prolific==1
eststo: margins, dydx(*) post

mprobit own_string i.informed_choice i.own_nonhetero_prolific if own_female_prolific==0
eststo: margins, dydx(*) post

mprobit own_string i.informed_choice i.own_nonhetero_prolific if own_female_prolific==1
eststo: margins, dydx(*) post


eststo clear




*****  TABLE D.1: OLS Regression Results for Amount Sent -- Pride vs. Non-Pride with Both Recipients *****

use "Data/cleaned-dict.dta", clear

// create a variable for biased LGBT views
gen biased_lgbt_views_index = -1*own_lgbt_view_index

egen lgbt_bias_mean = mean(biased_lgbt_views_index)
egen lgbt_bias_sd = sd(biased_lgbt_views_index)
replace biased_lgbt_views_index = (biased_lgbt_views_index - lgbt_bias_mean)/lgbt_bias_sd


// Define independent variables
global dict_ctrl_demo "own_female own_trans c.own_age own_bachelor own_ethn_black own_ethn_latino own_ethn_asian own_ethn_other_recat own_christian "
global dict_ctrl_ally "own_ally_consider "
global dict_ctrl_lgbtexpose "own_lgbt_interact_recat own_lgbt_friend"
global dict_ctrl_other "own_incon_relations_reported own_incon_attraction_reported recip_perc_nonhetero recip_ally recip_perc_female recip_perc_trans recip_perc_conservative recip_perc_age "


// Change the labels for tables

 label variable own_nonhetero_prolific "DM: Gay/Lesbian"
 label variable other_pride "Recip: Pride"
 label variable biased_lgbt_views_index "DM: Biased LGBTQ+ Views"
 label variable own_conservative_social_3cat "DM: Conservative Political Leaning"
 label variable own_female "DM: Female"

 
xtset ID round


 xtreg gave other_pride 2.round, vce(cluster ID)
 xtreg gave other_pride   i.own_nonhetero_prolific  i.own_female $dict_ctrl_demo  2.round, vce(cluster ID)
 xtreg gave other_pride   i.own_nonhetero_prolific biased_lgbt_views_index i.own_conservative_social_3cat IAT_score i.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other 2.round, vce(cluster ID)

** interacting DM's sexual orientation
 xtreg gave other_pride   biased_lgbt_views_index i.own_conservative_social_3cat  IAT_score i.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other i.other_pride##i.own_nonhetero_prolific 2.round, vce(cluster ID)
lincom 1.other_pride + 1.other_pride#1.own_nonhetero_prolific

** interacting dictator's gender-female
 xtreg gave other_pride i.other_pride##i.own_female  biased_lgbt_views_index  i.own_nonhetero_prolific i.own_conservative_social_3cat  IAT_score $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other 2.round, vce(cluster ID)
lincom 1.other_pride  + 1.other_pride#1.own_female


** interacting stand on biased lgbt views 
 xtreg gave other_pride i.other_pride##c.biased_lgbt_views_index   i.own_conservative_social_3cat i.own_nonhetero_prolific IAT_score i.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other  2.round, vce(cluster ID)
lincom 1.other_pride  + 1.other_pride#c.biased_lgbt_views_index


** interacting IAT
 xtreg gave other_pride i.other_pride##c.IAT_score  biased_lgbt_views_index i.own_conservative_social_3cat i.own_nonhetero_prolific i.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other 2.round, vce(cluster ID)
lincom 1.other_pride  + 1.other_pride#c.IAT_score

** interacting stand on social issues 
 xtreg gave other_pride i.other_pride##i.own_conservative_social_3cat  biased_lgbt_views_index  i.own_nonhetero_prolific  IAT_score i.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other 2.round, vce(cluster ID)
lincom 1.other_pride  + 1.other_pride#3.own_conservative_social_3cat







*****  TABLE D.2: OLS Regression Results for Amount Sent -- Using Perceptions with Both Recipients *****


use "Data/cleaned-dict.dta", clear

// create a variable for biased LGBT views
gen biased_lgbt_views_index = -1*own_lgbt_view_index

egen lgbt_bias_mean = mean(biased_lgbt_views_index)
egen lgbt_bias_sd = sd(biased_lgbt_views_index)
replace biased_lgbt_views_index = (biased_lgbt_views_index - lgbt_bias_mean)/lgbt_bias_sd


// Change the labels for tables

label variable own_nonhetero_prolific "DM: Non-Heterosexual"
label variable other_pride "Recip: Pride"
label variable biased_lgbt_views_index "DM: Biased LGBTQ+ Views"
label variable own_conservative_social_3cat "DM: Conservative Political Leaning"
label variable own_female "DM: Female"


// Define independent variables
global dict_ctrl_demo "own_female own_trans c.own_age own_bachelor own_ethn_black own_ethn_latino own_ethn_asian own_ethn_other_recat own_christian "
global dict_ctrl_ally "own_ally_consider "
global dict_ctrl_lgbtexpose "own_lgbt_interact_recat own_lgbt_friend"
global dict_ctrl_other "own_incon_relations_reported own_incon_attraction_reported  recip_ally recip_perc_female recip_perc_trans recip_perc_conservative recip_perc_age "


xtset ID round

 xtreg gave 1.recip_perc_nonhetero  2.round, vce(cluster ID)
 xtreg gave 1.recip_perc_nonhetero   1.own_nonhetero_prolific 1.own_female $dict_ctrl_demo  2.round, vce(cluster ID) 
 xtreg gave 1.recip_perc_nonhetero   1.own_nonhetero_prolific biased_lgbt_views_index i.own_conservative_social_3cat IAT_score 1.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other  2.round , vce(cluster ID)


** interacting DM's sexual orientation
 xtreg gave 1.recip_perc_nonhetero    biased_lgbt_views_index i.own_conservative_social_3cat  IAT_score 1.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other 1.recip_perc_nonhetero##1.own_nonhetero_prolific  2.round, vce(cluster ID)
lincom 1.recip_perc_nonhetero + 1.recip_perc_nonhetero#1.own_nonhetero_prolific

** interacting dictator's gender-female
 xtreg gave 1.recip_perc_nonhetero 1.recip_perc_nonhetero##1.own_female  biased_lgbt_views_index  1.own_nonhetero_prolific i.own_conservative_social_3cat  IAT_score $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other  2.round , vce(cluster ID)
lincom 1.recip_perc_nonhetero  + 1.recip_perc_nonhetero#1.own_female


** interacting stand on biased lgbt views 
 xtreg gave 1.recip_perc_nonhetero 1.recip_perc_nonhetero##c.biased_lgbt_views_index   i.own_conservative_social_3cat 1.own_nonhetero_prolific IAT_score  1.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other  2.round , vce(cluster ID)
lincom 1.recip_perc_nonhetero  + 1.recip_perc_nonhetero#c.biased_lgbt_views_index


** interacting IAT
 xtreg gave 1.recip_perc_nonhetero 1.recip_perc_nonhetero##c.IAT_score  biased_lgbt_views_index i.own_conservative_social_3cat 1.own_nonhetero_prolific 1.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other  2.round, vce(cluster ID)
lincom 1.recip_perc_nonhetero  + 1.recip_perc_nonhetero#c.IAT_score

** interacting stand on social issues 
 xtreg gave 1.recip_perc_nonhetero 1.recip_perc_nonhetero##i.own_conservative_social_3cat  biased_lgbt_views_index  1.own_nonhetero_prolific  IAT_score 1.own_female $dict_ctrl_demo $dict_ctrl_ally $dict_ctrl_lgbtexpose $dict_ctrl_other  2.round, vce(cluster ID)
lincom 1.recip_perc_nonhetero  + 1.recip_perc_nonhetero#3.own_conservative_social_3cat






********************************************************************************
********************************************************************************
*******************************  APPENDIX FIGURES ******************************
********************************************************************************
********************************************************************************	


***** FIGURE A.1: Recipients' Belief about Amount Sent to Other Pride and Non-Pride Recipients *****

use "Data/cleaned-recip.dta", clear
collapse (mean) mean_predict=predict_average_1 (sd) sd_predict=predict_average_1 (count) n_predict=predict_average_1 (sem) se_predict=predict_average_1, by(other_pride_1 informed_choice own_female_prolific)

generate hi_predict = mean_predict + invttail(n_predict-1,0.025)*(sd_predict / sqrt(n_predict))
generate lo_predict = mean_predict - invttail(n_predict-1,0.025)*(sd_predict / sqrt(n_predict))

gen group=.
replace group=1 if other_pride_1==0 & informed_choice==0 & own_female_prolific==0
replace group=2 if other_pride_1==1 & informed_choice==0 & own_female_prolific==0
replace group=3.5 if other_pride_1==0 & informed_choice==1 & own_female_prolific==0
replace group=4.5 if other_pride_1==1 & informed_choice==1 & own_female_prolific==0
replace group=6 if other_pride_1==0 & informed_choice==0 & own_female_prolific==1
replace group=7 if other_pride_1==1 & informed_choice==0 & own_female_prolific==1
replace group=8.5 if other_pride_1==0 & informed_choice==1 & own_female_prolific==1
replace group=9.5 if other_pride_1==1 & informed_choice==1 & own_female_prolific==1


foreach y of numlist 1 2 3.5 4.5 6 7 8.5 9.5 {
	local x=floor(`y')
	cap drop temp1
	cap drop temp2
	sum mean_predict if group==`y'
	gen temp1=r(mean)
	tostring temp1, gen(temp2) format("%12.1f") force
	local labmean`x'=temp2
	local labmean`x'position=5
	local labse`x'position=2

	cap drop temp1
	cap drop temp2
	sum se_predict if group==`y'
	gen temp1=r(mean)
	tostring temp1, gen(temp2) format("%12.1f") force
	local labse`x'=temp2
	
	local labmean`x'Xposition=`y'
	local labse`x'Xposition=`y'
	
}

twoway (bar mean_predict group if other_pride_1==0, fcolor(white) lcolor(black)) ///
	(bar mean_predict group if other_pride_1==1, fcolor(gray) lcolor(gray)) ///
	(rcap hi_predict lo_predict group, lcolor(black)), ///
	xtitle(" ") xlabel(1.5 "Uninformed" 4 "Informed" 6.5 "Uninformed" 9 "Informed" 2.75 `" " ""Men""' 7.75 `" " " "Women""', noticks) ///
	xscale(range(0 10.5)) ///
	yscale(range(0 60)) ylabel(0(20)60, labsize(medlarge) angle(horizontal)) ///
	ytitle("Belief in average amount sent", size(medlarge)) ///
	legend(order(1 "Non-Pride" 2 "Pride") rows(1)) ///
	graphregion(color(white)) bgcolor(white) ///
	text(`labmean1position' `labmean1Xposition' "`labmean1'", place(c) size(medsmall)) ///
	text(`labse1position' `labse1Xposition' "(`labse1')", place(c) size(medsmall)) ///
	text(`labmean2position' `labmean2Xposition' "`labmean2'", place(c) size(medsmall)) ///
	text(`labse2position' `labse2Xposition' "(`labse2')", place(c) size(medsmall)) ///
	text(`labmean3position' `labmean3Xposition' "`labmean3'", place(c) size(medsmall)) ///
	text(`labse3position' `labse3Xposition' "(`labse3')", place(c) size(medsmall)) ///
	text(`labmean4position' `labmean4Xposition' "`labmean4'", place(c) size(medsmall)) ///
	text(`labse4position' `labse4Xposition' "(`labse4')", place(c) size(medsmall)) ///
	text(`labmean6position' `labmean6Xposition' "`labmean6'", place(c) size(medsmall)) ///
	text(`labse6position' `labse6Xposition' "(`labse6')", place(c) size(medsmall)) ///
	text(`labmean7position' `labmean7Xposition' "`labmean7'", place(c) size(medsmall)) ///
	text(`labse7position' `labse7Xposition' "(`labse7')", place(c) size(medsmall)) ///
	text(`labmean8position' `labmean8Xposition' "`labmean8'", place(c) size(medsmall)) ///
	text(`labse8position' `labse8Xposition' "(`labse8')", place(c) size(medsmall)) ///
	text(`labmean9position' `labmean9Xposition' "`labmean9'", place(c) size(medsmall)) ///
	text(`labse9position' `labse9Xposition' "(`labse9')", place(c) size(medsmall))
	
graph export "figures/FigureA1.eps", replace
graph export "figures/FigureA1.png", replace
window manage close graph _all

// Wilcoxon rank-sum tests
use "Data/cleaned-recip.dta", clear
bys treatment own_female_prolific: ranksum predict_average_1, by(other_pride_1)




***** FIGURE A.2: Distributions of and Average Amount Sent by Decision-Makers by Sexual Orientation (Endowment = 100 ECU) *****


** Panel (a) - Heterosexual Decision-Makers **

use "Data/cleaned-dict.dta", clear

qui summarize gave if round==1 & own_hetero_prolific==1 & other_pride==0, detail
local Mean_hetero_to_nonpride = r(mean)
qui summarize gave if round==1 & own_hetero_prolific==1 & other_pride==1, detail
local Mean_hetero_to_pride = r(mean)	
	
twoway  (histogram gave if round==1 & own_hetero_prolific==1 & other_pride==1, width(10) start(-0.5) percent fcolor(gray) fintensity(50) lcolor(none)) ///
	(scatteri 0 `Mean_hetero_to_pride' 80 `Mean_hetero_to_pride', recast(line) lcolor(gray) lwidth(thick))  ///
	(histogram gave if round==1 & own_hetero_prolific==1 & other_pride==0, width(10) start(-0.5) percent fcolor(none) lcolor(black)) ///
	(scatteri 0 `Mean_hetero_to_nonpride' 80 `Mean_hetero_to_nonpride', recast(line) lcolor(black) lpattern(dash) lwidth(thick) ), ///
	legend(order(1 "Pride" 2 "Pride (mean)" 3 "Non-Pride" 4 "Non-Pride (mean)")) ///
	yscale(range(0 80)) ylabel(0(20)80, labsize(medlarge) angle(horizontal)) ///
	ytitle(% Decision-Makers, size(medium)) xtitle(Amount Sent, size(medium)) ///
	xscale(range(0 100)) xlabel(0(20)100) ///
	graphregion(color(white)) bgcolor(white)
graph export "figures/FigureA2a.eps", replace
graph export "figures/FigureA2a.png", replace
window manage close graph _all			

// Summary statistics, Kolmogorov-Smirnov, and rank-sum tests
bys other_pride: sum gave if round==1 & own_hetero_prolific==1
ranksum gave if round==1 & own_hetero_prolific==1 , by(other_pride)
ksmirnov gave if round==1 & own_hetero_prolific==1 , by(other_pride)


** Panel (b) - Non-Heterosexual Decision-Makers **
use "Data/cleaned-dict.dta", clear
qui summarize gave if round==1 & own_hetero_prolific==0 & other_pride==0, detail
local Mean_nonhetero_to_nonpride = r(mean)
qui summarize gave if round==1 & own_hetero_prolific==0 & other_pride==1, detail
local Mean_nonhetero_to_pride = r(mean)	
	
twoway  (histogram gave if round==1 & own_hetero_prolific==0 & other_pride==1, width(10) start(-0.5) percent fcolor(gray) fintensity(50) lcolor(none)) ///
	(scatteri 0 `Mean_nonhetero_to_pride' 80 `Mean_nonhetero_to_pride', recast(line) lcolor(gray) lwidth(thick))  ///
	(histogram gave if round==1 & own_hetero_prolific==0 & other_pride==0, width(10) start(-0.5) percent fcolor(none) lcolor(black)) ///
	(scatteri 0 `Mean_nonhetero_to_nonpride' 80 `Mean_nonhetero_to_nonpride', recast(line) lcolor(black) lpattern(dash) lwidth(thick) ), ///
	legend(order(1 "Pride" 2 "Pride (mean)" 3 "Non-Pride" 4 "Non-Pride (mean)")) ///
	yscale(range(0 80)) ylabel(0(20)80, labsize(medlarge) angle(horizontal)) ///
	ytitle(% Decision-Makers, size(medium)) xtitle(Amount Sent, size(medium)) ///
	xscale(range(0 100)) xlabel(0(20)100) ///
	graphregion(color(white)) bgcolor(white)
graph export "figures/FigureA2b.eps", replace
graph export "figures/FigureA2b.png", replace
window manage close graph _all			

// Summary statistics, Kolmogorov-Smirnov, and rank-sum tests
bys other_pride: sum gave if round==1 & own_hetero_prolific==0
ranksum gave if round==1 & own_hetero_prolific==0 , by(other_pride)
ksmirnov gave if round==1 & own_hetero_prolific==0 , by(other_pride)




***** FIGURE C.1: Choice of Pride Flag *****

** Panel (a) - by LGBTQ+ Allyship **

local t=1
foreach x of numlist 0/1 {
	foreach y of numlist 0/1 {
		use "Data/cleaned-recip.dta", clear
		ci proportions own_flag_pride if informed_choice==`x' & own_ally_consider==`y'
		return list
		svret r, keep(r(lb) r(ub))
		gen informed_choice=`x'
		gen own_ally_consider=`y'
		save "temp`t'.dta", replace
		local t=`t'+1
	}
}

clear
foreach x of numlist 1/4 {
	append using "temp`x'.dta"
	erase "temp`x'.dta"
}
save "temp.dta", replace

use "Data/cleaned-recip.dta", clear
merge m:1 informed_choice own_ally_consider using "temp.dta", force

collapse (mean) own_flag_pride r_lb r_ub, by(informed_choice own_ally_consider)

gen group=.
replace group=1 if informed_choice==0 & own_ally_consider==0
replace group=2 if informed_choice==1 & own_ally_consider==0
replace group=3.5 if informed_choice==0 & own_ally_consider==1
replace group=4.5 if informed_choice==1 & own_ally_consider==1

twoway (bar own_flag_pride group if informed_choice==0, fcolor(white) lcolor(black) barwidth(0.75)) ///
	(bar own_flag_pride group if informed_choice==1, fcolor(gs6) lcolor(gs6) barwidth(0.75)) ///
	(rcap r_ub r_lb group, lcolor(black)), ///
	xtitle(" ") xlabel(1.5 "Non-Ally" 4 "Ally", noticks) ///
	xscale(range(0 5.5)) ///
	yscale(range(0 1)) ylabel(0(.2)1, labsize(medlarge) angle(horizontal)) ///
	ytitle("Proportion of Recipients", size(medlarge)) ///
	legend(order(1 "Uninformed-Choice" 2 "Informed-Choice" ) rows(1)) ///
	graphregion(color(white)) bgcolor(white)
graph export "figures/FigureC1a.eps", replace
graph export "figures/FigureC1a.png", replace
window manage close graph _all
erase temp.dta

// Fisher's exact tests
use "Data/cleaned-recip.dta", clear
bys informed_choice: tab own_flag_pride own_ally_consider, exact
bys own_ally_consider: tab own_flag_pride informed_choice, exact


** Panel (b) - by Political Views on Social Issues **

local t=1
foreach x of numlist 0/1 {
	foreach y of numlist 1/4 {
		use "Data/cleaned-recip.dta", clear
		ci proportions own_flag_pride if informed_choice==`x' & own_conservative_social_recat==`y'
		return list
		svret r, keep(r(lb) r(ub))
		gen informed_choice=`x'
		gen own_conservative_social_recat=`y'
		save "temp`t'.dta", replace
		local t=`t'+1
	}
}

clear
foreach x of numlist 1/8 {
	append using "temp`x'.dta"
	erase "temp`x'.dta"
}
save "temp.dta", replace

use "Data/cleaned-recip.dta", clear
merge m:1 informed_choice own_conservative_social_recat using "temp.dta", force

collapse (mean) own_flag_pride r_lb r_ub, by(informed_choice own_conservative_social_recat)

gen group=.
replace group=1 if informed_choice==0 & own_conservative_social_recat==1
replace group=2 if informed_choice==1 & own_conservative_social_recat==1
replace group=3.5 if informed_choice==0 & own_conservative_social_recat==2
replace group=4.5 if informed_choice==1 & own_conservative_social_recat==2
replace group=6 if informed_choice==0 & own_conservative_social_recat==3
replace group=7 if informed_choice==1 & own_conservative_social_recat==3
replace group=8.5 if informed_choice==0 & own_conservative_social_recat==4
replace group=9.5 if informed_choice==1 & own_conservative_social_recat==4

twoway (bar own_flag_pride group if informed_choice==0, fcolor(white) lcolor(black)) ///
	(bar own_flag_pride group if informed_choice==1, fcolor(gs6) lcolor(gs6)) ///
	(rcap r_ub r_lb group, lcolor(black)), ///
	xtitle(" ") xlabel(1.5 "Very Lib" 4 "Lib" 6.5 "Neither" 9 "Cons / Very Cons", noticks) ///
	xscale(range(0 10.5)) ///
	yscale(range(0 1)) ylabel(0(.2)1, labsize(medlarge) angle(horizontal)) ///
	ytitle("Proportion of Recipients", size(medlarge)) ///
	legend(order(1 "Uninformed-Choice" 2 "Informed-Choice" ) rows(1)) ///
	graphregion(color(white)) bgcolor(white)
graph export "figures/FigureC1b.eps", replace
graph export "figures/FigureC1b.png", replace
window manage close graph _all
erase temp.dta

// Fisher's exact tests
use "Data/cleaned-recip.dta", clear
bys own_conservative_social_recat: tab own_flag_pride informed_choice, exact

// Distribution of views on social issues
use "Data/cleaned-recip.dta", clear
tab own_conservative_social_recat
tab own_conservative_social




***** FIGURE D.1: Proportion of Decision-Makers who Believe Recipient is Non-Heterosexual (left) or an LGBTQ+ Ally (right) *****


** Panel (a): Recipient is Perceived to be Non-Heterosexual **

use "Data/cleaned-dict.dta", clear

local t=1
foreach x of numlist 0/1 {
		use "Data/cleaned-dict.dta", clear
		keep if round==1
		ci proportions recip_perc_nonhetero if other_pride==`x'
		return list
		svret r, keep(r(lb) r(ub))
		gen other_pride=`x'
		save "temp`t'.dta", replace
		local t=`t'+1
}

clear
foreach x of numlist 1/2 {
	append using "temp`x'.dta"
	erase "temp`x'.dta"
}
save "temp.dta", replace

use "Data/cleaned-dict.dta", clear
keep if round==1
merge m:1 other_pride using "temp.dta", force

collapse (mean) recip_perc_nonhetero r_lb r_ub, by(other_pride)

gen group=.
replace group=1 if other_pride==0 
replace group=2 if other_pride==1 

twoway (bar recip_perc_nonhetero group if group==1, fcolor(white) lcolor(black) barwidth(0.75)) ///
(bar recip_perc_nonhetero group if group==2, fcolor(gray) lcolor(gray) barwidth(0.75)) ///
	(rcap r_ub r_lb group, lcolor(black)), ///
	xtitle(" ") xlabel("" , noticks) ///
	xscale(range(0.25 2.75)) ///
	yscale(range(0 1)) ylabel(0(0.2)1, labsize(medlarge) angle(horizontal)) ///
	ytitle("Proportion of Decision-Makers", size(medlarge)) ///
	legend(order(1 "Non-Pride" 2 "Pride" ) rows(1)) ///
	graphregion(color(white)) bgcolor(white)
graph export "figures/FigureD1a.eps", replace
graph export "figures/FigureD1a.png", replace
window manage close graph _all
erase temp.dta

// Summary statistics and Fisher's exact tests: Perceive recipient to be non-heterosexual (Pride vs. Non-Pride)
use "Data/cleaned-dict.dta", clear
bys other_pride : sum recip_perc_nonhetero if round==1
tab recip_perc_nonhetero other_pride if round==1, exact


** Panel (b): Recipient is Perceived to be an LGBTQ+ Ally **

use "Data/cleaned-dict.dta", clear

local t=1
foreach x of numlist 0/1 {
		use "Data/cleaned-dict.dta", clear
		keep if round==1
		ci proportions recip_ally if other_pride==`x'
		return list
		svret r, keep(r(lb) r(ub))
		gen other_pride=`x'
		save "temp`t'.dta", replace
		local t=`t'+1
}

clear
foreach x of numlist 1/2 {
	append using "temp`x'.dta"
	erase "temp`x'.dta"
}
save "temp.dta", replace

use "Data/cleaned-dict.dta", clear
keep if round==1
merge m:1 other_pride using "temp.dta", force

collapse (mean) recip_ally r_lb r_ub, by(other_pride)

gen group=.
replace group=1 if other_pride==0 
replace group=2 if other_pride==1 

twoway (bar recip_ally group if group==1, fcolor(white) lcolor(black) barwidth(0.75)) ///
(bar recip_ally group if group==2, fcolor(gray) lcolor(gray) barwidth(0.75)) ///
	(rcap r_ub r_lb group, lcolor(black)), ///
	xtitle(" ") xlabel("" , noticks) ///
	xscale(range(0.25 2.75)) ///
	yscale(range(0 1)) ylabel(0(0.2)1, labsize(medlarge) angle(horizontal)) ///
	ytitle("Proportion of Decision-Makers", size(medlarge)) ///
	legend(order(1 "Non-Pride" 2 "Pride" ) rows(1)) ///
	graphregion(color(white)) bgcolor(white)
graph export "figures/FigureD1b.eps", replace
graph export "figures/FigureD1b.png", replace
window manage close graph _all
erase temp.dta

// Summary statistics and Fisher's exact tests: Perceive recipient to be non-heterosexual (Pride vs. Non-Pride)
use "Data/cleaned-dict.dta", clear
bys other_pride : sum recip_ally if round==1
tab recip_ally other_pride if round==1, exact




********************************************************************************
********************************************************************************

capture log close _all
