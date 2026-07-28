* ==============================================================================
* SOURCE OF CARE & ANTIBIOTIC USE – CROSS‑TABS, CHI‑SQUARE, REGRESSION
* ==============================================================================
clear all
set more off
set linesize 120

local raw "C:/Users/HP/Desktop/MICS/Bangladesh MICS6 SPSS Datasets/"

* --- 1. Load and merge all necessary datasets ---
import spss using "`raw'hh.sav", clear
rename *, lower
tempfile hh_temp
save "`hh_temp'"

import spss using "`raw'hl.sav", clear
rename *, lower
tempfile hl_temp
save "`hl_temp'"

import spss using "`raw'wm.sav", clear
rename *, lower
rename ln uf4
tempfile women_temp
save "`women_temp'"

import spss using "`raw'ch.sav", clear
rename *, lower

merge m:1 hh1 hh2 uf4 using "`women_temp'", keep(master match) nogen
merge m:1 hh1 hh2 using "`hh_temp'", keep(master match) nogen

* --- 2. Sick children ---
gen byte child_had_ari = (ca17 == 1 | inlist(ca18, 1, 3)) if !missing(ca17, ca18)
replace child_had_ari = 0 if missing(child_had_ari)

gen byte child_was_sick = 0
replace child_was_sick = 1 if ca1 == 1
replace child_was_sick = 1 if ca14 == 1
replace child_was_sick = 1 if ca16 == 1
replace child_was_sick = 1 if child_had_ari == 1
replace child_was_sick = . if missing(ca1) & missing(ca14) & missing(ca16) & missing(child_had_ari)
keep if child_was_sick == 1

* --- 3. Care‑seeking & antibiotic use ---
gen byte care_was_sought = 0
replace care_was_sought = 1 if (ca5 == 1 & ca1 == 1)
replace care_was_sought = 1 if (ca20 == 1 & (ca14 == 1 | ca16 == 1 | child_had_ari == 1))

gen byte ab_diarr = .
replace ab_diarr = 0 if ca1 == 1 & missing(ca13a) & missing(ca13l)
replace ab_diarr = 1 if ca1 == 1 & ///
    (ca13a == "A" | ca13a == "a" | ca13l == "L" | ca13l == "l")

gen byte ab_fever = .
replace ab_fever = 0 if (ca14 == 1 | ca16 == 1 | child_had_ari == 1) & ///
    missing(ca23l) & missing(ca23m) & missing(ca23n) & missing(ca23o)
replace ab_fever = 1 if (ca14 == 1 | ca16 == 1 | child_had_ari == 1) & ///
    (ca23l == "L" | ca23l == "l" | ca23m == "M" | ca23m == "m" | ///
     ca23n == "N" | ca23n == "n" | ca23o == "O" | ca23o == "o")

gen byte any_antibiotic = (ab_diarr == 1 | ab_fever == 1)
replace care_was_sought = 1 if any_antibiotic == 1
keep if care_was_sought == 1

* --- 4. Detailed source of care (11 categories) ---
gen byte source11 = .

foreach v in ca6a ca21a {
    replace source11 = 1 if !missing(`v') & `v'!="" & missing(source11)
}
foreach v in ca6i ca21i {
    replace source11 = 2 if !missing(`v') & `v'!="" & missing(source11)
}
foreach v in ca6b ca6c ca21b ca21c {
    replace source11 = 3 if !missing(`v') & `v'!="" & missing(source11)
}
foreach v in ca6j ca21j {
    replace source11 = 4 if !missing(`v') & `v'!="" & missing(source11)
}
foreach v in ca6k ca21k {
    replace source11 = 5 if !missing(`v') & `v'!="" & missing(source11)
}
foreach v in ca6d ca6l ca21d ca21l {
    replace source11 = 6 if !missing(`v') & `v'!="" & missing(source11)
}
foreach v in ca6e ca6m ca21e ca21m {
    replace source11 = 7 if !missing(`v') & `v'!="" & missing(source11)
}
foreach v in ca6q ca21q {
    replace source11 = 8 if !missing(`v') & `v'!="" & missing(source11)
}
foreach v in ca6r ca21r {
    replace source11 = 9 if !missing(`v') & `v'!="" & missing(source11)
}
foreach v in ca6p ca21p {
    replace source11 = 10 if !missing(`v') & `v'!="" & missing(source11)
}
foreach v in ca6x ca6h ca6o ca6n ca21x ca21h ca21o ca21n {
    replace source11 = 11 if !missing(`v') & `v'!="" & missing(source11)
}
replace source11 = 11 if missing(source11)

capture label drop source11_lbl
label define source11_lbl ///
    1 "Public hospital" ///
    2 "Private hospital/clinic" ///
    3 "Public clinic/health centre" ///
    4 "Private physician" ///
    5 "Pharmacy" ///
    6 "Community health worker" ///
    7 "Mobile/outreach clinic" ///
    8 "Shop/market/street" ///
    9 "Traditional practitioner" ///
    10 "Relative/friend" ///
    11 "Other"
label values source11 source11_lbl

* --- 5. Combined source (4 groups) ---
gen byte source4 = .
replace source4 = 1 if inlist(source11, 1, 3, 6, 7)
replace source4 = 2 if inlist(source11, 2, 4)
replace source4 = 3 if source11 == 5
replace source4 = 4 if inlist(source11, 8, 9, 10, 11)

capture label drop source4_lbl
label define source4_lbl ///
    1 "Public health facility" ///
    2 "Private health facility (non-pharmacy)" ///
    3 "Pharmacy" ///
    4 "Other informal/other"
label values source4 source4_lbl

* ==============================================================================
* 6. COVARIATES (Mother's age group removed)
* ==============================================================================

* Child factors
gen byte child_is_male = (hl4 == 1) if !missing(hl4)

gen byte child_age_group = .
replace child_age_group = 1 if cage >= 0  & cage <= 5
replace child_age_group = 2 if cage >= 6  & cage <= 11
replace child_age_group = 3 if cage >= 12 & cage <= 23
replace child_age_group = 4 if cage >= 24 & cage <= 35
replace child_age_group = 5 if cage >= 36 & cage <= 47
replace child_age_group = 6 if cage >= 48 & cage <= 59
capture label drop lbl_age_group
label define lbl_age_group 1 "0–5 months" 2 "6–11 months" 3 "12–23 months" ///
                           4 "24–35 months" 5 "36–47 months" 6 "48–59 months"
label values child_age_group lbl_age_group

gen byte child_had_diarrhea = (ca1 == 1)
gen byte child_had_fever   = (ca14 == 1)
gen byte child_had_cough   = (ca16 == 1)
gen byte multiple_ill = ((child_had_diarrhea==1) + (child_had_fever==1) + (child_had_cough==1) + (child_had_ari==1)) >= 2
label var multiple_ill "Multiple symptoms (>=2)"

* Mother factors
gen byte mother_edu_level = melevel
capture label drop lbl_mother_edu
label define lbl_mother_edu 0 "No education" 1 "Primary" 2 "Secondary" 3 "Higher"
label values mother_edu_level lbl_mother_edu

* Household factors
gen byte urban = (hh6 == 1) if !missing(hh6)
label var urban "Urban residence"

gen byte hh_wealth_quintile = windex5
capture label drop lbl_wealth
label define lbl_wealth 1 "Poorest" 2 "Poor" 3 "Middle" 4 "Richer" 5 "Richest"
label values hh_wealth_quintile lbl_wealth

gen byte family_size = hh48 if !missing(hh48) & hh48 < 30
gen byte family_size_cat = .
replace family_size_cat = 1 if family_size >= 1 & family_size <= 3
replace family_size_cat = 2 if family_size >= 4 & family_size <= 6
replace family_size_cat = 3 if family_size >= 7
capture label drop lbl_familysize
label define lbl_familysize 1 "Small (1-3)" 2 "Medium (4-6)" 3 "Large (7+)"
label values family_size_cat lbl_familysize

gen double crowding_ratio = hh48 / hc3 if !missing(hh48, hc3) & hc3 > 0 & hc3 < 20
gen byte crowding_category = .
replace crowding_category = 1 if crowding_ratio < 3
replace crowding_category = 2 if crowding_ratio >= 3 & crowding_ratio < 5
replace crowding_category = 3 if crowding_ratio >= 5
capture label drop lbl_crowding
label define lbl_crowding 1 "Low (<3)" 2 "Medium (3-4)" 3 "High (5+)"
label values crowding_category lbl_crowding

* ==============================================================================
* 7. Complete cases
* ==============================================================================
drop if missing(source4, source11, any_antibiotic, ///
    child_is_male, child_age_group, multiple_ill, ///
    mother_edu_level, urban, ///
    hh_wealth_quintile, family_size_cat, crowding_category)

di "Final analytic sample size: " _N

* ==============================================================================
* 8. Survey design
* ==============================================================================
capture confirm variable stratum
if _rc {
    egen stratum = group(hh7 hh6)
}
svyset psu [pw = chweight], strata(stratum) singleunit(centered) vce(linearized)

* ==============================================================================
* 9. CROSS‑TABULATIONS & CHI‑SQUARE TESTS
* ==============================================================================

* Combined source (4 groups)
di _n "============================================================"
di "CROSS‑TAB: Combined source (4 groups) vs Antibiotic use"
di "============================================================"
tab source4 any_antibiotic, row col chi2

* Detailed source (11 categories)
di _n "============================================================"
di "CROSS‑TAB: Detailed source (11 categories) vs Antibiotic use"
di "============================================================"
tab source11 any_antibiotic, row col chi2

* ==============================================================================
* 10. LOGISTIC REGRESSION MODELS (Pharmacy as reference)
* ==============================================================================

* --- Combined source (4 groups) ---
di _n "========== COMBINED SOURCE (4 groups) – PHARMACY REF =========="
di "Crude model:"
svy: logit any_antibiotic ib3.source4, or
estimates store crude4

di "Adjusted model:"
svy: logit any_antibiotic ib3.source4 ///
    i.child_is_male i.child_age_group i.multiple_ill ///
    i.mother_edu_level i.urban ///
    i.hh_wealth_quintile i.family_size_cat i.crowding_category, or
estimates store adj4

* --- Detailed source (11 categories) ---
di _n "========== DETAILED SOURCE (11 categories) – PHARMACY REF =========="
di "Crude model:"
svy: logit any_antibiotic ib5.source11, or
estimates store crude11

di "Adjusted model:"
svy: logit any_antibiotic ib5.source11 ///
    i.child_is_male i.child_age_group i.multiple_ill ///
    i.mother_edu_level i.urban ///
    i.hh_wealth_quintile i.family_size_cat i.crowding_category, or
estimates store adj11

* ==============================================================================
* 11. Comparison table of odds ratios
* ==============================================================================
local all_possible 1.source4 2.source4 4.source4 ///
                   1.source11 2.source11 3.source11 4.source11 ///
                   6.source11 8.source11 9.source11 10.source11 11.source11
local keep_list
foreach v of local all_possible {
    capture _estimates hold `v', from(crude4)
    if _rc == 0 {
        local keep_list `keep_list' `v'
    }
}

if "`keep_list'" != "" {
    estimates table crude4 adj4 crude11 adj11, ///
        keep(`keep_list') eform ///
        title("Odds ratios for any antibiotic use vs. Pharmacy reference")
}
else {
    di "Note: No source coefficients available for the comparison table."
}

save "MICS6_Source_AB_CrossTabs.dta", replace
di _n "Analysis complete. Dataset saved."