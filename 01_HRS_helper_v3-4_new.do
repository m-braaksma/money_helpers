
* ==============================================================================
* HRS HELPER
*
* current version: 7/26/24
* author: Matt Braaksma
* ==============================================================================


* ==============================================================================
* Input: HRS Exit files for 2004-2020
*        HRS Core files for 2002-2018
* Output: Helper-level file
* ==============================================================================


* Clear Settings ===============================================================
clear *
clear matrix
set varabbrev off
set maxvar 30000
set more off
macro drop _all
* ==============================================================================


* Set Directory ================================================================
glo path="/Volumes/T7 Touch" // Matt's path
  glo data="$path/data_raw/HRS"
  glo output="$path/data_projects/HRS"
* ==============================================================================


* EXIT: MERGE WAVES ===================================================
// MERGE LOOP
foreach y in 02 04 06 08 10 12 14 16 18 20 {

  // WAVE
  if `y'==02 local wave "S"
  if `y'==04 local wave "T"
  if `y'==06 local wave "U"
  if `y'==08 local wave "V"
  if `y'==10 local wave "W"
  if `y'==12 local wave "X"
  if `y'==14 local wave "Y"
  if `y'==16 local wave "Z"
  if `y'==18 local wave "XQ"
  if `y'==20 local wave "XR"

  // RESPONDENT-LEVEL FILES
  use "$data/x`y'exit/x`y'dta/X`y'PR_R.dta", clear
  merge 1:1 HHID PN using "$data/x`y'exit/x`y'dta/X`y'A_R.dta", nogen
  merge 1:1 HHID PN using "$data/x`y'exit/x`y'dta/X`y'B_R.dta", nogen
  merge 1:1 HHID PN using "$data/x`y'exit/x`y'dta/X`y'C_R.dta", nogen
  merge 1:1 HHID PN using "$data/x`y'exit/x`y'dta/X`y'D_R.dta", nogen
  merge 1:1 HHID PN using "$data/x`y'exit/x`y'dta/X`y'E_R.dta", nogen
  merge 1:1 HHID PN using "$data/x`y'exit/x`y'dta/X`y'J_R.dta", nogen

  merge 1:1 HHID PN using "$data/x`y'exit/x`y'dta/X`y'G_R.dta", nogen
  merge 1:1 HHID PN using "$data/x`y'exit/x`y'dta/X`y'N_R.dta", nogen
  merge 1:1 HHID PN using "$data/x`y'exit/x`y'dta/X`y'T_R.dta", nogen
  isid HHID PN
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'
  gen xwave = (`y'/2) + 5
    // rename `wave'SUBHH _`wave'SUBHH
  rename `wave'* *
    // rename _`wave'SUBHH `wave'SUBHH
  tempfile r_exit_`y'
  save `r_exit_`y''

  // MC-LEVEL FILES
  use "$data/x`y'exit/x`y'dta/X`y'PR_MC.dta", clear
    drop if OPN=="997" // 2008 duplicates, Unknown children
    drop if (`y'==10 & HHID=="045173" & OPN=="154") | (`y'==10 & HHID=="052698" & OPN=="208")
  isid HHID `wave'SUBHH OPN
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'
  gen xwave = (`y'/2) + 5

    // rename `wave'SUBHH _`wave'SUBHH
  rename `wave'* *
    // rename _`wave'SUBHH `wave'SUBHH
  duplicates drop HHID OPN, force // DROP DUPLICATES IN SAME HH
  tempfile mc_exit_`y'
  save `mc_exit_`y''

  // HELPER-LEVEL FILES
  use "$data/x`y'exit/x`y'dta/X`y'G_HP.dta", clear
  isid HHID PN OPN
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'
  gen xwave = (`y'/2) + 5
    // rename `wave'SUBHH _`wave'SUBHH
  rename `wave'* *
    // rename _`wave'SUBHH `wave'SUBHH
  tempfile hp_exit_`y'
  save `hp_exit_`y''


  // TRANSFER-LEVEL FILES (TC/FC)
  use "$data/x`y'exit/x`y'dta/X`y'E_TC.dta", clear
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'
  gen xwave = (`y'/2) + 5
  rename `wave'* *
  tempfile tc_exit_`y'
  save `tc_exit_`y''
  use "$data/x`y'exit/x`y'dta/X`y'E_FC.dta", clear
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'
  gen xwave = (`y'/2) + 5
  rename `wave'* *
  tempfile fc_exit_`y'
  save `fc_exit_`y''
}

* ==============================================================================



* CORE: MERGE WAVES ===================================================
// MERGE LOOP
foreach y in 00 02 04 06 08 10 12 14 16 18 20 {

  // WAVE
  if `y'==00 local wave "G"
  if `y'==02 local wave "H"
  if `y'==04 local wave "J"
  if `y'==06 local wave "K"
  if `y'==08 local wave "L"
  if `y'==10 local wave "M"
  if `y'==12 local wave "N"
  if `y'==14 local wave "O"
  if `y'==16 local wave "P"
  if `y'==18 local wave "Q"
  if `y'==20 local wave "R"

  // RESPONDENT-LEVEL FILES
  use "$data/h`y'core/h`y'dta/H`y'PR_R.dta", clear
  merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'A_R.dta", nogen
  merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'B_R.dta", nogen
  merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'C_R.dta", nogen
  merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'D_R.dta", nogen
  merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'G_R.dta", nogen
  merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'J_R.dta", nogen
  merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'N_R.dta", nogen
  merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'T_R.dta", nogen
  if `y'>00 {
    merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'M1_R.dta", nogen 
    merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'P_R.dta", nogen 
    merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'S_R.dta", nogen 
    merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'V_R.dta", nogen 
    merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'W_R.dta", nogen 
  }
  if `y'>02 {
    merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'LB_R.dta", nogen 
  }
  if `y'>00 & `y'<16 {
    merge 1:1 HHID PN using "$data/h`y'core/h`y'dta/H`y'RC_R.dta", nogen
  }

  if `y'>00 {
  preserve
    use "$data/h`y'core/h`y'dta/H`y'IO_R.dta", clear
      rename (HHID PN `wave'SUBHH) (_HHID _PN _`wave'SUBHH)
    rename `wave'* `wave'IO*
      rename (_HHID _PN _`wave'SUBHH) (HHID PN `wave'SUBHH)
    tempfile r_IO
    save `r_IO'
  restore
  merge 1:1 HHID PN using `r_IO', nogen
  }

  isid HHID PN
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'+2
  gen xwave = (`y'/2) + 6
  gen in_core_r = 1
    rename HHID _HHID
    rename PN _PN
    // rename `wave'SUBHH _`wave'SUBHH
  if `y'==00 {
    rename GSUBHH SUBHH
  }
  if `y'>0 {
    rename `wave'* *
  }
    rename _HHID HHID
    rename _PN PN
    // rename _`wave'SUBHH `wave'SUBHH
  tempfile r_core_`y'
  save `r_core_`y''

  // MC-LEVEL FILES
  use "$data/h`y'core/h`y'dta/H`y'PR_MC.dta", clear
  if `y' > 00 {
    merge 1:1 HHID `wave'SUBHH OPN using "$data/h`y'core/h`y'dta/H`y'E_MC.dta", nogen keep(match master)
      drop if (`y'==8 & OPN=="997") // 2008 duplicates, Unknown children
      drop if (`y'==10 & HHID=="045173" & OPN=="154") | (`y'==10 & HHID=="052698" & OPN=="208")
    isid HHID `wave'SUBHH OPN
  }
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'+2
  gen xwave = (`y'/2) + 6
  gen in_core_mc = 1
    rename OPN _OPN
    rename HHID _HHID
    // rename `wave'SUBHH _`wave'SUBHH
    if `y'==00 {
      rename GSUBHH SUBHH
    }
    if `y'>0 {
      rename `wave'* *
    }
    rename _OPN OPN
    rename _HHID HHID
    // rename _`wave'SUBHH `wave'SUBHH
    duplicates drop HHID OPN, force // DROP DUPLICATES IN SAME HH
  tempfile mc_core_`y'
  save `mc_core_`y''

  // HH-LEVEL FILES
  if `y'==0 {
    use "$data/h`y'core/h`y'dta/H`y'CS_H.dta", clear
  }
  if `y'>00 {
    use "$data/h`y'core/h`y'dta/H`y'E_H.dta", clear
    merge 1:1 HHID `wave'SUBHH using "$data/h`y'core/h`y'dta/H`y'H_H.dta", nogen
    merge 1:1 HHID `wave'SUBHH using "$data/h`y'core/h`y'dta/H`y'Q_H.dta", nogen
  }
  if `y'>04 {
    merge 1:1 HHID `wave'SUBHH using "$data/h`y'core/h`y'dta/H`y'U_H.dta", nogen

    if `y' != 20 {
      preserve
      use "$data/h`y'core/h`y'dta/H`y'IO_H.dta", clear
        rename (HHID `wave'SUBHH) (_HHID _`wave'SUBHH)
        rename `wave'* `wave'IO*
        rename (_HHID  _`wave'SUBHH) (HHID `wave'SUBHH)
        tempfile hh_IO
        save `hh_IO'
    restore
    merge 1:1 HHID `wave'SUBHH using `hh_IO', nogen
    }
  }


  isid HHID `wave'SUBHH
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'+2
  gen xwave = (`y'/2) + 6
    if `y'==00 {
      rename GSUBHH SUBHH
    }
    rename HHID _HHID
    // rename `wave'SUBHH _`wave'SUBHH
    if `y'>0 {
      rename `wave'* *
    }
    rename _HHID HHID
    // rename _`wave'SUBHH `wave'SUBHH
  tempfile hh_core_`y'
  save `hh_core_`y''


  // HELPER-LEVEL FILES
  if `y'==00 {
    use "$data/h`y'core/h`y'dta/H`y'E_HP.dta", clear
  }
  if `y'>00 {
    use "$data/h`y'core/h`y'dta/H`y'G_HP.dta", clear
      drop if mi(OPN)
      drop if (`y'==14 & _n==2595) // 2014 duplicate OPN
    }
  di "`y'"
  isid HHID PN OPN
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'+2
  gen xwave = (`y'/2) + 6
  gen in_core_hp = 1
    if `y'==00 {
      rename GSUBHH SUBHH
    }
    rename OPN _OPN
    rename HHID _HHID
    rename PN _PN
    // rename `wave'SUBHH _`wave'SUBHH
    if `y'>0 {
      rename `wave'* *
    }
    rename _OPN OPN
    rename _HHID HHID
    rename _PN PN
    // rename _`wave'SUBHH `wave'SUBHH
  tempfile hp_core_`y'
  save `hp_core_`y''


  // TRANSFER-LEVEL FILES (TC/FC)
  if `y'==0 {
    use "$data/h`y'core/h`y'dta/H`y'D_TC.dta", clear
  }
  if `y'>0 {
    use "$data/h`y'core/h`y'dta/H`y'E_TC.dta", clear
  }
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'+2
  gen xwave = (`y'/2) + 6
  if `y'==00 {
    rename GSUBHH SUBHH
  }
  if `y'>0 {
    rename `wave'* *
  }
  tempfile tc_core_`y'
  save `tc_core_`y''
  if `y'==0 {
    use "$data/h`y'core/h`y'dta/H`y'D_FC.dta", clear
  }
  if `y'>0 {
    use "$data/h`y'core/h`y'dta/H`y'E_FC.dta", clear
  }
  // HARMONIZE VARIABLE NAMES
  gen xyear = `y'+2
  gen xwave = (`y'/2) + 6
    if `y'==00 {
      rename GSUBHH SUBHH
    }
    if `y'>4 {
      rename OPN _OPN
    }
    if `y'>0 {
      rename `wave'* *
    }
    if `y'>4 {
      rename _OPN OPN
    }
  tempfile fc_core_`y'
  save `fc_core_`y''
}

* ==============================================================================




* NEW PROCESS: MERGE EXIT & CORE FILES ======================================================
// use `r_core_12', clear
// isid HHID PN
//
// merge m:1 HHID SUBHH using `hh_core_12'
// keep if _merge==3
// drop _merge
// isid HHID PN
//
// // rename
// merge 1:1 HHID PN using `r_exit_14'
// keep if _merge==2 | _merge==3
// drop _merge
// isid HHID PN
// tempfile r_combined_14
// save `r_combined_14'
//
//
// use `mc_core_12', clear
// // duplicates drop HHID OPN, force
//
// // rename
// merge 1:1 HHID OPN using `mc_exit_14'
// drop _merge
//
// merge m:m HHID using `r_exit_14'
// keep if _merge==2 | _merge==3 // keep every MC that matches an exit R (and childless R)
// drop _merge
// isid HHID PN OPN, missok
// tempfile mc_combined_14
// save `mc_combined_14'
//
//
// use `hp_core_12', clear
// // rename
// merge 1:1 HHID PN OPN using `hp_exit_14'
// keep if _merge==2 | _merge==3 // keep exit HP
// drop _merge
//
// merge m:1 HHID PN using `r_combined_14'
// keep if _merge==3 // keep every HP
// drop _merge
// isid HHID PN OPN
//
// merge 1:1 HHID PN OPN using `mc_combined_14'
// isid HHID PN OPN, missok
//
//
//
// duplicates tag HHID NSUBHH OPN, gen(dup)
// br if dup==1


// MERGE CORE LEVELS: R + MC
local waves 02 04 06 08 10 12 14 16 18 20
foreach y of local waves {

  if `y'==02 local core_y "00"
  if `y'==04 local core_y "02"
  if `y'==06 local core_y "04"
  if `y'==08 local core_y "06"
  if `y'==10 local core_y "08"
  if `y'==12 local core_y "10"
  if `y'==14 local core_y "12"
  if `y'==16 local core_y "14"
  if `y'==18 local core_y "16"
  if `y'==20 local core_y "18"
  di "`y'"

  use `r_core_`core_y'', clear
  isid HHID PN

  merge m:1 HHID SUBHH using `hh_core_`core_y''
  keep if _merge==3
  drop _merge
  isid HHID PN

  if `core_y'>00 {
    rename Z* Z*c
    rename X* X*c
    rename A* A*c
    rename B* B*c
    rename C* C*c
    rename D* D*c
    rename G* G*c
    rename J* J*c
    rename M* M*c
    rename N* N*c
    rename P* P*c
    rename S* S*c
    rename T* T*c
    rename V* V*c
    rename W* W*c
    rename IO* IO*c
  }
  if `core_y'>02 {
    rename LB* LB*c
  }
  if `core_y'>00 & `core_y'<16 {
    rename RC* RC*c
  }

  if `core_y'>00 {
    rename E* E*c
    rename H* H*c
    rename Q* Q*c
  }
  if `core_y'>06 {
    rename U* U*c
  }

  if `core_y'>00 {
    rename HHIDc HHID
    rename PNc PN
    rename SUBHHc SUBHH
  }

  merge 1:1 HHID PN using `r_exit_`y''
  keep if _merge==2 | _merge==3
  drop _merge
  isid HHID PN
  tempfile r_combined_`y'
  save `r_combined_`y''

  // MC
  use `mc_core_`core_y'', clear
  if `core_y'>00 {
    rename X* X*c
    rename E* E*c
  }
  merge 1:1 HHID OPN using `mc_exit_`y''
  drop _merge

  merge m:m HHID using `r_exit_`y''
  keep if _merge==2 | _merge==3 // keep every MC that matches an exit R (and childless R)
  drop _merge
  isid HHID PN OPN, missok
  tempfile mc_combined_`y'
  save `mc_combined_`y''

  // HP
  use `hp_core_`core_y'', clear
    rename G* G*c
  merge 1:1 HHID PN OPN using `hp_exit_`y''
  keep if _merge==2 | _merge==3 // keep exit HP
  drop _merge

  merge m:1 HHID PN using `r_combined_`y''
  keep if _merge==3 // keep every HP
  drop _merge
  isid HHID PN OPN

  merge 1:1 HHID PN OPN using `mc_combined_`y''
  drop _merge

  tempfile hp_mc_r_`y'
  save `hp_mc_r_`y''

}


// ADD TRANFERS
// EXIT TC 2006-2018
use `tc_exit_06', clear
foreach y in 08 10 12 14 16 18 20 {
  append using `tc_exit_`y''
}
collapse (first) E076 E078 E086 (sum) E081, by(HHID PN OPN)
isid HHID PN OPN
tempfile tc_exit
save `tc_exit'

// FC 2006-2018
use `fc_exit_06', clear
foreach y in 08 10 12 14 16 18 20 {
  append using `fc_exit_`y''
}
collapse (first) E088 E090 E099 (sum) E093, by(HHID PN OPN)
isid HHID PN OPN
tempfile fc_exit
save `fc_exit'

// // CORE TC 2006-2018
// use `tc_core_06', clear
// foreach y in 08 10 12 14 16 {
//   append using `tc_core_`y''
// }
// collapse (first) E076 E078 E086 (sum) E081, by(HHID OPN xyear)
// rename E* E*c
// isid HHID OPN
// tempfile tc_core
// save `tc_core'
//
// // FC 2006-2018
// use `fc_core_06', clear
// foreach y in 08 10 12 14 16 {
//   append using `fc_core_`y''
// }
// collapse (first) E088 E090 E099 (sum) E093, by(HHID PN OPN)
// rename E* E*c
// isid HHID PN OPN
// tempfile fc_core
// save `fc_core'



* APPEND ALL WAVES =================================================================
use `hp_mc_r_02', clear
foreach y in 04 06 08 10 12 14 16 18 20 {
  append using `hp_mc_r_`y'', force
  isid HHID PN OPN xyear, missok
}

// MERGE TRANSFERS
merge 1:1 HHID PN OPN using `tc_exit'
keep if _merge==1 | _merge==3
drop _merge
merge 1:1 HHID PN OPN using `fc_exit'
keep if _merge==1 | _merge==3
drop _merge

// CONVERT ID
destring HHID PN OPN, replace
isid HHID PN OPN, missok
* ==============================================================================





* GENERATE KEY VARIABLES ===========================================
// GENERATE HELPER-LEVEL VARIABLES
// Section G : Functional Limitations and Helpers (Helper)
gen relationship = G069
label define rel 1 "SELF" ///
                 2 "SPOUSE/PARTNER" ///
                 3 "SON" ///
                 4 "STEPSON" ///
                 5 "SON-IN-LAW" ///
                 6 "DAUGHTER" ///
                 7 "STEPDAUGHTER" ///
                 8 "DAUGHTER-IN-LAW" ///
                 9 "GRANDCHILD" ///
                 10 "FATHER" ///
                 11 "FATHER-IN-LAW" ///
                 12 "MOTHER" ///
                 13 "MOTHER-IN-LAW" ///
                 14 "R'S PARENTS" ///
                 15 "BROTHER" ///
                 16 "BROTHER-IN-LAW" ///
                 17 "SISTER" ///
                 18 "SISTER-IN-LAW" ///
                 19 "OTHER RELATIVE" ///
                 20 "OTHER INDIVIDUAL" ///
                 21 "ORGANIZATION" ///
                 22 "EMPLOYEE OF INSTITUTION" ///
                 23 "PAID HELPER" ///
                 24 "PROFESSIONAL" ///
                 25 "PROFESSIONAL (SPECIFY)" ///
                 26 "LATE SPOUSE/PARTNER" ///
                 27 "EX-SPOUSE/PARTNER" ///
                 28 "UNLISTED CHILD OR CHILD-IN-LAW" ///
                 29 "NOT PROXY INTERVIEW" ///
                 30 "FORMER STEP-CHILD" ///
                 31 "FORMER CHILD-IN-LAW" ///
                 32 "RELATIONSHIP UNKNOWN" ///
                 33 "SP/P OF GRANDCHILD" ///
                 90 "AMBIGUOUS CHILD RELATIONSHIP" ///
                 91 "AMBIGUOUS CHILD-IN-LAW RELATIONSHIP"
label values relationship rel

// MONEY HELPER
destring G062_1, replace
destring G062_2, replace
destring G063_1, replace
destring G063_2, replace
gen r_mh1 = 0 if G061==5
replace r_mh1 = 1 if (G061==1 & G063_1 <= 91) | (G061==6 & G063_1 <= 91) | (G061==7 & G063_1 <= 91)
replace r_mh1 = . if (G061==6 & G063_1==.) | (G061==7 & G063_1==.) | G063_1==1
replace r_mh1 = 0 if G063_1==1 // self as MH

gen r_mh1_rel = G063_1 if inrange(G063_1,2,91) & r_mh1==1

gen mh1 = 0 if r_mh1==1 & G062_1!=OPN
replace mh1 = 1 if r_mh1==1 & G062_1==OPN
replace mh1 = . if r_mh1==.


gen mh1_rel = G063_1
replace mh1_rel = . if G062_1!=OPN & G063_1 <= 91
// if inlist(G061,1,6,7) & G063_1 <= 91 & G062_1==OPN & OPN!=.
gen rel = mh1_rel
replace rel = . if mh1_rel==1

egen r_mh1_match = max(mh1), by(HHID PN)

gen mh1d=.
byso HHID PN: gen id = _n
replace mh1d=0 if G061==5 & id==1

gen mh1dd = .
replace mh1dd = 1 if rel <= 91
replace mh1dd = 0 if G061==5

// (but I want mh1d = 0 for only one helper if an R has multiple helpers, and choose any one helper randomly,
// perhaps the first helper. It does not matter, because we simply need to count the number of Rs with G061==5 and we have to count an R
// once even if the R has multiple helpers).
// replace mh1d=1 if mh1_rel <=91 & (G061==1 & G063_1==.)

// CHILD INCOME
// child_income: categorical
gen child_income1 = 1 if E042c==1 // <= 10,000 (E042==1 for all years)
replace child_income1 = 2 if E042c==4 // > 10,000 and <= 35,000 (E042==4 for all years)
replace child_income1 = 3 if E042c==5 & E043c==5 // >=35,000 and <= 70,000 (E042==5 & E043==5 for all years)
replace child_income1 = 4 if E043c==1 // > 70,000 (E043==1 for all years)

// Child_income2: categorical
gen child_income2 = 1 if E042c==1 // <= 10,000 (E042==1 for all years)
replace child_income2 = 2 if E042c==4 // > 10,000 and <= 35,000 (E042==4 for all years)
replace child_income2 = 3 if E042c==5 & E043c==5 // >=35,000 and <= 70,000 (E042==5 & E043==5 for all years)
replace child_income2 = 4 if (E043c==1 & inrange(xyear,04,12)) | (E043c==1 & E039c==5 & inrange(xyear,14,18)) // > 70,000  and <= 100,000 (E043==1 for years 04, 06, 08, 10, and E043==1 & E039==5 for years 12, 14, 16, 18)
replace child_income2 = 5 if E039c==1 & inrange(xyear,14,18) // > 100,000 (E039==1 for years 12, 14, 16, 18)

// Child_income3: categorical
gen child_income3 = 1 if E042c==1 // <= 10,000 (E042==1 for all years)
replace child_income3 = 2 if E042c==4 // > 10,000 and <= 35,000 (E042==4 for all years)
replace child_income3 = 3 if E042c==5 & E043c==5 // >=35,000 and <= 70,000 (E042==5 & E043==5 for all years)
replace child_income3 = 4 if (E043c==1 & inlist(xyear,04,06,08,10,12,18)) | (E043c==1 & E039c==5 & inrange(xyear,14,16)) // > 70,000  and <= 100,000 (E043==1 for years 04, 06. 08, 10, 18 and E043==1 & E039==5 for years 12, 14, 16)
replace child_income3 = 5 if E039c==1 & inrange(xyear,14,16) // > 100,000 (E039==1 for years 12, 14, 16)
// Note: given the sharp difference in the median family income between 2018 and other years, I discounted $100,000 in 2018 to $70,000 in other years

// CHILD COUNT
gen son = 1 if X061_MC==3
gen daughter = 1 if X061_MC==6
gen child = 1 if son==1 | daughter==1
egen r_nson = sum(son), by(HHID PN)
egen r_ndaughter = sum(daughter), by(HHID PN)
egen r_nchild = sum(child), by(HHID PN)


// DEFINE FUNCTION TO GENERATE VARIABLES BASED ON MATCHING OPN
cap program drop gen_var_match_opn
program define gen_var_match_opn
args variable x4_diffname c resident_mc
  destring `variable'M1`c' `variable'M2`c' `variable'M3`c' `variable'M4`c' `variable'M5`c', replace
  if `x4_diffname'==1 { // variable different for x04
    destring `variable'M01`c' `variable'M02`c' `variable'M03`c' `variable'M04`c' `variable'M05`c', replace
    replace `variable'M1`c' = `variable'M01`c' if xyear==4 // variable different for x04
    replace `variable'M2`c' = `variable'M02`c' if xyear==4
    replace `variable'M3`c' = `variable'M03`c' if xyear==4
    replace `variable'M4`c' = `variable'M04`c' if xyear==4
    replace `variable'M5`c' = `variable'M05`c' if xyear==4
  }
  gen `variable'`c'_child = 0 if inrange(`variable'M1`c',0,997) | inrange(`variable'M2`c',0,997) | inrange(`variable'M3`c',0,997) | inrange(`variable'M4`c',0,997) | inrange(`variable'M5`c',0,997)
  replace `variable'`c'_child = 1 if (child==1) & (OPN==`variable'M1`c' | OPN==`variable'M2`c' | OPN==`variable'M3`c' | OPN==`variable'M4`c' | OPN==`variable'M5`c')
  replace `variable'`c'_child = 1 if (child==1) & (`variable'M1`c'==993 | `variable'M2`c'==993 | `variable'M3`c'==993 | `variable'M4`c'==993 | `variable'M5`c'==993)
    if "`variable'"=="E013" { // add resident children
      replace `variable'`c'_child = 1 if child==1 & X056_MC==1
    }
    if "`variable'"=="T004" { // add will condition - ALL CHILDREN - "EQUALLY" NOT MENTIONED
      replace `variable'`c'_child = 1 if (child==1) & (`variable'M1`c'==996 | `variable'M2`c'==996 | `variable'M3`c'==996 | `variable'M4`c'==996 | `variable'M5`c'==996)
    }
  gen `variable'`c'_son = 1 if `variable'`c'_child==1 & son==1
  gen `variable'`c'_daughter = 1 if `variable'`c'_child==1 & daughter==1
  egen `variable'`c'_r_nchild = sum(`variable'`c'_child), by(HHID PN)
  egen `variable'`c'_r_nson = sum(`variable'`c'_son), by(HHID PN)
  egen `variable'`c'_r_ndaughter = sum(`variable'`c'_daughter), by(HHID PN)
    if inlist("`variable'", "T017", "T029") { // add spouse beneficiaries
      gen `variable'`c'_r_spouse = 1 if (`variable'M1`c'==991 | `variable'M2`c'==991 | `variable'M3`c'==991 | `variable'M4`c'==991 | `variable'M5`c'==991)
    }
end

gen_var_match_opn E013 1 "c" // WHICH KID LIVE W/IN 10 MILES
gen_var_match_opn E016 1 "c" // WHICH CHILDREN OWN HOME
gen_var_match_opn N214 0 "c" // WHICH CHILD PAY HEALTH CARE COSTS
gen_var_match_opn T004 1 "c" // WHICH CHILD IS INCLUDED IN WILL
gen_var_match_opn T017 0 "c" // WHO ARE BENEFFICIARIES
gen_var_match_opn T029 0 "c" // WHO ARE BENEFICIARIES OF THESE INS
gen_var_match_opn T209 0 ""   // WHICH CHILD AUTHORITY (POWER OF ATTORNEY)




// GENERATE NEW MC-LEVEL VARIABLES
// Section PR : Preload (HHMemberChild)
gen mc_yearborn = X067_MC
gen mc_age = (xyear+2000) - mc_yearborn
gen mc_marital_status = X065_MC
gen mc_married = 0 if inrange(X065_MC,3,6)
replace mc_married = 1 if X065_MC==1
gen mc_res_status = X056_MC
gen mc_resident = 0 if inrange(X056_MC,2,6)
replace mc_resident = 1 if X056_MC==1
gen mc_nonresident = 0 if inlist(X056_MC,1,2,3,4,6)
replace mc_nonresident = 1 if X056_MC==5







// GENERATE RESPONDENT-LEVEL VARIABLES
// Section PR : Preload (Respondent)
gen r_yearborn = X067_R
gen r_age = (xyear+2000) - r_yearborn

gen r_coupleness = Z066_R
gen r_married = 0 if inrange(Z066_R,2,6)
replace r_married = 1 if Z066_R==1
gen r_partnered = 0 if inlist(Z066_R,1,2,4,6)
replace r_partnered = 1 if Z066_R==3

gen r_coupleness_c = Z066_Rc
gen r_married_c = 0 if inrange(Z066_Rc,2,6)
replace r_married_c = 1 if Z066_Rc==1
gen r_partnered_c = 0 if inlist(Z066_Rc,1,2,4,6)
replace r_partnered_c = 1 if Z066_Rc==3
gen r_home1_val_min = Z155c
gen r_home1_val_max = Z156c
gen r_home1_mort1_min = Z157c
gen r_home1_mort1_max = Z158c
gen r_home1_mort2_min = Z159c
gen r_home1_mort2_max = Z160c
gen r_home1_loan_min = Z161c
gen r_home1_loan_max = Z162c
gen r_home2_val_min = Z163c
gen r_home2_val_max = Z164c
gen r_stocks_min = Z175c
gen r_stocks_max = Z176c
gen r_checksave_min = Z179c
gen r_checksave_max = Z180c
gen r_cds_min = Z181c
gen r_cds_max = Z182c
gen r_transp_min = Z183c
gen r_transp_max = Z184c
gen r_debt_min = Z189c
gen r_debt_max = Z190c
gen r_realestate_min = Z198c
gen r_realestate_max = Z199c

// Section A	Coverscreen (Respondent)
gen r_nursinghome = 0 if A028==5 | A167==5
replace r_nursinghome = 1 if inrange(A167,1,2) & inlist(xyear,4,6,8)
replace r_nursinghome = 1 if A028==1 & inlist(xyear,10,12,14,16,18)
gen r_nchildren_resident = A099
gen r_nchildren_nonresident = A100
gen r_nchildren_nospouse = A101
gen r_nchildren_contact = A106
gen r_nchildren_childinlaw_gchild = A113

// Section B	Demographics (Respondent)
gen r_race_masked = B031M if inrange(B031M,1,7)

gen r_educ = Z216c if inrange(Z216c,0,17)
replace r_educ = B014Ac if inrange(B014Ac,0,17) & xyear==04
replace r_educ = G1074A if inrange(G1074A,0,17) & xyear==02
gen r_college = 0 if inrange(Z216c,0,12) // high school
replace r_college = 0 if inrange(B014Ac,0,12) & xyear==04
replace r_college = 1 if inrange(Z216c,13,17) // some college or higher
replace r_college = 0 if inrange(B014Ac,13,17) & xyear==04
gen r_hisp = 0 if B028Ac==5
replace r_hisp = 1 if B028Ac==1
gen r_race = B031Ac if inrange(B031Ac,1,7)
gen r_race_masked_c = B031M if inrange(B031Mc,1,7)

// Section C:	Physical Health (Respondent)
gen r_cancer = 1 if inlist(C018,1,3)
replace r_cancer = 0 if inlist(C018,4,5)
gen r_cancer_doc = 1 if C019==1
replace r_cancer_doc = 0 if C019==5
gen r_cancer_past = 1 if C020==1
replace r_cancer_past = 0 if C020==5
// C027 missing
gen r_heart_cond = 1 if inlist(C036,1,3)
replace r_heart_cond = 0 if inlist(C036,4,5)
gen r_heart_med = 1 if C037==1
replace r_heart_med = 0 if C037==5
gen r_heart_doc = 1 if C038==1
replace r_heart_doc = 0 if C038==5
gen r_heart_attack = 1 if C040==1
replace r_heart_attack = 0 if C040==5
gen r_heart_cfail = 1 if C048==1
replace r_heart_cfail = 0 if C048==5
gen r_heart_treat = 1 if C051==1
replace r_heart_treat = 0 if C051==5
gen r_heart_surgery = 1 if C052==1
replace r_heart_surgery = 0 if C052==5
gen r_stroke = 1 if inlist(C053,1,2,3) //includes "2. [VOL] POSSIBLE STROKE OR TIA (TRANSIENT ISCHEMIC ATTACK)"
replace r_stroke = 0 if inlist(C053,4,5)
// C056-59 missing
gen r_memory_disease = 1 if C069==1
replace r_memory_disease = 0 if C069==5
// C115
gen r_smoke_now = 1 if C117==1
replace r_smoke_now = 0 if C117==5
gen r_smoke_perday_cigs = C118 if inrange(C118,0,80)
gen r_smoke_perday_packs = C119 if inrange(C119,0,5)

gen r_health_rating = C001c if inrange(C001c,1,5)
gen r_cancer_c = 1 if inlist(C018c,1,3)
replace r_cancer_c = 0 if inlist(C018c,4,5)
gen r_cancer_doc_c = 1 if C019c==1
replace r_cancer_doc_c = 0 if C019c==5
gen r_cancer_past_c = 1 if C020c==1
replace r_cancer_past_c = 0 if C020c==5
// C027 missing
gen r_heart_cond_c = 1 if inlist(C036c,1,3)
replace r_heart_cond_c = 0 if inlist(C036c,4,5)
gen r_heart_med_c = 1 if C037c==1
replace r_heart_med_c = 0 if C037c==5
gen r_heart_doc_c = 1 if C038c==1
replace r_heart_doc_c = 0 if C038c==5
gen r_heart_attack_c = 1 if C040c==1
replace r_heart_attack = 0 if C040c==5
gen r_heart_attack_doc = 1 if C041c==1
replace r_heart_attack_doc = 0 if C041c==5
gen r_heart_attack_med = 1 if C042c==1
replace r_heart_attack_med = 0 if C042c==5
gen r_heart_cfail_c = 1 if C048c==1
replace r_heart_cfail_c = 0 if C048c==5
gen r_heart_cfail_hosp = 1 if C049c==1
replace r_heart_cfail_hosp = 0 if C049c==5
gen r_heart_cfail_med = 1 if C050c==1
replace r_heart_cfail_med = 0 if C050c==5
gen r_heart_treat_c = 1 if C051c==1
replace r_heart_treat_c = 0 if C051c==5
gen r_heart_surgery_c = 1 if C052c==1
replace r_heart_surgery_c = 0 if C052c==5
gen r_stroke_c = 1 if inlist(C053c,1,2,3) //includes "2. [VOL] POSSIBLE STROKE OR TIA (TRANSIENT ISCHEMIC ATTACK)"
replace r_stroke_c = 0 if inlist(C053c,4,5)
gen r_stroke_doc = 1 if C054c==1
replace r_stroke_doc = 0 if C054c==5
gen r_stroke_problems = 1 if C055c==1
replace r_stroke_problems = 0 if C055c==5
// C056-59 missing
gen r_stroke_med = 1 if C060c==1
replace r_stroke_med = 0 if C060c==5
gen r_stroke_therapy = 1 if C061c==1
gen r_psych = 1 if inlist(C065c,1,3)
replace r_psych = 0 if inlist(C065c,4,5)
gen r_psych_rating = C066c if inrange(C066c,1,3)
gen r_psych_treat = 1 if C067c==1
replace r_psych_treat = 0 if C067c==5
gen r_psych_med = 1 if C068c==1
replace r_psych_med = 0 if C068c==5
gen r_memory_disease_c = 1 if C069c==1
replace r_memory_disease_c = 0 if C069c==5
gen r_smoke_ever = 1 if C116c==1
replace r_smoke_ever = 0 if C116c==5
gen r_smoke_now_c = 1 if C117c==1
replace r_smoke_now_c = 0 if C117c==5
gen r_smoke_perday_cigs_c = C118c if inrange(C118c,0,80)
gen r_smoke_perday_packs_c = C119c if inrange(C119c,0,5)
gen r_drink_perweek = C129c if inrange(C129c,0,7)
gen r_drink_perday = C130c if inrange(C130c,0,27)
gen r_drink_binge = C131c if inrange(C131c,0,92)
gen r_excercise_vigorous = C223c if inrange(C223c,1,7)
gen r_excercise_moderate = C224c if inrange(C224c,1,7)
gen r_excercise_mild = C225c if inrange(C225c,1,7)

// Section D	Cognition (Respondent)
gen r_memory_rating = D501 if inrange(D501,1,5)
// missing D503-D504
gen r_remember_rating = D506 if inrange(D506,1,3)
gen r_convers_rating = D512 if inrange(D512,1,3)
gen r_phone_rating = D515 if inrange(D515,1,3)
gen r_date_rating = D518 if inrange(D518,1,3)
gen r_where_rating = D521 if inrange(D521,1,3)
gen r_finding_rating = D524 if inrange(D524,1,3)
gen r_machines_rating = D527 if inrange(D527,1,3)
gen r_machines_new_rating = D530 if inrange(D530,1,3)
gen r_learning_rating = D533 if inrange(D533,1,3)
gen r_story_rating = D536 if inrange(D536,1,3)
gen r_decisions_rating = D539 if inrange(D539,1,3)
gen r_shopping_rating = D542 if inrange(D542,1,3)
gen r_finances_rating = D545 if inrange(D545,1,3)
gen r_math_rating = D548 if inrange(D548,1,3)
gen r_reasoning_rating = D551 if inrange(D551,1,3)
gen r_lost = 1 if D554==1
replace r_lost = 0 if D554==5
gen r_wanderoff = 1 if D555==1
replace r_wanderoff = 0 if D555==5
gen r_left_alone = 1 if D556==1
replace r_left_alone = 0 if D556==5
gen r_halluc = 1 if D557==1
replace r_halluc = 0 if D557==5
// missing D558-563

gen r_memory_rating_c = D501c if inrange(D501c,1,5)
// missing D503-D504
gen r_remember_rating_c = D506c if inrange(D506c,1,3)
gen r_convers_rating_c = D512c if inrange(D512c,1,3)
gen r_phone_rating_c = D515c if inrange(D515c,1,3)
gen r_date_rating_c = D518c if inrange(D518c,1,3)
gen r_where_rating_c = D521c if inrange(D521c,1,3)
gen r_finding_rating_c = D524c if inrange(D524c,1,3)
gen r_machines_rating_c = D527c if inrange(D527c,1,3)
gen r_machines_new_rating_c = D530c if inrange(D530c,1,3)
gen r_learning_rating_c = D533c if inrange(D533c,1,3)
gen r_story_rating_c = D536c if inrange(D536c,1,3)
gen r_decisions_rating_c = D539c if inrange(D539c,1,3)
gen r_shopping_rating_c = D542c if inrange(D542c,1,3)
gen r_finances_rating_c = D545c if inrange(D545c,1,3)
gen r_math_rating_c = D548c if inrange(D548c,1,3)
gen r_reasoning_rating_c = D551c if inrange(D551c,1,3)
gen r_lost_c = 1 if D554c==1
replace r_lost_c = 0 if D554c==5
gen r_wanderoff_c = 1 if D555c==1
replace r_wanderoff_c = 0 if D555c==5
gen r_left_alone_c = 1 if D556c==1
replace r_left_alone_c = 0 if D556c==5
gen r_halluc_c = 1 if D557c==1
replace r_halluc_c = 0 if D557c==5
// missing D558-563


// Section J	Employment (Respondent)
gen r_salary = 1 if J031==1
replace r_salary = 0 if J031==5
gen r_salary_amt = J032 if inrange(J032,0,150000) // check
gen r_profits = 1 if J038==1
replace r_profits = 0 if J038==5
gen r_profits_amt = J039 if inrange(J039,0,350000)
gen r_pensions_num = J085 if inrange(J085,0,8)
gen r_pensions_one = J086 if inrange(J086,0,3)
gen r_pensions_type = JW001a if inrange(JW001a,1,3)

gen r_job_status = J005M1c if inrange(J005M1c,1,7)
gen r_working = 1 if J020c==1
replace r_working = 0 if J020c==5
gen r_working_for = J021c if inrange(J021c,1,2)
gen r_salary_c = 1 if J031c==1
replace r_salary_c = 0 if J031c==5
gen r_salary_amt_c = J032c if inrange(J032c,0,150000) // check
gen r_profits_c = 1 if J038c==1
replace r_profits_c = 0 if J038c==5
gen r_profits_amt_c = J039c if inrange(J039c,0,350000)
gen r_pensions_num_c = J085c if inrange(J085c,0,8)
gen r_pensions_one_c = J086c if inrange(J086c,0,3)
gen r_work_hrsperweek = J172c if inrange(J172c,0,100)
gen r_work_weeksperyear = J179c if inrange(J179c,0,52)
gen r_wage_amt = J216c if inrange(J216c,0,140)
gen r_wage_min = J217c
gen r_wage_max = J218c
replace r_wage_max = . if J218c>26 // EXCLUDES "99999996. Greater than Maximum Breakpoint"
gen r_wage_starting = J257c if inrange(J257c,0,135000)
gen r_pensions_type_c = JW001ac if inrange(JW001ac,1,3)









* GENERATE NEW MULTI-LEVEL VARIABLES ===========================================
// Section E : Family Structure (Children) (Respondent)
gen r_ngrandchildren = E046 if inrange(E046,0,80)
replace r_ngrandchildren = 0 if E046==95  // includes "95. NOT ASKED BECAUSE ASSUMED TO BE ZERO"
gen r_nggrandchildren = E048 if inrange(E048,0,80)
replace r_nggrandchildren = 0 if E048==95  // includes "95. NOT ASKED BECAUSE ASSUMED TO BE ZERO"
gen r_ggrandchildren_any = 0 if inrange(E047,5,6) // includes "ASSIGNED NO"
replace r_ggrandchildren_any = 1 if E047==1
destring E074M1, replace
destring E074M2, replace
destring E074M3, replace
gen deed_allchildren = 0 if inrange(E074M1,0,993) | inrange(E074M2,0,993) | inrange(E074M3,0,993)
replace deed_allchildren = 1 if E074M1==993 | E074M2==993 | E074M3==993
gen deed_child = 0 if inrange(E074M1,0,993) | inrange(E074M2,0,993) | inrange(E074M3,0,993)
replace deed_child = 1 if E074M1==OPN | E074M2==OPN | E074M3==OPN | deed_allchildren==1

gen r_ngrandchildren_c = E046c if inrange(E046c,0,80)
replace r_ngrandchildren_c = 0 if E046c==95  // includes "95. NOT ASKED BECAUSE ASSUMED TO BE ZERO"
gen r_nggrandchildren_c = E048c if inrange(E048c,0,80)
replace r_nggrandchildren_c = 0 if E048c==95  // includes "95. NOT ASKED BECAUSE ASSUMED TO BE ZERO"
gen r_ggrandchildren_any_c = 0 if inrange(E047c,5,6) // includes "ASSIGNED NO"
replace r_ggrandchildren_any_c = 1 if E047c==1
// destring E074M1c, replace
// destring E074M2c, replace
// destring E074M3c, replace
// gen deed_allchildren_c = 0 if inrange(E074M1c,0,993) | inrange(E074M2c,0,993) | inrange(E074M3c,0,993)
// replace deed_allchildren_c = 1 if E074M1c==993 | E074M2c==993 | E074M3c==993
// gen deed_child_c = 0 if inrange(E074M1c,0,993) | inrange(E074M2c,0,993) | inrange(E074M3c,0,993)
// replace deed_child_c = 1 if E074M1c==OPN | E074M2c==OPN | E074M3c==OPN | deed_allchildren_c==1





// Section G	Functional Limitations and Helpers (Respondent)
gen r_iadl_money_problem = 1 if G060==1
replace r_iadl_money_problem = 0 if G060==5
gen r_iadl_money_help = 1 if G061==1
replace r_iadl_money_help = 0 if inlist(G061,1,6,7)

gen moneyhelper1 = 0 if G061==5
replace moneyhelper1 = 1 if G061==1 | (G061==6 & G063_1 <= 91) | (G061==7 & G063_1 <= 91)
replace moneyhelper1 = . if (G061==6 & G063_1==.) | (G061==7 & G063_1==.)
// br xyear HHID PN OPN SUBHH mh1 mh1_rel r_mh1 r_mh1_rel G061 G062_1 G063_1 id


gen moneyhelper1_rel = G063_1 if inrange(G063_1,2,91)
gen moneyhelper2_rel = G063_2 if inrange(G063_2,2,91)
gen moneyhelper_rel = G063_1 if inrange(G063_1,2,91)
replace moneyhelper_rel = G063_2 if inrange(G063_2,2,91)


gen r_iadl_money = 1 if inlist(G059c,1,6,7) // INCLUDES "6. CAN'T DO 7. DON'T DO"
replace r_iadl_money = 0 if G059c==5
gen r_iadl_money_problem_c = 1 if G060c==1
replace r_iadl_money_problem_c = 0 if G060c==5
gen r_iadl_money_help_c = 1 if G061c==1
replace r_iadl_money_help_c = 0 if inlist(G061c,1,6,7)
destring G062_1c, replace
destring G062_2c, replace
gen moneyhelper1_c = 0 if inrange(G062_1c,0,995)
replace moneyhelper1_c = 1 if OPN==G062_1c & !mi(OPN)
gen moneyhelper2_c = 0 if inrange(G062_2c,0,995)
replace moneyhelper2_c = 1 if OPN==G062_2c & !mi(OPN)
gen moneyhelper_c = 0 if inrange(G062_1c,0,995) | inrange(G062_2c,0,995)
replace moneyhelper_c = 1 if (OPN==G062_1c | OPN==G062_2c) & !mi(OPN)
destring G063_1c, replace
destring G063_2c, replace
gen moneyhelper1_rel_c = G063_1c if inrange(G063_1c,2,91)
gen moneyhelper2_rel_c = G063_2c if inrange(G063_2c,2,91)
gen moneyhelper_rel_c = G063_1c if inrange(G063_1c,2,91)
replace moneyhelper_rel_c = G063_2c if inrange(G063_2c,2,91)


// Section Q : Assets and Income (Household)
gen hh_workforpay = 1 if Q010c==1
replace hh_workforpay = 0 if Q010c==5
gen hh_who_workforpay = Q011c if inrange(Q011c,1,3)
  label define who_label 1 "RESPONDENT ONLY" 2 "SPOUSE/PARTNER ONLY" 3 "BOTH"
  label values hh_who_workforpay who_label
gen r_workforpay = 1 if Q012c==1
replace r_workforpay = 0 if Q012c==5
gen sp_workforpay = 1 if Q013c==1
replace sp_workforpay = 0 if Q013c==5
gen r_selfemp_any = 1 if Q014c==1
replace r_selfemp_any = 0 if Q014c==5
gen r_selfemp_amt = Q015c if inrange(Q015c,0,250000)
gen r_wages_salary_any = 1 if Q019c==1
replace r_wages_salary_any = 0 if Q019c==5
gen r_wages_salary_amt = Q020c if inrange(Q020c,0,300000)
gen r_prof_prac_any = 1 if Q024c==1
replace r_prof_prac_any = 0 if Q024c==5
gen r_prof_prac_amt = Q025c if inrange(Q025c,0,200000)
gen sp_selfemp_any = 1 if Q039c==1
replace sp_selfemp_any = 0 if Q039c==5
gen sp_selfemp_amt = Q040c if inrange(Q040c,0,236000)
gen sp_wages_salary_any = 1 if Q044c==1
replace sp_wages_salary_any = 0 if Q044c==5
gen sp_wages_salary_amt = Q045c if inrange(Q045c,0,216000)
gen sp_prof_prac_any = 1 if Q049c==1
replace sp_prof_prac_any = 0 if Q049c==5
gen sp_prof_prac_amt = Q050c if inrange(Q050c,0,60000)
gen hh_unemp_inc = 1 if Q064c==1
replace hh_unemp_inc = 0 if Q064c==5
gen hh_who_unemp_inc = Q065c if inrange(Q065c,1,3)
  label values hh_who_unemp_inc who_label
gen r_unemp_inc_amt = Q066c if inrange(Q066c,0,12000)
gen sp_unemp_inc_amt = Q070c if inrange(Q070c,0,15600)
gen r_ss_inc_lmonth_amt = Q085c if inrange(Q085c,0,9503)
gen sp_ss_inc_any = 1 if Q084c==1
replace sp_ss_inc_any = 0 if Q084c==5
gen sp_ssinc_lmonth_amt = Q091c if inrange(Q091c,0,2770)
gen hh_who_ssi_inc = Q106c if inrange(Q106c,1,3)
  label values hh_who_ssi_inc who_label
gen hh_ssi_inc_amt = Q107c if inrange(Q107c,0,1330)

gen hh_welfare_notssi_inc_any = 1 if Q113c==1
replace hh_welfare_notssi_inc_any = 0 if Q113c==5
gen hh_welfare_notssi_inc_who = Q065c if inrange(Q114c,1,3)
  label values hh_welfare_notssi_inc_who who_label
gen hh_welfare_notssi_inc_amt = 1 if inrange(Q115c,0,41000)

gen hh_military_inc_any = 1 if Q119c==1
replace hh_military_inc_any = 0 if Q119c==5
gen hh_military_inc_who = Q120c if inrange(Q120c,1,3)
  label values hh_military_inc_who who_label

gen hh_real_estate_any = Q133c if inrange(Q133c,1,5)
  label define real_estate_label 1 "YES" 2 "YES, MORE THAN ONE" 3 "NO"
  label values hh_real_estate_any real_estate_label
gen hh_real_estate_amt = Q134c if inrange(Q134c,0,99999997)
gen hh_rental_inc_any = 1 if Q138c==1
replace hh_rental_inc_any = 0 if Q138c==5
gen hh_rental_inc_freq = Q139c if inrange(Q139c,1,7)
  label define freq_label 1 "WEEK" 2 "2 X MONTH" 3 "MONTH" 4 "QUARTER" 5 "6 MONTHS" 6 "YEAR" 7 "OTHER"
  label values hh_rental_inc_freq freq_label
gen hh_rental_inc_lperiod_amt = Q141c if inrange(Q141c,0,45000)
gen hh_rental_inc_same_amt = 1 if Q142c==1
replace hh_rental_inc_same_amt = 0 if Q142c==5
gen hh_rental_inc_lcy_amt = Q143c if inrange(Q143c,0,275000)

gen hh_business_any = 1 if Q147c==1
replace hh_business_any = 0 if Q147c==5
gen hh_business_amt = Q148c if inrange(Q148c,0,6500000)
gen hh_business_inc_any = 1 if Q152c==1
replace hh_business_inc_any = 0 if Q152c==5
gen hh_business_inc_freq = Q153c if inrange(Q153c,1,7)
  label values hh_business_inc_freq freq_label
gen hh_business_inc_lperiod_amt = Q155c if inrange(Q155c,0,50000)
gen hh_business_inc_same_amt = 1 if Q156c==1
replace hh_business_inc_same_amt = 0 if Q156c==5
gen hh_business_inc_lcy_amt = Q157c if inrange(Q157c,0,250000)

gen hh_ira_any = Q162c if inrange(Q162c,1,5)
  label define ira_label 1 "YES" 2 "PREVIOUSLY HAD ACCOUNTS, NOT NOW" 3 "NO"
  label values hh_ira_any ira_label
gen hh_ira_count = Q163c if inrange(Q163c,1,4)
gen hh_ira1_who = Q165_1c if inrange(Q165_1c,1,2)
  label define ira_label2 1 "R'S ACCOUNT" 2 "SPOUSE'S OR PARTNER'S ACCOUNT"
  label values hh_ira1_who ira_label2
gen hh_ira1_amt = Q166_1c if inrange(Q166_1c,0,1000000)
gen hh_ira1_invested = 1 if Q513_1c==1
replace hh_ira1_invested = 0 if Q513_1c==5
gen hh_ira1_invested_pct = Q514_1c if inrange(Q514_1c,0,100)
gen hh_ira1_invested_cutoff = Q515_1c if inrange(Q515_1c,1,5)
  label define ira_label3 1 "LESS THAN 50 PERCENT" 3 "ABOUT 50 PERCENT" 5 "MORE THAN 50 PERCENT"
  label values hh_ira1_invested_cutoff ira_label3
gen hh_ira1_withdraw_any = 1 if Q170_1c==1
replace hh_ira1_withdraw_any = 0 if Q170_1c==5
gen hh_ira1_withdraw_amt = Q171_1c if inrange(Q171_1c,0,375000)
gen hh_ira2_who = Q165_2c if inrange(Q165_2c,1,2)
  label values hh_ira2_who ira_label2
gen hh_ira2_amt = Q166_2c if inrange(Q166_2c,0,1000000)

gen hh_pension_inc_any = 1 if Q215c==1
replace hh_pension_inc_any = 0 if Q215c==5
gen hh_pension_inc_who = Q216c if inrange(Q216c,1,3)
  label values hh_pension_inc_who who_label
gen hh_pension_inc_morethanone = 1 if Q217c==1
replace hh_pension_inc_morethanone = 0 if Q217c==5
gen hh_pension_inc_amt = Q218c if inrange(Q218c,0,97)
gen hh_pension1 = 1 if Q219_1c==1
replace hh_pension1 = 0 if Q219_1c==2
gen hh_pension1_lmonth_amt = Q220_1c if inrange(Q220_1c,0,49000)
gen hh_pension1_acct_amt = Q227_1c if inrange(Q227_1c,0,560000)

gen sp_pension_lmonth_amt = Q246_1c if inrange(Q246_1c, 0, 99699)
gen sp_pension_acct_amt = Q253_1c if inrange(Q253_1c, 0, 99699)

gen hh_pension_notss_amt = Q269c if inrange(Q269c,0,99999997) // mostly missing
gen hh_annuity_inc_any = 1 if Q273c==1
replace hh_annuity_inc_any = 0 if Q273c==5
gen hh_annuity_inc_who = Q274c if inrange(Q274c,1,3)
  label values hh_annuity_inc_who who_label
gen hh_annuity_lmonth_amt = Q312c if inrange(Q312c, 0, 97) // mostly missing

gen hh_stock_any = 1 if Q316c==1
replace hh_stock_any = 0 if Q316c==5
gen hh_stock_value = Q317c if inrange(Q317c,0,5000000)
gen hh_stock_value_confirm = 1 if Q533c==1
replace hh_stock_value_confirm = 0 if Q533c==5
gen hh_stock_value_amended = Q534c if !mi(Q534c)


gen hh_stock_inc_any = 1 if Q321c==1
replace hh_stock_inc_any = 0 if Q321c==5
gen hh_stock_inc_freq = Q322c if inrange(Q322c,1,7)
  label define stock_freq_label 1 "ACCUMULATES/REINVEST" 3 "MONTH" 4 "QUARTER" 5 "6 MONTHS" 6 "YEAR" 7 "OTHER"
  label values hh_stock_inc_freq stock_freq_label
gen hh_stock_inc_lperiod_amt = Q324c if inrange(Q324c,0,50000)
gen hh_stock_inc_same_amt = 1 if Q325c==1
replace hh_stock_inc_same_amt = 0 if Q325c==5
gen hh_stock_inc_lcy_amt = Q326c if inrange(Q326c,0, 150000)

gen hh_bond_any = 1 if Q330c==1
replace hh_bond_any = 0 if Q330c==5
gen hh_bond_value = Q331c if inrange(Q331c,0,2500000)
gen hh_bond_inc_any = 1 if Q335c==1
replace hh_bond_inc_any = 0 if Q335c==5
gen hh_bond_inc_freq = Q336c if inrange(Q336c,1,7)
  label values hh_bond_inc_freq stock_freq_label

gen hh_bond_inc_lperiod_amt = Q338c if inrange(Q338c,0,50000)
gen hh_bond_inc_same_amt = 1 if Q339c==1
replace hh_bond_inc_same_amt = 0 if Q339c==5
gen hh_bond_inc_lcy_amt = Q340c if inrange(Q340c,0, 70000)

gen hh_checking_any = 1 if Q344c==1
replace hh_checking_any = 0 if Q344c==5
gen hh_checking_amt = Q345c if inrange(Q345c,0,2000000)

gen hh_foodstamps_any = 1 if Q400c==1
replace hh_foodstamps_any = 0 if Q400c==5
gen hh_foodstamps_lmonth_amt = Q410c if inrange(Q410c, 0, 500) // mostly missing
gen hh_subsidized_food_any = 1 if Q414c==1
replace hh_subsidized_food_any = 0 if Q414c==5
gen hh_enough_food = 1 if Q415c==1
replace hh_enough_food = 0 if Q415c==5
gen hh_skipped_meals = 1 if Q416c==1 | Q516c==1
replace hh_skipped_meals = 0 if Q416c==5 | Q516c==5
gen hh_food_weekly_amt = Q417c if inrange(Q417c, 0, 809)

destring Q431_1c, replace
gen hhmem1 = Q431_1c if inrange(Q431_1c,0,997)
gen hhmem1_work_lcy = 1 if Q432_1c==1
replace hhmem1_work_lcy = 0 if Q432_1c==5
gen hhmem1_earn_lcy_amt = Q433_1c if inrange(Q433_1c,0,3500000)
destring Q431_2c, replace
gen hhmem2 = Q431_2c if inrange(Q431_2c,0,997)
gen hhmem2_work_lcy = 1 if Q432_2c==1
replace hhmem2_work_lcy = 0 if Q432_2c==5
gen hhmem2_earn_lcy_amt = Q433_2c if inrange(Q433_2c,0,150000)

gen hh_trust_who1 = Q465M1c if inrange(Q465M1c,1,5)
  label define trust_label 1 "RESPONDENT" 2 "SPOUSE/PARTNER" 3 "CHILD/CHILD-IN-LAW/GRANDCHILD" 4 "OTHER RELATIVE" 5 "SOMEONE ELSE"
  label values hh_trust_who1 trust_label
gen hh_trust_who2 = Q465M2c if inrange(Q465M2c,1,5)
  label values hh_trust_who2 trust_label
gen hh_trust_who3 = Q465M3c if inrange(Q465M3c,1,5)
  label values hh_trust_who3 trust_label
gen hh_trust_who4 = Q465M4c if inrange(Q465M4c,1,5)
  label values hh_trust_who4 trust_label
gen hh_trust_who5 = Q465M5c if inrange(Q465M5c,1,5)
  label values hh_trust_who5 trust_label
gen hh_trust_r = 0 if inrange(hh_trust_who1,1,5) | inrange(hh_trust_who2,1,5) | inrange(hh_trust_who3,1,5) | inrange(hh_trust_who4,1,5) | inrange(hh_trust_who5,1,5)
replace hh_trust_r = 1 if hh_trust_who1==1 | hh_trust_who2==1 | hh_trust_who3==1 | hh_trust_who4==1 | hh_trust_who5==1
gen hh_trust_sp = 0 if inrange(hh_trust_who1,1,5) | inrange(hh_trust_who2,1,5) | inrange(hh_trust_who3,1,5) | inrange(hh_trust_who4,1,5) | inrange(hh_trust_who5,1,5)
replace hh_trust_sp = 1 if hh_trust_who1==2 | hh_trust_who2==2 | hh_trust_who3==2 | hh_trust_who4==2 | hh_trust_who5==2
gen hh_trust_child = 0 if inrange(hh_trust_who1,1,5) | inrange(hh_trust_who2,1,5) | inrange(hh_trust_who3,1,5) | inrange(hh_trust_who4,1,5) | inrange(hh_trust_who5,1,5)
replace hh_trust_child = 1 if hh_trust_who1==3 | hh_trust_who2==3 | hh_trust_who3==3 | hh_trust_who4==3 | hh_trust_who5==3
gen hh_trust_other_rel = 0 if inrange(hh_trust_who1,1,5) | inrange(hh_trust_who2,1,5) | inrange(hh_trust_who3,1,5) | inrange(hh_trust_who4,1,5) | inrange(hh_trust_who5,1,5)
replace hh_trust_other_rel = 1 if hh_trust_who1==4 | hh_trust_who2==4 | hh_trust_who3==4 | hh_trust_who4==4 | hh_trust_who5==4

destring Q466M01c Q466M02c Q466M03c Q466M04c Q466M05c Q466M06c Q466M07c Q466M08c, replace
gen hh_trust_child_benefits = 0 if inrange(Q466M01c,0,996) | inrange(Q466M02c,0,996) | inrange(Q466M03c,0,996) | inrange(Q466M04c,0,996) | inrange(Q466M05c,0,996) | inrange(Q466M06c,0,996) | inrange(Q466M07c,0,996) | inrange(Q466M08c,0,996)
replace hh_trust_child_benefits = 1 if Q466M01c==OPN | Q466M02c==OPN | Q466M03c==OPN | Q466M04c==OPN | Q466M05c==OPN | Q466M06c==OPN | Q466M07c==OPN | Q466M08c==OPN
replace hh_trust_child_benefits = 1 if inrange(Q466M01c,993,996) | inrange(Q466M02c,993,996) | inrange(Q466M03c,993,996) | inrange(Q466M04c,993,996) | inrange(Q466M05c,993,996) | inrange(Q466M06c,993,996) | inrange(Q466M07c,993,996) | inrange(Q466M08c,993,996)
gen hh_trust_child_equal_benefits = 0 if inrange(Q466M01c,0,996) | inrange(Q466M02c,0,996) | inrange(Q466M03c,0,996) | inrange(Q466M04c,0,996) | inrange(Q466M05c,0,996) | inrange(Q466M06c,0,996) | inrange(Q466M07c,0,996) | inrange(Q466M08c,0,996)
replace hh_trust_child_equal_benefits = 1 if inrange(Q466M01c,993,996) | inrange(Q466M02c,993,996) | inrange(Q466M03c,993,996) | inrange(Q466M04c,993,996) | inrange(Q466M05c,993,996) | inrange(Q466M06c,993,996) | inrange(Q466M07c,993,996) | inrange(Q466M08c,993,996)
gen hh_trust_val = Q467c if inrange(Q467c,0,999997)

gen hh_other_debts_any = 1 if Q477c==1
replace hh_other_debts_any = 0 if Q477c==5
gen hh_other_debts_amt = Q478c if inrange(Q478c,0,999997)
gen hh_other_debts_creditcard = 1 if Q517c==1
replace hh_other_debts_creditcard = 0 if Q517c==5
gen hh_creditcard_lmonth_paidoff = 1 if Q518c==1
replace hh_creditcard_lmonth_paidoff = 0 if Q518c==5
gen hh_credicard_lmonth_unpaid_amt = Q519c if inrange(Q519c,0,9999997)

gen hh_lumpsum1_any = 1 if Q483_1c==1
replace hh_lumpsum1_any = 0 if Q483_1c==5
gen hh_lumpsum1_form = Q484_1c if inrange(Q484_1c,1,7)
  label define lumpsum_label 1 "INSURANCE SETTLEMENT" 2 "PENSION SETTLEMENT" 3 "INHERITANCE (OR TRUST)" 4 "GIFT" 5 "LAWSUIT" 7 "OTHER (SPECIFY)"
  label values hh_lumpsum1_form lumpsum_label
gen hh_lumpsum1_amt = Q488_1c if inrange(Q488_1c,0,1200000)
gen hh_lumpsum1_cutoff_amt = Q489_1c if inrange(Q489_1c,1,7)
  label define lumpsum_label2 1 "LESS THAN $50,000" 2 "ABOUT $50,000" 5 "MORE THAN $50,000"
  label values hh_lumpsum1_cutoff_amt lumpsum_label2
gen hh_lumpsum1_who = Q490_1c if inrange(Q490_1c,14,34)
  label define lumpsum_label3 14 "R'S PARENTS" 15 "BROTHER" 17 "SISTER" 19 "OTHER RELATIVE" 20 "OTHER INDIVIDUAL" 27 "EX-SPOUSE/PARTNER" 34 "SP'S PARENTS"
  label values hh_lumpsum1_who lumpsum_label3

gen hh_lumpsum2_any = 1 if Q483_2c==1
replace hh_lumpsum2_any = 0 if Q483_2c==5
gen hh_lumpsum2_form = Q484_2c if inrange(Q484_2c,1,7)
  label values hh_lumpsum2_form lumpsum_label
gen hh_lumpsum2_amt = Q488_2c if inrange(Q488_2c,0,1200000)
gen hh_lumpsum2_cutoff_amt = Q489_2c if inrange(Q489_2c,1,7)
  label values hh_lumpsum2_cutoff_amt lumpsum_label2
gen hh_lumpsum2_who = Q490_2c if inrange(Q490_2c,14,34)
  label values hh_lumpsum2_who lumpsum_label3



// Section T	Wills and Life Insurance (Respondent)
gen r_will = T001c
replace r_will = . if T001c>5
gen r_will_family = 1 if T002c==1
replace r_will_family = 0 if T002c==5
gen r_will_children = 1 if T003c==1
replace r_will_children = 0 if T003c==5
destring T004M1c, replace
destring T004M2c, replace
destring T004M3c, replace
gen in_will_child = 0 if !mi(T004M1c) | !mi(T004M2c) | !mi(T004M3c)
replace in_will_child = 1 if (OPN==T004M1c | OPN==T004M2c | OPN==T004M1c | inrange(T004M1c,993,996) | inrange(T004M2c,993,996) | inrange(T004M3c,993,996))  & !mi(OPN)
// includes 996. ALL CHILDREN - "EQUALLY" NOT MENTIONED
gen r_will_equalchildren = 1 if T005c==1
replace r_will_equalchildren = 0 if T005c==5
gen r_will_gchildren = 1 if inlist(T006c,1,2) // INCLUDES "2. [VOL] ONLY THROUGH THEIR PARENTS"
replace r_will_gchildren = 0 if T006c==5
destring T007M1c, replace
destring T007M2c, replace
destring T007M3c, replace
gen in_will_gchild = 0 if !mi(T007M1c) | !mi(T007M2c) | !mi(T007M3c)
replace in_will_child = 1 if (OPN==T007M1c | OPN==T007M2c | OPN==T007M1c)  & !mi(OPN)
replace in_will_child = 1 if  T007M1c==993 | T007M2c==993 | T007M3c==993
// includes 993. ALL CHILDREN
gen r_will_charity = 1 if T008c==1
replace r_will_charity = 0 if T008c==5
gen r_lifeins = 1 if T011c==1
replace r_lifeins = 0 if T011c==5
gen r_lifeins_num = T012c if inrange(T012c,1,5)
gen r_lifeins_amt = T013c
gen r_lifeins_min = T014c
gen r_lifeins_max = T015c
replace r_lifeins_max = . if T015c>250000 // EXCLUDES "99999996. Greater than Maximum Breakpoint"
destring T017M1c, replace
destring T017M2c, replace
destring T017M3c, replace
gen in_lifeins = 0 if inrange(T017M1c,0,997) | inrange(T017M2c,0,997) | inrange(T017M3c,0,997)
replace in_lifeins = 1 if (OPN==T017M1c | OPN==T017M2c | OPN==T017M3c) & !mi(OPN)
gen r_lifeins_whole = 1 if T018c==1
replace r_lifeins_whole = 0 if T018c==5
gen r_lifeins_whole_num = T019c if inrange(T019c,1,5)
gen r_lifeins_whole_amt = T020c
gen r_lifeins_whole_min = T021c
gen r_lifeins_whole_max = T022c
replace r_lifeins_whole_max = . if T022c>250000 // EXCLUDES "99999996. Greater than Maximum Breakpoint"
gen r_lifeins_pay = T024c if inrange(T024c,0,80000)
gen r_lifeins_pay_per = T025c if inrange(T025c,1,2)
destring T029M1c, replace
destring T029M2c, replace
destring T029M3c, replace
gen in_lifeins_whole = 0 if inrange(T029M1c,0,997) | inrange(T029M2c,0,997) | inrange(T029M3c,0,997)
replace in_lifeins_whole = 1 if (OPN==T029M1c | OPN==T029M2c | OPN==T029M3c) & !mi(OPN)
gen r_lifeins_new = 1 if T031c==1
replace r_lifeins_new = 0 if T031c==5
gen r_lifeins_new_amt = T032c if inrange(T032c,0,125000)
gen r_lifeins_new_min = T033c
gen r_lifeins_new_max = T034c
replace r_lifeins_new_max = . if T034c>249999 // EXCLUDES "99999996. Greater than Maximum Breakpoint"


* ==============================================================================

* NEW/UPDATED VARIABLES 7/8/22 ==============================================================
drop r_educ rel mh1dd r_working

gen r_educ = .
replace r_educ = Z216c if Z216c <=17
replace r_educ = B014Ac if B014Ac<=17 & xyear==04

gen rel = mh1_rel
replace rel = . if mh1_rel==1

gen mh1dd = .
replace mh1dd = 1 if rel<=91
replace mh1dd = 0 if G061==5

gen r_ownshome = .
replace r_ownshome = 1 if Z079==1
replace r_ownshome = 2 if Z079==2
replace r_ownshome = 3 if Z079==3

gen r_couple = .
replace r_couple = 1 if Z066_R<=4
replace r_couple = 0 if Z066_R==6

gen r_working = .
replace r_working = 1 if Z123==1
replace r_working = 0 if Z123==5

gen r_realestate = .
replace r_realestate = 1 if Z098==1
replace r_realestate = 0 if Z098==5

gen r_retired = .
replace r_retired = 1 if Z124==1
replace r_retired = 0 if Z124==5

gen ADL_rel = G033_1

destring G032_1, replace
replace ADL_rel = . if (G032_1!=OPN & G033_1<=91) | G033_1 ==1

gen ADLd = .
replace ADLd = 1 if ADL_rel<=91 & G032_1==OPN

gen IADL_rel = G055_1

destring G054_1, replace
replace IADL_rel = . if (G054_1!=OPN & G055_1<=91) | G055_1==1

gen IADLd = .
replace IADLd = 1 if IADL_rel<=91 & G054_1==OPN

// Age
// Marital Status
// Whether child lives with R (resident)
// Number of children
// Frequency of contact
// Lives within 10 miles
// Works part time or full time
// Family income
// Owns home

// 7/12/2022
// Inflation adjusted wages (base 2000)
// local inf_vars "E081 E093"
// CPI, exit
// source: https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1913-
foreach var in E081 E093 T9002 T9012 T173 {
  gen `var'inf = .
  replace `var'inf = `var'*(172.2/179.9) if xyear==02
  replace `var'inf = `var'*(172.2/188.9) if xyear==04
  replace `var'inf = `var'*(172.2/201.6) if xyear==06
  replace `var'inf = `var'*(172.2/215.3) if xyear==08
  replace `var'inf = `var'*(172.2/218.1) if xyear==10
  replace `var'inf = `var'*(172.2/229.6) if xyear==12
  replace `var'inf = `var'*(172.2/236.7) if xyear==14
  replace `var'inf = `var'*(172.2/240.0) if xyear==16
  replace `var'inf = `var'*(172.2/251.1) if xyear==18
}

// CPI, core
gen cyear = xyear-2
foreach var in Z155c Z156c Z157c Z158c Z169c Z170c Z175c Z176c Z177c Z178c Z179c Z180c Z181c Z182c Z183c Z184c Z185c Z186c Z187c Z188c Z189c Z190c Z198c Z199c Q015c Q019c Q020c Q166_1c Q227_1c Q317c Q331c Q345c Q357c T013c T014c T015c T020c {
  gen `var'inf = .
  replace `var'inf = `var'*(172.2/179.9) if cyear==02
  replace `var'inf = `var'*(172.2/188.9) if cyear==04
  replace `var'inf = `var'*(172.2/201.6) if cyear==06
  replace `var'inf = `var'*(172.2/215.3) if cyear==08
  replace `var'inf = `var'*(172.2/218.1) if cyear==10
  replace `var'inf = `var'*(172.2/229.6) if cyear==12
  replace `var'inf = `var'*(172.2/236.7) if cyear==14
  replace `var'inf = `var'*(172.2/240.0) if cyear==16
}

// Home Price Index, core
// source: https://fred.stlouisfed.org/series/CSUSHPINSA#0
foreach var in H020c H025c H032c H166c {
  gen `var'inf = .
  replace `var'inf = `var'*(100/116.436) if cyear==02
  replace `var'inf = `var'*(100/140.705) if cyear==04
  replace `var'inf = `var'*(100/180.828) if cyear==06
  replace `var'inf = `var'*(100/171.079) if cyear==08
  replace `var'inf = `var'*(100/145.003) if cyear==10
  replace `var'inf = `var'*(100/134.167) if cyear==12
  replace `var'inf = `var'*(100/159.374) if cyear==14
  replace `var'inf = `var'*(100/175.055) if cyear==16
}

// Receive transfer dummy
destring E076, replace
gen E076d = .
replace E076d = 1 if OPN==E076 & E076<998

// R transfers to other children dummy
gen E086d = .
replace E086d = 1 if E086==1
replace E086d = 0 if E086==5

// Gave transfer dummy
destring E088, replace
gen E088d = .
replace E088d = 1 if OPN==E088 & E088<998

// R transfers from other children dummy
gen E099d = .
replace E099d = 1 if E099==1
replace E099d = 0 if E099==5

* ==============================================================================



* EXPORT DTA FILE ==============================================================
// REDUCE TO HELPERS ONLY
drop if mi(G069)

// ORGANIZE DATA
order xyear xwave HHID PN OPN SUBHH in_core*
sort xyear HHID PN OPN

save "$output/hrs_helper_v3-4_new", replace
* ==============================================================================





* END ==========================================================================
* ==============================================================================
