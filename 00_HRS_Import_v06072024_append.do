
* ==============================================================================
* HRS HELPER
*
* current version: 6/7/24
* author: Matt Braaksma
* NOTES: code written 7/09/22, updated to include 2020 exit, 2020 core, 2022 core
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
// HRS path
//   Data
//     00_HRS_Raw
//       h##core
//         h##da
//           rawfile.DA
//         h##sta
//           rawfile.DCT
//       x##exit
//         x##da
//           rawfile.DA
//         x##sta
//           rawfile.DCT
//     00_HRS_Import
//       newfile.dta

glo path="/Volumes/T7 Touch" // Matt's path

  glo scripts="$path/Files/HRS" // scripts directory
  glo data="$path/data_raw/HRS" // raw data directory
	glo sections="$path/data_projects/HRS/01 Sections" // infiled data directory
	glo annual="$path/data_projects/HRS/02 Annual" // merged annual data directory
	glo appended="$path/data_projects/HRS/03 Appended" // appended annual data directory


foreach wave in 00 02 04 06 08 10 12 14 16 18 20 22 {
  glo x`wave'_dict = "$data/x`wave'exit/x`wave'sta"
  glo x`wave'_da = "$data/x`wave'exit/x`wave'da"
  di "${x`wave'_dict}"

  glo h`wave'_dict = "$data/h`wave'core/h`wave'sta"
  glo h`wave'_da = "$data/h`wave'core/h`wave'da"
}
* ==============================================================================



* Import Exit Files ============================================================
cap program drop importexit
program define importexit
args wave exit_list

  foreach x in `exit_list' {
    infile using "${x`wave'_dict}/X`wave'`x'.dct", using("${x`wave'_da}/X`wave'`x'.da")
    save  "${sections}/X`wave'`x'.dta", replace
    clear
  }

end

importexit "20" "A_R B_R C_R D_R E_FC E_R E_TC G_HP G_R IO_R J_R N_R PR_MC PR_R T_R Y_R"
importexit "18" "A_R B_R C_R D_R E_FC E_R E_TC G_HP G_R IO_R J_R N_R PR_MC PR_R T_R Y_R"
importexit "16" "A_R B_R C_R D_R E_FC E_R E_TC G_HP G_R IO_R J_R N_R PR_MC PR_R T_R Y_R"
importexit "14" "A_R B_R C_R D_R E_FC E_R E_TC G_HP G_R IO_R J_R N_R PR_MC PR_R T_R Y_R"
importexit "12" "A_R B_R C_R D_R E_FC E_R E_TC G_HP G_R IO_R J_R N_R PR_MC PR_R T_R Y_R"
importexit "10" "A_R B_R C_R D_R E_FC E_R E_TC G_HP G_R IO_R J_R N_R PR_MC PR_R T_R Y_R"
importexit "08" "A_R B_R C_R D_R E_FC E_R E_TC G_HP G_R IO_R J_R N_R PR_MC PR_R T_R Y_R"
importexit "06" "A_R B_R C_R D_R E_FC E_R E_TC G_HP G_R IO_R J_R N_R PR_MC PR_R T_R Y_R"
importexit "04" "A_R B_R C_R D_R E_FC E_R E_TC G_HP G_R IO_R J_R N_R PR_MC PR_R T_R W_R Y_R"
importexit "02" "A_R B_R C_R D_R E_FC E_R E_TC G_HP G_R IO_R J_R N_R PR_MC PR_R T_R W_R Y_R"
* ==============================================================================



* Import Core Files ============================================================
cap program drop importcore
program define importcore
args wave core_list

  cap clear
  foreach x in `core_list' {
    infile using "${h`wave'_dict}/H`wave'`x'.dct", using("${h`wave'_da}/H`wave'`x'.da")
    save  "${sections}/H`wave'`x'.dta", replace
    clear
  }

end

importcore "22" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R J2_P J3_R LB_R M_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H S_R T_R TN_R V_R W_R Y_R"
importcore "20" "A_H A_R B_R C_R COV_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H IO_R J_R J2_P J3_R LB_R M1_R M2_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H S_R T_R TN_R U_H V_R W_R Y_R" // added COV_R
importcore "18" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R J2_P J3_R LB_R M1_R M2_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H S_R T_R TN_R U_H V_R W_R Y_R"
importcore "16" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R J2_P J3_R LB_R M1_R M2_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H S_R T_R TN_R U_H V_R W_R Y_R"
importcore "14" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_P J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H RC_R S_R T_R TN_R U_H V_R W_R Y_R"
// Remove duplicates from F_SB
  use "${sections}/H14F_SB.dta", clear
  byso HHID PN OPN: gen dup = _n
  drop if dup==2
  drop dup
  save "${sections}/H14F_SB.dta", replace
importcore "12" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_P J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H RC_R S_R T_R TN_R U_H V_R W_R Y_R"
importcore "10" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_H PR_MC PR_R PR_SB Q_H R_H S_R T_R TN_R U_H V_R W_R Y_R"
importcore "08" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_H PR_MC PR_R PR_SB Q_H R_H S_R T_R TN_R U_H V_R W_R Y_R"
importcore "06" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R K_R L_R LB_R M0_R M1_R M2_R N_R P_R PR_H PR_MC PR_R PR_SB Q_H R_H RC_R S_R T_R TN_R U_H V_R W_R Y_R"
importcore "04" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_H PR_MC PR_R PR_SB Q_H R_H RC_R S_R T_R U_H V_R W_R Y_R"
// Remove duplicates from F_SB
  use "${sections}/H04F_SB.dta", clear
  byso HHID JSUBHH OPN: gen dup = _n
  drop if dup==2
  drop dup
  save "${sections}/H04F_SB.dta", replace
importcore "02" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H IO_R J_R K_R L_R M1_R M2_R N_R P_R PR_H PR_MC PR_P PR_R PR_SB Q_H R_H RC_R S_R T_R U_A V_R W_R Y_R"
importcore "00" "A_R B_R C_R CS_H CS_MC CS_R D_R D_FC D_H D_MC D_SB D_TC E_HP E_R F_H G_R H_R J_H J_MC J_R M_R N_H N_R PC_R PR_MC PR_R PR_SB R_R T_R"
* ==============================================================================



* Merge into Annual Exit Files =================================================
cap program drop mergeexit
program define mergeexit
args wave exit_list

  use "${sections}/X`wave'A_R.dta", clear
  foreach x in `exit_list' {
    merge 1:1 HHID PN using "${sections}/X`wave'`x'.dta", nogen
  }
  gen xyear = `wave'
  save "${annual}/X`wave'_R.dta", replace

  // Only 1 exit file for HP, MC, Transfer
  use "${sections}/X`wave'G_HP.dta", clear
  gen xyear = `wave'
  save "${annual}/X`wave'_HP.dta", replace

  use "${sections}/X`wave'PR_MC.dta", clear
  gen xyear = `wave'
  save "${annual}/X`wave'_MC.dta", replace

  use "${sections}/X`wave'E_TC.dta", clear
  gen xyear = `wave'
  save "${annual}/X`wave'_TC.dta", replace

  use "${sections}/X`wave'E_FC.dta", clear
  gen xyear = `wave'
  save "${annual}/X`wave'_FC.dta", replace

end

mergeexit "20" "B_R C_R D_R E_R G_R IO_R J_R N_R PR_R T_R Y_R"
mergeexit "18" "B_R C_R D_R E_R G_R IO_R J_R N_R PR_R T_R Y_R"
mergeexit "16" "B_R C_R D_R E_R G_R IO_R J_R N_R PR_R T_R Y_R"
mergeexit "14" "B_R C_R D_R E_R G_R IO_R J_R N_R PR_R T_R Y_R"
mergeexit "12" "B_R C_R D_R E_R G_R IO_R J_R N_R PR_R T_R Y_R"
mergeexit "10" "B_R C_R D_R E_R G_R IO_R J_R N_R PR_R T_R Y_R"
mergeexit "08" "B_R C_R D_R E_R G_R IO_R J_R N_R PR_R T_R Y_R"
mergeexit "06" "B_R C_R D_R E_R G_R IO_R J_R N_R PR_R T_R Y_R"
mergeexit "04" "B_R C_R D_R E_R G_R IO_R J_R N_R PR_R T_R W_R Y_R"
mergeexit "02" "B_R C_R D_R E_R G_R IO_R J_R N_R PR_R T_R W_R Y_R"
* ==============================================================================


* Append Exit Files ============================================================
cap program drop appendexit
program define appendexit
args idvar

  use "${annual}/X02_`idvar'.dta", clear
  foreach i in 04 06 08 10 12 14 16 18 20 {
    append using "${annual}/X`i'_`idvar'.dta"
  }
  save "${appended}/X02-20_`idvar'.dta", replace

end

appendexit "R"
appendexit "MC"
appendexit "HP"
appendexit "FC"
appendexit "TC"
* ==============================================================================


* Merge into Annual Core Files =================================================
cap program drop mergecore
program define mergecore
args wave core_r core_h core_mc core_sb

    if `wave'==02 local letter "H"
    if `wave'==04 local letter "J"
    if `wave'==06 local letter "K"
    if `wave'==08 local letter "L"
    if `wave'==10 local letter "M"
    if `wave'==12 local letter "N"
    if `wave'==14 local letter "O"
    if `wave'==16 local letter "P"
	if `wave'==18 local letter "Q"

  use "${sections}/H`wave'A_R.dta", clear
  foreach x in `core_r' {
    merge 1:1 HHID PN using "${sections}/H`wave'`x'.dta", nogen force
  }
  gen cyear = `wave'
  save "${annual}/H`wave'_R.dta", replace

  use "${sections}/H`wave'A_H.dta", clear
  foreach x in `core_h' {
    merge 1:1 HHID `letter'SUBHH using "${sections}/H`wave'`x'.dta", nogen
  }
  gen cyear = `wave'
  save "${annual}/H`wave'_H.dta", replace

  use "${sections}/H`wave'E_MC.dta", clear
  foreach x in `core_mc' {
    merge 1:1 HHID `letter'SUBHH OPN using "${sections}/H`wave'`x'.dta", nogen
  }
  gen cyear = `wave'
  save "${annual}/H`wave'_MC.dta", replace

  use "${sections}/H`wave'F_SB.dta", clear
  foreach x in `core_sb' {
    if inlist(`wave', 04) {
      merge 1:1 HHID `letter'SUBHH OPN using "${sections}/H`wave'`x'.dta", nogen
    }
    else {
      merge 1:1 HHID PN OPN using "${sections}/H`wave'`x'.dta", nogen
    }
  }
  gen cyear = `wave'
  save "${annual}/H`wave'_SB.dta", replace

  // Only 1 core file for HP, TC, FC
  use "${sections}/H`wave'G_HP.dta", clear
  gen cyear = `wave'
  save "${annual}/H`wave'_HP.dta", replace

  use "${sections}/H`wave'E_TC.dta", clear
  gen cyear = `wave'
  save "${annual}/H`wave'_TC.dta", replace

  use "${sections}/H`wave'E_FC.dta", clear
  gen cyear = `wave'
  save "${annual}/H`wave'_FC.dta", replace

end


mergecore "18" "B_R C_R D_R F_R G_R I_R IO_R J_R J3_R LB_R M1_R M2_R N_R P_R PR_R S_R T_R TN_R V_R W_R Y_R" ///
               "E_H H_H IO_H PR_H Q_H R_H U_H" ///
               "PR_MC" ///
               "PR_SB"
mergecore "16" "B_R C_R D_R F_R G_R I_R IO_R J_R J3_R LB_R M1_R M2_R N_R P_R PR_R S_R T_R TN_R V_R W_R Y_R" ///
               "E_H H_H IO_H PR_H Q_H R_H U_H" ///
               "PR_MC" ///
               "PR_SB"
mergecore "14" "B_R C_R D_R F_R G_R I_R IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_R RC_R S_R T_R TN_R V_R W_R Y_R" ///
               "E_H H_H IO_H PR_H Q_H R_H U_H" ///
               "PR_MC" ///
               "PR_SB"
//check
mergecore "12" "B_R C_R D_R F_R G_R I_R IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_R RC_R S_R T_R TN_R V_R W_R Y_R" ///
               "E_H H_H IO_H PR_H Q_H R_H U_H" ///
               "PR_MC" ///
               "PR_SB"
mergecore "10" "B_R C_R D_R F_R G_R I_R IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_R S_R T_R TN_R V_R W_R Y_R" /// no RC_R
               "E_H H_H IO_H PR_H Q_H R_H U_H" ///
               "PR_MC" ///
               "PR_SB"
mergecore "08" "B_R C_R D_R F_R G_R I_R IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_R S_R T_R TN_R V_R W_R Y_R" /// no RC_R
               "E_H H_H IO_H PR_H Q_H R_H U_H" ///
               "PR_MC" ///
               "PR_SB"
mergecore "06" "B_R C_R D_R F_R G_R I_R IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_R S_R T_R TN_R V_R W_R Y_R" /// no RC_R
               "E_H H_H IO_H PR_H Q_H R_H U_H" ///
               "PR_MC" ///
               "PR_SB"
mergecore "04" "B_R C_R D_R F_R G_R I_R IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_R S_R T_R V_R W_R Y_R" /// no RC_R, TN_R
               "E_H H_H PR_H Q_H R_H U_H" /// No IO_H
               "PR_MC" ///
               "PR_SB"
mergecore "02" "B_R C_R D_R F_R G_R IO_R J_R K_R L_R M1_R M2_R N_R P_R PR_R S_R T_R V_R W_R Y_R" /// no RC_R, TN_R, I_R, LB_R
               "E_H H_H PR_H Q_H R_H" /// No IO_H, U_H
               "PR_MC" ///
               "PR_SB"

// Merge 00
local core_r "PR_R A_R B_R C_R PC_R D_R E_R G_R H_R J_R M_R N_R R_R T_R"
local core_h "D_H F_H J_H N_H"
local core_mc "PR_MC D_MC J_MC"

use "${sections}/H00CS_R.dta", clear
foreach x in `core_r' {
  merge 1:1 HHID PN using "${sections}/H00`x'.dta", nogen force
}
gen cyear = 00
save "${annual}/H00_R.dta", replace

use "${sections}/H00CS_H.dta", clear
foreach x in `core_h' {
  merge 1:1 HHID GSUBHH using "${sections}/H00`x'.dta", nogen
}
gen cyear = 00
save "${annual}/H00_H.dta", replace

use "${sections}/H00CS_MC.dta", clear
foreach x in `core_mc' {
  merge 1:1 HHID GSUBHH OPN using "${sections}/H00`x'.dta", nogen
}
gen cyear = 00
save "${annual}/H00_MC.dta", replace

use "${sections}/H00PR_SB.dta", clear
  merge 1:1 HHID GSUBHH OPN using "${sections}/H00D_SB.dta", nogen
gen cyear = 00
save "${annual}/H00_SB.dta", replace

// Only 1 core file for HP, TC, FC
use "${sections}/H00E_HP.dta", clear
gen cyear = 00
save "${annual}/H00_HP.dta", replace

use "${sections}/H00D_TC.dta", clear
gen cyear = 00
save "${annual}/H00_TC.dta", replace

use "${sections}/H00D_FC.dta", clear
gen cyear = 00
save "${annual}/H00_FC.dta", replace
* ==============================================================================


* Append Core Files ============================================================
cap program drop appendcore
program define appendcore
args idvar

  use "${annual}/H00_`idvar'.dta", clear
  foreach i in 02 04 06 08 10 12 14 16 18 {
    append using "${annual}/H`i'_`idvar'.dta"
  }
  save "${appended}/H00-18_`idvar'.dta", replace

end

// appendcore "R"
appendcore "MC"
appendcore "HP"
appendcore "FC"
appendcore "TC"
* ==============================================================================



* Combine & Rename variables ===================================================
/*
cap program drop rename_xvars
program define rename_xvars
args newvar

  gen `newvar' = S`newvar' if xyear==02
  replace `newvar' = T`newvar' if xyear==04
  replace `newvar' = U`newvar' if xyear==06
  replace `newvar' = V`newvar' if xyear==08
  replace `newvar' = W`newvar' if xyear==10
  replace `newvar' = X`newvar' if xyear==12
  replace `newvar' = Y`newvar' if xyear==14
  replace `newvar' = Z`newvar' if xyear==16

end

rename_xvars "E093"
*/
* ==============================================================================
