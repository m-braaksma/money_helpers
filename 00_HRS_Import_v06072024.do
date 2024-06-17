
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
  glo sections="$path/data_raw/HRS/x`wave'exit/x`wave'dta" // infiled data directory


foreach wave in 02 04 06 08 10 12 14 16 18 20 {
  glo x`wave'_dict = "$data/x`wave'exit/x`wave'sta"
  glo x`wave'_da = "$data/x`wave'exit/x`wave'da"
  glo x`wave'_dta = "$data/x`wave'exit/x`wave'dta"
  cap mkdir "${x`wave'_dta}"
  di "${x`wave'_dict}"
}
  
foreach wave in 00 02 04 06 08 10 12 14 16 18 20 22 {
  glo h`wave'_dict = "$data/h`wave'core/h`wave'sta"
  glo h`wave'_da = "$data/h`wave'core/h`wave'da"
  glo h`wave'_dta = "$data/h`wave'core/h`wave'dta"
  cap mkdir "${h`wave'_dta}"
  di "${h`wave'_dict}"
}
* ==============================================================================



* Import Exit Files ============================================================
cap program drop importexit
program define importexit
args wave exit_list

  foreach x in `exit_list' {
    infile using "${x`wave'_dict}/X`wave'`x'.dct", using("${x`wave'_da}/X`wave'`x'.da")
    save "${x`wave'_dta}/X`wave'`x'.dta", replace
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
	save "${h`wave'_dta}/H`wave'`x'.dta", replace
    clear
  }

end

importcore "22" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R J2_P J3_R LB_R M_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H S_R T_R TN_R V_R W_R Y_R"
importcore "20" "A_H A_R B_R C_R COV_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H IO_R J_R J2_P J3_R LB_R M1_R M2_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H S_R T_R TN_R U_H V_R W_R Y_R" // added COV_R
importcore "18" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R J2_P J3_R LB_R M1_R M2_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H S_R T_R TN_R U_H V_R W_R Y_R"
importcore "16" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R J2_P J3_R LB_R M1_R M2_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H S_R T_R TN_R U_H V_R W_R Y_R"
importcore "14" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_P J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H RC_R S_R T_R TN_R U_H V_R W_R Y_R"
// Remove duplicates from F_SB
  use "$data/h14core/h14dta/H14F_SB.dta", clear
  byso HHID PN OPN: gen dup = _n
  drop if dup==2
  drop dup
  save "$data/h14core/h14dta/H14F_SB.dta", replace
importcore "12" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_P J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_H PR_JB PR_MC PR_P PR_R PR_SB Q_H R_H RC_R S_R T_R TN_R U_H V_R W_R Y_R"
importcore "10" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_H PR_MC PR_R PR_SB Q_H R_H RC_R S_R T_R TN_R U_H V_R W_R Y_R"
importcore "08" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_H PR_MC PR_R PR_SB Q_H R_H RC_R S_R T_R TN_R U_H V_R W_R Y_R"
importcore "06" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_H IO_R J_R K_R L_R LB_R M0_R M1_R M2_R N_R P_R PR_H PR_MC PR_R PR_SB Q_H R_H RC_R S_R T_R TN_R U_H V_R W_R Y_R"
importcore "04" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H I_R IO_R J_R K_R L_R LB_R M1_R M2_R N_R P_R PR_H PR_MC PR_R PR_SB Q_H R_H RC_R S_R T_R U_H V_R W_R Y_R"
// Remove duplicates from F_SB
  use "$data/h04core/h04dta/H04F_SB.dta", clear
  byso HHID JSUBHH OPN: gen dup = _n
  drop if dup==2
  drop dup
  save "$data/h04core/h04dta/H04F_SB.dta", replace
importcore "02" "A_H A_R B_R C_R D_R E_FC E_H E_MC E_TC F_R F_SB G_HP G_R H_H IO_R J_R K_R L_R M1_R M2_R N_R P_R PR_H PR_MC PR_P PR_R PR_SB Q_H R_H RC_R S_R T_R U_A V_R W_R Y_R"
importcore "00" "A_R B_R C_R CS_H CS_MC CS_R D_R D_FC D_H D_MC D_SB D_TC E_HP E_R F_H G_R H_R J_H J_MC J_R M_R N_H N_R PC_R PR_MC PR_R PR_SB R_R T_R"
* ==============================================================================



