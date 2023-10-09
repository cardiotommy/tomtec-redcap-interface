library(vroom)
library(tidyverse)  
library(janitor)

combine_worksheets <- function(path) {
  setwd(path)
  files <- list.files(pattern = "*.txt", full.names = FALSE) 
  compiled <- vroom(files, 
                  skip = 6, 
                  na = c("NULL", "NA"), 
                  delim = ",", 
                  id = "id", 
                  col_select = c("id", "FindingId", "ValueToUse", "Label"), 
                  col_types = NULL)
  compiled <- compiled %>% 
  pivot_wider(names_from = FindingId, values_from = ValueToUse, id_cols = id) %>% 
  separate(id, into = c("study_id", "tte_date"), sep = "-", convert = TRUE, extra = "drop") %>% 
  select(-(starts_with(c("us.ped", "us.ca.p", "us.ca.stress", "us.ob", "us.ca.gv")))) %>% 
  mutate(tte_date = dmy(tte_date)) %>% 
  clean_names()
  names(compiled) = gsub(pattern = "us_ca_", replacement = "", x = names(compiled))
  compiled <- compiled %>% 
    group_by(study_id) %>% 
    arrange(tte_date, .by_group = TRUE) %>% 
    mutate(redcap_repeat_instance = row_number()) %>%
    rename(tte_ivs = lv_ivsd_bmode,
           tte_lvedd = lv_minaxd_bmode,
           tte_pw = lv_pwd_bmode,
           tte_lvesd = lv_minaxs_bmode,
           tte_lvedv = lv_edv_bp_bmode,
           tte_lvesv = lv_esv_bp_bmode,
           tte_lvefbp = lv_ef_bp_bmode,
           tte_aosv = ao_diam_sv_bmode,
           tte_aostj = ao_diam_stj_bmode,
           tte_lvot = lvot_diam_bmode,
           tte_sv = lvot_sv_dp_vol_flow,
           tte_hr = av_hr,
           tte_lavol = la_voles_bp_bmode,
           tte_ravol = ra_voles_sp_bmode_a4c,
           tte_lvotvti = lvot_vti_antflow_doppler,
           tte_lvotvmax = lvot_vmax_antflow_doppler,
           tte_avvti = av_vti_antflow_doppler,
           tte_avvmax = av_vmax_antflow_doppler,
           tte_avvmean = av_vmean_antflow_doppler,
           tte_avpgmax = av_pgmax_antflow_doppler,
           tte_avpgmean = av_pgmean_antflow_doppler,
           tte_avet = av_et_doppler,
           tte_avavti = av_ava_cont_vti_antflow_doppler,
           tte_mvewave = mv_vmax_e_antflow_doppler,
           tte_mvawave = mv_vmax_a_antflow_doppler,
           tte_lateraleprime = lvdti_ea_dti_lateral,
           tte_tvvmax = tv_vmax_regflow_doppler,
           tte_pasp = tv_pgmax_regflow_doppler,
           tte_tapse = rv_tapse_mmode,
           tte_lvglsa4c = lv_gpls_endo_strain_tp_a4c,
           tte_lvglsa2c = lv_gpls_endo_strain_tp_a2c,
           tte_lvglsa3c = lv_gpls_endo_strain_tp_a3c,
           tte_lvglsaverage = lv_gpls_endo_strain_tp,
           tte_lasred = la_lasred_strain_tp_a4c,
           tte_lascded = la_lascded_strain_tp_a4c,
           tte_lascted = la_lascted_strain_tp_a4c,
           tte_rvglsfreewall = rv_rvfwsl_strain_tp_a4c,
           tte_rvglsa4c = rv_rv4csl_strain_tp_a4c,
           tte_rvsprime = rvdti_sa_dti_lateral) %>% 
    select(study_id, tte_date, redcap_repeat_instance, tte_ivs, tte_lvedd, 
           tte_pw, tte_lvesd, tte_lvedv,
           tte_lvesv,tte_lvefbp,tte_aosv,tte_aostj,tte_lvot,tte_sv,tte_hr,
           tte_lavol,tte_ravol,tte_lvotvti,tte_lvotvmax,tte_avvti,tte_avvmax,
           tte_avvmean,tte_avpgmax,tte_avpgmean,tte_avet,tte_avavti,tte_mvewave,
           tte_mvawave,tte_lateraleprime,tte_tvvmax,tte_pasp,tte_tapse,
           tte_lvglsa4c,tte_lvglsa2c,tte_lvglsa3c,tte_lvglsaverage,tte_lasred,
           tte_lascded,tte_lascted,tte_rvglsfreewall,tte_rvglsa4c, tte_rvsprime) %>% 
    mutate(redcap_repeat_instrument = "tte_data", tomtec1 = study_id, 
           tomtec_csv = "1", tte_data_complete = "2",
           tte_lateraleprime = (tte_lateraleprime/100))
}

worksheets <- combine_worksheets(path = choose.dir())

