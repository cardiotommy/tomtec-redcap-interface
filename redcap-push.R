redcap_data <- redcap_read(batch_size = 100L,
                    interbatch_delay = 0.5,
                    continue_on_error = FALSE, 
                    redcap_uri = uri, token = token, verbose = FALSE, 
                    config_options = NULL)$data

echo_data <- redcap_data %>% 
  filter(redcap_repeat_instrument == "tte_data", tte_data_complete == 2) %>% 
  mutate(echo_instance = as.factor(redcap_repeat_instance)) %>% 
  select(study_id, echo_instance, starts_with("tte_")) %>% 
  mutate(pre_post = case_when(tte_seq <= 1 ~ "pre", tte_seq == 2 ~ "post"))


pre_strain_data <- echo_data %>% 
  filter(pre_post == "pre", !is.na(tte_lvglsaverage)) %>% 
  group_by(study_id) %>% 
  slice_max(order_by = tte_date, n = 1) %>% 
  ungroup() %>% 
  select(study_id, tte_lvglsaverage) %>% 
  rename(pre_gls = tte_lvglsaverage)


#this df includes whichever of the post-op ttes that has gls
post_strain_data <- echo_data %>% 
  filter(pre_post == "post", !is.na(tte_lvglsaverage)) %>% 
  group_by(study_id) %>% 
  slice_min(order_by = tte_date, n = 1) %>% 
  ungroup() %>% 
  select(study_id, tte_lvglsaverage) %>% 
  rename(post_gls = tte_lvglsaverage)


paired_strain_data <- pre_strain_data %>% 
  left_join(post_strain_data, by = "study_id") 

#write to redcap
pre_strain_data %>% 
  select(study_id) %>% 
  mutate(pre_tte_status = 4) %>% 
  redcap_write(
    interbatch_delay = 0.5,
    continue_on_error = FALSE,
    redcap_uri = uri,
    token = token,
    overwrite_with_blanks = FALSE,
    convert_logical_to_integer = FALSE,
    verbose = TRUE,
    config_options = NULL)

post_strain_data %>% 
  select(study_id) %>% 
  mutate(post_tte_status = 4) %>% 
  redcap_write(
    interbatch_delay = 0.5,
    continue_on_error = FALSE,
    redcap_uri = uri,
    token = token,
    overwrite_with_blanks = FALSE,
    convert_logical_to_integer = FALSE,
    verbose = TRUE,
    config_options = NULL)
