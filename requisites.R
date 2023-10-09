library(tidyverse)
library(readxl)
library(vroom)
library(janitor)
library(REDCapR)
uri <- "https://redcap.unsw.edu.au/api/"
token <- "9CE8EE19432714288DB4F92F2E1C5D60"
id_token <- "16C7BC58EE069F6BFAFCF5FC7A559E24"
redcap_data <- redcap_read(batch_size = 100L,
                    interbatch_delay = 0.5,
                    continue_on_error = FALSE, 
                    redcap_uri = uri, token = token, verbose = FALSE, 
                    config_options = NULL)$data
