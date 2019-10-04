

#'--- 
#' title: "LGH ED - LWBS rates for ED MHSU"
#' author: "Nayef Ahmad"
#' date: "2019-10-05"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---

#+ lib, include = FALSE
library(tidyverse)
library(denodoExtractor)
library(lubridate)
library(DT)
library(kableExtra)

setup_denodo()



#+ rest 
#' # LWBS data 
# LWBS data ----------

# lwbs codes: 
vw_eddata %>% 
  filter(facility_short_name == "LGH", 
         start_date_id >= "20140101", 
         start_date_id <= "20191001") %>% 
  select(disch_disp_lwbs_at_left_ed, 
         disch_disp_lwbs_desc_at_left_ed) %>% 
  distinct() %>% 
  collect %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                "condensed", 
                "responsive"))
              


# pull overall LWBS rates
df1.lwbs_overall <- 
  vw_eddata %>% 
  filter(facility_short_name == "LGH", 
         start_date_id >= "20140101", 
         start_date_id <= "20191001", 
         disch_disp_lwbs_at_left_ed != 0) %>% 
  select(start_date_id,
         patient_id, 
         is_admitted, 
         disch_disp_lwbs_at_left_ed, 
         start_to_left_ed_elapsed_time_minutes) %>% 
  collect 


df1.1.lwbs_overall_by_day <- 
  df1.lwbs_overall %>% 
  group_by(start_date_id) %>% 
  summarise(daily_lwbs = n()) %>% 
  
  ungroup() %>% 
  
  # fill missing dates:
  mutate(start_date = ymd(start_date_id)) %>% 
  fill_dates(start_date, 
             "2014-01-01", 
             "2019-10-01")

str(df1.1.lwbs_overall_by_day)
summary(df1.1.lwbs_overall_by_day)
