

#'--- 
#' title: "LGH MHSU ED visits projection"
#' author: "Nayef Ahmad"
#' date: "2019-10-25"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---
#' 

#+ lib, include = FALSE
library(tidyverse)
library(denodoExtractor)
library(lubridate)
library(DT)
library(kableExtra)

setup_denodo()



#+ rest 
#' ## Parameters
#' 
begin_date_id <- "20140101" 
end_date_id <- "20191001"

#' # Data 
#' 

df1.visits <- 
  vw_eddata %>% 
  filter(facility_short_name == "LGH", 
         start_date_id >= begin_date_id, 
         start_date_id <= end_date_id) %>% 
  select(start_date_id,
         patient_id, 
         is_admitted, 
         disch_disp_lwbs_at_left_ed, 
         start_to_left_ed_elapsed_time_minutes, 
         disch_ed_dx_1_cd, 
         chief_complaint_1_system, 
         age_at_start_date, 
         gender_desc_at_start_date, 
         patient_geographical_local_health_authority) %>% 
  collect()



#' ## MHSU filters
#' 
#' These are the filters being used for MHSU patients:
#'
#' * WHERE (`disch_ed_dx_1_cd` BETWEEN ('F00%') AND ('F99%') OR `disch_ed_dx_1_cd`
#' BETWEEN ('R44%') AND ('R46%') 
#' 
#' * OR `chief_complaint_1_system` in ('Mental
#' Health', 'SUBSTANCE MISUSE'))
#'
#' * and `disch_ed_dx_1_cd` not in ('F03', 'F00%', 'F01', 'F02%')  -- exclude dementia
#' 


df2.mhsu_visits <- 
  df1.visits %>% 
  filter(grepl("^F\\d\\d.*", disch_ed_dx_1_cd) | 
           grepl("^R4[4-9].*", disch_ed_dx_1_cd) | 
           chief_complaint_1_system %in% c("Mental Health", 
                                           "SUBSTANCE MISUSE"), 
         
         # Exclude dementia: 
         !disch_ed_dx_1_cd %in% c("F03", "F01"), 
         !grepl("^F00.*", disch_ed_dx_1_cd), 
         !grepl("^F02.*", disch_ed_dx_1_cd)) %>% 
  mutate(start_date = ymd(start_date_id), 
         lha = as.factor(patient_geographical_local_health_authority))

# str(df2.mhsu_visits)

# df2.mhsu_visits %>% 
#   datatable(extensions = 'Buttons',
#             options = list(dom = 'Bfrtip', 
#                            buttons = c('excel', "csv")))

#' ## Group by year and LHA 
#' 

df2.mhsu_visits %>% 
  group_by(year(start_date), 
           patient_geographical_local_health_authority) %>% 
  summarise(count = n()) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
  
  
  
  
  
  
  ggplot(aes(x = start_date, 
             y = count)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~patient_geographical_local_health_authority)
