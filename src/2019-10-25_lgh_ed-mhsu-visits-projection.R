

#'--- 
#' title: "LGH MHSU ED visits projection"
#' author: "Nayef Ahmad"
#' date: "2019-10-25"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#'     toc_folding: false
#' ---
#' 

#+ lib, include = FALSE
library(tidyverse)
library(denodoExtractor)
library(lubridate)
library(DT)
library(kableExtra)
library(plotly)

setup_denodo()



#+ rest 
#' ## Parameters
#' 
begin_date_id <- "20140101" 
end_date_id <- "20191001"
site_param <- "LGH"

#' # Data 
#' 

df1.visits <- 
  vw_eddata %>% 
  filter(facility_short_name == site_param, 
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
#' 

df2.mhsu_visits %>% 
  count(lha, 
        year(start_date)) %>% 
  # arrange(`year(start_date)`) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

#' ## Top 5 LHA only
#' 
#' 

df2.mhsu_visits %>% 
  
  # top 5 levels: 
  mutate(lha = fct_lump(lha, n = 5)) %>% 
  count(lha, 
        year(start_date)) %>% 
  arrange(lha, `year(start_date)`) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

#*************************************************************  
# graph daily MHSU visits by LHA: 
p <- 
  df2.mhsu_visits %>% 
  
  # top 5 levels: 
  mutate(lha = fct_lump(lha, n = 5)) %>% 
  count(lha, 
        start_date) %>% 
  
  # plot
  ggplot(aes(x = `start_date`, 
             y = n, 
             group = lha, 
             colour = lha)) + 
  # geom_point(alpha = .01) + 
  geom_smooth(se = FALSE) + 
  
  scale_color_brewer(type = "qual", 
                     palette = "Set1") + 
  
  coord_cartesian(ylim = c(0,8)) + 
  
  labs(title = sprintf("%s: Estimated average daily MHSU ED visits - top 5 LHAs", 
                       site_param), 
       y = "Number of visits", 
       x = "Start Date") +

  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")); # p
  
ggplotly(p)
  
  
  
#*************************************************************  
# graph annual MHSU visits by LHA:   
p <- 
  df2.mhsu_visits %>% 
  
  # top 5 levels: 
  mutate(lha = fct_lump(lha, n = 5)) %>% 
  count(lha, 
        year(start_date)) %>% 
  
  # plot
  ggplot(aes(x = `year(start_date)`, 
             y = n, 
             group = lha, 
             colour = lha)) + 
  geom_line() + 
  geom_point() + 
  
  scale_color_brewer(type = "qual",
                     palette = "Set1") +
  
  # coord_cartesian(ylim = c(0,10)) + 
  labs(title = sprintf("%s: Annual MHSU ED visits - top 5 LHAs", 
                       site_param)) +
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")) # p

ggplotly(p)



#' # Appendix 
#' 

# outputs 
# df2.mhsu_visits %>% 
#   count(lha, 
#         year(start_date)) %>% 
#   write_csv(here::here("results", 
#                        "dst", 
#                        "2019-10-25_lgh_mhsu-ed-visits-by-lha_historical.csv"))
#                