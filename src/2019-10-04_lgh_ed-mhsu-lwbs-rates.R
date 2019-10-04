

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
#' # LWBS data - overall 
#' 
#' ## Parameters
#' 
# LWBS data - overall----------
begin_date_id <- "20140101" 
end_date_id <- "20191001"

#' ## Data pull 
# > Data pull -----
# lwbs codes: 
vw_eddata %>% 
  filter(facility_short_name == "LGH", 
         start_date_id >= begin_date_id, 
         start_date_id <= end_date_id) %>% 
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
         start_date_id >= begin_date_id, 
         start_date_id <= end_date_id, 
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
             ymd(begin_date_id), 
             ymd(end_date_id)) %>% 
  
  replace_na(list(daily_lwbs = 0))


# str(df1.1.lwbs_overall_by_day)
# summary(df1.1.lwbs_overall_by_day)

df1.1.lwbs_overall_by_day %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

#' 
#' ## Plots
# > Plots ----------
df1.1.lwbs_overall_by_day %>% 
  ggplot(aes(x = dates_fill, 
             y = daily_lwbs)) + 
  geom_point(alpha = .3) + 
  geom_smooth() + 
  labs(title = "LGH ED - Daily LWBS count", 
       subtitle = sprintf("%s to %s", 
                          begin_date_id, 
                          end_date_id)) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
      




#' # LWBS data - MHSU patients 
#' 
#' ## Data pull 
#' 

# LWBS data - MHSU patients -------
df2.lwbs_mhsu <- 
  vw_eddata %>% 
  filter(facility_short_name == "LGH", 
         start_date_id >= begin_date_id, 
         start_date_id <= end_date_id, 
         disch_disp_lwbs_at_left_ed != 0) %>% 
  select(start_date_id,
         patient_id, 
         is_admitted, 
         disch_disp_lwbs_at_left_ed, 
         start_to_left_ed_elapsed_time_minutes, 
         disch_ed_dx_1_cd, 
         chief_complaint_1_system) %>% 
  collect() 

# view: 
df2.lwbs_mhsu %>%
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip',
                           buttons = c('excel', "csv")))


# > MHSU filters: -------------------
df2.1.lwbs_mhsu_filtered <- 
  df2.lwbs_mhsu %>% 
  filter(grepl("^F\\d\\d.*", disch_ed_dx_1_cd) | 
           grepl("^R4[4-9].*", disch_ed_dx_1_cd) | 
           chief_complaint_1_system %in% c("Mental Health", 
                                           "SUBSTANCE MISUSE"), 
         
         # Exclude dementia: 
         !disch_ed_dx_1_cd %in% c("F03", "F01"), 
         !grepl("^F00.*", disch_ed_dx_1_cd), 
         !grepl("^F02.*", disch_ed_dx_1_cd))

# view: 
df2.1.lwbs_mhsu_filtered %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


df2.2.lwbs_mhsu_daily <- 
  df2.1.lwbs_mhsu_filtered %>% 
  group_by(start_date_id) %>% 
  summarise(daily_mhsu_lwbs = n())

#' ## Plots
# > Plots ----

df2.3.fill_missing <- 
  df2.2.lwbs_mhsu_daily %>% 
  
  # fill missing dates:
  mutate(start_date = ymd(start_date_id)) %>% 
  fill_dates(start_date, 
             ymd(begin_date_id), 
             ymd(end_date_id)) %>% 
  
  replace_na(list(daily_mhsu_lwbs = 0))

# view: 
df2.3.fill_missing %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
  
  # plot
df2.3.fill_missing %>% 
  ggplot(aes(x = dates_fill, 
             y = daily_mhsu_lwbs)) + 
  geom_point(alpha = .3) + 
  geom_smooth() + 
  labs(title = "LGH ED - Daily LWBS count among MHSU patients", 
       subtitle = sprintf("%s to %s", 
                          begin_date_id, 
                          end_date_id)) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


  
  
  
 











#' *********************************************
#' # Appendix 
#' 
#' ## Checks 
#' 

#' Expected num rows? 
difftime(ymd(end_date_id), ymd(begin_date_id)) + 1 == nrow(df1.1.lwbs_overall_by_day)

#' Identifying MHSU properly? Copmare with Lily's results. Should have about
#' 4600 rows.

temp <- 
  vw_eddata %>% 
  filter(facility_short_name == "LGH", 
         start_date_id >= "20180401", 
         start_date_id <= "20190624") %>% 
  select(disch_ed_dx_1_cd, 
         chief_complaint_1_system) %>% 
  collect()

# nrow(temp)
         
num_row <- 
  temp %>% 
  filter(grepl("^F\\d\\d.*", disch_ed_dx_1_cd) | 
           grepl("^R4[4-9].*", disch_ed_dx_1_cd) | 
           chief_complaint_1_system %in% c("Mental Health", 
                                           "SUBSTANCE MISUSE"), 
         
         # Exclude dementia: 
         !disch_ed_dx_1_cd %in% c("F03", "F01"), 
         !grepl("^F00.*", disch_ed_dx_1_cd), 
         !grepl("^F02.*", disch_ed_dx_1_cd)
         ) %>% 
  nrow()

abs(num_row - 4600) < 50 
