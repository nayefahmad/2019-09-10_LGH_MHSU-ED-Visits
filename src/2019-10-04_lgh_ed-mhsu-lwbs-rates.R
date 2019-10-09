

#'--- 
#' title: "LGH ED - LWBS rates and LOS for MHSU patients"
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
#' # LWBS data - all patients 
#' 
#' ## Parameters
#' 
# LWBS data - all pts ----------
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
#' First just pull all pts with `disch_disp_lwbs_at_left_ed` != 0. 

# LWBS data - MHSU patients only -------
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
# df2.lwbs_mhsu %>%
#   datatable(extensions = 'Buttons',
#             options = list(dom = 'Bfrtip',
#                            buttons = c('excel', "csv")))

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

#' **No real evidence that num LWBS is increasing for this population. **
#' 
  
  
#' *********************************************
#' # EDLOS for MHSU LWBS pts 
# EDLOS for MHSU LWBS pts ----------

df3.ed_los <- 
  df2.1.lwbs_mhsu_filtered %>% 
  
  mutate(start_date = ymd(start_date_id)) %>% 
  fill_dates(start_date, 
             ymd(begin_date_id), 
             ymd(end_date_id)) 

 
# View 
df3.ed_los %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

#' Note that this table is not grouped by day. 
#' 

#' ## Avg ED LOS by year

df3.ed_los %>% 
  group_by(year(dates_fill)) %>% 
  summarize(avg_ed_los_min = mean(start_to_left_ed_elapsed_time_minutes, 
                              na.rm = TRUE), 
            num_pts = n()) %>% 
  
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(2, 2)


#'
#' ## Plots 
# > Plots ----

df3.ed_los %>% 
  ggplot(aes(x = dates_fill, 
             y = start_to_left_ed_elapsed_time_minutes)) + 
  geom_point(alpha = .3) + 
  geom_smooth() + 
  labs(title = "LGH ED - LOS in minutes for MHSU LWBS patients", 
       subtitle = sprintf("%s to %s", 
                          begin_date_id, 
                          end_date_id)) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
      

#' **No real evidence that ED LOS is increasing for this population. Might
#' actually be decreasing.**


df3.ed_los %>% 
  ggplot(aes(x = start_to_left_ed_elapsed_time_minutes)) + 
  geom_density() + 
  facet_wrap(~year(dates_fill)) + 
  labs(title = "LGH ED - Distribution of LOS in minutes for MHSU LWBS patients", 
       subtitle = sprintf("%s to %s", 
                          begin_date_id, 
                          end_date_id)) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
      

#' Interesting that from 2014 to 2019, it looks like there's a shift from a
#' single dominant distribution to a mixture of at least 2 distributions.
#' 
#' Todo: Wonder if we can split those distributions using CTAS? 
#' 
#' 


#' *********************************************
#' # EDLOS - admits vs non-admits 
# EDLOS - admits vs non-admits -----

df4.ed_los_admits_and_non_admits <- 
  vw_eddata %>% 
  filter(facility_short_name == "LGH", 
         start_date_id >= begin_date_id, 
         start_date_id <= end_date_id) %>% 
  select(start_date_id,
         patient_id, 
         is_admitted, 
         disch_disp_lwbs_at_left_ed, 
         start_to_left_ed_elapsed_time_minutes, 
         start_to_first_seen_by_care_provider_elapsed_time_minutes, 
         start_to_admit_elapsed_time_minutes, 
         disch_ed_dx_1_cd, 
         chief_complaint_1_system) %>% 
  collect() 

# now filter for MHSU: 
df4.ed_los_admits_and_non_admits <- 
  df4.ed_los_admits_and_non_admits %>% 
  filter(grepl("^F\\d\\d.*", disch_ed_dx_1_cd) | 
           grepl("^R4[4-9].*", disch_ed_dx_1_cd) | 
           chief_complaint_1_system %in% c("Mental Health", 
                                           "SUBSTANCE MISUSE"), 
         
         # Exclude dementia: 
         !disch_ed_dx_1_cd %in% c("F03", "F01"), 
         !grepl("^F00.*", disch_ed_dx_1_cd), 
         !grepl("^F02.*", disch_ed_dx_1_cd)) %>% 
  mutate(is_mhsu_filtered = 1)


# view 
df4.ed_los_admits_and_non_admits %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


#' # Summarize ED LOS for MHSU pts 
df4.1.summary <- 
  df4.ed_los_admits_and_non_admits %>% 
  group_by(year(ymd(start_date_id)), 
           is_admitted) %>% 
  summarise(avg_ed_los = mean(start_to_left_ed_elapsed_time_minutes, 
                              na.rm = TRUE)) %>% 
  spread(key = is_admitted, 
         value = avg_ed_los) %>% 
  mutate(population = "mhsu_patients") %>% 
  rename(nonadmit = `0`, 
         admit = `1`, 
         year = `year(ymd(start_date_id))`) # %>% 

df4.1.summary %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


#' For comparison, here are the values for the patient population as a whole: 
#' 

df5.all_pts_ed_los <- 
  vw_eddata %>% 
  filter(facility_short_name == "LGH", 
         start_date_id >= begin_date_id, 
         start_date_id <= end_date_id) %>% 
  select(start_date_id,
         patient_id, 
         is_admitted, 
         disch_disp_lwbs_at_left_ed, 
         start_to_left_ed_elapsed_time_minutes, 
         start_to_first_seen_by_care_provider_elapsed_time_minutes, 
         start_to_admit_elapsed_time_minutes, 
         disch_ed_dx_1_cd, 
         chief_complaint_1_system) %>% 
  collect() %>% 
  
  group_by(year(ymd(start_date_id)), 
           is_admitted) %>% 
  summarise(avg_ed_los = mean(start_to_left_ed_elapsed_time_minutes, 
                              na.rm = TRUE)) %>% 
  spread(key = is_admitted, 
         value = avg_ed_los) %>% 
  mutate(population = "all_patients") %>% 
  rename(nonadmit = `0`, 
         admit = `1`, 
         year = `year(ymd(start_date_id))`) # %>% 
  
# view:   
df5.all_pts_ed_los %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

  



#' ## Plots
# > Plots ----

df4.1.summary %>% 
  bind_rows(df5.all_pts_ed_los) %>% 
  gather(key = is_admit, 
         value = minutes, 
         -c(year, population)) %>% 
  
  ggplot(aes(x = year, 
             y = minutes, 
             group = population, 
             colour = population)) +
  geom_line() + 
  facet_wrap(~is_admit) + 
  labs(title = "LGH ED - Average ED LOS by year", 
       subtitle = sprintf("%s to %s", 
                          begin_date_id, 
                          end_date_id)) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
      
#'
#' ## Notes:
#'
#' What happened in 2017? Why did EDLOS for admits drop so much? As usual,
#' probably CST go-live prep
#'
#' Among admits, MHSU pts spend less time in ED than other patients.
#'
#' Opposite is true for non-admits.
#' 



#' *********************************************
#' # Appendix 
#' 
#' ## Checks 
#' 
# Appendix -----

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
