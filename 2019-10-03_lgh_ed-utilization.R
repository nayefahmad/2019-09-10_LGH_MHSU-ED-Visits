

#'--- 
#' title: "LGH ED utilization"
#' author: "Nayef Ahmad"
#' date: "2019-10-03"
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

setup_denodo()


#+ rest  
#' # Outline 
#' 
#' 1. Set up a "skeleton" with timestamps for every hour of day for 1 year
#' 
#' 2. Pull all ED start times and group by hour 
#' 
#' 3. Pull all ED end times and group by hour 
#' 
#' 4. Join the skeleton against the start and end times. We now have a queue 
#' that can keep track of number of patients in ED at any point in time. 
#' 
#' 
#' # Skeleton data frame 
#' 
#' ## Parameters
#' 

# Skeleton data frame -------
start_date_time_p <- ymd_hms("2018-01-01 00:00:00")
end_date_time_p <- ymd_hms("2018-01-10 00:00:00")  

start_date_p <- ymd(start_date_time_p)
end_date_p <- ymd(end_date_time_p)

start_date_id_p <- gsub("-", "", start_date_p)
end_date_id_p <- gsub("-", "", end_date_p)

#' 
#' ## Sequence of timestamps 
df1.skeleton <- 
  data.frame(timestamp = seq(start_date_time_p, 
                             end_date_time_p, 
                             by = "1 hour"))
# str(df1.skeleton)
# summary(df1.skeleton)

df1.skeleton %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

#+ denodo 
#' **************************************************
#' # ED Start times 
#' 

# ED Start times -----
df2.0.start_times <- 
  vw_eddata %>% 
  filter(facility_short_name == "LGH", 
         start_date_id >= start_date_id_p, 
         start_date_id <= end_date_id_p) %>% 
  select(patient_id, 
         start_date_id, 
         start_dt_tm) %>% 
  arrange(start_dt_tm) %>% 
  collect()

# str(df2.0.start_times)

df2.start_times_grouped <- 
  df2.0.start_times %>% 
  mutate(start_hour_floor = floor_date(start_dt_tm, 
                                       "hour")) %>% 
  count(start_hour_floor, 
        name = "num_start")

str(df2.start_times_grouped)

#' **************************************************
#' # ED End times 
#' 

# ED End times----------
df3.0.end_times <- 
  vw_eddata %>% 
  filter(facility_short_name == "LGH", 
         disch_date_id >= start_date_id_p, 
         disch_date_id <= end_date_id_p) %>% 
  select(patient_id, 
         disch_date_id, 
         disch_dt_tm) %>% 
  arrange(disch_dt_tm) %>% 
  collect()

# str(df3.0.end_times)

df3.end_times_grouped <- 
  df3.0.end_times %>% 
  mutate(end_hour_floor = floor_date(disch_dt_tm,
                                     "hour")) %>% 
  
  count(end_hour_floor, 
        name = "num_end")

str(df3.end_times_grouped)

#+ join 
#' **************************************************
#' # Join back to the skeleton  
#' 
# Join back to the skeleton--------------

df4.net_changes <- 
  df1.skeleton %>% 
  left_join(df2.start_times_grouped, 
            by = c("timestamp" = "start_hour_floor")) %>% 
  left_join(df3.end_times_grouped, 
            by = c("timestamp" = "end_hour_floor")) %>% 
  
  replace_na(replace_na(list(num_start = 0, 
                             num_end = 0))) %>% 
  
  mutate(net_change = num_start - num_end, 
         queue_length = cumsum(net_change), 
         hour_of_day = hour(timestamp)) %>% 
  
  select(timestamp, 
         hour_of_day, 
         everything())

# str(df4.net_changes)
# summary(df4.net_changes)

# sum(df4.net_changes$net_change)

# view: 
df4.net_changes %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

