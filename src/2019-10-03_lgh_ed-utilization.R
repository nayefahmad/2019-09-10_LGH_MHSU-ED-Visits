

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
library(plotly)

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
# > Parameters -----------
start_date_time_p <- ymd_hms("2018-07-01 00:00:00")
end_date_time_p <- ymd_hms("2019-07-01 00:00:00")  

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
  
  replace_na(list(num_start = 0, 
                             num_end = 0)) %>% 
  
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


#' Let's focus on just census at midnight: 

df4.net_changes %>% 
  filter(hour_of_day == 23) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))



#'*********************************************
#' # Plots 
# Plots -----

p <- 
  df4.net_changes %>% 
  ggplot(aes(x = timestamp, 
             y = queue_length)) + 
  geom_line() + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")) 
  

ggplotly(p)
    

#' ## Fixing the cold start problem
#'
#' Clearly, the queue length can't actually be negative.
#'
#' We might be getting negative values because we're implicitly assuming here
#' that at midnight at the start of the trial, there is no one in the ED.
#' There's no reason why that would actually be true.
#'
#' To get rid of the negative values, I'll add in a guess for how many people
#' there might be in ED at that start time.
#'
#' The guess is informed by the fact that the calculated queue values almost
#' never go below -20. So, assuming that there were 20 pts in the ED at the
#' start of the trial is a resonable guess that will eliminate nearly all
#' negative values.

# > Fixing the cold start problem -------
df5.queue_adjusted <- 
  df4.net_changes %>% 
  mutate(queue_length_adjusted = queue_length + 20)


# plot
p <- 
  df5.queue_adjusted %>% 
  ggplot(aes(x = timestamp, 
             y = queue_length_adjusted)) + 
  geom_line() + 
  geom_hline(yintercept = 0, 
             col = "blue") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")) 


ggplotly(p)



#' Okay, now let's look at summaries by hour of day. 
#' 

p <- 
  df5.queue_adjusted %>%
  ggplot(aes(x = hour_of_day, 
             y = queue_length_adjusted, 
             group = date(timestamp))) +
  geom_line(alpha = .1) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
      

ggplotly(p)

#' Seems like about 11 AM is the time when there's highest census. 
#' 
#' Okay why did I do that instead of just making a boxplot? 
#' 
#' 

p <- 
  df5.queue_adjusted %>%
  ggplot(aes(x = as.factor(hour_of_day), 
             y = queue_length_adjusted)) +  
  geom_boxplot() + 
  theme_light() +
  labs(title = "LGH - Number of patients in ED", 
       subtitle = sprintf("%s to %s", 
                          start_date_p, 
                          end_date_p)) + 
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))

p
ggplotly(p)





#'*********************************************


#'
#' \  
#' \  
#' \  
#'
#' # Appendix 
# Appendix -------
#' 
#' Is this a random walk? If so, differenced series will be white noise. 
#' 

df4.net_changes %>% 
  mutate(diff = queue_length - lag(queue_length)) %>% 
  ggplot(aes(x = timestamp, 
             y = diff)) + 
  geom_line() + 
  geom_hline(yintercept = 0, 
             col = "white") + 
  geom_smooth(col = "red") + 
  labs(title = "Time series of first difference of queue length", 
       subtitle = "If this is white noise, the original series is a random walk") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95")) 


df4.net_changes %>% 
  mutate(diff = queue_length - lag(queue_length)) %>% 
  slice(2:nrow(df4.net_changes)) %>% 
  pull(diff) %>% 
  acf(lag.max = 72, 
      main = "ACF of diff(queue_length)")
  

df4.net_changes %>% 
  mutate(diff = queue_length - lag(queue_length)) %>% 
  slice(2:nrow(df4.net_changes)) %>% 
  pull(diff) %>% 
  pacf(lag.max = 72, 
       main = "PACF of diff(queue_length)")

#' Okay scratch that hypothesis; it's periodic with 24 hour period (no surprise)
#' 
#' 
#' 
#' 