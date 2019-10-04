

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

start_date <- ymd_hms("2018-01-01 00:00:00")
end_date <- ymd_hms("2018-01-03 00:00:00")  

df1.skeleton <- 
  data.frame(timestamp = seq(start_date, 
                             end_date, 
                             by = "1 hour"))

df1.skeleton %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


#' 
#' # ED Start times 
#' 

df2.start_times <- 
  ed_data %>% 
  filter()