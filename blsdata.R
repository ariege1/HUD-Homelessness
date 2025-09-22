---
title: "BLS QCEW Data, 2009-2014-2019"
author: "ARIEGE BESSON"
date: "May 6, 2025"
---
  
  #longer description: Getting annual average employment level and total annual wage data for counties
  #necessary for our HUD study on factors that may be related to increased homelessness,
  #years 2009, 2014 and 2019. 
  
#____________________________________________________________________________________________
#setup

  #load packages 
  library(tidyverse)
  library(stringr)
  library(haven)
  library(tidycensus)
  library(tidyr)
  library(ipumsr)
  library(Hmisc)
  library(dplyr) #running dplyr after Hmisc is on purpose
  library(data.table)
  library(purrr)

  #code to clear environment
  rm(list = ls()) 
  
  #set working directory
  setwd(file.path("G:/My Drive/Lewis Center GSR/HUD Data"))
  
  #bring in data
  key <- read.csv(file.path("uncombined_key_0919.csv"))
  
#____________________________________________________________________________________________
#working up from this sample script from BLS:
  
  # ******************************************
  # qcewGetAreaData : This function takes a year, quarter, and area argument and
  # returns an array containing the associated area data. use 'a' for annual
  # averages. 
  # For all area codes and titles see:
  # http://data.bls.gov/cew/doc/titles/area/area_titles.htm
  
  qcewGetAreaData <- function(year, qtr, area) {
    url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
    url <- sub("YEAR", year, url, ignore.case=FALSE)
    url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
    url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
    read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
  }

# ********************************************

  #running the sample example:
  LAData <- qcewGetAreaData("2019", "a", "06037")

  #printing the first line example:
  LAData[1, ]

#______________________________________________________________________________________
#set up a df to be filled out in the next step 
#starting from the uncombined key, we will add columns for what we are looking for
  
  key <- key %>%
    mutate(FIPS = str_pad(FIPS, width = 5, pad = "0")) %>%
    mutate(FIPS = as.character(FIPS)) %>%
    mutate(Year = as.character(Year)) %>%
    rename("county_name" = "Name") %>%
    mutate(annual_avg_emplvl = NA,
           total_annual_wages = NA)
 
  #keep it all lowercase 
  names(key) <- tolower(names(key))
  
#______________________________________________________________________________________
#writing a master function to run through my df and get the needed numbers for each observation
  
  #first, set up a tibble framework for errors
  failures <- tibble(
    year = character(),
    area = character(),
    url = character(),
    error = character()
  )
  
  #second, define function as given by BLS, but add sub-function to get notification of failure
  qcewGetAreaData <- function(year, qtr, area) {
    url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
    url <- sub("YEAR", year, url, ignore.case = FALSE)
    url <- sub("QTR", tolower(qtr), url, ignore.case = FALSE)
    url <- sub("AREA", toupper(area), url, ignore.case = FALSE)
    
    tryCatch({
      read.csv(url, header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings = " ", skip = 0)
    }, error = function(e) {
      message(paste("Failed to fetch:", year, area))
      failures <<- bind_rows(failures, tibble(
        year = as.character(year),
        area = as.character(area),
        url = url,
        error = e$message
      ))
      return(NULL)
    })
  }
  
  #notes on tryCatch: without it, one bad county-year combo (or internet issues) would crash the 
  #entire loop. Here instead, if there's an error with one url, we get a notification printed to 
  #the console and the values are filled in as NULL so we can go back later and try another way.
  #well actually now we get a row in the tibble we set up
  
  #third, we write functions to fetch each df and then to filter and summarize
  summarize_qcew <- function(year, fips) {         
    data <- qcewGetAreaData(year, "a", fips)       #this fetches
    if (is.null(data)) return(tibble(employment_level = NA_real_, total_wages = NA_real_))
    
    filtered <- data %>% filter(own_code == 0)            #this filters
    
    tibble(                                               #and this gets the sums
      employment_level = sum(filtered$annual_avg_emplvl, na.rm = TRUE),
      total_wages = sum(filtered$total_annual_wages, na.rm = TRUE)
    )
  }
  
  #fourth, we print and look at any failures
  print(failures)
  
#_____________________________________________________________________________________________
#now we write a function to process each row and record outputs
  
  #Use mutate + map2 to apply the function row-wise
  results <- key %>%
    mutate(
      summary = map2(year, fips, summarize_qcew),
      annual_avg_emplvl = map_dbl(summary, ~ .x$employment_level),
      total_annual_wages = map_dbl(summary, ~ .x$total_wages)
    ) %>%
    select(-summary)
  
  #check results to see how they look
  view(results)
  
#___________________________________________________________________________________________
#troubleshooting: none of 2009 came out. 2014 and 2019 fine. maybe files saved differently.
#come back to this next week
  
  
