---
title: "PUMS 4th try; fixing previous errors n bringing it all together"
author: "ARIEGE BESSON"
date: "April 29, 2025"
---
  
#longer description: In this script I'm going to go over everything I did in the last 2, correcting and
#bringing together all 6 PUMS measures I can't get through social explorer. Notably, I need to fix my
#household-level variables to account for just one person per household for rent and income variables.
#for reference see https://forum.ipums.org/t/matching-acs-median-household-income/2099
#note that I still haven't accounted for missing geograhies (created a crosswalk) here.
  
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

#code to clear environment
rm(list = ls()) 

#set working directory
#use my local file directory for correct permission settings w pums data
#not the ususal one: setwd(file.path("G:/My Drive/Lewis Center GSR/HUD Data"))
setwd(file.path("C:/Users/Ariege/OneDrive/Desktop/PUMS data"))

#bring in data (PUMS: no rename, all in 1 folder, don't get spicy)
ddi <- read_ipums_ddi("usa_00004.xml")
data <- read_ipums_micro(ddi)

#______________________________________________________________________________________________
#quick look
View(data)
names(data)
str(data)

#______________________________________________________________________________________________
#geography
#cut df to geographies of interest

# step 1: get FIPS codes in data formatted like I been using
data <- data %>%
  mutate(
    FIPS = str_pad(STATEFIP, width = 2, pad = "0") %>%
      paste0(str_pad(COUNTYFIP, width = 3, pad = "0"))
  )

#step 2: bring in the keys for counties of interest
countykey <- read.csv(file.path("uncombined_key_0919.csv"))
countykey <- countykey %>% mutate(FIPS = str_pad(FIPS, width = 5, pad = "0"))

multkey <- read.csv(file.path("huds_multkey.csv"))
multkey <- multkey %>% mutate(FIPS = str_pad(FIPS, width = 5, pad = "0"))

#step 3: cut down data to counties of interest
data2 <- data %>% filter(FIPS %in% countykey$FIPS)  #all counties of interest
data3 <- data %>% filter(FIPS %in% multkey$FIPS)    #only counties in multiple county groupings

#_____________________________________________________________________________________________
#set up for grouping
#add in the join_key that lets us group by county groups and join to all other dfs for this project
#essentially unique ID for the observations I need (when combined with year)

#step 1: make sure FIPS is of same type in both dfs to prevent issues with the join/ dplr if_else
multkey <- multkey %>%
  mutate(FIPS = str_pad(FIPS, width = 5, pad = "0")) %>%
  mutate(FIPS = as.character(FIPS))

data <- data %>%
  mutate(FIPS = as.character(FIPS))

#step 2: left join key using FIPS
data <- left_join(data, multkey,
                  by = "FIPS",
                  relationship = "many-to-many")

#step 3: create the join_key column
data <- data %>%
  mutate(join_key = if_else(is.na(County.Names), FIPS, County.Names))

#step 4: re-run data2 and data3 to have the column as well
data2 <- data %>% filter(FIPS %in% countykey$FIPS)
data3 <- data %>% filter(FIPS %in% multkey$FIPS) 

#__________________________________________________________________________________________
#investigate what's missing

#for this round: we have 414 county/year observations in countykey but below i see 120 :(
n_distinct(data2$FIPS)

#I really just need data for multiple county/grouped counties, of which we have 37. In data: 29 :(
n_distinct(data3$FIPS)

#to see what exactly is missing:
#missing rows (county is missing all years of data)
missing_rows <- anti_join(multkey, data3, by = "FIPS")
#missing years (county is missing 1 or more years of data)
missing_years <- data2 %>%     
  distinct(FIPS, YEAR) %>%          
  count(FIPS, name = "years_available") %>%
  filter(years_available < 3)       

#lol we wanna make a single table so. do it.
missing_rows <- missing_rows %>% mutate(years_available = 0) %>% select(-County.Names)
missingdata <- bind_rows(missing_rows, missing_years)
missingdata <- missingdata %>% left_join(countykey %>% select(FIPS, Name), by = "FIPS") %>% 
  distinct() %>% arrange(years_available)

#save to share with pi
write.csv(missingdata, file = "missingdata_pums0919.csv", row.names = FALSE)

#come back to this later: can use a crosswalk to get PUMA approximations of counties
#come back to this later: if needed: cut partial mult county sums out of final df.

#_________________________________________________________________________________________
#measures needed

# household level rent variables
# #1 median gross rent 
# #2 lower quartile contract rent
# #3 median rent for buildings built in the 1980s
#household level income variables
# #4 median household income
# #5 median household income, owners
# #6 median household income, renters
#person level income variables
# #7 median income 

#__________________________________________________________________________________________
#check values of important variables
# ok so... for each variable, what are the values to cut?
#use data dictionary but also checks here

#mgr: variable is RENTGRS. exclude 0
#mgr 80s: median of RENTGRS (same as above) and filter for BUILTYR2 = 6 (1980-1989)
#contract rent: variable is RENT. exclude  0, 1, 9999
#hh income: exclude 0, 9999999

#methods for checking: try with all applicable variables
#table: print may only print lowest values but can see frequency of 0
table(data2$RENT)

#use count() and filter() to try to find topcodes
data2 %>% count(RENT) %>% filter(RENT > 5000)

#histogram with top and bottom values labelled
#first: we set up to label smallest n largest values
top5 <- data2 %>%
  arrange(desc(RENT)) %>%
  slice(1:5)
bottom5 <- data2 %>%
  arrange(RENT) %>%
  slice(1:5)
#now we plot
ggplot(data2, aes(x = RENT)) +
  geom_histogram(bins = 100, fill = "lightblue", color = "black") +
  geom_text(data = top5, aes(x = RENT, y = 0, label = RENT),
            vjust = -1, color = "red", size = 3) +
  geom_text(data = bottom5, aes(x = RENT, y = 0, label = RENT),
            vjust = 1.5, color = "blue", size = 3) +
  labs(title = "Distribution with Top 5 and Bottom 5 Values Labeled",
       x = "Value", y = "Count") +
  theme_minimal()

#____________________________________________________________________________________
#set up for inflation adjustment

#here is the function with my years of interest, using cpi values to adjust to 2022$
#https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1&year1=200701&year2=202208
adjust_for_inflation <- function(value, year) {
  multiplier <- case_when(
    year == 2009 ~ 1.33,
    year == 2014 ~ 1.20,
    year == 2019 ~ 1.12,
    TRUE ~ NA_real_
  )
  
  return(value * multiplier)
}

#_________________________________________________________________________________________
#create measures
#remember to filter for where PERNUM == 1 to get 1 observation per household for hh level variables

#hh income variables first
hhinc <- data2 %>%
  mutate(
    HHINCOME = adjust_for_inflation(HHINCOME, YEAR)) %>%
  filter(!(HHINCOME %in% c(0, 9999999)),
         OWNERSHP != 0,
         PERNUM == 1) %>%
  group_by(join_key, YEAR) %>%
  summarise(
    overall_hhinc = wtd.quantile(HHINCOME, weights = HHWT, probs = 0.5, na.rm = TRUE),
    owners_hhinc = wtd.quantile(HHINCOME[OWNERSHP ==1], weights = HHWT[OWNERSHP ==1], probs = 0.5, 
                          na.rm = TRUE),
    renters_hhinc = wtd.quantile(HHINCOME[OWNERSHP ==2], weights = HHWT[OWNERSHP ==2], probs = 0.5, 
                          na.rm = TRUE)) %>%
  mutate(across(
    c(overall_hhinc, owners_hhinc, renters_hhinc),
    ~ round(.x, 0)
  ))

#hh rent variables second
rentvars <- data2 %>%
  mutate(
    RENTGRS = adjust_for_inflation(RENTGRS, YEAR),
    RENT = adjust_for_inflation(RENT, YEAR)) %>%
  filter(
    RENTGRS != 0,
    !(RENT %in% c(0, 1, 9999)),
    PERNUM == 1                   #whether or not you filter for renters (OWNERSHIP==2) result is same
  ) %>%
  group_by(join_key, YEAR) %>%
  summarise(
    median_gross_rent = wtd.quantile(RENTGRS, weights = HHWT, probs = 0.5, na.rm = TRUE),
    mgr_80s = wtd.quantile(RENTGRS[BUILTYR2 == 6], weights = HHWT[BUILTYR2 == 6], probs = 0.5, 
                           na.rm = TRUE),
    lowerq_contractrent = wtd.quantile(RENT, weights = HHWT, probs = 0.25, na.rm = TRUE),) %>%
  mutate(across(
    c(median_gross_rent, mgr_80s, lowerq_contractrent),
    ~ round(.x, 0)
  ))

#person level income third
incomevars <- data2 %>%
  mutate(
    INCTOT = adjust_for_inflation(INCTOT, YEAR)) %>%
  filter(
    !(INCTOT %in% c(0, 1, 9999999,9999998))) %>%
  group_by(join_key, YEAR) %>%
  summarise(
    medianincome = round(wtd.quantile(INCTOT, weights =PERWT, probs = 0.5, na.rm = TRUE), 0)
  )

#note: all the hh vars could be done together in one command, but the person level couldn't
#because there we want all values of PERNUM included

#note: why is 2014 income so much lower than 2009 income in many cases? recovery from recession so bad?
#check: look at national level. And here, 2009 < 2014 < 2019 so all I think is good
data %>%
  filter(
    !(INCTOT %in% c(0, 1, 9999999,9999998))
  ) %>%
  group_by(YEAR) %>%
  summarise(
    mean_income = weighted.mean(INCTOT, weights = HHWT, na.rm = TRUE),
    median_income = wtd.quantile(INCTOT, weights = HHWT, probs = 0.5, na.rm = TRUE)
  )

#note: later if calculating many weighted quantiles, can set up helper function like this:
#wq <- function(x, weights, probs) {
#  round(wtd.quantile(x, weights = weights, probs = probs, na.rm = TRUE), 0)}

#___________________________________________________________________________________________
#some quick checks on what I did
#did accounting for PERNUM get my county household median income approximations
#to be closer to Social Explorer/ACS products?
#how similar are the single county medians calculated here to the medians given on Social Explorer?
#let's find out
#this is why I used data2, to calculate not only mult county groupings but indiv counties to compare

#bring in my census data for single counties
socexplorer <- read.csv(file.path("Censussingle_3yrs_medium.csv"))

#ugh pad Geo_FIPS without messing with the ABQ and NYC city codes
#this leaves alone codes longer than 5 digits as well
socexplorer <- socexplorer %>%
  mutate(Geo_FIPS = as.character(Geo_FIPS)) %>%
  mutate(Geo_FIPS = if_else(
    nchar(Geo_FIPS) < 5,
    str_pad(Geo_FIPS, width = 5, pad = "0"),
    Geo_FIPS))

#forget about mapping for now. Honestly-- just inner_join to mash it into one big df for plots
comparison_df <- pums_vars %>%
  inner_join(socexplorer, by = c("YEAR" = "Year", "join_key" = "Geo_FIPS"))

#but what even to do: manually calculate percent difference? make a scatter plot?
#or a heat map for pattern-spotting over time/geography?
#y'all we <3 a scatter plot

#overall
comparisonplot <- ggplot(comparison_df, aes(x = SE_A14015_001, y = overall_hhinc)) +
  geom_point(alpha = 0.6, color = "forestgreen") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  #facet_wrap(~ YEAR) +  # <--- this is the faceting line, can opt in or out of faceting by year
  labs(
    title = "Overall Median Household Income: PUMS vs Social Explorer",
    x = "Social Explorer (SE_A14015_001)",
    y = "PUMS (overall_hhinc)"
  ) +
  theme_minimal()
print(comparisonplot)

#ok much better! save and send to pi
ggsave("hhinccomparison2.png", plot = comparisonplot, width = 10, height = 6, dpi = 300)

#___________________________________________________________________________________________
#clean up and combine
#combine all tables with PUMS medians into one. rename columns as needed

#left join based on join_key
pums_vars <- hhinc %>% 
  left_join(rentvars, by = c("join_key", "YEAR")) %>%
  left_join(incomevars, by = c("join_key", "YEAR"))

#____________________________________________________________________________________________
#save 
#save the file of combined PUMS variables 
write.csv(pums_vars, file = "PUMS_variables_0919.csv", row.names = FALSE)

#____________________________________________________________________________________________
#last notes: can't use the above data for the project unless we first cut out/NA numbers for 
#multiple/group counties where we're missing parts. Or get data using a crosswalk.