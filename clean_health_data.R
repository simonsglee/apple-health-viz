# Clean Apple Health Data

# load library -----------------------------------------------------------------
library(tidyverse)
library(here)
library(xml2)
library(lubridate)
library(zoo)
library(ggrepel)

# load data --------------------------------------------------------------------

if(file.exists(here("data/apple_health_export/export.xml"))) {
    health_xml <- XML::xmlParse(here("data/apple_health_export/export.xml"))
    
    df_record <- XML:::xmlAttrsToDataFrame(health_xml["//Record"], 
                                           stringsAsFactors = FALSE) %>%
        as_tibble()
    
    # clean data
    # change data types
    df_record <-
        df_record %>%
        mutate(
            type = str_remove(type, "HKQuantityTypeIdentifier"),
            type = str_remove(type, "HKCategoryTypeIdentifier"),
            creationDate = ymd_hms(creationDate, tz = "America/Toronto"),
            startDate = ymd_hms(startDate, tz = "America/Toronto"),
            endDate = ymd_hms(endDate, tz = "America/Toronto"),
            date = date(endDate),
            year = year(endDate),
            value = value
        )
} else {
    
    df_record <- read_rds(here("data/random_health_data.RDS"))
    
}

# create DataFrames needed for visualizations ----------------------------------

# WEIGHT
df_weight <-
    df_record %>%
    filter(type == "BodyMass") %>%
    mutate(value = as.numeric(value))

# VO2 
df_vo2 <-
    df_record %>% 
    filter(type == "VO2Max") %>%
    mutate(value = as.numeric(value))

# exercise
df_exercise <-
    df_record %>% 
    filter(type == "AppleExerciseTime") %>%
    mutate(value = as.numeric(value))

# active energy
df_active_energy <- 
    df_record %>% 
    filter(type == "ActiveEnergyBurned") %>%
    mutate(value = as.numeric(value))

# mindfulness
df_mindfulness <-
    df_record %>% 
    filter(type == "MindfulSession") %>% 
    mutate(value = as.numeric((endDate - startDate) / 60))


# sleep
df_sleep_wide <- 
    df_record %>%
    filter(
        type == "SleepAnalysis",
        str_detect(sourceName, "Watch")
    ) %>%
    mutate(value = str_replace(value, "HKCategoryValueSleepAnalysis", "")) %>%
    arrange(desc(endDate)) %>%
    mutate(sleep_value = as.numeric((endDate - startDate)/60)) %>%
    select(year, value, date, startDate, endDate, sleep_value) %>%
    group_by(year, date, value) %>%
    summarise(sleep_value = sum(sleep_value) / 60) %>% 
    arrange(desc(date)) %>%
    pivot_wider(names_from = value, values_from = sleep_value) %>%
    ungroup() %>%
    mutate(
        Asleep = AsleepCore + AsleepDeep + AsleepREM,
        DeepPercent = 100 * AsleepDeep / Asleep
    )

df_sleep_long <- 
    df_record %>%
    filter(
        type == "SleepAnalysis",
        str_detect(sourceName, "Watch")
    ) %>%
    mutate(value = str_replace(value, "HKCategoryValueSleepAnalysis", "")) %>%
    arrange(desc(endDate)) %>%
    mutate(sleep_value = as.numeric((endDate - startDate)/60)) %>%
    select(year, value, date, startDate, endDate, sleep_value) %>%
    group_by(year, date, value) %>%
    summarise(sleep_value = sum(sleep_value) / 60) %>% 
    arrange(desc(date))

df_sleep_bedtime <-
    df_record %>%
    filter(
        type == "SleepAnalysis",
        str_detect(sourceName, "Watch")
    ) %>%
    mutate(value = str_replace(value, "HKCategoryValueSleepAnalysis", "")) %>%
    arrange(desc(endDate)) %>%
    group_by(date) %>%
    summarise(bedtime = min(startDate)) %>%
    ungroup() %>%
    mutate(diff1 = as_date(bedtime) - date) %>%
    mutate(time_only = hms::as_hms(bedtime))

x <- df_sleep_bedtime$bedtime

date(x) <- as.Date("2000-01-01") + df_sleep_bedtime$diff1

df_sleep_bedtime$bedtime <- x

