library(tidyverse)

# load library -----------------------------------------------------------------
library(tidyverse)
library(here)
library(xml2)
library(lubridate)
library(zoo)
library(ggrepel)
library(shiny)
library(bslib)
library(calendR)
library(ggthemes)

# source data cleaning script --------------------------------------------------
source(here("clean_health_data.R"))

# source data using random data ------------------------------------------------


# set date ranges for Watch data -----------------------------------------------
df_watch <-
    df_record %>%
    filter(str_detect(sourceName, "Watch")) 

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
    
    # App title ----
    titlePanel("Apple Health Viz"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(dateRangeInput("dateRange2",
                       label = paste("Date Range"),
                       start = min(df_watch$date), end = max(df_watch$date),
                       min = min(df_watch$date), max = max(df_watch$date),
                       separator = " - ", format = "dd/mm/yy",
                       startview = 'year', language = 'en', weekstart = 1
            ),
            width = 3
        )
        ,
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            plotOutput(outputId = "weightPlot"),
            br(),
            plotOutput(outputId = "vo2Plot"),
            br(),
            plotOutput(outputId = "energyPlot"),
            br(),
            plotOutput(outputId = "exercisePlot"),
            br(),
            plotOutput(outputId = "mindfulnessCalendarPlot"),
            br(),
            plotOutput(outputId = "mindfulnessPlot"),
            br(),
            plotOutput(outputId = "sleepByWeekPlot"),
            br(),
            plotOutput(outputId = "sleepTimeCalendarPlot"),
            br(),
            plotOutput(outputId = "sleepDeepPercentCalendarPlot"),
            br(),
            plotOutput(outputId = "sleepDeepAmountCalendarPlot"),
            br(),
            plotOutput(outputId = "sleepStagePlot"),
            br(),
            plotOutput(outputId = "sleepBedtimePlot"),
            width = 8
        )
    )
)

# Server -----------------------------------------------------------------------
server <- function(input, output) {
    
    output$weightPlot <- renderPlot({
        
        df_weight %>%
            filter(date >= input$dateRange2[1], date <= input$dateRange2[2]) %>%
            mutate(roll_avg = rollmean(value, k = 7, fill = NA, align = "right")) %>%
            ggplot(aes(x = date)) +
            geom_point(aes(y = value), colour = "#0072B2", alpha = 0.9) +
            geom_smooth(aes(y = roll_avg, linetype = "7-Day Rolling Average"), 
                        span = 0.2, col = "grey30", se = FALSE) +
            labs(
                title = "Weight Trend",
                x = "Date",
                y = "lbs"
            ) +
            scale_x_date(breaks = scales::breaks_pretty(10)) + 
            theme_fivethirtyeight() +
            theme(
                legend.position="bottom",
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(), 
                axis.title.x = element_text()
            ) 
        
        
    })
    
    output$vo2Plot <- renderPlot({
        
        df_vo2 %>% 
            filter(date >= input$dateRange2[1], date <= input$dateRange2[2]) %>%
            mutate(roll_avg = rollmean(value, k = 7, fill = NA, align = "right")) %>%
            ggplot(aes(x=date)) +
            geom_point(aes(y = value), colour = "#D55E00", alpha = 0.9) +
            geom_smooth(aes(y = roll_avg, linetype = "7-Day Rolling Average"), 
                        span = 0.2, col = "grey30", se = FALSE) +
            scale_x_date(breaks = scales::breaks_pretty(10)) + 
            theme_fivethirtyeight() +
            labs(
                title = "VO2Max Trend",
                x = "Date",
                y = "VO2Max"
            ) +
            theme(
                legend.position="bottom",
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(), 
                axis.title.x = element_text()
            ) 
    })
    
    output$energyPlot <- renderPlot({
        
        df_active_energy %>%
            filter(date >= input$dateRange2[1], date <= input$dateRange2[2]) %>%
            group_by(date) %>%
            summarise(
                value = sum(value)
            ) %>%
            group_by(
                month_year = lubridate::floor_date(date, 
                                                   "week",
                                                   week_start = getOption("lubridate.week.start", 1)
            )) %>%
            summarise(
                value = sum(value)
            ) %>%
            ggplot(aes(x=month_year, y= value)) +
            scale_x_date(breaks = scales::breaks_pretty(10)) + 
            geom_bar(fill = "#CC79A7", stat = "identity", alpha = 1) +
            theme_fivethirtyeight() +
            labs(
                title = "Active Calories",
                x = "Week",
                y = "Active Calories"
            ) +
            theme(
                legend.position="bottom",
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(), 
                axis.title.x = element_text()
            ) 
        
    })
    
    output$exercisePlot <- renderPlot({
        
        df_exercise %>%
            filter(date >= input$dateRange2[1], date <= input$dateRange2[2]) %>%
            group_by(date) %>%
            summarise(
                value = sum(value)
            ) %>%
            group_by(
                month_year = lubridate::floor_date(date, 
                                                   "week",
                                                   week_start = getOption("lubridate.week.start", 1)
                )) %>%
            summarise(
                value = sum(value)
            ) %>%
            ggplot(aes(x=month_year, y= value)) +
            scale_x_date(breaks = scales::breaks_pretty(10)) + 
            geom_bar(fill = "#7DCEA0", stat = "identity", alpha = 1) +
            theme_fivethirtyeight() +
            labs(
                title = "Exercise",
                x = "Week",
                y = "Minutes"
            ) +
            theme(
                legend.position="bottom",
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(), 
                axis.title.x = element_text()
            ) 
        
    })
    
    output$mindfulnessPlot <- renderPlot({
        
        # mindfulness by week
        df_mindfulness %>%
            filter(date >= input$dateRange2[1], date <= input$dateRange2[2]) %>%
            mutate(value = (endDate - startDate) / 60) %>%
            group_by(
                month_year = lubridate::floor_date(date, 
                                                   "week",
                                                   week_start = getOption("lubridate.week.start", 1)
                )) %>%
            summarise(
                value = sum(value)
            ) %>%
            ggplot(aes(month_year, value)) +
            geom_bar(fill = "#6495ED", stat = "identity", alpha = 1) +
            scale_x_date(breaks = scales::breaks_pretty(10)) + 
            theme_fivethirtyeight() +
            labs(
                title = "Mindfulness",
                x = "Week",
                y = "Minutes"
            ) +
            theme(
                legend.position="bottom",
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(), 
                axis.title.x = element_text()
            ) 
    })
    
    output$mindfulnessCalendarPlot <- renderPlot({
        daily_meds <- 
            df_mindfulness %>%
            group_by(date = lubridate::floor_date(date, "day")) %>%
            summarise(
                value = as.integer(sum(value) > 0)
            )
        
        dates <- tibble(date = seq.Date(from=as.Date(input$dateRange2[1]), 
                                        to=as.Date(input$dateRange2[2]), by="day"))
        
        med_data <- 
            dates %>% 
            left_join(daily_meds) %>%
            mutate(value = replace_na(value, 0))
        
        calendR(start_date = input$dateRange2[1],
                end_date = input$dateRange2[2],
                start = "M",
                special.days = med_data$value,
                gradient = TRUE,
                low.col = "#FFFFFF",
                special.col = "#6495ED",
                title = "Meditation Tracker") 
    })
    
    output$sleepByWeekPlot <- renderPlot({
        df_sleep_long %>%
            group_by(
                month_year = lubridate::floor_date(date, 
                                                   "week",
                                                   week_start = getOption("lubridate.week.start", 1)),
                value) %>%
            summarise(
                sleep_value = sum(sleep_value),
                count = n_distinct(date)
            ) %>%
            mutate(avg_value = sleep_value / count) %>%
            ggplot(aes(month_year, avg_value)) +
            geom_bar(aes(fill = value), stat = "identity") +
            scale_x_date(breaks = scales::breaks_pretty(10)) + 
            facet_wrap(~value, scales = "free_y") +
            theme_fivethirtyeight() +
            labs(
                title = "Sleep by Week",
                x = "Week",
                y = "Avg Hours"
            ) + 
            theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(), 
                axis.title.x = element_text(),
                legend.position = "none"
            ) 
        
    })
    
    output$sleepTimeCalendarPlot <- renderPlot({
        
        df_sleep <- 
            df_sleep_wide %>%
            filter(date >= input$dateRange2[1], date <= input$dateRange2[2])
        
        dates <- tibble(date = seq.Date(from=as.Date(input$dateRange2[1]), 
                                        to=as.Date(input$dateRange2[2]), by="day"))
        
        sleep_data <- 
            dates %>%
            left_join(filter(df_sleep)) %>%
            mutate(AsleepCore = replace_na(AsleepCore, 0),
                   AsleepDeep = replace_na(AsleepDeep, 0),
                   AsleepREM = replace_na(AsleepREM, 0),
                   Awake = replace_na(Awake, 0),
                   InBed = replace_na(InBed, 0),
                   Asleep = replace_na(Asleep, 0),
                   DeepPercent = replace_na(DeepPercent, 0)
            ) 
        
        data_fills <- rep(NA, length(dates$date))
        
        data_fills[sleep_data$Asleep < 6] <- "< 6"
        data_fills[sleep_data$Asleep >= 6 & sleep_data$Asleep < 7] <- "6-7"
        data_fills[sleep_data$Asleep >= 7 & sleep_data$Asleep < 8] <- "7-8"
        data_fills[sleep_data$Asleep >= 8] <- "8+"
        data_fills[sleep_data$Asleep == 0] <- "NA"
        
        # Calendar
        calendR(start_date = input$dateRange2[1],
                end_date = input$dateRange2[2],
                start = "M",
                special.days = data_fills,
                special.col = c("#D6EAF8", "#85C1E9", "#3498DB", "#2874A6", "#F5B7B1"),
                legend.pos = "bottom",
                title = "Sleep Hours Tracker")
        
    })
    
    output$sleepDeepPercentCalendarPlot <- renderPlot({

        df_sleep <- 
            df_sleep_wide %>%
            filter(date >= input$dateRange2[1], date <= input$dateRange2[2])
        
        dates <- tibble(date = seq.Date(from=as.Date(input$dateRange2[1]), 
                                        to=as.Date(input$dateRange2[2]), by="day"))
        
        sleep_data <- 
            dates %>%
            left_join(filter(df_sleep)) %>%
            mutate(AsleepCore = replace_na(AsleepCore, 0),
                   AsleepDeep = replace_na(AsleepDeep, 0),
                   AsleepREM = replace_na(AsleepREM, 0),
                   Awake = replace_na(Awake, 0),
                   InBed = replace_na(InBed, 0),
                   Asleep = replace_na(Asleep, 0),
                   DeepPercent = replace_na(DeepPercent, 0)
            ) 
        
        data_fills <- rep(NA, length(dates$date))
        
        data_fills[sleep_data$DeepPercent < 5] <- "A: 0%-5%"
        data_fills[sleep_data$DeepPercent >= 5 & sleep_data$DeepPercent < 10] <- "B: 5%-10%"
        data_fills[sleep_data$DeepPercent >= 10 & sleep_data$DeepPercent < 15] <- "C: 10%-15%"
        data_fills[sleep_data$DeepPercent >= 15 & sleep_data$DeepPercent < 20] <- "D: 15%-20%"
        data_fills[sleep_data$DeepPercent >= 20] <- "E: 20%+"
        data_fills[sleep_data$DeepPercent == 0] <- "NA"
        
        # Calendar
        calendR(start_date = input$dateRange2[1],
                end_date = input$dateRange2[2],
                start = "M",
                special.days = data_fills,
                special.col = c("#E8F8F5", "#A3E4D7", "#48C9B0", "#17A589", "#117864", "#F5B7B1"),
                legend.pos = "bottom",
                title = "Deep Sleep % Tracker") 
    })
    
    output$sleepDeepAmountCalendarPlot <- renderPlot({
        
        df_sleep <- 
            df_sleep_wide %>%
            filter(date >= input$dateRange2[1], date <= input$dateRange2[2])
        
        dates <- tibble(date = seq.Date(from=as.Date(input$dateRange2[1]), 
                                        to=as.Date(input$dateRange2[2]), by="day"))
        
        sleep_data <- 
            dates %>%
            left_join(filter(df_sleep)) %>%
            mutate(AsleepCore = replace_na(AsleepCore, 0),
                   AsleepDeep = replace_na(AsleepDeep, 0),
                   AsleepREM = replace_na(AsleepREM, 0),
                   Awake = replace_na(Awake, 0),
                   InBed = replace_na(InBed, 0),
                   Asleep = replace_na(Asleep, 0),
                   DeepPercent = replace_na(DeepPercent, 0)
            ) 
        
        data_fills <- rep(NA, length(dates$date))
        
        data_fills[sleep_data$AsleepDeep < 0.25] <- "A: <15"
        data_fills[sleep_data$AsleepDeep >= 0.25 & sleep_data$AsleepDeep < 0.5] <- "B: 15-30"
        data_fills[sleep_data$AsleepDeep >= 0.5 & sleep_data$AsleepDeep < 0.75] <- "C: 30-45"
        data_fills[sleep_data$AsleepDeep >= 0.75 & sleep_data$AsleepDeep < 1] <- "D: 45-60"
        data_fills[sleep_data$AsleepDeep >= 1] <- "E: 60+"
        data_fills[sleep_data$AsleepDeep == 0] <- "NA"
        
        # Calendar
        calendR(start_date = input$dateRange2[1],
                end_date = input$dateRange2[2],
                start = "M",
                special.days = data_fills,
                special.col = c("#E8F8F5", "#A3E4D7", "#48C9B0", "#17A589", "#117864", "#F5B7B1"),
                legend.pos = "bottom",
                title = "Deep Sleep Minutes Tracker") 
    })
    
    output$sleepStagePlot <- renderPlot({
        
        df_sleep_wide %>%
            filter(date >= input$dateRange2[1], date <= input$dateRange2[2]) %>%
            mutate(
                Core = AsleepCore / Asleep,
                REM = AsleepREM / Asleep,
                Deep = AsleepDeep / Asleep
            ) %>%
            pivot_longer(cols = Core:Deep, 
                         names_to = "SleepType",
                         values_to = "Percent") %>%
            ggplot(aes(date, Percent)) +
            geom_line(aes(group = SleepType, color = SleepType)) +
            scale_y_continuous(labels = scales::percent) +
            scale_x_date(breaks = scales::breaks_pretty(10)) + 
            theme_fivethirtyeight() +
            labs(
                title = "Sleep Stage Breakdown",
                x = "Date",
                y = "Percent"
            ) +
            scale_y_continuous(labels = scales::percent) +
            theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(), 
                axis.title.x = element_text()
            ) 
    })
    
    output$sleepBedtimePlot <- renderPlot({
        
        df_sleep_bedtime %>%
            mutate(roll_avg = as_datetime(rollmean(bedtime, k = 7, fill = NA, align = "right"), tz = "EST")) %>%
            ggplot(aes(date, y = as_datetime(bedtime, tz = "EST"))) + 
            geom_point() +
            geom_smooth(aes(y = roll_avg, linetype = "7-Day Rolling Average"), 
                        span = 0.2, col = "#D98880", se = FALSE) +
            scale_x_date(breaks = scales::breaks_pretty(20)) +
            scale_y_datetime(breaks = scales::breaks_pretty(10)) +
            geom_hline(
                aes(yintercept=as_datetime("2000-01-01 00:00:00", tz = "EST"),
                    linetype = "Midnight"),
                color="red"
            ) +
            theme_fivethirtyeight() +
            labs(
                title = "Bedtime Trend",
                x = "Date",
                y = "Time"
            ) +
            theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(), 
                axis.title.x = element_text()
            ) 

    })
    
}

shinyApp(ui, server)

