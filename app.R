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
library(shinyWidgets)

# source data cleaning script --------------------------------------------------
source(here("clean_health_data.R"))

# source data using random data ------------------------------------------------


# set date ranges for Watch data -----------------------------------------------
df_watch <-
    df_record %>%
    filter(str_detect(sourceName, "Watch")) 

# UI ---------------------------------------------------------------------------
ui <- 
    navbarPage("Apple Health Viz",
               tabPanel("Weight",
                        # sidebar panel
                        sidebarPanel(
                            dateRangeInput(
                                "dateRange2Weight",
                                label = paste("Date Range"),
                                start = min(df_watch$date), 
                                end = max(df_watch$date),
                                min = min(df_watch$date), 
                                max = max(df_watch$date),
                                separator = " - ", format = "dd/mm/yy",
                                startview = 'year', language = 'en', 
                                weekstart = 1),
                            style = "position:fixed;width:22%;",
                            width = 3),
                        # main panel
                        mainPanel(
                                plotOutput(outputId = "weightPlot"),
                                width = 8
                                )
                        ),
               tabPanel("Activity",
                        # sidebar panel
                        sidebarPanel(
                            dateRangeInput(
                                "dateRange2Activity",
                                label = paste("Date Range"),
                                start = min(df_watch$date), 
                                end = max(df_watch$date),
                                min = min(df_watch$date), 
                                max = max(df_watch$date),
                                separator = " - ", format = "dd/mm/yy",
                                startview = 'year', language = 'en', 
                                weekstart = 1),
                            
                            sliderTextInput(
                                inputId = "exerciseNumber",
                                choices = seq(1, length(unique(df_workout$workoutActivityType)), 1), 
                                selected = 3, 
                                label = "# of Exercises to Show",
                                grid = TRUE),
                            
                            style = "position:fixed;width:22%;",
                            width = 3),
                        # main panel
                        mainPanel(
                            plotOutput(outputId = "vo2Plot"),
                            br(),
                            plotOutput(outputId = "energyPlot"),
                            br(),
                            plotOutput(outputId = "exercisePlot"),
                            br(),
                            plotOutput(outputId = "exerciseDurationPlot"),
                            br(),
                            width = 8
                            )
                        ),
               tabPanel("Mindfulness",
                        # sidebar panel
                        sidebarPanel(
                            dateRangeInput(
                                "dateRange2Mindfulness",
                                label = paste("Date Range"),
                                start = min(df_watch$date), 
                                end = max(df_watch$date),
                                min = min(df_watch$date), 
                                max = max(df_watch$date),
                                separator = " - ", format = "dd/mm/yy",
                                startview = 'year', language = 'en', 
                                weekstart = 1),
                            style = "position:fixed;width:22%;",
                            width = 3),
                        # main panel
                        mainPanel(
                            plotOutput(outputId = "mindfulnessCalendarPlot"),
                            br(),
                            plotOutput(outputId = "mindfulnessPlot"),
                            br(),
                            width = 8
                            )
                        ),
               tabPanel("Sleep",
                        # sidebar panel
                        # sidebar panel
                        sidebarPanel(
                            dateRangeInput(
                                "dateRange2Sleep",
                                label = paste("Date Range"),
                                start = min(df_watch$date), 
                                end = max(df_watch$date),
                                min = min(df_watch$date), 
                                max = max(df_watch$date),
                                separator = " - ", format = "dd/mm/yy",
                                startview = 'year', language = 'en', 
                                weekstart = 1),
                            sliderTextInput(
                                inputId = "sleepGoal",
                                choices = seq(6, 9, 0.5), 
                                selected = 7, 
                                label = "Sleep Goal (Hours)",
                                grid = TRUE
                            ),
                            style = "position:fixed;width:22%;",
                            width = 3),
                        # main panel
                        mainPanel(
                            plotOutput(outputId = "sleepByWeekPlot"),
                            br(),
                            plotOutput(outputId = "sleepTimeCalendarPlot"),
                            br(),
                            plotOutput(outputId = "sleepTimeGoalCalendarPlot"),
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
               ),
               )
    
# Server -----------------------------------------------------------------------
server <- function(input, output) {
    
    output$weightPlot <- renderPlot({
        
        validate(
            need(input$dateRange2Weight[1] <= input$dateRange2Weight[2],
                 "Double check date range to create Weight Plot"), 
            need(length(df_weight$date[df_weight$date >= input$dateRange2Weight[1] & 
                                           df_weight$date <= input$dateRange2Weight[2]]) >= 7, 
                 "Need at least 7 data points to create Weight Plot")
        )

        df_weight %>%
            filter(date >= input$dateRange2Weight[1], date <= input$dateRange2Weight[2]) %>%
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
        
        validate(
            need(input$dateRange2Activity[1] <= input$dateRange2Activity[2],
                 "Double check date range to create VO2Max Plot"), 
            need(length(df_vo2$date[df_vo2$date >= input$dateRange2Activity[1] & 
                                           df_vo2$date <= input$dateRange2Activity[2]]) >= 7, 
                 "Need at least 7 data points to create VO2Max Plot")
        )
        
        df_vo2 %>% 
            filter(date >= input$dateRange2Activity[1], date <= input$dateRange2Activity[2]) %>%
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
        validate(
            need(input$dateRange2Activity[1] <= input$dateRange2Activity[2],
                 "Double check date range to create Active Calories Plot")
            )
        df_active_energy %>%
            filter(date >= input$dateRange2Activity[1], date <= input$dateRange2Activity[2]) %>%
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
                title = "Active Calories by Week",
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
        validate(
            need(input$dateRange2Activity[1] <= input$dateRange2Activity[2],
                 "Double check date range to create Exercise Plot")
        )
        
        df_exercise %>%
            filter(date >= input$dateRange2Activity[1], 
                   date <= input$dateRange2Activity[2]) %>%
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
                title = "Exercise by Week",
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
    
    output$exerciseDurationPlot <- renderPlot({
        validate(
            need(input$dateRange2Activity[1] <= input$dateRange2Activity[2],
                 "Double check date range to create Exercise Plot")
        )
        
        # find top n workouts
        df_top_n_workouts <-
            df_workout %>%
            group_by(workoutActivityType) %>%
            summarise(
                duration = sum(duration)
            ) %>%
            arrange(desc(duration)) %>%
            top_n(input$exerciseNumber)
        
        # replace non top n with Other category
        df_workout <-
            df_workout %>%
            mutate(workoutType = ifelse(workoutActivityType %in% df_top_n_workouts$workoutActivityType, 
                                        workoutActivityType, 
                                        "Other"))
        # plot workout duration by type
        df_workout %>% 
            filter(date >= input$dateRange2Activity[1], 
                   date <= input$dateRange2Activity[2]) %>%
            group_by(week = floor_date(date, 
                                       "week",
                                       week_start = getOption("lubridate.week.start", 1)),
                     workoutType) %>%
            summarise(
                duration = sum(duration)
            ) %>%
            ggplot(aes(week, duration, fill = workoutType)) +
            scale_x_date(breaks = scales::breaks_pretty(10)) + 
            geom_bar(stat = "identity", 
                     position = "stack",
                     aes(group = workoutType)) +
            theme_fivethirtyeight() +
            labs(
                title = "Exercise Type by Week",
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
        validate(
            need(input$dateRange2Mindfulness[1] <= input$dateRange2Mindfulness[2],
                 "Double check date range to create Mindfulness by Week Plot")
        )
        
        # mindfulness by week
        df_mindfulness %>%
            filter(date >= input$dateRange2Mindfulness[1], 
                   date <= input$dateRange2Mindfulness[2]) %>%
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
                title = "Mindfulness by Week",
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
        
        validate(
            need(input$dateRange2Mindfulness[1] <= input$dateRange2Mindfulness[2],
                 "Double check date range to create Mindfulness by Week Plot")
        )
        
        daily_meds <- 
            df_mindfulness %>%
            group_by(date = lubridate::floor_date(date, "day")) %>%
            summarise(
                value = n_distinct(date)
            )
        
        # edit last 12 months to avoid one year limit on calendR function
        end_date <- input$dateRange2Mindfulness[2]
        start_date <- max(input$dateRange2Mindfulness[1] , end_date - 366)
        
        month(start_date) <- month(start_date) + 1
        start_date <- floor_date(start_date, unit = "month")
        
        dates <- tibble(date = seq.Date(from = start_date, 
                                        to = end_date, by="day"))
        
        med_data <- 
            dates %>% 
            left_join(daily_meds, by = "date") %>%
            mutate(value = replace_na(value, 0))
        
        calendR(from = start_date,
                to = end_date,
                start = "M",
                special.days = med_data$value,
                gradient = TRUE,
                low.col = "#FFFFFF",
                special.col = "#6495ED",
                title = "Meditation Calendar") 
    })
    
    output$sleepByWeekPlot <- renderPlot({
        validate(
            need(input$dateRange2Sleep[1] <= input$dateRange2Sleep[2],
                 "Double check date range to create Sleep by Week Plot")
        )
        
        df_sleep_long %>%
            filter(
                date >= input$dateRange2Sleep[1],
                date <= input$dateRange2Sleep[2]
            ) %>%
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
        validate(
            need(input$dateRange2Sleep[1] <= input$dateRange2Sleep[2],
                 "Double check date range to create Sleep Calendar Plot")
        )
        
        # edit last 12 months to avoid one year limit on calendR function
        end_date <- input$dateRange2Sleep[2]
        start_date <- max(input$dateRange2Sleep[1] , end_date - 366)
        
        month(start_date) <- month(start_date) + 1
        start_date <- floor_date(start_date, unit = "month")
        
        dates <- tibble(date = seq.Date(from = start_date, 
                                        to = end_date, by="day"))
        
        df_sleep <- 
            df_sleep_wide %>%
            filter(date >= start_date, date <= end_date)
        
        sleep_data <- 
            dates %>%
            left_join(df_sleep, by = "date") %>%
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
        
        # category and filters for when there is no data points
        data_category <- c("< 6", "6-7", "7-8", "8+", "NA")
        
        data_filter <- c()
        
        for(c in data_category){
            if (c %in% data_fills) {
                data_filter <- append(data_filter, TRUE)
            } else {
                data_filter <- append(data_filter, FALSE)
            }
        }
        
        fill_colours <- c("#D6EAF8", "#85C1E9", "#3498DB", "#2874A6", "#F5B7B1")
        
        # validate(
        #     need(length(unique(data_fills)) == length(fill_colours),
        #          "Not enough data to create Sleep Calendar. Try selecting longer range of dates")
        # )
        
        # Calendar
        calendR(from = start_date,
                to = end_date,
                start = "M",
                special.days = data_fills,
                special.col = fill_colours[data_filter],
                legend.pos = "bottom",
                title = "Sleep Hours Calendar")
        
    })
    
    output$sleepTimeGoalCalendarPlot <- renderPlot({
        validate(
            need(input$dateRange2Sleep[1] <= input$dateRange2Sleep[2],
                 "Double check date range to create Sleep Calendar Plot")
        )
        
        # edit last 12 months to avoid one year limit on calendR function
        end_date <- input$dateRange2Sleep[2]
        start_date <- max(input$dateRange2Sleep[1] , end_date - 366)
        
        month(start_date) <- month(start_date) + 1
        start_date <- floor_date(start_date, unit = "month")
        
        dates <- tibble(date = seq.Date(from = start_date, 
                                        to = end_date, by="day"))
        
        df_sleep <- 
            df_sleep_wide %>%
            filter(date >= start_date, date <= end_date)
        
        sleep_data <- 
            dates %>%
            left_join(df_sleep, by = "date") %>%
            mutate(AsleepCore = replace_na(AsleepCore, 0),
                   AsleepDeep = replace_na(AsleepDeep, 0),
                   AsleepREM = replace_na(AsleepREM, 0),
                   Awake = replace_na(Awake, 0),
                   InBed = replace_na(InBed, 0),
                   Asleep = replace_na(Asleep, 0),
                   DeepPercent = replace_na(DeepPercent, 0)
            ) 
        
        
        data_fills <- rep(NA, length(dates$date))
        
        data_fills[sleep_data$Asleep >= input$sleepGoal] <- "1"
        data_fills[sleep_data$Asleep < input$sleepGoal] <- "0"
        
        fill_colours <- c("#FFFFFF", "#2874A6")
        
        validate(
            need(length(unique(data_fills)) == length(fill_colours),
                 "Not enough data to create Sleep Calendar. Try selecting longer range of dates")
        )
        
        
        calendR(from = start_date,
                to = end_date,
                start = "M",
                special.days = data_fills,
                special.col = fill_colours,
                title = "Sleep Goal Calendar") 
        
    })
    
    output$sleepDeepPercentCalendarPlot <- renderPlot({
        
        validate(
            need(input$dateRange2Sleep[1] <= input$dateRange2Sleep[2],
                 "Double check date range to create Deep Sleep Calendar Plot")
        )
        
        # edit last 12 months to avoid one year limit on calendR function
        end_date <- input$dateRange2Sleep[2]
        start_date <- max(input$dateRange2Sleep[1] , end_date - 366)
        
        month(start_date) <- month(start_date) + 1
        start_date <- floor_date(start_date, unit = "month")
        
        dates <- tibble(date = seq.Date(from = start_date, 
                                        to = end_date, by="day"))
        
        df_sleep <- 
            df_sleep_wide %>%
            filter(date >= start_date, date <= end_date)
        
        sleep_data <- 
            dates %>%
            left_join(df_sleep, by = "date") %>%
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
        
        # category and filters for when there is no data points
        data_category <- c("A: 0%-5%", "B: 5%-10%", "C: 10%-15%", "D: 15%-20%",
                           "E: 20%+", "NA")
        
        data_filter <- c()
        
        for(c in data_category){
            if (c %in% data_fills) {
                data_filter <- append(data_filter, TRUE)
            } else {
                data_filter <- append(data_filter, FALSE)
            }
        }
        
        fill_colours <- c("#E8F8F5", "#A3E4D7", "#48C9B0", "#17A589", "#117864", "#F5B7B1")
        
        # validate(
        #     need(length(unique(data_fills)) == length(fill_colours),
        #          "Not enough data to create Sleep Calendar. Try selecting longer range of dates")
        # )
        
        # Calendar
        calendR(from = start_date,
                to = end_date,
                start = "M",
                special.days = data_fills,
                special.col = fill_colours[data_filter],
                legend.pos = "bottom",
                title = "Deep Sleep % Calendar") 
    })
    
    output$sleepDeepAmountCalendarPlot <- renderPlot({
        validate(
            need(input$dateRange2Sleep[1] <= input$dateRange2Sleep[2],
                 "Double check date range to create Deep Sleep Calendar Plot")
        )
        
        # edit last 12 months to avoid one year limit on calendR function
        end_date <- input$dateRange2Sleep[2]
        start_date <- max(input$dateRange2Sleep[1] , end_date - 366)
        
        month(start_date) <- month(start_date) + 1
        start_date <- floor_date(start_date, unit = "month")
        
        dates <- tibble(date = seq.Date(from = start_date, 
                                        to = end_date, by="day"))
        
        df_sleep <- 
            df_sleep_wide %>%
            filter(date >= start_date, date <= end_date)
        
        sleep_data <- 
            dates %>%
            left_join(df_sleep, by = "date") %>%
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
        
        # category and filters for when there is no data points
        data_category <- c("A: <15", "B: 15-30", "C: 30-45", "D: 45-60",
                           "E: 60+", "NA")
        
        data_filter <- c()
        
        for(c in data_category){
            if (c %in% data_fills) {
                data_filter <- append(data_filter, TRUE)
            } else {
                data_filter <- append(data_filter, FALSE)
            }
        }
        fill_colours <- c("#E8F8F5", "#A3E4D7", "#48C9B0", "#17A589", "#117864", "#F5B7B1")
        
        # validate(
        #     need(length(unique(data_fills)) == length(fill_colours),
        #          "Not enough data to create Sleep Calendar. Try selecting longer range of dates")
        # )
        
        # Calendar
        calendR(from = start_date,
                to = end_date,
                start = "M",
                special.days = data_fills,
                special.col = fill_colours[data_filter],
                legend.pos = "bottom",
                title = "Deep Sleep Minutes Calendar") 
    })
    
    output$sleepStagePlot <- renderPlot({
        validate(
            need(input$dateRange2Sleep[1] <= input$dateRange2Sleep[2],
                 "Double check date range to create Sleep Stage Plot")
        )
        
        df_sleep_wide %>%
            filter(date >= input$dateRange2Sleep[1], date <= input$dateRange2Sleep[2]) %>%
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
            scale_y_continuous(labels = scales::percent,
                               breaks = scales::breaks_pretty(20)) +
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
        validate(
            need(input$dateRange2Sleep[1] <= input$dateRange2Sleep[2],
                 "Double check date range to create Bedtime Plot")
        )
            
            
            
        df_sleep_bedtime_long %>%
            filter(date >= input$dateRange2Sleep[1], date <= input$dateRange2Sleep[2]) %>%
            ggplot(aes(date, y = as_datetime(time, tz = "EST"))) + 
            geom_point(aes(colour = time_type)) +
            scale_x_date(breaks = scales::breaks_pretty(10)) +
            scale_y_datetime(breaks = scales::breaks_pretty(10)) +
            geom_hline(
                aes(yintercept=as_datetime("2000-01-01 00:00:00", tz = "EST"),
                    linetype = "Midnight", color="Midnight")
            ) +
            geom_hline(
                aes(yintercept=as_datetime("2000-01-02 07:00:00", tz = "EST"),
                    linetype = "7:00 AM", color="7:00 AM")
            ) +
            facet_wrap(~time_type, scales = "free_y") + 
            geom_smooth(se=FALSE, colour = "grey30")+
            theme_fivethirtyeight() +
            labs(
                title = "Bedtime Trend",
                x = "Date",
                y = "Time"
            ) +
            theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(), 
                axis.title.x = element_text()
            ) 
        
    })
    
}

shinyApp(ui, server)

