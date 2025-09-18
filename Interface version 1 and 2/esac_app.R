library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyjs)
library(plotly)
library(writexl)

setwd("C:/Users/AnnaMatuszewska/Documents/new_esac_app")
data <- read.csv("combined_data.csv")

# Load preliminary data 
data$region <- sapply(strsplit(as.character(data$ARE), ","), function(x) {
  if (length(x) >= 4) {
    return(trimws(paste(x[3], x[4], sep = ": "))) 
  } else if (length(x) >= 3) { 
    return(trimws(x[3]))
  } else {
    return("no region") #for NA values
  }
})
data$region <- gsub("\\.$", "", data$region) %>% trimws()
regions <- unique(data$region) %>% sort()

data$GTL_trimmed <- gsub("\\.+$", "", data$GTL) |> trimws()
types <- sort(unique(na.omit(data$GTL_trimmed)))

scales <- unique(na.omit(data$AMD))
meters <- unique(na.omit(data$meters))
meters <- sapply(meters, function(x) {
  if (grepl(",", as.character(x)) || is.na(x)) {  #reduce to mixed if there is a comma
    return("mixed") 
  } else {
    return(as.character(x))
  }
})
meters <- unique(meters)

tonalities <- unique(na.omit(data$key))  # Extract unique values from 'key'
titles <- unique(na.omit(data$OTL))
scale_degrees_all <- sort(unique(na.omit(as.character(data$scl_value))))


# Compute ambitus per filename and merge it with the main dataset
ambitus_data <- data %>%
  group_by(filename) %>%
  summarise(
    max_pitch = ifelse(n() > 0 && !all(is.na(pitch)), max(pitch, na.rm = TRUE), NA),
    min_pitch = ifelse(n() > 0 && !all(is.na(pitch)), min(pitch, na.rm = TRUE), NA),
    ambitus = ifelse(!is.na(max_pitch) && !is.na(min_pitch), max_pitch - min_pitch, NA)
  )

# Merge ambitus values into the main dataset
data <- left_join(data, ambitus_data %>% select(filename, ambitus), by = "filename")

# Create a list of unique Ambitus values for filtering
ambitus_values <- unique(na.omit(data$ambitus))

lyrics_df <- read.csv("lyrics.csv", stringsAsFactors = FALSE)


# DASHBOARD-UI
ui <- dashboardPage(
  dashboardHeader(title = "EsAC Deutsch | Subcollections Comparison"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",  
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Data Table - Set 1", tabName = "fulltable", icon = icon("table")),
                menuItem("Data Table - Set 2", tabName = "fulltable2", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box {
          height: 70px !important;
          width: 120px !important;           /* narrower width */
          margin-right: 10px !important;     /* horizontal spacing */
          text-align: center;                /* horizontal center */
          display: flex;
          flex-direction: column;
          justify-content: center;          /* vertical center */
          align-items: center;              /* horizontal center for all */
          border-radius: 8px !important;
        }
    
        .small-box .inner h3 {
          font-size: 20px !important;
          margin: 0;
          padding: 0;
        }
    
        .small-box .inner p {
          font-size: 13px !important;
          margin: 0;
          padding: 0;
        }
    
        .small-box .icon {
          display: none !important;         /* optional: hide icon if not used */
        }
    
        /* Optional: tweak outer layout */
        .row .col-sm-3 {
          max-width: 160px !important;
          padding-left: 5px !important;
          padding-right: 5px !important;
        }
        
      "))
    ),
    tabItems(
      # DASHBOARD
      tabItem(tabName = "dashboard",
              fluidRow(
                # LEFT: Filter Tabs (Set 1 & Set 2)
                column(width = 2,
                       tabBox(width = 12, id = "filterTabs", 
                              tabPanel("Set 1 Filters", div(style = "background-color: #E5F1FB; padding: 10px;",
                                       selectizeInput("region1", "Region", choices = regions, multiple = TRUE),
                                       selectizeInput("type1", "Social function", choices = types, multiple = TRUE),
                                       selectizeInput("scale1", "Scale", choices = scales, multiple = TRUE),
                                       selectizeInput("tonality1", "Tonality", choices = tonalities, multiple = TRUE),
                                       selectizeInput("scale_degrees1","Scale Degrees", choices = sort(unique(scale_degrees_all)), multiple = TRUE, options = list(placeholder = 'Select scale degrees...')),
                                       selectizeInput("ambitus1", "Ambitus", choices = sort(unique(ambitus_values)), multiple = TRUE),
                                       selectizeInput("meters1", "Meters", choices = meters, multiple = TRUE),
                                       selectizeInput("titles1", "Song Title", choices = titles, multiple = TRUE, options = list(placeholder = 'Type to search titles...')),
                                       textInput("lyrics_search1", "Search Lyrics", placeholder = "Enter word or phrase...")
                                )
                              ),
                              tabPanel("Set 2 Filters", div(style = "background-color: #FCE6E3; padding: 10px;",
                                       selectizeInput("region2", "Region", choices = regions, multiple = TRUE),
                                       selectizeInput("type2", "Social function", choices = types, multiple = TRUE),
                                       selectizeInput("scale2", "Scale", choices = scales, multiple = TRUE),
                                       selectizeInput("tonality2", "Tonality", choices = tonalities, multiple = TRUE),
                                       selectizeInput("scale_degrees2","Scale Degrees", choices = sort(unique(scale_degrees_all)), multiple = TRUE, options = list(placeholder = 'Select scale degrees...')),
                                       selectizeInput("ambitus2", "Ambitus", choices = sort(unique(ambitus_values)), multiple = TRUE),
                                       selectizeInput("meters2", "Meters", choices = meters, multiple = TRUE),
                                       selectizeInput("titles2", "Song Title", choices = titles, multiple = TRUE, options = list(placeholder = 'Type to search titles...')),
                                       textInput("lyrics_search2", "Search Lyrics", placeholder = "Enter word or phrase...")
                                )
                              )
                       ),
                       div(
                         style = "text-align: center; margin-top: 10px;",
                         downloadButton("download_excel", "Download Chart Data (.xlsx)")
                       )
                         
                ),
                
              # Column 1: Set 1
              column(width = 10,
                       
                  column(width = 5,
                       
                       # KPIs - set 1
                       fluidRow(
                         valueBoxOutput("distinct_filenames", width = 3),
                         valueBoxOutput("max_pitch", width = 3),
                         valueBoxOutput("min_pitch", width = 3),
                         valueBoxOutput("ambitus", width = 3)
                       ),
                       
                       # Tabbed distributions - set 1
                       fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Meters", uiOutput("metersDistributionUI")),
                                 tabPanel("Tonality", uiOutput("tonalityDistributionUI"))
                          )
                       )
                  ),
                
                  column(width = 1),
                       
                  column(width = 5,
                       
                       # KPIs - set 2
                       fluidRow(
                         valueBoxOutput("distinct_filenames2", width = 3),
                         valueBoxOutput("max_pitch2", width = 3),
                         valueBoxOutput("min_pitch2", width = 3),
                         valueBoxOutput("ambitus2", width = 3)
                       ),
                       
                       # Tabbed distributions - set 2
                       fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Meters", uiOutput("metersDistributionUI2")),
                                 tabPanel("Tonality", uiOutput("tonalityDistributionUI2"))
                          )
                       ),
                  ),
                       
                fluidRow(
                  box(width = 11,
                      fluidRow(
                        column(width = 5,
                               selectInput("chartType", label = NULL, 
                                                  choices = c("Interval Distribution", "Summative with Quality distinction", "Tree Map"), 
                                                  selected = "Interval Distribution")
                               )
                             ),
                        uiOutput("selectedChart")
                  )
                )
                       
                ) # end of column = 10, main dashbaord area
                
              ) # end fluidRow
      ), # end tabItem dashboard
      
      # FULL TABLE
      tabItem(tabName = "fulltable",
              fluidRow(
                box(title = "Data Table - Set 1", width = 12, DTOutput("dataTable"))
              )
      ),
      
      tabItem(tabName = "fulltable2",
              fluidRow(
                box(title = "Data Table - Set 2", width = 12, DTOutput("dataTable2"))
              )
      )
      
    ) # end tabItems
  ) # end dashboardBody
)


server <- function(input, output, session) {
  
  clean_meters <- function(x) {
    sapply(x, function(val) {
      if (is.na(val) || grepl(",", as.character(val))) {
        "mixed"
      } else {
        as.character(val)
      }
    })
  }
  
  matched_filenames <- reactive({
    req(input$lyrics_search1)
    search_term <- tolower(trimws(input$lyrics_search1))
    
    if (search_term == "") return(NULL)
    
    lyrics_df %>%
      filter(!is.na(lyrics)) %>%
      filter(grepl(search_term, tolower(lyrics))) %>%
      pull(filename)
  })
  
  matched_filenames2 <- reactive({
    req(input$lyrics_search2)
    search_term <- tolower(trimws(input$lyrics_search2))
    
    if (search_term == "") return(NULL)
    
    lyrics_df %>%
      filter(!is.na(lyrics)) %>%
      filter(grepl(search_term, tolower(lyrics))) %>%
      pull(filename)
  })
  
  filtered_for_choices1 <- reactive({
    df <- data
    df$meters <- clean_meters(df$meters)
    
    if (!is.null(input$region1) && length(input$region1) > 0) df <- df[df$region %in% input$region1, ]
    if (!is.null(input$type1) && length(input$type1) > 0) df <- df[df$GTL_trimmed %in% input$type1, ]
    if (!is.null(input$scale1) && length(input$scale1) > 0) df <- df[df$AMD %in% input$scale1, ]
    if (!is.null(input$meters1) && length(input$meters1) > 0) df <- df[df$meters %in% input$meters1, ]
    if (!is.null(input$tonality1) && length(input$tonality1) > 0) df <- df[df$key %in% input$tonality1, ]
    if (!is.null(input$ambitus1) && length(input$ambitus1) > 0) df <- df[df$ambitus %in% as.numeric(input$ambitus1), ]
    if (!is.null(input$titles1) && length(input$titles1) > 0) df <- df[df$OTL %in% input$titles1, ]
    if (!is.null(input$lyrics_search1) && input$lyrics_search1 != "") {
      matched <- matched_filenames()
      df <- df[df$filename %in% matched, ]
    }
    
    df
  })
  
  observe({
    df <- filtered_for_choices1()
    
    updateSelectizeInput(session, "region1",
                         choices = sort(unique(df$region)),
                         selected = input$region1)
    
    updateSelectizeInput(session, "type1",
                         choices = sort(unique(df$GTL_trimmed)),
                         selected = input$type1)
    
    updateSelectizeInput(session, "scale1",
                         choices = sort(unique(df$AMD)),
                         selected = input$scale1)
    
    updateSelectizeInput(session, "tonality1",
                         choices = sort(unique(df$key)),
                         selected = input$tonality1)
    
    updateSelectizeInput(session, "ambitus1",
                         choices = sort(unique(df$ambitus)),
                         selected = input$ambitus1)
    
    updateSelectizeInput(session, "meters1",
                         choices = sort(unique(df$meters)),
                         selected = input$meters1)
    
    updateSelectizeInput(session, "titles1",
                         choices = sort(unique(df$OTL)),
                         selected = input$titles1)
    
    updateSelectizeInput(session, "scale_degrees1",
                         choices = sort(unique(as.character(df$scl_value))),
                         selected = input$scale_degrees1)
  })
  
  
  filtered_for_choices2 <- reactive({
    df <- data
    df$meters <- clean_meters(df$meters)
    
    if (!is.null(input$region2) && length(input$region2) > 0) df <- df[df$region %in% input$region2, ]
    if (!is.null(input$type2) && length(input$type2) > 0) df <- df[df$GTL_trimmed %in% input$type2, ]
    if (!is.null(input$scale2) && length(input$scale2) > 0) df <- df[df$AMD %in% input$scale2, ]
    if (!is.null(input$meters2) && length(input$meters2) > 0) df <- df[df$meters %in% input$meters2, ]
    if (!is.null(input$tonality2) && length(input$tonality2) > 0) df <- df[df$key %in% input$tonality2, ]
    if (!is.null(input$ambitus2) && length(input$ambitus2) > 0) df <- df[df$ambitus %in% as.numeric(input$ambitus2), ]
    if (!is.null(input$titles2) && length(input$titles2) > 0) df <- df[df$OTL %in% input$titles2, ]
    if (!is.null(input$lyrics_search2) && input$lyrics_search2 != "") {
      matched <- matched_filenames2()
      df <- df[df$filename %in% matched, ]
    }
    
    df
  })
  
  observe({
    df <- filtered_for_choices2()
    
    updateSelectizeInput(session, "region2",
                         choices = sort(unique(df$region)),
                         selected = input$region2)
    
    updateSelectizeInput(session, "type2",
                         choices = sort(unique(df$GTL_trimmed)),
                         selected = input$type2)
    
    updateSelectizeInput(session, "scale2",
                         choices = sort(unique(df$AMD)),
                         selected = input$scale2)
    
    updateSelectizeInput(session, "tonality2",
                         choices = sort(unique(df$key)),
                         selected = input$tonality2)
    
    updateSelectizeInput(session, "ambitus2",
                         choices = sort(unique(df$ambitus)),
                         selected = input$ambitus2)
    
    updateSelectizeInput(session, "meters2",
                         choices = sort(unique(df$meters)),
                         selected = input$meters2)
    
    updateSelectizeInput(session, "titles2",
                         choices = sort(unique(df$OTL)),
                         selected = input$titles2)
    
    updateSelectizeInput(session, "scale_degrees2",
                         choices = sort(unique(as.character(df$scl_value))),
                         selected = input$scale_degrees2)
  })

  filtered_data <- reactive({
    df <- data
    
    df$meters <- clean_meters(df$meters)
    
    # Przekształcanie na tekst i usuwanie nadmiarowych spacji
    if (!is.null(input$region1) && length(input$region1) > 0) {
      selected_regions <- trimws(input$region1)
      
      if ("no region" %in% selected_regions) {
        # Include rows where region matches OR is missing
        df <- df[is.na(data$ARE) | df$region %in% selected_regions, ]
      } else {
        df <- df[df$region %in% selected_regions, ]
      }
    }
    if (!is.null(input$type1) && length(input$type1) > 0) {
      df <- df[df$GTL_trimmed %in% input$type1, ]
    }
    # Scale
    if (!is.null(input$scale1) && length(input$scale1) > 0) {
      df <- df[!is.na(df$AMD) & df$AMD %in% input$scale1, ]
    }
    # Meters
    if (!is.null(input$meters1) && length(input$meters1) > 0) {
      df <- df[df$meters %in% input$meters1, ]
    }
    # Tonality
    if (!is.null(input$tonality1) && length(input$tonality1) > 0) {
      df <- df[!is.na(df$key) & df$key %in% input$tonality1, ]
    }
    # Ambitus
    if (!is.null(input$ambitus1) && length(input$ambitus1) > 0) {
      df <- df[!is.na(df$ambitus) & df$ambitus %in% as.numeric(input$ambitus1), ]
    }
    if (!is.null(input$titles1) && length(input$titles1) > 0) {
      df <- df[df$OTL %in% input$titles1, ]
    }
    if (!is.null(input$lyrics_search1) && input$lyrics_search1 != "") {
      matched <- matched_filenames()
      df <- df[df$filename %in% matched, ]
    }
    validate(
      need(nrow(df) > 0, "No data available for the current filter selection.")
    )
    
    if (!is.null(input$scale_degrees1) && length(input$scale_degrees1) > 0) {
      df <- df[as.character(df$scl_value) %in% input$scale_degrees1, ]
    }
    
    df
  })
  
  filtered_data2 <- reactive({
    df <- data
    df$meters <- clean_meters(df$meters)
    
    if (!is.null(input$region2) && length(input$region2) > 0) {
      df <- df[df$region %in% input$region2, ]
    }
    if (!is.null(input$type2) && length(input$type2) > 0) {
      df <- df[df$GTL_trimmed %in% input$type2, ]
    }
    if (!is.null(input$scale2) && length(input$scale2) > 0) {
      df <- df[df$AMD %in% input$scale2, ]
    }
    if (!is.null(input$meters2) && length(input$meters2) > 0) {
      df <- df[df$meters %in% input$meters2, ]
    }
    if (!is.null(input$tonality2) && length(input$tonality2) > 0) {
      df <- df[df$key %in% input$tonality2, ]
    }
    if (!is.null(input$ambitus2) && length(input$ambitus2) > 0) {
      df <- df[df$ambitus %in% as.numeric(input$ambitus2), ]
    }
    if (!is.null(input$titles2) && length(input$titles2) > 0) {
      df <- df[df$OTL %in% input$titles2, ]
    }
    if (!is.null(input$lyrics_search2) && input$lyrics_search2 != "") {
      matched <- matched_filenames2()  # If lyrics are same file, reuse or duplicate function if needed
      df <- df[df$filename %in% matched, ]
    }
    
    validate(
      need(nrow(df) > 0, "No data available for the second filter set.")
    )
    
    if (!is.null(input$scale_degrees2) && length(input$scale_degrees2) > 0) {
      df <- df[as.character(df$scl_value) %in% input$scale_degrees2, ]
    }
    
    df
  })
  
 
  
  metrics <- reactive({
    df_filtered <- filtered_data()
    if (nrow(df_filtered) == 0) {
      return(data.frame(
        distinct_filenames = 0,
        max_pitch = NA,
        min_pitch = NA,
        ambitus = NA
      ))
    }
    
    data_summary <- df_filtered %>%
      group_by(filename) %>%
      summarise(
        max_pitch = ifelse(n() > 0 && !all(is.na(pitch)), max(pitch, na.rm = TRUE), NA),
        min_pitch = ifelse(n() > 0 && !all(is.na(pitch)), min(pitch, na.rm = TRUE), NA),
        num_notes = n(),
        num_pitch_zero = sum(ifelse(is.na(pitch), 0, pitch) == 0, na.rm = TRUE),
        ambitus = ifelse(!is.na(max_pitch) && !is.na(min_pitch), max_pitch - min_pitch, NA)
      )
    
    # Wyliczenie średniego ambitus
    avg_ambitus <- mean(data_summary$ambitus, na.rm = TRUE)
    
    data_summary %>%
      summarise(
        distinct_filenames = n_distinct(filename),
        max_pitch = ifelse(n() > 0, max(max_pitch, na.rm = TRUE), NA),
        min_pitch = ifelse(n() > 0, min(min_pitch, na.rm = TRUE), NA),
        ambitus = avg_ambitus
      )
  })
  
  metrics2 <- reactive({
    df_filtered <- filtered_data2()
    if (nrow(df_filtered) == 0) {
      return(data.frame(
        distinct_filenames = 0,
        max_pitch = NA,
        min_pitch = NA,
        ambitus = NA
      ))
    }
    
    data_summary <- df_filtered %>%
      group_by(filename) %>%
      summarise(
        max_pitch = ifelse(n() > 0 && !all(is.na(pitch)), max(pitch, na.rm = TRUE), NA),
        min_pitch = ifelse(n() > 0 && !all(is.na(pitch)), min(pitch, na.rm = TRUE), NA),
        num_notes = n(),
        num_pitch_zero = sum(ifelse(is.na(pitch), 0, pitch) == 0, na.rm = TRUE),
        ambitus = ifelse(!is.na(max_pitch) && !is.na(min_pitch), max_pitch - min_pitch, NA)
      )
    
    # Wyliczenie średniego ambitus
    avg_ambitus <- mean(data_summary$ambitus, na.rm = TRUE)
    
    data_summary %>%
      summarise(
        distinct_filenames = n_distinct(filename),
        max_pitch = ifelse(n() > 0, max(max_pitch, na.rm = TRUE), NA),
        min_pitch = ifelse(n() > 0, min(min_pitch, na.rm = TRUE), NA),
        ambitus = avg_ambitus
      )
  })
  
  meters_distribution <- reactive({
    df_filtered <- filtered_data()
    
    df_filtered$meters_grouped <- sapply(df_filtered$meters, function(x) {
      if (is.na(x) || grepl(",", as.character(x))) {
        return("mixed")
      } else {
        return(as.character(x))
      }
    })
    
    df_meters <- df_filtered %>%
      group_by(meters_grouped) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = round(count / sum(count) * 100, 2))
    
    # Group small categories under 2% into "Other"
    df_meters <- df_meters %>%
      mutate(group = ifelse(percentage < 2, "Other", meters_grouped)) %>%
      group_by(group) %>%
      summarise(percentage = sum(percentage), .groups = "drop") %>%
      arrange(desc(percentage))
    
    names(df_meters)[1] <- "meters_grouped"  # rename back for UI consistency
    
    df_meters
  })
  
  
  meters_distribution2 <- reactive({
    df_filtered <- filtered_data2()
    
    df_filtered$meters_grouped <- sapply(df_filtered$meters, function(x) {
      if (is.na(x) || grepl(",", as.character(x))) {
        return("mixed")
      } else {
        return(as.character(x))
      }
    })
    
    df_meters <- df_filtered %>%
      group_by(meters_grouped) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = round(count / sum(count) * 100, 2))
    
    # Group small categories under 2% into "Other"
    df_meters <- df_meters %>%
      mutate(group = ifelse(percentage < 2, "Other", meters_grouped)) %>%
      group_by(group) %>%
      summarise(percentage = sum(percentage), .groups = "drop") %>%
      arrange(desc(percentage))
    
    names(df_meters)[1] <- "meters_grouped"  # rename back for UI consistency
    
    df_meters
  })
  
  
  tonality_distribution <- reactive({
    df_filtered <- filtered_data()
    
    df_ground <- df_filtered %>%
      filter(!is.na(key)) %>%
      group_by(key) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = round(count / sum(count) * 100, 2))
    
    # Group small ones under 2% into "Other"
    df_ground <- df_ground %>%
      mutate(group = ifelse(percentage < 2, "Other", key)) %>%
      group_by(group) %>%
      summarise(percentage = sum(percentage), .groups = "drop") %>%
      arrange(desc(percentage))
    
    names(df_ground)[1] <- "key"  # for UI
    df_ground
  })
  
  
  tonality_distribution2 <- reactive({
    df_filtered <- filtered_data2()
    
    df_ground <- df_filtered %>%
      filter(!is.na(key)) %>%
      group_by(key) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = round(count / sum(count) * 100, 2))
    
    # Group small ones under 2% into "Other"
    df_ground <- df_ground %>%
      mutate(group = ifelse(percentage < 2, "Other", key)) %>%
      group_by(group) %>%
      summarise(percentage = sum(percentage), .groups = "drop") %>%
      arrange(desc(percentage))
    
    names(df_ground)[1] <- "key"  # for UI
    df_ground
  })
  
    
  interval_data <- reactive({
    df1 <- filtered_data()
    df2 <- filtered_data2()
    
    df1 <- df1[!is.na(df1$interval_name) & !is.na(df1$interval) & !is.na(df1$mint) & df1$interval <= 10, ]
    df2 <- df2[!is.na(df2$interval_name) & !is.na(df2$interval) & !is.na(df2$mint) & df2$interval <= 10, ]
    
    df1$direction <- ifelse(grepl("^\\-", df1$mint), "down", "up")
    df2$direction <- ifelse(grepl("^\\-", df2$mint), "down", "up")
    
    df1$set <- "Set 1"
    df2$set <- "Set 2"
    
    combined <- bind_rows(df1, df2)
    
    # Extract generic interval name
    combined$interval_generic <- sapply(strsplit(as.character(combined$interval_name), " "), tail, 1)
    
    # Create mapping: interval_generic → smallest interval number
    order_map <- combined %>%
      group_by(interval_generic) %>%
      summarise(order_val = min(interval, na.rm = TRUE), .groups = "drop")
    
    grouped <- combined %>%
      group_by(interval_generic, direction, set) %>%
      summarise(count = n(), .groups = "drop") %>%
      filter(count > 0) %>%
      mutate(signed_count = ifelse(direction == "down", -count, count)) %>%
      group_by(set) %>%
      mutate(percent = round(count / sum(count) * 100, 2)) %>%
      ungroup()
    
    # Merge back the sorting order
    grouped <- left_join(grouped, order_map, by = "interval_generic")
    grouped <- grouped %>% arrange(order_val)
    
    # Set factor levels for proper X-axis ordering
    grouped$interval_generic <- factor(grouped$interval_generic, levels = unique(grouped$interval_generic))
    
    grouped
  })
  
  
  interval_quality_data <- reactive({
    df1 <- filtered_data()
    df2 <- filtered_data2()
    
    df1 <- df1[!is.na(df1$interval_name) & !is.na(df1$interval_numerical) &
                 !is.na(df1$mint) & df1$interval_numerical <= 10, ]
    
    df2 <- df2[!is.na(df2$interval_name) & !is.na(df2$interval_numerical) &
                 !is.na(df2$mint) & df2$interval_numerical <= 10, ]
    
    df1$direction <- ifelse(grepl("^\\-", df1$mint), "down", "up")
    df2$direction <- ifelse(grepl("^\\-", df2$mint), "down", "up")
    
    df1$set <- "Set 1"
    df2$set <- "Set 2"
    
    combined <- bind_rows(df1, df2)
    
    grouped <- combined %>%
      group_by(interval_name, interval_numerical, direction, set) %>%
      summarise(count = n(), .groups = "drop") %>%
      filter(count > 0) %>%
      mutate(signed_count = ifelse(direction == "down", -count, count)) %>%
      group_by(set) %>%
      mutate(percent = round(count / sum(count) * 100, 2)) %>%
      ungroup() %>%
      arrange(interval_numerical)
    
    grouped$interval_name <- factor(grouped$interval_name, levels = unique(grouped$interval_name))
    
    grouped
  })
  
  
  tree_map_data <- reactive({
    
    df1 <- filtered_data()
    df2 <- filtered_data2()
    
    df1$set <- "Set 1"
    df2$set <- "Set 2"
    
    df <- bind_rows(df1, df2)
    df <- df[!is.na(df$note_length_name), ]
    
    df %>%
      group_by(set, note_length_name) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(set) %>%
      mutate(percent = round(count / sum(count) * 100, 2)) %>%
      ungroup()
    
  })
  #KPIs SET 1
  output$distinct_filenames <- renderValueBox({
    valueBox(metrics()$distinct_filenames, "Distinct Filenames", color = "blue")
  })
  
  output$max_pitch <- renderValueBox({
    valueBox(metrics()$max_pitch, "Max Pitch", color = "blue")
  })
  
  output$min_pitch <- renderValueBox({
    valueBox(metrics()$min_pitch, "Min Pitch", color = "blue")
  })
  
  output$ambitus <- renderValueBox({
    valueBox(round(metrics()$ambitus, 1), "Ambitus (Avg)", color = "blue")
  })
  
  #KPIs SET 2
  output$distinct_filenames2 <- renderValueBox({
    valueBox(metrics2()$distinct_filenames, "Distinct Filenames", color = "red")
  })
  
  output$max_pitch2 <- renderValueBox({
    valueBox(metrics2()$max_pitch, "Max Pitch", color = "red")
  })
  
  output$min_pitch2 <- renderValueBox({
    valueBox(metrics2()$min_pitch, "Min Pitch", color = "red")
  })
  
  output$ambitus2 <- renderValueBox({
    valueBox(round(metrics2()$ambitus, 1), "Ambitus (Avg)", color = "red")
  })
  
  #SET1
  output$metersDistributionUI <- renderUI({
    df_meters <- meters_distribution()
    fluidRow(
      lapply(1:nrow(df_meters), function(i) {
        column(width = 2, align = "center",
               strong(df_meters$meters_grouped[i]), br(),
               span(paste0(df_meters$percentage[i], "%")))
      })
    )
  })
  
  output$tonalityDistributionUI <- renderUI({
    df_ground <- tonality_distribution()
    fluidRow(
      lapply(1:nrow(df_ground), function(i) {
        column(width = 2, align = "center",
               strong(df_ground$key[i]), br(),
               span(paste0(df_ground$percentage[i], "%")))
      })
    )
  })
  
  #SET2
  output$metersDistributionUI2 <- renderUI({
    df_meters <- meters_distribution2()
    fluidRow(
      lapply(1:nrow(df_meters), function(i) {
        column(width = 2, align = "center",
               strong(df_meters$meters_grouped[i]), br(),
               span(paste0(df_meters$percentage[i], "%")))
      })
    )
  })
  
  output$tonalityDistributionUI2 <- renderUI({
    df_ground <- tonality_distribution2()
    fluidRow(
      lapply(1:nrow(df_ground), function(i) {
        column(width = 2, align = "center",
               strong(df_ground$key[i]), br(),
               span(paste0(df_ground$percentage[i], "%")))
      })
    )
  })
  
  output$intervalChart <- renderPlotly({
    df <- interval_data()
    df$fill_key <- interaction(df$set, df$direction, sep = " ")
    
    p <- ggplot(df, aes(x = interval_generic, y = signed_count, fill = fill_key,
                        text = paste0("Interval: ", interval_generic,
                                      "<br>Percent: ", percent, "% (", count, ")"))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "Set 1 up" = "#0073B7",
        "Set 1 down" = "#66B2E8",
        "Set 2 up" = "#DD4B39",
        "Set 2 down" = "#F08080"
      )) +
      geom_hline(yintercept = 0, color = "black") +
      labs(
        x = "Interval", y = "Signed Count", fill = "Set & Direction"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>% 
      layout(title = NULL, 
             showlegend = FALSE,
             hoverlabel = list(
               font = list(size = 16, family = "Arial")
             )
      )
  })
  
  
  output$intervalQualityChart <- renderPlotly({
    df <- interval_quality_data()
    df$fill_key <- interaction(df$set, df$direction, sep = " ")
    
    p <- ggplot(df, aes(x = interval_name, y = signed_count, fill = fill_key,
                        text = paste0("Interval: ", interval_name,
                                      "<br>Percent: ", percent, "% (", count, ")"))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "Set 1 up" = "#0073B7",
        "Set 1 down" = "#66B2E8",
        "Set 2 up" = "#DD4B39",
        "Set 2 down" = "#F08080"
      )) +
      geom_hline(yintercept = 0, color = "black") +
      labs(
        x = "Interval Name", y = "Signed Count", fill = "Set & Direction"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>% 
      layout(title = NULL,
             showlegend = FALSE,
             hoverlabel = list(
               font = list(size = 16, family = "Arial")
             )
      )
  })
  
  
  note_unicode_map <- list(
    "crotchet" = "𝅘𝅥",
    "dotted crotchet" = "𝅘𝅥.",
    "minim" = "𝅗𝅥",
    "dotted minim" = "𝅗𝅥.",
    "semibreve" = "𝅝",
    "quaver" = "𝅘𝅥𝅮",
    "dotted quaver" = "𝅘𝅥𝅮.",
    "semiquaver" = "𝅘𝅥𝅯"
  )
  
  get_note_label <- function(name) {
    unicode <- note_unicode_map[[tolower(name)]]
    if (is.null(unicode)) return(name)
    paste(name, unicode)
  }
  
  tree_map_data_filtered1 <- reactive({
    df <- tree_map_data()
    df1 <- df[df$set == "Set 1", ]
    df1$symbol <- sapply(df1$note_length_name, get_note_label)
    
    if (!is.null(input$note_symbol_filter1) && length(input$note_symbol_filter1) > 0) {
      df1 <- df1[df1$symbol %in% input$note_symbol_filter1, ]
    }
    
    df1 %>%
      mutate(percent = round(100 * count / sum(count), 2))
  })
  
  tree_map_data_filtered2 <- reactive({
    df <- tree_map_data()
    df2 <- df[df$set == "Set 2", ]
    df2$symbol <- sapply(df2$note_length_name, get_note_label)
    
    if (!is.null(input$note_symbol_filter2) && length(input$note_symbol_filter2) > 0) {
      df2 <- df2[df2$symbol %in% input$note_symbol_filter2, ]
    }
    
    df2 %>%
      mutate(percent = round(100 * count / sum(count), 2))
  })
  
  
  # Create a Tree Map chart  - SUNBURST CHART as alternative
  output$treeMapChart1 <- renderPlotly({
    df <- tree_map_data()
    df1 <- tree_map_data_filtered1()
    
    # Precompute labels
    df1$hover_text <- paste0(
      "<span style='font-size:20px; font-weight:bold;'>",
      sapply(df1$note_length_name, get_note_label),
      "</span><br>",
      "<span style='font-size:14px;'>Set 1</span><br>",
      "<span style='font-size:14px;'>Percent: ", df1$percent, "%</span>"
    )
    
    if (nrow(df1) > 0) {
      plot_ly(
        data = df1,
        type = "treemap",
        labels = sapply(df1$note_length_name, get_note_label),
        parents = "",
        values = ~count,
        marker = list(
          colors = df1$percent,
          colorscale = "Blues",
          reversescale = TRUE,
          showscale = TRUE,
          colorbar = list(title = "Percent")
        ),
        textinfo = "label+value+percent entry",
        text = ~hover_text,
        hoverinfo = "text"
      ) %>%
        layout(
          title = NULL,
          margin = list(t = 0),
          showlegend = FALSE,
          hoverlabel = list(font = list(size = 16, family = "Arial"))
        )
    } else {
      plot_ly() %>% layout(title = "No data for Set 1")
    }
  })
  
  
  
  output$treeMapChart2 <- renderPlotly({
    df <- tree_map_data()
    df2 <- tree_map_data_filtered2()
    
    # Precompute labels
    df2$hover_text <- paste0(
      "<span style='font-size:20px; font-weight:bold;'>",
      sapply(df2$note_length_name, get_note_label),
      "</span><br>",
      "<span style='font-size:14px;'>Set 2</span><br>",
      "<span style='font-size:14px;'>Percent: ", df2$percent, "%</span>"
    )
    
    if (nrow(df2) > 0) {
      plot_ly(
        data = df2,
        type = "treemap",
        labels = sapply(df2$note_length_name, get_note_label),
        parents = "",
        values = ~count,
        marker = list(
          colors = df2$percent,
          colorscale = "Reds",
          reversescale = FALSE,
          showscale = TRUE,
          colorbar = list(title = "Percent")
        ),
        textinfo = "label+value+percent entry",
        text = ~hover_text,
        hoverinfo = "text"
      ) %>%
        layout(
          title = NULL,
          margin = list(t = 0),
          showlegend = FALSE,
          hoverlabel = list(font = list(size = 16, family = "Arial"))
        )
    } else {
      plot_ly() %>% layout(title = "No data for Set 2")
    }
  })
  
  
  # Control to toggle between charts using a dropdown
  output$chartSelection <- renderUI({
    selectInput("chartType", label = NULL, 
                choices = c("Interval Distribution", "Tree Map", "SUMMATIVE with Quality distinction"), 
                selected = "Interval Distribution")
  })
  
  output$selectedChart <- renderUI({
    chart_type <- input$chartType
    
    if (chart_type == "Tree Map") {
      tagList(
        # Set 1: filter + chart
        selectizeInput(
          "note_symbol_filter1",
          label = "Set 1: Filter by Note Symbol",
          choices = sort(unique(sapply(unique(filtered_for_choices1()$note_length_name), get_note_label))),
          multiple = TRUE,
          selected = NULL,
          options = list(placeholder = "Select symbols...")
        ),
        plotlyOutput("treeMapChart1"),
        
        # Spacer (optional)
        tags$hr(),
        
        # Set 2: filter + chart
        selectizeInput(
          "note_symbol_filter2",
          label = "Set 2: Filter by Note Symbol",
          choices = sort(unique(sapply(unique(filtered_for_choices2()$note_length_name), get_note_label))),
          multiple = TRUE,
          selected = NULL,
          options = list(placeholder = "Select symbols...")
        ),
        plotlyOutput("treeMapChart2")
      )
      
    } else {
      # Non-tree-map charts
      switch(chart_type,
             "Interval Distribution" = plotlyOutput("intervalChart"),
             "Summative with Quality distinction" = plotlyOutput("intervalQualityChart")
      )
    }
  })
  
  
  # dataTable is on the other tab
  output$dataTable <- renderDT({
    df <- filtered_data()
    
    df_display <- df %>%
      distinct(filename, .keep_all = TRUE) %>%
      select(-ambitus) %>% 
      left_join(ambitus_data, by = "filename") %>%
      mutate(
        max_pitch = ifelse(is.na(max_pitch), NA, max_pitch),
        min_pitch = ifelse(is.na(min_pitch), NA, min_pitch),
        ambitus = ifelse(is.na(ambitus), NA, ambitus)
      ) %>%
      select(
        `Song title` = OTL,
        `Region` = ARE,
        `Social function` = GTL,
        `Meter` = meters,
        `Scale` = AMD,
        `Basic tone` = key,
        `Scale degrees` = scl_value,
        `Max Pitch` = max_pitch,
        `Min Pitch` = min_pitch,
        `Ambitus` = ambitus,
        `URL scan` = URL.scan,
        filename
      )
    
    datatable(
      df_display,
      options = list(scrollX = TRUE, pageLength = 25),
      rownames = FALSE
    )
  })
  
  
  output$dataTable2 <- renderDT({
    df <- filtered_data2()
    
    df_display <- filtered_data2() %>%
      distinct(filename, .keep_all = TRUE) %>%
      select(-ambitus) %>% 
      left_join(ambitus_data, by = "filename") %>%
      mutate(
        max_pitch = ifelse(is.na(max_pitch), NA, max_pitch),
        min_pitch = ifelse(is.na(min_pitch), NA, min_pitch)
      ) %>%
      select(
        `Song title` = OTL,
        `Region` = ARE,
        `Social function` = GTL,
        `Meter` = meters,
        `Scale` = AMD,
        `Basic tone` = key,
        `Scale degrees` = scl_value,
        `Max Pitch` = max_pitch,
        `Min Pitch` = min_pitch,
        `Ambitus` = ambitus,
        `URL scan` = URL.scan,
        filename
      )
    
    datatable(
      df_display,
      options = list(scrollX = TRUE, pageLength = 25),
      rownames = FALSE
    )
  })
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("esac_filtered_chart_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Prepare data
      
      intervals1 <- interval_data() %>% filter(set == "Set 1") %>% select(-set)
      intervals2 <- interval_data() %>% filter(set == "Set 2") %>% select(-set)
      
      quality1 <- interval_quality_data() %>% filter(set == "Set 1") %>% select(-set)
      quality2 <- interval_quality_data() %>% filter(set == "Set 2") %>% select(-set)
      
      meters1 <- meters_distribution() %>% mutate(set = "Set 1")
      meters2 <- meters_distribution2() %>% mutate(set = "Set 2")
      
      tonal1 <- tonality_distribution() %>% mutate(set = "Set 1")
      tonal2 <- tonality_distribution2() %>% mutate(set = "Set 2")
      
      # Combine into named list for xlsx
      sheets <- list(
        Set1_Intervals = intervals1,
        Set2_Intervals = intervals2,
        Set1_Quality = quality1,
        Set2_Quality = quality2,
        Set1_Meters = meters1,
        Set2_Meters = meters2,
        Set1_Tonality = tonal1,
        Set2_Tonality = tonal2
      )
      
      write_xlsx(sheets, path = file)
    }
  )
  
  

}

shinyApp(ui, server)

