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
  dashboardHeader(title = "EsAC Deutsch | Subcollections Comparison | VERSION 2"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Data Table - Set 1", tabName = "fulltable", icon = icon("table")),
                menuItem("Data Table - Set 2", tabName = "fulltable2", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .small-box {
          height: 70px !important;
          width: 120px !important;
          margin-right: 10px !important;
          text-align: center;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          border-radius: 8px !important;
        }
        .small-box .inner h3 { font-size: 20px !important; margin: 0; padding: 0; }
        .small-box .inner p { font-size: 13px !important; margin: 0; padding: 0; }
        .small-box .icon { display: none !important; }
        .row .col-sm-3 {
          max-width: 160px !important;
          padding-left: 5px !important;
          padding-right: 5px !important;
        }
        .sidebar-section-header {
          padding: 6px 10px;
          margin-top: 10px;
          margin-bottom: 5px;
          font-weight: bold;
          font-size: 15px;
          background-color: #c7e4f4; /* Default blue-tinted */
          color: black;
          border-radius: 4px;
        }
      
        .sidebar-section-header-set2 {
          background-color: #f5c9c7; /* Light red-tinted */
          color: black;
        }

      "))
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 2,
                       tabBox(width = 12, id = "filterTabs", 
                              tabPanel("Set 1 Filters", div(style = "background-color: #E5F1FB; padding: 10px;",
                                                            div(class = "sidebar-section-header", "Metadata"),
                                                            selectizeInput("region1", "Region", choices = regions, multiple = TRUE),
                                                            selectizeInput("type1", "Social function", choices = types, multiple = TRUE),
                                                            div(class = "sidebar-section-header", "Musical traits"),
                                                            selectizeInput("scale_degrees1", "Scale Degrees", choices = scale_degrees_all, multiple = TRUE),
                                                            selectizeInput("tonality1", "Basic Tone", choices = tonalities, multiple = TRUE),
                                                            selectizeInput("meters1", "Meter", choices = meters, multiple = TRUE),
                                                            selectizeInput("rh_pattern1", "Rhythmic Pattern", choices = NULL, multiple = TRUE),
                                                            div(class = "sidebar-section-header", "Textual traits"),
                                                            selectizeInput("titles1", "Title", choices = titles, multiple = TRUE),
                                                            selectizeInput("incipit1", "Incipit", choices = NULL, multiple = TRUE),
                                                            textInput("lyrics_search1", "Search Lyrics", placeholder = "Enter word or phrase..."),
                                                            selectizeInput("sentiment1", "Select emotion (Top 3)", choices = NULL, multiple = TRUE),
                                                            selectizeInput("valence1", "Dominant Valence", choices = NULL)
                              )),
                              tabPanel("Set 2 Filters", div(style = "background-color: #FCE6E3; padding: 10px;",
                                                            div(class = "sidebar-section-header sidebar-section-header-set2", "Metadata"),
                                                            selectizeInput("region2", "Region", choices = regions, multiple = TRUE),
                                                            selectizeInput("type2", "Social function", choices = types, multiple = TRUE),
                                                            div(class = "sidebar-section-header sidebar-section-header-set2", "Musical traits"),
                                                            selectizeInput("scale_degrees2", "Scale Degrees", choices = scale_degrees_all, multiple = TRUE),
                                                            selectizeInput("tonality2", "Basic Tone", choices = tonalities, multiple = TRUE),
                                                            selectizeInput("meters2", "Meter", choices = meters, multiple = TRUE),
                                                            selectizeInput("rh_pattern2", "Rhythmic Pattern", choices = NULL, multiple = TRUE),
                                                            div(class = "sidebar-section-header sidebar-section-header-set2", "Textual traits"),
                                                            selectizeInput("titles2", "Title", choices = titles, multiple = TRUE),
                                                            selectizeInput("incipit2", "Incipit", choices = NULL, multiple = TRUE),
                                                            textInput("lyrics_search2", "Search Lyrics", placeholder = "Enter word or phrase..."),
                                                            selectizeInput("sentiment2", "Select emotion (Top 3)", choices = NULL, multiple = TRUE),
                                                            selectizeInput("valence2", "Dominant Valence", choices = NULL)
                              ))
                       ),
                       div(
                         style = "text-align: center; margin-top: 10px;",
                         downloadButton("download_excel", "Download Chart Data (.xlsx)")
                       )
                ),
                
                column(width = 10,
                       column(width = 5,
                              fluidRow(
                                valueBoxOutput("distinct_filenames", width = 3),
                                valueBoxOutput("max_pitch", width = 3),
                                valueBoxOutput("min_pitch", width = 3),
                                valueBoxOutput("ambitus", width = 3)
                              ),
                              fluidRow(
                                tabBox(width = 12,
                                       tabPanel("Meter", uiOutput("metersDistributionUI")),
                                       tabPanel("Basic Tone", uiOutput("tonalityDistributionUI"))
                                )
                              )
                       ),
                       column(width = 1),  # Spacer
                       column(width = 5,
                              fluidRow(
                                valueBoxOutput("distinct_filenames2", width = 3),
                                valueBoxOutput("max_pitch2", width = 3),
                                valueBoxOutput("min_pitch2", width = 3),
                                valueBoxOutput("ambitus2", width = 3)
                              ),
                              fluidRow(
                                tabBox(width = 12,
                                       tabPanel("Meter", uiOutput("metersDistributionUI2")),
                                       tabPanel("Basic Tone", uiOutput("tonalityDistributionUI2"))
                                )
                              )
                       ),
                       
                       fluidRow(
                         box(width = 11,
                             selectInput("chartType", "Chart Type", 
                                         choices = c("Interval Distribution", 
                                                     "Intervals with quality distinction", 
                                                     "Rhythm analysis", 
                                                     "Degree Transitions (Sankey)")
                             ),
                             uiOutput("selectedChart")
                         )
                       )
                )
              )
      ),
      
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
    )
  )
)




server <- function(input, output, session) {
  
  selected_interval <- reactiveVal(NULL)
  
  get_pie_data <- function(set_label) {
    interval_data() %>%
      filter(set == set_label) %>%
      group_by(interval_generic) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      mutate(percent = round(100 * count / sum(count), 1)) %>%
      filter(percent >= 1)
  }
  
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
    if (!is.null(input$meters1) && length(input$meters1) > 0) df <- df[df$meters %in% input$meters1, ]
    if (!is.null(input$tonality1) && length(input$tonality1) > 0) df <- df[df$key %in% input$tonality1, ]
    if (!is.null(input$titles1) && length(input$titles1) > 0) df <- df[df$OTL %in% input$titles1, ]
    if (!is.null(input$lyrics_search1) && input$lyrics_search1 != "") {
      matched <- matched_filenames()
      df <- df[df$filename %in% matched, ]
    }
    
    df
  })
  
  # A little helper: re-apply all of set1’s filters except the one named in `skip`
  get_filtered_data1 <- function(skip = NULL) {
    df <- data
    df$meters <- clean_meters(df$meters)
    
    if (!is.null(input$region1)   && skip != "region1")   df <- df[df$region %in% input$region1, ]
    if (!is.null(input$type1)     && skip != "type1")     df <- df[df$GTL_trimmed %in% input$type1, ]
    if (!is.null(input$tonality1) && skip != "tonality1") df <- df[df$key %in% input$tonality1, ]
    if (!is.null(input$ambitus1)  && skip != "ambitus1")  df <- df[df$ambitus %in% as.numeric(input$ambitus1), ]
    if (!is.null(input$meters1)   && skip != "meters1")   df <- df[df$meters %in% input$meters1, ]
    if (!is.null(input$titles1)   && skip != "titles1")   df <- df[df$OTL %in% input$titles1, ]
    if (!is.null(input$lyrics_search1) && skip != "lyrics_search1") {
      matched <- matched_filenames()
      df <- df[df$filename %in% matched, ]
    }
    if (!is.null(input$scale_degrees1) && skip != "scale_degrees1") {
      df <- df[as.character(df$scl_value) %in% input$scale_degrees1, ]
    }
    
    df
  }
  
  observe({
    ### region1 ###
    {
      current <- input$region1
      df_ok   <- get_filtered_data1(skip = "region1")
      choices <- sort( union(unique(df_ok$region),   current) )
      updateSelectizeInput(session, "region1",
                           choices = choices,
                           selected = current)
    }
    
    ### type1 (Social function) ###
    {
      current <- input$type1
      df_ok   <- get_filtered_data1(skip = "type1")
      choices <- sort( union(unique(df_ok$GTL_trimmed), current) )
      updateSelectizeInput(session, "type1",
                           choices = choices,
                           selected = current)
    }
    
    ### tonality1 ###
    {
      current <- input$tonality1
      df_ok   <- get_filtered_data1(skip = "tonality1")
      choices <- sort( union(unique(df_ok$key), current) )
      updateSelectizeInput(session, "tonality1",
                           choices = choices,
                           selected = current)
    }
    
    ### ambitus1 ###
    {
      current <- input$ambitus1
      df_ok   <- get_filtered_data1(skip = "ambitus1")
      choices <- sort( union(unique(df_ok$ambitus), current) )
      updateSelectizeInput(session, "ambitus1",
                           choices = choices,
                           selected = current)
    }
    
    ### meters1 ###
    {
      current <- input$meters1
      df_ok   <- get_filtered_data1(skip = "meters1")
      choices <- sort( union(unique(df_ok$meters), current) )
      updateSelectizeInput(session, "meters1",
                           choices = choices,
                           selected = current)
    }
    
    ### titles1 ###
    {
      current <- input$titles1
      df_ok   <- get_filtered_data1(skip = "titles1")
      choices <- sort( union(unique(df_ok$OTL), current) )
      updateSelectizeInput(session, "titles1",
                           choices = choices,
                           selected = current)
    }
    
    ### scale_degrees1 ###
    {
      current <- input$scale_degrees1
      df_ok   <- get_filtered_data1(skip = "scale_degrees1")
      choices <- sort( union(unique(as.character(df_ok$scl_value)), current) )
      updateSelectizeInput(session, "scale_degrees1",
                           choices = choices,
                           selected = current)
    }
    
  })
  
  
  
  filtered_for_choices2 <- reactive({
    df <- data
    df$meters <- clean_meters(df$meters)
    
    if (!is.null(input$region2) && length(input$region2) > 0) df <- df[df$region %in% input$region2, ]
    if (!is.null(input$type2) && length(input$type2) > 0) df <- df[df$GTL_trimmed %in% input$type2, ]
    if (!is.null(input$meters2) && length(input$meters2) > 0) df <- df[df$meters %in% input$meters2, ]
    if (!is.null(input$tonality2) && length(input$tonality2) > 0) df <- df[df$key %in% input$tonality2, ]
    if (!is.null(input$titles2) && length(input$titles2) > 0) df <- df[df$OTL %in% input$titles2, ]
    if (!is.null(input$lyrics_search2) && input$lyrics_search2 != "") {
      matched <- matched_filenames2()
      df <- df[df$filename %in% matched, ]
    }
    
    df
  })
  
  get_filtered_data2 <- function(skip = NULL) {
    df <- data
    df$meters <- clean_meters(df$meters)
    
    if (!is.null(input$region2)   && skip != "region2")   df <- df[df$region %in% input$region2, ]
    if (!is.null(input$type2)     && skip != "type2")     df <- df[df$GTL_trimmed %in% input$type2, ]
    if (!is.null(input$tonality2) && skip != "tonality2") df <- df[df$key %in% input$tonality2, ]
    if (!is.null(input$ambitus2)  && skip != "ambitus2")  df <- df[df$ambitus %in% as.numeric(input$ambitus2), ]
    if (!is.null(input$meters2)   && skip != "meters2")   df <- df[df$meters %in% input$meters2, ]
    if (!is.null(input$titles2)   && skip != "titles2")   df <- df[df$OTL %in% input$titles2, ]
    if (!is.null(input$lyrics_search2) && skip != "lyrics_search2") {
      matched <- matched_filenames()
      df <- df[df$filename %in% matched, ]
    }
    if (!is.null(input$scale_degrees2) && skip != "scale_degrees2") {
      df <- df[as.character(df$scl_value) %in% input$scale_degrees2, ]
    }
    
    df
  }
  
  observe({
    ### region2 ###
    {
      current <- input$region2
      df_ok   <- get_filtered_data2(skip = "region2")
      choices <- sort( union(unique(df_ok$region),   current) )
      updateSelectizeInput(session, "region2",
                           choices = choices,
                           selected = current)
    }
    
    ### type2 (Social function) ###
    {
      current <- input$type2
      df_ok   <- get_filtered_data2(skip = "type2")
      choices <- sort( union(unique(df_ok$GTL_trimmed), current) )
      updateSelectizeInput(session, "type2",
                           choices = choices,
                           selected = current)
    }
    
    ### tonality2 ###
    {
      current <- input$tonality2
      df_ok   <- get_filtered_data2(skip = "tonality2")
      choices <- sort( union(unique(df_ok$key), current) )
      updateSelectizeInput(session, "tonality2",
                           choices = choices,
                           selected = current)
    }
    
    ### ambitus2 ###
    {
      current <- input$ambitus2
      df_ok   <- get_filtered_data2(skip = "ambitus2")
      choices <- sort( union(unique(df_ok$ambitus), current) )
      updateSelectizeInput(session, "ambitus2",
                           choices = choices,
                           selected = current)
    }
    
    ### meters2 ###
    {
      current <- input$meters2
      df_ok   <- get_filtered_data2(skip = "meters2")
      choices <- sort( union(unique(df_ok$meters), current) )
      updateSelectizeInput(session, "meters2",
                           choices = choices,
                           selected = current)
    }
    
    ### titles2 ###
    {
      current <- input$titles2
      df_ok   <- get_filtered_data2(skip = "titles2")
      choices <- sort( union(unique(df_ok$OTL), current) )
      updateSelectizeInput(session, "titles2",
                           choices = choices,
                           selected = current)
    }
    
    ### scale_degrees2 ###
    {
      current <- input$scale_degrees2
      df_ok   <- get_filtered_data2(skip = "scale_degrees2")
      choices <- sort( union(unique(as.character(df_ok$scl_value)), current) )
      updateSelectizeInput(session, "scale_degrees2",
                           choices = choices,
                           selected = current)
    }
    
    ### lyrics_search2 ###
    # for a textInput you can leave as is, since it's free text
  })
  
  
  note_filter_choices1 <- reactive({
    df <- rhythm_chart_data()
    note_names <- unique(df$note_length_name[df$set == "Set 1"])
    available <- note_length_order[note_length_order %in% note_names]
    setNames(available, sapply(available, get_note_label))
  })
  
  
  note_filter_choices2 <- reactive({
    df <- rhythm_chart_data()
    note_names <- unique(df$note_length_name[df$set == "Set 2"])
    available <- note_length_order[note_length_order %in% note_names]
    setNames(available, sapply(available, get_note_label))
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
    # Meters
    if (!is.null(input$meters1) && length(input$meters1) > 0) {
      df <- df[df$meters %in% input$meters1, ]
    }
    # Tonality
    if (!is.null(input$tonality1) && length(input$tonality1) > 0) {
      df <- df[!is.na(df$key) & df$key %in% input$tonality1, ]
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
    if (!is.null(input$meters2) && length(input$meters2) > 0) {
      df <- df[df$meters %in% input$meters2, ]
    }
    if (!is.null(input$tonality2) && length(input$tonality2) > 0) {
      df <- df[df$key %in% input$tonality2, ]
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
  
  source_degrees_set1 <- reactive({
    df <- filtered_data()
    if (!"degree" %in% names(df)) return(NULL)
    
    df <- df |>
      mutate(source_degree = gsub("^[\\^v]+", "", degree)) |>
      filter(!is.na(source_degree)) |>
      pull(source_degree) |>
      unique() |>
      sort()
    
    df
  })
  
  observe({
    req(input$chartType == "Degree Transitions (Sankey)")  # ⬅️ ensures the input exists
    
    degrees <- source_degrees_set1()
    
    if (is.null(degrees) || length(degrees) == 0) return()
    
    updateSelectInput(
      session,
      "selected_source_degree1",
      choices = degrees,
      selected = degrees[1]
    )
  })

  
  sankey_data_set1 <- reactive({
    req(input$selected_source_degree1)
    
    df <- filtered_data() |>
      arrange(filename, id) |>
      mutate(source_degree = gsub("^[\\^v]+", "", degree)) |>
      group_by(filename) |>
      mutate(target_degree = lead(source_degree)) |>
      ungroup() |>
      filter(
        !is.na(source_degree),
        !is.na(target_degree),
        source_degree == input$selected_source_degree1
      ) |>
      count(source_degree, target_degree, name = "value")
    
    df
  })
  
  source_degrees_set2 <- reactive({
    df <- filtered_data2()
    if (!"degree" %in% names(df)) return(NULL)
    
    df <- df |>
      mutate(source_degree = gsub("^[\\^v]+", "", degree)) |>
      filter(!is.na(source_degree)) |>
      pull(source_degree) |>
      unique() |>
      sort()
    
    df
  })
  
  observe({
    req(input$chartType == "Degree Transitions (Sankey)")
    
    degrees <- source_degrees_set2()
    
    if (is.null(degrees) || length(degrees) == 0) return()
    
    updateSelectInput(
      session,
      "selected_source_degree2",
      choices = degrees,
      selected = degrees[1]
    )
  })
  
  sankey_data_set2 <- reactive({
    req(input$selected_source_degree2)
    
    df <- filtered_data2() |>
      arrange(filename, id) |>
      mutate(source_degree = gsub("^[\\^v]+", "", degree)) |>
      group_by(filename) |>
      mutate(target_degree = lead(source_degree)) |>
      ungroup() |>
      filter(
        !is.na(source_degree),
        !is.na(target_degree),
        source_degree == input$selected_source_degree2
      ) |>
      count(source_degree, target_degree, name = "value")
    
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
  
  
  note_length_order <- c(
    "semiquaver", "quaver", "dotted quaver",  "crotchet", 
    "dotted crotchet", "minim", "dotted minim", "semibreve"
  )
  
  interval_order <- c(
    "unison", "minor second", "major second",
    "minor third", "major third",
    "perfect fourth", "tritone", "perfect fifth",
    "minor sixth", "major sixth",
    "minor seventh", "major seventh", "octave"
  )
  
  consonant_labels <- c(
    "unison", "minor third", "major third",
    "perfect fourth", "perfect fifth", "major sixth", "octave"
  )
  
  consonant_set <- c(
    "unison", "minor third", "major third",
    "perfect fourth", "perfect fifth",
    "minor sixth", "major sixth", "octave"
  )
  
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
    
    combined <- combined %>%
      mutate(
        interval_generic = sub(".* ", "", interval_name),
        consonance_group = ifelse(as.character(interval_name) %in% consonant_labels, "Consonant", "Dissonant"),
        interval_name = factor(interval_name, levels = interval_order),
        direction = factor(direction, levels = c("up", "down"))
      )
    
    grouped <- combined %>%
      group_by(interval_name, interval_generic, interval_numerical, direction, consonance_group, set) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(set) %>%
      mutate(
        signed_count = ifelse(direction == "down", -count, count),
        percent = round(count / sum(count) * 100, 1)
      ) %>%
      ungroup()
    
    grouped
  })
  
  
  rhythm_chart_data <- reactive({
    
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
  
  pie_data_set1 <- reactive({
    get_pie_data("Set 1")
  })
  
  output$intervalChartSet1 <- renderPlotly({
    df_summary <- pie_data_set1()
    
    p <- plot_ly(
      df_summary,
      labels = ~interval_generic,
      values = ~count,
      type = "pie",
      source = "set1_pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste(interval_generic, ": ", percent, "% (", count, ")"),
      domain = list(x = c(0.05, 0.95), y = c(0.05, 0.95))
    ) %>%
      layout(title = "Set 1 Intervals",
             showlegend = FALSE,
             margin = list(l = 40, r = 40, b = 40, t = 50) 
             )
     
    plotly::event_register(p, "plotly_click")
    
  })
  
  observeEvent(req(plotly::event_data("plotly_click", source = "set1_pie")), {
    event <- plotly::event_data("plotly_click", source = "set1_pie")
    df <- pie_data_set1()
    
    if (!is.null(event$pointNumber)) {
      clicked_label <- as.character(df$interval_generic[event$pointNumber + 1])
      
      if (identical(selected_interval(), clicked_label)) {
        selected_interval(NULL)  # toggle off
      } else {
        selected_interval(clicked_label)
      }
    }
  })
  
  
  filtered_quality_set1 <- reactive({
    df <- interval_quality_data() %>%
      filter(set == "Set 1")
    
    if (!is.null(selected_interval())) {
      df <- df %>% filter(interval_generic == selected_interval())
    }
    
    total <- sum(df$count, na.rm = TRUE)
    
    df <- df %>%
      mutate(
        signed_count = ifelse(direction == "down", -count, count),
        percent = 100 * count / total
      ) %>%
      filter(percent >= 1)
    
    df$interval_name <- factor(df$interval_name, levels = unique(df$interval_name))
    
    df
  })
  
 
  pie_data_set2 <- reactive({
    get_pie_data("Set 2")
  })

  output$intervalChartSet2 <- renderPlotly({
    df_summary <- pie_data_set2()
    
    p <- plot_ly(
      df_summary,
      labels = ~interval_generic,
      values = ~count,
      type = "pie",
      source = "set2_pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste(interval_generic, ": ", percent, "% (", count, ")"),
      domain = list(x = c(0.05, 0.95), y = c(0.05, 0.95))
    ) %>%
      layout(title = "Set 2 Intervals",
             showlegend = FALSE,
             margin = list(l = 40, r = 40, b = 40, t = 50)
             )

      plotly::event_register(p, "plotly_click")
  })
  
  observeEvent(req(plotly::event_data("plotly_click", source = "set2_pie")), {
    event <- plotly::event_data("plotly_click", source = "set2_pie")
    df <- pie_data_set2()
    
    if (!is.null(event$pointNumber)) {
      clicked_label <- as.character(df$interval_generic[event$pointNumber + 1])
      
      if (identical(selected_interval(), clicked_label)) {
        selected_interval(NULL)  # toggle off
      } else {
        selected_interval(clicked_label)
      }
    }
  })
  
  filtered_quality_set2 <- reactive({
    df <- interval_quality_data() %>%
      filter(set == "Set 2")
    
    if (!is.null(selected_interval())) {
      df <- df %>% filter(interval_generic == selected_interval())
    }
    
    total <- sum(df$count, na.rm = TRUE)
    
    df <- df %>%
      mutate(
        signed_count = ifelse(direction == "down", -count, count),
        percent = 100 * count / total
      ) %>%
      filter(percent >= 1)
    
    df$interval_name <- factor(df$interval_name, levels = unique(df$interval_name))
    
    df
  })
  
  combined_detail_quality <- reactive({
    df1 <- filtered_quality_set1()
    df2 <- filtered_quality_set2()
    
    if (nrow(df1) == 0 && nrow(df2) == 0) return(NULL)
    
    df1$set <- "Set 1"
    df2$set <- "Set 2"
    
    df <- bind_rows(df1, df2)
    
  })
  
  
  output$combinedDetailChart <- renderPlotly({
    df <- combined_detail_quality()
    req(df)
    
    plot_ly(
      df,
      x = ~interval_name,
      y = ~signed_count,
      color = ~interaction(set, direction, lex.order = TRUE),
      colors = c(
        "Set 1.up" = "steelblue", "Set 1.down" = "lightblue",
        "Set 2.up" = "salmon", "Set 2.down" = "lightcoral"
      ),
      type = "bar",
      text = ~paste0("Set: ", set,
                     "<br>Interval: ", interval_name,
                     "<br>Direction: ", direction,
                     "<br>Count: ", count,
                     "<br>Percent: ", sprintf("%.1f", percent), "%"),
      textposition = "none",
      hoverinfo = "text"
    ) %>%
      layout(
        title = if (is.null(selected_interval())) {
          "Select an interval to see detailed view"
        } else {
          NULL
        },
        barmode = "group",  # 👈 side-by-side grouping here
        xaxis = list(title = "Interval Name"),
        yaxis = list(title = "Signed Count"),
        legend = list(title = list(text = "Set + Direction"))
      )
  })
  
  
  output$intervalQualityChartSet1 <- renderPlotly({
    df <- interval_quality_data() %>%
      filter(set == "Set 1") %>%
      mutate(
        consonance_group = ifelse(as.character(interval_name) %in% consonant_labels, "Consonant", "Dissonant"),
        interval_name = factor(interval_name, levels = intersect(interval_order, unique(interval_name))),
        direction = factor(direction, levels = c("up", "down"))
      )%>%
      filter(percent >= 1, count > 0)
    
    consonant_df <- df %>% filter(consonance_group == "Consonant")
    dissonant_df <- df %>% filter(consonance_group == "Dissonant")
    
    p1 <- plot_ly(
      consonant_df,
      x = ~interval_name,
      y = ~signed_count,
      type = "bar",
      color = ~direction,
      colors = c("up" = "steelblue", "down" = "lightblue"),
      text = ~paste0(
        "Interval: ", interval_name,
        "<br>Direction: ", direction,
        "<br>Count: ", count,
        "<br>Percent: ", round(percent, 1), "%"
      ),
      textposition = "none",
      hoverinfo = "text",
      showlegend = TRUE
    )
    
    p2 <- plot_ly(
      dissonant_df,
      x = ~interval_name,
      y = ~signed_count,
      type = "bar",
      color = ~direction,
      colors = c("up" = "steelblue", "down" = "lightblue"),
      text = ~paste0(
        "Interval: ", interval_name,
        "<br>Direction: ", direction,
        "<br>Count: ", count,
        "<br>Percent: ", round(percent, 1), "%"
      ),
      textposition = "none",
      hoverinfo = "text",
      showlegend = FALSE
    )
    
    subplot(p1, p2, nrows = 1, shareY = TRUE, titleX = TRUE) %>%
      layout(
        title = "Set 1 – Intervals with quality distinction",
        barmode = "relative",
        xaxis = list(
          title = "Consonant",
          categoryorder = "array",
          categoryarray = unique(as.character(consonant_df$interval_name))
        ),
        xaxis2 = list(
          title = "Dissonant",
          categoryorder = "array",
          categoryarray = unique(as.character(dissonant_df$interval_name))
        ),
        yaxis = list(title = "Signed Count")
      )
    
  })
  
  
  output$intervalQualityChartSet2 <- renderPlotly({
    df <- interval_quality_data() %>%
      filter(set == "Set 2") %>%
      mutate(
        consonance_group = ifelse(as.character(interval_name) %in% consonant_labels, "Consonant", "Dissonant"),
        interval_name = factor(interval_name, levels = intersect(interval_order, unique(interval_name))),
        direction = factor(direction, levels = c("up", "down"))
      )%>%
      filter(percent >= 1, count > 0)
    
    consonant_df <- df %>% filter(consonance_group == "Consonant")
    dissonant_df <- df %>% filter(consonance_group == "Dissonant")
    
    p1 <- plot_ly(
      consonant_df,
      x = ~interval_name,
      y = ~signed_count,
      type = "bar",
      color = ~direction,
      colors = c("up" = "salmon", "down" = "lightcoral"),
      text = ~paste0(
        "Interval: ", interval_name,
        "<br>Direction: ", direction,
        "<br>Count: ", count,
        "<br>Percent: ", round(percent, 1), "%"
      ),
      textposition = "none",
      hoverinfo = "text",
      showlegend = TRUE
    )
    
    p2 <- plot_ly(
      dissonant_df,
      x = ~interval_name,
      y = ~signed_count,
      type = "bar",
      color = ~direction,
      colors = c("up" = "salmon", "down" = "lightcoral"),
      text = ~paste0(
        "Interval: ", interval_name,
        "<br>Direction: ", direction,
        "<br>Count: ", count,
        "<br>Percent: ", round(percent, 1), "%"
      ),
      textposition = "none",
      hoverinfo = "text",
      showlegend = FALSE
    )
    
    subplot(p1, p2, nrows = 1, shareY = TRUE, titleX = TRUE) %>%
      layout(
        title = "Set 2 – Intervals with quality distinction",
        barmode = "relative",
        xaxis = list(
          title = "Consonant",
          categoryorder = "array",
          categoryarray = unique(as.character(consonant_df$interval_name))
        ),
        xaxis2 = list(
          title = "Dissonant",
          categoryorder = "array",
          categoryarray = unique(as.character(dissonant_df$interval_name))
        ),
        yaxis = list(title = "Signed Count")
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
  
  rhythm_chart_data_filtered1 <- reactive({
    df <- rhythm_chart_data()
    df1 <- df[df$set == "Set 1", ]
    
    if (!is.null(input$note_symbol_filter1) && length(input$note_symbol_filter1) > 0) {
      df1 <- df1[df1$note_length_name %in% input$note_symbol_filter1, ]
    }
    
    df1 %>%
      mutate(
        percent = round(100 * count / sum(count), 2),
        label = sapply(note_length_name, get_note_label),
        symbol = sapply(note_length_name, function(x) {
          val <- note_unicode_map[[tolower(x)]]
          if (is.null(val)) NA_character_ else val
        }, USE.NAMES = FALSE)
      ) %>%
      filter(!is.na(symbol))
  })
  
  rhythm_chart_data_filtered2 <- reactive({
    df <- rhythm_chart_data()
    df2 <- df[df$set == "Set 2", ]
    
    if (!is.null(input$note_symbol_filter2) && length(input$note_symbol_filter2) > 0) {
      df2 <- df2[df2$note_length_name %in% input$note_symbol_filter2, ]
    }
    
    df2 %>%
      mutate(
        percent = round(100 * count / sum(count), 2),
        label = sapply(note_length_name, get_note_label),
        symbol = sapply(note_length_name, function(x) {
          val <- note_unicode_map[[tolower(x)]]
          if (is.null(val)) NA_character_ else val
        }, USE.NAMES = FALSE)
      ) %>%
      filter(!is.na(symbol))
  })
  
  
  output$rhythmChart1 <- renderPlotly({
    df1 <- rhythm_chart_data_filtered1()
    if (nrow(df1) == 0) return(NULL)
    
    plot_ly(
      df1,
      x = ~symbol,
      y = ~percent,
      type = "bar",
      marker = list(color = "steelblue"),
      text = ~paste0(label, "<br>Percent: ", percent, "%<br>Count: ", count),
      textposition = "none",
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Set 1: Rhythmic Note Distribution",
        xaxis = list(title = "Note Symbol"),
        yaxis = list(title = "Count")
      )
  })
  
  output$rhythmChart2 <- renderPlotly({
    df2 <- rhythm_chart_data_filtered2()
    
    if (nrow(df2) == 0) return(NULL)
    
    plot_ly(
      df2,
      x = ~symbol,
      y = ~percent,
      type = "bar",
      marker = list(color = "salmon"),
      text = ~paste0(label, "<br>Percent: ", percent, "%<br>Count: ", count),
      textposition = "none",
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Set 2: Rhythmic Note Distribution",
        xaxis = list(title = "Note Symbol"),
        yaxis = list(title = "Count")
      )
  })
  
  output$sankeyChart1 <- renderPlotly({
    df <- sankey_data_set1()
    if (nrow(df) == 0) return(NULL)
    
    nodes <- unique(c(df$source_degree, df$target_degree))
    df$source_id <- match(df$source_degree, nodes) - 1
    df$target_id <- match(df$target_degree, nodes) - 1
    
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(label = nodes),
      link = list(source = df$source_id, target = df$target_id, value = df$value)
    ) |>
      layout(title = "Set 1 – Degree Transitions (Sankey)")
  })
  
  output$sankeyChart2 <- renderPlotly({
    df <- sankey_data_set2()
    if (nrow(df) == 0) return(NULL)
    
    nodes <- unique(c(df$source_degree, df$target_degree))
    df$source_id <- match(df$source_degree, nodes) - 1
    df$target_id <- match(df$target_degree, nodes) - 1
    
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(label = nodes),
      link = list(source = df$source_id, target = df$target_id, value = df$value)
    ) |>
      layout(title = "Set 2 – Degree Transitions (Sankey)")
  })
  
  
  observeEvent(input$reset_interval, {
    selected_interval(NULL)
  })
  
  
  observe({
    if (is.null(selected_interval())) {
      shinyjs::hide("reset_interval")
    } else {
      shinyjs::show("reset_interval")
    }
  })
  
  group_by_consonance1 <- reactiveVal(FALSE)
  group_by_consonance2 <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_grouping1, {
    group_by_consonance1(!group_by_consonance1())
  })
  
  observeEvent(input$toggle_grouping2, {
    group_by_consonance2(!group_by_consonance2())
  })
  
  
  observe({
    df <- filtered_data2()
    
    if (!"degree" %in% names(df)) return()
    
    degrees <- gsub("^[\\^v]+", "", df$degree)
    degrees <- na.omit(degrees)
    degrees <- unique(degrees)
    degrees <- sort(degrees)
    
    updateSelectInput(session, "selected_source_degree2",
                      choices = if (length(degrees) > 0) degrees else c("No valid degrees" = ""),
                      selected = if ("1" %in% degrees) "1" else degrees[1]
    )
  })
  

  output$selectedChart <- renderUI({
    chart_type <- input$chartType
    
    if (chart_type == "Interval Distribution") {
      tagList(
        fluidRow(
          column(6,
                 plotlyOutput("intervalChartSet1", height = "300px"),
                 tags$div(style = "height: 40px;",
                          shinyjs::hidden(
                            actionButton("reset_interval", "Reset Selection", icon = icon("undo"))
                          )
                 )
          ),
          column(6,
                 plotlyOutput("intervalChartSet2", height = "300px")
          )
        ),
        fluidRow(
          column(12, plotlyOutput("combinedDetailChart"))
        )
      )
    } else if (chart_type == "Intervals with quality distinction") {
      fluidRow(
        column(6, plotlyOutput("intervalQualityChartSet1")),
        column(6, plotlyOutput("intervalQualityChartSet2"))
      )
    } else if (chart_type == "Rhythm analysis") {
      
      fluidRow(
        column(6,
               selectizeInput(
                 "note_symbol_filter1",
                 label = "Set 1: Filter by Note Symbol",
                 choices = note_filter_choices1(),
                 multiple = TRUE,
                 selected = NULL,
                 options = list(placeholder = "Select symbols...")
               ),
               plotlyOutput("rhythmChart1")
        ),
        column(6,
               selectizeInput(
                 "note_symbol_filter2",
                 label = "Set 2: Filter by Note Symbol",
                 choices = note_filter_choices2(),
                 multiple = TRUE,
                 selected = NULL,
                 options = list(placeholder = "Select symbols...")
               ),
               plotlyOutput("rhythmChart2")
        )
      )
    } else if (chart_type == "Degree Transitions (Sankey)") {
      fluidRow(
        column(6,
               selectInput(
                 inputId = "selected_source_degree1",
                 label = "Set 1: Select Source Degree",
                 choices = NULL
               ),
               plotlyOutput("sankeyChart1")
        ),
        column(6,
               selectInput(
                 inputId = "selected_source_degree2",
                 label = "Set 2: Select Source Degree",
                 choices = NULL
               ),
               plotlyOutput("sankeyChart2")
        )
      )
    } else {
      span("No chart selected.")
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
      # Interval pie chart (summarized)
      intervals1 <- interval_data() %>%
        filter(set == "Set 1") %>%
        group_by(interval_generic) %>%
        summarise(count = sum(count), .groups = "drop") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      intervals2 <- interval_data() %>%
        filter(set == "Set 2") %>%
        group_by(interval_generic) %>%
        summarise(count = sum(count), .groups = "drop") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      # Interval Quality w/ direction
      quality1 <- interval_quality_data() %>%
        filter(set == "Set 1") %>%
        group_by(interval_name, direction) %>%
        summarise(count = sum(count), .groups = "drop") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      quality2 <- interval_quality_data() %>%
        filter(set == "Set 2") %>%
        group_by(interval_name, direction) %>%
        summarise(count = sum(count), .groups = "drop") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      # Note length bar charts
      bar1 <- rhythm_chart_data() %>%
        filter(set == "Set 1") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      bar2 <- rhythm_chart_data() %>%
        filter(set == "Set 2") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      # Export sheets
      sheets <- list(
        Set1_Intervals = intervals1,
        Set2_Intervals = intervals2,
        Set1_Quality_Direction = quality1,
        Set2_Quality_Direction = quality2,
        Set1_Rhythm = bar1,
        Set2_Rhythm = bar2
      )
      
      writexl::write_xlsx(sheets, path = file)
    }
  )
  

}

shinyApp(ui, server)

