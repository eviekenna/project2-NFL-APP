#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(tidyverse)

# Define UI for application.
# app.R
library(shiny)
library(bslib)      # optional, just for nicer theme
library(DT)
library(tidyverse)

# --------- DATA LOADING ----------
# Expect nfl_09_18 to exist OR read it if a small demo file is present
if (!exists("nfl_09_18")) {
  # Replace with your own small CSV (don’t push the big ones to GitHub)
  # nfl_09_18 <- readr::read_csv("nfl_runpass_small.csv")
  validate <- function(...) NULL  # no-op if you haven't created a small file yet
}

# Helpers to identify column types
is_cat <- function(x) is.character(x) || is.factor(x) || is.logical(x)
is_num <- function(x) is.numeric(x) || inherits(x, "Date") || inherits(x, "POSIXt")

cat_vars <- names(Filter(is_cat, nfl_09_18))
num_vars <- names(Filter(is_num, nfl_09_18))

# --------- UI ----------
ui <- page_fluid(                       # bslib container (could use fluidPage)
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "NFL Play-by-Play Explorer",
  
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      h4("Filter data"),
      # --- Choose two categorical variables to subset by
      selectInput("cat1_var", "Categorical filter 1 (optional):", choices = c("None" = "", cat_vars)),
      uiOutput("cat1_levels_ui"),
      selectInput("cat2_var", "Categorical filter 2 (optional):", choices = c("None" = "", cat_vars)),
      uiOutput("cat2_levels_ui"),
      
      tags$hr(),
      
      # --- Pick two numeric variables; sliders appear dynamically
      selectInput("num1_var", "Numeric filter 1 (optional):", choices = c("None" = "", num_vars)),
      uiOutput("num1_range_ui"),
      selectInput("num2_var", "Numeric filter 2 (optional):", choices = c("None" = "", num_vars)),
      uiOutput("num2_range_ui"),
      
      tags$hr(),
      actionButton("apply_filters", "Apply filters", class = "btn btn-primary"),
      helpText("Data updates only when you click 'Apply filters'.")
    ),
    
    # --------- MAIN PANEL with tabs ----------
    card(
      card_header("NFL Play-by-Play App"),
      tabsetPanel(
        id = "tabs",
        
        tabPanel(
          title = "About",
          br(),
          h4("What this app does"),
          p("Explore NFL play-by-play data: filter, download subsets, and create EDA summaries/plots."),
          h4("Data source"),
          HTML('Data adapted from the nflfastR project. More info: 
               <a href="https://www.nflfastr.com" target="_blank">nflfastR</a>.'),
          h4("How to use the sidebar"),
          tags$ul(
            tags$li("Choose 0–2 categorical filters and (optionally) select levels."),
            tags$li("Choose up to 2 numeric variables; sliders will appear so you can set ranges."),
            tags$li("Click ", tags$strong("Apply filters"), " to update the app.")
          ),
          br(),
          tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/a/a2/National_Football_League_logo.svg",
                   height = 120, alt = "NFL logo")
        ),
        
        tabPanel(
          title = "Data Download",
          br(),
          DTOutput("data_table"),
          br(),
          downloadButton("download_csv", "Download filtered data")
        ),
        
        tabPanel(
          title = "Data Exploration",
          br(),
          # Top controls for exploration
          fluidRow(
            column(
              width = 12,
              radioButtons("eda_mode", "What do you want to explore?",
                           choices = c("Categorical summaries", "Numeric summaries"),
                           inline = TRUE)
            )
          ),
          conditionalPanel(
            condition = "input.eda_mode === 'Categorical summaries'",
            # One- and two-way tables + bar charts
            fluidRow(
              column(
                width = 4,
                selectInput("oneway_var", "One-way table variable:", choices = cat_vars)
              ),
              column(
                width = 8,
                tableOutput("oneway_table"),
                plotOutput("oneway_bar")
              )
            ),
            hr(),
            fluidRow(
              column(
                width = 4,
                selectInput("twoway_x", "Two-way table: Rows", choices = cat_vars, selected = "play_type")
              ),
              column(
                width = 4,
                selectInput("twoway_y", "Two-way table: Columns", choices = cat_vars, selected = "down")
              )
            ),
            fluidRow(
              column(width = 6, tableOutput("twoway_table")),
              column(width = 6, plotOutput("twoway_bar"))
            )
          ),
          conditionalPanel(
            condition = "input.eda_mode === 'Numeric summaries'",
            # Numeric stats grouped by a categorical var + plots
            fluidRow(
              column(
                width = 4,
                selectInput("num_summary_var", "Numeric variable:", choices = num_vars, selected = "yards_gained")
              ),
              column(
                width = 4,
                selectInput("group_var", "Group by (categorical):", choices = cat_vars, selected = "play_type")
              )
            ),
            fluidRow(
              column(width = 12, tableOutput("num_summary_table"))
            ),
            hr(),
            fluidRow(
              column(width = 6, plotOutput("hist_plot")),
              column(width = 6, plotOutput("box_plot"))
            )
          )
        )
      )
    )
  )
)

# --------- SERVER ----------
server <- function(input, output, session) {
  
  # --- Dynamic UI for categorical level pickers
  output$cat1_levels_ui <- renderUI({
    req(input$cat1_var)
    if (input$cat1_var == "") return(NULL)
    levs <- sort(unique(nfl_09_18[[input$cat1_var]]))
    selectizeInput("cat1_levels", "Choose levels for filter 1 (or leave blank for all):",
                   choices = levs, multiple = TRUE)
  })
  output$cat2_levels_ui <- renderUI({
    req(input$cat2_var)
    if (input$cat2_var == "") return(NULL)
    levs <- sort(unique(nfl_09_18[[input$cat2_var]]))
    selectizeInput("cat2_levels", "Choose levels for filter 2 (or leave blank for all):",
                   choices = levs, multiple = TRUE)
  })
  
  # --- Dynamic UI for numeric range sliders
  output$num1_range_ui <- renderUI({
    if (is.null(input$num1_var) || input$num1_var == "") return(NULL)
    v <- nfl_09_18[[input$num1_var]]
    rng <- range(v, na.rm = TRUE)
    sliderInput("num1_range", paste0("Range for ", input$num1_var),
                min = floor(rng[1]), max = ceiling(rng[2]),
                value = rng, step = diff(rng)/100)
  })
  output$num2_range_ui <- renderUI({
    if (is.null(input$num2_var) || input$num2_var == "") return(NULL)
    v <- nfl_09_18[[input$num2_var]]
    rng <- range(v, na.rm = TRUE)
    sliderInput("num2_range", paste0("Range for ", input$num2_var),
                min = floor(rng[1]), max = ceiling(rng[2]),
                value = rng, step = diff(rng)/100)
  })
  
  # --- Reactive filtered data (only updates when button is pressed)
  rv <- reactiveVal(nfl_09_18)
  
  observeEvent(input$apply_filters, {
    df <- nfl_09_18
    
    # cat filter 1
    if (!is.null(input$cat1_var) && input$cat1_var != "" && !is.null(input$cat1_levels) && length(input$cat1_levels) > 0) {
      df <- df %>% filter(.data[[input$cat1_var]] %in% input$cat1_levels)
    }
    
    # cat filter 2
    if (!is.null(input$cat2_var) && input$cat2_var != "" && !is.null(input$cat2_levels) && length(input$cat2_levels) > 0) {
      df <- df %>% filter(.data[[input$cat2_var]] %in% input$cat2_levels)
    }
    
    # num filter 1
    if (!is.null(input$num1_var) && input$num1_var != "" && !is.null(input$num1_range)) {
      df <- df %>% filter(between(.data[[input$num1_var]], input$num1_range[1], input$num1_range[2]))
    }
    
    # num filter 2
    if (!is.null(input$num2_var) && input$num2_var != "" && !is.null(input$num2_range)) {
      df <- df %>% filter(between(.data[[input$num2_var]], input$num2_range[1], input$num2_range[2]))
    }
    
    rv(df)
  }, ignoreInit = TRUE)
  
  # -------- Data Download tab --------
  output$data_table <- renderDT({
    datatable(rv(), options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("nfl_filtered_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(rv(), file)
  )
  
  # -------- Data Exploration: Categorical --------
  output$oneway_table <- renderTable({
    df <- rv(); req(input$oneway_var)
    tbl <- table(df[[input$oneway_var]], useNA = "ifany")
    as.data.frame(tbl)
  }, striped = TRUE, spacing = "xs")
  
  output$oneway_bar <- renderPlot({
    df <- rv(); req(input$oneway_var)
    ggplot(df, aes(x = .data[[input$oneway_var]])) +
      geom_bar(fill = "grey60") +
      labs(x = input$oneway_var, y = "Count", title = "One-way frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  output$twoway_table <- renderTable({
    df <- rv(); req(input$twoway_x, input$twoway_y)
    as.data.frame.matrix(table(df[[input$twoway_x]], df[[input$twoway_y]], useNA = "ifany"))
  }, striped = TRUE, spacing = "xs")
  
  output$twoway_bar <- renderPlot({
    df <- rv(); req(input$twoway_x, input$twoway_y)
    ggplot(df, aes(x = .data[[input$twoway_x]], fill = .data[[input$twoway_y]])) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = input$twoway_x, y = "Proportion", fill = input$twoway_y,
           title = "Two-way proportions") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  # -------- Data Exploration: Numeric --------
  output$num_summary_table <- renderTable({
    df <- rv(); req(input$num_summary_var, input$group_var)
    df %>%
      group_by(.data[[input$group_var]]) %>%
      summarise(
        n = sum(!is.na(.data[[input$num_summary_var]])),
        mean = mean(.data[[input$num_summary_var]], na.rm = TRUE),
        median = median(.data[[input$num_summary_var]], na.rm = TRUE),
        sd = sd(.data[[input$num_summary_var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(n))
  }, digits = 3)
  
  output$hist_plot <- renderPlot({
    df <- rv(); req(input$num_summary_var, input$group_var)
    ggplot(df, aes(x = .data[[input$num_summary_var]], fill = .data[[input$group_var]])) +
      geom_histogram(binwidth = diff(range(df[[input$num_summary_var]], na.rm = TRUE))/60,
                     position = "identity", alpha = 0.5) +
      labs(x = input$num_summary_var, y = "Count", fill = input$group_var,
           title = "Histogram by group") +
      theme_minimal()
  })
  
  output$box_plot <- renderPlot({
    df <- rv(); req(input$num_summary_var, input$group_var)
    ggplot(df, aes(x = .data[[input$group_var]], y = .data[[input$num_summary_var]], fill = .data[[input$group_var]])) +
      geom_boxplot(outlier.alpha = 0.15) +
      guides(fill = "none") +
      labs(x = input$group_var, y = input$num_summary_var, title = "Boxplot by group") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
