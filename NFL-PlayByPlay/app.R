#This app allows the user to subset from their chosen catagorical/numerical variables and explore the data. 
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rlang)
library(stringr)
library(shinycssloaders)
library(DT)
library(readr)

# --------- DATA LOADING ----------
# Load the NFL data
# Make sure nfl_09_18 exists in your environment before running
# You can load it with: nfl_09_18 <- read_csv("NFL Play by Play 2009-2018 (v5).csv")

if (!exists("nfl_09_18")) {
  stop("Please load nfl_09_18 data before running the app")
}

# Add season column if not already present
if (!"season" %in% colnames(nfl_09_18)) {
  nfl_09_18 <- nfl_09_18 %>%
    mutate(season = year(game_date))
}

# Helpers to identify column types
is_cat <- function(x) is.character(x) || is.factor(x) || is.logical(x)
is_num <- function(x) is.numeric(x) || inherits(x, "Date") || inherits(x, "POSIXt")

# ----- Allowed variables for the app -----
# Categorical filters the user can subset on:
ALLOWED_CAT_FILTERS <- c("play_type", "down")

# Numeric filters the user can subset on:
ALLOWED_NUM_FILTERS <- c("yards_gained", "yardline_100", "season", "score_differential")

# For numeric summaries:
ALLOWED_NUM_SUMMARY_VARS <- c("yards_gained", "score_differential")
ALLOWED_GROUP_VARS <- c("play_type", "down", "season")

# --------- UI ----------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "NFL Play-by-Play Explorer",
  
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      h4("Filter Data"),
      
      # --- Categorical filter 1: Play Type
      h5("Play Type Filter"),
      checkboxGroupInput(
        "play_type_filter",
        "Select play types to include:",
        choices = c("pass", "run", "punt", "field_goal", "kickoff", "extra_point", "qb_kneel", "qb_spike"),
        selected = c("pass", "run")
      ),
      
      tags$hr(),
      
      # --- Categorical filter 2: Down
      h5("Down Filter"),
      checkboxGroupInput(
        "down_filter",
        "Select downs to include:",
        choices = c("1", "2", "3", "4"),
        selected = c("1", "2", "3", "4")
      ),
      
      tags$hr(),
      
      # --- Numeric filter 1
      selectInput("num1_var", 
                  "Numeric filter 1 (optional):", 
                  choices = c("None" = "", num_vars),
                  selected = ""),
      uiOutput("num1_range_ui"),
      
      # --- Numeric filter 2
      selectInput("num2_var", 
                  "Numeric filter 2 (optional):", 
                  choices = c("None" = "", num_vars),
                  selected = ""),
      uiOutput("num2_range_ui"),
      
      tags$hr(),
      actionButton("apply_filters", "Apply Filters", class = "btn btn-primary btn-lg"),
      br(), br(),
      helpText("Click 'Apply Filters' to update the data based on your selections.")
    ),
    
    # --------- MAIN PANEL with tabs ----------
    card(
      card_header("NFL Play-by-Play Explorer (2009-2018)"),
      navset_tab(
        id = "tabs",
        
        # ========== ABOUT TAB ==========
        nav_panel(
          title = "About",
          br(),
          h3("Purpose of This App"),
          p("This Shiny application allows you to explore NFL play-by-play data from the 2009-2018 seasons. 
            You can filter the data based on play types, downs, and numeric variables, download subsets of the data, 
            and create exploratory data analysis (EDA) summaries including tables and visualizations."),
          
          h3("Data Source"),
          p("The data used in this app comes from the ", 
            tags$strong("nflfastR project"), 
            ", which provides detailed play-by-play data for NFL games. The dataset contains information about 
            every play from the 2009-2018 NFL seasons."),
          p("Each row represents a single play in an NFL game, with information about the teams, players, 
            play type, yards gained, down, quarter, and much more."),
          p("For more information about the data and the nflfastR project, visit:"),
          tags$ul(
            tags$li(tags$a(href = "https://www.nflfastr.com/", target = "_blank", "nflfastR Official Website")),
            tags$li(tags$a(href = "https://github.com/nflverse/nflfastR", target = "_blank", "nflfastR GitHub Repository"))
          ),
          
          h3("How to Use This App"),
          h4("Sidebar - Filter Controls"),
          p("Use the sidebar on the left to filter the dataset:"),
          tags$ul(
            tags$li(tags$strong("Play Type Filter:"), " Select which types of plays to include (pass, run, punt, field goal, etc.). 
                    By default, only pass and run plays are selected."),
            tags$li(tags$strong("Down Filter:"), " Select which downs to include in your analysis (1st, 2nd, 3rd, or 4th down). 
                    All downs are selected by default."),
            tags$li(tags$strong("Numeric Filters:"), " Optionally select up to two numeric variables (like yards_gained, quarter, or season) 
                    and use the sliders to specify the range of values to include."),
            tags$li(tags$strong("Apply Filters Button:"), " After making your selections, click this button to update 
                    all visualizations and tables with the filtered data. The app will NOT update until you click this button.")
          ),
          
          h4("Main Panel - Tabs"),
          tags$ul(
            tags$li(tags$strong("About Tab:"), " This tab provides information about the app, data source, and usage instructions 
                    (you are currently viewing this tab)."),
            tags$li(tags$strong("Data Download Tab:"), " View the filtered dataset in an interactive table. You can search, sort, 
                    and navigate through the data. You can also download the filtered data as a CSV file for further analysis 
                    in other programs like Excel or R."),
            tags$li(tags$strong("Data Exploration Tab:"), " Create various summaries and visualizations of your filtered data:"),
            tags$ul(
              tags$li(tags$strong("Categorical Summaries:"), " Create one-way and two-way contingency tables to see frequency 
                      distributions and relationships between categorical variables. Bar charts visualize these relationships."),
              tags$li(tags$strong("Numeric Summaries:"), " Calculate summary statistics (mean, median, standard deviation, etc.) 
                      for numeric variables grouped by categorical variables. View the distribution using histograms and boxplots.")
            )
          ),
          
          br(),
          div(
            style = "text-align: center;",
            tags$img(
              src = "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/National_Football_League_logo.svg/640px-National_Football_League_logo.svg.png",
              height = 150, 
              alt = "NFL Logo",
              style = "margin: 20px;"
            )
          ),
          br()
        ),
        
        # ========== DATA DOWNLOAD TAB ==========
        nav_panel(
          title = "Data Download",
          br(),
          h4("Filtered Dataset"),
          p("The table below shows the data based on your current filter selections. 
            You can sort columns by clicking the column headers, search for specific values using the search box, 
            and navigate through pages using the controls at the bottom."),
          p(tags$strong("Current dataset size:"), textOutput("data_size", inline = TRUE), " rows"),
          br(),
          DTOutput("data_table"),
          br(),
          downloadButton("download_csv", "Download Filtered Data as CSV", class = "btn btn-success"),
          br(), br()
        ),
        
        # ========== DATA EXPLORATION TAB ==========
        nav_panel(
          title = "Data Exploration",
          br(),
          
          # Radio buttons to choose exploration mode
          fluidRow(
            column(
              width = 12,
              radioButtons("eda_mode", 
                           "What would you like to explore?",
                           choices = c("Categorical Summaries", "Numeric Summaries"),
                           inline = TRUE,
                           selected = "Categorical Summaries")
            )
          ),
          
          tags$hr(),
          
          # ===== CATEGORICAL SUMMARIES =====
          conditionalPanel(
            condition = "input.eda_mode === 'Categorical Summaries'",
            
            h4("One-Way Contingency Table"),
            p("Explore the frequency distribution of a single categorical variable."),
            fluidRow(
              column(
                width = 4,
                selectInput("oneway_var", 
                            "Select categorical variable:", 
                            choices = cat_vars,
                            selected = "play_type")
              )
            ),
            fluidRow(
              column(
                width = 6, 
                h5("Frequency Table"),
                tableOutput("oneway_table")
              ),
              column(
                width = 6, 
                h5("Bar Chart"),
                plotOutput("oneway_bar", height = "400px")
              )
            ),
            
            tags$hr(),
            
            h4("Two-Way Contingency Table"),
            p("Explore the relationship between two categorical variables."),
            fluidRow(
              column(
                width = 4,
                selectInput("twoway_x", 
                            "Select row variable:", 
                            choices = cat_vars,
                            selected = "play_type")
              ),
              column(
                width = 4,
                selectInput("twoway_y", 
                            "Select column variable:", 
                            choices = cat_vars,
                            selected = "down")
              )
            ),
            fluidRow(
              column(
                width = 6, 
                h5("Frequency Table"),
                tableOutput("twoway_table")
              ),
              column(
                width = 6, 
                h5("Stacked Bar Chart (Proportions)"),
                plotOutput("twoway_bar", height = "400px")
              )
            )
          ),
          
          # ===== NUMERIC SUMMARIES =====
          conditionalPanel(
            condition = "input.eda_mode === 'Numeric Summaries'",
            
            h4("Numeric Variable Summary Statistics"),
            p("Calculate summary statistics for a numeric variable, grouped by a categorical variable."),
            fluidRow(
              column(
                width = 4,
                selectInput("num_summary_var", 
                            "Select numeric variable:", 
                            choices = num_vars,
                            selected = "yards_gained")
              ),
              column(
                width = 4,
                selectInput("group_var", 
                            "Group by (categorical variable):", 
                            choices = cat_vars,
                            selected = "play_type")
              )
            ),
            fluidRow(
              column(width = 12, tableOutput("num_summary_table"))
            ),
            
            tags$hr(),
            
            h4("Visualizations"),
            fluidRow(
              column(
                width = 6, 
                h5("Histogram by Group"),
                plotOutput("hist_plot", height = "400px")
              ),
              column(
                width = 6, 
                h5("Boxplot by Group"),
                plotOutput("box_plot", height = "400px")
              )
            )
          )
        )
      )
    )
  )
)

# --------- SERVER ----------
server <- function(input, output, session) {
  
  # --- Dynamic UI for numeric range sliders ---
  output$num1_range_ui <- renderUI({
    if (is.null(input$num1_var) || input$num1_var == "") return(NULL)
    
    v <- nfl_09_18[[input$num1_var]]
    rng <- range(v, na.rm = TRUE)
    sliderInput("num1_range", 
                paste0("Range for ", input$num1_var, ":"),
                min = floor(rng[1]), 
                max = ceiling(rng[2]),
                value = rng, 
                step = max(1, diff(rng)/100))
  })
  
  output$num2_range_ui <- renderUI({
    if (is.null(input$num2_var) || input$num2_var == "") return(NULL)
    
    v <- nfl_09_18[[input$num2_var]]
    rng <- range(v, na.rm = TRUE)
    sliderInput("num2_range", 
                paste0("Range for ", input$num2_var, ":"),
                min = floor(rng[1]), 
                max = ceiling(rng[2]),
                value = rng, 
                step = max(1, diff(rng)/100))
  })
  
  # --- Reactive filtered data (only updates when button is pressed) ---
  filtered_data <- reactiveVal(nfl_09_18)
  
  observeEvent(input$apply_filters, {
    df <- nfl_09_18
    
    # Apply play_type filter
    if (!is.null(input$play_type_filter) && length(input$play_type_filter) > 0) {
      df <- df %>% filter(play_type %in% input$play_type_filter)
    }
    
    # Apply down filter
    if (!is.null(input$down_filter) && length(input$down_filter) > 0) {
      df <- df %>% filter(down %in% as.numeric(input$down_filter))
    }
    
    # Apply numeric filter 1
    if (!is.null(input$num1_var) && input$num1_var != "" && !is.null(input$num1_range)) {
      df <- df %>% filter(between(.data[[input$num1_var]], input$num1_range[1], input$num1_range[2]))
    }
    
    # Apply numeric filter 2
    if (!is.null(input$num2_var) && input$num2_var != "" && !is.null(input$num2_range)) {
      df <- df %>% filter(between(.data[[input$num2_var]], input$num2_range[1], input$num2_range[2]))
    }
    
    filtered_data(df)
  })
  
  # ========== DATA DOWNLOAD TAB ==========
  
  output$data_size <- renderText({
    nrow(filtered_data())
  })
  
  output$data_table <- renderDT({
    datatable(
      filtered_data(), 
      options = list(
        pageLength = 25, 
        scrollX = TRUE,
        scrollY = "500px"
      ),
      rownames = FALSE
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("nfl_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  # ========== DATA EXPLORATION: CATEGORICAL ==========
  
  # One-way contingency table
  output$oneway_table <- renderTable({
    req(input$oneway_var)
    df <- filtered_data()
    
    # Error handling
    validate(
      need(nrow(df) > 0, "No data available with current filters. Please adjust your filters and click 'Apply Filters'.")
    )
    
    tbl <- table(df[[input$oneway_var]], useNA = "ifany")
    result <- as.data.frame(tbl)
    colnames(result) <- c(input$oneway_var, "Frequency")
    result
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # One-way bar chart
  output$oneway_bar <- renderPlot({
    req(input$oneway_var)
    df <- filtered_data()
    
    # Error handling
    validate(
      need(nrow(df) > 0, "No data available with current filters.")
    )
    
    ggplot(df, aes(x = .data[[input$oneway_var]])) +
      geom_bar(fill = "steelblue", alpha = 0.8) +
      labs(
        title = paste("Frequency of", input$oneway_var),
        x = input$oneway_var, 
        y = "Count"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")
      )
  })
  
  # Two-way contingency table
  output$twoway_table <- renderTable({
    req(input$twoway_x, input$twoway_y)
    df <- filtered_data()
    
    # Error handling
    validate(
      need(nrow(df) > 0, "No data available with current filters."),
      need(input$twoway_x != input$twoway_y, "Please select different variables for rows and columns.")
    )
    
    tbl <- table(df[[input$twoway_x]], df[[input$twoway_y]], useNA = "ifany")
    result <- as.data.frame.matrix(tbl)
    result <- cbind(rownames(result), result)
    colnames(result)[1] <- input$twoway_x
    result
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
  
  # Two-way stacked bar chart
  output$twoway_bar <- renderPlot({
    req(input$twoway_x, input$twoway_y)
    df <- filtered_data()
    
    # Error handling
    validate(
      need(nrow(df) > 0, "No data available with current filters."),
      need(input$twoway_x != input$twoway_y, "Please select different variables.")
    )
    
    ggplot(df, aes(x = .data[[input$twoway_x]], fill = .data[[input$twoway_y]])) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = paste("Proportion of", input$twoway_y, "by", input$twoway_x),
        x = input$twoway_x, 
        y = "Proportion",
        fill = input$twoway_y
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")
      )
  })
  
  # ========== DATA EXPLORATION: NUMERIC ==========
  
  # Numeric summary table
  output$num_summary_table <- renderTable({
    req(input$num_summary_var, input$group_var)
    df <- filtered_data()
    
    # Error handling
    validate(
      need(nrow(df) > 0, "No data available with current filters.")
    )
    
    df %>%
      group_by(.data[[input$group_var]]) %>%
      summarise(
        N = sum(!is.na(.data[[input$num_summary_var]])),
        Mean = mean(.data[[input$num_summary_var]], na.rm = TRUE),
        Median = median(.data[[input$num_summary_var]], na.rm = TRUE),
        SD = sd(.data[[input$num_summary_var]], na.rm = TRUE),
        Min = min(.data[[input$num_summary_var]], na.rm = TRUE),
        Max = max(.data[[input$num_summary_var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(N))
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 2)
  
  # Histogram by group
  output$hist_plot <- renderPlot({
    req(input$num_summary_var, input$group_var)
    df <- filtered_data()
    
    # Error handling
    validate(
      need(nrow(df) > 0, "No data available with current filters.")
    )
    
    ggplot(df, aes(x = .data[[input$num_summary_var]], fill = .data[[input$group_var]])) +
      geom_histogram(
        bins = 50,
        position = "identity", 
        alpha = 0.6
      ) +
      labs(
        title = paste("Distribution of", input$num_summary_var, "by", input$group_var),
        x = input$num_summary_var, 
        y = "Count",
        fill = input$group_var
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # Boxplot by group
  output$box_plot <- renderPlot({
    req(input$num_summary_var, input$group_var)
    df <- filtered_data()
    
    # Error handling
    validate(
      need(nrow(df) > 0, "No data available with current filters.")
    )
    
    ggplot(df, aes(x = .data[[input$group_var]], 
                   y = .data[[input$num_summary_var]], 
                   fill = .data[[input$group_var]])) +
      geom_boxplot(outlier.alpha = 0.3) +
      labs(
        title = paste(input$num_summary_var, "by", input$group_var),
        x = input$group_var, 
        y = input$num_summary_var
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)