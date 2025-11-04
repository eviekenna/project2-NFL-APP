library(shiny)
library(tidyverse)
library(DT)

# Load data (make sure this runs before the app)
nfl_09_18 <- read_csv("NFL Play by Play 2009-2018 (v5).csv")
nfl_09_18 <- nfl_09_18 |>
  mutate(season = year(game_date))
# Get categorical and numeric variables for exploration
cat_vars <- c("Team" = "posteam", "Down" = "down", "Play Type" = "play_type", "Season" = "season", "Quarter" = "qtr")
num_vars <- c("Yards Gained" = "yards_gained", "Field Position" = "yardline_100", "Score Differential" = "score_differential")


ui <- fluidPage(
  # Application title
  titlePanel("Exploration of NFL Play by Play Data 2009-2018"),
  
  # Sidebar with filters
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filter Data"),
      
      # Categorical Filter 1
      selectInput(
        "cat1_var",
        "Categorical Variable 1:",
        choices = c("Team" = "posteam", 
                    "Down" = "down", 
                    "Play Type" = "play_type",
                    "Season" = "season"),
        selected = "play_type"
      ),
      
      # Dynamic UI for selecting levels of the chosen catagorical variable
      uiOutput("cat1_levels_ui"),
      
      hr(),
      
      # Categorical Filter 2
      selectInput(
        "cat2_var",
        "Categorical Variable 2:",
        choices = c("Team" = "posteam", 
                    "Down" = "down", 
                    "Play Type" = "play_type",
                    "Season" = "season"),
        selected = "down"
      ),
      
      # Dynamic UI for selecting levels of the next chosen catagorical variable
      uiOutput("cat2_levels_ui"),
      
      hr(),
      
      # Numeric Variable 1
      selectInput(
        "num1_var",
        "Numeric Variable 1:",
        choices = c("None" = "", 
                    "Yards Gained" = "yards_gained", 
                    "Field Position (yardline_100)" = "yardline_100",
                    "Score Differential" = "score_differential"),
        selected = ""
      ),
      
      # Dynamic slider for num1
      uiOutput("num1_slider_ui"),
      
      hr(),
      
      # Numeric Variable 2
      selectInput(
        "num2_var",
        "Numeric Variable 2:",
        choices = c("None" = "", 
                    "Yards Gained" = "yards_gained", 
                    "Field Position (yardline_100)" = "yardline_100",
                    "Score Differential" = "score_differential"),
        selected = ""
      ),
      
      # Dynamic slider for num2
      uiOutput("num2_slider_ui"),
      
      hr(),
      
      # Apply button
      actionButton("apply_filters", "Apply Filters", class = "btn-primary btn-lg"),
      br(), br(),
      helpText("Click 'Apply Filters' to update the data based on your selections.")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        # ABOUT TAB
        tabPanel(
          title = "About",
          value = "about",
          br(),
          
          h3("Purpose of This App"),
          p("This Shiny application allows users to explore NFL play-by-play data from the 2009-2018 seasons. 
                      The app provides interactive filtering to create various exploratory data analysis (EDA) 
                      summaries including contingency tables, numerical summaries, and visualizations. Users can subset the data 
                      based on categorical variables (like play type and down) and numeric variables (like yards gained and field position) 
                      to discover patterns and insights about NFL strategies and play outcomes."),
          
          h3("About the Data"),
          p("The dataset used in this application contains detailed play-by-play information for every NFL game 
                      from the 2009 through 2018 seasons. Each row represents a single play and includes information such as:"),
          tags$ul(
            tags$li("Teams involved (offensive and defensive)"),
            tags$li("Play type (pass, run, punt, field goal, etc.)"),
            tags$li("Game situation (down, distance, field position)"),
            tags$li("Play outcomes (yards gained, score changes)"),
            tags$li("Game context (quarter, score differential, season)")
          ),
          
          h4("Data Source"),
          p(HTML("This data comes from the 'Detailed NFL Play-by-Play Data 2009-2018' dataset on the Kaggle website, which provides clean, 
                           well-structured play-by-play data for NFL analysis. For more information about the data 
                           and its methodology, visit:")),
          tags$ul(
            tags$li(tags$a(href = "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016", 
                           target = "_blank", 
                           "Kaggle Dataset Page"))
          ),
          
          h3("How to Use This App"),
          
          h4("Sidebar - Filtering Controls"),
          p("The sidebar on the left contains filtering options that allow you to subset the data:"),
          tags$ul(
            tags$li(tags$strong("Categorical Variable 1 & 2:"), 
                    " Select a categorical variable (Team, Down, Play Type, or Season) from the dropdown menu. 
                                Once selected, checkboxes will appear allowing you to choose which levels to include in your analysis. 
                                All levels are selected by default, and you can uncheck any you want to exclude."),
            tags$li(tags$strong("Numeric Variable 1 & 2:"), 
                    " Optionally select a numeric variable (Yards Gained, Field Position, or Score Differential). 
                                When selected, a slider will appear allowing you to specify a range of values to include."),
            tags$li(tags$strong("Apply Filters Button:"), 
                    " After making your selections, click this button to update all tabs with the filtered data. 
                                The app will NOT update until you click this button, allowing you to adjust multiple filters 
                                before applying them.")
          ),
          
          h4("Main Panel - Tabs"),
          p("The main panel contains multiple tabs for different purposes:"),
          tags$ul(
            tags$li(tags$strong("About:"), 
                    " This tab (currently viewing) provides information about the app, data source, and instructions."),
            tags$li(tags$strong("Data Download:"), 
                    " View the filtered dataset in an interactive table. You can search, sort, and navigate through 
                                the data. You can also download the filtered data as a CSV file for use in other programs."),
            tags$li(tags$strong("Data Exploration:"), 
                    " Create various summaries and visualizations of your filtered data. You can choose between 
                                categorical summaries (contingency tables and bar charts) or numeric summaries (summary statistics, 
                                histograms, and boxplots). This tab allows you to select which variables to analyze and how to 
                                display them.")
          ),
          
          br(),
          
          # NFL Logo
          div(
            style = "text-align: center;",
            tags$img(
              src = "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/National_Football_League_logo.svg/640px-National_Football_League_logo.svg.png",
              height = 150,
              alt = "NFL Logo",
              style = "margin: 20px;"
            )
          ),
          
          br(),
          
          h4("Getting Started"),
          p("To begin exploring the data:"),
          tags$ol(
            tags$li("Use the sidebar to select your desired filters"),
            tags$li("Click the 'Apply Filters' button"),
            tags$li("Navigate to the 'Data Download' or 'Data Exploration' tabs to view results"),
            tags$li("Adjust filters and click 'Apply Filters' again to update your analysis")
          ),
          
          br()
        ),
        
        # DATA DOWNLOAD TAB
        tabPanel(
          title = "Data Download",
          value = "download",
          br(),
          
          h3("Filtered Dataset"),
          p("This table displays the NFL play-by-play data based on your current filter selections. 
                      You can sort columns by clicking the column headers, search for specific values using the search box, 
                      and navigate through pages using the controls at the bottom of the table."),
          
          # Display current number of rows
          p(tags$strong("Current dataset size: "), 
            textOutput("dataset_size", inline = TRUE), 
            " rows"),
          
          br(),
          
          # Data table output
          DTOutput("data_table"),
          
          br(),
          
          # Download button
          downloadButton("download_data", "Download Filtered Data as CSV", class = "btn-success"),
          
          br(), br(),
          
          helpText("Note: The downloaded file will include all columns from the filtered dataset. 
                             The filename will include the date and time of download.")
        ),
        
        # DATA EXPLORATION TAB
        tabPanel(
          title = "Data Exploration",
          value = "exploration",
          br(),
          
          h3("Explore Your Filtered Data"),
          p("The tables and visualizations below are based on the filters you selected in the sidebar."),
          
          # Radio buttons to choose exploration mode
          fluidRow(
            column(
              width = 12,
              radioButtons(
                "eda_mode",
                "What would you like to explore?",
                choices = c("Categorical Summaries", "Numeric Summaries"),
                inline = TRUE,
                selected = "Categorical Summaries"
              )
            )
          ),
          
          hr(),
          
          # ===== CATEGORICAL SUMMARIES =====
          conditionalPanel(
            condition = "input.eda_mode === 'Categorical Summaries'",
            
            h4("One-Way Contingency Table"),
            p("Frequency distribution for the first categorical variable you selected."),
            fluidRow(
              column(width = 6, 
                     h5("Frequency Table"),
                     DTOutput("oneway_table")),
              column(width = 6, 
                     h5("Bar Chart"),
                     plotOutput("oneway_bar", height = "400px"))
            ),
            
            hr(),
            
            h4("Two-Way Contingency Table"),
            p("Relationship between your two selected categorical variables."),
            fluidRow(
              column(width = 6, 
                     h5("Frequency Table"),
                     DTOutput("twoway_table")),
              column(width = 6, 
                     h5("Stacked Bar Chart (Proportions)"),
                     plotOutput("twoway_bar", height = "400px"))
            )
          ),
          
          # ===== NUMERIC SUMMARIES =====
          conditionalPanel(
            condition = "input.eda_mode === 'Numeric Summaries'",
            
            h4("Numeric Variable Summary Statistics"),
            p("Summary statistics for the first numeric variable, grouped by the first categorical variable."),
            fluidRow(
              column(width = 12, DTOutput("num_summary_table"))
            ),
            
            hr(),
            
            h4("Visualizations"),
            fluidRow(
              column(width = 6, 
                     h5("Histogram by Group"),
                     plotOutput("hist_plot", height = "400px")),
              column(width = 6, 
                     h5("Boxplot by Group"),
                     plotOutput("box_plot", height = "400px"))
            )
          )
        )
      )
    )
  )
)  # Added closing parenthesis for sidebarLayout and fluidPage

server <- function(input, output, session) {
  
  # Dynamic UI for categorical filter 1 levels
  output$cat1_levels_ui <- renderUI({
    req(input$cat1_var)
    
    # Get unique levels for selected variable
    levels <- sort(unique(as.character(nfl_09_18[[input$cat1_var]])))
    
    checkboxGroupInput(
      "cat1_levels",
      "Select levels to include:",
      choices = levels,
      selected = levels  # All selected by default
    )
  })
  
  # Dynamic UI for categorical filter 2 levels
  output$cat2_levels_ui <- renderUI({
    req(input$cat2_var)
    
    # Get unique levels for selected variable
    levels <- sort(unique(as.character(nfl_09_18[[input$cat2_var]])))
    
    checkboxGroupInput(
      "cat2_levels",
      "Select levels to include:",
      choices = levels,
      selected = levels  # All selected by default
    )
  })
  
  # Dynamic UI for numeric filter 1 slider
  output$num1_slider_ui <- renderUI({
    req(input$num1_var)
    if (input$num1_var == "") return(NULL)
    
    # Get range for selected numeric variable
    var_data <- nfl_09_18[[input$num1_var]]
    var_range <- range(var_data, na.rm = TRUE)
    
    sliderInput(
      "num1_range",
      paste("Range for", input$num1_var, ":"),
      min = floor(var_range[1]),
      max = ceiling(var_range[2]),
      value = var_range,
      step = 1
    )
  })
  
  # Dynamic UI for numeric filter 2 slider
  output$num2_slider_ui <- renderUI({
    req(input$num2_var)
    if (input$num2_var == "") return(NULL)
    
    # Get range for selected numeric variable
    var_data <- nfl_09_18[[input$num2_var]]
    var_range <- range(var_data, na.rm = TRUE)
    
    sliderInput(
      "num2_range",
      paste("Range for", input$num2_var, ":"),
      min = floor(var_range[1]),
      max = ceiling(var_range[2]),
      value = var_range,
      step = 1
    )
  })
  
  # Reactive filtered data using reactiveVal
  filtered_data <- reactiveVal(nfl_09_18)
  
  # Only update data when Apply Filters button is clicked
  observeEvent(input$apply_filters, {
    df <- nfl_09_18
    
    # Apply categorical filter 1
    if (!is.null(input$cat1_levels) && length(input$cat1_levels) > 0) {
      df <- df |> filter(as.character(.data[[input$cat1_var]]) %in% input$cat1_levels)
    }
    
    # Apply categorical filter 2
    if (!is.null(input$cat2_levels) && length(input$cat2_levels) > 0) {
      df <- df |> filter(as.character(.data[[input$cat2_var]]) %in% input$cat2_levels)
    }
    
    # Apply numeric filter 1
    if (!is.null(input$num1_var) && input$num1_var != "" && !is.null(input$num1_range)) {
      df <- df |> filter(between(.data[[input$num1_var]], input$num1_range[1], input$num1_range[2]))
    }
    
    # Apply numeric filter 2
    if (!is.null(input$num2_var) && input$num2_var != "" && !is.null(input$num2_range)) {
      df <- df |> filter(between(.data[[input$num2_var]], input$num2_range[1], input$num2_range[2]))
    }
    
    # Update the reactive value
    filtered_data(df)
  })
  
  # ========== DATA DOWNLOAD TAB OUTPUTS ==========
  
  # Display dataset size
  output$dataset_size <- renderText({
    paste(format(nrow(filtered_data()), big.mark = ","))
  })
  
  # Render the data table
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "500px",
        searching = TRUE,
        ordering = TRUE
      ),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("nfl_filtered_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # ========== DATA EXPLORATION TAB: CATEGORICAL SUMMARIES ==========
  
  # One-way contingency table (uses cat1_var from sidebar)
  output$oneway_table <- renderDT({
    req(input$cat1_var)
    df <- filtered_data()
    
    # Validation
    validate(
      need(nrow(df) > 0, "No data available with current filters. Please adjust your filters and click 'Apply Filters'.")
    )
    
    # Create frequency table using the first categorical variable from sidebar
    tbl <- df %>%
      count(.data[[input$cat1_var]]) %>%
      arrange(desc(n)) %>%
      mutate(percent = round(100 * n / sum(n), 2))
    
    colnames(tbl) <- c(input$cat1_var, "Count", "Percent (%)")
    
    datatable(tbl, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  # One-way bar chart (uses cat1_var from sidebar)
  output$oneway_bar <- renderPlot({
    req(input$cat1_var)
    df <- filtered_data()
    
    # Validation
    validate(
      need(nrow(df) > 0, "No data available with current filters.")
    )
    
    ggplot(df, aes(x = .data[[input$cat1_var]])) +
      geom_bar(fill = "steelblue", alpha = 0.8) +
      labs(
        title = paste("Frequency of", input$cat1_var),
        x = input$cat1_var,
        y = "Count"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")
      )
  })
  
  # Two-way contingency table (uses cat1_var and cat2_var from sidebar)
  output$twoway_table <- renderDT({
    req(input$cat1_var, input$cat2_var)
    df <- filtered_data()
    
    # Validation
    validate(
      need(nrow(df) > 0, "No data available with current filters."),
      need(input$cat1_var != input$cat2_var, "Please select different variables for the two categorical filters in the sidebar.")
    )
    
    # Create two-way table using both categorical variables from sidebar
    tbl <- table(df[[input$cat1_var]], df[[input$cat2_var]])
    result <- as.data.frame.matrix(tbl)
    result <- cbind(rownames(result), result)
    colnames(result)[1] <- input$cat1_var
    
    datatable(result, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # Two-way stacked bar chart (uses cat1_var and cat2_var from sidebar)
  output$twoway_bar <- renderPlot({
    req(input$cat1_var, input$cat2_var)
    df <- filtered_data()
    
    # Validation
    validate(
      need(nrow(df) > 0, "No data available with current filters."),
      need(input$cat1_var != input$cat2_var, "Please select different variables for the two categorical filters.")
    )
    
    # Convert to factors to ensure proper plotting
    df_plot <- df %>%
      mutate(
        x_var = as.factor(.data[[input$cat1_var]]),
        y_var = as.factor(.data[[input$cat2_var]])
      )
    
    ggplot(df_plot, aes(x = x_var, fill = y_var)) +
      geom_bar(position = "fill", width = 0.7) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = paste("Proportion of", input$cat2_var, "by", input$cat1_var),
        x = input$cat1_var,
        y = "Proportion",
        fill = input$cat2_var
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "right"
      )
  })
  
  # ========== DATA EXPLORATION TAB: NUMERIC SUMMARIES ==========
  
  # Numeric summary table (uses num1_var grouped by cat1_var from sidebar)
  output$num_summary_table <- renderDT({
    req(input$num1_var, input$cat1_var)
    df <- filtered_data()
    
    # Validation
    validate(
      need(nrow(df) > 0, "No data available with current filters."),
      need(input$num1_var != "", "Please select a numeric variable in the sidebar to see summary statistics.")
    )
    
    # Calculate summary statistics using num1_var grouped by cat1_var
    summary_tbl <- df %>%
      group_by(.data[[input$cat1_var]]) %>%
      summarise(
        N = sum(!is.na(.data[[input$num1_var]])),
        Mean = round(mean(.data[[input$num1_var]], na.rm = TRUE), 2),
        Median = median(.data[[input$num1_var]], na.rm = TRUE),
        SD = round(sd(.data[[input$num1_var]], na.rm = TRUE), 2),
        Min = min(.data[[input$num1_var]], na.rm = TRUE),
        Max = max(.data[[input$num1_var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(N))
    
    datatable(summary_tbl, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  # Histogram by group (uses num1_var colored by cat1_var from sidebar)
  output$hist_plot <- renderPlot({
    req(input$num1_var, input$cat1_var)
    df <- filtered_data()
    
    # Validation
    validate(
      need(nrow(df) > 0, "No data available with current filters."),
      need(input$num1_var != "", "Please select a numeric variable in the sidebar.")
    )
    
    ggplot(df, aes(x = .data[[input$num1_var]], fill = .data[[input$cat1_var]])) +
      geom_histogram(bins = 50, position = "identity", alpha = 0.6) +
      labs(
        title = paste("Distribution of", input$num1_var, "by", input$cat1_var),
        x = input$num1_var,
        y = "Count",
        fill = input$cat1_var
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # Boxplot by group (uses num1_var grouped by cat1_var from sidebar)
  output$box_plot <- renderPlot({
    req(input$num1_var, input$cat1_var)
    df <- filtered_data()
    
    # Validation
    validate(
      need(nrow(df) > 0, "No data available with current filters."),
      need(input$num1_var != "", "Please select a numeric variable in the sidebar.")
    )
    
    ggplot(df, aes(x = .data[[input$cat1_var]], 
                   y = .data[[input$num1_var]], 
                   fill = .data[[input$cat1_var]])) +
      geom_boxplot(outlier.alpha = 0.3) +
      labs(
        title = paste(input$num1_var, "by", input$cat1_var),
        x = input$cat1_var,
        y = input$num1_var
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)