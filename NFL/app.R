library(shiny)
library(tidyverse)
library(DT)

# Load data (make sure this runs before the app)
nfl_09_18 <- read_csv("NFL Play by Play 2009-2018 (v5).csv")
nfl_09_18 <- nfl_09_18 |>
  mutate(season = year(game_date))

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
        
        # DATA DOWNLOAD TAB (placeholder)
        tabPanel(
          title = "Data Download",
          value = "download",
          br(),
          h3("Data Download Tab - Coming Soon")
        ),
        
        # DATA EXPLORATION TAB (placeholder)
        tabPanel(
          title = "Data Exploration",
          value = "exploration",
          br(),
          h3("Data Exploration Tab - Coming Soon")
        )
      )
    )
  )
)
  

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
}

# Run the application 
shinyApp(ui = ui, server = server)