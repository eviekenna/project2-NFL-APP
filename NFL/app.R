library(shiny)
library(tidyverse)
library(DT)

# Load data (make sure this runs before the app)
nfl_09_18 <- read_csv("NFL Play by Play 2009-2018 (v5).csv")
nfl_09_18 <- nfl_09_18 %>%
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
      
      # Dynamic UI for selecting levels of cat1
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
      
      # Dynamic UI for selecting levels of cat2
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
    
    # Main Panel (placeholder for now)
    mainPanel(
      width = 9,
      h3("Main panel content goes here...")
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
      df <- df %>% filter(as.character(.data[[input$cat1_var]]) %in% input$cat1_levels)
    }
    
    # Apply categorical filter 2
    if (!is.null(input$cat2_levels) && length(input$cat2_levels) > 0) {
      df <- df %>% filter(as.character(.data[[input$cat2_var]]) %in% input$cat2_levels)
    }
    
    # Apply numeric filter 1
    if (!is.null(input$num1_var) && input$num1_var != "" && !is.null(input$num1_range)) {
      df <- df %>% filter(between(.data[[input$num1_var]], input$num1_range[1], input$num1_range[2]))
    }
    
    # Apply numeric filter 2
    if (!is.null(input$num2_var) && input$num2_var != "" && !is.null(input$num2_range)) {
      df <- df %>% filter(between(.data[[input$num2_var]], input$num2_range[1], input$num2_range[2]))
    }
    
    # Update the reactive value
    filtered_data(df)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)