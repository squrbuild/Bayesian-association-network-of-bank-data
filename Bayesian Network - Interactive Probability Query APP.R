# 1. Load necessary libraries
library(shiny)
library(bnlearn)
library(dplyr)
library(stringr)

# 2. Load the model and all helper files (runs only once when the app starts)
tryCatch({
  bn_model <- readRDS("bayesian_network_model.rds")
  name_map <- readRDS("name_map.rds")
  var_levels <- readRDS("variable_levels.rds")
}, error = function(e) {
  # Translated error message
  stop("Error: Required files not found! Please ensure bayesian_network_model.rds, name_map.rds, and variable_levels.rds are in your working directory.")
})

# Get all queryable variables (all variables except the target 'y_1')
predictor_vars <- setdiff(names(bn_model), "y_1")

# 3. Define the User Interface (UI)
ui <- fluidPage(
  
  # App title
  titlePanel("Bank Marketing Bayesian Network - Interactive Probability Query"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel: for all input controls
    sidebarPanel(
      width = 4, # Sidebar width
      h4("Select Customer Features (Evidence):"),
      p("For features you are unsure about, please leave the default 'Not Specified' option."),
      
      # Dynamically create a dropdown menu for each variable using lapply
      lapply(predictor_vars, function(var_name) {
        # Find the original, human-readable name from the map for display
        original_label <- name_map$original_name[name_map$new_name == var_name]
        
        selectInput(
          inputId = var_name,
          label = paste0(original_label, " (", var_name, ")"),
          # Provide choices for each menu from the loaded var_levels list
          choices = c("Not Specified" = "", var_levels[[var_name]]) # "" represents not using it as evidence
        )
      }),
      
      # Query button
      actionButton("query_button", "Calculate Success Probability", class = "btn-primary", icon = icon("calculator"))
    ),
    
    # Main panel: for displaying the output
    mainPanel(
      width = 8, # Main panel width
      h3("Query Result"),
      # Use UIOutput to support richer HTML formatting
      uiOutput("probability_output")
    )
  )
)

# 4. Define the Server Logic
server <- function(input, output) {
  
  # Execute when the user clicks the button
  observeEvent(input$query_button, {
    
    # Collect all conditions selected by the user
    evidence_list <- list()
    for (var_name in predictor_vars) {
      if (!is.null(input[[var_name]]) && input[[var_name]] != "") {
        evidence_list[[var_name]] <- input[[var_name]]
      }
    }
    
    # Prepare an HTML string for display
    output_html <- ""
    
    # If the user selected no conditions, calculate the baseline success probability
    if (length(evidence_list) == 0) {
      prob_table <- bn_model$y_1$prob
      prob <- as.numeric(prop.table(prob_table)["1"]) # Get the baseline probability
      
      output_html <- paste(
        "<p>No conditions specified.</p>",
        "<p>Based on the model data, the overall baseline probability of success is:</p>",
        "<div style='font-size: 24px; font-weight: bold; color: #007BFF;'>",
        sprintf("%.2f%%", prob * 100),
        "</div>"
      )
      
    } else {
      # If the user selected conditions, perform the cpquery
      # Use the robust string construction method for the query
      evidence_string <- paste(
        sprintf("(`%s` == '%s')", names(evidence_list), unlist(evidence_list)),
        collapse = " & "
      )
      
      # Execute the query
      prob <- cpquery(
        fitted = bn_model,
        event = (y_1 == "1"),
        evidence = evidence_string,
        n = 100000 # Use enough simulations for a stable result
      )
      
      # Create a nicely formatted description of the conditions for display
      display_conditions <- paste(
        sapply(names(evidence_list), function(name) {
          original_name <- name_map$original_name[name_map$new_name == name]
          sprintf("<li><b>%s</b> = %s</li>", original_name, evidence_list[[name]])
        }),
        collapse = ""
      )
      
      output_html <- paste(
        "<p>Based on the following selected conditions:</p>",
        "<ul>", display_conditions, "</ul>",
        "<p>The model's posterior probability of marketing success is:</p>",
        "<div style='font-size: 24px; font-weight: bold; color: #28A745;'>",
        sprintf("%.2f%%", prob * 100),
        "</div>"
      )
    }
    
    # Render the result to the UI
    output$probability_output <- renderUI({
      HTML(output_html)
    })
  })
}

# 5. Run the App
shinyApp(ui = ui, server = server)