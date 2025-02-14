library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(DT)

# Helper function to initialize database and load data
init_database <- function(csv_file = "data.csv") {
  # Create/connect to SQLite database
  con <- dbConnect(RSQLite::SQLite(), "data.db")
  
  # Check if table exists
  if (!dbExistsTable(con, "data_table")) {
    # Read CSV and write to database
    data <- read.csv(csv_file)
    dbWriteTable(con, "data_table", data)
    
    # Create indexes for columns commonly used in filtering
    # Modify these based on your specific columns
    dbExecute(con, "CREATE INDEX idx_column1 ON data_table(column1)")
    dbExecute(con, "CREATE INDEX idx_column2 ON data_table(column2)")
  }
  
  return(con)
}

ui <- page_sidebar(
  title = "Database Table Viewer",
  sidebar = sidebar(
    # Add your filter inputs here
    selectInput("filter_column1", "Filter Column 1",
                choices = NULL),  # Will be populated in server
    selectInput("filter_column2", "Filter Column 2",
                choices = NULL),
    # Add more filters as needed
    hr(),
    downloadButton("download_filtered", "Download Filtered Data")
  ),
  card(
    full_screen = TRUE,
    card_body(
      DTOutput("table_output")
    )
  )
)

server <- function(input, output, session) {
  # Initialize database connection
  con <- init_database()
  onStop(function() {
    dbDisconnect(con)
  })
  
  # Populate filter choices from database
  observe({
    # Get unique values for filter dropdowns
    column1_values <- dbGetQuery(con, 
      "SELECT DISTINCT column1 FROM data_table ORDER BY column1")
    column2_values <- dbGetQuery(con, 
      "SELECT DISTINCT column2 FROM data_table ORDER BY column2")
    
    updateSelectInput(session, "filter_column1", 
                     choices = c("All", column1_values$column1))
    updateSelectInput(session, "filter_column2", 
                     choices = c("All", column2_values$column2))
  })
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    # Start with base query
    query <- "SELECT * FROM data_table WHERE 1=1"
    params <- list()
    
    # Add filters based on inputs
    if (!is.null(input$filter_column1) && input$filter_column1 != "All") {
      query <- paste0(query, " AND column1 = ?")
      params <- c(params, input$filter_column1)
    }
    if (!is.null(input$filter_column2) && input$filter_column2 != "All") {
      query <- paste0(query, " AND column2 = ?")
      params <- c(params, input$filter_column2)
    }
    
    # Execute query with pagination
    dbGetQuery(con, query, params)
  })
  
  # Render datatable
  output$table_output <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  
  # Download handler
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste("filtered-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
