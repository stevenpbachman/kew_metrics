

# elements to add whenever you have a new dataset:

# First add your new datasets:
  
  DatasetA <- data.frame(
    Species = c("species1", "species2", "species3"),
    Value = c(5, 10, 15)
  )
  DatasetB <- data.frame(
    Species = c("species4", "species5", "species6"),
    Value = c(25, 30, 35)
  )
  
# Add a new accordion panel in the UI:
    
    accordion_panel(
      "Plant Types 3",
      radioButtons("dataset3", "Select Dataset:",
                   choices = list("None" = "", "Dataset A" = "dsA", "Dataset B" = "dsB"),
                   selected = ""
      )
    )
  
# Update the selected_data() reactive to include the new options:
    
    selected_data <- reactive({
      if (!is.null(input$dataset1) && input$dataset1 != "") {
        switch(input$dataset1,
               "legumes" = Legumes,    
               "monocots" = Monocots   
        )
      } else if (!is.null(input$dataset2) && input$dataset2 != "") {
        switch(input$dataset2,
               "gymno" = Gymnosperms,  
               "angio" = Angiosperms   
        )
      } else if (!is.null(input$dataset3) && input$dataset3 != "") {  # New condition
        switch(input$dataset3,
               "dsA" = DatasetA,
               "dsB" = DatasetB
        )
      } else {
        NULL
      }
    })
  
# Add a new observer to handle the mutual exclusivity:
    
    observe({
      if (!is.null(input$dataset3) && input$dataset3 != "") {
        updateRadioButtons(session, "dataset1", selected = "")
        updateRadioButtons(session, "dataset2", selected = "")
      }
    })
  
# Update the selected dataset text output:
    
    output$selected_dataset <- renderText({
      if (is.null(selected_data())) {
        "Please select a dataset"
      } else {
        paste("Currently viewing:", 
              if (!is.null(input$dataset1) && input$dataset1 != "") {
                switch(input$dataset1,
                       "legumes" = "Legumes",
                       "monocots" = "Monocots")
              } else if (!is.null(input$dataset2) && input$dataset2 != "") {
                switch(input$dataset2,
                       "gymno" = "Gymnosperms",
                       "angio" = "Angiosperms")
              } else {
                switch(input$dataset3,
                       "dsA" = "Dataset A",
                       "dsB" = "Dataset B")
              })
      }
    })
    
# The key points to remember when adding more accordion panels are:
#     
# Give each new radio button group a unique input ID (e.g., "dataset3")
# Make sure the values in the choices list match exactly what you use in the switch statement
# Update all the observers to ensure mutual exclusivity across all panels
# Update the text output to handle the new dataset names
  