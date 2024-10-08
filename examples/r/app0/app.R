# Load the necessary libraries
webr::install("WASMP", repos = "https://joernih.github.io/WASMA/")
library(WASMP)
library(shiny)
library(DT)
# Define the UI
ui <- fluidPage(
    titlePanel("Data Table Example"),
    
    # Create a data table output
    DTOutput("table")
)

# Define the server logic
server <- function(input, output) {
    # Create a reactive data table to display
    output$table <- renderDT({
        # Use the built-in mtcars dataset as an example
        datatable(HousePrice)
    })
}

# Run the app
shinyApp(ui = ui, server = server)



