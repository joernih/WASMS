# Load the necessary libraries
library(shiny)
library(DT)
webr::install("WASMP", repos = "https://joernih.github.io/WASMA/")
ls(package:WASMP)
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


