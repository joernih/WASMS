if (grepl("wasm", sessionInfo()[[2]])) {
  # If the session info contains "wasm", install the package from the specified repository
  webr::install("WASMP", repos = "https://joernih.github.io/WASMA/")
  library("WASMP")
  data_vk <- WASMP::nb_ts
} else {
  # If the session info does not contain "wasm", load the package from the local library
  library("WASMP")
  data_vk <- WASMP::nb_ts
}
Packages
library(shiny)
library(dplyr)
library(ggplot2)
# Shiny
ui <- fluidPage(
  titlePanel("Valutakursutvikling for norske kroner"),
  sidebarLayout(
   sidebarPanel(
       selectInput("veks","Valg av valuta",c("value_EUR","value_USD","value_SEK")),
       dateInput("datef", "Velg fra dato:", value = "2020-01-01"),
       dateInput("datet", "Velg til dato:", value = "2021-10-05"),
       selectInput("frekv","Frekvens",c("Daglig","Månedlig","Årlig")),
    ),
   mainPanel(
     tabsetPanel(
       tabPanel("Tidserieplot",plotOutput("plot")),
       tabPanel("Tabell",tableOutput("table"))
            )
    )
  )
)
server <- function(input, output) {
  res <- reactive({
    veks <- input$veks
    startd_i <- as.Date(input$datef)
    startd_n <- as.Date(input$datet)
    frekvs_i <- pmatch(input$frekv,c("Årlig","Månedlig","Daglig"))
    # Melt
    dtf <- data_vk[[frekvs_i]][[1]]
    mvg <- dtf %>% dplyr::mutate(date=base::as.Date(dato)) %>%
      # Filter out all observations starting earlier or equal than startd_i
      dplyr::filter(date>startd_i) %>%
      # Filter out all observations starting later or equal than startd_n
      dplyr::filter(date<startd_n) 
    list(mvg,veks)
  })
  output$plot <- renderPlot({
    ggplot2::ggplot(res()[[1]], aes(x = date, y = !!sym(res()[[2]]))) + 
      ggplot2::geom_point() +
      ggplot2::labs(x = "Date", y = "Exchange Rate", color = "Currency") +
      ggplot2::theme_classic()

  })
  output$table <- renderTable({
    res()[[1]]
  })
}
shinyApp(ui = ui, server = server)



