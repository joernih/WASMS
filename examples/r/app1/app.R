if (grepl("wasm", sessionInfo()[[2]])) {
  # If the session info contains "wasm", install the package from the specified repository
  webr::install("WASMP", repos = "https://joernih.github.io/WASMA/")
  library("WASMP")
  data_ma <- fred_ts[[2]]
  vd1 <- as.Date("2022-01-01")
  vd2 <- as.Date("2020-02-01")
  plnv <- c('nvalue','growth')[1]
} else {
  # If the session info does not contain "wasm", load the package from the local library
  library("WASMP")
  data_ma <- fred_ts[[2]]
  vd1 <- as.Date("2022-01-01")
  vd2 <- as.Date("2020-02-01")
  plnv <- c('nvalue','growth')[1]
}
# Packages
library(shiny)
library(dplyr)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Tidsserieutvikling for makroøkonomisk variabler før, under og etter pandemien"),
  sidebarLayout(
   sidebarPanel(
      # Add any input options here if needed
       selectInput("cntr","Land",c("USA","JPN","EUZ","GBR","CAN","NOR","DEN","SWE","SKE")),
       selectInput("vars","Makrovariabel",c("mb3","cpi","une","mba","int","gdp")),
       selectInput("veks","Type",c("nvalue","lnalue","lvalue","growth")),
       dateInput("datef", "Valg fra dato:", value = "2018-01-01"),
       dateInput("datet", "Valg til dato:", value = "2024-01-01")
    ),
   mainPanel(
     tabsetPanel(
       tabPanel("Tidsserieplot",plotOutput("plot")),
       tabPanel("Tidsseriedata",tableOutput("table"))
            )
    )
  )
)
server <- function(input, output) {
  res <- reactive({
    cntr <- input$cntr
    varv <- input$vars
    veks <- input$veks
    startd_i <- as.Date(input$datef)
    startd_n <- as.Date(input$datet)
    #c("mb3"="Money","cpi"="Consumer","une"="Unemployment","mba"="Mba","int"="Interest rate","gdp"="GDB")
    dfg <- dplyr::filter(data_ma,id==varv,country%in%cntr) %>%
      dplyr::mutate(date=base::as.Date(date)) %>%
      dplyr::filter(date>startd_i) %>%
      dplyr::filter(date<startd_n) %>%
      dplyr::mutate(nvalue=100*value/value[1]) %>%
      dplyr::mutate(lnalue=log(value)) %>% 
      dplyr::mutate(lvalue=dplyr::lag(value,n=12)) %>% 
      dplyr::mutate(growth=round(value/lvalue-1, 6)*100) 
    mv <- mean(dfg[[veks]], na.rm=TRUE)
    list(dfg,mv,cntr,veks)
  })
  output$plot <- renderPlot({
    ggplot(res()[[1]],aes(x = date, y =!!as.name(res()[[4]]))) + 
    geom_point() +
    geom_vline(xintercept = as.numeric(vd1), linetype = "dashed", color = "blue") +
    geom_vline(xintercept = as.numeric(vd2), linetype = "dashed", color = "blue") +
    geom_hline(yintercept = res()[[2]], linetype = "dashed", color = "red") +
    theme_classic()
  })
  output$table <- renderTable({
    dplyr::select(res()[[1]],date,growth,country,nvalue,lnalue)
  })
}
shinyApp(ui = ui, server = server)


