if (grepl("wasm", sessionInfo()[[2]])) {
  # If the session info contains "wasm", install the package from the specified repository
  webr::install("WASMP", repos = "https://joernih.github.io/WASMA/")
  library("WASMP")
} else {
  # If the session info does not contain "wasm", load the package from the local library
  library("WASMP")
  vd1 <- as.Date("2022-01-01")
  vd2 <- as.Date("2020-02-01")
  plnv <- c('nvalue','growth')[1]
  data_ma <- fred_ts[[2]]
}
# Packages
library(shiny)
library(dplyr)
library(ggplot2)
# Shiny
ui <- fluidPage(
  titlePanel("Tidsserieutvikling for makroøkonomisk variabler for mange land før, under og etter pandemien"),
  sidebarLayout(
   sidebarPanel(
       selectizeInput("cntr","Valg av land",choices=c("USA","JPN","EUZ","GBR","CAN","NOR","DEN","SWE","SKE"),
		      multiple =TRUE,options=list(maxItems=4), selected=c("NOR")),
       selectInput("vars","Makroøknomisk variabel",c("mb3","cpi","une","mba","int","gdp")),
       selectInput("veks","Type",c("nvalue","growth")),
       dateInput("datef", "Valg fra dato:", value = "2018-01-01"),
       dateInput("datet", "Valg til dato:", value = "2024-01-01")
    ),
   mainPanel(
     tabsetPanel(
       tabPanel("Plot",plotOutput("plot")),
       tabPanel("Tidsserier",tableOutput("table"))
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
    dfg <- dplyr::filter(data_ma,id==varv) %>%
      dplyr::mutate(date=base::as.Date(date)) %>%
      dplyr::filter(date>startd_i) %>%
      dplyr::filter(date<startd_n) %>%
      dplyr::group_by(country) %>%
      dplyr::filter(country%in%cntr) %>%
      dplyr::mutate(couid=paste0(country,id)) %>%
      dplyr::mutate(nvalue=100*value/value[1]) %>%
      dplyr::mutate(lnalue=log(value)) %>% 
      dplyr::mutate(lvalue=dplyr::lag(value,n=12)) %>% 
      dplyr::mutate(growth=round(value/lvalue-1, 6)*100) %>%
      dplyr::ungroup() 
    mv <- mean(dfg[[veks]], na.rm=TRUE)
    list(dfg,mv,cntr,veks)
  })
  output$plot <- renderPlot({
    ggplot2::ggplot(data=res()[[1]], aes(x = date, y =!!sym(res()[[4]]))) +
      ggplot2::geom_line(aes(color = couid)) + 
      ggplot2::labs(title='Tidsserier') +
      geom_vline(xintercept = as.numeric(vd1), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(vd2), linetype = "dashed", color = "blue") +
      ggplot2::theme_minimal()
  })
  output$table <- renderTable({
    dplyr::select(res()[[1]],date,growth,country,nvalue,lnalue)
  })
}
shinyApp(ui = ui, server = server)
