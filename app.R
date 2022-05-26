library(shiny)
library(SCperf)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Modelo de Inventarios EOQ"),
  br(),
  sidebarLayout(
    sidebarPanel(
      numericInput("K","costo fijo K (*)", 0,0),
      br(),
      numericInput("c","costo unitario c ",0,0),
      br(),
      numericInput("d","demanda d (*)",0,0),
      br(),
      numericInput("h","costo de mantener inventario h",0,0),
      br(),
      numericInput("L","tiempo de entrega L",0,0),
      br(),
      br(),
      br(),
      actionButton("update","calcular")
    ),
    mainPanel(
      h3("Politica de inventario"),
      textOutput("politica"),
      br(),
      h3("Costo total por ciclo"),
      textOutput("Ctc"),
      h3("Costo total por unidad de tiempo"),
      textOutput("Total")
    )
  )
)

#Define server logic ----
server <- function(input, output){
  vals <- reactiveValues()
  observeEvent(input$update, {
    vals$Q = EOQ(input$d, input$K, input$h)[1]
    if(input$d*(input$L-(as.integer(L/EOQ(input$d, input$K, input$h)[2]))*EOQ(input$d, input$K, input$h)[2]) > 0){
      vals$PRO = input$d*(input$L-(as.integer(L/EOQ(input$d, input$K, input$h)[2]))*EOQ(input$d, input$K, input$h)[2])
    }else{
      vals$PRO = input$d*input$L
    }
    
    vals$Ct = input$K + input$c*EOQ(input$d, input$K, input$h)[1] + input$h*EOQ(input$d, input$K, input$h)[1]^2/(2*input$d)
    vals$T = input$d*input$K/EOQ(input$d, input$K, input$h)[1] + input$d*input$c + input$h*EOQ(input$d, input$K, input$h)[1]/2
  })
  
  output$politica<-renderText({
    paste("Pedir la cantidad ",as.integer(vals$Q),", siempre que el nivel del inventario se reduzca a ",as.integer(vals$PRO))
  })
  output$Ctc=renderText({
    vals$Ct
  })
  output$Total=renderText({
    vals$T
  })
}
shinyApp(ui = ui, server = server)