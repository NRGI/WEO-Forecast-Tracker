
library(shiny); library(data.table); library(ggplot2); library(plotly); library(DT); library(shinyjs)


shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  ###########################################################################################
  inlineCSS(
    list(
      ".modal-dialog"="width:800px",
      "body > div.container-fluid > div:nth-child(5)"="margin-top:40px"
      )
    ),
  tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
  tags$head(
    tags$link(
      rel="shortcut icon",
      href="favicon.ico",
      type="image/x-icon"
    )
  ),
  tags$style(
    type="text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  ###########################################################################################
  
  fluidRow(column(width=1, a(img(src="nrgi_logo.png", id='logo', height="66px"),target="_blank", href="https://resourcegovernance.org/"), style="padding-top:15px; padding-left:50px;"),
           column(width=1),
           column(width=9, h3("World Economic Outlook Forecast Tracker", style='font-size:34px'), style="padding-top:12px")),
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("data_select", label="Select a dataset", choices=c("Country-level", "Country groupings", "Commodities")),
      uiOutput("ui1"),
      uiOutput("ui2"),
      downloadButton("download", label="Download filtered data"),
      downloadButton("makeGif", label="Download .gif animation"),
      fluidRow(
        actionButton("open_modal", label="Information")
      )
    ),
    
    
    mainPanel(
      tabsetPanel(
        id='tabpanel',
        tabPanel("Plot", plotlyOutput("plotly")),
        tabPanel("Data", DT::dataTableOutput("table"))#,
        # tabPanel("About", htmlOutput("infotext"))
      )
    )
  )
))
