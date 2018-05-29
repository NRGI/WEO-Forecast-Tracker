
library(shiny); library(data.table); library(ggplot2); library(plotly); library(DT); library(shinyjs)


shinyUI(fluidPage(
  tags$head(tags$script("(function(w,d,s,l,i){w[l]=w[l]||[];
                        w[l].push({'gtm.start':new Date().getTime(),event:'gtm.js'});
                        var f=d.getElementsByTagName(s)[0],j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';
                        j.async=true;j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);})(window,document,'script','dataLayer','GTM-TK6BKNL');"
                        )
  ),
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
  tags$body(
    tags$noscript(
      HTML(
        '<iframe src="https://www.googletagmanager.com/ns.html?id=GTM-TK6BKNL" height="0" width="0" style="display:none;visibility:hidden"></iframe>'
        )
      )
    ),
  tags$head(
    tags$script(
      async="", 
      src=paste0(
        "https://www.googletagmanager.com/gtag/js?id=",
        ga_path
      )
    )
  ),
  tags$head(
    tags$script(
      paste0(
        'window.dataLayer = window.dataLayer || []; function gtag(){dataLayer.push(arguments);} gtag("js", new Date()); gtag("config", "',
        ga_path,'");              '
      )
    )
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
