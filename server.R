library(shiny); library(data.table); library(ggplot2); library(plotly); library(DT); library(magick); library(shinyjs)
# setwd("C:\\Users\\tmorrison\\Documents\\GitHub\\shiny-apps-deploy\\apps\\world-of-uncertainty")



curr <- reactiveValues(dt=NA,
                       an_dat=data.table(X=NA))

continue <- reactiveValues(makeGif = FALSE)

source("theme.R", local=TRUE)
source("Modules/modal.R", local=TRUE)

shinyServer(function(input, output, session) {
  world <- fread("data/world-data.csv")[, location:=Country.Group.Name]
  country <- as.data.table(read.csv("data/country-data.csv", stringsAsFactors = FALSE))[, location:=Country]
  commod <- fread("data/commodity.csv")[, location:="World"]
  
  country[location=="Sao Tome and Principe", location:="São Tomé and Príncipe"][location=="Cote d'Ivoire", location:="Côte d'Ivoire"]
  
  
  showModal(mod)
  
  output$infotext <- renderText({infotext_html})
  
  observeEvent(input$open_modal, {
    showModal(mod)
  })
  
  observeEvent(input$data_select, {
    
    disable(id="makeGif")
    
    if(input$data_select=="Country-level"){
      curr$an_dat <- country
      curr$dt <- country
    }
    if(input$data_select=="Country groupings"){
      curr$an_dat <- world
      curr$dt <- world
    }
    if(input$data_select=="Commodities"){
      curr$an_dat <- commod
      curr$dt <- commod
    }
  })
  
  
  output$ui1 <- renderUI({
    
    disable(id="makeGif")
    
    if(input$data_select=="Country-level"){
      curr$an_dat <- country
    }
    if(input$data_select=="Country groupings"){
      curr$an_dat <- world
    }
    if(input$data_select=="Commodities"){
      curr$an_dat <- commod
    }
    selectInput("location_select", label="Select a country/region", choices=c("", sort(unique(curr$dt$location))))
    
  })
  
  output$ui2 <- renderUI({
    
    disable(id="makeGif")
    req(input$location_select); req(input$location_select!="-")
    
    if(input$data_select=="Country-level"){
      z <- curr$dt[location==input$location_select][, c('vintage','year','ISO','WEO.Subject.Code','Subject.Descriptor','Units','Scale','value')]
      z <- unique(z)[!(year<vintage) | vintage==2018][, vintage:=paste0("IMF WEO ", vintage, " Spring Forecast")]
      z[grepl("2018", vintage) & year<2018, vintage:="Actual"]
      curr$an_dat <- z
      
    }
    if(input$data_select=="Country groupings"){
      z <- curr$dt[location==input$location_select][, c('vintage','year','Country.Group.Name','WEO.Subject.Code','Subject.Descriptor','Units','Scale','value')]
      z <- unique(z)[!(year<vintage) | vintage==2018][, vintage:=paste0("IMF WEO ", vintage, " Spring Forecast")]
      z[grepl("2018", vintage) & year<2018, vintage:="Actual"]
      curr$an_dat <- z
    }
    if(input$data_select=="Commodities"){
      z <- curr$dt[location==input$location_select][, c('vintage','year','Country.Group.Name','WEO.Subject.Code','Subject.Descriptor','Units','Scale','value')]
      z <- unique(z)[!(year<vintage) | vintage==2018][, vintage:=paste0("IMF WEO ", vintage, " Spring Forecast")]
      z[grepl("2018", vintage) & year<2018, vintage:="Actual"]
      curr$an_dat <- z
    }
    
    ch <- unique(curr$dt[vintage==2018 & location==input$location_select, c("Subject.Descriptor", "WEO.Subject.Code", "Units")][order(Subject.Descriptor)])
    ch2 <- c("", ch$WEO.Subject.Code)
    names(ch2) <- c("-", paste0(ch$Subject.Descriptor, " (", ch$Units, ")"))
    if(input$location_select==""){ch2 <- ""}
    
    selectInput("indicator_select", label="Select an indicator", choices=ch2)
    
  })
  
  observeEvent(input$indicator_select, {updateTabsetPanel(session, inputId="tabpanel", selected="Plot")})
  
#####################################  #####################################  #####################################  #####################################
  #####################################  #####################################  #####################################  #####################################
  #  _          _          _          _          _          _          _          _          _          _          _          _          _          _         
  # |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ |_|| _ _|_ 
  # |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  |  ||_| |  
  #                                                                                                                                                           
  #####################################  #####################################  #####################################  #####################################
#####################################  #####################################  #####################################  #####################################

  output$plotly <- renderPlotly({
    
    disable(id="makeGif")
    req(input$indicator_select)
    
    dt2 <- curr$dt[location==input$location_select][WEO.Subject.Code==input$indicator_select]
    # gif <- dt2[year>=2010][year>=(vintage-1)][vintage>=2012]
    gif <- dt2[year>=2008][year>=(vintage-1)][vintage>=2010]
    plotly_data <- list()
    
    # for(i in 0:10){
    for(i in 0:9){
      # data1 <- gif[vintage==2011+i][, frame:=i][, line:="Most recent projection"][, text:=paste0(vintage, " projection")]
      data1 <- gif[vintage==2009+i][, frame:=i][, line:="Most recent projection"][, text:=paste0(vintage, " projection")]
      # data2 <- gif[vintage<=2010+i][, frame:=i][, line:="Previous projections"][, text:=paste0(vintage, " projection")]
      data2 <- gif[vintage<=2008+i][, frame:=i][, line:="Previous projections"][, text:=paste0(vintage, " projection")]
      # data3 <- dt2[year>=2010 & year<=(2010+i) & vintage==2018][, frame:=i][, line:="Actual"][, text:="Actual"]
      data3 <- dt2[year>=2008 & year<=(2008+i) & vintage==2018][, frame:=i][, line:="Actual"][, text:="Actual"]
      
      dat_all <- rbind(data1, data2, data3)
      if(i>0){
        dat_all <- rbindlist(list(dat_all, plotly_data[[i+1-1]][line=="Actual"], plotly_data[[i+1-1]][line=="Previous projections"]))[, frame:=i]
      }
      plotly_data[[i+1]] <- dat_all

    }; rm(data1, data2, data3, dat_all); an_dat <- unique(rbindlist(plotly_data))
    an_dat <- rbind(an_dat, data.table(frame=0, line=c('Previous projections','Most recent projection'), text=c("a","b"), vintage=2011), fill=TRUE)
    
    
    
    if(input$data_select=="Country-level"){
      z <- an_dat[order(vintage, year)][!is.na(WEO.Subject.Code)][, c('vintage','year','ISO','WEO.Subject.Code','Subject.Descriptor','Units','Scale','value')]
      z <- unique(z)[!(year<vintage) | vintage==2018][, vintage:=paste0("IMF WEO ", vintage, " Spring Forecast")]
      z[grepl("2018", vintage) & year<2018, vintage:="Actual"]
      curr$an_dat <- z
    }
    if(input$data_select=="Country groupings"){
      z <- an_dat[order(vintage, year)][!is.na(WEO.Subject.Code)][, c('vintage','year','Country.Group.Name','WEO.Subject.Code','Subject.Descriptor','Units','Scale','value')]
      z <- unique(z)[!(year<vintage) | vintage==2018][, vintage:=paste0("IMF WEO ", vintage, " Spring Forecast")]
      z[grepl("2018", vintage) & year<2018, vintage:="Actual"]
      curr$an_dat <- z
    }
    if(input$data_select=="Commodities"){
      z <- an_dat[order(vintage, year)][!is.na(WEO.Subject.Code)][, c('vintage','year','Country.Group.Name','WEO.Subject.Code','Subject.Descriptor','Units','Scale','value')]
      z <- unique(z)[!(year<vintage) | vintage==2018][, vintage:=paste0("IMF WEO ", vintage, " Spring Forecast")]
      z[grepl("2018", vintage) & year<2018, vintage:="Actual"]
      curr$an_dat <- z
    }
    
    wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}
    plot_title <- paste0(an_dat$Subject.Descriptor[1], " (",an_dat$Units[1], ")")

    curr$gif <- an_dat
    enable(id="makeGif")
    
    an_dat$line <- factor(an_dat$line, levels = c("Previous projections","Most recent projection","Actual"))
    
    p <- ggplot(data=an_dat, aes(x=year, y=value, color=line, linetype=line, frame=frame, group=vintage)) +
    
      geom_line(aes(text=text)) +
      
      geom_hline(yintercept=0, alpha=.5) +
      
      # scale_x_continuous(limits=c(2010, 2023), breaks=c(2010,2012,2014,2016,2018,2020,2022,2024)) +
      scale_x_continuous(limits=c(2008, 2023), breaks=c(2008,2010,2012,2014,2016,2018,2020,2022,2024)) +
      
      # scale_y_continuous(expand=c(0,0)) +
      
      # scale_color_manual(values=c('black','red','gray'), name="") +
      scale_color_manual(values=c('gray','red','black'), name="") +

      # scale_linetype() +
      scale_linetype_manual(values=c(3,2,1)) +

      ylab(an_dat$Scale[1]) + xlab("Year") + ggtitle(wrapper(plot_title, width = 95)) +

      theme_nrgi(base_family = "Open Sans") +
      
      theme(legend.title=element_blank(), axis.line.x=element_line(color=NA))
    
    # p <- p + aes(group=rev(vintage))


    ggplotly(p, tooltip=c('text', 'y')) %>%
      animation_opts(
        frame=1000, redraw = FALSE, transition=0
      )
    
  })

#####################################  #####################################  #####################################  #####################################
  #####################################  #####################################  #####################################  #####################################
  # _          _          _          _          _          _          _          _          _          _          _          _          _         
  #|      _   |      _   |      _   |      _   |      _   |      _   |      _   |      _   |      _   |      _   |      _   |      _   |      _   
  #| _ . |_   | _ . |_   | _ . |_   | _ . |_   | _ . |_   | _ . |_   | _ . |_   | _ . |_   | _ . |_   | _ . |_   | _ . |_   | _ . |_   | _ . |_   
  #|_| | |    |_| | |    |_| | |    |_| | |    |_| | |    |_| | |    |_| | |    |_| | |    |_| | |    |_| | |    |_| | |    |_| | |    |_| | |    
  #
  #####################################  #####################################  #####################################  #####################################
#####################################  #####################################  #####################################  #####################################
  
  output$makeGif <- downloadHandler(
    
    filename = function() { paste0(input$location_select, "_", curr$gif$WEO.Subject.Code[1],".gif") }, 
    content = function(file) {
      
      
      t1 <- Sys.time()
      
      wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}
      plot_title <- paste0(curr$gif$location[1], ": ", curr$gif$Subject.Descriptor[1], " (",curr$gif$Units[1], ")")
      
      # merge_on <- data.table(expand.grid(line=levels(factor(curr$gif$line)), frame=0:7))[, text:=line][, combo:=paste(line, frame)][, Scale:=curr$gif$Scale[1]]
      merge_on <- data.table(expand.grid(line=levels(factor(curr$gif$line)), frame=0:10))[, text:=line][, combo:=paste(line, frame)][, Scale:=curr$gif$Scale[1]]
      z <- rbindlist(
        list(
          merge_on[!combo %in% curr$gif[, paste(line, frame)]][, c("line", "frame","text","Scale")],
          curr$gif
        ),
        fill=TRUE
      )
      
      z$line <- factor(z$line, levels = c("Previous projections","Most recent projection","Actual"))
      
      # img <- image_graph(width=700, height=340, res = 96)
      img <- image_graph(width=1200, height=600, res = 150)
      # for(i in 0:10){
      for(i in 1:9){
        
        p <- ggplot(data=z[frame==i], aes(x=year, y=value, color=line, group=text, linetype=line, frame=frame)) +
                                             
          geom_line() +
          
          geom_hline(yintercept=0, alpha=.9) +
          
          # scale_x_continuous(limits=c(2010, 2023), breaks=c(2010,2012,2014,2016,2018,2020,2022,2024)) +
          scale_x_continuous(limits=c(2008, 2023), breaks=c(2008,2010,2012,2014,2016,2018,2020,2022,2024)) +

          scale_y_continuous(limits=c(min(c(0, min(z$value, na.rm=TRUE))), max(0, max(z$value, na.rm=TRUE)*1.1))) +

          # scale_color_manual(values=c('black','red','gray')) +
          scale_color_manual(values=c('gray','red','black')) +
          
          # scale_linetype() +
          scale_linetype_manual(values=c(2,2,1)) +

          ylab(z$Scale[1]) + xlab("Year") + ggtitle(wrapper(plot_title, width = 75)) +
          
          labs(caption="Data: IMF World Economic Outlook 2008-2018 \nCreated at apps.resourcegovernance.org/imf-projections") +
          # labs(caption=i) +

          theme_nrgi(base_family = "Open Sans") +

          theme(legend.title=element_blank(),
                panel.grid.minor.y=element_line(size=.2),
                panel.grid.major.y=element_line(size=.2),
                axis.line.x=element_line(color=NA),
                plot.caption=element_text(margin=margin(t=15), face="italic", size=8, hjust=0))
        
        print(p)
        
        
      }
      dev.off()
      animation <- image_animate(img, fps = 1, loop=2)
      t2 <- Sys.time()
      print(t2-t1)
      image_write(animation, file)
      
    }
    
    
  )
  
  output$download <- downloadHandler(
    filename = function() { paste0(input$location_select, input$indicator_select, "weo_data.csv") }, 
    content = function(file) {
      write.csv(curr$an_dat, file, row.names = FALSE)
    }
  )
  
  
  output$table <- DT::renderDataTable({
    
    curr$an_dat
    
  }, options = list(
     dom='',
     pageLength = 500
   )
  )
  
  
  
})
