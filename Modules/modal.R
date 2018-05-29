
intro_mod <- modalDialog(
  easyClose=TRUE,
  footer=NULL,
  title=NULL,
  renderUI(info_text)
)

mod <- modalDialog(
  easyClose = TRUE,
  footer = NULL,
  title = NULL,
  tabsetPanel(
    tabPanel(title="Welcome",
             renderUI(
               info_text
             )
        ),
    tabPanel(title="Data",
             renderUI(
               data_text
             )
    )
  )
)

info_text <- HTML("<br><p style='width:100%; line-height: 1.6'>
       
                    Welcome to the World Economic Outlook (WEO) forecast-to-actual comparator, a tool that enables users to plot how 
                    International Monetary Fund (IMF) projections of key economic variables have evolved over time. The tool uses data 
                    from the IMF's WEO spring edition published between 2008 and 2018.
                    
                    <br><br>
                    
                    <b>How to use</b>. Select relevant data categories from the left-hand menu. Once data is selected your plot will
                    display on the right. Press the play button to see an animation of the historical data and forecasts from
                    2012 onward. Use the slider on the bottom to toggle between steps in the animation. Review the data with
                    mouseover or download an animation with the \"Download .gif\" button.
                    
                    <br><br>

                    <i>Learn more about the Natural Resource Governance Institute at 
                    <a href='https://resourcegovernance.org/' target='_blank'> resourcegovernance.org</a>.</i>
                    
                    </p>
                    
                    
                    ")
# 


data_text <- HTML("<br><p style='width:100%; line-height: 1.6'>
                   <b>Data</b>. All data are accessible from the IMF <a target='_blank' href='https://www.imf.org/external/ns/cs.aspx?id=28'>here</a>, 
                   but can only be downloaded one edition at a time. The processed data used for the visualizations 
                   in the application are also available for download, using the button on the left. It will update
                   as filters are applied, and can also be viewed on the Data tab. Some country and indicator pairs
                   do not have complete data. Scripts for processing the data are available <a target='_blank' href='https://github.com/NRGI/WEO-Forecast-Tracker'>here</a>.
                   
                   <br><br>
                     
                   For any given subset of country or group and indicator, the data is split by vintage, which corresponds
                   to the edition of the WEO from which the data come. The vintage category \"Actual\" are the historical 
                   values (pre-2018) from the 2018 WEO. All other vintage categories denote the edition of the WEO and 
                   show values for forecasts in that year and future years. The tool and data are available under 
                   <a href='https://creativecommons.org/licenses/by-sa/4.0/' target='_blank'>CC BY SA license</a>.
                   
                   <br><br>
                   
                   <b>Tool</b>. The tool was built in R Shiny, an open source web application framework using the R language. 
                   Scripts for creating the tool as well as processing the data are available 
                   <a target='_blank' href='https://github.com/NRGI/WEO-Forecast-Tracker'>on Github</a>.
                   It was created by 
                   <a href='mailto:dmihalyi@resourcegovernance.org'>David Mihalyi</a>, economic analyst, 
                   and 
                   <a href='mailto:tmorrison@resourcegovernance.org'>Tommy Morrison</a>, data associate, with the Natural Resource Governance Institute.
                   </p>")



