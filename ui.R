
shinyUI(fluidPage(
  titlePanel(h3("Sampling GFC data to derive training points")),
  
  fluidRow(
    column(4,sliderInput("sampling_rate_loss", 
                                label = "Sampling rate for loss", 
                                min = 0, max = 1000, value = 100)
          ),
    column(4, sliderInput("sampling_rate_noloss", 
                          label = "Sampling rate for stable", 
                          min = 0, max = 1000, value = 300)
          ),
    column(4,sliderInput("sampling_rate_gain", 
                         label = "Sampling rate for gain", 
                         min = 0, max = 1000, value = 100)
          )
    ),
  
  sidebarLayout(
                sidebarPanel(h4("Disclaimer for FAO"),
                             
                             img(src="FAO_blue_50.jpg", height = 50, width = 50),
                             
                             p("FAO can't be held responsible for anything here"),
                             br(),
                             
                             code("Application written by remi.dannunzio@fao.org"),
                             sliderInput("treecover", 
                                                  label = "Tree cover threshold to sample (%)",
                                                  min = 0, max = 100, value = 30 ),
                             h3("Points in the stable layer..."),
                             tableOutput("table_stable"),
                             h3("Final sampling"),
                             tableOutput("table_final")
                             ),          
                
                  mainPanel(width=4,
                  h3("Distribution of points"),
                  plotOutput("map_pts"),
                  textInput("basename", label = h3("Basename of KML to export"), 
                            value = paste("export",Sys.Date(),sep="")),
                  downloadButton('downloadData', 'Download the KML')
                            )
              )               
 ))