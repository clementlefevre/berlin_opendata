library(plotly)
library(shinycssloaders)


shinyUI(fluidPage(
  titlePanel("Berlin Baby Names"),
  
  sidebarLayout(
    sidebarPanel(
   
      radioButtons("gender", "Gender",
                   choiceNames = list(
                    'Girl','Boy'
                   ),
                   choiceValues = list(
                     "w", "m"
                   )),
     
      uiOutput('year'),
      uiOutput('names')
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel( "Names", br(),plotlyOutput('BarNames'), br(),plotlyOutput('LineNames')) )
                 
                  
      
    )))
)