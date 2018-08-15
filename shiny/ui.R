library(plotly)
library(DT)


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
      
      radioButtons("city", "City",
                   choiceNames = list(
                     'Berlin','Paris'
                   ),
                   choiceValues = list(
                     "berlin", "paris"
                   )),
     
      uiOutput('year'),
      uiOutput('names'),
      DT::dataTableOutput("table")
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel( "Names",  textOutput("selected_var"), br(),plotlyOutput('BarNames'), br(),plotlyOutput('LineNames')) )
                 
                  
      
    )))
)