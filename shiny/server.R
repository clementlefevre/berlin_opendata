library(leaflet)
library(dplyr)
library(tidyr)
library(plotly)
library(directlabels)
library(RColorBrewer)



df.surnames <- NULL
data <- NULL

surnames.juliane <-
  c(
    'lucia',
    'esther',
    'amelie',
    'eve',
    'lucie',
    'livia',
    'nora',
    'leonore',
    'aurelie',
    'lys',
    'liz',
    'marianne',
    
    'liv',
    'delia',
    'alma',
    'laurianne',
    'selma',
    'myriam',
    'elodie',
    
    'marit',
    'ines',
    'laure',
    'laureen',
    'helene',
    'raphaelle',
    'valerie',
    'yael'
  )

server <- function(input, output, session) {
  if (is.null(df.surnames)) {
    df.surnames <-
      read.csv('all_names_year_viertel.csv',
               stringsAsFactors = FALSE)
    
  }
  
  output$year = renderUI({
    selectInput('year', 'Year', selected = 2017, unique(df.surnames[df.surnames$geschlecht == input$gender[[1]],]$year))
    
  })
  
  output$names = renderUI({
    selectInput(
      'names',
      'Names',
      selected = surnames.juliane,
      multiple = TRUE,
      unique(df.surnames[df.surnames$geschlecht == input$gender[[1]] &
                           df.surnames$year == input$year[[1]],]$vorname)
    )
    
  })
  
  filter_df <- eventReactive(input$names, {
    data <-
      df.surnames %>% filter((geschlecht == input$gender) &
                               ((vorname %in% input$names)))
    data <- data %>% filter(viertel == 'berlin') %>% arrange(year)
    data <- data %>% select(vorname, year, anzahl)
    data <-  data %>%
      group_by(vorname) %>%
      complete(year = full_seq(unique(data$year), 1)) %>%
      fill(anzahl) %>% ungroup() %>% as.data.frame
    
    data$anzahl <- data$anzahl %>% replace_na(0)
    
    data
  })
  
  
  
  output$BarNames <- renderPlotly({
    if (is.null(df.surnames)) {
      return (NULL)
    }
    
    data <- filter_df()
    
    if (input$gender == 'w') {
      colPalette <- 'rgba(222,45,38,0.8)'
    } else{
      colPalette <- 'rgba(32,178,170,0.8)'
    }
    
    
    data.bar <-
      data  %>% filter(year == input$year) %>% arrange(desc(anzahl))
    
    xform <- list(categoryorder = "array",
                  categoryarray = data.bar$vorname)
    
    data.bar$color <- 'rgba(204,204,204,1)'
    if (length(data.bar[data.bar$vorname %in% surnames.juliane,]$color) >
        0) {
      data.bar[data.bar$vorname %in% surnames.juliane,]$color <- colPalette
    }
    
    
    marker.color <- list(color = data.bar$color)
    
    p1 <- plot_ly(
      x = data.bar$vorname,
      y = data.bar$anzahl,
      type = 'bar',
      xaxis = xform,
      marker = marker.color
    )
  })
  
  output$LineNames <- renderPlotly({
    if (input$gender == 'w') {
      colPalette <- 'Reds'
    } else{
      colPalette <- 'Blues'
    }
    data <- filter_df()
    
    label_y <- data[data$year == 2012,]$anzahl * 1.05
    label_text <- data[data$year == 2015,]$vorname
    
    colourCount = length(unique(data$vorname))
    getPalette = colorRampPalette(brewer.pal(15, colPalette))
    
    
    p2 <- ggplot(data, aes(
      x = year,
      y = anzahl,
      group = vorname,
      colour = vorname
    )) + geom_line() +     scale_color_manual(values = getPalette(colourCount)) +
      theme(legend.position = "none") +
      annotate(
        "text",
        x = 2012,
        y = label_y,
        label = label_text,
        size = 2
      )
    
    ggplotly()
    
    
  })
  
  
  
}
