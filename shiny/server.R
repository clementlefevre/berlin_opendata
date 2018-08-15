library(leaflet)
library(dplyr)
library(tidyr)
library(plotly)
library(directlabels)
library(RColorBrewer)



df.surnames <- NULL
data <- NULL
total.names <- 0

gender.list <- list(w='girl', m='boy')

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
    selectInput('year', 'Year', selected = 2017, unique(df.surnames[df.surnames$geschlecht == input$gender[[1]], ]$year))
    
  })
  
  output$names = renderUI({
    selectInput(
      'names',
      'Names',
      selected = surnames.juliane,
      multiple = TRUE,
      unique(df.surnames[df.surnames$geschlecht == input$gender[[1]] &
                           df.surnames$year == input$year[[1]], ]$vorname)
    )
    
  })
  
  filter_df <-
    eventReactive(c(input$names, input$city, input$gender), {
      print(input$city)
      
      data <-
        df.surnames %>% filter(geschlecht == input$gender)
      
     
      data <-
        data %>% filter(viertel == input$city) %>% arrange(year)
      
      data.table <- data %>% arrange(desc(anzahl))
      total.names <- nrow(data)
      
      data <-
        data %>% filter(vorname %in% input$names)
      
     
      
      data <-
        data %>% select(vorname, year, anzahl)
      
      data <-  data %>%
        group_by(vorname) %>%
        complete(year = full_seq(unique(data$year), 1)) %>%
        fill(anzahl) %>% ungroup() %>% as.data.frame
      
      data$anzahl <-
        data$anzahl %>% replace_na(0)
      
      result.data <- combo <- list(total.names = total.names, df= data, data.table = data.table)
      result.data
    })
  
 
  
  output$selected_var <- renderText({
    result <- filter_df()
    
    paste0("Total ", gender.list[input$gender], " for ",input$city," in ", input$year, " : ", result$total.names)
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
   data <- filter_df()
   data.table <- data$data.table %>% filter(year==input$year) %>% select(vorname,anzahl)
   data.table
  }, rownames = FALSE,  options = list(lengthMenu = c(30, 50))))
  
  
  output$BarNames <- renderPlotly({
    if (is.null(df.surnames)) {
      return (NULL)
    }
    
    result <- filter_df()
    data <- result$df
    
    if (input$gender == 'w') {
      colPalette <- 'rgba(194,0,93,0.8)'
    } else{
      colPalette <- 'rgba(32,178,170,0.8)'
    }
    
    
    data.bar <-
      data  %>% filter(year == input$year) %>% arrange(desc(anzahl))
    
    xform <- list(categoryorder = "array",
                  categoryarray = data.bar$vorname)
    
    data.bar$color <- 'rgba(204,204,204,1)'
    if (length(data.bar[data.bar$vorname %in% surnames.juliane, ]$color) >
        0) {
      data.bar[data.bar$vorname %in% surnames.juliane, ]$color <-
        colPalette
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
    result <- filter_df()
    data <- result$df
    
    label_y <- data[data$year == min(data$year), ]$anzahl * 1.05
    label_text <- data[data$year == min(data$year), ]$vorname
    
    colourCount = length(unique(data$vorname))
    getPalette = colorRampPalette(brewer.pal(15, colPalette))
    
    
    p2 <- ggplot(data,
                 aes(
                   x = year,
                   y = anzahl,
                   group = vorname,
                   colour = vorname
                 )) + geom_line() +     scale_color_manual(values = getPalette(colourCount)) +
      theme(legend.position = "none") +
      annotate(
        "text",
        x = min(data$year),
        y = label_y,
        label = label_text,
        size = 2
      )
    
    ggplotly()
    
    
  })
  
  
  
}
