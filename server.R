
library(shinythemes)
library(shiny)
library(leaflet)

shinyServer(function(input, output,session) 
{
 
  output$map = renderLeaflet({ leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -73.95, lat = 40.745, zoom =13)
  })
   
  
 #for years 
  bins <- reactive({ 
  
    if (input$Year == c(2005:2015)) {
      
      bins = c(0, 1000000, 3000000, 5000000, 10000000, 30000000, Inf) # color bin for volume
    } else if (input$Year == 'Median Price') {
      bins = c(0, 1000000, 1250000, 1500000, 1750000, 2000000, Inf) # color bin for price
    }
  })
  
  choose1 <- reactive({
    if (input$Year == c(2005:2015)) {
      choose1 = group_by(map_data2@data$YEAR) %>% summarise(mean(map_data2@data$total.transaction.vol)) # select column n for map vlaue input
    } else if (input$Year == 'Median Price') {
      choose1 = group_by(map_data2@data$YEAR) %>% summarise(mean(map_data2@data$n))
    }
  })
  
  
  select.chart1 <- reactive({
    if (input$Parameters == 'Volume') {
      select.chart1 = chart$n 
    } else if (input$Parameters == 'Median Price') {
      selectDonutAll = chart$total.volume
    }  
  })  
  
  select.chart2 <- reactive({
  select.chart2 = chart2$YEAR
  }) 
  
  observe({
      
    pal <- colorBin('YlOrRd', bins = bins(), domain = choose1())
    
      labels <- paste("<strong>",map_data2@data$neighborhood,"</strong>",'<br/>',
                    "Total Transaction:", map_data2@data$total.transaction.vol) %>%
      lapply(htmltools::HTML)
    
          leafletProxy('map', data= map_data2) %>%
            addTiles() %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                        fillColor = ~pal(choose1()),
                        label = ~labels) %>%
            addLegend(
              "bottomleft", pal = pal, values = choose1(),
              title = input$Year,
              labFormat = labelFormat(prefix = " "), opacity = 0.75)
          })  
  
})

