library(googleVis)
library(shinythemes)
library(shiny)
library(leaflet)


shinyUI(fluidPage(
  
  
navbarPage('New York', id='ny',
             theme = shinytheme("cyborg"),
          
#--------first tab--------------  
        tabPanel('Historic Sales', div(class="outer", leafletOutput('map', width = '80%', height = '80%')),
        
                 
        absolutePanel( id='functions', class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 50, left = "auto", right = 50,
                           width = 330, height = "auto",
                                                           
        h2('Sales from 2005-2017'),
        
        selectInput("Parameters", label= "Parameters", choices = Parameters, selected = 'Volume'),
                    
        selectInput("Borough1", label= "Borough", choices = Select.Borough, selected = 'All'),
                    
        selectInput("Neighborhood1", label= "Neighbourhood", choices = Select.Neighborhood, selected = 'All'),
                                                           
        checkboxGroupInput("HomesSize",
                          label = h3("Home Size"),
                          choices = list(" One Family Homes" = 1,
                                         " Two Family Homes" = 2,
                                         " Three Family Homes" = 3),selected = 1),
        sliderInput("Year", 
                             label = "Year Selected:",
                             min = 2005, max = 2017, value = 2015, step = 1, round = TRUE),
                                                           
                                                           
        leafletOutput("map", width = '100%', height= '100%')
                                            )
),
             
#-------------second tab-----------------
             
  tabPanel('Past Year Transactions',div(class="outer", leafletOutput('map', width = '100%', height = '100%'))),
             
             
#-------------third tab------------------
             
             
             
  tabPanel('Data', fluidRow( selectInput(inputId = 'data', "Boroughs", choices = c('Manhattan','Brooklyn','Queens',
                                                                                              'Bronx','Staten Island'), selected = 'Manhattan'),
                  sliderInput("LowPriceRange", label = "Price Range From:", min = 100000, max = 10000000, value = 10000),
                  sliderInput('HighPriceRange', label = "Price Range To:", min = 100000, max = 10000000, value = 100000),
                                        
                  dataTableOutput("NYCfinal"))
                      
                      
             )
  )
)
)
