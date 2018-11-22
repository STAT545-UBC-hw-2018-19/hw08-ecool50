library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(DT)
library(rsconnect)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  
 
  
  #add a theme selector so the user can decide what theme to view the app in
  themeSelector(),
  
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    
    sidebarPanel(
      
      
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      
      
      radioButtons("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
      
      uiOutput("countryOutput"),
      
      checkboxInput("sort", "Sort by Price", value = FALSE)
    ),
    
    mainPanel(
      #create the tab panels
      tabsetPanel(type = "tabs",
                 
                  tabPanel("Plot", plotOutput("coolplot")),
                  tabPanel("Results", dataTableOutput("results"))
                  )
      
    )
  ),
  
  img(src = "BC_Liquor_Stores.png")
)

#setup the server function
server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    #if we are sorting by price
    if(input$sort){
      
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      ) %>% arrange(Price)
    }
    
    #if we are not
    else{
        
      bcl %>%
        filter(Price >= input$priceInput[1],
               Price <= input$priceInput[2],
               Type == input$typeInput,
               Country == input$countryInput
        )
      
      
    }
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  output$results <- renderDataTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)
