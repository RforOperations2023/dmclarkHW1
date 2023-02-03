# load the necessary libraries
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)



#Importing my data

US_Police_shootings_15_22 <- read.csv("~/dmclarkHW1/dmclarkHW1/US Police shootings in from 2015-22_alt_race.csv")

# create a basic shiny app
ui <- fluidPage(
  
  # add a title
  titlePanel("Police Shooting Data from 2015 thur 2022"),
  
  # add a side panel for the inputs
  sidebarLayout(
    sidebarPanel(
      
      #create a drop down selection for the race tied to the histogram
      selectInput("race_dropdown", "Select a Race for the Histogram:",
                  choices = unique(US_Police_shootings_15_22$race)),
      
      
      # create a numeric input for the threshold value
      numericInput("threshold", "Enter threshold value:", 
                   min = 0, max = 100, value = 50),
      
      # create a download button
      downloadButton("downloadData", "Download Data")
    ),
    
    # add a main panel for the outputs
    mainPanel(
      
      # create a tabset for the plots and table
      tabsetPanel(
        
        # create a tab for the line plot
        tabPanel("Violin Plot", plotOutput("violinPlot")),
        
        # create a tab for the bar plot
        tabPanel("Bar Plot", plotOutput("barPlot")),
        
        # create a tab for the histogram plot
        tabPanel("Histogram", plotOutput("histogram")),
        
        # create a tab for the data table
        tabPanel("Data Table", DT::dataTableOutput("dataTable"))
        
        
      )
    )
  )
)


server <- function(input, output) {
  
  shootings_by_date_race <- US_Police_shootings_15_22 %>% 
    group_by(date, race) %>% 
    summarise(count = n())
  
  
  output$violinPlot <- renderPlot({
    ggplot(US_Police_shootings_15_22, aes_string(x = "race", y = 'age', fill = 'race')) +
      geom_violin() +
      ggtitle('Age comparisons across races')
    
  })
  
  # create the bar plot
  output$barPlot <- renderPlot({
    ggplot(US_Police_shootings_15_22, aes_string(x = 'race', fill = "race")) +
      geom_bar(position = "dodge") +
      ggtitle('Fatal shooting by police across races ')
  })
  
  
  #create the Histogram
  output$histogram <- renderPlot({
    ggplot(shootings_by_date_race,
           aes(x = 'date', y = input$race_dropdown, ylab="Your Selected Race"))+
    geom_histogram(binwidth = 30)
      
  })
  
  # create the data table
  output$dataTable <- DT::renderDataTable({
    DT::datatable(subset(US_Police_shootings_15_22, US_Police_shootings_15_22$race > input$threshold))
  })
  
  # create the download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(US_Police_shootings_15_22, file)
    }
  )
}

# run the shiny app
shinyApp(ui = ui, server = server)
