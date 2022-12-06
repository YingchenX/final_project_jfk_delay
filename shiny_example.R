library(shiny)
library(tidyverse)
library(ggplot2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("State Exploration"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
    
      #label for the drop down menu
      h6("Choose a Region to focus on"), 
      
      # Input: Drop down menu for plot ---- selecting region 
      selectInput(inputId = "region1", label = strong("Region"),
                  choices = c("Northeast"     = "Northeast",
                              "South"         = "South",
                              "North Central" = "North Central",
                              "West"          = "West"),
                  selected = "Northeast"),
      
      #label for the drop down menu
      h6("Choose a type of data to be displayed on the y-axis"),
      
      # Input: Drop down menu for plot ---- selecting type of data
      radioButtons(inputId = "info", label = strong("Type of Data"),
                  choices = c("Income"   = "Income", 
                              "Life.Exp" = "Life.Exp",
                              "Murder"   = "Murder",
                              "Area"     = "Area"), 
                  selected = "Income")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot ----
      plotOutput(outputId = "distPlot"),
      tableOutput(outputId = "distTable")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Plot of the State Data by Region ----
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    # pulling the info from user input 
    region1 <- input$region1 
    info_st <- input$info 
    
    # creating the database
    state.df <- data.frame(state.abb,state.region,state.division,state.x77)

    # subsectioning the data by region
    subset <- state.df[state.region == region1,] 
    
    # plotting the desired region
    tibble(state_abb = subset$state.abb, info_selected = subset[,info_st]) %>%
       ggplot(aes(x=state_abb,y=info_selected)) +
           geom_bar(stat = "identity") + 
       labs(
         title = str_c(info_st, " data on ", region1, " Region in the USA"),
         y = info_st,
         x = "State"
       )
    
  })
  
  output$distTable <- renderTable({
    # pulling the info from user input 
    region1 <- input$region1 
    info_st <- input$info 
    
    # creating the database of interest 
    state.df <- data.frame(state.abb,state.region,state.division,state.x77) 

    # sub-sectioning the data by region
    subset <- state.df[state.region == region1,]
    
    # creating a table with the information of interest 
    table = data.frame(state_abb = factor(subset$state.abb), 
                       info_selected = subset[,info_st])
    
    # renaming the columns 
    colnames(table) = c("State", info_st)
    
    # output the table 
    table
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)



