library(shiny)
library(tidyverse)
library(ggplot2)

delay = read_csv("tidied_data/delay.csv")

delay = 
  delay %>% 
  filter(delay_minutes > 0) %>% 
  group_by(month, day, airline_name) %>% 
  mutate(
    count = n(),
    mean = mean(delay_minutes)
  ) %>% 
  ungroup() %>% 
  mutate(
    month = recode(month, "11" = "Nov, 2021", "12" = "Dec, 2021", "1" = "Jan, 2022")
  ) 


airlines = delay %>% distinct(airline_name) %>% pull()



months = 
  delay %>% 
  distinct(month) %>% 
  pull()




# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Delay Dashboard"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #label for the drop down menu
      h6("Choose a Region to focus on"), 
      
      # Input: Drop down menu for plot ---- selecting region 
      selectInput(inputId = "airline_choice",
                  label = h3("Airline"),
                  choices = airlines,
                  selected = "JetBlue Airways"),
      
      #label for the drop down menu
      h6("Choose a type of data to be displayed on the y-axis"),
      
      # Input: Drop down menu for plot ---- selecting type of data
      radioButtons(inputId = "month_choice",
                   label = h3("Month"),
                   choices = months,
                   selected = "Nov, 2021"
        
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot ----
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "distTable")
      
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
    delay %>% 
      filter(
        airline_name == input[["airline_choice"]],
        month == input[["month_choice"]]) %>% 
      mutate(
        text_label = str_c("Date: ", date)
      ) %>% 
      ggplot(aes(x = as.factor(day), y = delay_minutes)) +
      geom_boxplot()
  })
  
  output$distTable <- renderPlot({
    delay %>%
      distinct(airline_name, date, month, day, year, count) %>% 
      filter(
        airline_name == input[["airline_choice"]],
        month == input[["month_choice"]]
      ) %>% 
      ggplot(aes(x = day, y = count)) +
      geom_col()
  })
    
}
  
# Create Shiny app ----
shinyApp(ui = ui, server = server)





