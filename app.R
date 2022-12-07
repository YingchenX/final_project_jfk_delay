library(tidyverse)
library(plotly)
library(shiny)

cancel = read_csv("tidied_data/cancel.csv")

cancel = 
  cancel %>% 
  group_by(month, day, airline_name) %>% 
  mutate(
    count = n()
  ) %>% 
  ungroup()

covid = read_csv("tidied_data/covid.csv")

airlines = cancel %>% distinct(airline_name) %>% pull()

ui = fluidPage(
  headerPanel("Cancelation vs COVID Case Count Dashboard"),
  sidebarPanel(
    selectInput(
      inputId = "airline_choice",
      label = h3("Airline"),
      choices = airlines,
      selected = "JetBlue Airways"
    )
   ),
  mainPanel(
    plotlyOutput("cancel_plot"),
    plotlyOutput("covid_plot")
  )
)

server = function(input, output) {

  output$cancel_plot = renderPlotly({
    cancel %>% 
      filter(
        airline_name == input[["airline_choice"]]
      ) %>% 
      mutate(
        text_label = str_c("Date: ", date, "\nCount: ", count)
      ) %>% 
      plot_ly(x = ~date, y = ~count, color = "rgb(255, 65, 54)",
              size = ~count, sizes = c(10, 100), text = ~text_label,
              hoverinfo = "text",
              type = "scatter", mode = "markers", opacity = .7
      ) %>% 
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Count"),
        title = "Number of Cancelations on Each Day"
      )
  })
    
  output$covid_plot = renderPlotly({
    covid %>% 
      mutate(
        text_label = str_c("Date: ", date, "\nCase Count: ", case_count)
      ) %>% 
      plot_ly(x = ~date, y = ~case_count, color = "rgb(255, 65, 54)",
              text = ~text_label, hoverinfo = "text",
              type = "scatter", mode = "lines", opacity = .7
      ) %>% 
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Case Count"),
        title = "Number of COVID Cases on Each Day"
    )
    
  })
  
}

shinyApp(ui, server)