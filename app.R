library(tidyverse)
library(plotly)
library(shiny)

delay = read_csv("tidied_data/delay.csv")
cancel = read_csv("tidied_data/cancel.csv")
covid = read_csv("tidied_data/covid.csv")

delay = 
  delay %>% 
  filter(delay_minutes > 0) %>% 
  group_by(month, day, airline_name) %>% 
  mutate(
    count = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    month = recode(month, "11" = "Nov, 2021", "12" = "Dec, 2021", "1" = "Jan, 2022")
  )

cancel = 
  cancel %>% 
  group_by(month, day, airline_name) %>% 
  mutate(
    count = n()
  ) %>% 
  ungroup()

airline = delay %>% distinct(airline_name) %>% pull()
months = delay %>% distinct(month) %>% pull()
airlines = cancel %>% distinct(airline_name) %>% pull()

ui = fluidPage(
  
  headerPanel("Flight Delay and Cancelation Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
    conditionalPanel(
      condition = "input.tabselected == 1",
      selectInput(
        inputId = "airline_choice",
        label = h3("Airline"),
        choices = airline,
        selected = "JetBlue Airways"),
      radioButtons(
        inputId = "month_choice",
        label = h3("Month"),
        choices = months,
        selected = "Nov, 2021")),
    conditionalPanel(
      condition = "input.tabselected == 2",
      selectInput(
        inputId = "airline_choice",
        label = h3("Airline"),
        choices = airlines,
        selected = "JetBlue Airways"))),
    
  mainPanel(
    tabsetPanel(type = "tabs", id = "tabselected", selected = 1,
      tabPanel("Delay", fluidRow(plotlyOutput("delay_count"), plotlyOutput("delay_minute"), value = 1)),
      tabPanel("Cancelation", fluidRow(plotlyOutput("cancel_plot"), plotlyOutput("covid_plot")), value = 2))))
    
  )

server = function(input, output) {

  output$delay_count = renderPlotly({
    delay %>%
      distinct(airline_name, date, month, day, year, count) %>% 
      filter(
        airline_name == input[["airline_choice"]],
        month == input[["month_choice"]]
      ) %>% 
      mutate(
        text_label = str_c("Date: ", date, "\nCount: ", count)
      ) %>% 
      plot_ly(x = ~day, y = ~count, text = ~text_label,
              hoverinfo = "text", color = "rgb(255, 65, 54)",
              type = "bar", mode = "markers", alpha = .5) %>% 
      layout(
        xaxis = list(title = "Day"),
        yaxis = list(title = "Count"),
        title = "Number of Delays on Each Day")
  })
  
  output$delay_minute = renderPlotly({
    delay %>% 
      filter(
        airline_name == input[["airline_choice"]],
        month == input[["month_choice"]]
      ) %>% 
      mutate(
        text_label = str_c("Date: ", date)
      ) %>% 
      plot_ly(x = ~day, y = ~delay_minutes, text = ~text_label, hoverinfo = "text",
              color = "rgb(255, 65, 54)",
              type = "box", mode = "markers", alpha = .5) %>% 
      layout(
        xaxis = list(title = "Day"),
        yaxis = list(title = "Delay Time (Minutes)"),
        title = "Distribution of Delay Time on Each Day")
  })
  
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