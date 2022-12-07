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
    cancel_count = n()
  ) %>% 
  ungroup() 

covid_cancel = right_join(cancel, covid, by = c("year", "month", "day")) %>% 
  select(-date.x) %>% 
  rename(date = date.y)

covid_cancel = 
  covid_cancel %>% 
  mutate(
    month = recode(month, "11" = "Nov, 2021", "12" = "Dec, 2021", "1" = "Jan, 2022")
  )

airlines = cancel %>% arrange(airline_name) %>% distinct(airline_name) %>% pull()
months = delay %>% distinct(month) %>% pull()

ui = fluidPage(
  headerPanel(h3("Flight Delay and Cancelation")),
  sidebarPanel(
    selectInput(
      inputId = "airline_choice",
      label = h4("Airline"),
      choices = airlines,
      selected = "JetBlue Airways"
    ),
    radioButtons(
      inputId = "month_choice",
      label = h4("Month"),
      choices = months,
      selected = "Dec, 2021")
   ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Delay", fluidRow(br(), plotlyOutput("delay_count"), br(),
                                           br(), plotlyOutput("delay_minute"))),
                tabPanel("Cancelation", fluidRow(br(), plotlyOutput("cancel_plot"), br(),
                                                 br(), plotlyOutput("covid_plot"))))
  )
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
    covid_cancel %>% 
      distinct(airline_name, date, month, day, year, cancel_count) %>% 
      filter(
        airline_name == input[["airline_choice"]],
        month == input[["month_choice"]]
      ) %>% 
      mutate(
        text_label = str_c("Date: ", date, "\nCount: ", cancel_count)
      ) %>% 
      plot_ly(x = ~day, y = ~cancel_count, color = "rgb(255, 65, 54)",
              size = ~cancel_count, sizes = c(10, 100), text = ~text_label,
              hoverinfo = "text",
              type = "scatter", mode = "markers", opacity = .7
      ) %>% 
      layout(
        xaxis = list(title = "Day"),
        yaxis = list(title = "Count"),
        title = "Number of Cancelations on Each Day"
      )
  })
    
  output$covid_plot = renderPlotly({
    covid_cancel %>% 
      filter(
        month == input[["month_choice"]]
      ) %>%
      mutate(
        text_label = str_c("Date: ", date, "\nCase Count: ", case_count)
      ) %>% 
      plot_ly(x = ~day, y = ~case_count, color = "rgb(255, 65, 54)",
              size = ~case_count, sizes = c(10, 100),
              text = ~text_label, hoverinfo = "text",
              type = "scatter", mode = "markers", opacity = .7
      ) %>% 
      layout(
        xaxis = list(title = "Day"),
        yaxis = list(title = "Case Count"),
        title = "Number of COVID Cases on Each Day"
    )
    
  })
  
}

shinyApp(ui, server)
