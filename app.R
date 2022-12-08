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
  headerPanel(h3("Flight Cancelation and Delay")),
  sidebarPanel(
    selectInput(
      inputId = "airline_choice",
      label = h4("Airline"),
      choices = airlines,
      selected = "JetBlue Airways"),
    radioButtons(
      inputId = "month_choice",
      label = h4("Month"),
      choices = months,
      selected = "Dec, 2021")),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Cancelation", fluidRow(br(), plotlyOutput("cancel"))),
                tabPanel("Delay", fluidRow(br(), plotlyOutput("delay"))))))



server = function(input, output) {

  output$delay = renderPlotly({
    plot1 =
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
              type = "bar", mode = "markers", alpha = .5)
    
    plot2 = 
      delay %>% 
      filter(
        airline_name == input[["airline_choice"]],
        month == input[["month_choice"]]
      ) %>% 
      mutate(
        text_label = str_c("Date: ", date)
      ) %>% 
      plot_ly(x = ~day, y = ~delay_minutes,
              color = "rgb(255, 65, 54)",
              type = "box", mode = "markers", alpha = .5) 
    
    subplot(list(plot1, plot2), nrows = 1, margin = 0.06) %>% 
      layout(
        annotations = list(
          list(x = 0.08, y = 1.1, text = "Number of Delays by Day", font = list(size = 16), showarrow = F, xref = "paper", yref = "paper"),
          list(x = 0.92, y = 1.1, text = "Delay Time in Minutes by Day", font = list(size = 16), showarrow = F, xref = "paper", yref = "paper")
        ),
        showlegend = FALSE,
        margin = list(l = 50, r = 50, b = 50, t = 30)
      )
    
    
  })
  
  output$cancel = renderPlotly({
    plot3 = 
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
              type = "scatter", mode = "markers", opacity = .7)
    
    plot4 =
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
              type = "scatter", mode = "markers", opacity = .7)
    
    subplot(list(plot3, plot4), nrows = 1, margin = 0.06) %>% 
      layout(
        annotations = list(
          list(x = 0.06, y = 1.1, text = "Number of Cancelations by Day", font = list(size = 16), showarrow = F, xref = "paper", yref = "paper"),
          list(x = 0.94, y = 1.1, text = "Number of COVID Cases by Day", font = list(size = 16), showarrow = F, xref = "paper", yref = "paper")
        ),
        margin = list(l = 50, r = 50, b = 50, t = 30),
        showlegend = FALSE
      )
    
  })
    
  
}

shinyApp(ui, server)
