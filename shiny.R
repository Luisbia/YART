library(tidyverse)
library(shiny)

tmp1 <- readxl::read_xlsx(
  path = "data/df_publication_2023.xlsx",
  sheet = "data"
) %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "time_period",
    values_to = "obs_value"
  ) %>%
  mutate(
    time_period = as.integer(time_period),
    NUTS = as.factor(NUTS),
    obs_value = as.numeric(obs_value)
  ) %>% 
  filter(Country == "EL" & vintage =="V2023")

# Define UI for app that draws line chart----
ui <- fluidPage(

  # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput("time_period", "Years:",
        min = min(tmp1$time_period),
        max = max(tmp1$time_period),
        value = c(min(tmp1$time_period), max = max(tmp1$time_period)),
        step = 1,
        round = TRUE
      ),
      selectInput("geo", "Geo", 
                  choices = unique(tmp1$geo), 
                  selected = unique(tmp1$geo),
                  multiple = TRUE),
      selectInput("unit", "Unit", choices = unique(tmp1$unit),selected="EUR_HAB")),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Line chart ----
        plotOutput(outputId = "Plot")
      )
    )
  )


# Define server logic required to draw the chart ----
server <- function(input, output) {
  
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs change
  # 2. Its output type is a plot

    
    tmp1_f<- reactive({tmp1 %>%  
        filter(geo %in% input$geo &
                unit %in% input$unit &
        time_period >= input$time_period[1] &
        time_period <= input$time_period[2]) })
    
    output$Plot <- renderPlot({   
    ggplot(tmp1_f(), aes(x=time_period, y = obs_value, colour = geo ))+
      geom_line()+
      theme_minimal()+
      scale_colour_viridis_d()
    
  })
  
}

shinyApp(ui = ui, server = server)
