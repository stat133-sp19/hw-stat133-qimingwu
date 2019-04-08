library(shiny)
library(ggplot2)

ui <- fluidPage(
   
   # Application title
   titlePanel("Investment Returns of Different Saving Modalities"),
   
   fluidRow(
     column(4, 
            sliderInput(inputId = "amount",
                        label = "Initial Amount",
                        min = 0,
                        max = 100000,
                        step = 500,
                        value = 1000)),
     column(4, 
            sliderInput(inputId = "rate",
                        label = "Return Rate (in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 5)),
     column(4, 
            sliderInput(inputId = "years",
                        label = "Years",
                        min = 0,
                        max = 50,
                        step = 1,
                        value = 20))
   ),
   
   fluidRow(
     column(4, 
            sliderInput(inputId = "contrib",
                        label = "Annual Contribution",
                        min = 0,
                        max = 50000,
                        step = 500,
                        value = 2000)),
     column(4, 
            sliderInput(inputId = "growth",
                        label = "Growth Rate (in %)",
                        min = 0,
                        max = 20,
                        step = 0.1,
                        value = 2)),
     column(4,
            selectInput(inputId = "facet",
                        label = "Facet?",
                        choices = c("No", "Yes"),
                        selected = "No"))
     ),
   
   fluidRow(
     column(4,
            h4("Timelines"))
   ),
   
   fluidRow(
     column(12,
            plotOutput("timelines"))
   ),
   
   fluidRow(
     column(4,
            h4("Balances"))
   ),
   
   fluidRow(
     column(12,
            tableOutput("balances"))
   )
)


server <- function(input, output) {
  
  future_value <- function(amount, rate, years) {
      return (amount * (1 + rate)^years)
  }
  
  annuity <- function(contrib, rate, years) {
      return (contrib * ((1 + rate)^years - 1) / rate)
  }
  
  growing_annuity <- function(contrib, rate, growth, years) {
      return (contrib * (((1 + rate)^years - (1 + growth)^years) / (rate - growth)))
  }
  
  years <- reactive({
    0:input$years
  })
  
  mode1 <- reactive({
    mode1 <- c()
    for (i in 0:input$years) {
      mode1[i + 1] <- future_value(amount = input$amount, rate = input$rate / 100, years = i)
    }
    mode1
  })
  
  mode2 <- reactive({
    mode2 <- c()
    for (i in 0:input$years) {
      mode2[i + 1] <- future_value(amount = input$amount, rate = input$rate / 100, years = i) + annuity(contrib = input$contrib, rate = input$rate / 100, years = i)
    }
    mode2
  })
  
  mode3 <- reactive({
    mode3 <- c()
    for (i in 0:input$years) {
      mode3[i + 1] <- future_value(amount = input$amount, rate = input$rate / 100, years = i) + growing_annuity(contrib = input$contrib, rate = input$rate / 100, growth = input$growth / 100, years = i)
    }
    mode3
  })
  
  
  # Create the data frame for non-facet graphs and balances display
  modalities <- reactive({
    modalities <- data.frame(years(), mode1(), mode2(), mode3(), stringsAsFactors = FALSE)
    names(modalities) <- c("year", "no_contrib", "fixed_contrib", "growing_contrib")
    modalities
  })

  # Create ggplot object for non-facet graphs
  no_facet <-  reactive({
    ggplot(data = modalities()) + 
      geom_line(aes(x = year, y = no_contrib, color = "No Contribution"), size = 1) +
      geom_point(aes(x = year, y = no_contrib, color = "No Contribution")) +
      geom_line(aes(x = year, y = fixed_contrib, color = "Fixed Contribution"), size = 1) +
      geom_point(aes(x = year, y = fixed_contrib, color = "Fixed Contribution")) +
      geom_line(aes(x = year, y = growing_contrib, color = "Growing Contribution"), size = 1) +
      geom_point(aes(x = year, y = growing_contrib, color = "Growing Contribution")) +
      xlab("Number of Years") +
      ylab("Total Value") +
      ggtitle("Three modes of investment") +
      theme_grey() +
      scale_color_manual(name = "Mode", values = c("#7f00ff", "#108945", "#ff0000"))
  })

  # Create another data frame for facet graphs
  df2 <- reactive({
    year <- rep(years(), 3)
    mode <- c(rep("No contribution", input$years + 1), rep("Fixed contribution", input$years + 1), rep("Growing contribution", input$years + 1))
    value <- c(mode1(), mode2(), mode3())
    df2 <- data.frame(mode, year, value, stringsAsFactors = FALSE)
    df2
  })
  
  # Create ggplot object for facet graphs
  with_facet <- reactive({
    ggplot(data = df2()) +
      geom_line(aes(x = year, y = value, color = mode)) +
      geom_point(aes(x = year, y = value, color = mode)) +
      geom_area(aes(x = year, y = value, fill = mode)) +
      facet_grid(~ mode) +
      ggtitle("Return of Investment for Different Investment Modes") +
      xlab("Number of Years") +
      ylab("Total Value") +
      scale_color_manual(values = c("#ff0000", "#108945", "#7f00ff")) +
      theme_bw()
  })
  
   
  output$timelines <- renderPlot(
    if (input$facet == "No") {
      no_facet()
    } else {
      with_facet()
    }
   )
  
  output$balances <- renderTable(
    modalities()
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
