#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(highcharter)
library(tidyquant)
library(timetk)
library(scales)
library(shiny)

ui <- fluidPage(
    # Define UI for application that draws a histogram
  sidebarLayout(
    sidebarPanel(
      fluidRow(        
        column(6,
               textInput("stock1", "Stock 1", "SPY")),
        column(5,
               numericInput("w1", "Portf. %", 25, min = 1, max = 100))
      ),  
      
      fluidRow(
        column(6,
               textInput("stock2", "Stock 2", "EFA")),
        column(5,
               numericInput("w2", "Portf. %", 25, min = 1, max = 100))
      ),
      
      fluidRow(
        column(6,
               textInput("stock3", "Stock 3", "IJS")),
        column(5,
               numericInput("w3", "Portf. %", 20, min = 1, max = 100))
      ),
      
      fluidRow(
        column(6,
               textInput("stock4", "Stock 4", "EEM")),
        column(5,
               numericInput("w4", "Portf. %", 20, min = 1, max = 100))
      ),
      
      fluidRow(
        column(6,
               textInput("stock5", "Stock 5", "AGG")),
        column(5,
               numericInput("w5", "Portf. %", 10, min = 1, max = 100))
      ),
      
      fluidRow(
        column(7,
               dateInput("date", "Starting Date", "2013-01-01", format = "yyyy-mm-dd"))
      ),
      
      fluidRow(
        column(5,
               numericInput("sim_months", "Months", 120, min = 6, max = 240, step = 6)),
        column(5,
               numericInput("sims", "Sims", 51, min = 31, max = 101, step = 10))
      ),
      
      actionButton("go", "Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Sim"),
      plotOutput("min_max_sim")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  prices <- eventReactive(input$go, {
    
    symbols <- c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
    
    getSymbols(symbols, src = 'yahoo', from = input$date, 
               auto.assign = TRUE, warnings = FALSE) %>% 
      map(~Ad(get(.))) %>% 
      reduce(merge) %>%
      `colnames<-`(symbols)
  })
  
  
  portfolio_returns_tq_rebalanced_monthly <- eventReactive(input$go, {
    
    prices <- prices()
    w <- c(input$w1/100, input$w2/100, input$w3/100, input$w4/100, input$w5/100)
    
    portfolio_returns_tq_rebalanced_monthly <- 
      prices %>% 
      to.monthly(indexAt = "last", OHLC = FALSE) %>% 
      tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
      gather(asset, returns, -date) %>% 
      group_by(asset) %>%  
      mutate(returns = (log(returns) - log(lag(returns)))) %>%
      tq_portfolio(assets_col  = asset, 
                   returns_col = returns,
                   weights     = w,
                   col_rename  = "returns",
                   rebalance_on = "months")
  })
  
  mean_port_return <- eventReactive(input$go, {
    
    portfolio_returns_tq_rebalanced_monthly <- portfolio_returns_tq_rebalanced_monthly()
    
    mean(portfolio_returns_tq_rebalanced_monthly$returns)
  })
  
  stddev_port_return <- eventReactive(input$go, {
    
    portfolio_returns_tq_rebalanced_monthly <- portfolio_returns_tq_rebalanced_monthly()
    
    sd(portfolio_returns_tq_rebalanced_monthly$returns)
  })
  
  simulation_accum_1 <- function(init_value, N, mean, stdev) {
    tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
      `colnames<-`("returns") %>%
      mutate(growth = accumulate(returns, function(x, y) x * y)) %>% 
      select(growth)
  }
  
  sims <- eventReactive(input$go, {input$sims})
  
  monte_carlo_sim_51 <- eventReactive(input$go, { 
    
    sims <- sims()
    
    starts <-  
      rep(1, sims) %>%
      set_names(paste("sim", 1:sims, sep = ""))
    
    map_dfc(starts, simulation_accum_1,
            N = input$sim_months, mean = mean_port_return(), 
            stdev = stddev_port_return()) %>% 
      mutate(month = seq(1:nrow(.))) %>% 
      select(month, everything()) %>% 
      `colnames<-`(c("month", names(starts))) %>% 
      gather(sim, growth, -month) %>% 
      group_by(sim) %>% 
      mutate_all(funs(round(., 2)))
    
  })
  
  output$Sim<- renderPlot(
      hchart( monte_carlo_sim_51(), 
              type = 'line', 
              hcaes(y = growth,
                    x = month,
                    group = sim)) %>% 
        hc_title(text = paste(sims(), "Simulations", sep = " ")) %>%
        hc_xAxis(title = list(text = "months")) %>%
        hc_yAxis(title = list(text = "dollar growth"),
                 labels = list(format = "${value}")) %>%
        hc_add_theme(hc_theme_flat()) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_legend(enabled = FALSE)
    )


  output$min_max_sim<- renderPlot({
      
      sim_summary <- 
        monte_carlo_sim_51() %>%
        summarise(final = last(growth)) %>% 
        summarise(
          max = max(final), 
          min = min(final),
          median = median(final))
      
      mc_max_med_min <- 
        monte_carlo_sim_51() %>%
        filter(
          last(growth) == sim_summary$max || 
            last(growth) == sim_summary$median ||
            last(growth) == sim_summary$min)
      
      hchart(mc_max_med_min, 
             type = 'line', 
             hcaes(y = growth,
                   x = month,
                   group = sim)) %>% 
        hc_title(text = "Min Max Median Simulations") %>%
        hc_xAxis(title = list(text = "months")) %>%
        hc_yAxis(title = list(text = "dollar growth"),
                 labels = list(format = "${value}")) %>%
        hc_add_theme(hc_theme_flat()) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_legend(enabled = FALSE)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

