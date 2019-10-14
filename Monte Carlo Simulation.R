#homework
install.packages("tidyverse")
install.packages("lubridate")
install.packages("readxl")
install.packages("highcharter")
install.packages("tidyquant")
install.packages("timetk")
install.packages("tibbletime")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("scales")
install.packages("zoo")
install.packages("xts")
install.packages("TTR")
install.packages("quantmod")
install.packages("ggplot2")
install.packages("purrr")
install.packages("shiny")

#清空工作空间
ls()
rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(tidyquant)
library(timetk)
library(tibbletime)
library(PerformanceAnalytics)
library(scales)
library(quantmod)
library(ggplot2)
library(purrr)
library(shiny)



install.packages('DBI')
install.packages('RMySQL')
library(DBI)
library(RMySQL)


#用Mysql接入实验室数据库
mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', 
                dbname='cn_stock_quote', host='172.19.3.250') 


#选取标的：国证银行（CN2431）、国证传媒（CN2434）、国证农牧（CN2435）、
#...国证煤炭（CN2436）、国证证券（CN2437）
#时间区间：2017-12-29至2018-12-31
SQL_statement<- "SELECT  `trade_date`,  `index_code`,  `index_name`,  `last`,  `input_time` 
FROM `cn_stock_index`.`daily_quote`
WHERE index_code IN ('CN2431','CN2434','CN2435','CN2436','CN2437') 
      AND trade_date between ('2017-12-29 00:00:00') and ('2018-12-31 00:00:00')
ORDER BY `index_name` DESC, `trade_date` DESC"
dbGetQuery(mydb,SQL_statement)

#将数据保存为csv文件
data<- dbGetQuery(mydb,SQL_statement)
write.table (data, file ="C:\\Users\\houyi\\Desktop\\data_mysql.csv", 
             sep =",", row.names =FALSE)
#文件读取
data=read.csv("C:\\Users\\houyi\\Desktop\\data_mysql.csv",header=TRUE)

#获取各标的的价格数据
symbols<-c("bank","media","agri","coal","secu")
p31=data[1:244,4]
p34=data[245:488,4]
p35=data[489:732,4]
p36=data[733:976,4]
p37=data[977:1220,4]
#将价格数据合并，形成新的数据框,
price=cbind(p31,p34,p35,p36,p37)
for(i in 1:5){
  price[,i]=rev(price[,i])
}

#将数据框改为时间序列格式
trade_date=data[1:244,1]
trade_date=rev(trade_date)
prices=as.xts(price,as.Date(trade_date))
colnames(prices)=symbols
head(prices,3)


#To Monthly Returns in the tidyverse
asset_returns_dplyr_byhand <- 
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  # convert the index to a date
  data.frame(date = index(.)) %>%
  # now remove the index because it got converted to row names
  remove_rownames() %>% 
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>%
  spread(asset, returns) %>% 
  select(date, symbols) %>% 
  na.omit()
head(asset_returns_dplyr_byhand, 3)

#Converting Daily Prices to Monthly Returns
asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, -date) %>%
  group_by(asset)
head(asset_returns_long, 3)
tail(asset_returns_long, 3)


#Visualizing Asset Returns Histogram One Chart in the tidyverse
asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = .01) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2018") +
  theme_update(plot.title = element_text(hjust = 0.5))

#Asset Returns Density
asset_returns_long %>%
  ggplot(aes(x = returns)) +
  geom_density(aes(color = asset), alpha = 1) +
  geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = .01) +
  guides(fill = FALSE) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2018") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))


#portfolio allocation
w <- c(0.25,
       0.25,
       0.20,
       0.20,
       0.10)

tibble(w, symbols)

#Portfolio Returns in the tidyquant world
portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")
head(portfolio_returns_tq_rebalanced_monthly)

#Monte Carlo Simulation
#By way of specific tasks we will do the following:
#  1) write several simulation functions
#  2) run several simulations with purrr
#  3) visualize our results with highcharter and ggplot2


#Simulating Growth of a Dollar
mean_port_return<- 
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

stddev_port_return<- 
  sd(portfolio_returns_tq_rebalanced_monthly$returns)

simulated_monthly_returns<- rnorm(120, 
                                  mean_port_return, 
                                  stddev_port_return)
head(simulated_monthly_returns)
## [1] 0.01872420  0.02454947 -0.01961409 -0.03504850 -0.01437761 0.03468959
tail(simulated_monthly_returns)
## [1] -0.039470922 -0.067176766  0.009024608  0.020852213 -0.012775129  -0.114466700

#how a dollar would have grown given those random monthly returns
simulated_returns_add_1 <- 
  tibble(c(1, 1 + simulated_monthly_returns)) %>% 
  `colnames<-`("returns")
head(simulated_returns_add_1)

#cumulative growth of a dollar(run 3 simulations of dollar growth over 120 months)
simulated_growth <-
  simulated_returns_add_1 %>%
  mutate(growth1 = accumulate(returns, function(x, y) x * y),
         growth2 = accumulate(returns, `*`),
         growth3 = cumprod(returns)) %>%
  select(-returns)
tail(simulated_growth)

cagr <-
  ((simulated_growth$growth1[nrow(simulated_growth)]^
      (1/10)) -1)*100
print(cagr)


#Several Simulation Functions
##function 1 using accumulate()
simulation_accum_1 <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>%
    `colnames<-`("returns") %>%
    mutate(growth =
             accumulate(returns,
                        function(x, y) x * y)) %>%
    select(growth)
}

##function2: similar to function 1
simulation_accum_2 <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>%
    `colnames<-`("returns") %>%
    mutate(growth = accumulate(returns, `*`)) %>%
    select(growth)
}

##function 3 using cumprod()
simulation_cumprod <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>%
    `colnames<-`("returns") %>%
    mutate(growth = cumprod(returns)) %>%
    select(growth)
}

##three functions
simulation_confirm_all <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>%
    `colnames<-`("returns") %>%
    mutate(growth1 = accumulate(returns, function(x, y) x * y),
           growth2 = accumulate(returns, `*`),
           growth3 = cumprod(returns)) %>%
    select(-returns)
}

## test that confirm_all() function with an init_value of 1, N of 120
simulation_confirm_all_test <-
  simulation_confirm_all(1, 120,
                         mean_port_return, stddev_port_return)
tail(simulation_confirm_all_test)



#run 51 simulations to see how the randomness is distributed
##create 51 columns with a 1 as the value
sims <- 51
starts <-
  rep(1, sims) %>%
  set_names(paste("sim", 1:sims, sep = ""))
head(starts)
tail(starts)


#apply function 1 to each of the 51 columns 
monte_carlo_sim_51 <-
  map_dfc(starts, simulation_accum_1,
          N = 120,
          mean = mean_port_return,
          stdev = stddev_port_return)
tail(monte_carlo_sim_51 %>%
       select(growth1, growth2,
              growth49, growth50), 3)

#模拟未来120个月的增长
monte_carlo_sim_51 <-
  monte_carlo_sim_51 %>%
  mutate(month = seq(1:nrow(.))) %>%
  select(month, everything()) %>%
  `colnames<-`(c("month", names(starts))) %>%
  mutate_all(funs(round(., 2)))
tail(monte_carlo_sim_51 %>% select(month, sim1, sim2,
                                   sim49, sim50), 3)

##another method:using return() function
monte_carlo_rerun_5 <-
  rerun(.n = 5,                   #.n =number of times to rerurn
        simulation_accum_1(1,
                           120,
                           mean_port_return,
                           stddev_port_return))

map(monte_carlo_rerun_5, head)

###合并结果
reruns <- 51
monte_carlo_rerun_51 <-
  rerun(.n = reruns,
        simulation_accum_1(1,
                           120,
                           mean_port_return,
                           stddev_port_return)) %>%
  simplify_all() %>%
  `names<-`(paste("sim", 1:reruns, sep = " ")) %>%
  as_tibble() %>%
  mutate(month = seq(1:nrow(.))) %>%
  select(month, everything())
tail(monte_carlo_rerun_51 %>%
       select(`sim 1`, `sim 2`,
              `sim 49`, `sim 50`), 3)


#Visualizing Simulations with ggplot
monte_carlo_sim_51 %>%
  gather(sim, growth, -month) %>%
  group_by(sim) %>%
  ggplot(aes(x = month, y = growth, color = sim)) +
  geom_line() +
  theme(legend.position="none")


# check the minimum, maximum and median simulation with the summarise() function 
sim_summary <-
  monte_carlo_sim_51 %>%
  gather(sim, growth, -month) %>%
  group_by(sim) %>%
  summarise(final = last(growth)) %>%
  summarise(
    max = max(final),
    min = min(final),
    median = median(final))
sim_summary


# tweak visualization by including only the maximum, minimum and median simulation result
monte_carlo_sim_51 %>%
  gather(sim, growth, -month) %>%
  group_by(sim) %>%
  filter(
    last(growth) == sim_summary$max ||
      last(growth) == sim_summary$median ||
      last(growth) == sim_summary$min) %>%
  ggplot(aes(x = month, y = growth)) +
  geom_line(aes(color = sim))


# assign different probability values to a vector
probs <- c(.005, .025, .25, .5, .75, .975, .995)


# create a new object calle sim_final_quantile to hold final values of 51 dollar growth simulations.
sim_final_quantile <-
  monte_carlo_sim_51 %>%
  gather(sim, growth, -month) %>%
  group_by(sim) %>%
  summarise(final = last(growth))

# use the quantile() function on sim_final_quantile$final and pass in our vector of probability values which we assigned
quantiles <-
  quantile(sim_final_quantile$final, probs = probs) %>%
  tibble() %>%
  `colnames<-`("value") %>%
  mutate(probs = probs) %>%
  spread(probs, value)
quantiles[, 1:6]



## convert xts to a tidy tibble, we use the gather() function from tidyr.
mc_gathered <-
  monte_carlo_sim_51 %>%
  gather(sim, growth, -month) %>%
  group_by(sim)


#plotted 51 lines in highcharter using a tidy tibble
hchart(mc_gathered,
       type = 'line',
       hcaes(y = growth,
             x = month,
             group = sim)) %>%
  hc_title(text = "51 Simulations") %>%
  hc_xAxis(title = list(text = "months")) %>%
  hc_yAxis(title = list(text = "dollar growth"),
           labels = list(format = "${value}")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)


# isolate the maximum, minimum and median simulations and save them to an object
mc_max_med_min <-
  mc_gathered %>%
  filter(
    last(growth) == sim_summary$max ||
      last(growth) == sim_summary$median ||
      last(growth) == sim_summary$min) %>%
  group_by(sim)



# pass the filtered object to hchart()
hchart(mc_max_med_min,
       type = 'line',
       hcaes(y = growth,
             x = month,
             group = sim)) %>%
  hc_title(text = "Min, Max, Median Simulations") %>%
  hc_xAxis(title = list(text = "months")) %>%
  hc_yAxis(title = list(text = "dollar growth"),
           labels = list(format = "${value}")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)

------------------------------------------------------------
#Shiny App Monte Carlo
##build Shiny app wherein a user can choose a number of simulations and a number of months into the future
  fluidRow(
    column(5,numericInput("sim_months", "Months", 120,
                          min = 6, max = 240, step = 6)),
    column(5,numericInput("sims", "Sims", 51,
                          min = 31, max = 101, step = 10))
  )

##calculate mean and standard deviation of portfolio returns  and save them as portfolio_returns_tq_rebalanced_monthly 
mean_port_return <- eventReactive(input$go, {
  portfolio_returns_tq_rebalanced_monthly <-
    portfolio_returns_tq_rebalanced_monthly()
  mean(portfolio_returns_tq_rebalanced_monthly$returns)
})
stddev_port_return <- eventReactive(input$go, {
  portfolio_returns_tq_rebalanced_monthly <-
    portfolio_returns_tq_rebalanced_monthly()
  sd(portfolio_returns_tq_rebalanced_monthly$returns)
})

#insert one of  simulation functions
simulation_accum_1 <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>%
    `colnames<-`("returns") %>%
    mutate(growth =
             accumulate(returns, function(x, y) x * y)) %>%
    select(growth)
}

# call eventReactive() to run the simulation
sims <- eventReactive(input$go, {input$sims})
monte_carlo_sim <- eventReactive(input$go, {
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

#visualize with highcharter() 
renderHighchart(
  hchart( monte_carlo_sim(),
          type = 'line',
          hcaes(y = growth,
                x = month,
                group = sim)) %>%
    hc_title(text = paste(sims(),
                          "Simulations",
                          sep = " ")) %>%
    hc_xAxis(title = list(text = "months")) %>%
    hc_yAxis(title = list(text = "dollar growth"),
             labels = list(format = "${value}")) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
)



# isolate the minimum, median and maximum simulations
renderHighchart({
  sim_summary <-
    monte_carlo_sim() %>%
    summarise(final = last(growth)) %>%           
    summarise(
      max = max(final),
      min = min(final),
      median = median(final))
  mc_max_med_min <-
    monte_carlo_sim() %>%
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





