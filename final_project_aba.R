
library("tidyverse")
library("readxl")
library("dplyr")
library("lubridate")
library("date")
library('forecast')
library('zoo')

df<-read_excel('SalesDataF.xlsx')


df<-df[order(as.Date(df$`Order Date`, format="%Y/%m/%d")),]



df$week <- floor_date(df$`Order Date`, "week")
df$week



df$month <- floor_date(df$`Order Date`, "month")
df$month





new.data.sales.month<-df %>%
  group_by(month,`Sub-Category`) %>%
  summarize(sales_sum = sum(Sales))

new.data.profit.month<-df %>%
  group_by(month,`Sub-Category`) %>%
  summarize(avg_profit = mean(Profit))

View(new.data.sales.month)

subCategories <- unique(sales.data$Sub.Category)

for (i in subCategories) {
  
  subcategory_sdata <- new.data.sales.month[new.data.sales.month['Sub-Category']==i,]
  View(subcategory_sdata)
  df_subcategory.ts=ts(subcategory_sdata$sales_sum, start = c(2013, 12), end = c(2017, 12), freq = 12)
  decompose(df_subcategory.ts) %>% plot()
  df_subcategory.ts
  nValid<-12
  nTrain<-length(df_subcategory.ts)-nValid
  train_subcategory.ts <- window(df_subcategory.ts, start = c(2013, 12), end = c(2013, 11+nTrain)) #partion data
  valid_subcategory.ts <- window(df_subcategory.ts, start = c(2013, 11+nTrain + 1), end = c(2013, 11+nTrain + nValid))
  train_subcategory.ts
  
  
  
  
  #Decomposition
  train_subcategory.qm<-tslm(train_subcategory.ts~trend+season)
  fore_subcategory.qm<-forecast(train_subcategory.qm,h=12)
  # accuracy(fore_subcategory.qm$mean,valid_subcategory.ts)
  fore_subcategory.qm.mse<-mean((fore_subcategory.qm$mean-valid_subcategory.ts)^2)^0.5
  fore_subcategory.qm.mse
  
  
  
  #Auto-ARIMA
  train_subcategory.auto<-auto.arima(train_subcategory.ts)
  fore_subcategory.auto<-forecast(train_subcategory.auto,h=12)
  # accuracy(fore_subcategory.auto$mean,valid_subcategory.ts)
  fore_subcategory.auto.mse<-mean((fore_subcategory.auto$mean-valid_subcategory.ts)^2)^0.5
  fore_subcategory.auto.mse
  
  
  
  #Holt's Winter
  hwin<-ets(train_subcategory.ts,model="ZZZ")
  fore_subcategory.hw<-forecast(hwin,h=12)
  # accuracy(fore_subcategory.hw$mean,valid_subcategory.ts)
  fore_subcategory.hw.mse<-mean((fore_subcategory.hw$mean-valid_subcategory.ts)^2)^0.5
  fore_subcategory.hw.mse
  
  
  
  
  mse <- c(fore_subcategory.qm.mse, fore_subcategory.auto.mse, fore_subcategory.hw.mse)
  min(mse)
  which.min(mse)
  
  
  
  
  modellist <- list()
  modellist[[1]] <- 'tslm(df_subcategory.ts~trend+season)'
  modellist[[2]] <- 'auto.arima(df_subcategory.ts)'
  modellist[[3]] <- 'ets(df_subcategory.ts,model="ZZZ")'
  
  
  
  modellist[[which.min(mse)]]
  
  
  
  
  subcategory.final <- eval(parse(text=modellist[[which.min(mse)]]))
  subcategory.final
  
  
  
  subcategory.qpred <- forecast(subcategory.final, h=12)
  subcategory.qpred
  print(i)
  print(modellist[[which.min(mse)]])
}


