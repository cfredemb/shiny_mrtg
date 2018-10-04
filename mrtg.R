library(ggplot2)
require(scales)
#master file for repayments
#computes projection from start of mortgage to repayment over the 3 year period


#read the actual data from the file
aa <- read.csv("~/Desktop/mrtg_actuals.csv")

actual_mrtg <- aa$due
actual_dates <- as.Date(aa$actual_date,format="%d/%m/%y")
actual_cash <- aa$cash
actual_offset <- aa$offset

current_df <- cbind.data.frame(actual_dates,actual_mrtg,actual_cash,actual_offset)

#start with outstanding debt
base_owed <- 484000

#base amount of money held in cash
base_offset <- 60000
base_cash <- 66666.67
#interest rate: put at 4%
interest_annual <- 0.04

interest_daily <- interest_annual/365

#repayments
annual_increase_cash <- 20000 #from B and C

monthly_offset <- 8000 #from B and C
monthly_repayment <- 2250
#principal_repay <- 
#  monthly_repayment -
#    (current_outstanding - (current_offset + current_cash))*interest_daily*30



timeline <- seq(as.Date("2017/05/26"),as.Date("2020/12/26"),by="month")

for (i in 1:length(timeline)){
  if(timeline[i] < as.Date("2018/01/01")){
    monthly_offset = 5700
  }else{monthly_offset=8000}
  if(i==1){
    current_offset <- base_offset
    current_cash <- base_cash
    principal_repay <- 
      monthly_repayment -
      (base_owed - (current_offset + current_cash))*interest_daily*30.42
    current_owed <- base_owed - principal_repay
  }else{
    current_offset[i] <- current_offset[i-1] + (monthly_offset-monthly_repayment)
    current_cash[i] <- current_cash[i-1] + annual_increase_cash/12
    principal_repay <- 
      monthly_repayment -
      (current_owed[i-1] - (current_offset[i-1] + current_cash[i-1]))*interest_daily*30
    current_owed[i] <- current_owed[i-1] - principal_repay
    
  }
  
}

total_outstanding <- current_owed - current_offset - current_cash
toto <- cbind.data.frame(timeline,current_owed,current_cash,current_offset,total_outstanding)



ggplot(toto,aes(timeline,current_owed,colour='mrtg')) + 
  geom_line(linetype="dashed") + 
  geom_line(data = toto,aes(timeline,current_cash,colour='cash'),linetype="dashed") +
  geom_line(data = toto,aes(timeline,current_offset,colour='offset'),linetype="dashed") +
  geom_line(data = toto,aes(timeline,total_outstanding,colour='total'),linetype="dashed") +
  geom_point(data = current_df,aes(actual_dates,actual_mrtg,colour='mrtg')) +
  geom_point(data = current_df,aes(actual_dates,actual_cash,colour='cash')) +
  geom_point(data = current_df,aes(actual_dates,actual_offset,colour='offset')) +
  geom_point(data = current_df,aes(actual_dates,(actual_mrtg-actual_cash-actual_offset),colour='total')) +
  scale_x_date(name="calendar date",date_breaks="3 months") +
  scale_y_continuous(name="buckeroos",breaks = scales::pretty_breaks(n = 20)) +
  scale_color_brewer(palette='Dark2') + theme(legend.position = 'top')

