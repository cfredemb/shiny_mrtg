library(shiny)
library(ggplot2)
require(scales)
require(tidyquant)
#master file for repayments
#computes projection from start of mortgage to repayment over the 3 year period


#read the actual data from the file
#structure as per github project 
aa <- read.csv("~/data/mrtg_actuals.csv")

#get the variables, maybe not wholly necessary... could use the variable names, still need to reformat dates though
actual_mrtg   <- aa$due
actual_dates  <- as.Date(aa$actual_date,format="%d/%m/%y")
actual_cash   <- aa$cash
actual_offset <- aa$offset
current_df    <- cbind.data.frame(actual_dates,actual_mrtg,actual_cash,actual_offset)
current_df$total <- current_df$actual_mrtg - current_df$actual_cash - current_df$actual_offset

########################
####BASIC PARAMETERS####
########################
#start with outstanding debt (may2017)
base_owed <- 484000
#base amount of money held in cash at the start of the period (may 2017)
base_offset <- 60000
base_cash <- 66666.67
#interest rate: put at 4%
interest_annual <- 0.04
interest_daily <- interest_annual/365
#repayments

#target increase in cash holdings
annual_increase_cash <- 20000 #from B and C

#target monthly global offset repay
monthly_offset <- 8000 #from B and C
#actual repayment
monthly_repayment <- 2250



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

##last row of data
my_last_date <- max(actual_dates[!is.na(actual_mrtg)])
#function for prediction
get_expected_date <- function(timeline, current_df){
  asd <- lm(total ~ actual_dates, data=current_df)
  asd2 <- predict(asd,timeline)
  return(min(timeline[asd2<0]))
}
#layout defintion. need some work to get the image layouts

#options(width = 1500, height = 50)
ui <- fluidPage(
  
  titlePanel("Repayment Tracker")
  ,sidebarLayout(
    sidebarPanel(tableOutput('tbl')
    )
    ,mainPanel(plotOutput("coolplot", dblclick = "coolplot_dblclick",
                          brush = brushOpts(id = "coolplot_brush",resetOnNew = TRUE) 
                          )
    )             
  )
)

server <- function(input, output) {
  ranges <- reactiveValues(x = NULL, y = NULL)
  output$coolplot <- renderPlot(
    {
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
        scale_y_continuous(name="buckeroos",breaks = scales::pretty_breaks(n = 12)) +
        geom_hline(yintercept=0,color='red') +
        scale_color_brewer(palette='Dark2') + theme(legend.position = 'bottom', axis.text.x = element_text(angle=90,hjust=1)) + 
        coord_x_date(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      
    })
  
  observeEvent(input$coolplot_dblclick, {
    brush <- input$coolplot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  output$tbl <- renderTable({rbind.data.frame(c('data correct as of: ', as.character.Date(my_last_date) ) 
                                              ,c('cash variation: ', sprintf('%4.2f',current_df[current_df$actual_dates==my_last_date,3] - toto[toto$timeline==my_last_date,3] ) ) 
                                              ,c('offset variation: ', sprintf('%4.2f',current_df[current_df$actual_dates==my_last_date,4] - toto[toto$timeline==my_last_date,4] ) ) 
                                              ,c('outstanding variation: ', sprintf('%4.2f', - current_df[current_df$actual_dates==my_last_date,2] + toto[toto$timeline==my_last_date,2] ) ) 
                                              ,c('total variation: ', sprintf('%4.2f',   toto[toto$timeline==my_last_date,5] 
                                                                              -   ( current_df[current_df$actual_dates==my_last_date,2] 
                                                                                    - current_df[current_df$actual_dates==my_last_date,3] 
                                                                                    - current_df[current_df$actual_dates==my_last_date,4] )
                                              ) )
                                              ,c('original planned date of completion:' ,'2020-12-26')
                                              ,c('current planned date of completion: ', as.character.Date(get_expected_date(timeline, current_df)))
                                              
  )
  },colnames = F, striped = T, digits = 2, align = '?'
  )
  
  
} 
shinyApp(ui = ui, server = server)
