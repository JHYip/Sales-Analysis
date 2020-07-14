library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(shiny)
library(DT)
library(plotly)
library(RColorBrewer)
#library(tmap)

Data <- read_excel("Intern Testing_data.xlsx", sheet = "POS DATA")

any(is.na(Data$date))
any(is.na(Data$location))
any(is.na(Data$description))
which(is.na(Data$description))
any(is.na(Data$main_group))
any(is.na(Data$style))
any(is.na(Data$sub_group))
any(is.na(Data$plu_code))
any(is.na(Data$qty))
any(is.na(Data$rrp))
any(is.na(Data$total_amount))
any(is.na(Data$unit_price))
any(is.na(Data$netam))
any(is.na(Data$margin))
any(is.na(Data$cost))
any(is.na(Data$total_cost))
any(is.na(Data$profit))


Data$date <- as.character(Data$date)
Data$date <- ymd(Data$date)
#Data$date <- format(Data$date, "%d-%m-%Y")


Jan <- filter(Data, date >= "2018-01-01" & date <= "2018-01-31")
Feb <- filter(Data, date >= "2018-02-01" & date <= "2018-02-28")
March <- filter(Data, date >= "2018-03-01" & date <= "2018-03-31")
Apr <- filter(Data, date >= "2018-04-01" & date <= "2018-04-30")
May <- filter(Data, date >= "2018-05-01" & date <= "2018-05-31")
Jun <- filter(Data, date >= "2018-06-01" & date <= "2018-06-30")
Jul <- filter(Data, date >= "2018-07-01" & date <= "2018-07-31")
Aug <- filter(Data, date >= "2018-08-01" & date <= "2018-08-31")
Sept <- filter(Data, date >= "2018-09-01" & date <= "2018-09-30")
Oct <- filter(Data, date >= "2018-10-01" & date <= "2018-10-31")
Nov <- filter(Data, date >= "2018-11-01" & date <= "2018-11-30")
Dec <- filter(Data, date >= "2018-12-01" & date <= "2018-12-31")

Jan_Profit <- sum(Jan$profit)
Feb_Profit <- sum(Feb$profit)
March_Profit <- sum(March$profit)
Apr_Profit <- sum(Apr$profit)
May_Profit <- sum(May$profit)
Jun_Profit <- sum(Jun$profit)
Jul_Profit <- sum(Jul$profit)
Aug_Profit <- sum(Aug$profit)
Sept_Profit <- sum(Sept$profit)
Oct_Profit <- sum(Oct$profit)
Nov_Profit <- sum(Nov$profit)
Dec_Profit <- sum(Dec$profit)

#Total Profit in Piechart
Profit2018 <- c(Jan_Profit, Feb_Profit, March_Profit, Apr_Profit, May_Profit, Jun_Profit, Jul_Profit, Aug_Profit, Sept_Profit, Oct_Profit, Nov_Profit, Dec_Profit)
Month <- c("Jan", "Feb", "March", "Apr", "May", "Jun" , "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")
pct <- round(Profit2018/sum(Profit2018)*100)
Month <- paste(Month, pct)
Month <- paste(Month, "%", sep = "")
pie(Profit2018, labels = Month, main = "Total Profit over the year", border = "white", col = brewer.pal(12,"Set3"))

TPtable <- matrix(Profit2018, ncol = 2)
Monthlabel <- c("January", "Febuary", "March", "April", "May", "Jun", "July", "August", "September", "October", "November", "December")
TPtable <- cbind(Monthlabel, Profit2018)
TPtable <- data.frame(TPtable)
colnames(TPtable) <- c("Month", "Total Profit (RM)")
TPtable

#Quarterly 
QP1 <- sum(c(Jan_Profit, Feb_Profit, March_Profit)) 
QP2 <- sum(c(Apr_Profit, May_Profit, Jun_Profit))
QP3 <- sum(c(Jul_Profit, Aug_Profit, Sept_Profit))
QP4 <- sum(c(Oct_Profit, Nov_Profit, Dec_Profit))

QPTotal <- matrix(c(QP1, QP2, QP3, QP4))
Quarterlabel = c("Quarter1", "Quarter2", "Quarter3", "Quarter4")
QPTable <- cbind(Quarterlabel, QPTotal)
QPTable <- data.frame(QPTable)
colnames(QPTable) = c("Quarter", "Total Profit")
QPTable

#Quarterly Piechart
QProfit <- c(QP1, QP2, QP3, QP4)
Group <- c("Q1", "Q2", "Q3", "Q4")
Qpct <- round(QProfit/ sum(QProfit)*100)
Group <- paste(Group, Qpct)
Group <- paste(Group, "%", sep = "")
pie(QProfit, labels = Group, main = "Total Profit by quarter")

#Total Profit by state
Data %>% count(location)
SProfit <- group_by(Data, location)
SProfit <- summarise(SProfit, profit = sum(profit))
colnames(SProfit) <- c("Location", "Total Profit (RM)")
SProfit$Location[SProfit$Location == "JB"] <- "Johor Bharu"
SProfit$Location[SProfit$Location == "KELANTAN"] <- "Kelantan"
SProfit$Location[SProfit$Location == "KL"] <- "Kuala Lumpur"
SProfit$Location[SProfit$Location == "PENANG"] <- "Penang"
SProfit

#Quarterly Profit by state
#Penang
PenangQ1 <- Data %>% filter(date >= "2018-01-01" & date < "2018-04-01") %>% filter(location == "PENANG") %>%
  summarise(profit = sum(profit))
PenangQ2 <- Data %>% filter(date >= "2018-04-01" & date < "2018-07-01") %>% filter(location == "PENANG") %>%
  summarise(profit = sum(profit))
PenangQ3 <- Data %>% filter(date >= "2018-07-01" & date < "2018-10-01") %>% filter(location == "PENANG") %>%
  summarise(profit = sum(profit))
PenangQ4 <- Data %>% filter(date >= "2018-10-01" & date < "2019-01-01") %>% filter(location == "PENANG") %>%
  summarise(profit = sum(profit))
#Johor Bharu
JBQ1 <- Data %>% filter(date >= "2018-01-01" & date < "2018-04-01") %>% filter(location == "JB") %>%
  summarise(profit = sum(profit))
JBQ2 <- Data %>% filter(date >= "2018-04-01" & date < "2018-07-01") %>% filter(location == "JB") %>%
  summarise(profit = sum(profit))
JBQ3 <- Data %>% filter(date >= "2018-07-01" & date < "2018-10-01") %>% filter(location == "JB") %>%
  summarise(profit = sum(profit))
JBQ4 <- Data %>% filter(date >= "2018-10-01" & date < "2019-01-01") %>% filter(location == "JB") %>%
  summarise(profit = sum(profit))
#Kuala Lumpur
KLQ1 <- Data %>% filter(date >= "2018-01-01" & date < "2018-04-01") %>% filter(location == "KL") %>%
  summarise(profit = sum(profit))
KLQ2 <- Data %>% filter(date >= "2018-04-01" & date < "2018-07-01") %>% filter(location == "KL") %>%
  summarise(profit = sum(profit))
KLQ3 <- Data %>% filter(date >= "2018-07-01" & date < "2018-10-01") %>% filter(location == "KL") %>%
  summarise(profit = sum(profit))
KLQ4 <- Data %>% filter(date >= "2018-10-01" & date < "2019-01-01") %>% filter(location == "KL") %>%
  summarise(profit = sum(profit))
#Kelantan
KQ1 <- Data %>% filter(date >= "2018-01-01" & date < "2018-04-01") %>% filter(location == "KELANTAN") %>%
  summarise(profit = sum(profit))
KQ2 <- Data %>% filter(date >= "2018-04-01" & date < "2018-07-01") %>% filter(location == "KELANTAN") %>%
  summarise(profit = sum(profit))
KQ3 <- Data %>% filter(date >= "2018-07-01" & date < "2018-10-01") %>% filter(location == "KELANTAN") %>%
  summarise(profit = sum(profit))
KQ4 <- Data %>% filter(date >= "2018-10-01" & date < "2019-01-01") %>% filter(location == "KELANTAN") %>%
  summarise(profit = sum(profit))

PenangQT <- rbind(PenangQ1, PenangQ2, PenangQ3, PenangQ4)
PenangQT <- mutate(PenangQT, Quarter = c("Jan - March", "Apr - Jun", "Jul - Sept", "Oct - Dec"))
PenangQT <- PenangQT[,c(2,1)]
PenangQT$Quarter <- factor(PenangQT$Quarter, levels = c("Jan - March", "Apr - Jun", "Jul - Sept", "Oct - Dec"), ordered = TRUE)
ggplot(PenangQT, aes(x = Quarter, y = profit)) + geom_col(fill = brewer.pal(4, "Set2"))


JBQT <- rbind(JBQ1, JBQ2, JBQ3, JBQ4)
JBQT <- mutate(JBQT, Quarter = c("Jan - March", "Apr - Jun", "Jul - Sept", "Oct - Dec"))
JBQT <- JBQT[,c(2,1)]
JBQT$Quarter <- factor(JBQT$Quarter, levels = c("Jan - March", "Apr - Jun", "Jul - Sept", "Oct - Dec"), ordered = TRUE)
ggplot(JBQT, aes(x = Quarter, y = profit)) + geom_col(fill = brewer.pal(4, "Set2"))


KLQT <- rbind(KLQ1, KLQ2, KLQ3, KLQ4)
KLQT <- mutate(KLQT, Quarter = c("Jan - March", "Apr - Jun", "Jul - Sept", "Oct - Dec"))
KLQT <- KLQT[,c(2,1)]
KLQT$Quarter <- factor(KLQT$Quarter, levels = c("Jan - March", "Apr - Jun", "Jul - Sept", "Oct - Dec"), ordered = TRUE)
ggplot(KLQT, aes(x = Quarter, y = profit)) + geom_col(fill = brewer.pal(4, "Set2"))

KQT <- rbind(KQ1, KQ2, KQ3, KQ4)
KQT <- mutate(KQT, Quarter = c("Jan - March", "Apr - Jun", "Jul - Sept", "Oct - Dec"))
KQT <- PenangQT[,c(2,1)]
KQT$Quarter <- factor(KQT$Quarter, levels = c("Jan - March", "Apr - Jun", "Jul - Sept", "Oct - Dec"), ordered = TRUE)
ggplot(KQT, aes(x = Quarter, y = profit)) + geom_col(fill = brewer.pal(4, "Set2"))


TotalQty <- sum(Data$qty)
StyleQty <- Data %>% group_by(style) %>% summarise(qty = sum(qty), profit = sum(profit))
StyleQty <- Data %>% group_by(style, location) %>% summarise(qty = sum(qty), profit = sum(profit))
ItemGroup <- Data %>% group_by(main_group, style, location) %>% summarise(qty = sum(qty))
ItemGroup <- ItemGroup[order(ItemGroup$qty),]
WorstSales <- ItemGroup[c(1:5),]
ItemGroup <- ItemGroup[order(-ItemGroup$qty),]
BestSales <- ItemGroup[c(1:5),]

ggplot(data = StyleQty, aes(x = style, y = profit, fill = location)) + geom_bar(stat = "identity", position = position_dodge())
ggplot(data = StyleQty, aes(x = style, y = qty, fill = location)) + geom_bar(stat = "identity", position = position_dodge())

#Shiny app
ui <- fluidPage(
  titlePanel("Sales analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Quarter", "Quarterly", choices = c("First Quarter", "Second Quarter", "Third Quarter", "Fourth Quarter") )
    ),
  mainPanel(
    p("This will display monthly profit"),
    tabsetPanel(
      tabPanel("Montly Profit",
      p("The table below shows the monthly profit of the company"),
      dataTableOutput("MonthlyProfitTable"), br(), br(),
      p("The pie chart below shows the monthly profit by percentage, the highest sales is Jun and December, the lowest profit month is January, April, May and October"),
      plotOutput("MonthlyProfitPie")),
      tabPanel("Quarterly Profit",
      p("The table below shows the quarterly profit of the company"),
      dataTableOutput("QuarterlyProfitTable"), br(), br(),
      p("The pie chart below shows the quarterly profit by percentage"),
      plotOutput("QuarterlyProfitPie")),
      tabPanel("Location",
      p("The table below shows the Total profit by each branch"),
      dataTableOutput("LocationTotalProfit"),
      plotOutput("JBQuarterlyProfit"), 
      plotOutput("KelantanQuarterlyProfit"), 
      plotOutput("KLQuarterlyProfit"), 
      plotOutput("PenangQuarterlyProfit")),
      tabPanel("Product",
      p("The results below is the total quantity of product been sold in year 2018"),
      verbatimTextOutput("TotalProduct"),
      p("The graph below shows the number of product been sold by style"),
      plotOutput("ProductSales"),
      plotOutput("ProductProfit"),
      p("The table below shows the top 5 best sales based on product style"),
      dataTableOutput("Top5Best"),
      p("The table below shows the top 5 worse sales based on product style"),
      dataTableOutput("Top5Worst"))
    )
  )
)
)

server <- function(input, output, session){
  output$MonthlyProfitTable <- renderDataTable({
    TPtable
  })
  output$MonthlyProfitPie <- renderPlot({
    pie(Profit2018, labels = Month, main = "Total Profit over the year", border = "white", col = brewer.pal(12,"Set3"))
  })
  output$QuarterlyProfitPie <- renderPlot({
    pie(QProfit, labels = Group, main = "Total Profit by quarter", border = "white", col = brewer.pal(4, "Set3"))
  })
  output$QuarterlyProfitTable <- renderDataTable({
    QPTable
  })
  output$LocationTotalProfit <- renderDataTable({
    SProfit
  })
  output$PenangQuarterlyProfit <- renderPlot({
    ggplot(PenangQT, aes(x = Quarter, y = profit)) + geom_col(fill = brewer.pal(4, "Set2"))
  })
  output$JBQuarterlyProfit <- renderPlot({
    ggplot(JBQT, aes(x = Quarter, y = profit)) + geom_col(fill = brewer.pal(4, "Set2"))
  })
  output$KelantanQuarterlyProfit <- renderPlot({
    ggplot(KQT, aes(x = Quarter, y = profit)) + geom_col(fill = brewer.pal(4, "Set2"))
  })
  output$KLQuarterlyProfit <- renderPlot({
    ggplot(KQT, aes(x = Quarter, y = profit)) + geom_col(fill = brewer.pal(4, "Set2"))
  })
  output$ProductSales <- renderPlot({
    ggplot(data = StyleQty, aes(x = style, y = qty, fill = location)) + 
      geom_bar(stat = "identity", position = position_dodge()) + 
      labs(fill = "Location") +
      scale_fill_discrete(name = "Location", label = c("Johor Bharu", "Kelantan", "Kuala Lumpur", "Penang"))
  })
  output$ProductProfit <- renderPlot({
    ggplot(data = StyleQty, aes(x = style, y = profit, fill = location)) + 
      geom_bar(stat = "identity", position = position_dodge()) + 
      labs(fill = "Location") +
      scale_fill_discrete(name = "Location", label = c("Johor Bharu", "Kelantan", "Kuala Lumpur", "Penang"))
  })
  output$TotalProduct <- renderPrint({
    TotalQty
  })
  output$Top5Best <- renderDataTable({
    BestSales
  })
  output$Top5Worst <- renderDataTable({
    WorstSales
  })
}

shinyApp(ui = ui, server = server)
