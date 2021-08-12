#Necessary Libraries
library(data.table)
library(stringr)
library(dygraphs)
library(plotly)
library(dplyr)
library(ggplot2)
library(lubridate)

#Setting Working Directory
setwd("/Users/tanyacukierman/Library/Mobile Documents/com~apple~CloudDocs/UChicago/Career/BroadPeak")

#This reads a csv from disk into a data.table
data2 <- fread('cpca-data_v5.csv')
cnvspsgr <- fread('CNVSPSGR Index.csv')

#Renaming cols and keeping only relevant months
monthlydata <- cnvspsgr[162:203]
setnames(monthlydata, c("Start Date", "1/1/00"), c("Date", "Val"))
monthlydata[, Compar := as.Date(Date, "%d/%m/%Y")]
monthlydata

monthlydata[, m := as.numeric(format(monthlydata$Compar, "%m"))]
monthlydata[, y := as.numeric(format(monthlydata$Compar, "%Y"))]

#Splitting Wholesale and Retail Data
wholesale <- data2[title %like% "批发"]
retail <- data2[title %like% "零售"] 

#-----------------------------------------------
#Working with retail data

#Extracting year 
extracty <- function(s) {tail(unlist(regmatches(s, regexec("^[0-9]*年$", s))), 1) }
extractx <- function(s) {tail(unlist(regmatches(s, regexec("^[0-9]*", s))), 1) }
ryear <- retail[grepl('年', period), extractx(extracty(as.character(period))), by=period]
retail[, ryear:=extractx(extracty(as.character(period))), by=period]

#tests
extractx(extracty("21年同比19年"))
extractx(extracty("20年"))
extractx(extracty("环比8月同期"))

#Extracting month
extractm <- function(s) {tail(unlist(regmatches(s, regexec("[0-9]+", s))), 1) }
rmonth <- retail[grepl('月', title), extractm(as.character(title)), by=title]
retail[, rmonth:=extractm(as.character(title)), by=title]

#tests
extractm("乘联会主要厂商19月周度零售数量和增速")
extractm("乘联会主要厂商3月周度零售数量和增速")

#Merging V1 - V7
retail <- merge(melt(retail[period == "period", !"period"], 
                     value.name = "date_range", measure.vars = paste0("V", seq(7))), 
                melt(retail[period != "period"], measure.vars = paste0("V", seq(7))), 
                by = c("title", "file", "date", "variable", "rmonth"))
head(retail)

#extract Start and end Date
retail[, c('start', 'end'):=tstrsplit(date_range, '-')]

retail[, start_date:=as.Date(paste0(strtoi(ryear.y) + 2000, '-', rmonth, '-', start))]
retail[, end_date:=as.Date(paste0(strtoi(ryear.y) + 2000, '-', rmonth, '-', end))]

#Plotting
plot(retail$start_date, retail$value,
     xlab = "Start Date",
     ylab = "Value for Retail")

plot(retail$end_date, retail$value,
     xlab = "End Date",
     ylab = "Value for Retail")

#high values around 150000 come from V5 (highlighted in green on numbers)

#-----------------------------------------------
#Working with wholesale data

#Extracting year 
ryear <- wholesale[grepl('年', period), extractx(extracty(as.character(period))), by=period]
wholesale[, ryear:=extractx(extracty(as.character(period))), by=period]

#Extracting month
rmonth <- wholesale[grepl('月', title), extractm(as.character(title)), by=title]
wholesale[, rmonth:=extractm(as.character(title)), by=title]

#Merging V1 - V7
wholesale <- merge(melt(wholesale[period == "period", !"period"], 
                        value.name = "date_range", measure.vars = paste0("V", seq(7))), 
                   melt(wholesale[period != "period"], measure.vars = paste0("V", seq(7))), 
                   by = c("title", "file", "date", "variable", "rmonth"))
head(wholesale)

#extract Start and end Date
wholesale[, c('start', 'end'):=tstrsplit(date_range, '-')]

wholesale[, start_date:=as.Date(paste0(strtoi(ryear.y) + 2000, '-', rmonth, '-', start))]
wholesale[, end_date:=as.Date(paste0(strtoi(ryear.y) + 2000, '-', rmonth, '-', end))]

#Plotting
plot(wholesale$start_date, wholesale$value,
     xlab = "Start Date",
     ylab = "Value for wholesale")

plot(wholesale$end_date, wholesale$value,
     xlab = "End Date",
     ylab = "Value for wholesale")

#outlier is line 107 on spreadsheet
wholesale[value == 624476]
#can and should I delete this?? 

#-----------------------------------------------

#Function to generate a graph of month and year
generategraph <- function(s, t) {ggplotly(ggplot(wholesale[rmonth == s & ryear.y == t] , 
                                                 aes(x = end_date, y = value)) + geom_point() 
                                          + facet_wrap(~rmonth))}
generategraph(3, 20)
generategraph(4, 19)

wholesale[, min(value), rmonth == 3]

unique(wholesale[rmonth == 3 & ryear.y == 20 & end == 14,!c("file", "date")])
unique(wholesale[rmonth == 3 & ryear.y == 20,!c("file", "date")])

#Adds the end of month to the data table
wholesale[, endofmonth := ceiling_date(wholesale$end_date, unit = "month") - 1]
wholesale

#Add an estimate for value for day
wholesale[, cumval := strtoi(value)*(strtoi(end) - strtoi(start) + 1L)]

#Generates a graph for date vs per day value
generategraph2 <- function(s, t) {ggplotly(ggplot(wholesale[rmonth == s & ryear.y == t] , 
                                                 aes(x = end_date, y = perday)) + geom_point() 
                                          + facet_wrap(~rmonth))}


#Tests
generategraph2(3, 20)
unique(wholesale[rmonth == 4 & ryear.y == 19,!c("file", "date")])
generategraph2(4, 19)
unique(wholesale[rmonth == 2 & ryear.y == 20,!c("file", "date")])
generategraph2(12, 20)
generategraph2(6, 20)
paste0("20", 19)
monthlydata
#[m == s & y == paste("20", t)]
test1 <- function(s, t) {ggplotly(ggplot() + geom_point(data = monthlydata[m == s & y == paste0("20", t)], aes(x = Date, y = Val), color = "blue"))}
                                      ##+ facet_wrap(~rmonth))}
test2 <- function(s, t) {ggplotly(ggplot() + geom_point(wholesale[rmonth == s & ryear.y == t] , 
                                                            mapping = aes(x = end_date, y = perday), colour = "red"))}
test1
par(new =TRUE)

test1(3, 19)
test2(3, 19)

doubgraph <- function(s, t) {ggplotly(ggplot() + 
                                        geom_point(wholesale[rmonth == s & ryear.y == t] , 
                                                   mapping = aes(x = end_date, y = perday), colour = "red")+ 
                                        geom_point(data = monthlydata[m == s & y == paste0("20", t)], 
                                                   aes(x = Date, y = Val), color = "blue")
                                      + facet_wrap(~rmonth))}
doubgraph(3, 19)
doubgraph(3, 20)

doubgraph <- function(s, t) {ggplotly(ggplot(wholesale[rmonth == s & ryear.y == t] , 
                                             mapping = aes(x = end_date, y = perday), color = "red") + geom_point()
                                      + geom_point(data = monthlydata[m == s & y == paste("20", t)], 
                                                   mapping = aes(x = Date, y = Val), color = "blue")
                                      + facet_wrap(~rmonth))}
doubgraph(3, 20)

#Each test works individually, but not together



#Trying to form cumulative avg
cumu <- unique(wholesale[rmonth == 2 & ryear.y == 20,!c("file", "date")])
cumu <- cumu[order(start)]
cumu[, aggr := cumsum(cumval), by = title]
cumu[, cumavg := strtoi(aggr) / strtoi(end)]
cumu

