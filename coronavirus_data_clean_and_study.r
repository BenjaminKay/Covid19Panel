library("tidyverse")
library("openxlsx")
library(zoo)
library(lubridate)
library(ggthemes) #tufte themes
library(directlabels) #label ends of line plots
library(httr) #github data
library(XML) #wikipedia data

#Convert factor to number
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
r <- GET(url)
pop <- readHTMLTable(
    doc=content(r, "text"), header=TRUE)

df_pop = pop[["NULL"]]

df_pop$Name = as.character(df_pop$"Country (or dependent territory)")

df_pop2 <- within(df_pop, FOO<-data.frame(do.call('rbind', strsplit(as.character(df_pop$"Country (or dependent territory)"), '[', fixed=TRUE))))

df_pop2$Country <- df_pop2$FOO$X1
df_pop2$CountryPop = as.numeric(gsub(",","",as.character(df_pop2$Population)))

df_pop3 <- df_pop2 %>% select(Country, CountryPop)


url2 <- "https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations"
r2 <- GET(url2)
codes <- readHTMLTable(
    doc=content(r2, "text"))

df_codes <-codes[["Codes and abbreviations for U.S. states, federal district, territories, and other regions
"]]
df_codes$StateLong = as.character(df_codes$V1)
df_codes$State2 = as.character(df_codes$V6)

df_codes2 <- df_codes[13:63,] %>% select(StateLong, State2)

setwd("D:/Users/yakne_000/Documents/R")
text_confirmed = GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
text_deaths    = GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
text_recovered = GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
    
df_confirmed <- content(text_confirmed, type = "text/csv", encoding="UTF-8")
df_deaths    <- content(text_deaths, type = "text/csv", encoding="UTF-8")
df_recovered <- content(text_recovered, type = "text/csv", encoding="UTF-8")


df_confirmed2   <- df_confirmed %>% gather(Date, ConfirmedCases, -`Province/State`, -`Country/Region`,   -Lat,   -Long)
df_deaths2      <- df_deaths %>% gather(Date, DealthCases, -`Province/State`, -`Country/Region`,   -Lat,   -Long)
df_recovered2   <- df_recovered %>% gather(Date, RecoveredCases, -`Province/State`, -`Country/Region`,   -Lat,   -Long)


## Create Workbook object and add worksheets
wb <- createWorkbook()

## Add worksheets
addWorksheet(wb, "confirmed")
addWorksheet(wb, "deaths")
addWorksheet(wb, "recovered")



writeData(wb, "confirmed", df_confirmed2, rowNames = TRUE)
writeData(wb, "deaths", df_confirmed2, rowNames = TRUE)
writeData(wb, "recovered", df_confirmed2, rowNames = TRUE)
saveWorkbook(wb, paste("JHU_Covi19_Data_As_Of_",today(),  ".xlsx", sep=""), overwrite = TRUE)



df_confirmed2$Date2 = mdy(df_confirmed2$Date)
df_confirmed2$Country = df_confirmed2$`Country/Region`

# df_confirmed3 <- left_join(df_confirmed2, df_confirmed2 %>% group_by(Country) %>% summarise(MaxConfirmedCases = max(ConfirmedCases)), by=c("Country"))

MinCases = 500
df_confirmed2_grpDC <- df_confirmed2 %>% group_by(Date2, Country) %>% summarise(TotalConfirmedCases = sum(ConfirmedCases))
df_confirmed3 <- left_join(df_confirmed2, df_confirmed2_grpDC %>% group_by(Country) %>% summarise(MaxConfirmedCases = max(TotalConfirmedCases)), by=c("Country"))
df_confirmed3$Locale = df_confirmed3$`Province/State`

df_confirmed4 <- within(df_confirmed3, FOO<-data.frame(do.call('rbind', strsplit(as.character(df_confirmed3$`Province/State`), ',', fixed=TRUE))))


df_confirmed4$State <- as.character(df_confirmed4$FOO$X2)


df_confirmed3_grpDC <- df_confirmed3 %>%  group_by(Date2, Country) %>% summarise(TotalConfirmedCases = sum(ConfirmedCases), MaxConfirmedCases2=median(MaxConfirmedCases))

# %>% filter(MaxConfirmedCases>=MinCases)

df_confirmed3_grpDC2 = df_confirmed3_grpDC %>% filter(MaxConfirmedCases2>=MinCases, TotalConfirmedCases>=5) 


plt1 <- df_confirmed3_grpDC2 %>% ggplot(aes(x=Date2, y=TotalConfirmedCases, group=Country, color = Country)) +
    scale_y_continuous(trans='log10') + 
    geom_line()+
    geom_point()+
    scale_colour_discrete(guide = 'none') +
    scale_x_discrete(expand=c(0, 4)) +
    geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
    geom_dl(aes(label = Country), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8))     
plt1


df_confirmedUS1 <- df_confirmed4 %>% filter(`Country/Region`=="US")



df_confirmedUS1$State2 <- sub("^\\s+|\\s+$","",df_confirmedUS1$State)
df_confirmedUS1$State2 <- sub("D.C.","DC",df_confirmedUS1$State2)
df_confirmedUS1$State3 <- df_confirmedUS1$State2

df_confirmedUS2 <- left_join(df_confirmedUS1, df_codes2, by="State2")
df_confirmedUS2$StateClean <- ifelse(nchar(df_confirmedUS2$State3)==2,df_confirmedUS2$StateLong,df_confirmedUS2$State3)


df_confirmed4_grpDC <-  df_confirmedUS2 %>%  group_by(Date2, StateClean) %>% summarise(TotalConfirmedCases = sum(ConfirmedCases), MaxConfirmedCases2=median(MaxConfirmedCases))  %>%  
    filter(MaxConfirmedCases2 > 0, TotalConfirmedCases>0)

df_confirmed4_grpDC$TotalConfirmedCases2 = df_confirmed4_grpDC$TotalConfirmedCases+1

#dev.off()
setSessionTimeLimit()
setTimeLimit()
plt2 <- df_confirmed4_grpDC %>% ggplot(aes(x=Date2, y=TotalConfirmedCases2, group=StateClean, color = StateClean)) +
    scale_y_continuous(trans='log10', limits=c(1, 100000)) + 
    geom_line()+
    geom_point()+
    scale_colour_discrete(guide = 'none') +
    scale_x_date(expand=c(0, 4)) +
    geom_dl(aes(label = StateClean), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
    labs(x = "Date", y = "Confirmed Cases") 
    #geom_dl(aes(label = StateClean), method = list(dl.trans(y = dl.jitter(y) - 1), dl.trans(x = x - 0.2), "first.points", cex = 0.8))
    #geom_dl(aes(label = StateClean), method = list(dl.trans(y =  y -1 ), dl.trans(x = x-0.2), "first.points", cex = 0.8)) 
plt2
