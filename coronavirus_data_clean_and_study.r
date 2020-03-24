library("tidyverse")
library("openxlsx")
#str_replace_all
#https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
# install_github("statnet/statnet.common")
# install.packages("statnet.common")
# library(devtools)
# install_github("GeoBosh/Rdpack")
library(zoo) #rollapply
library(lubridate)
library(ggthemes) #tufte themes
library(directlabels) #label ends of line plots
# Single day snapshot
#  install.packages("coronavirus")
# library(coronavirus)

#library("remotes")
#remotes::install_github("GuangchuangYu/nCov2019")
#library(source("example.R"))
#x <- get_nCov2019(lang='en')
#df_cases <- x$global

# https://towardsdatascience.com/an-r-package-to-explore-the-novel-coronavirus-590055738ad6
library(httr) #github data
library(XML) #wikipedia data

#Convert factor to number
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

MinCases = 500
MinDeaths = 20
# G7 Plus south korea
# France, West Germany, Italy, Japan, the United Kingdom, and the United States
SelectedCountryList = c("United States", "US", "Germany", "France", "Canada","Italy", "United Kingdom", "Japan", "Korea, South")
url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
url2 <- "https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations"
r <- GET(url)
pop <- readHTMLTable(
    doc=content(r, "text"), header=TRUE)

df_pop = pop[["NULL"]]
df_pop$Name = as.character(df_pop$"Country (or dependent territory)")

df_pop2 <- within(df_pop, FOO<-data.frame(do.call('rbind', strsplit(as.character(df_pop$"Country (or dependent territory)"), '[', fixed=TRUE))))
df_pop2$Country <- df_pop2$FOO$X1
df_pop2$CountryPop = as.numeric(gsub(",","",as.character(df_pop2$Population)))

df_pop3 <- df_pop2 %>% select(Country, CountryPop)



r2 <- GET(url2)
codes <- readHTMLTable(
    doc=content(r2, "text"))

df_codes <-codes[["Codes and abbreviations for U.S. states, federal district, territories, and other regions
"]]
df_codes$StateLong = as.character(df_codes$V1)
df_codes$State2 = as.character(df_codes$V6)

df_codes2 <- df_codes[13:63,] %>% select(StateLong, State2)


text_confirmed = GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
text_deaths    = GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
text_recovered = GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

    
df_confirmed <- content(text_confirmed, type = "text/csv", encoding="UTF-8")
df_deaths    <- content(text_deaths, type = "text/csv", encoding="UTF-8")
df_recovered <- content(text_recovered, type = "text/csv", encoding="UTF-8")


df_confirmed2   <- df_confirmed %>% gather(Date, ConfirmedCases, -`Province/State`, -`Country/Region`,   -Lat,   -Long)
df_deaths2      <- df_deaths %>% gather(Date, DeathCases, -`Province/State`, -`Country/Region`,   -Lat,   -Long)
df_recovered2   <- df_recovered %>% gather(Date, RecoveredCases, -`Province/State`, -`Country/Region`,   -Lat,   -Long)

## Create Workbook object and add worksheets
wb <- createWorkbook()

## Add worksheets
addWorksheet(wb, "confirmed")
addWorksheet(wb, "deaths")
addWorksheet(wb, "recovered")



writeData(wb, "confirmed", df_confirmed2, rowNames = TRUE)
writeData(wb, "deaths", df_deaths2, rowNames = TRUE)
writeData(wb, "recovered", df_recovered2, rowNames = TRUE)
saveWorkbook(wb, paste("JHU_Covid19_Data_As_Of_",today(),  ".xlsx", sep=""), overwrite = TRUE)



df_confirmed2$Date2 = mdy(df_confirmed2$Date)
df_confirmed2$Country = df_confirmed2$`Country/Region`
df_confirmed2$ConfirmedCases[is.na(df_confirmed2$ConfirmedCases)] = 0


# df_confirmed3 <- left_join(df_confirmed2, df_confirmed2 %>% group_by(Country) %>% summarise(MaxConfirmedCases = max(ConfirmedCases)), by=c("Country"))


df_confirmed2_grpDC <- df_confirmed2 %>% group_by(Date2, Country) %>% summarise(TotalConfirmedCases = sum(ConfirmedCases, rm.na=TRUE))
df_confirmed3 <- left_join(df_confirmed2, df_confirmed2_grpDC %>% group_by(Country) %>% summarise(MaxConfirmedCases = max(TotalConfirmedCases, rm.na=TRUE)), by=c("Country"))
df_confirmed3$Locale = df_confirmed3$`Province/State`

df_confirmed4 <- within(df_confirmed3, FOO<-data.frame(do.call('rbind', strsplit(as.character(df_confirmed3$`Province/State`), ',', fixed=TRUE))))
df_confirmed4$State <- as.character(df_confirmed4$FOO$X2)


df_confirmed3_grpDC <- df_confirmed3 %>%  group_by(Date2, Country) %>% summarise(TotalConfirmedCases = sum(ConfirmedCases, rm.na=TRUE), MaxConfirmedCases2=max(MaxConfirmedCases, rm.na=TRUE))
df_confirmed3_grpDCC <- df_confirmed3_grpDC %>%  filter(TotalConfirmedCases >=100) %>% group_by(Country) %>% summarise(MinDate=min(Date2), MaxDate=max(Date2), MinCases=min(TotalConfirmedCases, rm.na=TRUE))

df_confirmed3_grpDC = left_join(df_confirmed3_grpDC, df_confirmed3_grpDCC, by="Country")


df_confirmed3_grpDC2 = df_confirmed3_grpDC %>% filter(MaxConfirmedCases2>=MinCases, TotalConfirmedCases>=5) 
df_confirmed3_grpDC2$GrowthDays = df_confirmed3_grpDC2$MaxDate - df_confirmed3_grpDC2$MinDate
df_confirmed3_grpDC2$LogCaseGrowth = log(df_confirmed3_grpDC2$MaxConfirmedCases2) - log(df_confirmed3_grpDC2$MinCases)

df_confirmed3_grpDC2$GrowthRate = df_confirmed3_grpDC2$LogCaseGrowth  / as.numeric(df_confirmed3_grpDC2$GrowthDays)
df_confirmed3_grpDC2$GrowthRatePct = df_confirmed3_grpDC2$GrowthRate * 100

df_confirmed3_grpDC2$AverageGrowthRate = (log(df_confirmed3_grpDC2$TotalConfirmedCases) - log(df_confirmed3_grpDC2$MinCases)) / (as.numeric(df_confirmed3_grpDC2$Date2 - df_confirmed3_grpDC2$MinDate )) 
df_confirmed3_grpDC2$AverageGrowthRatePct = df_confirmed3_grpDC2$AverageGrowthRate * 100


View(df_confirmed3_grpDC2 %>% filter(Date2 ==mdy("3/21/2020"))) 


df_compare_growth_rates = df_confirmed3_grpDC2 %>% group_by(Country) %>% summarise(EstGrowthRatePct = min(GrowthRatePct))


plt1 <- df_confirmed3_grpDC2 %>% ggplot(aes(x=Date2, y=TotalConfirmedCases, group=Country, color = Country)) +
    scale_y_continuous(trans='log10', limits=c(1, 100000)) +
    geom_line()+
    geom_point()+
    scale_colour_discrete(guide = 'none') +
    scale_x_date(expand=c(0, 4)) +
    geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
    labs(x = "Date", y = "Confirmed Cases") 
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


TestState = "New York"
df_confirmedCA = df_confirmedUS2 %>% filter(StateClean==TestState)
df_confirmedCA$HasLocale = df_confirmedCA$Locale!=TestState
df_confirmedCA_grpDL <- df_confirmedCA %>% group_by(Date2, HasLocale) %>% summarise(TotalConfirmedCases = sum(ConfirmedCases))
plt3 <- df_confirmedCA_grpDL %>% ggplot(aes(x=Date2, y=TotalConfirmedCases, group=HasLocale, color = HasLocale)) +
    scale_y_continuous(trans='log10', limits=c(1, 100000)) + 
    geom_line()+
    geom_point()+
    scale_colour_discrete(guide = 'none') +
    scale_x_date(expand=c(0, 4)) +
    geom_dl(aes(label = HasLocale), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
    labs(x = "Date", y = "Confirmed Cases") 
plt3


plt4 <- df_confirmed3_grpDC2 %>% filter(Date2>=mdy("3/1/2020"), TotalConfirmedCases > MinCases, Country %in% SelectedCountryList) %>% ggplot(aes(x=Date2, y=AverageGrowthRatePct, group=Country, color = Country)) +
    geom_line()+
    geom_point()+
    scale_colour_discrete(guide = 'none') +
    scale_x_date(expand=c(0, 4)) +
    geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
    labs(x = "Date", y = "Daily Growth Rate (%) Since 500 Cases") 
plt4


plt7 <- df_confirmed3_grpDC2 %>% filter(Date2>=mdy("3/1/2020"), TotalConfirmedCases > MinCases, Country %in% SelectedCountryList) %>% ggplot(aes(x=Date2, y=TotalConfirmedCases, group=Country, color = Country)) +
    geom_line()+
    geom_point()+
    scale_y_continuous(trans='log10', limits=c(1, 100000)) +     
    scale_colour_discrete(guide = 'none') +
    scale_x_date(expand=c(0, 4)) +
    geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
    labs(x = "Date", y = "Confirmed Cases") 
plt7


# df_deaths2

df_deaths2$Date2 = mdy(df_deaths2$Date)
df_deaths2$Country = df_deaths2$`Country/Region`
df_deaths2$DeathCases[is.na(df_deaths2$DeathCases)] = 0


df_deaths2_grpDC <- df_deaths2 %>% group_by(Date2, Country) %>% summarise(TotalDeathCases = sum(DeathCases))
df_deaths3 <- left_join(df_deaths2, df_deaths2_grpDC %>% group_by(Country) %>% summarise(MaxDeathCases = max(TotalDeathCases)), by=c("Country"))
df_deaths3$Locale = df_deaths3$`Province/State`

df_deaths4 <- within(df_deaths3, FOO<-data.frame(do.call('rbind', strsplit(as.character(df_deaths3$`Province/State`), ',', fixed=TRUE))))
df_deaths4$State <- as.character(df_deaths4$FOO$X2)


df_deaths4_grpDC <- df_deaths4 %>%  group_by(Date2, Country) %>% summarise(TotalDeathCases = sum(DeathCases), MaxDeathCases2=median(MaxDeathCases))
df_deaths4_grpDCC <- df_deaths4_grpDC %>%  filter(TotalDeathCases >=MinDeaths) %>% group_by(Country) %>% summarise(MinDate=min(Date2), MaxDate=max(Date2), MinCases=min(TotalDeathCases))

df_deaths4_grpDC = left_join(df_deaths4_grpDC, df_deaths4_grpDCC, by="Country")

df_deaths4_grpDC2 = df_deaths4_grpDC %>% filter(MaxDeathCases2>=MinDeaths, TotalDeathCases>=5) 
df_deaths4_grpDC2$GrowthDays = df_deaths4_grpDC2$MaxDate - df_deaths4_grpDC2$MinDate
df_deaths4_grpDC2$LogCaseGrowth = log(df_deaths4_grpDC2$MaxDeathCases2) - log(df_deaths4_grpDC2$MinCases)

df_deaths4_grpDC2$GrowthRate = df_deaths4_grpDC2$LogCaseGrowth  / as.numeric(df_deaths4_grpDC2$GrowthDays)
df_deaths4_grpDC2$GrowthRatePct = df_deaths4_grpDC2$GrowthRate * 100

df_deaths4_grpDC2$AverageGrowthRate = (log(df_deaths4_grpDC2$TotalDeathCases) - log(df_deaths4_grpDC2$MinCases)) / (as.numeric(df_deaths4_grpDC2$Date2 - df_deaths4_grpDC2$MinDate )) 
df_deaths4_grpDC2$AverageGrowthRatePct = df_deaths4_grpDC2$AverageGrowthRate * 100

# Calculate lagged values
df_deaths4_grpDC2 <- 
    df_deaths4_grpDC2 %>%
    group_by(Country) %>%
    mutate(lag.TotalDeathCases = dplyr::lag(TotalDeathCases, n = 1, default = NA, order_by=Date2))
df_deaths4_grpDC2 <- 
    df_deaths4_grpDC2 %>%
    group_by(Country) %>%
    mutate(lag.Date2 = dplyr::lag(Date2, n = 1, default = NA, order_by=Date2))

   
df_deaths4_grpDC2$Date2Diff = df_deaths4_grpDC2$Date2 - df_deaths4_grpDC2$lag.Date2
df_deaths4_grpDC2$LogTotalDeathCases = log(df_deaths4_grpDC2$TotalDeathCases)
df_deaths4_grpDC2$lag.LogTotalDeathCases = log(df_deaths4_grpDC2$lag.TotalDeathCases)

df_deaths4_grpDC2$DailyGrowthRate = (log(df_deaths4_grpDC2$LogTotalDeathCases) - log(df_deaths4_grpDC2$lag.LogTotalDeathCases)) / (as.numeric(df_deaths4_grpDC2$Date2Diff)) 
df_deaths4_grpDC2$DailyGrowthRatePct = df_deaths4_grpDC2$DailyGrowthRate * 100

df_deaths4_grpDC2 <- 
    df_deaths4_grpDC2 %>%
    group_by(Country) %>% arrange(Date2) %>% 
    # mutate(MADailyGrowthRatePct = rollapply(DailyGrowthRatePct, 7, mean, align="right", fill=NA)) 
    mutate(MADailyGrowthRatePct = rollapply(DailyGrowthRatePct, 7, mean, align="right", partial = TRUE, na.rm = TRUE)) 



plt4 <- df_deaths4_grpDC2 %>% filter(Date2>=mdy("3/1/2020"), TotalDeathCases > MinDeaths, Country %in% SelectedCountryList) %>% ggplot(aes(x=Date2, y=AverageGrowthRatePct, group=Country, color = Country)) +
    geom_line()+
    geom_point()+
    scale_colour_discrete(guide = 'none') +
    scale_x_date(expand=c(0, 4)) +
    geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
    labs(x = "Date", y = "Daily Growth Rate (%) Since 20 Deaths") 
plt4

plt5 <- df_deaths4_grpDC2 %>% filter(Date2>=mdy("3/1/2020"), Country %in% SelectedCountryList) %>% ggplot(aes(x=Date2, y=DailyGrowthRatePct, group=Country, color = Country)) +
    geom_line()+
    geom_point()+
    scale_colour_discrete(guide = 'none') +
    scale_x_date(expand=c(0, 4)) +
    geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
    labs(x = "Date", y = "Day over Day Growth Rate (%)") 
plt5

plt6 <- df_deaths4_grpDC2 %>% filter(Date2>=mdy("3/1/2020"), Country %in% SelectedCountryList) %>% ggplot(aes(x=Date2, y=MADailyGrowthRatePct, group=Country, color = Country)) +
    geom_line()+
    geom_point()+
    scale_colour_discrete(guide = 'none') +
    scale_x_date(expand=c(0, 4)) +
    geom_dl(aes(label = Country), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
    labs(x = "Date", y = "MA(7) Day over Day Growth Rate in Deaths (%)") 
plt6


# Alternative county level data

# USA Facts: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
text_confirmed_county <- GET("https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv")
text_death_county <- GET("https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv")
df_confirmed_county <- content(text_confirmed_county, type = "text/csv", encoding="UTF-8")
df_deaths_county    <- content(text_death_county, type = "text/csv", encoding="UTF-8")
df_confirmed_county2   <- df_confirmed_county %>% gather(Date, ConfirmedCases, -countyFIPS, -`County Name`,   -State,   -stateFIPS)
df_deaths_county2      <- df_deaths_county %>% gather(Date, DeathCases, -countyFIPS, -`County Name`,   -State,   -stateFIPS)


wb2 <- createWorkbook()

## Add worksheets
addWorksheet(wb2, "confirmed")
addWorksheet(wb2, "deaths")


writeData(wb2, "confirmed", df_confirmed_county2, rowNames = TRUE)
writeData(wb2, "deaths", df_deaths_county2, rowNames = TRUE)
saveWorkbook(wb2, paste("USA_County_Covid19_Data_As_Of_",today(),  ".xlsx", sep=""), overwrite = TRUE)

