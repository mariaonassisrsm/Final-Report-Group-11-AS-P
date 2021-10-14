#Setting working directory and importing data
dir <- "/Users/mariaonassis/Documents/Advanced Statistics & Programming/R tutorials/"
dirData <- paste0(dir, "Data/")

dfcovid <- read.table(file=paste0(dirData, "owid-covid-data.csv"), sep = ",",quote="")
schools <- read_excel("Documents/Advanced Statistics & Programming/R tutorials/Data/schools.xlsx", 
                            col_types = c("numeric", "text", "text", 
                                                    "text", "skip"))
library(dplyr)
library(janitor)
dfcovid <- dfcovid %>%
  row_to_names(row_number = 1)

#change the column names to be able to match
colnames(dfcovid)[3] <- "Country"
colnames(dfcovid)[4] <- "Date"

#make numerical
dfcovid$new_cases_per_million <- as.numeric(as.character(dfcovid$new_cases_per_million))
dfcovid$population_density <- as.numeric(as.character(dfcovid$population_density))

#subset only February and March of 2020
schools$month <- month(schools$Date)
schools$year <- year(schools$Date)
schools$day <- day(schools$Date)

dfcovid$Date <- as.Date(as.character(dfcovid$Date))
dfcovid$month <- month(dfcovid$Date)
dfcovid$year <- year(dfcovid$Date)
dfcovid$day <- day(dfcovid$Date)

subschools <- subset(schools, year == 2020)
subschools <- subset(subschools, month == 3)

subcovid <- subset(dfcovid, year == 2020)
subcovid <- subset(dfcovid, month == 3)

#drop partially open
subschools <- subset(subschools, Status != "Partially open")

subschools$closure <- ifelse(subschools$Status == "Fully open", 0, 1)

#drop columns we dont need from subcovid
subcovid <- subcovid %>%
  select(Country, Date, new_cases_per_million, population_density, median_age)

#merge the two dataframes
covidcomb <- left_join(subcovid, subschools, by=c("Country","Date"))

#complete cases based on new cases
covidcomb.comp <- subset(covidcomb, new_cases_per_million >0)

#complete cases of school status
covidcomb.comp <- subset(covidcomb.comp, !is.na(closure))

#subset from first two weeks of march
before <- subset(covidcomb.comp, Date > "2020-02-29")
before <- subset(before, Date < "2020-03-16")
before$period<- ifelse (before$Date < "2020-03-17",0,1 )

after <- subset(covidcomb.comp, Date > "2020-03-16")
after <- subset(after, Date < "2020-04-01")
after$period<- ifelse (after$Date > "2020-03-15",1,0 )

#average per country
before.avg <- 
  ddply(before, .(Country), summarise,
        avg.closure   = mean(closure, na.rm=TRUE),
        avg.cases = mean(new_cases_per_million, na.rm=TRUE))

after.avg <- 
  ddply(after, .(Country), summarise,
        avg.closureafter   = mean(closure, na.rm=TRUE),
        avg.casesafter = mean(new_cases_per_million, na.rm=TRUE))

#merge the averages by country
try <- merge(before.avg, after.avg, by="Country")

#remove all countries that were already closed in the first period
try2 <- subset(try, avg.closure < 0.5)

try.longclosure <-
  melt(try2,      
       id.vars= "Country",   
       measure.vars=c("avg.closure", "avg.closureafter"),  
       variable.name = "period",  
       value.name = "closure")

try.longcases <-
  melt(try2,      
       id.vars= "Country",   
       measure.vars=c("avg.cases", "avg.casesafter"),  
       variable.name = "period",  
       value.name = "cases")

#change name of period values to 0 or 1
try.longclosure$period <- ifelse(try.longclosure$period == "avg.closure", 0,1)
try.longcases$period <- ifelse(try.longcases$period == "avg.cases", 0,1)

covidfinaltry <- merge(try.longclosure, try.longcases, by=c("Country", "period"))

#merge before after datasets and define treatment and control
dfagg <- aggregate(x = covidfinaltry$closure, by = list(covidfinaltry$Country), FUN = sum)

colnames(dfagg)[colnames(dfagg)=="Group.1"]   <- "Country"
covidfinal2 <- left_join(covidfinaltry, dfagg, by="Country")

covidfinal2$D <- ifelse(covidfinal2$x < 0.5, 0, 1)

#make the table
avgcases <- ddply(covidfinal2, .(period, D), summarise,
                 averagecases=mean(cases, na.rm=TRUE))
#remodel the table
tmp <- dcast(avgcases, period ~ D, value.var="averagecases") 
tmp <- rbind(tmp, tmp[2,]-tmp[1,])

#rename the row names
rownames(tmp) <- c("Before", "After", "Difference")
tmp[3, "period"] <- NA

stargazer(tmp, summary = FALSE, align=TRUE)

#Define models
mdlA <- cases ~ D + period + D:period
rsltOLSA <- lm(mdlA, data=covidfinal2)
stargazer(rsltOLSA,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE, type="text")


#Control Variable: Population Density
pdagg <- aggregate(x = dfcovid$population_density, by = list(dfcovid$Country), FUN = mean)
colnames(pdagg)[colnames(pdagg)=="Group.1"]   <- "Country"
covidfinal2 <- left_join(covidfinal2, pdagg, by="Country")
colnames(covidfinal2)[colnames(covidfinal2)=="x.y"]   <- "PopulationDensity"

mdlB <- cases ~ D + period + D:period + PopulationDensity
rsltOLSB <- lm(mdlB, data=covidfinal2)
stargazer(rsltOLSA, rsltOLSB,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE)

#Make graphs
covidfinal2$Period <- ifelse (covidfinal2$period == 0, " Before", "After")
covidfinal2$Status <- ifelse (covidfinal2$D == 0, "Open", "Closed")

bar <- ggplot(covidfinal2, aes(Period, cases, fill=Period))
bar + stat_summary(fun= mean, geom= "bar") + stat_summary(fun.data= mean_cl_normal, geom="errorbar", width= 0.2) + facet_wrap(~ Status) + labs(x= "School Closure", y= "New cases per million")

#Breusch-Pagan test
lmtest::bptest(rsltOLSB)
#p-value is 0.3106

