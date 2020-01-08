#packages
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

#Trap Data Mgmt
#-------------------------------------------------------------------------
HB <- read.xlsx(xlsxFile = "Data/Trap Data/ALCM trap catches 2004-2019 .xlsx", 
              sheet = "HB", skipEmptyRows = T, startRow = 1, na.strings = "*")

HB <- HB[,1:9]

HB$Date <- convertToDate(HB$Date)

Nelson <- read.xlsx(xlsxFile = "Data/Trap Data/ALCM trap catches 2004-2019 .xlsx", 
              sheet = "Nelson", skipEmptyRows = T, startRow = 1, na.strings = "*")
Nelson$Date <- convertToDate(Nelson$Date)


Otago <- read.xlsx(xlsxFile = "Data/Trap Data/ALCM trap catches 2004-2019 .xlsx", 
                   sheet = "Central Otago", skipEmptyRows = T, startRow = 1, na.strings = "*")


Otago$Date <- convertToDate(Otago$Date)


Data_FULL <- rbind(HB, Nelson, Otago)
unique(Data_FULL[,8])
Data_FULL[is.na(Data_FULL[,9]),]

rm(HB, Nelson,Otago)
#removing observaions that do not have a district attached to them
Data_FULL <- Data_FULL[-which(is.na(Data_FULL[,3])),]

#renaming TE Aute to Te Aute
table(Data_FULL$Orchard)
Data_FULL[Data_FULL$Orchard %in% "TE Aute","Orchard"] <- "Te Aute"

#making some of the variables factors
Data_FULL[,c(1:3,5,6,7)] <- lapply(Data_FULL[,c(1:3,5,6,7)],function(x) as.factor(x))

#making a station variable for the climate data
Data_FULL$station <- Data_FULL$District

#renaming the levels
levels(Data_FULL$station)  <- c("CLD", "HAV","RIR","PAK","RXE", "LND")

#taking out the calendar week from the date
Data_FULL$week_dat <-  floor_date(Data_FULL$Date, "week",week_start = 1)

#calcualting the difference in weeks from 1st of Sptember
start_date <- as.Date(paste(2004:2018,"-09-01",sep=""),format="%Y-%m-%d")
Data_FULL$start_date <- factor(Data_FULL$Season)
levels(Data_FULL$start_date) <- start_date

Data_FULL$start_date <- as.Date(Data_FULL$start_date,format="%Y-%m-%d")

#Finding the number of traps set by orchard each year
Data_FULL2 <- data.frame()

for(i in 1:15){
df <- Data_FULL[Data_FULL$Season %in% levels(Data_FULL$Season)[i],]
df$Orchard <- factor(df$Orchard)
n.orchards <- length(unique(df$Orchard))

for(j in 1:n.orchards){
  df2 <- df[df$Orchard %in% levels(df$Orchard)[j], ]
  N_traps <- data.frame(week_dat = as.Date(names(rowSums(table(df2$week_dat,df2$Trap))), format="%Y-%m-%d"),
             No_traps=rowSums(table(df2$week_dat,df2$Trap)))

  df1 <- merge(df2,N_traps,by="week_dat")
  Data_FULL2 <- rbind(Data_FULL2,df1)
  }

}

Data_FULL2$cal_week <- week(Data_FULL2$week_dat)
Data_FULL2$year <- year(Data_FULL2$week_dat)

#Aggregating into weekly counts
for(i in 1:15){
print(max(Data_FULL[Data_FULL2$Season %in% levels(Data_FULL2$Season)[i],"Date"]))}

Data_2 <- aggregate(
  x = Data_FULL2[c("No..of.ALCM")],
  by = Data_FULL2[c("year","cal_week","week_dat","start_date","Season",
                    "District","Orchard","station","No_traps","Region")],
  FUN = sum, na.rm = TRUE
)


max(Data_2$No_traps)
Data_2[Data_2$No_traps %in%9,]

Data_FULL2[Data_FULL2$cal_week %in% 49 & Data_FULL2$Season %in% "2005-06" & Data_FULL2$Orchard %in% "Mt Erin",]
Data_FULL2[Data_FULL2$week_diff %in% 18 & Data_FULL2$Season %in% "2005-06" & Data_FULL2$Orchard %in% "Mt Erin",]
colnames(Data_2)[11] <- "ALCM_count"

rm(Data_FULL,Data_FULL2,df,df1,df2,N_traps, n.orchards)
#-----------------------------------------------------------


#Leaf wetness Data
#------------------------------------------------------------

#read in leafwetness data
LF_wet <- read.csv("Data/Leafwetness/LF_day.csv")
colnames(LF_wet)[1] <- c("Date")
LF_wet$Date <- as.Date(substr(LF_wet[,1],1,10),format="%Y-%m-%d",tz="NZ")
LF_wet$month <- format(LF_wet$Date, "%m")
#taking out the calendar week from the date
LF_wet$week_dat <-  floor_date(LF_wet$Date, "week",week_start = 1)

LF_wet$cal_week <- week(LF_wet$week_dat)
LF_wet$year <- year(LF_wet$week_dat)

#calculating the number of hours wet

LW_dat <- LF_wet[,c("station","year","cal_week","hours_wet")] %>% group_by(station, cal_week, year) %>%
         mutate(hrs_wet=sum(hours_wet)) %>% filter(row_number(hrs_wet) == 1) %>% as.data.frame()
LW_dat$hours_wet <- NULL
colnames(LW_dat)

Data_2 <- left_join(Data_2, LW_dat, by=c("station","year","cal_week"))
Data_2 <- Data_2[with(Data_2, order(year,Orchard)),]

rm(LF_wet, LW_dat)
#---------------------------------------------------------------

#Growing Degree Days
#----------------------------------------------------------------
GDD_files <- paste("Data/Rainfall and Temp/",list.files("Data/Rainfall and Temp"),sep="")
GDD_data <- lapply(GDD_files[2:3],read.csv)
GDD_data <- do.call(rbind, GDD_data)

GDD_data$Date <- as.Date(GDD_data$Start.Date, format="%Y-%m-%d")

GDD_data$week_dat <-  floor_date(GDD_data$Date, "week",week_start = 1)
GDD_data$cal_week <- week(GDD_data$week_dat)
GDD_data$year <- year(GDD_data$week_dat)
colnames(GDD_data)[1] <- "station"

head(GDD_data[GDD_data$cal_week %in% 39 & GDD_data$year %in% 2004,])
head(Data_2[Data_2$cal_week %in% 39 & Data_2$year %in% 2004,])


#daily GDD with base temperature of 7
GDD_data$GDD_daily <- GDD_data$Temperature.Dry.Bulb.Mean - 7
GDD_data$GDD_daily <- ifelse(GDD_data$GDD_daily<0, 0 , GDD_data$GDD_daily)

GDD_dat <- GDD_data[,c("station","year","cal_week","week_dat","GDD_daily", "Temperature.Dry.Bulb.Mean","Rain.Total")] %>% 
  group_by(station, cal_week,week_dat, year) %>%
  mutate(GDD_week=sum(GDD_daily),
         mean_temp=mean(Temperature.Dry.Bulb.Mean,na.rm=T),
         total_rainfall = sum(Rain.Total, na.rm=T)) %>% filter(row_number(GDD_week) == 1) %>% as.data.frame()
GDD_dat$GDD_daily <- NULL ; GDD_dat$Temperature.Dry.Bulb.Mean <- NULL; GDD_dat$Rain.Total <- NULL
head(Data_2$week_dat)


WD <- seq(floor_date(as.Date("2004-01-01",format="%Y-%m-%d"),"week",week_start = 1), 
          floor_date(as.Date("2020-01-01",format="%Y-%m-%d"),"week",week_start = 1),by="1 week")


fillin_dat <- expand.grid(week_dat=WD,station=levels(GDD_dat$station))
fillin_dat$year <- year(fillin_dat$week_dat)
fillin_dat$cal_week <- week(fillin_dat$week_dat)
fillin_dat <- fillin_dat[with(fillin_dat, order(station)),]

GDD_data2 <- left_join(fillin_dat, GDD_dat, by=colnames(fillin_dat))

fillin_dat[fillin_dat$cal_week %in% 27 & fillin_dat$year %in% 2004,]
head(GDD_data[GDD_data$cal_week %in% 27 & GDD_data$year %in% 2004,])
rm(fillin_dat,GDD_data,GDD_data,GDD_files)

GDD_data2 <- GDD_data2[with(GDD_data2, order(year,station)),]

data_all <- data.frame()

i=j=1
for(j in 1:length(levels(Data_2$Season))){
df<- Data_2[Data_2$Season %in% levels(Data_2$Season)[j],]
n_stat <- length(unique(as.character(df$station)))

for(i in 1:n_stat){
print(c(j,i))
df2 <- df[df$station %in% unique(df$station)[i],]
stat <- unique(df2$station)
yrs <- c(min(df2$year),max(df2$year))

start_GDD <- floor_date(as.Date(paste(yrs[1],"-07-01", sep=""), format="%Y-%m-%d"), "week",week_start=1)
dates_look <- df2[c(which.min(df2[df2$year %in% yrs[1],"week_dat"]),which.max(df2[df2$year %in%yrs[2],"week_dat"])),"week_dat"]

GDD_dat_yr <- GDD_data2[GDD_data2$station %in% stat & GDD_data2$year %in% yrs,]
if(dim(GDD_dat_yr)[1]<1) next
GDD_dat_yr <- GDD_dat_yr[which(GDD_dat_yr$week_dat %in% start_GDD):dim(GDD_dat_yr)[1],]
GDD_dat_yr$GDD <- ifelse(is.na(GDD_dat_yr$GDD_week), 0,GDD_dat_yr$GDD_week)
GDD_dat_yr$CGDD <- cumsum(GDD_dat_yr$GDD)
GDD_dat_yr$GDD <- NULL

df3 <- left_join(df2, GDD_dat_yr)
data_all <- rbind(data_all,df3)
}

}


#write.csv(data_all, file="Data/Trap Data/ALCMdata_CLEANED.csv", row.names=F)

ggplot(data_all, aes(x=week_dat,y=CGDD, group=Orchard)) + geom_line() + facet_wrap(~interaction(Season,station))
