rm(list=ls())

#libraries
library(ggplot2)
library(dplyr)
library(zoo)
library(e1071)
library(randomForest)
library(rpart)

#Read in Data



#----------------------------------------------------------------------------------------------------------------------

traps_DF <- read.csv("Data/Trap Data/ALCMdata_CLEANED.csv",stringsAsFactors = F) 
traps_DF$week <- NULL
traps_DF <- traps_DF %>% group_by(District,Season) %>% mutate(sum = cumsum(ALCM_count)) %>% as.data.frame()
traps_DF$week_dat <- as.Date(traps_DF$week_dat, format='%Y-%m-%d')
traps_DF$cal_week <- strftime(traps_DF$week_dat, format = "%V")
traps_DF$rate <- traps_DF$ALCM_count/traps_DF$No_traps
traps_DF <- traps_DF %>% group_by(District, Season) %>% mutate(sumCGDD = cumsum(weekly_CGDD)) %>% as.data.frame()

traps_DF$Season <- as.factor(traps_DF$Season)
traps_DF$Season2 <- as.numeric(traps_DF$Season)

traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(prop_count = ALCM_count/max(sum)) %>%as.data.frame()

#Remove these are Sandy road, St Georges road, Robinson and maybe Lawn rd. 
traps_DF <- traps_DF[!traps_DF$Orchard %in% c("Sandy Rd", "St Georges Rd", "Robinson", "Lawn Rd"),]

#Organic OrchardsTe Koha, Falls, Norton road, Te Aute
traps_DF$Organic <- ifelse(traps_DF$Orchard %in% c("Te Koha", "Falls", "Norton road", "Te Aute"),1,0)
traps_DF[traps_DF$Orchard %in% "Te Koha",]

#keep only last 5 monitoring years
traps_DF <- traps_DF[traps_DF$Season2 %in% 10:15,]

#calculating the lag for each orchard
traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(lag_count = lag(ALCM_count)) %>% as.data.frame()

traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(lag_count2 = lag(ALCM_count,2)) %>% as.data.frame()


#----------------------------------------------------------------------------------------------------------------------

#Dealing with missing climate data
#----------------------------------------------------------------------------------------------------------------------
#impute missing rainfall and temp data as previous weeks measurements

clim_miss <- traps_DF[which(rowSums(is.na(traps_DF[,c("Total_Rainfall","Mean_mintemp","Mean_maxtemp","Mean_meantemp")]))==4),]

for(i in 1:length(clim_miss[,1])){
  week <- c(clim_miss$monitoring_week[i]-1,clim_miss$monitoring_week[i])
  traps_DF[traps_DF$Orchard%in% clim_miss$Orchard[i] & traps_DF$Season%in% clim_miss$Season[i] & traps_DF$monitoring_week %in% week,]
  
  traps_DF[traps_DF$Orchard%in% clim_miss$Orchard[i] & traps_DF$Season%in% clim_miss$Season[i] & traps_DF$monitoring_week %in% week[2],
           c("Total_Rainfall","Mean_mintemp","Mean_maxtemp","Mean_meantemp")] <-
    traps_DF[traps_DF$Orchard%in% clim_miss$Orchard[i] & traps_DF$Season%in% clim_miss$Season[i] & traps_DF$monitoring_week %in% week[1],
             c("Total_Rainfall","Mean_mintemp","Mean_maxtemp","Mean_meantemp")] }

#crosses Rd
traps_DF[traps_DF$Orchard %in% "Crosses Road" & traps_DF$Season2 %in% 10,
         c("weekly_CGDD","Mean_mintemp","Mean_maxtemp","Mean_meantemp")] <-
  traps_DF[traps_DF$station %in% "HAV" & traps_DF$Season2 %in% 10 & 
             traps_DF$cal_week %in% c(38) & traps_DF$Orchard %in% "Mt Erin", c("weekly_CGDD","Mean_mintemp","Mean_maxtemp","Mean_meantemp")]

#----------------------------------------------------------------------------------------------------------------------

HAV <- traps_DF[traps_DF$station %in% "HAV",]
CLD <- traps_DF[traps_DF$station %in% "CLD",]
LND <- traps_DF[traps_DF$station %in% "LND",]
PAK <- traps_DF[traps_DF$station %in% "PAK",]
RIR <- traps_DF[traps_DF$station %in% "RIR",]
RXE <- traps_DF[traps_DF$station %in% "RXE",]

HAV$pred_tree <- NA

for(i in 1:dim(HAV)[1]){
t1 <- rpart(ALCM_count~ CGDD + I(CGDD^2) +I(CGDD^3) +Organic + Total_Rainfall + Mean_meantemp + as.numeric(cal_week) + Season2+lag_count+lag_count2, data=HAV[-i,])
HAV$pred_tree[i] <- predict(t1, newdata=HAV[i,c("CGDD","Organic","Total_Rainfall","Mean_meantemp","cal_week","Season2","lag_count","lag_count2")])
}

ggplot(HAV,
       aes(x=monitoring_week, y=ALCM_count)) + geom_line() + facet_wrap(~interaction(Orchard,Season)) +
       geom_line(data=HAV, aes(x=monitoring_week, y=pred_tree, group=Season),col="red")
  

CLD$pred_tree <- NA

for(i in 1:dim(CLD)[1]){
  t1 <- rpart(ALCM_count~ CGDD + I(CGDD^2) +I(CGDD^3) +Organic + Total_Rainfall + Mean_meantemp + as.numeric(cal_week) + Season2+lag_count+lag_count2, data=CLD[-i,])
  CLD$pred_tree[i] <- predict(t1, newdata=CLD[i,c("CGDD","Organic","Total_Rainfall","Mean_meantemp","cal_week","Season2","lag_count","lag_count2")])
}

ggplot(CLD[CLD$Season2 %in% 10:15,],
       aes(x=monitoring_week, y=ALCM_count)) + geom_line() + facet_wrap(~interaction(Orchard,Season)) +
  geom_line(data=CLD[CLD$Season2 %in% 10:15,], aes(x=monitoring_week, y=pred_tree, group=Season),col="red")

LND$pred_tree <- NA

for(i in 1:dim(LND)[1]){
  t1 <- rpart(ALCM_count~ CGDD + I(CGDD^2) +I(CGDD^3) +Organic + Total_Rainfall + Mean_meantemp + as.numeric(cal_week) + Season2+lag_count+lag_count2, data=LND[-i,])
  LND$pred_tree[i] <- predict(t1, newdata=LND[i,c("CGDD","Organic","Total_Rainfall","Mean_meantemp","cal_week","Season2","lag_count","lag_count2")])
}

ggplot(LND,
       aes(x=monitoring_week, y=ALCM_count)) + geom_line() + facet_wrap(~interaction(Orchard,Season)) +
  geom_line(data=LND, aes(x=monitoring_week, y=pred_tree, group=Season),col="red")
plot(t1)
text(t1)

table(traps_DF$station)

PAK$pred_tree <- NA

for(i in 1:dim(PAK)[1]){
  t1 <- rpart(ALCM_count~ CGDD + I(CGDD^2) +I(CGDD^3) +Organic + Total_Rainfall + Mean_meantemp + as.numeric(cal_week) + Season2+lag_count+lag_count2, data=PAK[-i,])
  PAK$pred_tree[i] <- predict(t1, newdata=PAK[i,c("CGDD","Organic","Total_Rainfall","Mean_meantemp","cal_week","Season2","lag_count","lag_count2")])
}

ggplot(PAK,
       aes(x=monitoring_week, y=ALCM_count)) + geom_line() + facet_wrap(~interaction(Orchard,Season)) +
  geom_line(data=PAK, aes(x=monitoring_week, y=pred_tree, group=Season),col="red")
plot(t1)
text(t1)

RIR$pred_tree <- NA

for(i in 1:dim(RIR)[1]){
  t1 <- rpart(ALCM_count~ CGDD + I(CGDD^2) +I(CGDD^3) +Organic + Total_Rainfall + Mean_meantemp + as.numeric(cal_week) + Season2+lag_count+lag_count2, data=RIR[-i,])
  RIR$pred_tree[i] <- predict(t1, newdata=RIR[i,c("CGDD","Organic","Total_Rainfall","Mean_meantemp","cal_week","Season2","lag_count","lag_count2")])
}

ggplot(RIR,
       aes(x=monitoring_week, y=ALCM_count)) + geom_line() + facet_wrap(~interaction(Orchard,Season)) +
  geom_line(data=RIR, aes(x=monitoring_week, y=pred_tree, group=Season),col="red")
plot(t1)
text(t1)

RXE$pred_tree <- NA

for(i in 1:dim(RXE)[1]){
  t1 <- rpart(ALCM_count~ CGDD + I(CGDD^2) +I(CGDD^3) +Organic + Total_Rainfall + Mean_meantemp + as.numeric(cal_week) + Season2+lag_count+lag_count2, data=RXE[-i,])
  RXE$pred_tree[i] <- predict(t1, newdata=PAK[i,c("CGDD","Organic","Total_Rainfall","Mean_meantemp","cal_week","Season2","lag_count","lag_count2")])
}

ggplot(RXE,
       aes(x=monitoring_week, y=ALCM_count)) + geom_line() + facet_wrap(~interaction(Orchard,Season)) +
  geom_line(data=RXE, aes(x=monitoring_week, y=pred_tree, group=Season),col="red")
plot(t1)
text(t1)


