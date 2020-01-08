rm(list=ls())

#libraries
library(ggplot2)
library(dplyr)
library(zoo)
library(e1071)
library(randomForest)
library(rpart)
library(lubridate)

#Functions
#----------------------------------------------------------------------------------------

#function for moving average
roll_mean <- function(x,win_size){
   xlen <- length(x)
   roll_mean <- numeric()
   for(i in 1:xlen){roll_mean[i] <- mean(x[max(1,i-win_size):min(length(x),i+win_size)])}
   return(roll_mean)
     }

#-----------------------------------------------------------------------------------------


#Read in Data
#----------------------------------------------------------------------------------------------------------------------

traps_DF <- read.csv("Data/Trap Data/ALCMdata_CLEANED.csv",stringsAsFactors = F) 
traps_DF <- traps_DF %>% group_by(Orchard,Season) %>% mutate(sum = cumsum(ALCM_count)) %>% as.data.frame()
traps_DF$rate <- traps_DF$ALCM_count/traps_DF$No_traps
traps_DF$Season <- as.factor(traps_DF$Season)
traps_DF$Season2 <- as.numeric(traps_DF$Season)
traps_DF$week_diff <- c(ymd(traps_DF$week_dat) - floor_date(ymd(traps_DF$start_date), "weeks",week_start = 1))/7

traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(prop_rate = rate/sum(rate)) %>%as.data.frame()



#Remove these are Sandy road, St Georges road, Robinson and maybe Lawn rd. 
traps_DF <- traps_DF[!traps_DF$Orchard %in% c("Sandy Rd", "St Georges Rd", "Robinson", "Lawn Rd"),]

#Organic OrchardsTe Koha, Falls, Norton road, Te Aute
traps_DF$Organic <- ifelse(traps_DF$Orchard %in% c("Te Koha", "Falls", "Norton road", "Te Aute"),1,0)

#keep only last 5 monitoring years
traps_DF <- traps_DF[traps_DF$Season2 %in% 9:15,]

#calculating the lag for each orchard
traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(lag_rate = lag(rate),
         lag_rate2 = lag(rate,2)) %>% as.data.frame()


ggplot(traps_DF[traps_DF$Season2 %in%15,],aes(x=week_diff,y=rate,group=Orchard)) + geom_line() +facet_wrap(~Orchard)

#----------------------------------------------------------------------------------------------------------------------



#Dealing with missing climate data
#----------------------------------------------------------------------------------------------------------------------
#impute missing rainfall and temp data as previous weeks measurements

clim_miss <- traps_DF[which(rowSums(is.na(traps_DF[,c("total_rainfall","mean_temp")]))==2),]

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

#--------------------#
#--      Trees     --#
#--------------------#

#----------------------------------------------------------------------------------------------------
#Season k-fold CV
#CV

t1 <- rpart(rate~CGDD + cal_week + Region + station + mean_temp + total_rainfall +lag_rate,data=traps_DF)
plot(t1)
text(t1)

plot(traps_DF$rate[2300:2421],type = "l")
lines(predict(t1)[2300:2421],col=2,lty=2)


traps_DF$tree_pred_season <- NA

levels(traps_DF$station)
traps_DF$Orchard <- as.factor(traps_DF$Orchard)
traps_DF$cal_week <- as.numeric(traps_DF$cal_week)

trap_DF <- traps_DF[-which(is.na(traps_DF$lag_count)),]


for(j in 1:length(unique(traps_DF$station))){
  df <- traps_DF[traps_DF$station %in% unique(traps_DF$station)[j],]
  n_length <- length(unique(df$Season2))
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    tree_mod <- rpart(ALCM_count~ CGDD + Total_Rainfall + Mean_meantemp + Season2 + as.numeric(cal_week)
                      + Organic,  data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2",
                                                        "cal_week","Organic")])
    
  
    traps_DF[traps_DF$station %in% unique(traps_DF$station)[j] & traps_DF$Season2 %in% unique(df$Season2)[i],c("tree_pred_season")] <- tree_pred  
  }
  
  
}

#-------------------------------------------------------------------------------------------------

#--------------------#
#--      RFore     --#
#--------------------#
#WARNING: THIS TAKES AGES
#----------------------------------------------------------------------------------------------------
#CV
traps_DF$for_pred_season <- NA
traps_DF_rough_fix <- na.roughfix(traps_DF[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2",
                                              "cal_week","Organic","lag_count","lag_count2","ALCM_count")])
traps_DF_rough_fix$station <- traps_DF$station

#using total counts
a1 <- Sys.time()
for(j in 1:length(unique(traps_DF$station))){
  df <- traps_DF_rough_fix[traps_DF_rough_fix$station %in% unique(traps_DF_rough_fix$station)[j],]
  n_length <- length(unique(df$Season2))
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    tree_mod <- randomForest(ALCM_count~ CGDD + Total_Rainfall + Mean_meantemp + Season2 + cal_week
                      + Organic + lag_count + lag_count2, data=train_df, n.tree=3000)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","Total_Rainfall","Mean_meantemp","Season2",
                                                     "cal_week","Organic","lag_count","lag_count2")])
    
    
    traps_DF[traps_DF$station %in% unique(traps_DF_rough_fix$station)[j] & traps_DF$Season2 %in% unique(df$Season2)[i],c("for_pred_season")] <- tree_pred  
  }

}

a2 <- Sys.time()
print(a1-a2)

x_seq <- 1000:1100
plot(traps_DF$rate[x_seq],type="l",lty=2)
lines(traps_DF$for_pred_rate[x_seq],col="red")

plot(traps_DF$ALCM_count[x_seq],type="l",lty=2)
lines(traps_DF$for_pred_season[x_seq],col="red",type="l")


#-------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------


#--------------------#
#--    Results     --#
#--------------------#

#----------------------------------------------------------------------------------------------------
traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(tree_pred_season_prop = tree_pred_season/sum(tree_pred_season),
         for_pred_season_prop = for_pred_season/sum(for_pred_season)) %>% as.data.frame()
plot(log(traps_DF$ALCM_count+1) ~ log(traps_DF$lag_count+1))

#trees
ggplot(traps_DF[traps_DF$Season2 %in% 9,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 10,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 11,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 12,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 13,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 14,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 15,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season, group=Orchard),col="red")

#prop
#trees
ggplot(traps_DF[traps_DF$Season2 %in% 9,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 10,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 11,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 12,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 13,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 14,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 15,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=tree_pred_season_prop, group=Orchard),col="red")


#random forest

ggplot(traps_DF[traps_DF$Season2 %in% 9,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 10,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 11,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 12,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 13,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 14,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 15,], aes(x=monitoring_week, y=ALCM_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season, group=Orchard),col="red")

#---

ggplot(traps_DF[traps_DF$Season2 %in% 9,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 10,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 11,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 12,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 13,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 14,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season_prop, group=Orchard),col="red")

ggplot(traps_DF[traps_DF$Season2 %in% 15,], aes(x=monitoring_week, y=prop_count, group=Orchard)) + geom_line() +
  facet_wrap(~Orchard) + geom_line(aes(x=monitoring_week, y=for_pred_season_prop, group=Orchard),col="red")


#----------------------------------------------------------------------------------------------------


#---------------------------#
#--    Obtaining Peaks    --#
#---------------------------#

#----------------------------------------------------------------------------------------------------

pred_df <- traps_DF[,c("monitoring_week","Season2","Orchard","station","prop_count","tree_pred_season_prop","cal_week")]


pred_df <- pred_df %>% group_by(Season2,Orchard) %>%
  mutate(data_peak = ifelse(prop_count>roll_mean(prop_count,5),1,0),
         pred_peak = ifelse(tree_pred_season_prop>roll_mean(tree_pred_season_prop,5),1,0)) %>% as.data.frame()


pred_df[pred_df$Season2 %in% 15 & pred_df$Orchard %in% "Nagel","data_peak"]
pred_df[pred_df$Season2 %in% 15 & pred_df$Orchard %in% "Nagel","pred_peak"]

plot(pred_df[pred_df$Season2 %in% 15 & pred_df$Orchard %in% "Nagel","prop_count"])
lines(pred_df[pred_df$Season2 %in% 15 & pred_df$Orchard %in% "Nagel","tree_pred_season_prop"])

par(mfrow=c(2,1))
plot(pred_df[pred_df$Season2 %in% 15 & pred_df$Orchard %in% "Nagel","prop_count"],type="l")
lines(pred_df[pred_df$Season2 %in% 15 & pred_df$Orchard %in% "Nagel","tree_pred_season_prop"],col="red")

plot(pred_df[pred_df$Season2 %in% 15 & pred_df$Orchard %in% "Nagel","data_peak"],type="l")
lines(pred_df[pred_df$Season2 %in% 15 & pred_df$Orchard %in% "Nagel","pred_peak"],col="red")



