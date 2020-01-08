rm(list=ls())

#libraries
library(ggplot2)
library(dplyr)
library(zoo)
library(e1071)
library(randomForest)
library(rpart)
library(lubridate)
library(gbm)

#Functions
#----------------------------------------------------------------------------------------

#function for moving average
roll_mean <- function(x,win_size){
  xlen <- length(x)
  roll_mean <- numeric()
  for(i in 1:xlen){roll_mean[i] <- mean(x[max(1,i-win_size):min(length(x),i+win_size)])}
  return(roll_mean)
}

#----------------------------------------------------------------------------------------------------------------------

#-----------------------#
#     Read in Data      #
#-----------------------#

#----------------------------------------------------------------------------------------------------------------------

traps_DF <- read.csv("Data/Trap Data/ALCMdata_CLEANED.csv",stringsAsFactors = F) 
traps_DF$week <- NULL

traps_DF$avg_GDD <- NULL
traps_DF$sumCGDD <- NULL
traps_DF$weekly_CGDD <- NULL

traps_DF <- traps_DF %>% group_by(District,Season) %>% mutate(sum = cumsum(ALCM_count)) %>% as.data.frame()
traps_DF$week_dat <- as.Date(traps_DF$week_dat, format='%Y-%m-%d')
traps_DF$cal_week <-week(traps_DF$week_dat)
traps_DF$rate <- traps_DF$ALCM_count/traps_DF$No_traps
traps_DF$Season <- as.factor(traps_DF$Season)
traps_DF$Season2 <- as.numeric(traps_DF$Season)


#----------------------------------------------------------------------------------------------------------------------

#-----------------------#
#  Data preprocessing   #
#-----------------------#

#----------------------------------------------------------------------------------------------------------------------


traps_DF$meantemp_og <- traps_DF$mean_temp
traps_DF$total_rainfall_og <- traps_DF$total_rainfall


#impute missing rainfall and tempearure by mean
traps_DF <- traps_DF %>% group_by(station, Season)  %>% 
  mutate(mean_temp = na.roughfix(mean_temp),
         total_rainfall = na.roughfix(total_rainfall)) %>% as.data.frame()

traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(prop_count = ALCM_count/max(sum)) %>%as.data.frame()

traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(prop_rate = rate/sum(rate)) %>%as.data.frame()

#Remove these are Sandy road, St Georges road, Robinson and maybe Lawn rd. 
traps_DF <- traps_DF[!traps_DF$Orchard %in% c("Sandy Road", "St Georges Rd", "Robinson", "Lawn Rd","Crosses Road", "Whakarewa St", "Williams", "Te Aute"),]

#Organic OrchardsTe Koha, Falls, Norton road, Te Aute
traps_DF$Organic <- ifelse(traps_DF$Orchard %in% c("Te Koha", "Falls", "Norton road", "Te Aute"),1,0)

#calculate number of weeks from 1st of september
start_date <- as.Date(paste(2004:2018,"-09-01",sep=""),format="%Y-%m-%d")
traps_DF$start_date <- factor(traps_DF$Season)
levels(traps_DF$start_date) <- start_date

traps_DF$start_date <- as.Date(traps_DF$start_date,format="%Y-%m-%d")
traps_DF$week_diff <- as.numeric(round(difftime(traps_DF$week_dat,traps_DF$start_date,units="weeks")))


#keep only last 5 monitoring years=
traps_DF <- traps_DF[traps_DF$Season2 %in% 11:15,]
traps_DF$Season <- factor(traps_DF$Season)

#calculating the lag for each orchard
traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(lag_count = lag(ALCM_count),
         lag_temp = lag(mean_temp),
         lag_rain = lag(total_rainfall)) %>% as.data.frame()


traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(rate_norm = (rate - mean(rate))/sd(rate, na.rm=T)) %>% as.data.frame()

#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#            Mean              #
#                              #
#------------------------------#

Mean_obs <- traps_DF[,c("year","cal_week","week_dat","week_diff","Season2","station","Orchard","rate_norm")]
pred_mean <- data.frame()

for(i in 1: length(unique(traps_DF$Orchard))){
  Orc_DF  <- traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],]
  
  df <- data.frame(Orchard = unique(traps_DF$Orchard)[i],
                   week_diff = as.numeric(names(apply(xtabs(Orc_DF$rate_norm~Orc_DF$week_diff+Orc_DF$Season2),1,function(x)mean(x, na.rm=T)))), 
                   mean_rate = apply(xtabs(Orc_DF$rate_norm~Orc_DF$week_diff+Orc_DF$Season2),1,function(x)mean(x, na.rm=T))) 
  
  pred_mean <- rbind(pred_mean, df)
}

Mean_obs <- left_join(Mean_obs, pred_mean)
traps_DF <- left_join(traps_DF, pred_mean)

ggplot(traps_DF[traps_DF$Season2 %in% 14,], aes(x=week_diff, y=rate_norm)) + geom_line() + facet_wrap(~Orchard) +
  geom_line(data=traps_DF[traps_DF$Season2 %in% 14,], aes(x=week_diff, y=mean_rate), col="red")


#----------------------------------------------------------------------------------------------------------------------


#----------------------------------#
#                                  #
#     summarising by region        #
#                                  #
#----------------------------------#

#----------------------------------------------------------------------------------------------------------------------

traps_DF2 <- traps_DF %>% group_by(Region, Season,Season2, week_diff) %>%
  summarise(rate = mean(rate),
            rate_norm = mean(rate_norm),
            mean_rate = mean(mean_rate),
            mean_temp = mean(mean_temp, na.rm=T),
            mean_RF = mean(total_rainfall, na.rm=T),
            mean_CGDD = mean(CGDD), 
            mean_lagtemp = mean(lag_temp, na.rm=T),
            mean_lagrain = mean(lag_rain, na.rm=T)) %>% as.data.frame()


ggplot(traps_DF2[traps_DF2$Season2 %in% 14,], aes(x=week_diff, y=rate_norm)) + geom_line() + facet_wrap(~Region) +
  geom_line(data=traps_DF2[traps_DF2$Season2 %in% 14,], aes(x=week_diff, y=mean_rate), col="red")


#-----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#        Regression Trees      #
#                              #
#------------------------------#

#----------------------------------------------------------------------------------------------------------------------
tree_pred_DF <- traps_DF2[,c("Season","Season2","Region","rate_norm","week_diff")]

tree_pred_DF$pred_ratenorm<- NA
j=1

for(j in 1:length(unique(traps_DF$Region))){
  df <- traps_DF2[traps_DF2$Region %in% unique(traps_DF2$Region)[j],]
  n_length <- length(unique(df$Season2))
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    
    #diff rate norm
    
    tree_mod <- rpart(rate_norm~ mean_CGDD + mean_RF + mean_temp +  week_diff +mean_lagrain+ mean_lagtemp + mean_rate, data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")])
    
    
    tree_pred_DF[tree_pred_DF$Region %in% unique(traps_DF2$Region)[j] & tree_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_ratenorm")] <- tree_pred  
    
    
    
  }
  
  
}


plot(tree_mod)
text(tree_mod)

#rate; rate, log rate, diff rate, diff_rate_norm

tree_rate_plots <- ggplot(tree_pred_DF, aes(x=week_diff, y=rate_norm, group=Region)) + geom_line() + 
   facet_wrap(~interaction(Region, Season)) + 
    geom_line(data=tree_pred_DF, aes(x=week_diff, y=pred_ratenorm, group=Region),col="red")
  


#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#        Random Forest         #
#                              #
#------------------------------#

#----------------------------------------------------------------------------------------------------------------------
RF_pred_DF <- traps_DF2[,c("Season","Season2","Region","rate_norm","week_diff")]

RF_pred_DF$pred_ratenorm<- NA
j=1

for(j in 1:length(unique(traps_DF$Region))){
  df <- traps_DF2[traps_DF2$Region %in% unique(traps_DF2$Region)[j],]
  n_length <- length(unique(df$Season2))
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    
    train_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")] <- 
      na.roughfix(train_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")])
    
    test_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")] <- 
      na.roughfix(test_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")])
    
    #diff rate norm
    
    tree_mod <- randomForest(rate_norm~ mean_CGDD + mean_RF + mean_temp +  week_diff +mean_lagrain+ mean_lagtemp + mean_rate, data=train_df,ntree=1000)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")])
    
    
    RF_pred_DF[RF_pred_DF$Region %in% unique(traps_DF2$Region)[j] & RF_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_ratenorm")] <- tree_pred  
    
    
    
  }
  
  
}


plot(tree_mod)
text(tree_mod)

#rate; rate, log rate, diff rate, diff_rate_norm

RF_rate_plots <- ggplot(RF_pred_DF, aes(x=week_diff, y=rate_norm, group=Region)) + geom_line() + 
  facet_wrap(~interaction(Region, Season)) + 
  geom_line(data=RF_pred_DF, aes(x=week_diff, y=pred_ratenorm, group=Region),col="red")



#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#             SVM              #
#                              #
#------------------------------#

#----------------------------------------------------------------------------------------------------------------------
SVM_pred_DF <- traps_DF2[,c("Season","Season2","Region","rate_norm","week_diff")]

SVM_pred_DF$pred_ratenorm<- NA
j=1

for(j in 1:length(unique(traps_DF$Region))){
  df <- traps_DF2[traps_DF2$Region %in% unique(traps_DF2$Region)[j],]
  n_length <- length(unique(df$Season2))
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    
    train_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")] <- 
      na.roughfix(train_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")])
    
    test_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")] <- 
      na.roughfix(test_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")])
    
    #diff rate norm
    
    tree_mod <- svm(rate_norm~ mean_CGDD + mean_RF + mean_temp +  week_diff +mean_lagrain+ mean_lagtemp + mean_rate, data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")])
    
    
    SVM_pred_DF[SVM_pred_DF$Region %in% unique(traps_DF2$Region)[j] & SVM_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_ratenorm")] <- tree_pred  
    
    
    
  }
  
  
}


plot(tree_mod)
text(tree_mod)

#rate; rate, log rate, diff rate, diff_rate_norm

SVM_rate_plots <- ggplot(SVM_pred_DF, aes(x=week_diff, y=rate_norm, group=Region)) + geom_line() + 
  facet_wrap(~interaction(Region, Season)) + 
  geom_line(data=SVM_pred_DF, aes(x=week_diff, y=pred_ratenorm, group=Region),col="red")


#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#             GBM              #
#                              #
#------------------------------#

#----------------------------------------------------------------------------------------------------------------------
gbm_pred_DF <- traps_DF2[,c("Season","Season2","Region","rate_norm","week_diff")]

gbm_pred_DF$pred_ratenorm<- NA
j=1

for(j in 1:length(unique(traps_DF$Region))){
  df <- traps_DF2[traps_DF2$Region %in% unique(traps_DF2$Region)[j],]
  n_length <- length(unique(df$Season2))
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    
    train_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")] <- 
      na.roughfix(train_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")])
    
    test_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")] <- 
      na.roughfix(test_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")])
    
    #diff rate norm
    
    tree_mod <- gbm(rate_norm~ mean_CGDD + mean_RF + mean_temp +  week_diff +mean_lagrain+ mean_lagtemp + mean_rate, data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("mean_CGDD" ,"mean_RF","mean_temp" ,"week_diff","mean_lagrain", "mean_lagtemp","mean_rate")],n.trees = 100)
    
    
    gbm_pred_DF[gbm_pred_DF$Region %in% unique(traps_DF2$Region)[j] & gbm_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_ratenorm")] <- tree_pred  
    
    
    
  }
  
  
}


plot(tree_mod)
text(tree_mod)

#rate; rate, log rate, diff rate, diff_rate_norm

GBM_rate_plots <- ggplot(gbm_pred_DF, aes(x=week_diff, y=rate_norm, group=Region)) + geom_line() + 
  facet_wrap(~interaction(Region, Season)) + 
  geom_line(data=gbm_pred_DF, aes(x=week_diff, y=pred_ratenorm, group=Region),col="red")


#----------------------------------------------------------------------------------------------------------------------
