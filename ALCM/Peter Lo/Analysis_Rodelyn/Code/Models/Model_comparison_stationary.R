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
         lag_count2 = lag(ALCM_count,2),
         lag_rate = lag(rate),
         lag_temp = lag(mean_temp),
         lag_rain = lag(total_rainfall)) %>% as.data.frame()

traps_DF$diff_count <- traps_DF$ALCM_count - traps_DF$lag_count
traps_DF$diff_rate <- traps_DF$rate - traps_DF$lag_rate

traps_DF <- traps_DF %>% group_by(Orchard,Season) %>%
  mutate(diff_count_norm = diff_count/sd(diff_count,na.rm=T),
         diff_rate_norm = diff_rate/sd(diff_rate,na.rm=T),
         rate_norm = (rate - mean(rate))/sd(rate)) %>% as.data.frame()



traps_DF2 <- traps_DF
traps_DF <- traps_DF[-(which(is.na(traps_DF$diff_count))),]

#periodicity

traps_DF$week_diff2 <- as.numeric(round(difftime(traps_DF$week_dat,unique(traps_DF$start_date)[1],units="weeks")))

traps_DF <- traps_DF[with(traps_DF, order(Orchard)),]

periodicity <- data.frame(Orchard=unique(traps_DF$Orchard),per=NA )

for(i in 1:18){
  ssp <- spectrum(traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],"rate_norm"])
  traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],"per_rate_norm"] <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
}


traps_DF$diff_ratenorm_sin2 <- sin(2*pi/traps_DF$per_rate_norm*traps_DF$week_diff2)
traps_DF$diff_ratenorm_cos2 <- cos(2*pi/traps_DF$per_rate_norm*traps_DF$week_diff2)

traps_DF$diff_ratenorm_sin4 <- sin(4*pi/traps_DF$per_rate_norm*traps_DF$week_diff2)
traps_DF$diff_ratenorm_cos4 <- cos(4*pi/traps_DF$per_rate_norm*traps_DF$week_diff2)



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
                   mean_rate = apply(xtabs(Orc_DF$rate_norm~Orc_DF$week_diff+Orc_DF$Season2),1,mean)) 
  
  pred_mean <- rbind(pred_mean, df)
}

Mean_obs <- left_join(Mean_obs, pred_mean)
traps_DF <- left_join(traps_DF, pred_mean)

ggplot(Mean_obs[Mean_obs$Season2 %in% 14,], aes(x=week_diff, y=rate_norm)) + geom_line() + facet_wrap(~Orchard) +
  geom_line(data=Mean_obs[Mean_obs$Season2 %in% 14,], aes(x=week_diff, y=mean_rate), col="red")


#----------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#        Regression Trees      #
#                              #
#------------------------------#

#----------------------------------------------------------------------------------------------------------------------
tree_pred_DF <- traps_DF[,c("year","cal_week","week_dat","week_diff","Season2","station","Orchard","rate","rate_norm")]

#models to try
#diff rate
#diff count

#diff rate norm
#diff count norm

tree_pred_DF$pred_ratenorm<- NA

for(j in 1:length(unique(traps_DF$station))){
  df <- traps_DF[traps_DF$station %in% unique(traps_DF$station)[j],]
  n_length <- length(unique(df$Season2))
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    
    #diff rate norm
    
    tree_mod <- rpart(rate_norm~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week) + week_diff +diff_ratenorm_cos2+ 
                        diff_ratenorm_sin2 + lag_rain+
                        diff_ratenorm_cos4+ diff_ratenorm_sin4 + lag_temp + mean_rate,  
                      data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2", "mean_rate",
                                                     "cal_week","Organic","week_diff","diff_ratenorm_sin2", "diff_ratenorm_cos2",
                                                     "diff_ratenorm_sin4","diff_ratenorm_cos4","lag_temp","lag_rain")])
    
    
    tree_pred_DF[tree_pred_DF$station %in% unique(traps_DF$station)[j] & tree_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_ratenorm")] <- tree_pred  
    
    
    
  }
  
  
}


plot(tree_mod)
text(tree_mod)

#rate; rate, log rate, diff rate, diff_rate_norm
tree_rate_plots <- list()

Ssn <- 11:15

i
for(i in 1:5){
  
  tree_rate_plots[[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=rate_norm, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_ratenorm, group=Orchard),col="red")
  
}

tree_rate_plots[[2]]

#----------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#        Random Forest         #
#                              #
#------------------------------#

#----------------------------------------------------------------------------------------------------------------------
RF_pred_DF <- traps_DF[,c("year","cal_week","week_dat","week_diff","Season2","station","Orchard","rate","rate_norm")]

#models to try
#diff rate
#diff count

#diff rate norm
#diff count norm

RF_pred_DF$pred_ratenorm<- NA

for(j in 1:length(unique(traps_DF$station))){
  df <- traps_DF[traps_DF$station %in% unique(traps_DF$station)[j],]
  n_length <- length(unique(df$Season2))
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    
    #diff rate norm
    
    tree_mod <- randomForest(rate_norm~ CGDD + total_rainfall + mean_temp + Season2 + cal_week + week_diff +diff_ratenorm_cos2+ 
                        diff_ratenorm_sin2 + lag_rain+
                        diff_ratenorm_cos4+ diff_ratenorm_sin4 + lag_temp + mean_rate,  
                      data=train_df, ntree=5000)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2", "mean_rate",
                                                     "cal_week","Organic","week_diff","diff_ratenorm_sin2", "diff_ratenorm_cos2",
                                                     "diff_ratenorm_sin4","diff_ratenorm_cos4","lag_temp","lag_rain")])
    
    
    RF_pred_DF[tree_pred_DF$station %in% unique(traps_DF$station)[j] & tree_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_ratenorm")] <- tree_pred  
    
    
    
  }
  
  
}


plot(tree_mod)
text(tree_mod)

#rate; rate, log rate, diff rate, diff_rate_norm
tree_rate_plots <- list()

Ssn <- 11:15

i
for(i in 1:5){
  
  tree_rate_plots[[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=rate_norm, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_ratenorm, group=Orchard),col="red")
  
}

tree_rate_plots[[2]]

#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#             SVM              #
#                              #
#------------------------------#


#------------------------------------------------------------------------------------------------------------------------------------------



#svm data set
svm_pred_DF <- traps_DF[,c("year","cal_week","week_dat","week_diff","Season2","station","Orchard","rate","rate_norm")]

svm_pred_DF$pred_ratenorm <- NA

for(j in 1:length(unique(traps_DF$station))){
  df <- traps_DF[traps_DF$station %in% unique(traps_DF$station)[j],]
  n_length <- length(unique(df$Season2))
  
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    
    #diff rate
    
    tree_mod <- svm(rate_norm~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week) + week_diff +diff_ratenorm_cos2+ 
                      diff_ratenorm_sin2 + lag_rain+
                      diff_ratenorm_cos4+ diff_ratenorm_sin4 + lag_temp + mean_rate,  
                    data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2", "mean_rate",
                                                     "cal_week","Organic","week_diff","diff_ratenorm_sin2", "diff_ratenorm_cos2",
                                                     "diff_ratenorm_sin4","diff_ratenorm_cos4","lag_temp","lag_rain")])
    
    
    svm_pred_DF[svm_pred_DF$station %in% unique(traps_DF$station)[j] & svm_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_ratenorm")] <- tree_pred  
    

  }
  
  
}




#rate; rate, log rate, diff rate, diff_rate_norm
svm_rate_plots <- list()

Ssn <- 11:15

i
for(i in 1:5){
  
  svm_rate_plots[[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=rate_norm, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_ratenorm, group=Orchard),col="red")
}



#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#             GBM              #
#                              #
#------------------------------#


#------------------------------------------------------------------------------------------------------------------------------------------
gbm_pred_DF <- traps_DF[,c("year","cal_week","week_dat","week_diff","Season2","station","Orchard","rate","rate_norm")]


gbm_pred_DF$pred_ratenorm <- NA

for(i in 1:length(traps_DF$station)){
  
  train_df <- df[-i,]
  test_df <- df[i,]
  
  gbm_pred_DF[i,c("pred_diffrate")] <- tree_pred  
  
  #diff rate norm
  
  tree_mod <- gbm(rate_norm~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week) + week_diff +diff_ratenorm_cos2+ 
                    diff_ratenorm_sin2 + lag_rain+
                    diff_ratenorm_cos4+ diff_ratenorm_sin4 + lag_temp + mean_rate,  
                  data=train_df,n.tree=5000)
  tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2", "mean_rate",
                                                   "cal_week","Organic","week_diff","diff_ratenorm_sin2", "diff_ratenorm_cos2",
                                                   "diff_ratenorm_sin4","diff_ratenorm_cos4","lag_temp","lag_rain")],n.tree=5000)
  
  
  gbm_pred_DF[i,c("pred_ratenorm")] <- tree_pred  
  
  

  
}





gbm_rate_plots <- list()

for(i in 1:5){
  
  gbm_rate_plots[[i]] <- ggplot(gbm_pred_DF[gbm_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=rate_norm, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=gbm_pred_DF[gbm_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_ratenorm, group=Orchard),col="red")
  
}

gbm_rate_plots[[1]][[6]]


#-----------------------------------------------------------------------------------------------------------------