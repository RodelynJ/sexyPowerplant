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
         diff_rate_norm = diff_rate/sd(diff_rate,na.rm=T)) %>% as.data.frame()

traps_DF <- traps_DF[-(which(is.na(traps_DF$diff_count))),]

#periodicity

traps_DF$week_diff2 <- as.numeric(round(difftime(traps_DF$week_dat,unique(traps_DF$start_date)[1],units="weeks")))

traps_DF <- traps_DF[with(traps_DF, order(Orchard)),]

periodicity <- data.frame(Orchard=unique(traps_DF$Orchard),per=NA )

for(i in 1:18){
ssp <- spectrum(traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],"diff_rate_norm"])
traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],"per_rate_norm"] <- 1/ssp$freq[ssp$spec==max(ssp$spec)]

ssp <- spectrum(traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],"diff_rate"])
traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],"per_rate"] <- 1/ssp$freq[ssp$spec==max(ssp$spec)]

ssp <- spectrum(traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],"diff_count_norm"])
traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],"per_count_norm"] <- 1/ssp$freq[ssp$spec==max(ssp$spec)]

ssp <- spectrum(traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],"diff_count"])
traps_DF[traps_DF$Orchard %in% unique(traps_DF$Orchard)[i],"per_count"] <- 1/ssp$freq[ssp$spec==max(ssp$spec)]

}


traps_DF$diff_rate_sin2 <- sin(2*pi/traps_DF$per_rate*traps_DF$week_diff2)
traps_DF$diff_rate_cos2 <- cos(2*pi/traps_DF$per_rate*traps_DF$week_diff2)

traps_DF$diff_rate_sin4 <- sin(4*pi/traps_DF$per_rate*traps_DF$week_diff2)
traps_DF$diff_rate_cos4 <- cos(4*pi/traps_DF$per_rate*traps_DF$week_diff2)

traps_DF$diff_ratenorm_sin2 <- sin(2*pi/traps_DF$per_rate_norm*traps_DF$week_diff2)
traps_DF$diff_ratenorm_cos2 <- cos(2*pi/traps_DF$per_rate_norm*traps_DF$week_diff2)

traps_DF$diff_ratenorm_sin4 <- sin(4*pi/traps_DF$per_rate_norm*traps_DF$week_diff2)
traps_DF$diff_ratenorm_cos4 <- cos(4*pi/traps_DF$per_rate_norm*traps_DF$week_diff2)



traps_DF$diff_count_sin2 <- sin(2*pi/traps_DF$per_count*traps_DF$week_diff2)
traps_DF$diff_count_cos2 <- cos(2*pi/traps_DF$per_count*traps_DF$week_diff2)

traps_DF$diff_count_sin4 <- sin(4*pi/traps_DF$per_count*traps_DF$week_diff2)
traps_DF$diff_count_cos4 <- cos(4*pi/traps_DF$per_count*traps_DF$week_diff2)

traps_DF$diff_countnorm_sin2 <- sin(2*pi/traps_DF$per_count_norm*traps_DF$week_diff2)
traps_DF$diff_countnorm_cos2 <- cos(2*pi/traps_DF$per_count_norm*traps_DF$week_diff2)

traps_DF$diff_countnorm_sin4 <- sin(4*pi/traps_DF$per_count_norm*traps_DF$week_diff2)
traps_DF$diff_countnorm_cos4 <- cos(4*pi/traps_DF$per_count_norm*traps_DF$week_diff2)

#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#        Regression Trees      #
#                              #
#------------------------------#

#----------------------------------------------------------------------------------------------------------------------
tree_pred_DF <- traps_DF[,c("year","cal_week","week_dat","week_diff","Season2","station","Orchard","rate",
                            "ALCM_count","diff_rate","diff_count","diff_rate_norm","diff_count_norm")]

#models to try
#diff rate
#diff count

#diff rate norm
#diff count norm

tree_pred_DF$pred_diffrate <- NA
tree_pred_DF$pred_diffcount <- NA


tree_pred_DF$pred_diffratenorm <- NA
tree_pred_DF$pred_diffcountnorm <- NA


for(j in 1:length(unique(traps_DF$station))){
  df <- traps_DF[traps_DF$station %in% unique(traps_DF$station)[j],]
  n_length <- length(unique(df$Season2))
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    
    #diff rate
    
    tree_mod <- rpart(diff_rate~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week)+week_diff +diff_rate_cos2+ diff_rate_sin2 +
                        diff_rate_cos4+ diff_rate_sin4 +lag_temp + lag_rain,  
                      data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2",
                                                     "cal_week","Organic","week_diff","diff_rate_sin2", "diff_rate_cos2",
                                                     "diff_rate_sin4","diff_rate_cos4","lag_temp","lag_rain")])
    
    tree_pred_DF[tree_pred_DF$station %in% unique(traps_DF$station)[j] & tree_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_diffrate")] <- tree_pred  
    
    #diff rate norm
    
    tree_mod <- rpart(diff_rate_norm~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week) + week_diff +diff_ratenorm_cos2+ 
                        diff_ratenorm_sin2 + lag_rain+
                        diff_ratenorm_cos4+ diff_ratenorm_sin4 + lag_temp,  
                      data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2", 
                                                     "cal_week","Organic","week_diff","diff_ratenorm_sin2", "diff_ratenorm_cos2",
                                                     "diff_ratenorm_sin4","diff_ratenorm_cos4","lag_temp","lag_rain")])
    
    
    tree_pred_DF[tree_pred_DF$station %in% unique(traps_DF$station)[j] & tree_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_diffratenorm")] <- tree_pred  
    
    
      #diff count
    tree_mod <- rpart(diff_count~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week)+week_diff + lag_rain+ 
                        diff_count_cos2+ 
                        diff_count_sin2 +
                        diff_count_cos4+ 
                        diff_count_sin4 + lag_temp,   
                      data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2","lag_rain",
                                                     "cal_week","Organic","week_diff", "diff_count_cos2", "diff_count_cos4",
                                                     "diff_count_sin2","diff_count_sin4","lag_temp")])
    
    
    tree_pred_DF[tree_pred_DF$station %in% unique(traps_DF$station)[j] & tree_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_diffcount")] <- tree_pred  
    
    
    
    #diff count norm
    tree_mod <- rpart(diff_count_norm~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week) + week_diff +
                        diff_countnorm_cos2+ 
                        diff_countnorm_sin2 +
                        diff_countnorm_cos4+ 
                        diff_countnorm_sin4 +lag_temp + lag_rain,
                      data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2","lag_rain",
                                                     "cal_week","Organic","week_diff", "diff_countnorm_cos2", "diff_countnorm_cos4",
                                                     "diff_countnorm_sin2","diff_countnorm_sin4","lag_temp")])
    
    
    tree_pred_DF[tree_pred_DF$station %in% unique(traps_DF$station)[j] & tree_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_diffcountnorm")] <- tree_pred  
    
    
    
    }
  

}



#rate; rate, log rate, diff rate, diff_rate_norm
tree_rate_plots <- list(diff_rate = vector(mode = "list", length = 7),
                        diff_ratenorm = vector(mode = "list", length = 7))

Ssn <- 9:15

i
for(i in 1:7){

tree_rate_plots[["diff_rate"]][[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_rate, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
                                  geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffrate, group=Orchard),col="red")

tree_rate_plots[["diff_ratenorm"]][[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_rate_norm, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
                                           geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffratenorm, group=Orchard),col="red")
}



#count; count, log count, diff count, diff_count_norm
tree_count_plots <- list(diff_count = vector(mode = "list", length = 7),
                         diff_countnorm = vector(mode = "list", length = 7))

Ssn <- 9:15

for(i in 1:7){
  tree_count_plots[["diff_count"]][[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_count, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffcount, group=Orchard),col="red")
  
  tree_count_plots[["diff_countnorm"]][[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_count_norm, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffcountnorm, group=Orchard),col="red")
}


plot(tree_mod)
text(tree_mod)
#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#             SVM              #
#                              #
#------------------------------#


#------------------------------------------------------------------------------------------------------------------------------------------



#svm data set
svm_pred_DF <- traps_DF[,c("year","cal_week","week_dat","week_diff","Season2","station","Orchard","rate",
                            "ALCM_count","diff_rate","diff_count","diff_rate_norm","diff_count_norm")]


svm_pred_DF$pred_diffrate <- NA
svm_pred_DF$pred_diffcount <- NA


svm_pred_DF$prsvm_pred_DFed_diffratenorm <- NA
svm_pred_DF$pred_diffcountnorm <- NA


traps_DF$Orchard <- as.factor(traps_DF$Orchard)
traps_DF$cal_week <- as.numeric(traps_DF$cal_week)



for(j in 1:length(unique(traps_DF$station))){
  df <- traps_DF[traps_DF$station %in% unique(traps_DF$station)[j],]
  n_length <- length(unique(df$Season2))
  
  
  for(i in 1:n_length){
    
    train_df <- df[!df$Season2 %in% unique(df$Season2)[i],]
    test_df <- df[df$Season2 %in% unique(df$Season2)[i],]
    
    #diff rate
    
    tree_mod <- svm(diff_rate~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week)+week_diff +diff_rate_cos2+ diff_rate_sin2 +
                        diff_rate_cos4+ diff_rate_sin4 + lag_temp + lag_rain,  
                      data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2",
                                                     "cal_week","week_diff","diff_rate_sin2", "diff_rate_cos2",
                                                     "diff_rate_sin4","diff_rate_cos4","lag_temp","lag_rain")])
    
    svm_pred_DF[svm_pred_DF$station %in% unique(traps_DF$station)[j] & svm_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_diffrate")] <- tree_pred  
    
    #diff rate norm
    
    tree_mod <- svm(diff_rate_norm~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week) + week_diff +diff_ratenorm_cos2+ 
                        diff_ratenorm_sin2 + lag_rain+
                        diff_ratenorm_cos4+ diff_ratenorm_sin4 + lag_temp,  
                      data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2", 
                                                     "cal_week","Organic","week_diff","diff_ratenorm_sin2", "diff_ratenorm_cos2",
                                                     "diff_ratenorm_sin4","diff_ratenorm_cos4","lag_temp","lag_rain")])
    
    
    svm_pred_DF[svm_pred_DF$station %in% unique(traps_DF$station)[j] & svm_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_diffratenorm")] <- tree_pred  
    
    
    #diff count
    tree_mod <- svm(diff_count~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week)+week_diff + lag_rain+ 
                        diff_count_cos2+ 
                        diff_count_sin2 +
                        diff_count_cos4+ 
                        diff_count_sin4 + lag_temp,   
                      data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2","lag_rain",
                                                     "cal_week","Organic","week_diff", "diff_count_cos2", "diff_count_cos4",
                                                     "diff_count_sin2","diff_count_sin4","lag_temp")])
    
    
    svm_pred_DF[svm_pred_DF$station %in% unique(traps_DF$station)[j] & svm_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_diffcount")] <- tree_pred  
    
    
    
    #diff count norm
    tree_mod <- svm(diff_count_norm~ CGDD + total_rainfall + mean_temp + Season2 + as.numeric(cal_week) + week_diff +
                        diff_countnorm_cos2+ 
                        diff_countnorm_sin2 +
                        diff_countnorm_cos4+ 
                        diff_countnorm_sin4 + lag_temp + lag_rain,
                      data=train_df)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2", "lag_rain",
                                                     "cal_week","Organic","week_diff", "diff_countnorm_cos2", "diff_countnorm_cos4",
                                                     "diff_countnorm_sin2","diff_countnorm_sin4","lag_temp")])
    
    
    svm_pred_DF[svm_pred_DF$station %in% unique(traps_DF$station)[j] & svm_pred_DF$Season2 %in% unique(df$Season2)[i],c("pred_diffcountnorm")] <- tree_pred  
    
    
    
  }
  
  
}




#rate; rate, log rate, diff rate, diff_rate_norm
svm_rate_plots <- list(diff_rate = vector(mode = "list", length = 7),
                        diff_ratenorm = vector(mode = "list", length = 7))

Ssn <- 9:15

i
for(i in 1:7){
  
  svm_rate_plots[["diff_rate"]][[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_rate, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffrate, group=Orchard),col="red")
  
  svm_rate_plots[["diff_ratenorm"]][[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_rate_norm, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffratenorm, group=Orchard),col="red")
}

svm_rate_plots[[2]][[6]]

#count; count, log count, diff count, diff_count_norm
svm_count_plots <- list(diff_count = vector(mode = "list", length = 7),
                         diff_countnorm = vector(mode = "list", length = 7))

Ssn <- 9:15

for(i in 1:7){
  svm_count_plots[["diff_count"]][[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_count, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffcount, group=Orchard),col="red")
  
  svm_count_plots[["diff_countnorm"]][[i]] <- ggplot(tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_count_norm, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=tree_pred_DF[tree_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffcountnorm, group=Orchard),col="red")
}


svm_count_plots[["diff_countnorm"]][[7]]
svm_rate_plots[["diff_ratenorm"]][[7]]

#----------------------------------------------------------------------------------------------------------------------

#------------------------------#
#                              #
#             GBM              #
#                              #
#------------------------------#


#------------------------------------------------------------------------------------------------------------------------------------------
gbm_pred_DF <- traps_DF[,c("year","cal_week","week_dat","week_diff","Season2","station","Orchard","rate",
                           "ALCM_count","diff_rate","diff_count","diff_rate_norm","diff_count_norm")]


gbm_pred_DF$pred_diffrate <- NA
gbm_pred_DF$pred_diffcount <- NA


gbm_pred_DF$prgbm_pred_DFed_diffratenorm <- NA
gbm_pred_DF$pred_diffcountnorm <- NA


traps_DF$Orchard <- as.factor(traps_DF$Orchard)
traps_DF$cal_week <- as.numeric(traps_DF$cal_week)


j=1=1
for(i in 1:length(traps_DF$station)){

    train_df <- df[-i,]
    test_df <- df[i,]
    
    #diff rate
    
    tree_mod <- gbm(diff_rate~ CGDD + total_rainfall + mean_temp + Season2 + cal_week+week_diff +diff_rate_cos2+ diff_rate_sin2 +
                      diff_rate_cos4+ diff_rate_sin4 + lag_temp + lag_rain,  
                    data=train_df,n.tree=5000)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2",
                                                     "cal_week","Organic","week_diff","diff_rate_sin2", "diff_rate_cos2",
                                                     "diff_rate_sin4","diff_rate_cos4","lag_temp","lag_rain")], n.tree=5000)
    
    gbm_pred_DF[i,c("pred_diffrate")] <- tree_pred  
    
    #diff rate norm
    
    tree_mod <- gbm(diff_rate_norm~ CGDD + total_rainfall + mean_temp + Season2 + cal_week + week_diff +diff_ratenorm_cos2+ 
                      diff_ratenorm_sin2 + lag_rain+
                      diff_ratenorm_cos4+ diff_ratenorm_sin4 + lag_temp,  
                    data=train_df,n.tree=5000)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2", 
                                                     "cal_week","Organic","week_diff","diff_ratenorm_sin2", "diff_ratenorm_cos2",
                                                     "diff_ratenorm_sin4","diff_ratenorm_cos4","lag_temp","lag_rain")],n.tree=5000)
    
    
    gbm_pred_DF[i,c("pred_diffratenorm")] <- tree_pred  
    
    
    #diff count
    tree_mod <- gbm(diff_count~ CGDD + total_rainfall + mean_temp + Season2 + cal_week+week_diff + lag_rain+ 
                      diff_count_cos2+ 
                      diff_count_sin2 +
                      diff_count_cos4+ 
                      diff_count_sin4 + lag_temp,   
                    data=train_df,n.tree=5000)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2","lag_rain",
                                                     "cal_week","Organic","week_diff", "diff_count_cos2", "diff_count_cos4",
                                                     "diff_count_sin2","diff_count_sin4","lag_temp")],n.tree=5000)
    
    
    gbm_pred_DF[i,c("pred_diffcount")] <- tree_pred  
    
    
    
    #diff count norm
    tree_mod <- gbm(diff_count_norm~ CGDD + total_rainfall + mean_temp + Season2 + cal_week + week_diff +
                      diff_countnorm_cos2+ 
                      diff_countnorm_sin2 +
                      diff_countnorm_cos4+ 
                      diff_countnorm_sin4 + lag_temp + lag_rain,
                    data=train_df,n.tree=5000)
    tree_pred <- predict(tree_mod,newdata=test_df[,c("CGDD","total_rainfall","mean_temp","Season2", "lag_rain",
                                                     "cal_week","Organic","week_diff", "diff_countnorm_cos2", "diff_countnorm_cos4",
                                                     "diff_countnorm_sin2","diff_countnorm_sin4","lag_temp")],n.tree=5000)
    
    
    gbm_pred_DF[i,c("pred_diffcountnorm")] <- tree_pred  
    
    
    
  }
  
  





#rate; rate, log rate, diff rate, diff_rate_norm

tree_mod <- gbm(diff_count_norm~ CGDD + total_rainfall + mean_temp + Season2 + cal_week + week_diff +
                  diff_countnorm_cos2+ 
                  diff_countnorm_sin2 +
                  diff_countnorm_cos4+ 
                  diff_countnorm_sin4 + lag_temp + lag_rain,
                data=traps_DF,n.tree=5000)
predict(tree_mod,n.trees=5000)

plot(traps_DF$diff_count_norm[2250:2309],type="l")
lines(predict(tree_mod,n.trees=5000)[2250:2309],col="red")

str(traps_DF$cal_week)
Ssn <- 9:15

gbm_rate_plots <- list(diff_rate = vector(mode = "list", length = 7),
                       diff_ratenorm = vector(mode = "list", length = 7))

for(i in 1:7){
  
  gbm_rate_plots[["diff_rate"]][[i]] <- ggplot(gbm_pred_DF[gbm_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_rate, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=gbm_pred_DF[gbm_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffrate, group=Orchard),col="red")
  
  gbm_rate_plots[["diff_ratenorm"]][[i]] <- ggplot(gbm_pred_DF[gbm_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_rate_norm, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=gbm_pred_DF[gbm_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffratenorm, group=Orchard),col="red")
}

gbm_rate_plots[[1]][[6]]

#count; count, log count, diff count, diff_cosvm_pred_DFunt_norm
gbm_count_plots <- list(diff_count = vector(mode = "list", length = 7),
                        diff_countnorm = vector(mode = "list", length = 7))

Ssn <- 9:15

for(i in 1:7){
  gbm_count_plots[["diff_count"]][[i]] <- ggplot(gbm_pred_DF[gbm_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_count, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=gbm_pred_DF[gbm_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffcount, group=Orchard),col="red")
  
  gbm_count_plots[["diff_countnorm"]][[i]] <- ggplot(gbm_pred_DF[gbm_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=diff_count_norm, group=Orchard)) + geom_line() + facet_wrap(~Orchard)+
    geom_line(data=gbm_pred_DF[gbm_pred_DF$Season2 %in% Ssn[i],], aes(x=week_diff, y=pred_diffcountnorm, group=Orchard),col="red")
}

gbm_count_plots[["diff_count"]][[7]]

par(mfrow=c(2,1))
plot(gbm_pred_DF$diff_rate[2200:2300], type="l")
plot(gbm_pred_DF$pred_diffrate[2200:2300], type="l")
#-----------------------------------------------------------------------------------------------------------------