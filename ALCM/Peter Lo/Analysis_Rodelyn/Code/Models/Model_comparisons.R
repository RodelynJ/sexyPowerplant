#libraries
library(ggplot2)
library(dplyr)
library(zoo)
library(e1071)
library(randomForest)
library(rpart)

#Read in Data
traps_DF <- read.csv("Data/Trap Data/ALCMdata_district.csv") 
traps_DF <- traps_DF %>% group_by(District,Season) %>% mutate(sum = cumsum(ALCM_count)) %>% as.data.frame()
traps_DF$week_dat <- as.Date(traps_DF$week_dat, format='%Y-%m-%d')
traps_DF$cal_week <- strftime(traps_DF$week_dat, format = "%V")
traps_DF$rate <- traps_DF$ALCM_count/traps_DF$No_traps
traps_DF <- traps_DF %>% group_by(District, Season) %>% mutate(sumCGDD = cumsum(weekly_CGDD)) %>% as.data.frame()

traps_DF <- traps_DF[traps_DF$cal_week %in% c(44:53,0:10),]


#--------------------#
#--      Trees     --#
#--------------------#

#----------------------------------------------------------------------------------------------------
#CV
traps_DF$tree_pred <- NA
traps_DF$tree_pred_k <- NA

levels(traps_DF$station)
#LVOOCV
i=1


for(k in 1:length(levels(traps_DF$station))){
stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
n.obs <- dim(stat)[1]

tree_pred <- c()
for(i in 1:n.obs){
  HAV_sub <- stat[-i,]
  HAV_pred <- stat[i,]
  tree_mod <- rpart(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
  tree_pred[i] <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])}

traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],"tree_pred"] <- tree_pred }

#K-fold CV
traps_DF$Season <- as.character(traps_DF$Season) 

for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.seasons <- length(unique(stat$Season))
for(i in 1:n.seasons){
  Season <- unique(stat$Season)[i]
  HAV_sub <- stat[!stat$Season%in% unique(stat$Season)[i],]
  HAV_pred <- stat[stat$Season%in% unique(stat$Season)[i],]
  tree_mod <- rpart(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
  tree_pred_k <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])
  traps_DF[traps_DF$station %in% levels(traps_DF$station)[k] & traps_DF$Season %in% Season,"tree_pred_k"] <- tree_pred_k
  
  }
  
  
}  
plot(traps_DF$ALCM_count,type="l",lty=2)
lines(traps_DF$tree_pred,col="red")

plot(traps_DF$ALCM_count,type="l",lty=2)
lines(traps_DF$tree_pred_k,col="red")


#-------------------------------------------------------------------------------------------------

#--------------------#
#--      RFore     --#
#--------------------#

#----------------------------------------------------------------------------------------------------
#CV
traps_DF$for_pred <- NA
traps_DF$for_pred_k <- NA

levels(traps_DF$station)
#LVOOCV
for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.obs <- dim(stat)[1]
  
  tree_pred <- c()
  for(i in 1:n.obs){
    HAV_sub <- stat[-i,]
    HAV_pred <- stat[i,]
    tree_mod <- randomForest(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub,ntree=5000)
    tree_pred[i] <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])}
  
  traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],"for_pred"] <- tree_pred }

#K-fold CV
traps_DF$Season <- as.character(traps_DF$Season) 

for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.seasons <- length(unique(stat$Season))
  for(i in 1:n.seasons){
    Season <- unique(stat$Season)[i]
    HAV_sub <- stat[!stat$Season%in% unique(stat$Season)[i],]
    HAV_pred <- stat[stat$Season%in% unique(stat$Season)[i],]
    tree_mod <- randomForest(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub,ntree=5000)
    tree_pred_k <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])
    traps_DF[traps_DF$station %in% levels(traps_DF$station)[k] & traps_DF$Season %in% Season,"fore_pred_k"] <- tree_pred_k
    
  }
  
  
}  
plot(traps_DF$ALCM_count,type="l",lty=2)
lines(traps_DF$for_pred,col="red")

plot(traps_DF$ALCM_count,type="l",lty=2)
lines(traps_DF$for_pred_k,col="red")


