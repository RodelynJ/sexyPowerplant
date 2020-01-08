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

traps_DF$Season2 <- as.numeric(traps_DF$Season)
traps_DF$Season <- as.character(traps_DF$Season) 

#--------------------#
#--      Trees     --#
#--------------------#

#----------------------------------------------------------------------------------------------------
#CV
traps_DF$tree_pred <- NA
traps_DF$tree_pred_rate <- NA

traps_DF$tree_pred_k <- NA
traps_DF$tree_pred_k_rate <- NA

levels(traps_DF$station)
#LVOOCV
i=1


#using total counts
for(k in 1:length(levels(traps_DF$station))){
stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
n.obs <- dim(stat)[1]

tree_pred <- c()
tree_pred_rate <- c()
for(i in 1:n.obs){
  HAV_sub <- stat[-i,]
  HAV_pred <- stat[i,]
  tree_mod <- rpart(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp + Season2 +factor(station), data=HAV_sub)
  tree_pred[i] <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp","Season2","station")])
  
  tree_mod_r <- rpart(rate~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp + Season2+factor(station), data=HAV_sub)
  tree_pred_rate[i] <- predict(tree_mod_r,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp","Season2","station")])
  }

  traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],"tree_pred"] <- tree_pred 
  
  
  traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],"tree_pred_rate"] <- tree_pred_rate}

#K-fold CV


for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.seasons <- length(unique(stat$Season))
for(i in 1:n.seasons){
  Season <- unique(stat$Season)[i]
  HAV_sub <- stat[!stat$Season%in% unique(stat$Season)[i],]
  HAV_pred <- stat[stat$Season%in% unique(stat$Season)[i],]
  
  tree_mod <- rpart(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp +Season2 + factor(station), data=HAV_sub)
  tree_pred_k <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp", "Season2","station")])
  traps_DF[traps_DF$station %in% levels(traps_DF$station)[k] & traps_DF$Season %in% Season,"tree_pred_k"] <- tree_pred_k
  
  }
  
  
}  

i=15
plot(traps_DF[traps_DF$Season %in% unique(traps_DF$Season)[i],"ALCM_count"],type="l",lty=2)
lines(traps_DF[traps_DF$Season %in% unique(traps_DF$Season)[i],"tree_pred"],col="red")

plot(traps_DF$rate,type="l",lty=2)
lines(traps_DF$tree_pred_rate,col="red")


plot(traps_DF$ALCM_count,type="l",lty=2)
lines(traps_DF$tree_pred_k,col="red")


traps_DF$tree_pred_rate <- NA
traps_DF$tree_pred_k_rate <- NA

levels(traps_DF$station)
#LVOOCV
i=1

#using total counts
for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.obs <- dim(stat)[1]
  
  tree_pred <- c()
  for(i in 1:n.obs){
    HAV_sub <- stat[-i,]
    HAV_pred <- stat[i,]
    tree_mod <- rpart(rate~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
    tree_pred[i] <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])}
  
  traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],"tree_pred_rate"] <- tree_pred }

#K-fold CV

for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.seasons <- length(unique(stat$Season))
  for(i in 1:n.seasons){
    Season <- unique(stat$Season)[i]
    HAV_sub <- stat[!stat$Season%in% unique(stat$Season)[i],]
    HAV_pred <- stat[stat$Season%in% unique(stat$Season)[i],]
    tree_mod <- rpart(rate~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
    tree_pred_k <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])
    traps_DF[traps_DF$station %in% levels(traps_DF$station)[k] & traps_DF$Season %in% Season,"tree_pred_k_rate"] <- tree_pred_k
    
  }
  
  
}  

plot(traps_DF$rate,type="l",lty=2)
lines(traps_DF$tree_pred_rate,col="red")

plot(traps_DF$rate,type="l",lty=2)
lines(traps_DF$tree_pred_k_rate,col="red")


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
    traps_DF[traps_DF$station %in% levels(traps_DF$station)[k] & traps_DF$Season %in% Season,"for_pred_k"] <- tree_pred_k
    
  }
 
  
}

plot(traps_DF$ALCM_count,type="l",lty=2)
lines(traps_DF$for_pred,col="red")

plot(traps_DF$ALCM_count,type="l",lty=2)
lines(traps_DF$fore_pred_k,col="red")

#using rate
traps_DF$for_pred_rate <- NA
traps_DF$for_pred_k_rate <- NA

#LVOOCV
for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.obs <- dim(stat)[1]
  
  tree_pred <- c()
  for(i in 1:n.obs){
    HAV_sub <- stat[-i,]
    HAV_pred <- stat[i,]
    tree_mod <- randomForest(rate~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub,ntree=5000)
    tree_pred[i] <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])}
  
  traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],"for_pred_rate"] <- tree_pred }

#K-fold CV

for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.seasons <- length(unique(stat$Season))
  for(i in 1:n.seasons){
    Season <- unique(stat$Season)[i]
    HAV_sub <- stat[!stat$Season%in% unique(stat$Season)[i],]
    HAV_pred <- stat[stat$Season%in% unique(stat$Season)[i],]
    tree_mod <- randomForest(rate~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub,ntree=5000)
    tree_pred_k <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])
    traps_DF[traps_DF$station %in% levels(traps_DF$station)[k] & traps_DF$Season %in% Season,"for_pred_k_rate"] <- tree_pred_k
    
  }
  
  
}  

plot(traps_DF$rate,type="l",lty=2)
lines(traps_DF$for_pred_rate,col="red")

plot(traps_DF$rate,type="l",lty=2)
lines(traps_DF$for_pred_k_rate,col="red")



#-------------------------------------------------------------------------------------------------

#--------------------#
#--      SVM       --#
#--------------------#

#----------------------------------------------------------------------------------------------------
#CV
traps_DF$svm_pred <- NA
traps_DF$svm_pred_k <- NA

levels(traps_DF$station)
#LVOOCV
for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.obs <- dim(stat)[1]
  
  svm_pred <- c()
  for(i in 1:n.obs){
    HAV_sub <- stat[-i,]
    HAV_pred <- stat[i,]
    svm_mod <- svm(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
    svm_pred[i] <- predict(svm_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])}
  
  traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],"svm_pred"] <- svm_pred }

#K-fold CV

traps_DF$Season <- as.character(traps_DF$Season) 

for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.seasons <- length(unique(stat$Season))
  
  for(i in 1:n.seasons){
    Season <- unique(stat$Season)[i]
    HAV_sub <- stat[!stat$Season%in% unique(stat$Season)[i],]
    HAV_pred <- stat[stat$Season%in% unique(stat$Season)[i],]
    svm_mod <- svm(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
    svm_pred_k <- predict(svm_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])
    traps_DF[traps_DF$station %in% levels(traps_DF$station)[k] & traps_DF$Season %in% Season,"svm_pred_k"] <- svm_pred_k
    
  }
  
  
}  

plot(traps_DF$ALCM_count,type="p",lty=2)
points(traps_DF$svm_pred,col="red")

plot(traps_DF$ALCM_count,type="l",lty=2)
lines(traps_DF$svm_pred_k,col="red")

#rate

traps_DF$svm_pred_rate <- NA
traps_DF$svm_pred_k_rate <- NA
#LVOOCV
for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.obs <- dim(stat)[1]
  
  svm_pred <- c()
  for(i in 1:n.obs){
    HAV_sub <- stat[-i,]
    HAV_pred <- stat[i,]
    svm_mod <- svm(rate~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
    svm_pred[i] <- predict(svm_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])}
  
  traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],"svm_pred_rate"] <- svm_pred }

#K-fold CV

traps_DF$Season <- as.character(traps_DF$Season) 

for(k in 1:length(levels(traps_DF$station))){
  stat <- traps_DF[traps_DF$station %in% levels(traps_DF$station)[k],]
  n.seasons <- length(unique(stat$Season))
  
  for(i in 1:n.seasons){
    Season <- unique(stat$Season)[i]
    HAV_sub <- stat[!stat$Season%in% unique(stat$Season)[i],]
    HAV_pred <- stat[stat$Season%in% unique(stat$Season)[i],]
    svm_mod <- svm(rate~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
    svm_pred_k <- predict(svm_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])
    traps_DF[traps_DF$station %in% levels(traps_DF$station)[k] & traps_DF$Season %in% Season,"svm_pred_k_rate"] <- svm_pred_k
    
  }
  
  
}  

plot(traps_DF$rate,type="l",lty=2)
lines(traps_DF$svm_pred_rate,col="red")

plot(traps_DF$rate,type="l",lty=2)
lines(traps_DF$svm_pred_k_rate,col="red")

#--------------------#
#--    Results     --#
#--------------------#

#----------------------------------------------------------------------------------------------------

colnames(traps_DF)
levels(traps_DF$station)
#Plots

# single trees
ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[1],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[2],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred, group=Season,col="red")) + theme_bw()  

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[3],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred, group=Season,col="red")) + theme_bw()  

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[4],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred, group=Season,col="red")) + theme_bw()  


ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[5],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred, group=Season,col="red")) + theme_bw()  

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[6],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred, group=Season,col="red")) + theme_bw()  


ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[1],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred_rate, group=Season,col="red")) + theme_bw()


ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[1],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[2],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[3],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[4],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[5],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[6],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=tree_pred_rate, group=Season,col="red")) + theme_bw()


#forests
ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[1],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=for_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[2],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=for_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[3],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=for_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[4],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=for_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[5],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=for_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[6],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=for_pred, group=Season,col="red")) + theme_bw()


ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[1],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=for_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[2],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=for_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[3],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=for_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[4],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=for_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[5],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=for_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[6],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=for_pred_rate, group=Season,col="red")) + theme_bw()


#svm
ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[1],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=svm_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[2],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=svm_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[3],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=svm_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[4],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=svm_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[5],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=svm_pred, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[6],], aes(x=monitoring_week, y=ALCM_count, group=Season)) + geom_point() + 
  facet_wrap(~Season) + geom_point(aes(x=monitoring_week, y=svm_pred, group=Season,col="red")) + theme_bw()


ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[1],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=svm_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[2],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=svm_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[3],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=svm_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[4],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=svm_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[5],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=svm_pred_rate, group=Season,col="red")) + theme_bw()

ggplot(traps_DF[traps_DF$station %in% levels(traps_DF$station)[6],], aes(x=monitoring_week, y=rate, group=Season)) + geom_line() + 
  facet_wrap(~Season) + geom_line(aes(x=monitoring_week, y=svm_pred_rate, group=Season,col="red")) + theme_bw()


