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

HAV <- traps_DF[traps_DF$station %in% "HAV",]
HAV <- HAV[HAV$cal_week %in% c(44:53,0:10),]



# <- traps_DF[traps_DF$station %in% "",]
# <- trp_0405[trp_0405$cal_week %in% c(44:53,0:10),]


ggplot(HAV, aes(x=sumCGDD, y=ALCM_count,group=interaction(District))) + geom_line() + scale_y_log10() + facet_wrap(~Season)


#--------------------#
#--      Trees     --#
#--------------------#

#----------------------------------------------------------------------------------------------------
T1 <- rpart(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV)
plot(T1)
text(T1)
plot(HAV$ALCM_count,type="l",lty=2)
points((predict(T1)),type="l",col="red")

#CV
HAV$tree_pred <- NA
HAV$tree_pred_k <- NA

#LVOOCV
for(i in 1:128){
  HAV_sub <- HAV[-i,]
  HAV_pred <- HAV[i,]
  tree_mod <- rpart(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
  HAV$tree_pred[i] <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])}

#K-fold CV
for(i in 1:15){
  HAV_sub <- HAV[!HAV$Season%in% levels(HAV$Season)[i],]
  HAV_pred <- HAV[HAV$Season%in% levels(HAV$Season)[i],]
  tree_mod <- rpart(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
  HAV[HAV$Season%in% levels(HAV$Season)[i],"tree_pred_k"] <- predict(tree_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])
  
}

plot(HAV$ALCM_count,type="l",lty=2)
lines(HAV$tree_pred,col="red")

plot(HAV$ALCM_count,type="l",lty=2)
lines(HAV$tree_pred_k,col="red")


#----------------------------------------------------------------------------------------------------

#--------------------#
#--     Rndm For   --#
#--------------------#

#----------------------------------------------------------------------------------------------------
F1 <- randomForest(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV, ntree=5000)

plot(HAV$ALCM_count,type="l",lty=2)
points(predict(F1),type="l",col="red")

HAV$for_pred <- NA
HAV$for_pred_k <- NA

#LVOOCV
for(i in 1:128){
  HAV_sub <- HAV[-i,]
  HAV_pred <- HAV[i,]
  for_mod <- randomForest(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub,ntree=5000)
  HAV$for_pred[i] <- predict(for_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])}

#k fold CV
for(i in 1:15){
  HAV_sub <- HAV[!HAV$Season%in% levels(HAV$Season)[i],]
  HAV_pred <- HAV[HAV$Season%in% levels(HAV$Season)[i],]
  for_mod <- randomForest(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub,ntree=5000)
  HAV[HAV$Season%in% levels(HAV$Season)[i],"for_pred_k"] <- predict(for_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])
  
}

plot(HAV$ALCM_count,type="l",lty=2)
lines(HAV$for_pred,col="red")

plot(HAV$ALCM_count,type="l",lty=2)
lines(HAV$for_pred_k,col="red")


#----------------------------------------------------------------------------------------------------


#--------------------#
#--      SVM       --#
#--------------------#

#----------------------------------------------------------------------------------------------------
SVM1 <- svm(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV)
plot(HAV$ALCM_count,type="l",lty=2)
points(predict(SVM1),type="l",col="red")


HAV$svm_pred <- NA
HAV$svm_pred_k <- NA

#LVOOCV
for(i in 1:128){
HAV_sub <- HAV[-i,]
HAV_pred <- HAV[i,]
SVM_mod <- svm(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
HAV$svm_pred[i] <- predict(SVM_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])}

#kfold CV
for(i in 1:15){
  HAV_sub <- HAV[!HAV$Season%in% levels(HAV$Season)[i],]
  HAV_pred <- HAV[HAV$Season%in% levels(HAV$Season)[i],]
  SVM_mod <- svm(ALCM_count~ weekly_CGDD + hrs_wet+ Total_Rainfall + Mean_meantemp, data=HAV_sub)
  HAV[HAV$Season%in% levels(HAV$Season)[i],"svm_pred_k"] <- predict(SVM_mod,newdata=HAV_pred[,c("weekly_CGDD","hrs_wet","Total_Rainfall","Mean_meantemp")])

}

plot(log(HAV$ALCM_count),type="l",lty=2)
lines(log(HAV$svm_pred),col="red")

plot(log(HAV$ALCM_count),type="l",lty=2)
lines(log(HAV$svm_pred_k),col="red")


#-----------------------------------------------------------------------------------------------------


#--------------------#
#--   Results      --#
#--------------------#

#----------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(HAV$ALCM_count,type="l",lty=2,main="SVM regression")
lines(HAV$svm_pred,col="red")

plot(HAV$ALCM_count,type="l",lty=2,main="Random Forest")
lines(HAV$for_pred,col="red")

plot(HAV$ALCM_count,type="l",lty=2,main="Regression Tree")
lines(HAV$tree_pred,col="red")

plot(HAV$ALCM_count,type="l",lty=2,main="Ensemble Model")
lines(apply(HAV[,c("svm_pred","for_pred","tree_pred")],1,mean),col="red")

plot(log(HAV$ALCM_count),type="l",lty=2)
lines(log(HAV$svm_pred_k),col="red")
