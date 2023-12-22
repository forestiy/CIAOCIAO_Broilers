rm(list=ls()) 

library(haven)
library(tibble)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(SciViews)
library(geepack)
library(data.table)
library(car)
library(ggpubr)
library(Hmisc)
library(broom)

#Load data, correct some values and merge them
data<- read_sas("DB_poultry_15052018 (1).sas7bdat",NULL)
AMU<-read.csv2("AMU_pur_CSV.csv") 

AMU$TIdddvet<-as.numeric(as.character(AMU$TIdddvet))
AMU$farmid<-as.factor(AMU$farmid)

a<-aggregate(TIdddvet ~ farmid , data=AMU, FUN=sum)

b<-aggregate(country ~ farmid , data=AMU, FUN=function(x) paste0(unique(x)))
b[,2]<-as.character(b[,2])
b[,2]<-substring(b[,2], 4, 5)
b[39,2]="DE"
b[57,2]="FR"
b[c(71,72,80),2]="ES"
b[c(142:161),2]="DK"
b[c(162:169),2]="BG"

c<-cbind(b,a[,2])
colnames(c)[3] <- "TIdddvet"
c[,4]<-ln(c$TIdddvet+((c$TIdddvet^2) + 1)^(1/2))
colnames(c)[4] <- "ln_TIdddvet"



c_df<-as.data.frame(transform(c, counter_farm = ave(country, rleid( country), FUN = seq)))
c_df$counter_farm<-as.numeric(as.character(c_df$counter_farm))
c_df$counter_country <- c_df%>% group_indices(country) 


FDF<-merge(c_df,data,by.x="farmid", by.y="farmid")
#Data ready
#____Next-step_____
#Replace NAs with "missing"
blank_col<-unique(ceiling(which(is.na(FDF[]))/NROW(FDF)))

colnames(FDF[,blank_col])

for (i in blank_col){
  FDF[,i]<-ifelse(is.na(FDF[,i])==TRUE, yes ="missing", no = FDF[,i])
}


#reorder columns
refcols <- c("counter_country", "counter_farm")

FDF <- FDF[, c(refcols, setdiff(names(FDF), refcols))]

FDF <-FDF[order(FDF[,1], FDF[,2]), ]

glimpse(FDF)

#Transform and correct variables
############
FDF$bqq002<-ln(FDF$bqq002)

#
FDF$bqq011<-ln(FDF$bqq011)

#1
FDF$bqq017<-as.character(FDF$bqq017)
FDF$bqq017<-ifelse(FDF$bqq017=="0","0",
                   ifelse(FDF$bqq017=="1","1",
                          ifelse(FDF$bqq017=="2","0",
                                 ifelse(FDF$bqq017=="3","0",
                                        "---ALERT---" ))))
FDF$bqq017<-as.factor(FDF$bqq017)

#2
FDF$bqq019<-as.character(FDF$bqq019)
FDF$bqq019<-ifelse(FDF$bqq019=="1","1",
                   ifelse(FDF$bqq019=="2","1",
                          ifelse(FDF$bqq019=="3","2",
                                 "---ALERT---" )))
FDF$bqq019<-as.numeric(FDF$bqq019)

#3
FDF$bqq022<-as.character(FDF$bqq022)
FDF$bqq022<-ifelse(FDF$bqq022=="0","0",
                   ifelse(FDF$bqq022=="1","1",
                          ifelse(FDF$bqq022=="2","0",
                                 "---ALERT---" )))
FDF$bqq022<-as.numeric(FDF$bqq022)

#4
FDF$bqq023<-as.character(FDF$bqq023)
FDF$bqq023<-ifelse(FDF$bqq023=="0","0",
                   ifelse(FDF$bqq023=="1","1",
                          ifelse(FDF$bqq023=="2","1",
                                 "---ALERT---" )))
FDF$bqq023<-as.numeric(FDF$bqq023)

#5
FDF$bqq024<-as.character(FDF$bqq024)
FDF$bqq024<-ifelse(FDF$bqq024=="1","1",
                   ifelse(FDF$bqq024=="2","2",
                          ifelse(FDF$bqq024=="3","2",
                                 "---ALERT---" )))
FDF$bqq024<-as.numeric(FDF$bqq024)

#6
FDF$bqq025<-as.character(FDF$bqq025)
FDF$bqq025<-ifelse(FDF$bqq025=="1","1",
                   ifelse(FDF$bqq025=="2","1",
                          ifelse(FDF$bqq025=="3","2",
                                 "---ALERT---" )))
FDF$bqq025<-as.factor(FDF$bqq025)



FDF$bqq031<-as.character(FDF$bqq031)
FDF$bqq031<-ifelse(FDF$bqq031=="1","1",
                   ifelse(FDF$bqq031=="2","1",
                          ifelse(FDF$bqq031=="3","2",
                                 "---ALERT---" )))
FDF$bqq031<-as.numeric(FDF$bqq031)

#7
FDF$bqq032<-as.character(FDF$bqq032)
FDF$bqq032<-ifelse(FDF$bqq032=="1","1",
                   ifelse(FDF$bqq032=="2","2",
                          ifelse(FDF$bqq032=="3","2",
                                 ifelse(FDF$bqq032=="0","2",
                                        "---ALERT---" ))))
FDF$bqq032<-as.factor(FDF$bqq032)
#8

FDF$bqq033<-as.character(FDF$bqq033)
FDF$bqq033<-ifelse(FDF$bqq033=="1","1",
                   ifelse(FDF$bqq033=="2","2",
                          ifelse(FDF$bqq033=="3","1",
                                 ifelse(FDF$bqq033=="0","missing",
                                        ifelse(FDF$bqq033=="missing","missing",
                                               "---ALERT---" )))))
FDF$bqq033<-as.factor(FDF$bqq033)


#6
library(stringr)
FDF$bqq034b<-as.character(FDF$bqq034b)
FDF$bqq034<-as.character(FDF$bqq034)


FDF$wood_shavings<-ifelse((str_detect(FDF$bqq034b,"wood")|str_detect(FDF$bqq034b,"saw")|str_detect(FDF$bqq034,"1")|str_detect(FDF$bqq034b,"1")),"1","0")
FDF$wood_shavings<-as.factor(FDF$wood_shavings)

FDF$straw<-ifelse((str_detect(FDF$bqq034b,"straw")|str_detect(FDF$bqq034b,"corn")|str_detect(FDF$bqq034,"2")|str_detect(FDF$bqq034b,"2")),"1","0")
FDF$straw<-as.factor(FDF$straw)

FDF$rice<-ifelse(str_detect(FDF$bqq034b,"rice"),"1","0")
FDF$rice<-as.factor(FDF$rice)

FDF$flax<-ifelse(str_detect(FDF$bqq034b,"flax"),"1","0")
FDF$flax<-as.factor(FDF$flax)

FDF$peat<-ifelse((str_detect(FDF$bqq034b,"peat")|str_detect(FDF$bqq034b,"turf")|str_detect(FDF$bqq034b,"Turf")),"1","0")
FDF$peat<-as.factor(FDF$peat)

FDF$pine<-ifelse((str_detect(FDF$bqq034b,"pine")|str_detect(FDF$bqq034b,"Pine")),"1","0")
FDF$pine<-as.factor(FDF$pine)

FDF$shpagnum<-ifelse((str_detect(FDF$bqq034b,"gnum")),"1","0")
FDF$shpagnum<-as.factor(FDF$shpagnum)

FDF$bqq034b<-as.factor(FDF$bqq034b)
FDF$bqq034<-as.factor(FDF$bqq034)

#9
FDF$bqq042<-as.character(FDF$bqq042)
FDF$bqq042<-ifelse(FDF$bqq042=="0","0",
                   ifelse(FDF$bqq042=="1","1",
                          ifelse(FDF$bqq042=="2","0",
                                 "---ALERT---" )))
FDF$bqq042<-as.factor(FDF$bqq042)

#9
FDF$bqq044<-as.character(FDF$bqq044)
FDF$bqq044<-ifelse(FDF$bqq044=="0","0",
                   ifelse(FDF$bqq044=="1","1",
                          ifelse(FDF$bqq044=="2","0",
                                 "---ALERT---" )))
FDF$bqq044<-as.factor(FDF$bqq044)

#10
FDF$bqq051<-as.character(FDF$bqq051)
FDF$bqq051<-ifelse(FDF$bqq051=="0","0",
                   ifelse(FDF$bqq051=="1","1",
                          ifelse(FDF$bqq051=="2","1",
                                 "---ALERT---" )))
FDF$bqq051<-as.factor(FDF$bqq051)


#11
FDF$bqq060<-as.character(FDF$bqq060)
FDF$bqq060<-ifelse(FDF$bqq060=="0","0",
                   ifelse(FDF$bqq060=="1","1",
                          ifelse(FDF$bqq060=="2","0",
                                 "---ALERT---" )))
FDF$bqq060<-as.numeric(FDF$bqq060)



#12
FDF$bqq061<-as.character(FDF$bqq061)
FDF$bqq061<-ifelse(FDF$bqq061=="0","0",
                   ifelse(FDF$bqq061=="1","1",
                          ifelse(FDF$bqq061=="2","0",
                                 "---ALERT---" )))
FDF$bqq061<-as.factor(FDF$bqq061)


#13
FDF$bqq067<-as.character(FDF$bqq067)
FDF$bqq067<-ifelse(FDF$bqq067=="1","1",
                   ifelse(FDF$bqq067=="2","2",
                          ifelse(FDF$bqq067=="3","2",
                                 "---ALERT---" )))
FDF$bqq067<-as.factor(FDF$bqq067)


#13
FDF$bqq072<-as.character(FDF$bqq072)
FDF$bqq072<-ifelse(FDF$bqq072=="0","0",
                   ifelse(FDF$bqq072=="1","1",
                          ifelse(FDF$bqq072=="2","1",
                                 "---ALERT---" )))
FDF$bqq072<-as.factor(FDF$bqq072)

#13
FDF$bqq073<-as.character(FDF$bqq073)
FDF$bqq073<-ifelse(FDF$bqq073=="0","0",
                   ifelse(FDF$bqq073=="1","1",
                          ifelse(FDF$bqq073=="2","1",
                                 "---ALERT---" )))
FDF$bqq073<-as.factor(FDF$bqq073)


#14
FDF$bqq078<-as.character(FDF$bqq078)
FDF$bqq078<-ifelse(FDF$bqq078=="1","1",
                   ifelse(FDF$bqq078=="2","1",
                          ifelse(FDF$bqq078=="3","1",
                                 ifelse(FDF$bqq078=="4","1",
                                        ifelse(FDF$bqq078=="5","1",
                                               ifelse(FDF$bqq078=="6","2",
                                                      ifelse(FDF$bqq078=="7","2", 
                                                             ifelse(FDF$bqq078=="8","2",
                                                                    ifelse(FDF$bqq078=="9","2",
                                                                           ifelse(FDF$bqq078=="10","2",
                                                                                  ifelse(FDF$bqq078=="11","2","---ALERT---"
                                                                                  )))))))))))
FDF$bqq078<-as.numeric(FDF$bqq078)


#15
FDF$bqq084<-as.character(FDF$bqq084)
FDF$bqq084<-ifelse(FDF$bqq084=="0","0",
                   ifelse(FDF$bqq084=="1","1",
                          ifelse(FDF$bqq084=="2","1",
                                 "---ALERT---" )))
FDF$bqq084<-as.numeric(FDF$bqq084)

#15
FDF$bqq086<-as.character(FDF$bqq086)
FDF$bqq086<-ifelse(FDF$bqq086=="1","1",
                   ifelse(FDF$bqq086=="2","1",
                          ifelse(FDF$bqq086=="3","2",
                                 "---ALERT---" )))
FDF$bqq086<-as.factor(FDF$bqq086)


#15
FDF$bqq096<-as.character(FDF$bqq096)
FDF$bqq096<-ifelse(FDF$bqq096=="0","0",
                   ifelse(FDF$bqq096=="1","1",
                          ifelse(FDF$bqq096=="2","0",
                                 "---ALERT---" )))
FDF$bqq096<-as.numeric(FDF$bqq096)

#15
FDF$bqq097<-as.character(FDF$bqq097)
FDF$bqq097<-ifelse(FDF$bqq097=="0","0",
                   ifelse(FDF$bqq097=="1","1",
                          ifelse(FDF$bqq097=="2","0",
                                 "---ALERT---" )))
FDF$bqq097<-as.factor(FDF$bqq097)

#15
FDF$bqq098<-as.character(FDF$bqq098)
FDF$bqq098<-ifelse(FDF$bqq098=="0","0",
                   ifelse(FDF$bqq098=="1","1",
                          ifelse(FDF$bqq098=="2","0",
                                 "---ALERT---" )))
FDF$bqq098<-as.factor(FDF$bqq098)


##########
##########End of trnasforming 
###################################################

FDF_low_variation_list=c()         # Function to identify variables with same value >85%
for (i in 1:length(FDF)){
  a<-FDF %>% 
    dplyr::group_by(FDF[,i]) %>%
    dplyr::summarise(count = n (),fr=n()*100/nrow(FDF))
  if (any(a[,3]>85)==TRUE){
    FDF_low_variation_list[[length(FDF_low_variation_list)+1]]=i
  }
}
#_________________________missings morethan 10%
FDF_for_missings<-FDF
FDF_for_missings[]<-lapply(FDF_for_missings[],as.character)

FDF_missings_list=c()         # Function to identify missings with >10%
for (i in 1:length(FDF_for_missings)){
  a<-FDF_for_missings %>% 
    dplyr::group_by(FDF_for_missings[,i]) %>%
    dplyr::summarise(count = n (),fr=n()*100/nrow(FDF_for_missings))
  if ("missing"%in%unlist(a[,1]) & a[ifelse(any(a[,1]=="missing"),which(a[,1]=="missing"),1),3][[1]]>10){
    FDF_missings_list[[length(FDF_missings_list)+1]]=i
  }
}


colnames(FDF[unlist(FDF_low_variation_list)])

colnames(FDF[unlist(FDF_missings_list)])

excl<-unique(c(unlist(FDF_low_variation_list),
               unlist(FDF_missings_list),
               as.numeric(which(sapply(FDF, typeof)=='character'))[-c(54,57)]
               ))

FDF<-FDF[,-c(excl)]

glimpse(FDF)

#_________________________________________________________________________________________________________________
######RFE
#Part of Recursive feature elimination

library(mlbench)
library(caret)


FDForest<-FDF[,-c(which(colnames(FDF)=="bqq034b"),
                   which(colnames(FDF)=="bqq103f2"),
                   which(colnames(FDF)=="bqq014b"),
                   which(colnames(FDF)=="name"),
                   which(colnames(FDF)=="date1"),
                   which(colnames(FDF)=="bqq001b"),
                   which(colnames(FDF)=="counter_farm"),
                   which(colnames(FDF)=="country.x"),
                   which(colnames(FDF)=="country.y"),
                   which(colnames(FDF)=="contact1"),
                   which(colnames(FDF)=="contact2"),
                   which(colnames(FDF)=="longitude"),
                   which(colnames(FDF)=="latitude"),
                   which(colnames(FDF)=="slaperm"),
                   which(colnames(FDF)=="bqq007"),
                   which(colnames(FDF)=="bqq006"),
                   which(colnames(FDF)=="bqq013"),
                   which(colnames(FDF)=="TIdddvet"),
                   which(colnames(FDF)=="bqq003"),
                   which(colnames(FDF)=="bqq012"),
                   which(colnames(FDF)=="bqq104b"),
                   which(colnames(FDF)=="bqq071g2"),
                   which(colnames(FDF)=="bqq068k2"),
                   which(colnames(FDF)=="bqq037i"),
                   which(colnames(FDF)=="average"),
                   which(colnames(FDF)=="bqq019"),
                   which(colnames(FDF)=="bqq034")
)]

rownames(FDForest)<-c(1:nrow(FDForest))
glimpse(FDForest)

`%notin%` <- Negate(`%in%`)



library(Metrics)

#################################
TIMES<-10
#Create test and train set
final_repeatedCV_RMSE_values<-list()
final_repeatedCV_voted_predictors<-list()

Forest_train<-list()
Forest_test<-list()

Forest_suffled<-FDForest[c(sample(nrow(FDForest), size=nrow(FDForest),replace=F)),]
ranges_df<-data.frame(cbind(seq(from=1, to=nrow(FDForest),by=floor(nrow(FDForest)/10))[-length(seq(from=1, to=nrow(FDForest),by=floor(nrow(FDForest)/10)))],
                            c(seq(from=0, to=nrow(FDForest),by=floor(nrow(FDForest)/10))[-c(1,length(seq(from=0, to=nrow(FDForest),by=floor(nrow(FDForest)/10))))],
                              seq(from=1, to=nrow(FDForest),by=floor(nrow(FDForest)/10))[length(seq(from=1, to=nrow(FDForest),by=floor(nrow(FDForest)/10)))])))
for (i in 1:TIMES){
  
  Forest_test[[length(Forest_test)+1]]<-Forest_suffled[c(ranges_df$X1[i]:ranges_df$X2[i]),]
  
  Forest_train[[length(Forest_train)+1]]<-Forest_suffled[!(Forest_suffled$farmid %in% Forest_test[[i]]$farmid),]
  
}


#Predict function for Mix-RF
predict.MixRF = function(object, newdata, EstimateRE=TRUE){
  
  forestPrediction <- predict(object$forest, newdata=newdata, OOB=T)
  
  # If not estimate random effects, just use the forest for prediction.
  if(!EstimateRE) return(forestPrediction)
  
  RandomEffects <- predict(object$MixedModel, newdata=newdata, allow.new.levels=T)
  
  completePrediction = forestPrediction + RandomEffects
  
  return(completePrediction)
}


library(MixRF)
#!st loop to identify top 25% of variables in terms of Gini importance for each fold
pruned_predictors_list<-list()
for (w in 1:length(Forest_train)){
  
  eliminate<-c()
  for (p in 1:length(Forest_train[[w]])){
    
    if(length(eliminate)<(ncol(Forest_train[[w]])-4)){
      ######initial forest
      MixRF_model_initial<-MixRF(Y=Forest_train[[w]]$ln_TIdddvet, 
                                 X= Forest_train[[w]][,-c(1,2,3,unlist(eliminate))], 
                                 random="(1|counter_country)" , 
                                 data = Forest_train[[w]],
                                 #initialRandomEffects = 0,
                                 ErrorTolerance = 0.01, 
                                 MaxIterations = 1)
      
      results_within<-data.frame(cbind(rownames(MixRF_model_initial$forest$importance),MixRF_model_initial$forest$importance))
      results_within$IncNodePurity<-as.numeric(as.character(results_within$IncNodePurity))
      row.names(results_within)=c(1:nrow(results_within))
      
      less_important<-as.character(results_within[c(which(results_within$IncNodePurity==min(results_within$IncNodePurity))),1])
    }
    else 
    {
      print(paste("done",p,w))
    }
    
    eliminate[[length(eliminate)+1]]<-which(colnames(Forest_train[[w]]) %in% less_important[1])
  }  
  
  
  final_subset_RFE_it<-c(colnames(Forest_train[[w]][,which(colnames(Forest_train[[w]]) %notin% colnames(Forest_train[[w]][,c(head(unique(unlist(eliminate)),
                                                                          n=(length(unique(unlist(eliminate)))-(ceiling((length(Forest_train[[w]])-3)/4)-1))))]))]))
  
  
  pruned_predictors_list[[length(pruned_predictors_list)+1]]<-final_subset_RFE_it
  
  Forest_train_pruned<-Forest_train[[w]][,(colnames(Forest_train[[w]]) %in% final_subset_RFE_it)]
  glimpse(Forest_train_pruned)
}
##______________________________________________________stops here the first loop

library(rlist)
#Do the frequency count of variables for all folds
final_features<-data.frame(table(unlist(pruned_predictors_list)))
final_features<-final_features[order(-final_features$Freq),]


final_features<-final_features[c(which(final_features$Var1=="counter_country"),
                                 which(final_features$Var1=="farmid"),
                                 which(final_features$Var1=="ln_TIdddvet"),setdiff(seq(1,nrow(final_features),by=1),
                                                                                   c(which(final_features$Var1=="counter_country"),
                                                                                     which(final_features$Var1=="farmid"),
                                                                                     which(final_features$Var1=="ln_TIdddvet")))),]

row.names(final_features)=c(1:nrow(final_features))


votes<-list()
voted_predictors<-list()
for(i in 1:TIMES){
  votes[[length(votes)+1]]<-which(final_features$Freq>i-1)}
for(i in 1:TIMES){
  voted_predictors[[length(voted_predictors)+1]]<-final_features[which(rownames(final_features) %in% votes[[i]]),1]
}





##Running mixrf for all subests to compare for model selection
assess_predictors<-list()
for (i in 1:length(voted_predictors)){
  assess_predictors[[length(assess_predictors)+1]]<-c(as.character(voted_predictors[[i]]))
}
assess_feature_sets<-list()
for (i in 1:length(assess_predictors)){
  assess_feature_sets[[length(assess_feature_sets)+1]]<-FDForest[,(colnames(FDForest) %in% assess_predictors[[i]])]
  row.names(assess_feature_sets[[i]])=c(1:nrow(assess_feature_sets[[i]]))
}

#glimpse(assess_feature_sets)

#2nd loop to identify which frequency of variables accross the folds has lowest RMSE
#####------------>>>>#Run forest
results<-list()
rmse_value_final<-list()
error_rate_final<-list()
threshold<-list()
number_of_features<-list()

for (i in 1:length(assess_feature_sets)){
  MixRF_model<-MixRF(Y=assess_feature_sets[[i]]$ln_TIdddvet, 
                     X= assess_feature_sets[[i]][,-c(1,2,3)], 
                     random="(1|counter_country)" , 
                     data = assess_feature_sets[[i]],
                     #initialRandomEffects = 0,
                     ErrorTolerance = 0.01, 
                     MaxIterations = 1)
  
  results[[length(results)+1]]<-data.frame(cbind(rownames(MixRF_model$forest$importance),MixRF_model$forest$importance))
  results[[i]]$IncNodePurity<-as.numeric(as.character(results[[i]]$IncNodePurity))
  row.names(results[[i]])=c(1:nrow(results[[i]]))
  
  results[[i]][,1] <-factor(results[[i]][,1] ,
                            levels=results[[i]][,1][order(-results[[i]][,2])])  
  
  
  
  MixRF_Predictions_list_All_Test_Sets<-list()
  for (y in 1:length(Forest_test)){
    
    MixRF_Predictions_list_All_Test_Sets[[length(MixRF_Predictions_list_All_Test_Sets)+1]]<-predict.MixRF(MixRF_model,Forest_test[[y]],EstimateRE=TRUE)
    
  }
  
  
  rmse_for_all_datases<-list()
  error_rate_for_all_datases<-list()
  for( m in 1:length(Forest_test)){
    rmse_for_all_datases[[length(rmse_for_all_datases)+1]]<-rmse(Forest_test[[m]]$ln_TIdddvet,
                                                                 MixRF_Predictions_list_All_Test_Sets[[m]])
    error_rate_for_all_datases[[length(error_rate_for_all_datases)+1]]<-rmse(Forest_test[[m]]$ln_TIdddvet,
                                                                             MixRF_Predictions_list_All_Test_Sets[[m]])/mean(Forest_test[[m]]$ln_TIdddvet)
    
  }
  
  rmse_value_final[[length(rmse_value_final)+1]]<-mean(unlist(rmse_for_all_datases))
  error_rate_final[[length(error_rate_final)+1]]<-mean(unlist(error_rate_for_all_datases))
  
  
  threshold[[length(threshold)+1]]<-i-1
  number_of_features[[length(number_of_features)+1]]<-NROW(votes[[i]])
  
  print(paste("done feature subset",i))
}
#End of 2nd loop


final_repeatedCV_RMSE_values<-cbind(threshold,number_of_features,rmse_value_final,error_rate_final)
final_repeatedCV_voted_predictors<-voted_predictors

final_repeatedCV_voted_predictors[[
  which(final_repeatedCV_RMSE_values[,3]==min(data.frame(final_repeatedCV_RMSE_values[,3])))
  ]]

#End of selection of best subset


#Solution plan algorithm
#This is the best model from before
colnames(FDForest)[which(colnames(FDForest)=="bqq103b")]<-"qq103b"
colnames(FDForest)[which(colnames(FDForest)=="bqq103e")]<-"qq103e"
############# Effect sizes itteretive RF #############
strongest_model <- c("bqq023", "bqq024", "bqq033",
                     "bqq045", "bqq050", "bqq055",
                     "bqq060", "bqq066", "bqq075", "bqq078", "bqq080", "bqq084", "bqq087",
                     "bqq096", "bqq101", "bqq102", "qq103b", "qq103e"
)
null_model <-c("bqq002", "bqq010", "bqq011")



colnames(FDForest)[1]<-"countryindex"
comb2_num<-FDForest[,c("countryindex", "ln_TIdddvet", "farmid",null_model,strongest_model)]

comb2_num$bqq033<-as.character(comb2_num$bqq033)
comb2_num$bqq033<-ifelse(comb2_num$bqq033=="missing", NA,comb2_num$bqq033)
comb2_num$bqq033<-as.numeric(comb2_num$bqq033)

comb2_num$qq103b<-ifelse(comb2_num$qq103b=="missing", NA,comb2_num$qq103b)
comb2_num$qq103b<-as.numeric(comb2_num$qq103b)

comb2_num$qq103e<-ifelse(comb2_num$qq103e=="missing", NA,comb2_num$qq103e)
comb2_num$qq103e<-as.numeric(comb2_num$qq103e)

comb2_num$countryindex<-as.factor(comb2_num$countryindex)

#impute the 3 misings
library(mice)
imp<-mice(comb2_num, meth = "rf")
comb2_imp<-complete(imp)

comb2_num<-comb2_imp
glimpse(comb2_num)
###Here is for adding a farm to test the solution algorithm
##This for ncompletely
test_farm_random_profile<-data.frame(
  countryindex = 1,
  ln_TIdddvet  = ln(1 + sqrt((20^2)+1)), #if NA not included 
  farmid       = "id_test",
  bqq002       = ln(1),
  bqq010       = 2,
  bqq011       = ln(10),
  bqq023       = 0,
  bqq024       = 1,
  bqq033       = 1,
  bqq045       = 1,
  bqq050       = 0,
  bqq055       = 0,
  bqq060       = 0,
  bqq066       = 0,
  bqq075       = 1,
  bqq078       = 0,
  bqq080       = 0,
  bqq084       = 0,
  bqq087       = 0,
  bqq096       = 1,
  bqq101       = 1,
  bqq102       = 0,
  qq103b      = 1,
  qq103e      = 1
)#**********************************
row.names(test_farm_random_profile)<-"id_test"

#comb2_num<-rbind(comb2_num,test_farm_random_profile);test_farm_random<-182 #<<--Uncomment if completely new farm

#comb2_num$countryindex<-as.numeric(as.character(comb2_num$countryindex))
#comb2_num$farmid<-as.numeric(as.character(comb2_num$farmid))

#***************This is for a farm within the original database****************

test_farm_random<-170#<--

test_farm_random_id<-comb2_num[test_farm_random,"farmid"]


glimpse(comb2_num)

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

All_scenarios_list<-list()
All_wkns_list<-list()

library(MixRF)
#!!!!!!!!!!!!!!!!!!!!!!!!
#Uncomment the line below before running the loop; Run this to update Mix-RF by placing keep.inbag argument
#!!!!!!!!!!!!!!!!!!!!!!!!
#trace("MixRF",edit=TRUE)#modify to " ,keep.inbag=TRUE " na.action = na.omit "
#!!!!!!!!!!!!!!!!!!!!!!!!

#This is the loop for fitting 10 different mix-RFs and extracting from all the best scenarios that predict AMU decrease
for (q in 1:10){
  
  
  row.names(comb2_num)<-NULL
  if(nrow(comb2_num)>181){
    row.names(comb2_num)[row.names(comb2_num)==182]<-"id_test"
  }
  
 

  
  
  RF <- MixRF(Y = comb2_num$ln_TIdddvet,
              X = comb2_num[, c(strongest_model
                                , null_model
              )],
              random = "(1|countryindex)",
              data = comb2_num,
              initialRandomEffects = 0,
              ErrorTolerance = 0.01,
              MaxIterations = 1)
  

  
  impr<-data.frame(row.names(RF$forest$importance),RF$forest$importance)
  glimpse(impr)
  
  impr[order(-impr$IncNodePurity),]
  
  
  ############## Adding effect size calculation for Random Forest ############## 
  library(rfUtilities)
  library(stats)
  all_RF_features<-c(null_model,strongest_model)
  # Parameter effect sizes
  
  es_bqq002<-rf.effectSize(RF$forest, y = comb2_num$bqq002,  pred.data = comb2_num, x.var = bqq002)
  es_bqq010<-rf.effectSize(RF$forest, y = comb2_num$bqq010,  pred.data = comb2_num, x.var = bqq010)
  es_bqq011<-rf.effectSize(RF$forest, y = comb2_num$bqq011,  pred.data = comb2_num, x.var = bqq011)
  es_bqq023<-rf.effectSize(RF$forest, y = comb2_num$bqq023,  pred.data = comb2_num, x.var = bqq023)
  es_bqq024<-rf.effectSize(RF$forest, y = comb2_num$bqq024,  pred.data = comb2_num, x.var = bqq024)
  es_bqq033<-rf.effectSize(RF$forest, y = comb2_num$bqq033,  pred.data = comb2_num, x.var = bqq033)
  es_bqq045<-rf.effectSize(RF$forest, y = comb2_num$bqq045,  pred.data = comb2_num, x.var = bqq045)
  es_bqq050<-rf.effectSize(RF$forest, y = comb2_num$bqq050,  pred.data = comb2_num, x.var = bqq050)
  es_bqq055<-rf.effectSize(RF$forest, y = comb2_num$bqq055,  pred.data = comb2_num, x.var = bqq055)
  es_bqq060<-rf.effectSize(RF$forest, y = comb2_num$bqq060,  pred.data = comb2_num, x.var = bqq060)
  es_bqq066<-rf.effectSize(RF$forest, y = comb2_num$bqq066,  pred.data = comb2_num, x.var = bqq066)
  es_bqq075<-rf.effectSize(RF$forest, y = comb2_num$bqq075,  pred.data = comb2_num, x.var = bqq075)
  es_bqq078<-rf.effectSize(RF$forest, y = comb2_num$bqq078,  pred.data = comb2_num, x.var = bqq078)
  es_bqq080<-rf.effectSize(RF$forest, y = comb2_num$bqq080,  pred.data = comb2_num, x.var = bqq080)
  es_bqq084<-rf.effectSize(RF$forest, y = comb2_num$bqq084,  pred.data = comb2_num, x.var = bqq084)
  es_bqq087<-rf.effectSize(RF$forest, y = comb2_num$bqq087,  pred.data = comb2_num, x.var = bqq087)
  es_bqq096<-rf.effectSize(RF$forest, y = comb2_num$bqq096,  pred.data = comb2_num, x.var = bqq096)
  es_bqq101<-rf.effectSize(RF$forest, y = comb2_num$bqq101,  pred.data = comb2_num, x.var = bqq101)
  es_bqq102<-rf.effectSize(RF$forest, y = comb2_num$bqq102,  pred.data = comb2_num, x.var = bqq102)
  es_qq103b<-rf.effectSize(RF$forest, y = comb2_num$qq103b,  pred.data = comb2_num, x.var = qq103b)
  es_qq103e<-rf.effectSize(RF$forest, y = comb2_num$qq103e,  pred.data = comb2_num, x.var = qq103e)
  
  ############## Not run:############## 
  ############## Bootstrap of effect size for Wind and Temp ############## 
  B = 1 #999 #Number of bootstrap iterations
  n = nrow(comb2_num)
  es.boot.bqq002 <- vector()
  es.boot.bqq010 <- vector()
  es.boot.bqq011 <- vector()
  es.boot.bqq023 <- vector()
  es.boot.bqq024 <- vector()
  es.boot.bqq033 <- vector()
  es.boot.bqq045 <- vector()
  es.boot.bqq050 <- vector()
  es.boot.bqq055 <- vector()
  es.boot.bqq060 <- vector()
  es.boot.bqq066 <- vector()
  es.boot.bqq075 <- vector()
  es.boot.bqq078 <- vector()
  es.boot.bqq080 <- vector()
  es.boot.bqq084 <- vector()
  es.boot.bqq087 <- vector()
  es.boot.bqq096 <- vector()
  es.boot.bqq101 <- vector()
  es.boot.bqq102 <- vector()
  es.boot.qq103b <- vector()
  es.boot.qq103e <- vector()
  
  for(i in 1:B) {
    boot.samples <- comb2_num[sample(1:nrow(comb2_num), n, replace = TRUE),]
    
    fit <- MixRF(Y = boot.samples$ln_TIdddvet,
                 X = boot.samples[, c(all_RF_features)],
                 random = "(1|countryindex)",
                 data = boot.samples,
                 initialRandomEffects = 0,
                 ErrorTolerance = 0.01,
                 MaxIterations = 1)
    print(i)
    es.boot.bqq002 <- append(es.boot.bqq002, rf.effectSize(fit$forest, y = boot.samples$bqq002, pred.data = boot.samples, x.var = bqq002))
    es.boot.bqq010 <- append(es.boot.bqq010, rf.effectSize(fit$forest, y = boot.samples$bqq010, pred.data = boot.samples, x.var = bqq010))
    es.boot.bqq011 <- append(es.boot.bqq011, rf.effectSize(fit$forest, y = boot.samples$bqq011, pred.data = boot.samples, x.var = bqq011))
    es.boot.bqq023 <- append(es.boot.bqq023, rf.effectSize(fit$forest, y = boot.samples$bqq023, pred.data = boot.samples, x.var = bqq023))
    es.boot.bqq024 <- append(es.boot.bqq024, rf.effectSize(fit$forest, y = boot.samples$bqq024, pred.data = boot.samples, x.var = bqq024))
    es.boot.bqq033 <- append(es.boot.bqq033, rf.effectSize(fit$forest, y = boot.samples$bqq033, pred.data = boot.samples, x.var = bqq033))
    es.boot.bqq045 <- append(es.boot.bqq045, rf.effectSize(fit$forest, y = boot.samples$bqq045, pred.data = boot.samples, x.var = bqq045))
    es.boot.bqq050 <- append(es.boot.bqq050, rf.effectSize(fit$forest, y = boot.samples$bqq050, pred.data = boot.samples, x.var = bqq050))
    es.boot.bqq055 <- append(es.boot.bqq055, rf.effectSize(fit$forest, y = boot.samples$bqq055, pred.data = boot.samples, x.var = bqq055))
    es.boot.bqq060 <- append(es.boot.bqq060, rf.effectSize(fit$forest, y = boot.samples$bqq060, pred.data = boot.samples, x.var = bqq060))
    es.boot.bqq066 <- append(es.boot.bqq066, rf.effectSize(fit$forest, y = boot.samples$bqq066, pred.data = boot.samples, x.var = bqq066))
    es.boot.bqq075 <- append(es.boot.bqq075, rf.effectSize(fit$forest, y = boot.samples$bqq075, pred.data = boot.samples, x.var = bqq075))
    es.boot.bqq078 <- append(es.boot.bqq078, rf.effectSize(fit$forest, y = boot.samples$bqq078, pred.data = boot.samples, x.var = bqq078))
    es.boot.bqq080 <- append(es.boot.bqq080, rf.effectSize(fit$forest, y = boot.samples$bqq080, pred.data = boot.samples, x.var = bqq080))
    es.boot.bqq084 <- append(es.boot.bqq084, rf.effectSize(fit$forest, y = boot.samples$bqq084, pred.data = boot.samples, x.var = bqq084))
    es.boot.bqq087 <- append(es.boot.bqq087, rf.effectSize(fit$forest, y = boot.samples$bqq087, pred.data = boot.samples, x.var = bqq087))
    es.boot.bqq096 <- append(es.boot.bqq096, rf.effectSize(fit$forest, y = boot.samples$bqq096, pred.data = boot.samples, x.var = bqq096))
    es.boot.bqq101 <- append(es.boot.bqq101, rf.effectSize(fit$forest, y = boot.samples$bqq101, pred.data = boot.samples, x.var = bqq101))
    es.boot.bqq102 <- append(es.boot.bqq102, rf.effectSize(fit$forest, y = boot.samples$bqq102, pred.data = boot.samples, x.var = bqq102))
    es.boot.qq103b <- append(es.boot.qq103b, rf.effectSize(fit$forest, y = boot.samples$qq103b, pred.data = boot.samples, x.var = qq103b))
    es.boot.qq103e <- append(es.boot.qq103e, rf.effectSize(fit$forest, y = boot.samples$qq103e, pred.data = boot.samples, x.var = qq103e))
  }
  
  # range(es.boot.bqq024)
  ############## Confidence intervals of Bootstrap of effect size for Wind ############## 
  es_df<-data.frame(c(es_bqq002,es.boot.bqq002
  ),
  c(es_bqq010,es.boot.bqq010
  ),
  c(es_bqq011,es.boot.bqq011
  ),
  c(es_bqq023,es.boot.bqq023
  ),
  c(es_bqq024,es.boot.bqq024
  ),
  c(es_bqq033,es.boot.bqq033
  ),
  c(es_bqq045,es.boot.bqq045
  ),
  c(es_bqq050,es.boot.bqq050
  ),
  c(es_bqq055,es.boot.bqq055
  ),
  c(es_bqq060,es.boot.bqq060
  ),
  c(es_bqq066,es.boot.bqq066
  ),
  c(es_bqq075,es.boot.bqq075
  ),
  c(es_bqq078,es.boot.bqq078
  ),
  c(es_bqq080,es.boot.bqq080
  ),
  c(es_bqq084,es.boot.bqq084
  ),
  c(es_bqq087,es.boot.bqq087
  ),
  c(es_bqq096,es.boot.bqq096
  ), 
  c(es_bqq101,es.boot.bqq101
  ), 
  c(es_bqq102,es.boot.bqq102
  ), 
  c(es_qq103b,es.boot.qq103b
  ), 
  c(es_qq103e,es.boot.qq103e
  ))
  
  oldnames<-colnames(es_df)
  newnames<-c()
  for (i in 1:length(oldnames)){
    #if(nchar(oldnames[i])<29){
    newname<-paste(str_sub(oldnames[i],-7,-2),"ES", sep = "")#(nchar(oldnames[i])-5)
    #}else{
    # newname<-paste(str_sub(oldnames[i],-8,-2),"ES", sep = "")
    #}
    newnames<-append(newnames,newname)
  }
  
  ES_DF<-es_df %>% rename_at(vars(oldnames), ~ newnames)
  row.names(ES_DF)<-NULL
  glimpse(ES_DF)
  #save(ES_DF,file = "ES_DF.Rdata")
  lnn<-function(x){ln(x+((x^2) + 1)^(1/2))}
  un_ln <- function (x) {(exp(2*(x))-1)/(2*exp((x)))}
  library(Rmisc)
  ES_CI<-NULL
  for (i in 1:length(ES_DF)){
    val<-c(substr(colnames(ES_DF[i]),1,nchar(colnames(ES_DF[i]))-2),CI(ES_DF[,i]))
    
    ES_CI<-data.frame(rbind(ES_CI,val))
  }
  #ES_CI[,c(1,3,4,2)]
  
  ES_CI$drct<-ifelse(ES_CI$V1=="bqq002",-1,#-1
              ifelse(ES_CI$V1=="bqq010",NA,#1
              ifelse(ES_CI$V1=="bqq011",NA,#1
              ifelse(ES_CI$V1=="bqq023",1,
              ifelse(ES_CI$V1=="bqq024",1,
              ifelse(ES_CI$V1=="bqq033",1,
              ifelse(ES_CI$V1=="bqq045",-1,
              ifelse(ES_CI$V1=="bqq050",-1,
              ifelse(ES_CI$V1=="bqq055",-1,
              ifelse(ES_CI$V1=="bqq060",-1,
              ifelse(ES_CI$V1=="bqq066",1,
              ifelse(ES_CI$V1=="bqq075",-1,
              ifelse(ES_CI$V1=="bqq078",1,             
              ifelse(ES_CI$V1=="bqq080",-1,
              ifelse(ES_CI$V1=="bqq084",-1,
              ifelse(ES_CI$V1=="bqq087",-1,  
              ifelse(ES_CI$V1=="bqq096",-1,
              ifelse(ES_CI$V1=="bqq101",-1,
              ifelse(ES_CI$V1=="bqq102",-1,
              ifelse(ES_CI$V1=="qq103b",-1,
              ifelse(ES_CI$V1=="qq103e",1,"wrong"
                 )))))))))))))))))))))
  
  ES_CI$sense<-ifelse(is.na(ES_CI$drct),FALSE,(ES_CI$mean>0 &ES_CI$drct>0 |
                                                 ES_CI$mean<0 &ES_CI$drct<0))
  ES_CI$upper <- as.numeric(ES_CI$upper)
  ES_CI$mean <- as.numeric(ES_CI$mean)
  ES_CI$lower <- as.numeric(ES_CI$lower)
  
  
  
  
  
  ############# Investigate Change in AMU #############
  
  comb_entry <- comb2_num[test_farm_random,]
  comb_entry$forest <- predict(RF$forest)[test_farm_random]
  comb_entry$mixmod <- predict(RF$MixedModel, comb2_num, allow.new.levels=TRUE)[test_farm_random]
  comb_entry$pred <- comb_entry$forest + comb_entry$mixmod
  
  
  ##Weaknesses
  wkns_df<-data.frame(t(cbind(
    comb_entry[which(comb_entry==0)][which(colnames(comb_entry[which(comb_entry==0)])%in% ES_CI[which(ES_CI$mean<0),1]==TRUE)],
    comb_entry[which(comb_entry==1)][which(colnames(comb_entry[which(comb_entry==1)])%in% ES_CI[which(ES_CI$mean>0),1]==TRUE)]
  )))
  
  wkns_df$V1<-row.names(wkns_df)
  rownames(wkns_df) <- NULL
  
  wkns_df<-merge(wkns_df,ES_CI,by='V1')
  
  wkns_df[order(abs(wkns_df$mean),decreasing = TRUE),]
  
  ##Strengths
  stgs_df<-data.frame(t(cbind(
    comb_entry[which(comb_entry==0)][which(colnames(comb_entry[which(comb_entry==0)])%in% ES_CI[which(ES_CI$mean>0),1]==TRUE)],
    comb_entry[which(comb_entry==1)][which(colnames(comb_entry[which(comb_entry==1)])%in% ES_CI[which(ES_CI$mean<0),1]==TRUE)]
  )))
  
  stgs_df$V1<-row.names(stgs_df)
  rownames(stgs_df) <- NULL
  
  stgs_df<-merge(stgs_df,ES_CI,by='V1')
  
  stgs_df[order(abs(stgs_df$mean),decreasing = TRUE),]
  
  
  #Solution plans scenarios
  library(devtools) 
  #install_github("swager/randomForestCI")
  library(randomForestCI)
  
  wkns_df$sol<-ifelse(wkns_df[,2]==1,0,1)
  
  
  
  sol_scen<-lapply(1:nrow(wkns_df), function(y) combn(wkns_df$V1, y))
  
  scenarios_list<-list()
  scenarios_df<-NULL
  
  for (i in 1:length(sol_scen)){
    
    ind_sce_df<-NULL
    
    for (j in 1:ncol(sol_scen[[i]])){
      
      
      comb_entry_ud <- comb2_num[test_farm_random,]
      
      if (is.null(ncol(comb_entry_ud[,sol_scen[[i]][,j]]))){
        comb_entry_ud[,sol_scen[[i]][,j]] <- wkns_df[which(wkns_df$V1==sol_scen[[i]][,j]),which(colnames(wkns_df)=="sol")]
        
      }
      else{
        for (t in 1:ncol(comb_entry_ud[,sol_scen[[i]][,j]])){
          comb_entry_ud[,sol_scen[[i]][,j][t]] <- wkns_df[which(wkns_df$V1==sol_scen[[i]][,j][t]),which(colnames(wkns_df)=="sol")]
          
        }
        
      }
      
      
      ind_sce_df<-data.frame(rbind(ind_sce_df,comb_entry_ud))
    }
    scenarios_df<-data.frame(rbind(scenarios_df,ind_sce_df))
    scenarios_list[[i]]<-scenarios_df
    scenarios_df<-NULL
    ind_sce_df<-NULL
  }
  
  
  
  
  
  AMUplan_df<-bind_rows(scenarios_list, .id = "column_label")
  
  #AMUplan_df<-AMUplan_df[order(AMUplan_df$pred,decreasing = FALSE),]
  
  AMUplan_df$id<-"scenario"
  comb2_num$id<-"original_data"
  #glimpse(AMUplan_df[,-c(1,63,64,65)])
  #glimpse(comb2_num)
  
  row.names(comb2_num)[] <- paste(row.names(comb2_num),"org")
  
  comb2_num_upd<-rbind(comb2_num,AMUplan_df[,-1])
  
  
  comb2_num_upd$forest <- predict(RF$forest, comb2_num_upd)
  comb2_num_upd$mixmod <- predict(RF$MixedModel, comb2_num_upd)
  comb2_num_upd$pred   <- comb2_num_upd$forest + comb2_num_upd$mixmod 
  
  
  comb2_num_upd$y_hat  <-randomForestInfJack(RF$forest, comb2_num_upd, calibrate = TRUE)[,1]
  comb2_num_upd$var_hat<-randomForestInfJack(RF$forest, comb2_num_upd, calibrate = TRUE)[,2]
  
  comb2_num_upd$perc_ch<-ifelse(comb2_num_upd$farmid==test_farm_random_id,
                                ((comb2_num_upd$y_hat - comb2_num_upd$y_hat[which(test_farm_random_id == comb2_num_upd$farmid[1:nrow(comb2_num)])])/
                                   comb2_num_upd$y_hat[which(test_farm_random_id == comb2_num_upd$farmid[1:nrow(comb2_num)])]),NA)
  comb2_num_upd$pre_vs_tr<-ifelse(comb2_num_upd$farmid==test_farm_random_id &
                                    comb2_num_upd$pred>comb2_num_upd$ln_TIdddvet ,"bigger", ###comb2_num_upd$pred>comb2_num_upd$ln_TIdddvet,"bigger",
                                  ifelse(comb2_num_upd$farmid==test_farm_random_id &
                                           comb2_num_upd$pred<comb2_num_upd$ln_TIdddvet  ,"smaller",NA))###ln_TIdddvet
  
  
  # 
  # library("lmerTest")
  # 
  # confint(RF$MixedModel,
  #         level = 0.95,
  #         method="boot",
  #         boot.type = c("norm"),
  #         oldNames=FALSE)
  
  
  library(broom.mixed)
  
  tidy(RF$MixedModel,
       effects="ran_vals",
       conf.int = TRUE,
       conf.level = 0.95,
       conf.method = "Wald",
       ddf.method = "lme4")
  
  
  
  
  
  
  
  unq_scen_id<-list()
  for (u in 1:length(sol_scen)){
    helper_id_stor<-list()
    for (w in 1:ncol(sol_scen[[u]])){
      helper_id<-rep(paste(q,"-",u,"-",w),nrow(sol_scen[[u]]))
      helper_id_stor[[w]]<-helper_id
    }
    unq_scen_id[[u]]<-data.frame(unlist(helper_id_stor))
  }
  
  #ijtquw
  
  unlist_sol_df<-cbind(data.frame(unlist(sol_scen)),
                       data.frame(unlist(unq_scen_id)))
  
  
  
  unlist_sol_df$id_index <- match(unlist_sol_df$unlist.unq_scen_id., unique(unlist_sol_df$unlist.unq_scen_id.))
  unlist_sol_df$id_index <- as.character(unlist_sol_df$id_index)
  
  
  comb2_num_upd$iter<-q
  comb2_num_upd$scenario_id<-ifelse(grepl("org", row.names(comb2_num_upd), fixed=TRUE)==TRUE,row.names(comb2_num_upd),paste(row.names(comb2_num_upd),"scen"))
  comb2_num_upd <- transform(comb2_num_upd, id_index = ave(id, id, FUN = seq_along))
  comb2_num_upd$id_index<-ifelse(grepl("scen", comb2_num_upd$scenario_id, fixed=TRUE)==TRUE,comb2_num_upd$id_index,"-")
  
  
  f<-merge(comb2_num_upd, unlist_sol_df, by="id_index",all = TRUE)
  
  
  
  
  ####*************************************************************
  is.na(unique(f$unlist.unq_scen_id.)[1])#######Check to be True!
  ####*************************************************************
  
  
  un_com_pra<-unique(f$unlist.unq_scen_id.)[-1]
  
  stored_all_sol<-NULL
  for (r in 1:length(un_com_pra)){
    help_st<-paste0(f[which(f$unlist.unq_scen_id.==un_com_pra[r]),
                      "unlist.sol_scen."],collapse="|"
    )
    
    stored_all_sol<-rbind(stored_all_sol,cbind(help_st,un_com_pra[r])) 
  }
  colnames(stored_all_sol) <- c("Comb_sol_plan", "unlist.unq_scen_id.")
  
  
  
  fin_f<-merge(f, stored_all_sol, by="unlist.unq_scen_id.",all = TRUE)
  
  fin_f<-fin_f[,c(1,which(colnames(fin_f)=="scenario_id"),
                  which(colnames(fin_f)=="Comb_sol_plan"),
                  which(colnames(fin_f)=="unlist.sol_scen."),2:c(length(fin_f)-3))]#becareful here
  
  
  
  
  #f<-f[order(as.numeric(f$id_index)),]
  All_scenarios_list[[q]]<-fin_f
  All_wkns_list[[q]]<-wkns_df
  # f<-NULL
  # comb2_num_upd<-NULL
  # unlist_sol_df<-NULL
  # sol_scen<-NULL
  
  print(paste("Iteration",q))
  
  
}


###Check that no NAs have been created should be 1,3,4
#Filtering scenarios
library(rlist)
var_s_na<-list()
var_s_cor<-list()
var_s_fal<-list()
sc_apr<-list()
selected_farm <- test_farm_random
selected_farm_id<-test_farm_random_id
glimpse(comb2_num)
for (i in 1:length(All_scenarios_list)){
  g_df<-data.frame(which(is.na(All_scenarios_list[[i]]), arr.ind=TRUE))
  #print(table(g_df[,2]))
  
  var_s_na[[i]]<-All_wkns_list[[i]]$V1[which(is.na(All_wkns_list[[i]]$sense))]
  var_s_cor[[i]]<-All_wkns_list[[i]]$V1[which(All_wkns_list[[i]]$sense==TRUE)]
  
  var_s_fal[[i]]<-All_wkns_list[[i]]$V1[which(All_wkns_list[[i]]$sense==FALSE)]
  
  
  All_scenarios_list[[i]]$unlist.unq_scen_id.<-ifelse(grepl("org",All_scenarios_list[[i]]$scenario_id, fixed=TRUE),
                                                      All_scenarios_list[[i]]$scenario_id,All_scenarios_list[[i]]$unlist.unq_scen_id.)
  
  
  All_scenarios_list[[i]]$unlist.unq_scen_id.<- ifelse(grepl(ifelse(selected_farm>181,paste(as.character(selected_farm_id),"org"),paste(which(comb2_num$farmid==selected_farm_id),"org")),
                                                             All_scenarios_list[[i]]$unlist.unq_scen_id., fixed=TRUE)   &
                                                         nchar(ifelse(selected_farm>181,paste(as.character(selected_farm_id),"org"),paste(which(comb2_num$farmid==selected_farm_id),"org"))) == 
                                                         nchar(All_scenarios_list[[i]]$unlist.unq_scen_id.) ,
                                                       substr(All_scenarios_list[[i]]$scenario_id[which(grepl(ifelse(selected_farm>181,
                                                                                                                     paste(as.character(selected_farm_id),"org"),
                                                                                                                     paste(which(comb2_num$farmid==selected_farm_id),"org")) ,
                                                                                                              All_scenarios_list[[i]]$unlist.unq_scen_id., fixed=TRUE)  &
                                                                                                          nchar(ifelse(selected_farm>181,paste(as.character(selected_farm_id),"org"),paste(which(comb2_num$farmid== selected_farm_id),"org"))) == 
                                                                                                          nchar(All_scenarios_list[[i]]$unlist.unq_scen_id.))],1,
                                                              nchar(All_scenarios_list[[i]]$scenario_id[which(grepl(ifelse(selected_farm>181,paste(as.character(selected_farm_id),"org"),paste(which(comb2_num$farmid==selected_farm_id),"org")),
                                                                                                                    All_scenarios_list[[i]]$unlist.unq_scen_id., fixed=TRUE)  &
                                                                                                                nchar(ifelse(selected_farm>181,paste(as.character(selected_farm_id),"org"),paste(which(comb2_num$farmid==selected_farm_id),"org"))) == nchar(All_scenarios_list[[i]]$unlist.unq_scen_id.))])-2),
                                                       All_scenarios_list[[i]]$unlist.unq_scen_id.)
  
  
  
  ############# Remove Increased TIdddvet Predictions #############
  if(is.integer0(which(All_scenarios_list[[i]]$perc_ch>0 & All_scenarios_list[[i]]$pre_vs_tr=="bigger" |
                       All_scenarios_list[[i]]$perc_ch<0 & All_scenarios_list[[i]]$pre_vs_tr=="bigger"))==TRUE){
    sc_apr[[i]]<- All_scenarios_list[[i]][-c(which(All_scenarios_list[[i]]$perc_ch>0)),]#alert for minus addition
    print("1st_if")
  }else if(length(table(All_scenarios_list[[i]]$pre_vs_tr))==1 & data.frame(table(All_scenarios_list[[i]]$pre_vs_tr))[1,1]=="smaller"){
    sc_apr[[i]]<- All_scenarios_list[[i]][-c(which(All_scenarios_list[[i]]$perc_ch>0)),]
    
    print("2nd_if")
    
  } else{
    sc_apr[[i]]<- All_scenarios_list[[i]][-c(which(All_scenarios_list[[i]]$perc_ch>0  & All_scenarios_list[[i]]$pre_vs_tr=="bigger" |
                                                     #All_scenarios_list[[i]]$perc_ch<0 & All_scenarios_list[[i]]$pre_vs_tr=="bigger" |
                                                     All_scenarios_list[[i]]$perc_ch>0 & All_scenarios_list[[i]]$pre_vs_tr=="smaller"
    )),]
    print("3rd_if")
    
  }
  
  
  #sc_apr[[i]]<- All_scenarios_list[[i]][-c(which(All_scenarios_list[[i]]$perc_ch>0)),]
  print("4th_outside")
}

which(sc_apr[[i]]$perc_ch>0)
table(sc_apr[[i]]$pre_vs_tr)

sc_apr_df<-do.call("rbind", sc_apr)


sc_apr_df<-sc_apr_df[-which(grepl("org",sc_apr_df$unlist.unq_scen_id., fixed=TRUE)),]

rem_c<-NULL
for (b in 1:nrow(sc_apr_df)){
  
  if(ifelse(is.na(grepl(sc_apr_df$Comb_sol_plan[b],unique(unlist(var_s_fal)), fixed=FALSE)),
            FALSE,any(grepl(sc_apr_df$Comb_sol_plan[b],unique(unlist(var_s_fal)), fixed=FALSE)))==TRUE){
    rem_c[b]<-b 
  }
}


rem_c <- rem_c[!is.na(rem_c)]

sc_apr_df<-sc_apr_df[-rem_c,]


D<-sc_apr_df %>% 
  dplyr::group_by(sc_apr_df[,c("Comb_sol_plan")]) %>%
  dplyr::summarise(count = n() ,fr=n()*100/nrow(sc_apr_df),mean(perc_ch),mean(y_hat),mean(var_hat),mean(mixmod),mean(pred),mean(ln_TIdddvet))

D$count<-ifelse(is.na(D$`sc_apr_df[, c("Comb_sol_plan")]`),D$count,D$count/(str_count(D$`sc_apr_df[, c("Comb_sol_plan")]`,"-")+1))

for(b in 1:nrow(D)){
  D$count[b]<- D$count[b]/length(unlist(strsplit(D$`sc_apr_df[, c("Comb_sol_plan")]`[b],"|", fixed=TRUE)))
  
  D$fr[b]<-(D$count[b]*100/q)
  
}
#View(D)

table(unlist(var_s_cor))
table(unlist(var_s_na))
table(unlist(var_s_fal))

Nm_days_round<-ceiling(365/comb2_num$bqq010[which(comb2_num$farmid==test_farm_random_id)])
Nm_workers<-exp(comb2_num$bqq002[which(comb2_num$farmid==test_farm_random_id)])
Nm_chickens_r<-comb2_num$bqq011[which(comb2_num$farmid==test_farm_random_id)]
Nm_rounds<-comb2_num$bqq010[which(comb2_num$farmid==test_farm_random_id)]
Stables<-2

costs_df<-data.frame(practise=c("---", "bqq023", "bqq024","bqq033", "bqq045","bqq050", "bqq055","bqq060", "bqq066", "bqq075",
                                "bqq078","bqq080", "bqq084","bqq087", "bqq096","bqq101", "bqq102", "qq103b", "qq103e"),
                     Impl_cost_Euro  =c(0, 0,  (Nm_chickens_r/10),  0,    0, 107.9, 143.8, NA, NA, 0.02399*Nm_chickens_r*Nm_rounds,
                                        (Nm_chickens_r/10), 5200, 80, 6000,  200, 130*Stables,(45*(Nm_workers+5)*Stables),NA, NA),
                     Oper_cost_Euro_y=c(0, 0, 0,  0,  0, 0, 0,  NA, NA,  0,
                                        0,   0,   40,  0, 80,     0,  0, 0,  0),
                     Last_y     =c(0, Inf, Inf, Inf, Inf, 10,  10,  NA, NA, Nm_days_round/365,#change accordin to farm
                                   0,   0,  0,  10,  Inf, 5, 3,  NA,  NA),
                     Rounds_impl=c(0, 1, 1, 1, 1, 1, 1, 1, NA, 1,
                                   NA, NA,  1,  NA,  1,   1,  1,NA, NA  ))
costs_df$Depr_y<-ifelse(costs_df$Last_y==0,0,
                        ifelse(costs_df$Last_y==Inf,0,
                               ifelse(is.na(costs_df$Last_y),0,costs_df$Impl_cost_Euro/costs_df$Last_y)))

costs_df$Total_Cost_Euro_y <- costs_df$Impl_cost_Euro + costs_df$Oper_cost_Euro_y +costs_df$Depr_y

costs_df$Total_Cost_Euro_r_y <- costs_df$Impl_cost_Euro + ((costs_df$Oper_cost_Euro_y + costs_df$Depr_y)/comb2_num$bqq010[which(comb2_num$farmid==test_farm_random_id)])




lk<-do.call("rbind",ifelse(is.na(nchar(D$`sc_apr_df[, c("Comb_sol_plan")]`)),list(NA),
                           ifelse(nchar(D$`sc_apr_df[, c("Comb_sol_plan")]`)>6,
                                  strsplit(as.character(data.frame(D)[,1]), split='|', fixed=TRUE),
                                  D$`sc_apr_df[, c("Comb_sol_plan")]`)))

for(i in 1:nrow(lk)){
  lk[i,][duplicated(lk[i,])] <- "---" 
}

lk<-data.frame(lk)

costs<-NULL
for(i in 1:nrow(lk)){
  costs<-rbind(costs,sum(costs_df[which(costs_df$practise %in% lk[i,]),"Total_Cost_Euro_y"]))
  
}  
lk$Total_Cost_Euro_y<-costs
D$Total_Cost_Euro_y<-costs


#D$Quantile<-findInterval(comb2_num[which(comb2_num$farmid==test_farm_random_id),"ln_TIdddvet"], quantile(comb2_num$ln_TIdddvet, c(0, .25, .5, .75, 1) ))
#D$Pred_Quantile<-findInterval(D$`mean(pred)`, quantile(comb2_num_upd$pred , c(0, .25, .5, .75, 1) ))             

colnames(D)[colnames(D) == 'sc_apr_df[, c("Comb_sol_plan")]'] <- "var_name" # Rename column

D$var_name <- gsub("bqq024", "<>bqq024_Depop_steps<>", D$var_name, ignore.case = TRUE)
D$var_name <- gsub("bqq055", "<>bqq055_MtSpl_prev_meas<>", D$var_name, ignore.case = TRUE)
D$var_name <- gsub("bqq066", "<>bqq066_1km_nat_water<>", D$var_name, ignore.case = TRUE)
D$var_name <- gsub("bqq080", "<>bqq080_disnf_b_for_vehcls<>", D$var_name, ignore.case = TRUE)
D$var_name <- gsub("bqq084", "<>bqq084_hygienogram<>", D$var_name, ignore.case = TRUE)
D$var_name <- gsub("bqq096", "<>bqq096_Full_disc_water_sys<>", D$var_name, ignore.case = TRUE)
D$var_name <- gsub("bqq101", "<>bqq101_Mat_stable__recgn<>", D$var_name, ignore.case = TRUE)
D$var_name <- gsub("bqq102", "<>bqq102_Stable_spec_cloth<>", D$var_name, ignore.case = TRUE)


#D$var_name <- gsub("|", "<>", D$var_name, ignore.case = TRUE)
D<-D[c(which(D$fr==100)),]

glimpse(D)

stgs_df$V1 <- gsub("bqq033", "bqq033_Wat_checked_at", stgs_df$V1, ignore.case = TRUE)
stgs_df$V1 <- gsub("bqq055", "bqq055_MtSpl_prev_meas", stgs_df$V1, ignore.case = TRUE)
stgs_df$V1 <- gsub("bqq075", "bqq075_Vacc", stgs_df$V1, ignore.case = TRUE)
stgs_df$V1 <- gsub("bqq078", "bqq078_Smpld_hs_Density", stgs_df$V1, ignore.case = TRUE)
stgs_df$V1 <- gsub("bqq080", "bqq080_disnf_b_for_vehcls", stgs_df$V1, ignore.case = TRUE)
stgs_df$V1 <- gsub("bqq087", "bqq087_FARM_hygiene_lock", stgs_df$V1, ignore.case = TRUE)
stgs_df$V1 <- gsub("bqq096", "bqq096_Full_disc_water_sys", stgs_df$V1, ignore.case = TRUE)
stgs_df$V1 <- gsub("bqq102", "bqq102_Stable_spec_cloth", stgs_df$V1, ignore.case = TRUE)

stgs_df


wkns_df$V1 <- gsub("bqq023", "bqq023_Dir_contact_vis",  wkns_df$V1, ignore.case = TRUE)
wkns_df$V1 <- gsub("bqq024", "bqq024_Depop_steps",      wkns_df$V1, ignore.case = TRUE)
wkns_df$V1 <- gsub("bqq045", "bqq045_Check_in_vis",     wkns_df$V1, ignore.case = TRUE)
wkns_df$V1 <- gsub("bqq050", "bqq050_Handwsh_bfr_entr", wkns_df$V1, ignore.case = TRUE)
wkns_df$V1 <- gsub("bqq060", "bqq060_Fence",            wkns_df$V1, ignore.case = TRUE)
wkns_df$V1 <- gsub("bqq066", "bqq066_1km_nat_water",    wkns_df$V1, ignore.case = TRUE)
wkns_df$V1 <- gsub("bqq084", "bqq084_hygienogram",      wkns_df$V1, ignore.case = TRUE)
wkns_df$V1 <- gsub("bqq101", "bqq101_Mat_stable__recgn",wkns_df$V1, ignore.case = TRUE)

wkns_df


colnames(ES_DF)[colnames(ES_DF) == "bqq002ES"] <- "bqq002_Workers_num" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq010ES"] <- "bqq010_rounds_y" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq011ES"] <- "bqq011_chicks_round" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq023ES"] <- "bqq023_Dir_contact_vis" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq024ES"] <- "bqq024_Depop_steps" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq033ES"] <- "bqq033_Wat_checked_at" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq045ES"] <- "bqq045_Check_in_vis" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq050ES"] <- "bqq050_Handwsh_bfr_entr" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq055ES"] <- "bqq055_MtSpl_prev_meas" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq060ES"] <- "bqq060_Fence" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq066ES"] <- "bqq066_1km_nat_water" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq075ES"] <- "bqq075_Vacc" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq078ES"] <- "bqq078_Smpld_hs_Density" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq080ES"] <- "bqq080_disnf_b_for_vehcls" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq084ES"] <- "bqq084_hygienogram" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq087ES"] <- "bqq087_FARM_hygiene_lock" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq096ES"] <- "bqq096_Full_disc_water_sys" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq101ES"] <- "bqq101_Mat_stable__recgn" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "bqq102ES"] <- "bqq102_Stable_spec_cloth" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "qq103bES"] <- "bqq103b_Roof_vent" # Rename column
colnames(ES_DF)[colnames(ES_DF) == "qq103eES"] <- "bqq103e_Length_vent" # Rename column

glimpse(ES_DF)

#__________________________________________________________________________________________________________________________________
#Prototypes and proximity matrix visualisation
############# Expand MDS frame #############
##### create predictive column #####

#Reform comb2_num as before the plans
row.names(comb2_num)<-sub(" org", "", rownames(comb2_num), fixed = TRUE)
comb2_num$id<-NULL

RF <- MixRF(Y = comb2_num$ln_TIdddvet,
            X = comb2_num[, c(strongest_model
                              , null_model
            )],
            random = "(1|countryindex)",
            data = comb2_num,
            initialRandomEffects = 0,
            ErrorTolerance = 0.01,
            MaxIterations = 1)


forest_prediction <- predict(RF$forest, comb2_num)
random_effects <- predict(RF$MixedModel, comb2_num)
predicted_use <- forest_prediction + random_effects

##### 3D MDS ##### 
mds <- MDSplot(RF$forest, fac = comb2_num$ln_TIdddvet, k = 3, 
               pdf(file = NULL), palette = rep(c(1:9)))

comb_pred <- cbind(comb2_num, predicted_use, mds[[1]][,1], mds[[1]][,2],mds[[1]][,3])

##### predictive column continued #####
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
comb_pred$norm_ln_TIdddvet <- normalize(comb_pred$ln_TIdddvet)
comb_pred$norm_pred <- normalize(comb_pred$predicted_use)


comb_pred <- transform(comb_pred, Real = ifelse(norm_ln_TIdddvet > 0.5, "High User Real", "Low User Real"))
comb_pred$Real <- as.factor(comb_pred$Real)
comb_pred <- transform(comb_pred, Predicted = ifelse(norm_pred > 0.5, "High User Predicted", "Low User Predicted"))
comb_pred$Predicted <- as.factor(comb_pred$Predicted)
comb_pred$norm_ln_TIdddvet <- NULL
comb_pred$norm_pred <- NULL

##### Clusters #####
library(mclust)

d_clust <- Mclust(as.matrix(comb_pred[, c(26:28)]), G=1:20)
m.best <- dim(d_clust$z)[2]
expanded_set <- comb_pred

strongest_model_index <-  match(strongest_model, names(expanded_set))
null_model_index <- match(null_model, names(expanded_set))

expanded_set$Real <- factor(expanded_set$Real)
expanded_set$Predicted <- factor(expanded_set$Predicted)

km <- kmeans(expanded_set[, c(26:28)], centers = m.best)
expanded_set$cluster <- factor(km$cluster)

#name_list <- names(expanded_set[,c(1:6, strongest_model_index, null_model_index, 28, 29, 30)])

############# Cluster List ############# 
for (i in 2:(ncol(expanded_set)-7)) {
  if (i >= which(colnames(expanded_set) == "bqq023")) {
    expanded_set[,i] <- factor(expanded_set[,i])
  } else {
    expanded_set[,i] <- as.numeric(expanded_set[,i])
  }
}


cluster_list <- list()
for (i in 1:nlevels(as.factor(expanded_set$cluster))) {
  cluster_list[[length(cluster_list) + 1]] <- expanded_set[which(expanded_set$cluster == i),]
}

length(cluster_list[[i]])

lnn<-function(x){ln(x+((x^2) + 1)^(1/2))}
un_ln <- function (x) {(exp(2*(x))-1)/(2*exp((x)))}

for(i in 1:length(cluster_list)) {
  cluster_list[[i]]$ln_TIdddvet <- un_ln(cluster_list[[i]]$ln_TIdddvet)
  cluster_list[[i]]$predicted_use <- un_ln(cluster_list[[i]]$predicted_use)
  cluster_list[[i]]$bqq002 <- exp(cluster_list[[i]]$bqq002)
  cluster_list[[i]]$bqq011 <- exp(cluster_list[[i]]$bqq011)
}



############# Create Prototypes Per Cluster ############# 
##### Prepare Data #####


strongest_model_index <-  match(strongest_model, names(cluster_list[[1]]))
null_model_index <- match(null_model, names(cluster_list[[1]]))
name_list <- names(cluster_list[[1]])

proximities <- RF$forest$proximity
rownames(proximities) <- c(1:nrow(expanded_set))
colnames(proximities) <- c(1:nrow(expanded_set))

expanded_set$Predicted <- relevel(expanded_set$Predicted, "Low User Predicted")

proto_set <- expanded_set

proto_set$ln_TIdddvet <-  un_ln(proto_set$ln_TIdddvet)
proto_set$predicted_use <- un_ln(proto_set$predicted_use)
proto_set$bqq002 <- exp(proto_set$bqq002)
proto_set$bqq011 <- exp(proto_set$bqq011)

##### Create Cluster Subsets #####
occurance_list_high <- c()
occurance_list_low <- c()

for (j in 1:length(cluster_list)) {
  
  
  for (k in 1:nlevels(cluster_list[[j]]$Predicted)) {
    cluster <- cluster_list[[j]][which(cluster_list[[j]]$Predicted == levels(cluster_list[[j]]$Predicted)[k]),]
    
    if (nrow(cluster) > 1) {
      
      ### Use full set ###
      n_pred <- table(cluster_list[[j]]$Predicted[which(cluster_list[[j]]$Predicted == levels(cluster_list[[j]]$Predicted)[k])])
      n_knn <- n_pred[which(n_pred == min(n_pred[n_pred>0]))]
      names(n_knn) <- NULL
      ###
      
      percent_clust <- c()
      range <- as.numeric(row.names(cluster))
      prox <- proximities[c(range), c(range)]
      
      knn_list <- c()
      for(h in 1:NROW(prox)) {
        knn <- ifelse(NROW(prox)==1,1,sum(tail(sort(prox[h,]), n = n_knn))-1)
        knn_it <- cbind(rownames(cluster[h,]), knn)
        knn_list <- rbind(knn_list, knn_it)
      }
      
      mknn <- knn_list[which(knn_list[,2] == max(knn_list[,2]))]
      prox_mknn <- ifelse(length(prox)==1,prox,prox[mknn[1],])
      nums <- ifelse(length(prox)==1,range,as.numeric(mknn))
      subsetknn <- proto_set[nums,]#c(1:6, strongest_model_index, null_model_index, 28, 29, 30)
      percent_clust <- rbind(percent_clust, subsetknn)
      
      if (k == 1) {
        occurance_list_high <- rbind(occurance_list_high, percent_clust)
      } else {
        occurance_list_low <- rbind(occurance_list_low, percent_clust)
      }
      
    } else if (nrow(cluster) == 1) {
      
      ### Use full set ###
      n_pred <- table(cluster_list[[j]]$Predicted[which(cluster_list[[j]]$Predicted == levels(cluster_list[[j]]$Predicted)[k])])
      n_knn <- n_pred[which(n_pred == min(n_pred[n_pred>0]))]
      names(n_knn) <- NULL
      ###
      
      percent_clust <- c()
      
      range <- as.numeric(rownames(cluster))
      prox <- proximities[c(range), c(range)]
      
      knn_list <- c()
      for(h in 1:NROW(prox)) {
        knn <- ifelse(NROW(prox)==1,1,sum(tail(sort(prox[h,]), n = n_knn))-1)
        knn_it <- cbind(rownames(cluster[h,]), knn)
        knn_list <- rbind(knn_list, knn_it)
      }
      
      mknn <- knn_list[which(knn_list[,2] == max(knn_list[,2]))]
      prox_mknn <- ifelse(length(prox)==1,prox,prox[mknn[1],])
      nums <- ifelse(length(prox)==1,range,as.numeric(mknn))
      subsetknn <- proto_set[nums,]#c(1:6, strongest_model_index, null_model_index, 28, 29, 30)
      percent_clust <- rbind(percent_clust, subsetknn)
      
      if (k == 1) {
        occurance_list_high <- rbind(occurance_list_high, percent_clust)
      } else {
        occurance_list_low <- rbind(occurance_list_low, percent_clust)
      }
      
    } else {
      print(paste("Empty Set"))
    }
  }
}
#print(c("proto_set",proto_set))
#print(c("occurance_list_high",occurance_list_high));
#print(c("occurance_list_low",occurance_list_low));

##### Create Overall Prototype #####
prototypes_occurance <- c()

for (i in 1:(length(occurance_list_high))) {
  if (class(occurance_list_high[,i]) == "factor") {
    name <- names(occurance_list_low)[i]
    table <- table(proto_set[which(proto_set$Predicted=="Low User Predicted"),name])#table(occurance_list_low[,i])
    most_freq <- table[which(table == max(table))]
    value <- names(most_freq[1])
    percentage <- (max(table)/sum(table))
    predicted_class <- "Low User Predicted"
    prototypes_occurance_it <- cbind(name, value, percentage, predicted_class)
    prototypes_occurance <- rbind(prototypes_occurance, prototypes_occurance_it)
    
    name <- names(occurance_list_high)[i]
    table <- table(proto_set[which(proto_set$Predicted=="High User Predicted"),name])#table(occurance_list_high[,i])
    most_freq <- table[which(table == max(table))]
    value <- names(most_freq[1])
    percentage <- (max(table)/sum(table))
    predicted_class <- "High User Predicted"
    prototypes_occurance_it <- cbind(name, value, percentage, predicted_class)
    prototypes_occurance <- rbind(prototypes_occurance, prototypes_occurance_it)
    
  } else {
    name <- names(occurance_list_low)[i]
    value <-round(median(as.numeric(proto_set[which(proto_set$Predicted=="Low User Predicted"),name])),2)#round(median(occurance_list_low[,i]), 2)
    #print(c(name,"-->",as.numeric(proto_set[which(proto_set$Predicted=="Low User Predicted"),name])))
    percentage <- "-"
    predicted_class <- "Low User Predicted"
    prototypes_occurance_it <- cbind(name, value, percentage, predicted_class)
    prototypes_occurance <- rbind(prototypes_occurance, prototypes_occurance_it)
    
    name <- names(occurance_list_high)[i]
    value <- round(median(as.numeric(proto_set[which(proto_set$Predicted=="High User Predicted"),name])),2)##round(median(occurance_list_high[,i]), 2)
    percentage <- "-"
    predicted_class <- "High User Predicted"
    prototypes_occurance_it <- cbind(name, value, percentage, predicted_class)
    prototypes_occurance <- rbind(prototypes_occurance, prototypes_occurance_it)
    
  }
}
prototypes_occurance <- data.frame(prototypes_occurance)

##### Create Cluster Prototypes #####
occurance_list <- rbind(occurance_list_high, occurance_list_low)
prototypes_clusters <- list()
#nlevels
for (x in 1:length(table(occurance_list$cluster))) {
  #occurance_iteration <- occurance_list[which(occurance_list$cluster == x),]
  occurance_iteration <- proto_set[which(proto_set$cluster == x),colnames(occurance_list)]
  
  prototypes_iteration <- c()
  occurance_iteration$Predicted <- droplevels(occurance_iteration$Predicted)
  #nlevels
  
  for (j in 1:nlevels(as.factor(occurance_iteration$Predicted))) {
    iteration_set <- occurance_iteration[which(occurance_iteration$Predicted == levels(occurance_iteration$Predicted)[j]),]
    
    for (k in 1:(length(iteration_set))) {
      if (class(iteration_set[,k]) == "factor") {
        name <- names(iteration_set)[k]
        table <- table(iteration_set[,k])
        most_freq <- table[which(table == max(table))]
        value <- names(most_freq[1])
        percentage <- (max(table)/sum(table))
        predicted_class <- levels(as.factor(occurance_iteration$Predicted))[j]
        prototypes_occurance_it <- cbind(name, value, percentage, predicted_class)
        prototypes_iteration <- rbind(prototypes_iteration, prototypes_occurance_it)
        
      } else {
        name <- names(iteration_set)[k]
        value <- round(median(iteration_set[,k]),2)
        #print(c(name,"--->",iteration_set[,k]))
        percentage <- "-"
        predicted_class <- levels(occurance_iteration$Predicted)[j]
        prototypes_occurance_it <- cbind(name, value, percentage, predicted_class)
        prototypes_iteration <- rbind(prototypes_iteration, prototypes_occurance_it)
        
      }
    }
  }
  prototypes_clusters[[length(prototypes_clusters) + 1]] <- data.frame(prototypes_iteration)
  prototypes_clusters[[x]] <- arrange(prototypes_clusters[[x]], name,desc(predicted_class))
  
}

##### Find Gini Importances #####
Gini_importance <- data.frame(RF$forest$importance)
Gini_importance$variable_names <- factor(rownames(Gini_importance))
Gini_importance <- Gini_importance[, c(2,1)]
total_importance <- sum(Gini_importance[,2])
Gini_importance$percentage <- (Gini_importance[,2]/total_importance)


#################################### Importance Cluster Check ####################################
is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

proto_lengths <- c()
proto_names <- c()

for (m in 1:length(prototypes_clusters)) {
  #proto_lengths <- append(proto_lengths, nrow(prototypes_clusters[[i]]))
  proto_lengths <- append(proto_lengths, length(table(prototypes_clusters[[m]]$predicted_class)))
  
  if(proto_lengths[m]==1){
    proto_names<-append(proto_names, names(table(prototypes_clusters[[m]]$predicted_class)))
    
  }
}

#dk_cluster <- match(min(proto_lengths), proto_lengths)
dk_cluster <- which(proto_lengths==1)
print(c("Clusters-->",proto_lengths))
print(c("1Class_cluster-->",dk_cluster))


if(length(dk_cluster)!=0){
  for (n_dk_cluster in dk_cluster){
    for (p in 1:nrow(prototypes_clusters[[n_dk_cluster]])) {
      name <- prototypes_clusters[[n_dk_cluster]][p, 1]
      value <- "-"
      percentage <- "-"
      predicted_class <-ifelse(proto_names[which(dk_cluster==n_dk_cluster)]=="Low User Predicted",
                               "High User Predicted","Low User Predicted")
      #print(c("here-->",predicted_class))
      iteration <- cbind(name, value, percentage, predicted_class)
      prototypes_clusters[[n_dk_cluster]] <- rbind(prototypes_clusters[[n_dk_cluster]], iteration)
    }
    
    prototypes_clusters[[n_dk_cluster]] <- arrange(prototypes_clusters[[n_dk_cluster]], name,desc(predicted_class))
  }
}


gini_imp_var <- rownames(Gini_importance[order(Gini_importance$percentage, decreasing = TRUE),])
gini_imp_var <- c("cluster", "ln_TIdddvet", "predicted_use", gini_imp_var)

#___----------------   

imp_clust <- c()
for (i in 1:length(gini_imp_var)) {
  imp_clust <- append(imp_clust, gini_imp_var[i])
  imp_clust <- append(imp_clust, gini_imp_var[i])
}
predicted_class <- c("Low User Predicted", "High User Predicted")
imp_clust <- cbind(imp_clust, predicted_class)

##### Create Final Prototype Table #####
cluster_occurance <- c()
for (j in 1:length(gini_imp_var)) {
  iteration <- imp_clust[which(imp_clust[,1] == gini_imp_var[j]),]
  
  for (i in 1:length(prototypes_clusters)) {
    cluster_value <- prototypes_clusters[[i]][which(prototypes_clusters[[i]][,1] == gini_imp_var[j]),][,c(2,3)]
    iteration <- cbind(iteration, cluster_value)
    
  }
  total_value <- prototypes_occurance[which(prototypes_occurance[,1] == gini_imp_var[j]),c(2,3)]
  iteration <- cbind(iteration, total_value)
  cluster_occurance <- rbind(cluster_occurance, iteration)
}

 cluster_occurance #this is the final result
 
 library(plotly)
 #Plot proximity matrix 3d
 my_colors <- c( "dark green", "dark red")
 my_colors_strokes<- c( "green", "red")
 
 ##### Original MDS-Plot #####
 new_pl <- plot_ly(proto_set,
                   x=~proto_set$mds..1.....1.,
                   y=~proto_set$mds..1.....2.,
                   z=~proto_set$mds..1.....3.,
                   color=proto_set$Predicted,
                   #fill=as.numeric(FDForest_ggplott$rownames.ranges_AMU..i.)*4,
                   text=row.names(proto_set),
                   symbol=proto_set$cluster,
                   symbols=c("square", "diamond", "circle", "cross",
                             "square-open", "diamond-open", "circle-open", "cross-open"),
                   colors=rev(my_colors),
                   strokes=rev(my_colors_strokes),
                   marker = list(size = 8,line = list(width = 0.5)),
                   type="scatter3d",
                   mode= "markers",
                   stroke   = (proto_set$Real),
                   alpha=0.5)
 axx <- list(
   title = "x"
 )
 
 axy <- list(
   title = "y"
 )
 
 axz <- list(
   title = "z"
 )
 
 new_pl <-  new_pl %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz),
                              legend = list(title=list(text='<b> Clusters </b>'),
                                            font = list(size = 12),
                                            itemsizing='constant'))
 
 ##### Entered Farm MDS-entry ##### 
 entry_pl <- plot_ly(proto_set[test_farm_random,],
                     x=~proto_set$mds..1.....1.[test_farm_random],
                     y=~proto_set$mds..1.....2.[test_farm_random],
                     z=~proto_set$mds..1.....3.[test_farm_random],
                     color=proto_set$Real[test_farm_random],
                     text=proto_set$cluster,
                     symbol="x",
                     symbols=c("x"),
                     colors=c("red4", "green4"),
                     strokes=c( "red", "green"),
                     marker = list(size = 10,line = list(width = 0.5)),
                     type="scatter3d",
                     mode= "markers",
                     stroke   = (proto_set$Predicted[test_farm_random]),
                     alpha=0.95,
                     showlegend = FALSE)
 entry_pl <-  entry_pl %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
 
 ##### Combine subbplots ##### 
 subplot(new_pl, entry_pl)#, p2)
