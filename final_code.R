library(plyr)
library(dplyr)
library(ggplot2)
library(InformationValue)
library(fmsb)
library(caret)
library(ROCR)
library(StatMeasures)

#--------------------------Loaded the train file and randomly splitted into train(70%) and validation(30%) set------------------------------#
#----------------Note that the train and validation sets were created and saved using the code (line 10 - 21) and then loaded---------------# 
                          
# basefile <- read.csv("C:/Users/rupangi.kalra/Downloads/ds_data_big/ds_data/data_train.csv",header = T,sep = ",",stringsAsFactors = T, na.strings = c(""," ",NA))
# 
# basefile$id<-as.character(basefile$id)
# 
# set.seed(22)
# n = nrow(basefile)
# trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
# train = basefile[trainIndex ,]
# test = basefile[-trainIndex ,]
# 
# save(train,file = "C:/Users/rupangi.kalra/Downloads/ds_data_big/ds_data/train.rda")
# save(test,file = "C:/Users/rupangi.kalra/Downloads/ds_data_big/ds_data/test.rda")

load("C:/Users/rupangi.kalra/Downloads/ds_data_big/ds_data/train.rda")
load("C:/Users/rupangi.kalra/Downloads/ds_data_big/ds_data/test.rda")

#Making all categorical variables as factors
cat<-c(44:58)
train[,cat] <- lapply(train[,cat],factor)

#Removing columns with high missing values
round((colSums(is.na(train))/nrow(train)) * 100,0)
train<-train[!colnames(train) %in% c("cat6","cat8")]

#-------------------------------------  Missing value imputation for categorical variables -------------------------------------------------# 
#I have used the proportion of each level available in the data for each categorical variable and then distributed the missing values using 
#these proportions

x<-colnames(train[,c(44:55)])[apply(train[,c(44:55)], 2, anyNA)]

for( j in x)
{
temp<- data.frame(table(train[,j]))
temp$Var1<- as.character(temp$Var1)
temp$contri<- round(temp$Freq/sum(temp$Freq),3)
temp1<-train[is.na(train[,j]),colnames(train) %in% c("id",j)]

set.seed(22)
cc<-0
temp2<-temp1[sample(nrow(temp1)),]
total<-nrow(temp2)
temp3<-data.frame()
  
  for(i in 1:nrow(temp))
  {
    temp2[1:round(total*temp$contri[i]),j]<-temp$Var1[i]
    temp1<-temp2[!is.na(temp2[,j]),]
    temp3<-rbind(temp3,temp1)
    temp2<-temp2[is.na(temp2[,j]),]
  }
  
  if(nrow(temp2) != 0)
  {temp2[,j]<-temp$Var1[i]
  temp3<-rbind(temp3,temp2)
  }
  train[,j]<-as.character(train[,j])
  temp3[,j]<-as.character(temp3[,j])
  train[,j] <-ifelse(is.na(train[,j]),temp3[,j][match(train$id, temp3$id)],train[,j])
  train[,j]<-as.factor(train[,j])
}

#-------------------------------------  Missing value imputation for continuous variables -------------------------------------------------# 
#Imputed the missing values by the Median

x<-colnames(train[,c(2:43)])[apply(train[,c(2:43)], 2, anyNA)]

for(j in x)
{
  train[,j] <-ifelse(is.na(train[,j]),median(train[,j],na.rm = TRUE),train[,j])
}

rm("temp","temp1","temp2","temp3",cc,i,j,total,x,cat)

cts_var<-train[c(2:43)]

vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

vif_output<-vif_func(cts_var,thresh = 5,trace = FALSE)

#---------------------------------------------Applied Prinicipal Component Analysis------------------------------------------------------#

data.pca <- prcomp(cts_var, center = TRUE,scale. = TRUE)
summary(data.pca)

#------------------------------------------------------- Calculated WOE and IV------------------------------------------------------------#

train$target<-as.numeric(as.character(train$target))

cat_var<-colnames(train[,c(44:55)])

for(var in cat_var)
{
print(WOETable(X=train[,c(var)], Y=train$target))
print(IV(X=train[,c(var)], Y=train$target))
print(var)
}

#---------------------------------------------- Shortlisted variables using VIF and IV ----------------------------------------------------#

train <- train[, colnames(train) %in% c("cat3","cat4","cat7","cat9","cat14","id","target") | colnames(train) %in% vif_output]

#----------------------------------------------Removed variables which have near zero variance---------------------------------------------#

basefile1<-train[,c(2:45)]

if (length(nearZeroVar(basefile1)) > 0) {
  basefile1 <- basefile1[, -nearZeroVar(basefile1)] 
}

train<-train[colnames(train) %in% colnames(basefile1) | colnames(train) %in% c("target")]

#-----------------------------------------------Building a Logit Model on train data -----------------------------------------------------#

#Treating "cat14" (with 104 levels) as continuous variable
train$cat14<-as.numeric(as.character(train$cat14))

#Model Iterations (line 197-206)

# logit1<-glm(target ~.,data = train,family = "binomial")
# summary(logit)
# 
# logit2<-glm(target ~.-(der1 + der3 + der4 + der5 + der6 + der7 + der8 + der9 + der10 + der11 + der12 + der13 + der14 + der15 + 
#                          der16 + der17 + der18 + der19),data = train,family = "binomial")
# summary(logit1)
# 
# logit3<-glm(target ~.-( der1 + der3 + der4 + der5 + der6 + der7 + der8 + der9 + der10 + der11 + der12 + der13 + der14 + der15 + 
#                           der16 + der17 + der18 + der19 + num15 + num6),data = train,family = "binomial")
# summary(logit2)

#Final Model

final_model<-glm(target ~.-(der1 + der2 + der3 + der4 + der5 + der6 + der7 + der8 + der9 + der10 + der11 + der12 + der13 + der14 + der15 + 
                         der16 + der17 + der18 + der19 + num15 + num6 + cat14),data = train,family = "binomial")
#save(final_model,file = "C:/Users/rupangi.kalra/Downloads/ds_data_big/ds_data/final_model.rda")
summary(final_model)

train$prediction <- predict(final_model, newdata = train, type = "response")

#-----------------------------------------------------Plotting the ROC Curve ----------------------------------------------------------------#

plotROC(train$target, train$prediction)

#----------------------------------- Looking at the Count of Target = 1 in each decile ------------------------------------------------------#

#sorting dataframe according to probabilities
train <- train[order(train$prediction, decreasing = T),]

#creating deciles
train$decile <- decileScores <- decile(train$prediction, decreasing = TRUE)
table(train$decile, train$target)

#---------------------------------------------Performing the same activities on VALIDATION DATA----------------------------------------------#

cat<-c(44:58)
test[,cat] <- lapply(test[,cat],factor)

test<-test[!colnames(test) %in% c("cat6","cat8")]

#-------------------------------------  Missing value imputation for categorical variables -------------------------------------------------# 

x<-colnames(test[,c(44:55)])[apply(test[,c(44:55)], 2, anyNA)]

for( j in x)
{
  temp<- data.frame(table(test[,j]))
  temp$Var1<- as.character(temp$Var1)
  temp$contri<- round(temp$Freq/sum(temp$Freq),3)
  temp1<-test[is.na(test[,j]),colnames(test) %in% c("id",j)]
  
  set.seed(22)
  cc<-0
  temp2<-temp1[sample(nrow(temp1)),]
  total<-nrow(temp2)
  temp3<-data.frame()
  
  for(i in 1:nrow(temp))
  {
    temp2[1:round(total*temp$contri[i]),j]<-temp$Var1[i]
    temp1<-temp2[!is.na(temp2[,j]),]
    temp3<-rbind(temp3,temp1)
    temp2<-temp2[is.na(temp2[,j]),]
  }
  
  if(nrow(temp2) != 0)
  {temp2[,j]<-temp$Var1[i]
  temp3<-rbind(temp3,temp2)
  }
  test[,j]<-as.character(test[,j])
  temp3[,j]<-as.character(temp3[,j])
  test[,j] <-ifelse(is.na(test[,j]),temp3[,j][match(test$id, temp3$id)],test[,j])
  test[,j]<-as.factor(test[,j])
}

#-------------------------------------  Missing value imputation for continuous variables -------------------------------------------------# 

x<-colnames(test[,c(2:43)])[apply(test[,c(2:43)], 2, anyNA)]

for(j in x)
{
  test[,j] <-ifelse(is.na(test[,j]),median(test[,j],na.rm = TRUE),test[,j])
}

rm("temp","temp1","temp2","temp3",cc,i,j,total,x,cat)

test$cat14<-as.numeric(as.character(test$cat14))

#----------------------------------------------------  Predicting on Test Data -----------------------------------------------------------# 

test$prediction <- predict(final_model, newdata = test, type = "response")

# distribution of the prediction score grouped by known outcome
ggplot( test, aes( prediction, color = as.factor(target) ) ) +
 geom_density( size = 1 ) +
 ggtitle( "Testing Set's Predicted Score" )

#---------------------------------------------------------Plotting the ROC Curve----------------------------------------------------------#

plotROC(test$target, test$prediction)

#----------------------------------- Looking at the Count of Target = 1 in each decile ---------------------------------------------------#

#sorting dataframe according to probabilities
test <- test[order(test$prediction, decreasing = T),]

#creating deciles
test$decile <- decileScores <- decile(test$prediction, decreasing = TRUE)
table(test$decile, test$target)

rm(list = setdiff(ls(), c("final_model")))

#---------------------------------------------Performing the same activities on TEST DATA-------------------------------------------------#
 
basefile <- read.csv("C:/Users/rupangi.kalra/Downloads/ds_data_big/ds_data/data_test.csv",header = T,sep = ",",stringsAsFactors = T, na.strings = c(""," ",NA))

cat<-c(44:57)
basefile[,cat] <- lapply(basefile[,cat],factor)

#-------------------------------------  Missing value imputation for categorical variables -------------------------------------------------# 

x<-colnames(basefile[,c(44:57)])[apply(basefile[,c(44:57)], 2, anyNA)]

for( j in x)
{
  temp<- data.frame(table(basefile[,j]))
  temp$Var1<- as.character(temp$Var1)
  temp$contri<- round(temp$Freq/sum(temp$Freq),3)
  temp1<-basefile[is.na(basefile[,j]),colnames(basefile) %in% c("id",j)]
  
  set.seed(22)
  cc<-0
  temp2<-temp1[sample(nrow(temp1)),]
  total<-nrow(temp2)
  temp3<-data.frame()
  
  for(i in 1:nrow(temp))
  {
    temp2[1:round(total*temp$contri[i]),j]<-temp$Var1[i]
    temp1<-temp2[!is.na(temp2[,j]),]
    temp3<-rbind(temp3,temp1)
    temp2<-temp2[is.na(temp2[,j]),]
  }
  
  if(nrow(temp2) != 0)
  {temp2[,j]<-temp$Var1[i]
  temp3<-rbind(temp3,temp2)
  }
  basefile[,j]<-as.character(basefile[,j])
  temp3[,j]<-as.character(temp3[,j])
  basefile[,j] <-ifelse(is.na(basefile[,j]),temp3[,j][match(basefile$id, temp3$id)],basefile[,j])
  basefile[,j]<-as.factor(basefile[,j])
}

#-------------------------------------  Missing value imputation for continuous variables -------------------------------------------------# 

x<-colnames(basefile[,c(2:43)])[apply(basefile[,c(2:43)], 2, anyNA)]

for(j in x)
{
  basefile[,j] <-ifelse(is.na(basefile[,j]),median(basefile[,j],na.rm = TRUE),basefile[,j])
}

rm("temp","temp1","temp2","temp3",cc,i,j,total,x,cat)

basefile$cat14<-as.numeric(as.character(basefile$cat14))

basefile$prediction <- predict(final_model, newdata = basefile, type = "response")
basefile1 <- basefile[c("id","prediction")]
#write.csv(basefile1,"C:/Users/rupangi.kalra/Downloads/ds_data_big/ds_data/test_with_predictions.csv",row.names = FALSE)
