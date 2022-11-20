library(readr)
library(dplyr)
library(caret)
library(tidyr)
library(geojsonR)
#library(randomForest)

Rec.all <- function(data, lev = NULL, model = NULL) {
  uni<-unique(data$obs)
  k<-length(uni)
  b<-0
  for  (i in 1:k)
  {
    a<-MLmetrics::Recall (data$obs, data$pred, positive=uni[i])
    if (is.na(a)) a<-0
    b<-b+a
  }
  c(Rec.all=b/i)
  
}  
set.seed(123)
ctrl <- trainControl(method = "cv", summaryFunction = Rec.all,#mnLogLoss,
                     number = 5)


setwd("E:/R/_cp2022/_ross/inno")
train<-read_csv("train_dataset_train.csv")
test<-read_csv("test_dataset_test.csv")
all<-rbind(train[,-74],test)


wei<-data.frame(crop=train$crop)%>%
  left_join(train%>%
              group_by(crop)%>%
              summarise(n=n()/nrow(train)))

tmp<-list()
#############
for (i in 1:nrow(all))
{
  print (i)
  tmp[[i]]<-FROM_GeoJson(all$.geo[i], Average_Coordinates = TRUE)[[4]]
}

tmp<-as.data.frame(do.call(rbind,tmp))
tmp$X<-as.numeric(as.factor(paste0(round(tmp$V1),"_",round(tmp$V2))))

ndmi<-all%>%select(sort(colnames(all)), -id, -.geo, -area)

ndmi2<-ndmi

ndmi2$q0<-apply(ndmi,1, function (x) sum(x==0))
ndmi2$qOtr<-apply(ndmi,1, function (x) sum(x<0))
ndmi2$qmin<-apply(ndmi,1, function (x) sum(x==min(x)))
#ndmi2$qmax<-apply(ndmi,1, function (x) sum(x==max(x)))
ndmi2$DayMax<-apply(ndmi,1, function (x) which.max(x))

all<-cbind(tmp, area=all$area, ndmi2)

trM<-all[1:nrow(train),]
tstM<-all[(1+nrow(train)):nrow(all),]

trM$crop<-as.factor(train$crop)

#trM2<-trM%>%select(bestfeat, "crop")

set.seed(123)
mod<-train(crop~., 
           data=trM, 
           method="xgbTree",  
       #    method="ranger",  
           metric="Rec.all",
           verbosity=0,
           weights=1/wei$n,
           tuneLength=5,
        #   num.trees = 1000,
      #  tuneGrid=expand.grid(.mtry=c(1,2,9)),
        #tuneGrid=expand.grid(.kmax=c(1:5), .distance = 2 , .kernel = "optimal"),
          #  tuneGrid=expand.grid(.mtry=c(9,39), 
           #                      .splitrule=c("gini","extratrees"), 
            #                    .min.node.size=1),
           trControl = ctrl)
mod
max(mod$results$Rec.all)
mod$resample
plot(mod)
plot(varImp(mod))
#bestfeat<-gsub("`","",rownames(varImp(mod)[[1]])[1:52])
#save(bestfeat, file="bestfeat.RDA")

pred<-predict(mod, newdata=tstM)
table(pred)
sample<-read_csv("sample_solution.csv")
sample$crop<-pred
write_csv(sample, "final2011.csv.csv")