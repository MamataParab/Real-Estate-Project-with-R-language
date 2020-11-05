setwd("C:\\Users\\Admin\\Desktop\\Data Scientist\\Project\\Project1_RealEstate")
trainD=read.csv("housing_train.csv")
testD=read.csv("housing_test.csv")

trainD$Flag="Train"
testD$Flag="Test"
testD$Price=NA

Data=rbind(trainD, testD)

#####NAs
sapply(Data, function(x) sum(is.na(x)))
for(i in 1:length(Data))
{
  if(names(Data[i]) %in% c("Bedroom2","Bathroom", "Car","Landsize", "BuildingArea", "YearBuilt"))
  {
    Data[is.na(Data[,i]),i]=median(Data[,i],na.rm=T)
  }
}

########Data elimination

str(Data)
Data2=Data %>% select(-Address, -SellerG, -Postcode, -YearBuilt, -CouncilArea)
str(Data2)

Data3=Data2 %>% mutate(Suburb=ifelse(Suburb %in% c("Malvern", "Albert Park", "Balwyn"), "Malvern", Suburb),
                         Suburb=ifelse(Suburb %in% c("Middle Park", "Brighton"), "Middle Park", Suburb),
                         Suburb=ifelse(Suburb %in% c("Kew", " Balwyn North", "Camberwell", "Ivanhoe East", "Toorak"), "Kew", Suburb),
                         Suburb=ifelse(Suburb %in% c("Hampton", "Eaglemont", "Brighton East"), "Hampton", Suburb),
                         Suburb=ifelse(Suburb %in% c("Ashburton", "Glen Iris","Kew East", "Armadale", "Mont Albert", "Princes Hill"), "Ashburton", Suburb),
                         Suburb=ifelse(Suburb %in% c("Surrey Hills", "Hawthorn East", "Carlton North", "alvern East"), "Surrey Hills", Suburb),
                         Suburb=ifelse(Suburb %in% c("Seaholme","East Melbourne", "Williamstown"), "Seaholme", Suburb),
                         Suburb=ifelse(Suburb %in% c("Parkville", "Kooyong", "Bentleigh", "Elsternwick", "Aberfeldie", "Northcote", "South Melbourne", "Doncaste"), "Parkville", Suburb),
                         Suburb=ifelse(Suburb %in% c("Alphington", "Fitzroy North", "Port Melbourne", "Strathmore", "Burwood", "Burnley", "Bulleen","Box Hill", "Hawthorn", "Moonee Ponds", 
                                                     "Caulfield South", "Ormond", "Templestowe Lower", "Travancore","Richmond"),"Alphington", Suburb),
                         Suburb=ifelse(Suburb %in% c("Caulfield East", "Hampton East", "Ivanhoe", "Fitzroy", "Prahran","Carlton", "Abbotsford", "Caulfield North", "Clifton Hill", 
                                                     "Ashwood", "Fairfield", "Hughesdale", "Murrumbeena", "Bentleigh East", "Essendon", "South Yarra", "Chadstone"), "Caulfield East", Suburb),
                         Suburb=ifelse(Suburb %in% c("Essendon West", "Caulfield"), "Essendon West", Suburb),
                         Suburb=ifelse(Suburb %in% c("Oakleigh South", "Brunswick"),  "Oakleigh South", Suburb),
                         Suburb=ifelse(Suburb %in% c("Seddon", "Cremorne", "Yarraville"), "Seddon", Suburb),
                         Suburb=ifelse(Suburb %in% c("Docklands", "Rosanna", "Oakleigh", "Moorabbin"),"Docklands", Suburb),
                         Suburb=ifelse(Suburb %in% c("Heidelberg", "Elwood"), "Heidelberg", Suburb),
                         Suburb=ifelse(Suburb %in% c("Carnegie", "Spotswood"), "Carnegie", Suburb),
                         Suburb=ifelse(Suburb %in% c("West Melbourne", "Niddrie", "Thornbury"), "West Melbourne", Suburb),
                         Suburb=ifelse(Suburb %in% c(" Gardenvale", "Brunswick East"), " Gardenvale", Suburb),
                         Suburb=ifelse(Suburb %in% c("Collingwood", "Kingsville", "Coburg"), "Collingwood", Suburb),
                         Suburb=ifelse(Suburb %in% c("North Melbourne", "Preston"), "North Melbourne", Suburb),
                         Suburb=ifelse(Suburb %in% c("Essendon North","Kensington"), "Essendon North", Suburb),
                         Suburb=ifelse(Suburb %in% c("Glen Huntly", "Strathmore Heights"), "Glen Huntly", Suburb),
                         Suburb=ifelse(Suburb %in% c("Bellfield", "Avondale Heights"), "Bellfield", Suburb),
                         Suburb=ifelse(Suburb %in% c("Altona", "Maribyrnong"), "Altona", Suburb),
                         Suburb=ifelse(Suburb %in% c("Oak Park", "Keilor East", "Brunswick West"), " Oak Park",Suburb),
                         Suburb=ifelse(Suburb %in% c("Brooklyn","Albion "), "Albion", Suburb),
                         Suburb=ifelse(Suburb %in% c("Ripponlea", "Fawkner"), "Fawkner", Suburb),
                         Suburb=ifelse(Suburb %in% c("Keilor Park","Heidelberg West","Reservoir", "Braybrook"),"Braybrook", Suburb),
                         Suburb=ifelse(Suburb %in% c("Kingsbury","Gowanbrae","Hadfield"), "Hadfield", Suburb),
                         Suburb=ifelse(Suburb %in% c("Footscray", "South Kingsville","Balaclava"), "Balaclava", Suburb),
                         Suburb=ifelse(Suburb %in% c("Maidstone","Sunshine"), "Sunshine", Suburb),
                         Suburb=ifelse(Suburb %in% c("Heidelberg Heights", "Pascoe Vale", "West Footscray", "Altona North"), "Altona North", Suburb))
length(unique(Data2$Suburb))
length(unique(Data3$Suburb))

library(car)
library(caret)
library(dummies)

str(Data2)
str(Data3)

data4=dummy.data.frame(Data3, names=c("Suburb", "Method", "Type"))
str(data4)

trainD_new=data4 %>% filter(Flag=="Train") %>% select(-Flag)
testD_new=data4 %>% filter(Flag=="Test") %>% select(-Flag, -Price)


set.seed(1)
s=sample(nrow(trainD_new), 0.8*nrow(trainD_new))
t1=trainD_new[s,]
t2=trainD_new[-s,]

LinModel=lm(Price ~., t1)
summary(LinModel)
vif(LinModel) #Issue of alias

#?alias
#alias(LinModel)

summary(LinModel)
LinModel=step(LinModel)
summary(LinModel)

LinModel=lm(formula = Price ~ `Suburb Gardenvale` + `SuburbAirport West` + 
              SuburbAlbion + SuburbAlphington + SuburbAltona + `SuburbAscot Vale` + 
              SuburbAshburton + SuburbBalaclava + `SuburbBalwyn North` + 
              SuburbCanterbury + SuburbCarnegie + `SuburbCaulfield East` + 
              SuburbCollingwood + SuburbDocklands + SuburbDoncaster + `SuburbEssendon North` + 
              SuburbFawkner + SuburbFlemington + SuburbGardenvale + `SuburbGlen Huntly` + 
              SuburbHadfield + SuburbHampton + SuburbHeidelberg + SuburbKealba + 
              SuburbKew + SuburbMalvern + `SuburbMalvern East` + SuburbMelbourne + 
              `SuburbMiddle Park` + SuburbNewport + `SuburbNorth Melbourne` + 
              `SuburbOakleigh South` + SuburbParkville + SuburbSeaholme + 
              SuburbSeddon + SuburbSouthbank + `SuburbSt Kilda` + `SuburbSunshine North` + 
              `SuburbSunshine West` + `SuburbSurrey Hills` + `SuburbWest Melbourne` + 
              `SuburbWilliamstown North` + SuburbWindsor + Rooms + Typeh + 
              Typet + MethodS + MethodSA + MethodSP + Distance + Bathroom + 
              Car + Landsize + BuildingArea, data = t1)
summary(LinModel)
predictdata=predict(LinModel, t2)

error=t2$Price-predictdata
RMSE=error**2 %>% mean() %>% sqrt()
RMSE
#RMSE= 379707

#GBM
library(gbm)
GBMmodel=gbm(formula = Price ~ `Suburb Gardenvale` + `SuburbAirport West` + 
               SuburbAlbion + SuburbAlphington + SuburbAltona + `SuburbAscot Vale` + 
               SuburbAshburton + SuburbBalaclava + `SuburbBalwyn North` + 
               SuburbCanterbury + SuburbCarnegie + `SuburbCaulfield East` + 
               SuburbCollingwood + SuburbDocklands + SuburbDoncaster + `SuburbEssendon North` + 
               SuburbFawkner + SuburbFlemington + SuburbGardenvale + `SuburbGlen Huntly` + 
               SuburbHadfield + SuburbHampton + SuburbHeidelberg + SuburbKealba + 
               SuburbKew + SuburbMalvern + `SuburbMalvern East` + SuburbMelbourne + 
               `SuburbMiddle Park` + SuburbNewport + `SuburbNorth Melbourne` + 
               `SuburbOakleigh South` + SuburbParkville + SuburbSeaholme + 
               SuburbSeddon + SuburbSouthbank + `SuburbSt Kilda` + `SuburbSunshine North` + 
               `SuburbSunshine West` + `SuburbSurrey Hills` + `SuburbWest Melbourne` + 
               `SuburbWilliamstown North` + SuburbWindsor + Rooms + Typeh + 
               Typet + MethodS + MethodSA + MethodSP + Distance + Bathroom + 
               Car + Landsize + BuildingArea, distribution="gaussian", data = t1)

test.predicted=predict.gbm(GBMmodel,newdata=t2,n.trees=100)
RMSE=(test.predicted-t2$Price)**2 %>% mean() %>% sqrt()
RMSE
#RMSE=426384

#xgboost
x_train=t1 %>% select(-Price)
y_train=t1$Price
x_test=t2 %>% select(-Price)
xgb.fit=xgboost(data=data.matrix(x_train),
                label = y_train,
                objective='reg:linear',
                verbose=1,
                nrounds = 10)

test.predicted=predict(xgb.fit,data.matrix(x_test))
(test.predicted-t2$Price)**2 %>% mean() %>% sqrt()

#RMSE=351275

actualTest=predict(xgb.fit, data.matrix(testD_new))
write.csv(actualTest,"submision1.csv",row.names = F)

