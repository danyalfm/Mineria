library(randomForest)
library(caret)

df=read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/soybean/soybean-large.data"),sep = ",", header = FALSE)
names(df)<-c("Class", "Date", "PlantStand", "Precip", "Temp", "Hail", "CropHist", "AreaDamaged", "Severity", "SeedTmt", "Germination", "PlantGrowth", "Leaves", "LeafspotsHalo", "LeafspotsMarg", "LeafspotSize" , "LeafspotShread", "LeafMalf" , "LeafShread", "LeadMild", "Stem", "Lodging", "StemCrankers", "CrankerLesion", "FruitingBodies", "ExternalDecay", "Mycelium", "IntDiscolor", "Sclerotia", "FruitPods", "FruitSpots", "Seed", "MoldGrowth", "SeedSize", "Shriveling", "Roots")

getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df$Date[which(df$Date=="?")] <- 5
df$PlantStand[which(df$PlantStand=="?")] <-0
df$Precip[which(df$Precip=="?")] <-2
df$Temp[which(df$Temp=="?")] <-1
df$Hail[which(df$Hail=="?")] <-0
df$CropHist[which(df$CropHist=="?")] <-2
df$AreaDamaged[which(df$AreaDamaged=="?")] <-1
df$Severity[which(df$Severity=="?")] <-1
df$SeedTmt[which(df$SeedTmt=="?")] <-0
df$Germination[which(df$Germination=="?")] <-1
df$PlantGrowth[which(df$PlantGrowth=="?")] <-0
df$Leaves[which(df$Leaves=="?")] <-1
df$LeafspotsHalo[which(df$LeafspotsHalo=="?")] <-2
df$LeafspotsMarg[which(df$LeafspotsMarg=="?")] <-0
df$LeafspotSize[which(df$LeafspotSize=="?")] <-1
df$LeafspotShread[which(df$LeafspotShread=="?")] <-0
df$LeafMalf[which(df$LeafMalf=="?")] <-0
df$LeafShread[which(df$LeafShread=="?")] <-0
df$LeadMild[which(df$LeadMild=="?")] <-1
df$Stem[which(df$Stem=="?")] <-0
df$Lodging[which(df$Lodging=="?")] <-0
df$StemCrankers[which(df$StemCrankers=="?")] <-0 - 
df$CrankerLesion[which(df$CrankerLesion=="?")] <-0
df$FruitingBodies[which(df$FruitingBodies=="?")] <-0
df$ExternalDecay[which(df$ExternalDecay=="?")] <-0
df$Mycelium[which(df$Mycelium=="?")] <-0
df$IntDiscolor[which(df$IntDiscolor=="?")] <-0
df$Sclerotia[which(df$Sclerotia=="?")] <-0
df$FruitPods[which(df$FruitPods=="?")] <-0
df$FruitSpots[which(df$FruitSpots=="?")] <-0
df$Seed[which(df$Seed=="?")] <-0
df$MoldGrowth[which(df$MoldGrowth=="?")] <-0
df$SeedSize[which(df$SeedSize=="?")] <-0
df$Shriveling[which(df$Shriveling=="?")] <-0
df$Roots[which(df$Roots=="?")] <-0

df$Date[is.na(df$Date)] <- 5
df$PlantStand[is.na(df$PlantStandis.na)] <-0
df$Precip[is.na(df$Precip)] <-2
df$Temp[is.na(df$Temp)] <-1
df$Hail[is.na(df$Hail)] <-0
df$CropHist[is.na(df$CropHist)] <-2
df$AreaDamaged[is.na(df$AreaDamaged)] <-1
df$Severity[is.na(df$Severity)] <-1
df$SeedTmt[is.na(df$SeedTmt)] <-0
df$Germination[is.na(df$Germination)] <-1
df$PlantGrowth[is.na(df$PlantGrowth)] <-0
df$Leaves[is.na(df$Leaves)] <-1
df$LeafspotsHalo[is.na(df$LeafspotsHalo)] <-2
df$LeafspotsMarg[is.na(df$LeafspotsMarg)] <-0
df$LeafspotSize[is.na(df$LeafspotSize)] <-1
df$LeafspotShread[is.na(df$LeafspotShread)] <-0
df$LeafMalf[is.na(df$LeafMalf)] <-0
df$LeafShread[is.na(df$LeafShread)] <-0
df$LeadMild[is.na(df$LeadMild)] <-1
df$Stem[is.na(df$Stem)] <-0
df$Lodging[is.na(df$Lodging)] <-0
df$StemCrankers[is.na(df$StemCrankers)] <-0 
df$CrankerLesion[is.na(df$CrankerLesion)] <-0
df$FruitingBodies[is.na(df$FruitingBodies)] <-0
df$ExternalDecay[is.na(df$ExternalDecay)] <-0
df$Mycelium[is.na(df$Mycelium)] <-0
df$IntDiscolor[is.na(df$IntDiscolor)] <-0
df$Sclerotia[is.na(df$Sclerotia)] <-0
df$FruitPods[is.na(df$FruitPods)] <-0
df$FruitSpots[is.na(df$FruitSpots)] <-0
df$Seed[is.na(df$Seed)] <-0
df$MoldGrowth[is.na(df$MoldGrowth)] <-0
df$SeedSize[is.na(df$SeedSize)] <-0
df$Shriveling[is.na(df$Shriveling)] <-0
df$Roots[is.na(df$Roots)] <- 0

set.seed(7)

df2=data.matrix(df)
dfs <- as.data.frame(df2)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(dfs[,2:36], dfs[,1], sizes=c(2:36), rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"))

#Eliminar atributos que no se necesiten
dfs$PlantStand<-NULL
dfs$Hail <-NULL
dfs$CropHist<-NULL
dfs$Severity<-NULL
dfs$SeedTmt<-NULL
dfs$Germination<-NULL
dfs$Leaves<-NULL
dfs$LeafMalf<-NULL
dfs$Stem<-NULL
dfs$ExternalDecay<-NULL
dfs$IntDiscolor<-NULL
dfs$Shriveling<-NULL
dfs$SeedSize<-NULL
dfs$MoldGrowth<-NULL

#Particionamiento
index=createDataPartition(dfs$Class,p=0.8, list = FALSE)
train=dfs[index, ];
test=dfs[-index, ]

train$Class= as.factor(train$Class)
modelo=randomForest(Class~., data=train, importance=TRUE)

