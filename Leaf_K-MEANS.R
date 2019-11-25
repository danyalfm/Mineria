library(MASS)
library(cluster)
library(caret)

df = train

datos_scal = as.data.frame(scale(df[3:194]))

df2=data.matrix(df)
dfl <- as.data.frame(df2)

#Clusters
set.seed(1)
k = 99
datos_km <- kmeans(datos_scal,k) #centers = Numero de Clusters
names(datos_km)
train_preds <- predict(datos_km, train$species)

#asignacion de observaciones al cluster
datos_km$cluster
#inercia total
datos_km$totss
#inercia inter grupos (Mientras mas Alta mejor)
datos_km$betweenss
#inercia intra grupos
datos_km$withinss
#inercia intra grupos total (Mientras mas Baja Mejor)
datos_km$tot.withinss 

#Determinar el numero de clusters optimo
submt <- kmeans(datos_scal, centers = 1)$betweenss
for(i in 2:10) submt[i] <- kmeans(datos_scal,center = i)$betweenss

plot(1:10, submt, type = "b", xlab = "numero de clusters", ylab = "Suma de cuadrados intergrupos")

#calidad de la agrupacion
IB = datos_km$betweenss/datos_km$totss
#Representacion por pares de atributos
plot(datos_scal$margin1,dfl$margin2, col=datos_km$cluster, xlab = "Margin1",ylab = "Margin2")
pairs(datos_scal, col=datos_km$cluster, las=1, main=paste("Leaf DataSet\nK-means, k=",k,",IB=",round(IB,4)),font.main=4)

#Segmentacion
kS <- kmeans(datos_scal,k,iter.max=100)
kS$centers
table(kS$cluster)


#Crear grafico con distribucion de todos los clusters
center<-kS$centers
mini <- apply(datos_scal,2,min)
maxi <- apply(datos_scal,2,max)
maxi <- (as.numeric(maxi))
mini <- (as.numeric(mini))
centersde <- t(mini+t(center)*(maxi-mini))
data_centers <- data.frame(centersde)
parcoord(data_centers,col=1:4,var.label = TRUE)
par(xpd = TRUE)

clusplot(datos_scal,datos_km$cluster,color=T, Labels=0, lines=0)

