library(ggplot2)
library(dplyr)
library(moments)
library(Hmisc)
data(diamonds)

str(diamonds)
dim(diamonds)
summary(diamonds)

#variables continuas:
#7: price, carat, x,y,z,depth (2 * z / (x + y) (43--79)) y table 
#variables categóricas
#3: cut (ordinal), color (ordinal), clarity (ordinal)

#frecuencias
par(mfrow=c(1,3))
barplot(table(diamonds$cut),xlab="Calidad del corte", 
        ylab="% Frecuencia",col = 484,lwd = 2,cex.names =.8, 
        cex.axis = .7, ylim=c(0,25000))

barplot(table(diamonds$color),xlab="Color del diamante", 
        ylab="% Frecuencia",col = 321,lwd = 2,cex.names =.8, 
        cex.axis = .7, ylim=c(0,25000))

barplot(table(diamonds$clarity),xlab="Claridad del diamante", 
        ylab="% Frecuencia",col = 123,lwd = 2,cex.names =.8, 
        cex.axis = .7, ylim=c(0,25000))


names(table(diamonds$cut))

data.frame(table(diamonds$cut))

pairs(diamonds[,c("price","carat","depth")])
pairs(diamonds[,c("price","x","y","z")])
pairs(diamonds[,c("depth","table","x","y","z")])

par(mfrow=c(1,3))
hist(diamonds$price, col="red", main="Precio",freq=T,ylim=c(0,18000),breaks=40, xlab="rango de precios",ylab="Freuencia")
hist(diamonds$carat, col=981, main="Carat",freq=T,ylim=c(0,18000),breaks=40, xlab="rango de carat",ylab="Freuencia")
hist(diamonds$depth, col =654, main="Profundidad",freq=T,ylim=c(0,18000),breaks=40,xlab="rango de carat",ylab="Freuencia")

par(mfrow=c(1,3))
hist(diamonds$price, col="red", main="Precio",freq=F,breaks=40, xlab="rango de precios",ylab="Frecuencia Relativa")
curve(dnorm(x, mean=mean(diamonds$price), sd=sd(diamonds$price)), add=TRUE)
hist(diamonds$carat, col=981, main="Carat",freq=F,breaks=40, xlab="rango de carat",ylab="Frecuencia Relativa")
curve(dnorm(x, mean=mean(diamonds$carat), sd=sd(diamonds$carat)), add=TRUE)
hist(diamonds$depth, col =654, main="Profundidad",freq=F,breaks=40,xlab="rango de carat",ylab="Frecuencia Relativa")
curve(dnorm(x, mean=mean(diamonds$depth), sd=sd(diamonds$depth)), add=TRUE)


diamonds %>% summarise(media = round(mean(price),2),
                       min = round(min(price),2),
                       p25 = round(quantile(price,.25),2),
                       mediana = round(median(price),2),
                       p75 = round(quantile(price,.75),2),
                       max = round(max(price),2),
                       recorrido = max - min,
                       RI = p75 - p25,
                       Desv.Est = round(sd(price),2),
                       CV = round(Desv.Est/abs(media),2),
                       Simetria = round(skewness(price),2),
                       Curtosis = round(kurtosis(price),2))

??kable
mode(diamonds$price)

#ANEXOS

par(mfrow=c(2,2))
hist(diamonds$x,main="x",freq=F)
curve(dnorm(x, mean=mean(diamonds$x), sd=sd(diamonds$x)), add=TRUE)
hist(diamonds$y,main="y")
hist(diamonds$z,main="z")
hist(diamonds$table,main="table")


cut_fair <- diamonds$carat[diamonds$cut=="Fair"]
cut_good <- diamonds$carat[diamonds$cut=="Good"]
t.test(cut_fair,cut_good, var.equal=FALSE, paired=FALSE,alternative="g")


par(mfrow=c(1,3))
boxplot(price ~ cut,data=diamonds,main="Precio contra corte",ylab="Precio",xlab="Cut",col=484)
boxplot(price ~ color,data=diamonds,main="Precio contra color",ylab="Precio",xlab="Color",col=621)
boxplot(price ~ clarity,data=diamonds,main="Precio contra claridad",ylab="Precio",xlab="Claridad",col=123)

cor(diamonds[,c("price","carat","depth")])

kable(cor(diamonds[,c("price","carat","depth")]))

cor(diamonds[,c("cut","clarity")],method="spearman")

class(diamonds$cut)
class(diamonds$clarity)
class(diamonds$color)

price_g <- cut2(diamonds$price,g=9)
carat_g <- cut2(diamonds$carat,g=9)
depth_g <- cut2(diamonds$depth,g=9)

var1 <- cbind(as.numeric(diamonds$cut),as.numeric(diamonds$clarity),as.numeric(diamonds$color))
colnames(var1) <- c("cut","clarity","color")
cor(cbind(var1,price_g,carat_g,depth_g),method="spearman")
