#Cargamos los datos que buscamos ajustar.

X = c(18, 13 ,18 ,15 ,10 ,12 ,8 ,4, 7, 3)
Y = c(23, 20, 18, 16, 14, 11 ,10 ,7 ,6 ,4 )
datos = data.frame(Y, X)
n=dim(datos)[1]
datos

#Hacemos la regresión lineal simple 
model = lm(Y ~ X, datos)
r=summary(model)$r.squared
summary(model)

  ################ R-Cuadrado:  ##################
#Calculamos el coeficiente de determinación R-Cuadrado
print(r)
sqrt_r=sqrt(r)
sqrt_r

#y calculamos el coeficiente de correlación de Pearson r
cor(Y,X)

#Calculamos un intervalo de confianza y de predicción del modelo para una X = 12
predict(model,data.frame(X=12), interval = "confidence")
predict(model,data.frame(X=12), interval = "predict")

#Finalmente graficamos.
nuevas.x=data.frame(X=seq(0,25,by=25/(n)))
ic=predict(model, nuevas.x,interval = "confidence")
ip=predict(model, nuevas.x,interval = "prediction")
plot(Y,X,pch = 19, frame = TRUE,grid()) +abline(lm(Y ~ X,datos),col="red")+lines(nuevas.x$X,ic[,2],lty=2)+lines(nuevas.x$X,ic[,3],lty=2,col="black")+lines(nuevas.x$X,ip[,2],lty=2)+lines(nuevas.x$X,ip[,3],lty=2)
