#Cargamos los datos a utilizar.
y = c(61.6,53.2,65.5,64.9,72.7,52.2,50.2,44,53.8,53.5)
x1 = c(6,4.4,9.1,8.1,9.7,4.8,7.6,4.4,9.1,6.7)
x2 = c(6.3,5.5,3.6,5.8,6.8,7.9,4.2,6,2.8,6.7)
datos=data.frame(y,x1,x2)

#Hacemos el ajuste lineal múltiple
model = lm(y ~ x1 + x2, datos)
summary(model)

#Obtenemos "Multiple R-square=  0.6682".

