######## Instalar la paqueteria "datos"
install.packages ("datos")

######## Abrir la libreria
library (datos)

######## Elegir una matriz
#****Leer las bases que estan contenidas en la libreria
#datos::
#*** Seleccionar la matriz de datos (flores)
x<- datos::flores

#######Pasos para explorar la matriz
#Dimensión
dim (x)
#Variables
str (x)
#Nombre de las variables
colnames(x)
#Datos nulos
anyNA(x)

####Filtrado de las variables (solo necesitamos las cuantitativas)
#Filtramos la matriz por especie Versicolor
X<-x[51:100,1:4]
####Desarrollamos el PCA por pasos
#1.- Trasformamos la matriz en un data frame
X<-as.data.frame(X)
#2.- Definir n (individuos) y p (variables)
dim(X)

n<-dim(X)[1]
p<-dim(X)[2]
#3.- Generacion de un scatterplot de las variables originales
pairs(X,col="darkviolet", pch=19, 
      main="Variables Originales")
#4.- Obtencion de la media por columnas y la matriz de covarianza muestral
#Media
mu<-colMeans(X)
mu
#Covarianza
s<-cov(X)
s
#5.- Obtencion de los valores-vectores propios desde la matriz de covarianza muestral
es<-eigen(s)
es
#5.1.- Matriz de valores propios
eigen.val<-es$values
eigen.val

#5.2.- Matriz de vectores propios
eigen.vec<-es$vectors

#6.- Proporcion de variabilidad para cada valor
#6.1.- Proporcion de variabilidad de valores propios
pro.var<-eigen.val/sum(eigen.val)

#6.2.- Proporcion de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)

# 7.-Construccion del primer y segundo componente con las variables originales
#Obtencion de los componentes principales con
# base en la matriz de correlaciones muestrales

R<-cor(X)

# 8.-Obtencion de valores-vectores propios de la 
#matriz de correlaciones.
eR<-eigen(R)
eR
# 9.- Separacion de valores propios desde la matriz de correlaciones

# 9.1.- Obtencion de valores propios
eigen.val.R<-eR$values

# 9.2.- Obtencion de vectores propios
eigen.vec.R<-eR$vectors

# 10.-Calculo de la proporcion variabilidad
# 10.1.- Variabilidad para valores propios
pro.var.R<-eigen.val/sum(eigen.val.R)

# 10.2.- Proporcion de variabilidad acumulada de valores propios
pro.var.acum<-cumsum(eigen.val.R)/sum(eigen.val.R)

# 11.-Media de los valores propios
mean(eigen.val.R)

#Obtencion de los coeficientes

# 12.- Centrar los datos con respecto a la media
# 12.1.- Matriz uno
ones<-matrix(rep(1,n),nrow=n, ncol=1)

# 12.2.- Construccion de la matriz centrada

X.cen<-as.matrix(X[,-5])-mu
X.cen

# 13.- Construccion de la matriz diagonal de las covarianzas
Dx<-diag(diag(s))
Dx

# 14.- Construccion de la matriz centrada multiplicada por Dx^1/2

Y<-X.cen%*%solve(Dx)^(1/2)
Y #datos normalizados

# 15.- Construccion de los coeficientes o scores
# eigen.vec matriz de autovectores
scores<-Y%*%eigen.vec

#16.- Nombramos las columnas PC1...PC4
colnames(scores)<-c("PC1","PC2","PC3","PC4")

#17.- Visualizamos los datos
scores

#18.- Generacion del grafico de los scores
pairs(scores, main="scores", col="deeppink4", pch=19)



# ACP VÍA SINTETIZADA

#1.- Cálculo de la varianza a las columnas (1=filas, 2=columnas)

apply(X[,-5],2, var)

#2.- Centrado por la media y escalada por la desviacion standar (dividir entre sd).

acp<-prcomp(X[,-5], center=TRUE, scale=TRUE)
acp

#3.- Generación del gráfico screeplot
plot(acp, type="l")

#4.- Visualizar el resumen
summary(acp)
