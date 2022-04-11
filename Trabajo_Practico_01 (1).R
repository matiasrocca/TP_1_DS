rm(list=ls())
setwd() # Definir directorio de trabajo.

#~~ Introducción a Ciencia de Datos: Taller 01 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Fuente de los datos: http://stat-computing.org/dataexpo/2009/the-data.html

## Ejercicio 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
vuelos_01_12 <- read.table("vuelos_01_12.txt", header = TRUE,
                                   sep = ";")

vuelos_01_12$UniqueCarrier = as.factor(vuelos_01_12$UniqueCarrier)
vuelos_01_12$TailNum=as.factor(vuelos_01_12$TailNum)
vuelos_01_12$Origin=as.factor(vuelos_01_12$Origin)
vuelos_01_12$Dest=as.factor(vuelos_01_12$Dest)
vuelos_01_12$CancellationCode=as.factor(vuelos_01_12$CancellationCode)

## Ejercicio 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dim(vuelos_01_12)
nrow(vuelos_01_12)
"Como indica el primer numero de la función dim o la función nrow,
el dataframe tiene 3318080 vuelos"

## Ejercicio 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dim(vuelos_01_12)
ncol(vuelos_01_12)
"RTA:Como indica el segundo numero de la función dim o la función ncol,
el dataframe tiene 28 variables"

## Ejercicio 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
"Para ver todas las unidades de analisis que poseen más de 20' de demora usamos:" 
vuelos_01_12[vuelos_01_12$ArrDelay>20,]
vuelos_01_12$ArrDelay>20 
"con esa línea podemos identificar de forma booleana quienes cumplen o no 
esa condición. Aquellos que son mayores a 20 tendrán el valor true"

"Por lo tanto generamos una tabla cruzada que muestre 
cuantos vuelos tiene cada aeorolinea con demora y cuantos no 
(es decir cuales cumple o no esa condición, respectivamente):"
aerolinea_dem <- table(aerolinea=vuelos_01_12$UniqueCarrier, demora=vuelos_01_12$ArrDelay > 20)
aerolinea_dem

"finalmente, escribimos de forma relativa a los aerolineas
cuales tienen más o menos retraso"
prop.table(aerolinea_dem, margin=1)
#RTA 2,1: La aerolínea con mayor numero de vuelos demorados es AA.

#Para resolver la segunda pregunta (2,2) hay que seguir un proceso similar
aeropuerto_dem= table(aerop_lleg=vuelos_01_12$Dest, demora=vuelos_01_12$ArrDelay>20)
aeropuerto_dem
prop.table(aeropuerto_dem, 1)

"como esta segunda requiere un máximo donde existen muchos aeropuertos,
es cómodo verlo asi:"
which.max(prop.table(aeropuerto_dem, 1)[,2])
"RTA 2,2: El  aeropuerto de destino en el que es más común (en términos relativos)
que haya demoras en el arribo es OTH"

## Ejercicio 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
"Lo primero que hacemos es crear una variable booleana para Delays (retraso 
true o no) y otra para Fin de semana (fin de semana true o no)"
vuelos_01_12$Delay=ifelse(vuelos_01_12$ArrDelay>20, TRUE, FALSE)
vuelos_01_12$EsFinDeSemana=ifelse(vuelos_01_12$DayOfWeek>=6, TRUE, FALSE)
"Con esas variables, resulta mas simple verificar si hay mayor vuelos 
relativo a si es un día de semana o no:"
prop.table(table(Dia=vuelos_01_12$EsFinDeSemana, Demora=vuelos_01_12$Delay),1)

"RTA: Encontramos que los dias de semana hay mayor demora relativamente."

## Ejercicio 6 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
mean(vuelos_01_12$WeatherDelay[vuelos_01_12$WeatherDelay!=0], na.rm = T)

"RTA: Para los casos en los que hay demora (es decir: casos registrados
 y mayor a 0), la demora es de aproximadamente 41 minutos."

## Ejercicio 7 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
which.max(vuelos_01_12$ActualElapsedTime) #Nos da el vuelo que más tiempo estuvo en el aire
vuelos_01_12[which.max(vuelos_01_12$ActualElapsedTime),"FlightNum"]

#RTA: El vuelo número 5 fue el que más tiempo estuvo en el aire

## Ejercicio 8 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
origen_dest= table(origen = vuelos_01_12$Origin, destino = vuelos_01_12$Dest)
head(origen_dest, 3)

## Ejercicio 9 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
origen_destino= as.data.frame(origen_dest)
rm(origen_dest)
head(origen_destino, 3)

"En este caso, la función as.data.frame lo que hizo fue agregar una 
columna de frecuencias en la cual dice el valor absoluto de vuelos que 
generaron dicho viaje. Además, al verlo como dataframe, las filas se vuelven 
recorridos de origen-destino. Nuestra Unidad de analisis pasó de ser Origen
a ser Origen-Destino. Una vez que se creó el dataframe, R permite eliminar 
la matriz anterior y aun así no perder los datos."

"RTA: Para un dataframe más largo puede resultar incomodo y
poco natural trabajar con la variable origen_destino. Por eso podríamos usar 
la función as.data.frame.matrix() para visualizarlo tal y como si fuese una 
matriz." #origen_destino= as.data.frame.matrix(origen_dest)

## Ejercicio 10 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
which.max(origen_destino$Freq)
origen_destino[which.max(origen_destino$Freq),]
viaje_más_comun= origen_destino[which.max(origen_destino$Freq),] 
#El viaje más común es LAX-SAN, con un número de 11256 vuelos realizados.

## Ejercicio 11 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Lo primero que hacemos es identificar las tres restricciones:
vuelos_01_12$Origin== viaje_más_comun[,"origen"]
vuelos_01_12$Dest== viaje_más_comun[,"destino"]
vuelos_01_12$Cancelled==0 #Es decir vuelos NO cancelados

#Sucesivamente, creamos una variable con dichas restricciones
MasFreqNoCancelado= vuelos_01_12[vuelos_01_12$Cancelled==0 & 
                                vuelos_01_12$Origin==viaje_más_comun$origen &
                                vuelos_01_12$Dest==viaje_más_comun$destino,]
#Finalmente realizamos el boxplot de lo que se nos pide
boxplot(MasFreqNoCancelado$ActualElapsedTime,horizontal = T)

"RTA: Los Outliers pueden pueden deberse a distintas razones. Con el dataframe
inicial que poseemos, podríamos decir que se debe a factores externos, como el
clima (WeatherDelay). Además podríamos pensar que el trafico aereo es muy 
concurrido y eso genera retraso."

## Ejercicio 12 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
boxplot(MasFreqNoCancelado$ActualElapsedTime
        ~ MasFreqNoCancelado$EsFinDeSemana, horizontal=T)

"RTA: Dados los boxplot de ambas figuras, podemos afirmar que la distribución
para fines de semana y para días de semana del vuelo más común es similar."

## Ejercicio 13 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
vuelos_01_12$Month= as.factor(vuelos_01_12$Month)
levels(vuelos_01_12$Month)##Estas líneas nos dan los meses del año
"Pero para poder contabilizar los vuelos, necesitamos pasar de variables booleanas
a valores absolutos de vuelos el numero de vuelos demorados. Para ellos necesitamos:"
a = vuelos_01_12$ArrDelay>0
table(vuelos_01_12$Month, a)
b= vuelos_01_12$DepDelay>0
table(vuelos_01_12$Month, b)

"Con esa información, obtuvimos medidas cuantitativas numéricas (integer) de los
vuelos que estuvieron demorados en la llegada (c) y/o en la salida (d):"
c=table(vuelos_01_12$Month, a)[,2]
d=table(vuelos_01_12$Month, b)[,2]

#Con ellas ya podemos graficar:
plot(levels(vuelos_01_12$Month), c, main= "Demoras en salida y arribos",
     type="b", pch=19, col="red", xlab="Mes", ylab="Vuelos")
#Agregamos la segunda linea:
lines(levels(vuelos_01_12$Month), d, pch=18, col="blue", type="b", lty=2)
#Agregamos las etiquetas:
legend(2, 105000, legend=c("Arrivals", "Departure"),
       col=c("red", "blue"), lty=1:2, cex=0.8)