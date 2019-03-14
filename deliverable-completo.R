---
title: "Deliverable 1"
author: "Guillem Valls, Sergio Mazzariol"
output:
  word_document:
    toc: yes
    toc_depth: '3'
  pdf_document:
    number_sections: yes
    toc: yes
    toc_depth: '3'
  html_document:
    number_sections: yes
    toc: yes
    toc_depth: 3
editor_options:
  chunk_output_type: console
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Loading packages

```{r, include=FALSE}
# Load Required Packages: to be increased over the course

requiredPackages <- c("mvoutlier","chemometrics","mice","missForest","missMDA","DMwR","pbkrtest","jomo","readxl","haven","sf","rgdal","missMDA","effects","FactoMineR","car","factoextra","RColorBrewer","ggplot2","dplyr","data.table", "ggmap","ggthemes","knitr","MVA")
missingPackages <- requiredPackages[!(requiredPackages %in% as.vector(installed.packages(lib.loc="~/R/win-library/3.5")[,"Package"]))]

if(length(missingPackages)) install.packages(missingPackages)
#install.packages(requiredPackages,dependencies = TRUE,repos = c("http://rstudio.org/_packages","http://cran.rstudio.com"))
lapply(requiredPackages, require, character.only = TRUE)

```

# Loading data

Establecemos el directorio de trabajo, luego importamos todos los datos del archivo csv bank-additional-full, establecemos una semilla para obtener siempre la misma muestra "aleatoria", obtenemos 5000 individuos que se usarán para el análisis a lo largo de toda la asignatura.


```{r}
setwd("C:/Users/Sergio/Dropbox/UPC/FIB/Analisis de datos y explotacion de la informacion (ADEI)/FIB-ADEI-Big-Data-Analysis")
#setwd("C:/Users/usuario/Documents/ADEI/FIB-ADEI-Big-Data-Analysis")

# Data file alread
df<-read.table('bank-additional-full.csv',header=TRUE,sep=";")

# Select your 5000 register sample (random sample)
set.seed(19101990)
llista<-sample(size=5000,x=1:nrow(df),replace=FALSE)
llista<-sort(llista)

#llista
df<-df[llista,]
dim(df)
#save.image("set-datos.RData")
```

# Inicializamos datos y funciones
Creamos un dataframe que llamamos data quality report "dqr" para almacenar missings, errors, outliers. También creamos uno para los datos individuales "dqri". Inicializamos el "dqr" todo a 0, y el dqri lo Inicializamos a 0 pero despues de eliminar los individuos que nos dan outliers o errores en las variables target.
Declaramos la función calcQ que nos permitirá discriminar los outliers leves y severos en los boxplots.
Para poder tratar los datos con mayor facilidad separamos las variables en tres grupos, las variables target "duration, y", las variables categoricas "job", "marital", "education", "default", "housing", "loan", "contact", "month", "day_of_week", "poutcome" y las variables númericas "age", "campaign", "pdays", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed"


```{r}
dqr <- data.frame(variable=character(), missings=integer(), errors=integer(), outliers=integer())
dqr[length(names(df)),]<-0
dqr$variable <-names(df)
dqr[,2:4]<-0

dqri <- data.frame(missings=integer(), errors=integer(), outliers=integer())

calcQ <- function(x) {
  s.x <- summary(x)
  iqr<-s.x[5]-s.x[2]
  list(souti=s.x[2]-3*iqr, mouti=s.x[2]-1.5*iqr, min=s.x[1], q1=s.x[2], q2=s.x[3], 
       q3=s.x[5], max=s.x[6], mouts=s.x[5]+1.5*iqr, souts=s.x[5]+3*iqr ) }

df[1,]
vars_target<-c("duration","y");vars_target
vars_cat<-c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "day_of_week", "poutcome");vars_cat
vars_num<-c("age", "campaign", "pdays", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed");vars_num

```

# Step 1:
## Analisis y exploración de datos

  Empezamos con la exploración de datos, verificamos los nombres de las variables, tambien un summary para comprobar que los datos son correctos.
```{r}
summary(df)
```
## Variables target
  En primer lugar trataremos las variables target porque de estas se puden desprender errores y outliers que implicaran eliminación de individuos ya que estos errores no pueden imputarse, seria falsificación de la variable target. Tenemos dos variables targets, una categorica y otra númerica, empezamos con la categorica.

### Tratamiento de la variable target Y
  Hacemos un summary de la variable y podemos ver que los unicos valores que toma es yes o no, de los cuales podemos decir que no hay errores, outliers o missings.
```{r}
summary(df$y)
```

### Tratamiento de la variable númerica target duration
Vemos que hay valores muy pequeños, incluso 0, tambien valores muy grandes. Miramos distribución
Hacemos boxplot para ver outliers y solo se contemplan outliers superiores con la función calc fijamos límite soft y fijamos límite extremo, revisamos los 10 valores más extremos de los cuales consideramos que los  últimos 6 son outliers. Hacemos boxplot nuevamente para ver el nuevo resultado el cual almacenamos en nuestro data frame.
 Luego procedemos a revisar los errores, los cuales consideramos que pueden ser llamadas con una duración inferior a 5 segundos.
 Tanto errores como outliers son eliminados de la muestra.
```{r}
summary(df$duration)

hist(df$duration,100) 
par(mfrow=c(1,2))   
boxplot(df$duration) 
aux<-calcQ(df$duration)
abline(h=aux[8],col="red",lwd=2) 
abline(h=aux[9],col="magenta",lwd=2) 
aux<-order(df$duration,decreasing=TRUE)[1:10];df[aux,'duration'] 
df<-df[-aux[1:6],]
boxplot(df$duration) 
par(mfrow=c(1,1))   

aux<-which(df$duration<5);length(aux);df[aux,'duration'] 
df<-df[-aux,] 

dqr[dqr$variable=="duration","outliers"]<-6
dqr[dqr$variable=="duration","errors"]<-length(aux)

# Inicializamos el dqri ya que en este punto hemos eliminado todos los individuos que se consideraban como outliers o errores en las variables target.
dqri[nrow(df),]<-0
dqri[,]<-0
```

## No-Target + Categorica
  Primero realizamos un summary de todas las variables categoricas, para analizar sus valores. En este analisis podemos ver que la variable default tiene una cantidad alta de valores unknows, por lo que nos da indicios de que esta variable no nos será útil. 
  Vemos que todas los factores con nieveles unknow, menos "default" se pueden considerar como missings, por lo que procedemos a pasar estos valores a NA´s, para esto utilizamos un bucle for. Para evitar realizar el cambio de variables que tengan una cantidad de uknowns mayor a 300, ya que en estos casos debe permanecer como un nivel más, como lo es en el caso de la variable "default".

```{r}
for(i in vars_cat){
  cat("################ ",i," ##################\n")
  print(summary(df[,i]))
}

for(i in vars_cat){
  aux<-which(df[,i]=="unknown")
  if(length(aux)>0 && length(aux)<300){ # Solo si como máximo la variable tiene 300 unknowns (Para filtrar a default)
    cat(i, " -- ", length(aux), "\n")
    df[aux,i]<-NA
    dqri[aux,"missings"]<-dqri[aux,"missings"]+1
    df[,i]<-factor(df[,i])
  }
}

# Para el data analisis guardamos los missings de las variables categoricas
for(i in vars_cat){ 
  dqr[dqr$variable==i,"missings"]<-sum(is.na(df[,i]))
}
```
Ahora realizamos la imputación de las variables categóricas.
Contrastamos las summmaries originales e imputados, para comprobar que la imputación se hizo correctamente. Por lo que vemos que todo ha sido correcto y aceptamos estos datos, por lo que procedemos a almacenarlo en nuestro data frame, por seguridad solo sobreescribimos aquellas variables que han sido modificadas.


```{r}
aux2<-imputeMCA(df[,vars_cat],ncp=10)

for(i in vars_cat){ 
  cat("################ ",i," ##################\n")
  print(summary(df[,i]))
  print("--- --- --- ---")
  print(summary(aux2$completeObs[,i]))
}

no_imputadas<-c("poutcome","day_of_week","month","contact","default")
df[,setdiff(vars_cat,no_imputadas)]<-aux2$completeObs[,setdiff(vars_cat,no_imputadas)]

```

# Step 2: 
## Refactorización

Agrupamos subcategorias en menos categorias. En el resumen anterior de las variables categoricas, podemos ver como reagruparlas.
En jobs realizamos la agrupación en función del posible ingreso monetario. Finalmente vemos la reagrupación final la cual no ha quedado uniformemente distribuida pero sin embargo, los grupos tienen una relación más significativa.

```{r}
# Job

table(df$job)

df$f.job <- 4
# 1 level - Admin-Managment
aux<-which(df$job %in% c("admin.", "management"))
df$f.job[aux] <-1

# 2 level - Entrep-Retired-selfEmpl
aux<-which(df$job %in% c("entrepreneur", "retired", "self-employed"))
df$f.job[aux] <-2

# 3 level - Not working
aux<-which(df$job %in% c("housemaid","unemployed","student"))
df$f.job[aux] <-3

# 4 level - Serv-Tech-BlueC
aux<-which(df$job %in% c("services","technician","blue-collar"))
df$f.job[aux] <-4

df$f.job<-factor(df$f.job,levels=1:4,labels=c("Admin-Managment", "Entrep-Retired-selfEmpl", "Not-working", "Serv-Tech-BlueC"))
levels(df$f.job)<-paste0("f.job.",levels(df$f.job))
summary(df$f.job)
```

En months realizamos la agrupación en función de las temporadas aunque no tan estrictamente. 

```{r}
# Months to groups
table(df$month)

df$f.season <- 3
# 1 level - mar-may 
aux<-which(df$month %in% c("mar","apr","may"))
df$f.season[aux] <-1

# 2 level - jun-ago
aux<-which(df$month %in% c("jun","jul","aug"))
df$f.season[aux] <-2

# 3 level - aug-feb
aux<-which(df$month %in% c("dec","sep","oct","nov"))
df$f.season[aux] <-3

summary(df$f.season)
df$f.season<-factor(df$f.season,levels=1:3,labels=c("Mar-May","Jun-Aug","Sep-Dec"))
levels(df$f.season)<-paste0("f.season.",levels(df$f.season)) # Hacemos las etiquetas más informativas
summary(df$f.season)
```

En Education realizamos la agrupación en función del nivel de estudio de cada individuo. Hemos puesto la categoria illiterate dentro de la que consideramos los estudios eran inferiores. Al realizar la agrupación los factoes quedaron realativamente bien equilibrados.

```{r}
#Education
table(df$education)

df$f.education <- 3
# 1 level - Basic 
aux<-which(df$education %in% c("illiterate","basic.4y","basic.6y","basic.9y"))
df$f.education[aux] <-1

# 2 level - Higb School 
aux<-which(df$education %in% c("high.school"))
df$f.education[aux] <-2

# 3 level - Professional
aux<-which(df$education %in% c("professional.course","university.degree"))
df$f.education[aux] <-3

table(df$f.education);summary(df$f.education)
df$f.education<-factor(df$f.education,levels=1:3,labels=c("Basic","High School","Professional"))
```

# Step 3: 
  ## No-Target+Numerica

    ### age
     Consideramos que no presenta ningún outlier, ya que las edades comprendidas entre 18 y 92 años, son considerados normal.

    ### campaing
    Consideramos que en los 10 meses que dura la campaña, un máximo de 20 contactos es aceptable puesto que eso implica una media de un contacto cada 15 días.

```{r}
  # campaign
  summary(df$campaign)
  hist(df$campaign,col="cyan",main="campaign - Histogram")
  par(mfrow=c(1,2)) 
  boxplot(df$campaign, labels=row.names(df))
  aux<-calcQ(df$campaign);
  abline(h=aux[8],col="red",lwd=2) 
  abline(h=aux[9],col="magenta",lwd=2) 
  aux<-which(df$campaign<=0);aux  # Si se incluye el último contacto, este valor no puede ser 0
  aux<-which(df$campaign>20);length(aux);df[aux,'campaign']
  df[aux,"campaign"]<-NA 
  boxplot(df$campaign)
  par(mfrow=c(1,1)) 
  
  # Para el data analisis guardamos los missings
  dqr[dqr$variable=='campaing','missings']<-sum(is.na(df[,"campaign"]))
  # Para los individuales
  dqri[aux,'outliers']<-dqri[aux,'outliers']+1
```

## Verificamos si hay algun error o outlier en pdays  
Para pdays/previous/outcome creemos que estan relacionadas, por lo que podemos detectar errores si encontramos algún valor nonexistent ó 0 y que no sea 0 ó nonexistent respectivamente y además pdays=999.
Al ver el resultado podemos decir que hay incongruencias entre el pdays y previous, ya que todos los que son pdays = 999, deberian ser previous = nonexistent, lo que en este caso nos dan 527 individuos que no cumplen esta condición. Como suponen más de un 10% de la muestra y nuestro trabajo no es exhaustivo vamos a ignorar esta incongruencia.

```{r}
  rel_pdays<-which(df$pdays==999)
  rel_previous<-which(df$previous==0)
  rel_poutcome<-which(df$poutcome=='nonexistent')
  length(setdiff(rel_poutcome, rel_previous)) 
  length(setdiff(rel_previous, rel_poutcome)) 
  length(setdiff(rel_previous, rel_pdays))    
  length(setdiff(rel_pdays,    rel_previous)) 
  summary(df[setdiff(rel_pdays,rel_previous),c('previous','poutcome')]) # Miramos el perfil de esos individuos
```
### pdays
  Con el summary podemos ver que no tenemos outliers ni errores, tampoco missings.

```{r}
  summary(df$pdays)
```

## previous
  Consideramos que para esta variable no hay outliers, ya que por los valores se ve que pueden haber sido contactado hasta en 6 capañas previas, lo que tiene sentido.
  
```{r}
  summary(df$previous)#Vemos que gran parte de los valores es 0
  par(mfrow=c(1,2)) 
  hist(df$previous,col="cyan",main="previous - Histogram")
  boxplot(df$previous, labels=row.names(df))
  par(mfrow=c(1,1)) 
```

### 
  Para los índices trimestrales/mensuales (emp.var.rate/nr.employed/cons.prize.idx/cons.conf.idx) cabe esperar que tengan los mismos valores para cada mes, si no es que son errores.
  Aparecen muchas discordancias, ya que para cada individo y para un mismo mes el valor debería ser el mismo, en este caso no lo son.
   Nuestro trabajo no es exhaustivo vamos a ignorar esta incongruencia.

```{r}
  aux<-c('emp.var.rate','nr.employed','cons.price.idx','cons.conf.idx')
  for(i in aux){
    cat("################ ",i," ##################\n")
    for(j in levels(df$month)){
      #cat("-- ",j,"--\n")
      aux2<-unique(df[which(df$month==j),i])
      cat(j,": ",aux2,"\n")
    }
  }
```
  ### Emp.var.rate,cons.price.idx, cons.conf.idx, euribor3m, nr.employed
  Necesitamos saber como se han optenido estos datos para poder validarlos, como no tenemos es información solo podemos comprobar los missings values.
  En este casa al hacer summary de cada variable, podemos ver que no existen missings.

```{r}
  summary(df$emp.var.rate)
  summary(df$cons.price.idx)
  summary(df$cons.conf.idx)
  summary(df$euribor3m)
  summary(df$nr.employed)
```
Realizamos la imputación de las variables númericas y comparamos los datos imputados con los originales.
Observamos que da valores razonados, solamente que debemos redondearlos en ambos casos ya que se trata de "número de contactos"
Igual que en  el caso anterior, solo se sobre escriben las variables imputadas en nuestro df.

```{r}
  vars_num_imp<-imputePCA(df[,vars_num],npc=5)
  
  summary(df[,vars_num])
  summary(vars_num_imp$completeObs)
  
  df[,vars_num]<-vars_num_imp$completeObs[,vars_num]
  aux<-c('previous','campaign')
  df[,aux]<-round(df[,aux])
```
# Step 4:
## Crear factores adicionales para cada variable cuantitativa
  ### Age
  Primero miramos cuan distribuidos quedan aplicando unos cortes según los cuartiles, como estos no difieren demasiado con los niveles naturales(20 añeros, 30 añeros...) preferimos quedarnos con los niveles naturales.
```{r}
aux<-quantile(df$age,seq(0,1,0.25),na.rm=TRUE) # Niveles por quartiles
aux<-factor(cut(df$age,breaks=aux,include.lowest=T))
table(aux)
tapply(df$age,aux,median)
aux2<-c(18,30,40,50,92) # Niveles "naturales"
aux<-factor(cut(df$age,breaks=aux2,include.lowest=T))
table(aux)
tapply(df$age,aux,median)
df$f.age<-factor(cut(df$age,breaks=aux2,include.lowest=T))
levels(df$f.age)<-paste0("f.age-",levels(df$f.age))
summary(df$f.age)
```
### Duration
  
  Hemos buscado una distribución más o menos equilibrada y hemos conseguido separarlo en niveles de 2min, 3min,5min, y el resto.

```{r}
# Para duration
aux<-quantile(df$duration,seq(0,1,0.25),na.rm=TRUE)#Niveles por quartiles
aux<-factor(cut(df$duration,breaks=aux,include.lowest=T))
table(aux)
tapply(df$duration,aux,median)
aux2<-c(5,120,180,300,2100)#Niveles "naturales"
aux<-factor(cut(df$duration,breaks=aux2,include.lowest=T))
table(aux)
tapply(df$duration,aux,median)
df$f.duration<-factor(cut(df$duration,breaks=aux2,include.lowest=T))#Nos quedamos con los niveles naturales
levels(df$f.duration)<-paste0("f.duration-",levels(df$f.duration))#Hacemos las etiquetas más informativas
summary(df$f.duration)
```
### Campaign
  Como para esta variable la mayoria de los valores están entre 0 y 1, no se puede hacer la separción por cuartiles.
  Hemos realizado una factorización manual viendo la cantidad de valores en ca

```{r}
aux<-levels(factor(df$campaign))
aux<-factor(cut(df$campaign,breaks=aux,include.lowest=T))
table(aux)
tapply(df$campaign,aux,median)
aux2<-c(0,1,2,20)
aux<-factor(cut(df$campaign,breaks=aux2,include.lowest=T))
table(aux)
df$f.campaign<-factor(cut(df$campaign,breaks=aux2,include.lowest=T))
levels(df$f.campaign)<-paste0("f.campaign-",levels(df$f.campaign))
summary(df$f.campaign)
```

### pdays
```{r}
aux2<-c(0,998,999) # Nos quedamos solo con dos niveles ya que no tiene sentido hacer más
pdays_cutted<-factor(cut(df$pdays,breaks=aux2,include.lowest=T))
table(pdays_cutted)
tapply(df$pdays,pdays_cutted,median)
df$f.pdays<-pdays_cutted # Nos quedamos con los niveles naturales
levels(df$f.pdays)<-paste0("f.pdays-",levels(df$f.pdays))#Hacemos las etiquetas más informativas
summary(df$f.pdays)
```

### previous - Vemos que solo hay 4 niveles por lo que los pasamos directamente a factores
```{r}
table(df$previous)
df$f.previous<-factor(df$previous)
summary(df$f.previous)
```


# Step 5:
## Profiling
  Para poder hacer profiling, necesitamos darle nombres a los subniveles de los factores, para esto hacemos un bucle que recorre cada variable categorica y le añade el nombre de la variable . el nombre del nivel.
Luego procedemos a ejecutar la función condes con la variable target duration, la cual se encuentra en la posición 11 de nuestro data frame, usamos una probabilidad de 0.01 la cual consideramos puede mostrarnos el resultado que queremos. Para la función catdes usamos a variable Y la cual se encuenta en la posición 12 de nuestro data frame.

```{r}
vars_cat_con_y<-c(vars_cat,"y")
for (i in vars_cat_con_y){
  levels(df[,i])<-paste0(i,".",levels(df[,i]))
}

condes(df,11,proba=0.01)
 
tapply(df$duration,df$f.dur,mean)
summary(df$duration)
tapply(df$duration,df$y,mean)

catdes(df,21,proba=0.001)

```
