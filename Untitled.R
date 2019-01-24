library(grid) 
library(gridExtra)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
library(corrplot)

#opción A
studentMat <- read.table("student/student-mat.csv", row.names=NULL, sep=";", header=TRUE)
studentMat$G3Pass <- ifelse(studentMat$G3 > 9, 1, 0)
studentMat$MeduSup <- ifelse(studentMat$Medu > 3, 1, 0) # si tiene estudios superiores o no
studentMat$FeduSup <- ifelse(studentMat$Fedu > 3, 1, 0) # si tiene estudios superiores o no
studentMat$G1Pass <- ifelse(studentMat$G1 > 9, 1, 0) # si ha aprovado o no G1
studentMat$G2Pass <- ifelse(studentMat$G2 > 9, 1, 0)# si ha aprovado o no G2
str(studentMat)

studentMat.aso <- studentMat %>% select(G1Pass, G2Pass, G3Pass, MeduSup, FeduSup, Mjob, Fjob, romantic)

studentMat.aso <- data.frame(sapply(studentMat.aso, as.factor))
studentMat.trans <- as(studentMat.aso, "transactions")
itemFrequencyPlot(studentMat.trans, support = 0.1, cex.names=0.8)


#opción B
studentPor <- read.table("student/student-por.csv", row.names=NULL, sep=";", header=TRUE)
names(studentPor) <- tolower(names(studentPor))
studentPor$pass <- ifelse(studentPor$g3 > 9, 1, 0)
studentPor$school <- ifelse(studentPor$school == 'GP', 1, 0)
studentPor$address <- ifelse(studentPor$address == 'U', 1, 0)
studentPor$pstatus <- ifelse(studentPor$pstatus == 'T',1,0)
studentPor$sex <- ifelse(studentPor$sex == 'M', 1, 0)
studentPor$famsize <- ifelse(studentPor$famsize == 'LE3', 1 , 0)
levelsFedu <- c("none", "4th grade", "5th-9th grade", "secondary", "high")
studentPor$medu.factor <- factor(sapply(studentPor$medu,  function(x) {
  x <- car::recode(x,"0='none'; 1='4th grade'; 2='5th-9th grade' ; 3='secondary'; 4='high'"); x}), 
  levels = levelsFedu)
studentPor$fedu.factor <- factor(sapply(studentPor$fedu,  function(x) {
  x <- car::recode(x,"0='none'; 1='4th grade'; 2='5th-9th grade' ; 3='secondary'; 4='high'"); x}), 
  levels = levelsFedu)
indice <- sapply(studentPor, is.factor)
studentPor[indice] <- lapply(studentPor[indice], function(x) if (any(x %in% c("yes","no"))) as.numeric(x) - 1 else x)
reason.levels <- c('home', 'reputation', 'course', 'other')                          
studentPor$reason <- as.numeric(studentPor$reason, levels = reason.levels)
guardian.levels <-  c('mother', 'father', 'other')
studentPor$guardian <- as.numeric(studentPor$guardian, levels = guardian.levels)
job.levels <- c('teacher', 'health' , 'services' , 'at_home', 'other')
studentPor$fjob <- as.integer(studentPor$fjob, levels = job.levels)
studentPor$mjob <- as.integer(studentPor$mjob, levels = job.levels)
studentPor <- as.data.frame(studentPor)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(studentPor[ ,-c(36,35)])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
M <- cor(studentPor[ ,-c(36,35)])
cex.before <- par("cex")
par(cex = 0.7)
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "grey", # Add coefficient of correlation
         tl.cex = 1/par("cex"),
         cl.cex = 1/par("cex"),
         tl.col="black", 
         tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, 
         sig.level = 0.01,
         addCoefasPercent = TRUE,
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
par(cex = cex.before)

# variables nivel educativo de los padres (medu y mjob)

#opción C
studentMat <- read.table("student/student-mat.csv", row.names=NULL, sep=";", header=TRUE)

names(studentMat) <- tolower(names(studentMat))

studentMat$pass <- ifelse(studentMat$g3 > 9, 1, 0)
studentMat$school <- ifelse(studentMat$school == 'GP', 1, 0)
studentMat$address <- ifelse(studentMat$address == 'U', 1, 0)
studentMat$pstatus <- ifelse(studentMat$pstatus == 'T',1,0)
studentMat$sex <- ifelse(studentMat$sex == 'M', 1, 0)
studentMat$famsize <- ifelse(studentMat$famsize == 'LE3', 1 , 0)

levelsFedu <- c("none", "4th grade", "5th-9th grade", "secondary", "high")
studentMat$medu.factor <- factor(sapply(studentMat$medu,  function(x) {
  x <- car::recode(x,"0='none'; 1='4th grade'; 2='5th-9th grade' ; 3='secondary'; 4='high'"); x}), 
  levels = levelsFedu)
studentMat$fedu.factor <- factor(sapply(studentMat$fedu,  function(x) {
  x <- car::recode(x,"0='none'; 1='4th grade'; 2='5th-9th grade' ; 3='secondary'; 4='high'"); x}), 
  levels = levelsFedu)

indice <- sapply(studentMat, is.factor)
studentMat[indice] <- lapply(studentMat[indice], function(x) if (any(x %in% c("yes","no"))) as.numeric(x) - 1 else x)

reason.levels <- c('home', 'reputation', 'course', 'other')                          
studentMat$reason <- as.numeric(studentMat$reason, levels = reason.levels)

guardian.levels <-  c('mother', 'father', 'other')
studentMat$guardian <- as.numeric(studentMat$guardian, levels = guardian.levels)

job.levels <- c('teacher', 'health' , 'services' , 'at_home', 'other')
studentMat$fjob <- as.integer(studentMat$fjob, levels = job.levels)
studentMat$mjob <- as.integer(studentMat$mjob, levels = job.levels)
studentMat <- as.data.frame(studentMat)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


# matrix of the p-value of the correlation
p.mat <- cor.mtest(studentMat[ ,-c(36,35)])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
M <- cor(studentPor[ ,-c(36,35)])
cex.before <- par("cex")
par(cex = 0.7)
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "grey", # Add coefficient of correlation
         tl.cex = 1/par("cex"),
         cl.cex = 1/par("cex"),
         tl.col="black", 
         tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, 
         sig.level = 0.01,
         addCoefasPercent = TRUE,
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
par(cex = cex.before)



#Analizamos la relación de algunas variables con la variable objetivo G3 = evaluación final:

#Para la variable tipo factor podemos ver su distribución en relación al target, con diagramas de barras
ggplot(studentMat, aes(school)) + geom_bar() + facet_wrap(~ pass) + ggtitle ("Diagrama barras school, por nota final") + theme(plot.title = element_text(vjust = +1.5, size = 12))
ggplot(studentMat, aes(famsup)) + geom_bar() + facet_wrap(~ pass) + ggtitle ("Diagrama barras famsup, por nota final") + theme(plot.title = element_text(vjust = +1.5, size = 12))
# [Ok] Se observan diferencias en la distribución de la variable famsub (soporte educacional), para alumnos que aprueban.

ggplot(studentMat, aes(sex)) + geom_bar() + facet_wrap(~ pass) + ggtitle ("Diagrama barras sex, por nota final") + theme(plot.title = element_text(vjust = +1.5, size = 12))
#No se observan diferencias en la distribución de la variable Sex (Sexo), para alumnos que aprueban.

ggplot(studentMat, aes(Mjob)) + geom_bar() + facet_wrap(~ pass) + ggtitle ("Diagrama barras Trabajo materno, por nota final") + theme(plot.title = element_text(vjust = +1.5, size = 12))
#No se observan diferencias en la distribución de la variable Mjob (trabajo materno), para alumnos que aprueban.
ggplot(studentMat, aes(Fjob)) + geom_bar() + facet_wrap(~ pass) + ggtitle ("Diagrama barras Trabajo paterno, por nota final") + theme(plot.title = element_text(vjust = +1.5, size = 12))
#No se observan diferencias en la distribución de la variable Fjob (trabajo paterno), para alumnos que aprueban.

ggplot(studentMat, aes(romantic)) + geom_bar() + facet_wrap(~ pass) + ggtitle ("Diagrama barras Romantinc, por nota final") + theme(plot.title = element_text(vjust = +1.5, size = 12))

ggplot(studentMat, aes(Medu)) + geom_bar() + facet_wrap(~ pass) + ggtitle ("Diagrama barras Educacion materno, por nota final") + theme(plot.title = element_text(vjust = +1.5, size = 12))
ggplot(studentMat, aes(Fedu)) + geom_bar() + facet_wrap(~ pass) + ggtitle ("Diagrama barras Educacion paterno, por nota final") + theme(plot.title = element_text(vjust = +1.5, size = 12))
# [OK] Correlación alta entre los niveles educativos de los padres y la proporción de alumnos aprobados. 

ggplot(studentMat, aes(Dalc)) + geom_bar() + facet_wrap(~ pass) + ggtitle ("Diagrama barras Consumo de alcohol en el trabajo, por nota final") + theme(plot.title = element_text(vjust = +1.5, size = 12))
ggplot(studentMat, aes(Walc)) + geom_bar() + facet_wrap(~ pass) + ggtitle ("Diagrama barras Consumo de alcohol fuera del trabajo, por nota final") + theme(plot.title = element_text(vjust = +1.5, size = 12))

# Para las variables continuas, podemos ver su distribución en relación al target, con histogramas. Por ejemplo:

ggplot(studentMat, aes(x = traveltime)) + geom_histogram(binwidth = 1, fill = "white", colour = "black") + facet_grid(pass ~ .) + ggtitle ("Histograma Tiempo itinerario casa-escuela por nota final") + theme(plot.title=element_text(vjust = +1.5, size = 12))
#La distribución del tiempo que tarda el alumno en ir de casa a la escuela, parece similar para aquellos con “pass” y aquellos con “fail” en la nota final (G3)

ggplot(studentMat, aes(x = studytime))+ geom_histogram(binwidth = 1, fill = "white", colour = "black") + facet_grid(pass ~ .) + ggtitle ("Histograma Tiempo estudio por nota final") + theme(plot.title=element_text(vjust = +1.5, size = 12))
# [OK] La distribución del tiempo que dedica al estudio, tienden a ser distintas (como es lógico)

ggplot(studentMat, aes(x = goout)) + geom_histogram(binwidth = 1, fill = "white", colour = "black") + facet_grid(pass ~ .) + ggtitle ("Histograma salidas con amigos, por nota final") + theme(plot.title=element_text(vjust = +1.5, size = 12))

ggplot(studentMat, aes(x = G1)) + geom_histogram(binwidth = 1, fill = "white", colour = "black") + facet_grid(pass ~ .) + ggtitle ("Histograma G1 por nota final") + theme(plot.title=element_text(vjust = +1.5, size = 12))
ggplot(studentMat, aes(x = G2)) + geom_histogram(binwidth = 1, fill = "white", colour = "black") + facet_grid(pass ~ .) + ggtitle ("Histograma G2 por clasificación nota final") + theme(plot.title=element_text(vjust = +1.5, size = 12))
# [OK] La distribución de G1 y G2, tienen distribuciones muy diferentes para los casos “pass” y “fail” en la nota final G3

studentMat.aso <- studentMat %>% select(G1Pass, G2Pass, G3Pass, MeduSup, FeduSup, Mjob, Fjob, romantic)
studentMat.aso <- data.frame(sapply(studentMat.aso, as.factor))

studentMat.trans <- as(studentMat.aso, "transactions")
itemFrequencyPlot(studentMat.trans, support = 0.1, cex.names=0.8)

reglas <- apriori(studentMat.aso ,parameter = list( supp = 0.05 , conf = 0.7 , target = "rules", minlen = 2),
                  appearance = list(rhs = c("G3Pass=1", "G3Pass=0"), default = "lhs"))

reglas_redundantes <- reglas[is.redundant(x = reglas, measure = "confidence")]
summary(reglas_redundantes)
inspect(sort(reglas_redundantes, by = "support")[1:10])
plot(reglas_redundantes, method = "grouped", control = list(k = 15, col = heat.colors(12)))
plot(reglas_redundantes)
plot(reglas_redundantes, method="graph")

subsetMatrix <- is.subset(reglas, reglas)
subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <- FALSE
redundant <- colSums(subsetMatrix, na.rm=TRUE) >= 1
reglas <- reglas[!redundant] # remove redundant rules
inspect(head(reglas))

quality(reglas) <- round(quality(reglas), digits = 3)
summary(reglas)
inspect(sort(reglas, by = "support")[1:10])
plot(reglas, method = "grouped", control = list(k = 15, col = heat.colors(12)))
plot(reglas)
plot(reglas, method="graph")

interestMeasure(reglas, c("support", "chiSquare", "confidence", "conviction", "cosine", "coverage", "leverage", "lift", "oddsRatio"), studentMat.aso)


studentMat$school <-  factor(studentMat$school, levels = c('GP', 'MS'), labels = c(0, 1))
studentMat$sex <-  factor(studentMat$sex, levels = c('F', 'M'), labels = c(0, 1))

studentMat$address <-  factor(studentMat$address, levels = c('R', 'U'), labels = c(0, 1))
studentMat$famsize <-  factor(studentMat$famsize, levels = c('GT3', 'LE3'), labels = c(0, 1))
studentMat$pstatus <-  factor(studentMat$pstatus, levels = c('A', 'T'), labels = c(0, 1))

#Medu - mother's education 
#     0 - none,
#     1 - primary education (4th grade),
#     2 â€“ 5th to 9th grade, 
#     3 â€“ secondary education 
#     4 â€“ higher education) 
levelsFedu <- c("none", "4th grade", "5th-9th grade", "secondary", "high")

#studentMat$Medu <- factor(sapply(studentPor$Medu,  function(x) {
#  x <- car::recode(x,"0='none'; 1='4th grade'; 2='5th-9th grade' ; 3='secondary'; 4='high'"); x}), 
#  levels = levelsFedu)
studentMat$Medu

#Fedu - father's education 
#studentMat$Fedu.factor <- factor(sapply(studentPor$Fedu,  function(x) {
#  x <- car::recode(x,"0='none'; 1='4th grade'; 2='5th-9th grade' ; 3='secondary'; 4='high'"); x}), 
#  levels = levelsFedu)
studentMat$Fedu

job.levels <- c('teacher', 'health' , 'services' , 'at_home', 'other')
studentMat$Fjob <- as.integer(studentMat$Fjob, levels = job.levels)
studentMat$Mjob <- as.integer(studentMat$Mjob, levels = job.levels)

# Reason to choose this school
reason.levels <- c('home', 'reputation', 'course', 'other')                          
studentMat$reason <- as.numeric(studentMat$reason, levels = reason.levels)

#student's guardian
guardian.levels <-  c('mother', 'father', 'other')
studentMat$guardian <- as.numeric(studentMat$guardian, levels = guardian.levels)

studentMat$traveltime
studentMat$studytime
studentMat$failures

#schoolsup - extra educational support 
studentMat$schoolsup <-  factor(studentMat$schoolsup,levels = c('no', 'yes'), labels = c(0, 1))
#famsup - family educational support
studentMat$famsup <-  factor(studentMat$famsup,levels = c('no', 'yes'), labels = c(0, 1))
#paid - extra paid classes within the course subject
studentMat$paid <-  factor(studentMat$paid,levels = c('no', 'yes'), labels = c(0, 1))
#activities - extra-curricular activities
studentMat$activities <-  factor(studentMat$activities,levels = c('no', 'yes'), labels = c(0, 1))
#nursery - attended nursery school
studentMat$nursery <-  factor(studentMat$nursery,levels = c('no', 'yes'), labels = c(0, 1))
#higher - wants to take higher education
studentMat$higher <-  factor(studentMat$higher,levels = c('no', 'yes'), labels = c(0, 1))
# internet - Internet access at home 
studentMat$internet <-  factor(studentMat$internet,levels = c('no', 'yes'), labels = c(0, 1))
#romantic - with a romantic relationship
studentMat$romantic <-  factor(studentMat$romantic,levels = c('no', 'yes'), labels = c(0, 1))

#famrel - quality of family relationships 
studentMat$famrel

#studentMat$freetime = [-inf, 2) - [2-3) - [3, Inf)
studentMat$freetime = ordered(cut(studentMat$freetime, c(-Inf,2,3,Inf), right = FALSE) , labels = c("low", "medium", "high"))

#goout - going out with friends = [-inf, 2) - [2-3) - [3, Inf)
studentMat$goout = ordered(cut(studentMat$goout, c(-Inf,2,3,Inf), right = FALSE) , labels = c("low", "medium", "high"))

#Dalc - workday alcohol consumption = [-inf, 2) - [2-3) - [3, Inf)
studentMat$Dalc = ordered(cut(studentMat$Dalc, c(-Inf,2,3,Inf), right = FALSE) , labels = c("low", "medium", "high"))

#Walc - weekend alcohol consumption = [-inf, 2) - [2-3) - [3, Inf) 
studentMat$Walc = ordered(cut(studentMat$Walc, c(-Inf,2,3,Inf), right = FALSE) , labels = c("low", "medium", "high"))

#health - current health status
studentMat$health

#absences - number of school absences
studentMat$absences

studentMat$G1
studentMat$G2
studentMat$G3

studentMat$pass <- ifelse(studentMat$G3 > 9, 1, 0)

str(studentMat)
cor(studentMat$Medu, studentMat$G1)