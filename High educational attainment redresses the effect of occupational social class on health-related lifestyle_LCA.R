
# Lectura de datos
library(foreign)
datos_educ <- read.spss("C:/Users/anton/Desktop/Umubox/datos_educacion.sav", to.data.frame = T)

# Latent class sin codificar bien los datos
library(poLCA)

f1 = cbind(Inactivos_LCA, Fruta_no_diariaLCA, No_verdura_diarioLCA, fumarLCA, AlcoholLCA) ~ 1
M1 <- poLCA(f1, datos_educ, nclass = 3, verbose = F, graphs = T, na.rm = T)
M1

levels(datos_educ$AlcoholLCA)
levels(datos_educ$fumarLCA)
levels(datos_educ$No_verdura_diarioLCA)
levels(datos_educ$Fruta_no_diariaLCA)
levels(datos_educ$Inactivos_LCA)

#### Recodificación correcta de los datos en variables numéricas con valores 1 (conducta positiva) y 2 (negativa) ####

datos_educ$AlcoholLCA_n[datos_educ$AlcoholLCA == "Non-alcohol use"] <- 1
datos_educ$AlcoholLCA_n[datos_educ$AlcoholLCA == "Alcohol use"] <- 2


datos_educ$fumarLCA_n[datos_educ$fumarLCA == "Non-current smoker"] <- 1
datos_educ$fumarLCA_n[datos_educ$fumarLCA == "Current smoker"] <- 2


datos_educ$No_verdura_diarioLCA_n[datos_educ$No_verdura_diarioLCA == "Daily vegetables intake"] <- 1
datos_educ$No_verdura_diarioLCA_n[datos_educ$No_verdura_diarioLCA == "Non-daily vegetables intake"] <- 2


datos_educ$Fruta_no_diariaLCA_n[datos_educ$Fruta_no_diariaLCA == "Daily fruit intake"] <- 1
datos_educ$Fruta_no_diariaLCA_n[datos_educ$Fruta_no_diariaLCA == "Non-daily fruit intake"] <- 2


datos_educ$Inactivos_LCA_n[datos_educ$Inactivos_LCA == "Active"] <- 1
datos_educ$Inactivos_LCA_n[datos_educ$Inactivos_LCA == "Inactive"] <- 2


datos_educ$Inactivos_LCA_n  <- as.factor(datos_educ$Inactivos_LCA_n)
datos_educ$Fruta_no_diariaLCA_n  <- as.factor(datos_educ$Fruta_no_diariaLCA_n)
datos_educ$No_verdura_diarioLCA_n  <- as.factor(datos_educ$No_verdura_diarioLCA_n)
datos_educ$fumarLCA_n  <- as.factor(datos_educ$fumarLCA_n)
datos_educ$AlcoholLCA_n  <- as.factor(datos_educ$AlcoholLCA_n)

#### Latent class analisis correcto ####
f2 = cbind(Inactivos_LCA_n, Fruta_no_diariaLCA_n, No_verdura_diarioLCA_n, fumarLCA_n, AlcoholLCA_n) ~ 1

M2 <- poLCA(f2, datos_educ, nclass = 2, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M2

M3 <- poLCA(f2, datos_educ, nclass = 3, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M3

M4 <- poLCA(f2, datos_educ, nclass = 4, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M4


# Analisis adicionales - Cambios en la codificacion en Verdura, fruta y AF: 1 = NO; 2 = SI
datos_educ$AlcoholLCA_n2[datos_educ$AlcoholLCA == "Non-alcohol use"] <- 1
datos_educ$AlcoholLCA_n2[datos_educ$AlcoholLCA == "Alcohol use"] <- 2


datos_educ$fumarLCA_n2[datos_educ$fumarLCA == "Non-current smoker"] <- 1
datos_educ$fumarLCA_n2[datos_educ$fumarLCA == "Current smoker"] <- 2


datos_educ$No_verdura_diarioLCA_n2[datos_educ$No_verdura_diarioLCA == "Daily vegetables intake"] <- 2
datos_educ$No_verdura_diarioLCA_n2[datos_educ$No_verdura_diarioLCA == "Non-daily vegetables intake"] <- 1


datos_educ$Fruta_no_diariaLCA_n2[datos_educ$Fruta_no_diariaLCA == "Daily fruit intake"] <- 2
datos_educ$Fruta_no_diariaLCA_n2[datos_educ$Fruta_no_diariaLCA == "Non-daily fruit intake"] <- 1


datos_educ$Inactivos_LCA_n2[datos_educ$Inactivos_LCA == "Active"] <- 2
datos_educ$Inactivos_LCA_n2[datos_educ$Inactivos_LCA == "Inactive"] <- 1



f3 = cbind(Inactivos_LCA_n2, Fruta_no_diariaLCA_n2, No_verdura_diarioLCA_n2, fumarLCA_n2, AlcoholLCA_n2) ~ 1

M2.2 <- poLCA(f3, datos_educ, nclass = 2, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M2.2

M3.2 <- poLCA(f3, datos_educ, nclass = 3, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M3.2

M4.2 <- poLCA(f3, datos_educ, nclass = 4, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M4.2


#### Analisis en la poblacion 18-64 años y segmentando por sexo ####
datos_educ_18_64 <- datos_educ[which(datos_educ$EDADa >= 18 & datos_educ$EDADa < 65), ]
datos_educ_18_64_men <- datos_educ_18_64[which(datos_educ_18_64$SEXOa == "Varón"), ]
datos_educ_18_64_women <- datos_educ_18_64[which(datos_educ_18_64$SEXOa == "Mujer"), ]

# LCA Mujeres
M2w <- poLCA(f2, datos_educ_18_64_women, nclass = 2, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M2w
poLCA.entropy(M2w)

##RELATIVE ENTROPY
##Numerator:
nume.E2w <- -sum(M2w$posterior * log(M2w$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E2w <- 35941*log(2)
##Relative Entropy
Entro2w <- 1-(nume.E2w/deno.E2w)
Entro2w



M3w <- poLCA(f2, datos_educ_18_64_women, nclass = 3, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M3w
poLCA.entropy(M3w)

##RELATIVE ENTROPY
##Numerator:
nume.E3w <- -sum(M3w$posterior * log(M3w$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E3w <- 35941*log(3)
##Relative Entropy
Entro3w <- 1-(nume.E3w/deno.E3w)
Entro3w



M4w <- poLCA(f2, datos_educ_18_64_women, nclass = 4, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M4w
poLCA.entropy(M4w)


##RELATIVE ENTROPY
##Numerator:
nume.E4w <- -sum(M4w$posterior * log(M4w$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E4w <- 35941*log(4)
##Relative Entropy
Entro4w <- 1-(nume.E4w/deno.E4w)
Entro4w




M5w <- poLCA(f2, datos_educ_18_64_women, nclass = 5, verbose = F, graphs = T, na.rm = T, maxiter = 30000, nrep = 2)
M5w
poLCA.entropy(M5w)


##RELATIVE ENTROPY
##Numerator:
nume.E5w <- -sum(M5w$posterior * log(M5w$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E5w <- 35941*log(5)
##Relative Entropy
Entro5w <- 1-(nume.E5w/deno.E5w)
Entro5w






# LCA Hombres
M2m <- poLCA(f2, datos_educ_18_64_men, nclass = 2, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M2m
poLCA.entropy(M2m)

##RELATIVE ENTROPY
##Numerator:
nume.E2m <- -sum(M2m$posterior * log(M2m$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E2m <- 30636*log(2)
##Relative Entropy
Entro2m <- 1-(nume.E2m/deno.E2m)
Entro2m




M3m <- poLCA(f2, datos_educ_18_64_men, nclass = 3, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M3m
poLCA.entropy(M3m)

##RELATIVE ENTROPY
##Numerator:
nume.E3m <- -sum(M3m$posterior * log(M3m$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E3m <- 30636*log(3)
##Relative Entropy
Entro3m <- 1-(nume.E3m/deno.E3m)
Entro3m





M4m <- poLCA(f2, datos_educ_18_64_men, nclass = 4, verbose = F, graphs = T, na.rm = T, maxiter = 20000, nrep = 3)
M4m
poLCA.entropy(M4m)

##RELATIVE ENTROPY
##Numerator:
nume.E4m <- -sum(M4m$posterior * log(M4m$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E4m <- 30636*log(4)
##Relative Entropy
Entro4m <- 1-(nume.E4m/deno.E4m)
Entro4m




M5m <- poLCA(f2, datos_educ_18_64_men, nclass = 5, verbose = F, graphs = T, na.rm = T, maxiter = 20000, nrep = 3)
M5m
poLCA.entropy(M5m)

##RELATIVE ENTROPY
##Numerator:
nume.E5m <- -sum(M5m$posterior * log(M5m$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E5m <- 30636*log(5)
##Relative Entropy
Entro5m <- 1-(nume.E5m/deno.E5m)
Entro5m



#### Adjudicando a cada sujeto su clase del latent class analysis ####
# Mujeres
datos_educ_18_64_women <- datos_educ_18_64_women[complete.cases(datos_educ_18_64_women$Fruta_no_diariaLCA_n),]
datos_educ_18_64_women <- datos_educ_18_64_women[complete.cases(datos_educ_18_64_women$No_verdura_diarioLCA_n),]
datos_educ_18_64_women <- datos_educ_18_64_women[complete.cases(datos_educ_18_64_women$Inactivos_LCA_n),]
datos_educ_18_64_women <- datos_educ_18_64_women[complete.cases(datos_educ_18_64_women$fumarLCA_n),]
datos_educ_18_64_women <- datos_educ_18_64_women[complete.cases(datos_educ_18_64_women$AlcoholLCA_n),]

datos_educ_18_64_women$predclass <- M4w$predclass
datos_educ_18_64_women$predclass  <- as.factor(datos_educ_18_64_women$predclass)
table(datos_educ_18_64_women$predclass);prop.table(table(datos_educ_18_64_women$predclass))


# Hombres
datos_educ_18_64_men <- datos_educ_18_64_men[complete.cases(datos_educ_18_64_men$Fruta_no_diariaLCA_n),]
datos_educ_18_64_men <- datos_educ_18_64_men[complete.cases(datos_educ_18_64_men$No_verdura_diarioLCA_n),]
datos_educ_18_64_men <- datos_educ_18_64_men[complete.cases(datos_educ_18_64_men$Inactivos_LCA_n),]
datos_educ_18_64_men <- datos_educ_18_64_men[complete.cases(datos_educ_18_64_men$fumarLCA_n),]
datos_educ_18_64_men <- datos_educ_18_64_men[complete.cases(datos_educ_18_64_men$AlcoholLCA_n),]

datos_educ_18_64_men$predclass <- M4m$predclass
datos_educ_18_64_men$predclass  <- as.factor(datos_educ_18_64_men$predclass)
table(datos_educ_18_64_men$predclass);prop.table(table(datos_educ_18_64_men$predclass))

