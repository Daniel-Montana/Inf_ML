setwd("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\CSV")

Hogar <- read.csv(file = "C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\CSV\\Características generales, seguridad social en salud y educación.xlsx"
                  , sep = "\t")
library(readr)
caracteristicas <- read_delim("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\CSV\\Características generales, seguridad social en salud y educación.csv",";")



library(foreign)
General<- read.spss("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\SAV\\Características generales, seguridad social en salud y educación.sav", to.data.frame=TRUE)
Ocup <- read.spss("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\SAV\\Ocupados.sav", to.data.frame=TRUE)
Hogar<- read.spss("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\SAV\\Datos del hogar y la vivienda.sav", to.data.frame=TRUE)
FL <- read.spss("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\SAV\\Fuerza de trabajo.sav", to.data.frame=TRUE)
Migra <- read.spss("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\SAV\\Migración.sav", to.data.frame=TRUE)
N_Ocup <- read.spss("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\SAV\\No ocupados.sav", to.data.frame=TRUE)
O_trab <- read.spss("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\SAV\\Otras formas de trabajo.sav", to.data.frame=TRUE)
O_ing <- read.spss("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Enero\\SAV\\Otros ingresos e impuestos.sav", to.data.frame=TRUE)

General<- read.spss("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Diciembre 22\\SAV\\Características generales, seguridad social en salud y educación.sav", to.data.frame=TRUE)
Ocup <- read.spss("C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Mercado Laboral\\Diciembre 22\\SAV\\Ocupados.sav", to.data.frame=TRUE)



orden <- !(names(General) %in% names(Hogar))
orden[1:2] <- TRUE
Hogar <- Hogar[, orden]
dataJoin <- merge(datCaracteristicas, datVivienda, 
                  by = c("DIRECTORIO", "SECUENCIA_P"),
                  all = TRUE)


ML <- merge(General, Ocup,
            by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
            all = TRUE)

ML <- merge(ML, FL,
            by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
            all = TRUE)

ML <- merge(ML, Hogar,
            by = c("DIRECTORIO", "SECUENCIA_P"),
            all = TRUE)

ML <- merge(ML, Migra,
            by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
            all = TRUE)

ML <- merge(ML, N_Ocup,
            by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
            all = TRUE)

ML <- merge(ML, O_ing,
            by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
            all = TRUE)
ML <- merge(ML, O_trab,
            by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
            all = TRUE)



ML_JOV <- ML[ML$P6040 >= 14 & ML$P6040 <= 28 ,]


ML$DPTO_NAME <- vector(mode='character',length=dim(ML)[1])
ML$DPTO_NAME[ML$DPTO.x == 11] <- "Bogota"
lista <- split(ML, ML$DPTO_NAME)

bogota <- as.data.frame(lista$Bogota)

ML_Bog_jov <- ML_Bog[ML_Bog$P6040 >= 14 & ML_Bog$P6040 <= 28 ,]



tablita_MAD <- EM_MAD %>% filter(NPCKPA46 == 1) %>% group_by(DIV_n) %>% 
  summarise(no = sum(FEX_C))  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)

ML$DPTO_NAME <- vector(mode='character',length=dim(ML)[1])


#Informalidad
bogota$Informalidad <- rep(0, nrow(bogota))
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 1] <- 1
bogota$Informalidad[bogota$P3069 <= 3 &  bogota$P6430 == 6] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 3] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 8] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 4 
                    & bogota$P3042 != 8 & bogota$P3042 != 10
                    & bogota$P3042 != 11 & bogota$P3042 != 12
                    & bogota$P3042 != 13] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 5] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 7] <- 1

Informales_BOG <- sum(bogota[bogota$Informalidad == 1,]$FEX_C18)
TI_BOG <- Informales_BOG/Ocupados_BOG*100




bogota$anios <- bogota$PER.x-1
bogota$ocup_2D_c <- sprintf("%04d", as.numeric(bogota$OFICIO_C8))
bogota$ocup_2D <- substr(bogota$ocup_2D_c,1,2)
bogota$RAMA_2D_R4 <- as.numeric(bogota$RAMA2D_R4)

bogota$FORMAL[bogota$P6430 == 3] <- "."
bogota$FORMAL[bogota$P6430 == 6] <- 0
bogota$FORMAL[bogota$RAMA_2D_R4 == 84 | bogota$RAMA_2D_R4 == 99] <- 1 
bogota$FORMAL[bogota$P6430 == 8] <- 0

bogota$FORMAL[bogota$P6430 == 2] <- 1
bogota$FORMAL[(bogota$P6430 == 1 | bogota$P6430 == 7) & bogota$P3045S1==1] <- 1
bogota$FORMAL[(bogota$P6430 == 1 | bogota$P6430 == 7) & (bogota$P3045S1==2 | bogota$P3045S1==9) & bogota$P3046==1] <- 1
bogota$FORMAL[(bogota$P6430 == 1 | bogota$P6430 == 7) & (bogota$P3045S1==2 | bogota$P3045S1==9) & bogota$P3046==2] <- 0
bogota$FORMAL[(bogota$P6430 == 1 | bogota$P6430 == 7) & (bogota$P3045S1==2 | bogota$P3045S1==9) & bogota$P3046==1 & bogota$P3069 >= 4] <- 1 XXXXXXXX
bogota$FORMAL[(bogota$P6430 == 1 | bogota$P6430 == 7) & (bogota$P3045S1==2 | bogota$P3045S1==9) & bogota$P3046==1 & bogota$P3069 <= 3] <- 0 XXXXXXX


bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 == 5) & (bogota$P6765==1 | bogota$P6765==2 | bogota$P6765==3 | bogota$P6765==4 |
                                                          bogota$P6765==5 | bogota$P6765==6 | bogota$P6765==8) & bogota$P3065==1] <- 1
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 == 5) & (bogota$P6765==1 | bogota$P6765==2 | bogota$P6765==3 | bogota$P6765==4 |
                                                           bogota$P6765==5 | bogota$P6765==6 | bogota$P6765==8) & (bogota$P3065==2 | bogota$P3065==9) & bogota$P3066==1] <- 1
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 == 5) & (bogota$P6765==1 | bogota$P6765==2 | bogota$P6765==3 | bogota$P6765==4 |
                                                           bogota$P6765==5 | bogota$P6765==6 | bogota$P6765==8) & (bogota$P3065==2 | bogota$P3065==9) & bogota$P3066==2] <- 0
bogota$FORMAL[(bogota$P6430 == 5) & (bogota$P6765==1 | bogota$P6765==2 | bogota$P6765==3 | bogota$P6765==4 |
                                                           bogota$P6765==5 | bogota$P6765==6 | bogota$P6765==8) & (bogota$P3065==2 | bogota$P3065==9) & bogota$P3066==9 & bogota$P3069 >= 4] <- 1
bogota$FORMAL[(bogota$P6430 == 5) & (bogota$P6765==1 | bogota$P6765==2 | bogota$P6765==3 | bogota$P6765==4 |
                                       bogota$P6765==5 | bogota$P6765==6 | bogota$P6765==8) & (bogota$P3065==2 | bogota$P3065==9) & bogota$P3066==9 & bogota$P3069 <= 3] <- 0
bogota$FORMAL[(bogota$P6430 == 4) & (bogota$P6765==1 | bogota$P6765==2 | bogota$P6765==3 | bogota$P6765==4 |
                                       bogota$P6765==5 | bogota$P6765==6 | bogota$P6765==8) & (bogota$P3065==2 | bogota$P3065==9) & bogota$P3066==9 & (bogota$ocup_2D >= 00 & bogota$ocup_2D <= 20)] <- 1
bogota$FORMAL[(bogota$P6430 == 4) & (bogota$P6765==1 | bogota$P6765==2 | bogota$P6765==3 | bogota$P6765==4 |
                                       bogota$P6765==5 | bogota$P6765==6 | bogota$P6765==8) & (bogota$P3065==2 | bogota$P3065==9) & bogota$P3066==9 & (bogota$ocup_2D >= 21)] <- 0


bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 == 5) & (bogota$P6765 == 7) & bogota$P3067 == 1 & bogota$P3067s1 == 1 & bogota$P3067s2 >= bogota$anios] <-1 XXXXXXX
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 == 5) & bogota$P6765 == 7 & bogota$P3067 == 1 & bogota$P3067s1 == 1 & bogota$P3067s2 < bogota$anios] <- 0 XXXXXXX
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 == 5) & (bogota$P6765 == 7) & bogota$P3067 == 1 & bogota$P3067s1 == 2 & bogota$P6775 == 1] <- 1 XXXXXX
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 == 5) & (bogota$P6765 == 7) & bogota$P3067 == 1 & bogota$P3067s1 == 2 & bogota$P6775 == 3 & (bogota$ocup_2D >= 00 | bogota$ocup_2D <= 20)] <- 1 XXXXXXXXXX
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 == 5) & (bogota$P6765 == 7) & bogota$P3067 == 1 & bogota$P3067s1 == 2 & bogota$P6775 == 3 & bogota$ocup_2D >= 21] <- 0 XXXXXXXXXXX
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 == 5) & (bogota$P6765 == 7) & bogota$P3067 == 1 & bogota$P3067s1 == 2 & bogota$P6775 == 2] <- 0 XXXXXXXXXXXXXX
bogota$FORMAL[(bogota$P6430 == 4) & (bogota$P6765==7) & bogota$P3067==1 & bogota$P3067s1==2 & bogota$P6775==9 & (bogota$ocup_2D >= 00 & bogota$ocup_2D <= 20)] <- 1 XXXXXXXXXXxx
bogota$FORMAL[(bogota$P6430 == 4) & (bogota$P6765==7) & bogota$P3067==1 & bogota$P3067s1==2 & bogota$P6775==9 & (bogota$ocup_2D >= 21)] <- 0 XXXXXXXXXXXXXXXXX
bogota$FORMAL[(bogota$P6430 == 5) & (bogota$P6765==7) & bogota$P3067==1 & bogota$P3067s1==2 & bogota$P6775==9 & bogota$P3069 >= 4] <- 1 XXXXXXXXXXXXXXXXXXXXX
bogota$FORMAL[(bogota$P6430 == 5) & (bogota$P6765==7) & bogota$P3067==1 & bogota$P3067s1==2 & bogota$P6775==9 & bogota$P3069 <= 3] <- 0 XXXXXXXXXXXXXXXXXXX


bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 ==5) & (bogota$P6765==7) & bogota$P3067==2 & bogota$P6775==1 & bogota$P3068==1] <- 1
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 ==5) & (bogota$P6765==7) & bogota$P3067==2 & bogota$P6775==1 & bogota$P3068==2] <- 0
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 ==5) & (bogota$P6765==7) & bogota$P3067==2 & bogota$P6775==3 & bogota$ocup_2D >= 00 & bogota$ocup_2D <= 20] <- 1
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 ==5) & (bogota$P6765==7) & bogota$P3067==2 & bogota$P6775==3 & bogota$ocup_2D >= 21] <- 0
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 ==5) & (bogota$P6765==7) & bogota$P3067==2 & bogota$P6775==1 & bogota$P3068==9] <- 0
bogota$FORMAL[(bogota$P6430 == 4 | bogota$P6430 ==5) & (bogota$P6765==7) & bogota$P3067==2 & bogota$P6775==2] <- 0
bogota$FORMAL[(bogota$P6430 ==5) & (bogota$P6765==7) & bogota$P3067==2 & bogota$P6775==9 & bogota$P3069 >= 4] <- 1
bogota$FORMAL[(bogota$P6430 ==5) & (bogota$P6765==7) & bogota$P3067==2 & bogota$P6775==9 & bogota$P3069 <= 3] <- 0
bogota$FORMAL[(bogota$P6430 == 4) & (bogota$P6765==7) & bogota$P3067==2 & bogota$P6775==9 & bogota$ocup_2D >= 00 & bogota$ocup_2D <= 20] <- 1
bogota$FORMAL[(bogota$P6430 == 4) & (bogota$P6765==7) & bogota$P3067==2 & bogota$P6775==9 & bogota$ocup_2D >= 21] <- 0


bogota$SALUD <- 0
bogota$SALUD[(bogota$P6430 == 1 | bogota$P6430 == 2 | bogota$P6430 == 3 | bogota$P6430 == 7) & (bogota$P6100 == 1 | bogota$P6100 == 2) & (bogota$P6110 == 1 | bogota$P6110 == 2 | bogota$P6110 ==4)] <- 1
bogota$SALUD[(bogota$P6430 == 1 | bogota$P6430 == 2 | bogota$P6430 == 3 | bogota$P6430 == 7) & (bogota$P6100 == 9) & (bogota$P6450 == 2)] <- 1
bogota$SALUD[(bogota$P6430 == 1 | bogota$P6430 == 2 | bogota$P6430 == 3 | bogota$P6430 == 7) & (bogota$P6110 == 9) & (bogota$P6450 == 2)] <- 1

bogota$PENSION <- 0
bogota$PENSION[(bogota$P6430 == 1 | bogota$P6430 == 2 | bogota$P6430 == 3 | bogota$P6430 == 7) & bogota$P6920==3] <- 1
bogota$PENSION[(bogota$P6430 == 1 | bogota$P6430 == 2 | bogota$P6430 == 3 | bogota$P6430 == 7) & bogota$P6920==1 & (bogota$P6930 == 1 | bogota$P6930 == 2 | bogota$P6930 == 3)
               & (bogota$P6940 == 1 | bogota$P6940 == 3)] <- 1


bogota$EI <- 0
bogota$EI[(bogota$P6430==4 | bogota$P6430==5)]<-bogota$FORMAL 

bogota$EI[(bogota$P6430==4 | bogota$P6430==5) & bogota$FORMAL==1]<-1
bogota$EI[(bogota$P6430==4 | bogota$P6430==5) & bogota$FORMAL==0]<-0

bogota$EI[(bogota$P6430==1 | bogota$P6430==2| bogota$P6430==3 | bogota$P6430==7) & bogota$SALUD==1 & bogota$PENSION==1]<-1
bogota$EI[(bogota$P6430==4 ) & (bogota$RAMA_2D_R4==84 | bogota$RAMA_2D_R4==99)]<-1



tablita_MAD <- EM_MAD %>% filter(NPCKPA46 == 1) %>% group_by(DIV_n) %>% 
  summarise(no = sum(FEX_C))  %>% arrange(desc(no)) %>% mutate(Percentage=no/sum(no)*100) %>% slice_head(n=6)


library(dplyr)
inf <- bogota %>% filter(OCI==1) %>% group_by(EI) %>% summarise(no = sum(FEX_C18.x)) 

inf <- bogota %>% group_by(EI) %>% summarise(no = sum(FEX_C18.x)) 




EM21F$DIV_n[EM21F$DIV == "00"] <- "00"
