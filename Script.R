a
###################### LEER LAS BASES DE DATOS ###############################
setwd("C:/Users/karme/Desktop/Prácticas/Datos/Mercado Laboral/Mayo/DTA")
library(haven)

Gen <- read_dta("Características generales, seguridad social en salud y educación.DTA")
FT <- read_dta("Fuerza de trabajo.DTA")
Ocup <- read_dta("Ocupados.DTA")


base<- "C:\\Users\\karme\\Desktop\\Prácticas\\Datos\\Encuesta Multiproposito\\Microdatos\\Multiproposito_completa.rds"
EM21F <- readRDS(base)

ML <- merge(Gen, Ocup, by = "DIRECTORIO", "SECUENCIA_P", "ORDEN")

a

