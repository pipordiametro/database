library(reshape2)
library(scales)
library(plyr)
library(RODBC)
library(ggplot2)
library(dplyr)
library(plotly)
library(kableExtra)
library(knitr)
library(fpp)
library(PerformanceAnalytics)
library(RCurl)
library(XML)
#load registros
prod <- read.csv("Anaberries/prod.csv")%>%
  filter(strCan_cel == "NO")%>%
  select(intCla_pro, strNom_bre, strApe_pat, strApe_mat)
names(prod)[2] <- "Nombre"

registros <- read.csv("Anaberries/registros.csv")%>%
  mutate(Fecha = as.Date(Fecha))
            




#load Anaberries
anab <- read.csv("Anaberries/anaberries.csv", stringsAsFactors = FALSE)
anab$Year <- as.integer(substr(anab$Fecha, 3, 6))
anab$Semanats <- anab$Semana + (anab$Year - 2013)*52
anab <- select( anab, Year, Semana, Semanats, Enviado.Real..cajas., Pronóstico..cajas., USDA..cajas. )%>%
  transmute(Year, 
            Semana,
            Semanats = ifelse(Year == 2015, Semanats - 1, Semanats), 
            Real = as.numeric(gsub(",", "", anab$Enviado.Real..cajas., fixed = TRUE)),
            Pronostico = as.numeric(gsub(",", "", Pronóstico..cajas., fixed = TRUE)),
            USDA = as.numeric(gsub(",", "", anab$USDA..cajas., fixed = TRUE)))#%>%
#filter(!(Cajasanab == 0))
