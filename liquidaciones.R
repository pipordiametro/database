library(RODBC)
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)

source("funciones.R")
source("productos.R")
source("fruta.R")



rm(reembales, productos, entradas_fruta)            


presentaciones <- presentaciones%>%
  select(Clave_presentacion, Presentacion)

frutas <- frutas%>%
  select(Clave_fruta, Fruta)
            
precios <- myfetch("tbPreciosEstimadosSalidas")%>%
  transmute(Folio = intNum_fol, Clave_presentacion = intCla_pre, Clave_fruta = intCla_fru ,
            Cantidad = intCan_tid, Precio_real = floPre_rea, Comision_aduanal = floCus_tom, 
            Manejo = floHan_dliD, Handlio = floHan_dliO, Comision_ventas = floSal_es,
            Caja = floCaj_a, Frio = floFri_o, Mano_obra = floMan_obr, Energia = floEne_rgi)%>%
  merge(presentaciones[,c("Clave_presentacion", "Presentacion")], all.x = TRUE)%>%
  merge(frutas[,c("Clave_fruta","Fruta")], all.x = TRUE)%>%
  mutate(Producto = paste(Fruta, Presentacion))%>%
  select(-Clave_fruta, - Clave_presentacion, -Fruta, -Presentacion)%>%
  mutate(Importe = Cantidad*Precio_real)


  

#--------------------LIQUIDACIONE ABY-----------------------------------

liq1516 <- read.csv("Liquid/15-16.csv", stringsAsFactors = FALSE)%>%
  select(Folio, Fecha, TOTAL, CLIENTE, Destino, LIQUIDACION,ANTICIPO)

liq1617 <- read.csv("Liquid/16-17.csv", stringsAsFactors = FALSE)%>%
  select(Folio, Fecha, TOTAL, CLIENTE, Destino, LIQUIDACION,ANTICIPO)

liquidaciones <- rbind(liq1516, liq1617)%>%
  mutate(LIQUIDACION = as.double(gsub(",","",LIQUIDACION)),
         ANTICIPO = as.double(gsub(",","",ANTICIPO)))

rm(liq1516, liq1617)

names(liquidaciones) <- paste0("l_",names(liquidaciones))

liquidaciones[is.na(liquidaciones$l_LIQUIDACION),]$l_LIQUIDACION <- 0

liquidaciones[is.na(liquidaciones$l_ANTICIPO),]$l_ANTICIPO <- 0

liquidaciones$l_Pago <- liquidaciones$l_LIQUIDACION + liquidaciones$l_ANTICIPO

#--------------------------liquidaciones sistema---------------------------



liquidaciones_sistema <- salidas_fruta%>% 
  filter(Fecha>= as.Date("2015-09-01"))%>%
  ddply(.(Folio,Fecha,Destino, Cliente, Pais,Producto), summarize, Cantidad = sum(Cantidad))%>%
  spread(Producto, Cantidad, fill = 0)%>%
  mutate(Total = rowSums(.[6:length(names(.))]))%>%
  merge(liquidaciones[,c("l_Folio","l_Pago" )], 
        by.x = "Folio", by.y = "l_Folio", all.x = TRUE)%>%
  merge(precios,all.x = TRUE, all.y = TRUE)%>%
  filter(Fecha > as.Date("2015-09-01"))
  





#%>%
  #filter(Diferencia == 0 & !Pais %in% c("MEXICO", "NINGUNO", "TEPEJI DEL RIO"))%>%
  #transmute(Folio = Folio, Fecha = Fecha, Destino = Destino, Producto = Producto, Cantidad = Cantidad, 
  #         Cliente = Cliente, Pais = Pais, Liquidacion = l_LIQUIDACION + l_ANTICIPO)
  


#----------------------------------------checar 4 registros duplicados

prueba <- m
