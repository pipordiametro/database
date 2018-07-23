
source("fruta.R")
library(ggplot2)
library(plotly)

salidas_fruta <- salidas_fruta%>%
  select(Folio ,Fecha , Destino, Cliente, Pais,
         Clave_fruta, Clave_presentacion, Cantidad, Producto, Clave_producto)

precios <- myfetch("tbPreciosEstimadosSalidas")%>%
  transmute(Folio = intNum_fol, Clave_presentacion = intCla_pre, Clave_fruta = intCla_fru ,
            Cantidad = intCan_tid, Precio_real = floPre_rea, Comision_aduanal = floCus_tom, 
            Manejo = floHan_dliD, Handlio = floHan_dliO, Comision_ventas = floSal_es,
            Caja = floCaj_a, Frio = floFri_o, Mano_obra = floMan_obr, Energia = floEne_rgi)


salidas <- salidas_fruta%>% 
  filter(Fecha>= as.Date("2015-09-01"))%>%
  ddply(.(Folio,Fecha,Destino, Cliente, Pais, Clave_presentacion, Clave_fruta, Producto), 
        summarize, Cantidad = sum(Cantidad))



folios_fecha <- unique(salidas_fruta[,c("Folio", "Fecha")])

resumen_precios <- precios%>%
  merge(folios_fecha)%>%
  group_by(Folio, Fecha) %>%
  summarize(Liquidacion = sum(Cantidad*Precio_real))
  
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

liquidaciones$Liquidacion_aby <- liquidaciones$l_LIQUIDACION + liquidaciones$l_ANTICIPO

comparativo <- merge(resumen_precios, liquidaciones, 
                     by.x= "Folio", by.y = "l_Folio", all.x = TRUE, all.y = TRUE)%>%
  select(Folio, Liquidacion, Liquidacion_aby, l_Fecha)%>%
  merge(folios_fecha, all.x = TRUE, all.y = TRUE)%>%
  filter(!is.na(Liquidacion) & !is.na((Liquidacion_aby)))%>%
  mutate(Diferencia = Liquidacion - Liquidacion_aby)

salidas_uno <- unique(salidas_fruta[,c("Folio", "Producto")])%>%
  group_by(Folio)%>%
  summarize(Cuenta = n())%>%
  filter(Cuenta ==1 )

precios_check <- salidas_fruta%>%
  filter(Folio %in% salidas_uno$Folio)%>%
  ddply(.(Folio, Fecha, Clave_producto, Pais, Cliente), summarize, Total = sum(Cantidad))%>%
  merge(liquidaciones, by.x = "Folio", by.y = "l_Folio")%>%
  filter(Pais != "MEXICO" & Liquidacion_aby != 0 )%>%
  mutate(Precio_unitario = Liquidacion_aby/Total)%>%
  merge(productos, by = "Clave_producto")%>%
  filter(Clave_fruta %in% c(1,2,4,10))%>%
  mutate(Cajas6oz = Total*Fraccion6oz, Precio_unitario6oz = Liquidacion_aby/Total)%>%
  select(Folio, Fecha, Clave_producto, Producto, Fruta, Cliente, Pais, Total, 
          Precio_unitario, Cajas6oz, Precio_unitario6oz)%>%
  filter(Pais != "TEPEJI DEL RIO")%>%
  filter(!Folio %in% c(1522, 543, 601, 1161, 1023))
  
  source("precios_usda.R")  

precios_semanales <- precios_diarios%>%
  gather(Fruta, Precio_usda, -Fecha)%>%
  mutate(Semana = as.integer(format(Fecha,format =  "%U")), 
         Year = as.integer(format(Fecha, format = "%Y" )),
         Semanats = (Year - 2010)*52 + Semana)%>%
  ddply(.(Fruta, Semanats), summarize, Promedio_usda = mean(Precio_usda))
 

precios_semanales[precios_semanales$Fruta == "ARA CN",]$Fruta <- "ARA BER"

precios_cor <- merge(precios_check%>%
                       mutate(Semana = as.integer(format(Fecha,format =  "%U")), 
                              Year = as.integer(format(Fecha, format = "%Y" )),
                              Semanats = (Year - 2010)*52 + Semana), 
                     precios_semanales, by = c("Semanats", "Fruta"), all.x = TRUE)%>%
  filter(Fruta != "ZAR ORG")%>%
  filter(Cliente == "HBF INTERNATIONAL LLC")
  
ggplotly(ggplot(precios_cor, aes(x = Promedio_usda, y = Precio_unitario, colour = Cliente)) + 
  geom_point() + geom_abline(slope = 1))
                     