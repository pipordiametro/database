source("funciones.R")
library(ggplot2)
library(plotly)
library(plyr)
#list.files("proyecto/")


#---------------ENTRADAS---------------------
materiales <- myfetch("tbMateriales")%>%
  mutate(strPre_sen =as.character(strPre_sen))


#---------------------PREGUNTAR VACIOS-----------------------

materiales[materiales$strPre_sen == "",]$strPre_sen <- "Vacios"

hentradas <- myfetch("tbAlmEnt")

bentradas <- myfetch("tbAlmEntReg")

names(hentradas) <- paste0("h",names(hentradas))

names(bentradas) <- paste0("b",names(bentradas))

entradas <- merge(hentradas,bentradas, 
                  by.x = "hintNum_reg", by.y= "bintNum_reg", all.x = TRUE)%>%
  filter(bstrCan_cel == "NO")%>%
  select(hfecCre_aci, bintCla_mat, bintCan_tid)%>%
  merge(materiales, 
        by.x = "bintCla_mat", by.y ="intCla_mat", all.x = TRUE)%>%
  transmute(Fecha = as.Date(hfecCre_aci), Cantidad = bintCan_tid,  Presentacion = strPre_sen)

entradas[entradas$Presentacion == "6 OZ",]$Presentacion <- "6OZ"
entradas[entradas$Presentacion == "8X12 OZ",]$Presentacion <- "8X12OZ" 
entradas[entradas$Presentacion == "12X125 G",]$Presentacion <- "12X4.4OZ" 
entradas[entradas$Presentacion == "125 G",]$Presentacion <- "4.4OZ" 
entradas[entradas$Presentacion == "12X4.4 OZ",]$Presentacion <- "12X4.4OZ"
entradas[entradas$Presentacion == "4.4 OZ",]$Presentacion <- "4.4OZ"
entradas[entradas$Presentacion == "18 OZ",]$Presentacion <- "18OZ"
entradas[entradas$Presentacion == "12 X 6OZ",]$Presentacion <- "12X6OZ"
entradas[entradas$Presentacion == "12 OZ",]$Presentacion <- "12OZ"
entradas[entradas$Presentacion == "12 X 18 OZ",]$Presentacion <- "12X18OZ"
entradas[entradas$Presentacion == "150 G" ,]$Presentacion <- "150G"
entradas[entradas$Presentacion == "12 X 150 G" ,]$Presentacion <- "12X150G"
entradas[entradas$Presentacion == "12 X 6 OZ" ,]$Presentacion <- "12X6OZ"

#------------------PREGUNTAR ESTE MATERIAL---------------------
entradas[entradas$Presentacion == "150" ,]$Presentacion <- "12X150G"

entradas <- ddply(entradas,.(Fecha,Presentacion), summarize, Total = sum(Cantidad))



rm(hentradas,bentradas)


#----------------------SALIDAS---------------


hsalidas <- myfetch("tbAlmSal")
bsalidas <- myfetch("tbAlmsalReg")
names(hsalidas) = paste0("h",names(hsalidas))
names(bsalidas) = paste0("b",names(bsalidas))
salidas <- merge(hsalidas,bsalidas, by.x = "hintNum_reg", 
                 by.y = "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  select(hfecCre_aci, bintCla_mat, bintCan_tid)%>%
  mutate(bintCan_tid = -bintCan_tid)



rm(bsalidas,hsalidas)
#-------------fruta----------------
hfruta <- myfetch("tbRecFruEmp")
bfruta <- myfetch("tbRecFruEmpReg")
names(hfruta) <- paste0("h",names(hfruta))
names(bfruta) <- paste0("b",names(bfruta))
productos <- myfetch("tbProductos")
names(productos) <- paste0("p", names(productos))

presentaciones <- myfetch("tbPresentaciones")

embalajes <- merge(productos, presentaciones, 
      by.x = "pintCla_pre", by.y = "intCla_pre")%>%
  select(pstrCla_prod, intCan_tid, pintCla_fru, intPes_o, strUni_med)

fruta <- merge(hfruta, bfruta, 
      by.x = "hintNum_reg", by.y =  "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  transmute(Fecha = as.Date(bfecCre_aci),
            intCla_prod = bintCla_prod,
            Recibidas = bintCan_tid,
            Rechazadas = bintCan_rec)%>%
  merge(embalajes, 
        by.x = "intCla_prod", by.y = "pstrCla_prod" , all.x = TRUE )%>%
  mutate(Presentacion = paste0(intCan_tid,"X", intPes_o,strUni_med),
         Cantidad = intCan_tid, Peso = intPes_o,
         Unidad = strUni_med,
         Aceptadas = as.integer(Recibidas)-as.integer(Rechazadas))%>%
  select(Fecha, Aceptadas, Presentacion, Cantidad, Unidad, Peso)

cajas <- fruta%>%
  select(Fecha, Aceptadas, Presentacion)%>%
  mutate(Tipo = Presentacion)%>%
  select(-Presentacion)
         
clams <- fruta%>%
  select(Fecha, Aceptadas, Peso, Unidad, Cantidad)%>%
  mutate(Aceptadas = Aceptadas*Cantidad,
         Tipo = paste0(Peso,Unidad))%>%
  select(Fecha, Aceptadas, Tipo)
  
enviados <- rbind(cajas,clams)%>%
  ddply(.(Fecha,Tipo), summarize, Total = sum(Aceptadas))%>%
  transmute(Fecha = Fecha, Presentacion = Tipo, Total = -Total)

enviados[enviados$Presentacion == "125G",]$Presentacion <-  "4.4OZ"
enviados[enviados$Presentacion == "12X125G",]$Presentacion <-  "12X4.4OZ"

rm(hfruta,bfruta)
rm(fruta, cajas, clams)

#-----------------ANALISIS---------------------

datos <- rbind(entradas,enviados)%>%
  filter(Fecha >= as.Date("2015-08-14"))

flujos <- ddply(datos,.(Fecha,Presentacion), 
                summarize, Total =sum(Total))

for (var in unique(flujos$Presentacion)){
  assign(paste0(var,".plot"), ggplotly(ggplot(flujos[flujos$Presentacion == var,], 
                                aes(x = Fecha, y = Total)) + geom_line(colour = "cyan") + 
                                  labs(title = var)))
}











