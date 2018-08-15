## @knitr datos

source("funciones.R")
library(stringr)
library(kableExtra)
# inventario inicial

# entradas_material

compras <-read.csv("compras1718.csv")


entradas <- select(compras, Fecha, Cantidad, Embalaje, Unitario_iva, Material)%>%
  filter(Material %in% c("CLAMS", "CAJAS"))

compras <- ddply(entradas,.(Embalaje,Material), summarize, Compras = sum(Cantidad))%>%
  rename(Presentacion = Embalaje, Tipo_material = Material)%>%
  mutate(Tipo_material = ifelse(Tipo_material == "CLAMS", "CLAMSHELL", 
                                ifelse(Tipo_material == "CAJAS", "CAJA EMP.", Tipo_material)))


source("fruta.R")

rm(clientes, destinos, frutas, productos, entradas, reembales)

inicio_temporada <- as.Date("2017-09-01")


#  enviados
cajas <- salidas_fruta%>%
  select(Fecha, Presentacion,Cantidad)%>%
  filter(Fecha >= inicio_temporada)%>%
  ddply(.(Presentacion), summarize, Enviados = sum(Cantidad))%>%
  mutate(Tipo_material = "CAJA EMP.")

clams <- salidas_fruta%>%
  select(Fecha, Cantidad, Peso, Unidad, Clams)%>%
  mutate(Cantidad = Cantidad*Clams,
         Presentacion = paste0(Peso,Unidad))%>%
  select(Fecha, Presentacion, Cantidad)%>%
  filter(Fecha >= inicio_temporada)%>%
  ddply(.(Presentacion), summarize, Enviados = sum(Cantidad))%>%
  mutate(Tipo_material = "CLAMSHELL")


enviados <- rbind(cajas,clams)

rm(cajas, clams)


enviados[enviados$Presentacion == "125G",]$Presentacion <-  "4.4OZ"
enviados[enviados$Presentacion == "12X125G",]$Presentacion <-  "12X4.4OZ"

enviados <- ddply(enviados,.(Presentacion, Tipo_material), summarize, Enviados = sum(Enviados))

#Productores


materiales <- myfetch("tbMateriales")%>%
  transmute(Clave_material = intCla_mat, Marca = strMar_ca,
            Descripcion_corta = strDes_cor, Fabricante = strFab_ric, 
            Unidad = strUni_med, Tipo_material = strTip_o, Clave_presentacion = intCla_pre)%>%
  merge(presentaciones[,c("Clave_presentacion", "Presentacion")], by = "Clave_presentacion")

rm(presentaciones)

##entradas

hentradas <- myfetch("tbAlmEnt")

bentradas <- myfetch("tbAlmEntReg")

names(hentradas) <- paste0("h",names(hentradas))

names(bentradas) <- paste0("b",names(bentradas))

devoluciones <- merge(hentradas,bentradas, 
                  by.x = "hintNum_reg", by.y= "bintNum_reg", all.x = TRUE)%>%
  filter(bstrCan_cel == "NO")%>%
  transmute(Clave_almacen = hintCla_alm, Tipo_entrada =  hstrTip_ent, Fecha = as.Date(hfecFec_fac), 
            Tipo_proveedor = hstrTip_prov, Cantidad = bintCan_tid , Clave_material = bintCla_mat,
            Clave_proveedor = hintCla_prov)%>%
  merge(materiales, 
        by = "Clave_material", all.x = TRUE)%>%
  filter(Tipo_entrada == "DEVOLUCION", Fecha >  inicio_temporada)%>%
  select(Fecha, Cantidad, Clave_proveedor, Tipo_material, Presentacion)%>%
  mutate(Presentacion = ifelse(Tipo_material == "CLAMSHELL",
                               str_match(Presentacion,
                                         "[1,2,5,8,0,6,4.]{1,3}[O,Z,G]{1,2}"), Presentacion))%>%
  ddply(.(Presentacion, Tipo_material), summarize, Devoluciones = sum(Cantidad))

devoluciones[devoluciones$Presentacion == "125G",]$Presentacion <-  "4.4OZ"
devoluciones[devoluciones$Presentacion == "12X125G",]$Presentacion <-  "12X4.4OZ"

devoluciones <- devoluciones%>%
  ddply(.(Presentacion, Tipo_material), summarize, Devoluciones = sum(Devoluciones))
rm(hentradas, bentradas)

#entregas de material

hsalidas <- myfetch("tbAlmSal")
bsalidas <- myfetch("tbAlmsalReg")
names(hsalidas) = paste0("h",names(hsalidas))
names(bsalidas) = paste0("b",names(bsalidas))

entregas <- merge(hsalidas,bsalidas, by.x = "hintNum_reg", 
                          by.y = "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  transmute(Clave_almacen = hintCla_alm, Tipo_salida = hstrTip_sal, Clave_productor = hintCla_pro,
            Fecha = as.Date(hfecFec_not), Cantidad = bintCan_tid, Clave_material = bintCla_mat)%>%
  merge(materiales, by = "Clave_material")%>%
  filter(Tipo_salida %in% c("PRESTAMO", "."), Fecha >= inicio_temporada)%>%
  mutate(Presentacion = ifelse(Tipo_material == "CLAMSHELL",
                               str_match(Presentacion,
                                         "[1,2,5,8,0,6,4.]{1,3}[O,Z,G]{1,2}"), Presentacion))%>%
  filter(Tipo_material %in% c("CLAMSHELL", "CAJA EMP."))%>%
  ddply(.(Presentacion, Tipo_material), summarize, Entregas = sum(Cantidad))

entregas[entregas$Presentacion == "125G",]$Presentacion <-  "4.4OZ"
entregas[entregas$Presentacion == "12X125G",]$Presentacion <-  "12X4.4OZ"

entregas <- entregas%>%
  ddply(.(Presentacion, Tipo_material), summarize, Entregas = sum(Entregas))
#ventas
ventas <- merge(hsalidas,bsalidas, by.x = "hintNum_reg", 
                                     by.y = "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  transmute(Clave_almacen = hintCla_alm, Tipo_salida = hstrTip_sal, Clave_productor = hintCla_pro,
            Fecha = as.Date(hfecFec_not), Cantidad = bintCan_tid, Clave_material = bintCla_mat)%>%
  merge(materiales, by = "Clave_material")%>%
  filter(Tipo_salida %in% c("VENTA"), Fecha >= inicio_temporada)%>%
  mutate(Presentacion = ifelse(Tipo_material == "CLAMSHELL",
                               str_match(Presentacion,
                                         "[1,2,5,8,0,6,4.]{1,3}[O,Z,G]{1,2}"), Presentacion))%>%
  ddply(.(Presentacion, Tipo_material), summarize, Ventas = sum(Cantidad))%>%
  filter(Tipo_material != "TAPA")



rm(bsalidas,hsalidas)


#entradas de fruta


cajas <- entradas_fruta%>%
  select(Fecha, Presentacion, Cantidad)%>%
  filter(Fecha >= inicio_temporada)%>%
  ddply(.(Presentacion), summarize, Recepcion = sum(Cantidad))%>%
  mutate(Tipo_material = "CAJA EMP.")

clams <- entradas_fruta%>%
  select(Fecha, Cantidad, Peso, Unidad, Clams)%>%
  mutate(Cantidad = Cantidad*Clams,
         Presentacion = paste0(Peso,Unidad))%>%
  select(Fecha, Presentacion, Cantidad)%>%
  filter(Fecha >= inicio_temporada)%>%
  ddply(.(Presentacion), summarize, Recepcion = sum(Cantidad))%>%
mutate(Tipo_material = "CLAMSHELL")
recepcion <-  rbind(cajas,clams)

recepcion[recepcion$Presentacion == "125G",]$Presentacion <-  "4.4OZ"
recepcion[recepcion$Presentacion == "12X125G",]$Presentacion <-  "12X4.4OZ"

recepcion <- recepcion%>%
  ddply(.(Presentacion, Tipo_material), summarize, Recepcion = sum(Recepcion))

rm(cajas,clams, entradas_fruta, salidas_fruta, materiales)


posecion_productores <- merge(entregas, recepcion, by ="Presentacion",
                              all.x = TRUE, all.y = TRUE, fill = 0)%>%
  merge(devoluciones, by = "Presentacion",
        all.x = TRUE, all.y = TRUE, fill = 0)

posecion_productores[is.na(posecion_productores)] <- 0

  
posecion_productores <- posecion_productores%>%
  mutate(Posecion_productores = Entregas - Recepcion - Devoluciones)%>%
  select(-Tipo_material.x, -Tipo_material)%>%
  rename(Tipo_material = Tipo_material.y)%>%
  arrange(Tipo_material)%>%
  select(-Tipo_material)%>%
  filter(Presentacion != "0X0OZ")




compras_salidas <- merge(compras, enviados, by = "Presentacion", all = TRUE)%>%
  mutate(Diferencia = Compras-Enviados)%>%
  select(-Tipo_material.y)%>%
  rename(Tipo_material = Tipo_material.x)%>%
  arrange(Tipo_material)%>%
  select(-Tipo_material)%>%
  filter(!is.na(Presentacion))



resumen <- merge(compras, ventas, all = TRUE, by = "Presentacion")%>%
  merge(enviados, all = TRUE, by = "Presentacion")%>%
  merge(posecion_productores[,c("Presentacion", "Posecion_productores")])%>%
  select(-Tipo_material.y, -Tipo_material)%>%
  rename(Tipo_material = Tipo_material.x)

resumen[is.na(resumen)] <- 0

resumen <- resumen%>%
  mutate(Saldo = ifelse(Posecion_productores >0,
                        Compras - Ventas - Enviados - Posecion_productores,
                        Compras - Ventas - Enviados))%>%
  arrange(Tipo_material)%>%
  select(-Tipo_material)


  






