source("funciones.R")

materiales <- myfetch("tbMateriales")%>%
  transmute(Presentacion =as.character(strPre_sen), Clave_material = intCla_mat, Marca = strMar_ca,
            Descripcion_corta = strDes_cor, Fabricante = strFab_ric, 
            Unidad = strUni_med, Tipo_material = strTip_o, Clave_presentacion = intCla_pre)

materiales[materiales$Presentacion == "",]$Presentacion <- "Vacios"

#entradas

hentradas <- myfetch("tbAlmEnt")

bentradas <- myfetch("tbAlmEntReg")

names(hentradas) <- paste0("h",names(hentradas))

names(bentradas) <- paste0("b",names(bentradas))

entradas <- merge(hentradas,bentradas, 
                  by.x = "hintNum_reg", by.y= "bintNum_reg", all.x = TRUE)%>%
  filter(bstrCan_cel == "NO")%>%
  transmute(Clave_almacen = hintCla_alm, Tipo_entrada =  hstrTip_ent, Fecha = as.Date(hfecFec_fac), 
            Tipo_proveedor = hstrTip_prov, Cantidad = bintCan_tid , Clave_material = bintCla_mat,
            Clave_proveedor = hintCla_prov)%>%
    merge(materiales, 
        by = "Clave_material", all.x = TRUE)

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


#checar este material
entradas[entradas$Presentacion == "150" ,]$Presentacion <- "12X150G"

entradas <- ddply(entradas,
                  .(Fecha, Tipo_entrada, Presentacion, Clave_almacen, Clave_proveedor), 
                  summarize, Total = sum(Cantidad))%>%
  rename(Tipo = Tipo_entrada)



rm(hentradas,bentradas)



hsalidas <- myfetch("tbAlmSal")
bsalidas <- myfetch("tbAlmsalReg")
names(hsalidas) = paste0("h",names(hsalidas))
names(bsalidas) = paste0("b",names(bsalidas))

salidas_material <- merge(hsalidas,bsalidas, by.x = "hintNum_reg", 
                 by.y = "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  transmute(Clave_almacen = hintCla_alm, Tipo_salida = hstrTip_sal, Clave_productor = hintCla_pro,
            Fecha = as.Date(hfecFec_not), Cantidad = bintCan_tid, Clave_material = bintCla_mat)%>%
  merge(materiales, by = "Clave_material")

salidas <- salidas_material%>%
  ddply(.(Fecha, Tipo_salida, Presentacion, Clave_almacen, Clave_productor), 
        summarize, Total = sum(Cantidad))%>%
  rename(Tipo = Tipo_salida)

rm(bsalidas,hsalidas, salidas_material)



#entradas fruta

source("fruta.R")

rm(clientes, destinos, frutas, presentaciones, productos)

cajas <- entradas_fruta%>%
  mutate(Tipo = "")%>%
  select(Fecha, Tipo, Presentacion, Clave_acopio, Clave_productor, Aceptadas)
  
  
  

clams <- entradas_fruta%>%
  select(Fecha, Aceptadas, Peso, Unidad, Aceptadas, Clams , Clave_acopio, Clave_productor)%>%
  mutate(Aceptadas = Aceptadas*Clams,
         Presentacion = paste0(Peso,Unidad), Tipo = "")%>%
  select(-Clams, - Unidad)%>%
  select(Fecha, Tipo, Presentacion, Clave_acopio, Clave_productor, Aceptadas)


enviados <- rbind(cajas,clams)%>%
  ddply(.(Fecha, Tipo, Presentacion, Clave_acopio, Clave_productor), summarize, Total = -sum(Aceptadas))%>%
  rename(Clave_almacen = Clave_acopio)
  

enviados[enviados$Presentacion == "125G",]$Presentacion <-  "4.4OZ"
enviados[enviados$Presentacion == "12X125G",]$Presentacion <-  "12X4.4OZ"

enviados$Tipo <- "ENVIADAS"

rm(entradas_fruta, cajas, clams)

flujo_materiales <- rbind(entradas, salidas, enviados)

