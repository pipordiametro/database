source("fruta.R")

#Productores
inicio_temporada <- as.Date("2017-09-01")

materiales <- myfetch("tbMateriales")%>%
  transmute(Clave_material = intCla_mat, Marca = strMar_ca,
            Descripcion_corta = strDes_cor, Fabricante = strFab_ric, 
            Unidad = strUni_med, Tipo_material = strTip_o, Clave_presentacion = intCla_pre)%>%
  merge(presentaciones[,c("Clave_presentacion", "Presentacion")], by = "Clave_presentacion")

rm(presentaciones, clientes, destinos, frutas, productos, reembales)


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
  filter(Tipo_material %in% c("CLAMSHELL", "CAJA EMP."))

entregas[entregas$Presentacion == "125G",]$Presentacion <-  "4.4OZ"
entregas[entregas$Presentacion == "12X125G",]$Presentacion <-  "12X4.4OZ"

rm(hsalidas, bsalidas)

#entradas de fruta


cajas <- entradas_fruta%>%
  select(Fecha, Presentacion, Clave_productor, Cantidad)%>%
  filter(Fecha >= inicio_temporada)%>%
  mutate(Tipo_material = "CAJA EMP.")

clams <- entradas_fruta%>%
  select(Fecha, Clave_productor, Cantidad, Peso, Unidad, Clams)%>%
  mutate(Cantidad = Cantidad*Clams,
         Presentacion = paste0(Peso,Unidad))%>%
  select(Fecha, Presentacion, Clave_productor, Cantidad)%>%
  filter(Fecha >= inicio_temporada)%>%
  mutate(Tipo_material = "CLAMSHELL")

recepcion <-  rbind(cajas,clams)

recepcion[recepcion$Presentacion == "125G",]$Presentacion <-  "4.4OZ"
recepcion[recepcion$Presentacion == "12X125G",]$Presentacion <-  "12X4.4OZ"



rm(cajas,clams, entradas_fruta)


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
                                         "[1,2,5,8,0,6,4.]{1,3}[O,Z,G]{1,2}"), Presentacion))


devoluciones[devoluciones$Presentacion == "125G",]$Presentacion <-  "4.4OZ"
devoluciones[devoluciones$Presentacion == "12X125G",]$Presentacion <-  "12X4.4OZ"

rm(hentradas, bentradas)

#  enviados
cajas <- salidas_fruta%>%
  select(Fecha, Presentacion,Cantidad)%>%
  filter(Fecha >= inicio_temporada)%>%
  mutate(Tipo_material = "CAJA EMP.")

clams <- salidas_fruta%>%
  select(Fecha, Cantidad, Peso, Unidad, Clams)%>%
  mutate(Cantidad = Cantidad*Clams,
         Presentacion = paste0(Peso,Unidad))%>%
  select(Fecha, Presentacion, Cantidad)%>%
  filter(Fecha >= inicio_temporada)%>%
  mutate(Tipo_material = "CLAMSHELL")

enviados <- rbind(cajas,clams)

rm(cajas, clams)

enviados[enviados$Presentacion == "125G",]$Presentacion <-  "4.4OZ"
enviados[enviados$Presentacion == "12X125G",]$Presentacion <-  "12X4.4OZ"

rm(salidas_fruta)
