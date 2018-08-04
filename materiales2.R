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




#checar este material
entradas[entradas$Presentacion == "150" ,]$Presentacion <- "12X150G"

entradas <- ddply(entradas,
                  .(Fecha, Tipo_entrada, Presentacion, Clave_almacen, Clave_proveedor, Clave_material), 
                  summarize, Total = sum(Cantidad))%>%
  rename(Tipo = Tipo_entrada, Clave_productor = Clave_proveedor)



rm(hentradas,bentradas)


#salidas
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
  ddply(.(Fecha, Tipo_salida, Presentacion, Clave_almacen, Clave_productor, Clave_material), 
        summarize, Total = sum(Cantidad))%>%
  rename(Tipo = Tipo_salida)

rm(bsalidas,hsalidas, salidas_material)


#inventario almacenes
origen <- as.Date("2017-09-01")
inventario_entradas <- ddply(entradas[entradas$Fecha > origen & entradas$Clave_almacen == 1,],.(Clave_material), summarize, Entradas = sum(Total))
inventario_salidas <-  ddply(salidas[salidas$Fecha > origen & salidas$Clave_almacen == 1,],.(Clave_material), summarize, Salidas = sum(Total))
inventarios <- merge(inventario_entradas, inventario_salidas, by = "Clave_material", all.x = TRUE, all.y = TRUE)%>%
  mutate(Saldo = Entradas + Salidas)


#source("Productos.R")

omar <- read.csv("170901-180725.csv", encoding = "UTF-8", stringsAsFactors = FALSE)[c(-1),]%>%
  rename(Clave_material = X.U.FEFF.Clave)%>%
  mutate(Entradas_omar = as.integer(gsub("\\,{1}","",.$Entradas)), Salidas_omar = as.integer(gsub("\\,{1}","",.$Salidas)))%>%
  filter(!is.na(Clave_material))%>%
  select(-Entradas, -Salidas, -Saldo)
  
 comparativo <- merge(omar,inventarios, by= "Clave_material")
 
 comparativo[is.na(comparativo)] <- 0
 
 comparativo <- mutate(comparativo, Salidas_dif = Salidas_omar - Salidas, 
                       Entradas_dif = Entradas_omar - Entradas)
 
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
 
 flujo_materiales <- rbind(entradas, salidas)
 
 