library(RODBC)
library(plyr)
library(dplyr)
library(reshape2)

source("funciones.R")

precios1 <- myfetch("tbPreciosEstimadosSalidas")

precios2 <- myfetch("tbPreciosSalidas")

hsalidas <- myfetch("tbSalFruEmp")
names(hsalidas) <- paste0("h", names(hsalidas))

bsalidas <- myfetch("tbSalFruEmpReg")
names(bsalidas) <- paste0("b",names(bsalidas))

clientes <-  myfetch("tbClientes")%>%
  select(intCla_cli, strNom_bre)

destinos <-  myfetch("tbDestinos")%>%
  transmute(Pais = strPai_s, Clave_destino= intCla_des)

productos <- myfetch("tbProductos")
names(productos) <- paste0("p", names(productos))

presentaciones <- myfetch("tbPresentaciones")

embalajes <- merge(productos, presentaciones, 
                   by.x = "pintCla_pre", by.y = "intCla_pre")%>%
  select(pstrNom_bre, pstrCla_prod, intCan_tid, pintCla_fru, intPes_o, strUni_med)%>%
  transmute(Clave_producto = pstrCla_prod, 
            Producto = paste0(pstrNom_bre,intCan_tid,"X",intPes_o,strUni_med))

liq1516 <- read.csv("Liquid/15-16.csv", stringsAsFactors = FALSE)%>%
  select(Folio, Fecha, TOTAL, CLIENTE, Destino, LIQUIDACION,ANTICIPO)

liq1617 <- read.csv("Liquid/16-17.csv", stringsAsFactors = FALSE)%>%
  select(Folio, Fecha, TOTAL, CLIENTE, Destino, LIQUIDACION,ANTICIPO)

liquidaciones <- rbind(liq1516, liq1617)%>%
  mutate(LIQUIDACION = as.double(gsub(",","",LIQUIDACION)),
         ANTICIPO = as.double(gsub(",","",ANTICIPO)))

names(liquidaciones) <- paste0("l_",names(liquidaciones))
  

liquidaciones[is.na(liquidaciones$l_LIQUIDACION),]$l_LIQUIDACION <- 0
liquidaciones[is.na(liquidaciones$l_ANTICIPO),]$l_ANTICIPO <- 0

salidas <- merge(hsalidas, bsalidas, 
                 by.x = "hintNum_reg", by.y = "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  transmute(Folio = hintNum_fol, Fecha = as.Date(hfecFec_sal),
            Clave_cliente = hintCla_cli, Clave_destino =  hintCla_des,
            Destino = hstrDes_tin,Pallets =  hstrPal_let, No_pallet = bintNum_pal,
            Clave_producto = bstrCla_prod, Cantidad = bintCan_tid)%>%
  merge(clientes, by.x = "Clave_cliente", by.y = "intCla_cli" , all.x = TRUE)%>%
  mutate(Cliente = strNom_bre)%>% select(-Clave_cliente, -strNom_bre)%>%
  merge(destinos, by.x = "Clave_destino", by.y = "Clave_destino", all.x = TRUE)%>%
  select(-Clave_destino)%>%
  merge(embalajes, by.x = "Clave_producto", by.y = "Clave_producto", all.x = TRUE)%>%
  select(-Clave_producto)%>% 
  filter(Fecha>= as.Date("2015-09-01") & Fecha < as.Date("2017-09-01" ))%>%
  ddply(.(Folio,Fecha,Destino, Cliente, Pais,Producto), summarize, Cantidad = sum(Cantidad))%>%
  spread(Producto, Cantidad, fill = 0)%>%
  mutate(Total = rowSums(.[6:length(names(.))]))%>%
  merge(liquidaciones[,c("l_TOTAL", "l_Folio" )], 
        by.x = "Folio", by.y = "l_Folio", all.x = TRUE)%>%
  mutate( Diferencia = as.integer(Total)-as.integer(l_TOTAL))%>%
  filter(Diferencia == 0 & !Pais %in% c("MEXICO", "NINGUNO", "TEPEJI DEL RIO"))
  


%>%
  transmute(FOlio = Folio, Fecha = Fecha, Destino = Destino, Producto = Producto, Cantidad = Cantidad, 
            Cliente = Cliente, Pais = Pais, Liquidacion = l_LIQUIDACION + l_ANTICIPO)
  
#checar 4 registros duplicados

prueba <- m
