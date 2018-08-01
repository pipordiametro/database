source("funciones.R")
source("productos.R")
#-------------entradas------------
hentradas <- myfetch("tbrecFruEmp")
names(hentradas) <- paste0("h",names(hentradas))

bentradas <- myfetch("tbrecFruEmpReg")
names(bentradas) <- paste0("b",names(bentradas))

entradas_fruta <- merge(hentradas, bentradas, by.x = "hintNum_reg", by.y = "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  transmute(Id_entradas = hintNum_reg, Idb_entradas = bintNum_regA, Fecha = hfecFec_not, Clave_productor = hintCla_pro, 
            Clave_acopio = hintCen_aco, Cantidad = bintCan_tid, Rechazadas = bintCan_rec, Clave_producto =  bintCla_prod,
         Pallet = bintNum_pal, Precio = bfloPre_uni, Numero_pago = bintNum_pag, Pagado = bstrPag_ado)%>%
  mutate(Fecha = as.Date(Fecha), Cantidad = as.integer(Cantidad), 
         Rechazadas = as.integer(Rechazadas), Precio = as.numeric(Precio))%>%
  merge(productos, all.x = TRUE)


rm(hentradas, bentradas)

#---------------------SALIDAS-------------------------

hsalidas <- myfetch("tbSalFruEmp")%>%
  filter(strCan_cel == "NO")

names(hsalidas) <- paste0("h", names(hsalidas))

bsalidas <- myfetch("tbSalFruEmpReg")%>%
  filter(strCan_cel == "NO")

names(bsalidas) <- paste0("b",names(bsalidas))

source("cliente_destino.R")

salidas_fruta <- merge(hsalidas, bsalidas, 
                 by.x = "hintNum_reg", by.y = "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  transmute(Folio = hintNum_fol, Fecha = hfecFec_sal,
         Clave_cliente = hintCla_cli, Clave_destino =  hintCla_des,
         Destino = hstrDes_tin,Pallets =  hstrPal_let, Mexican_cus = hstrMex_cus, 
         American_cus = hstrAme_cus, hstrWar_hou =  hstrWar_hou, Fecha_arrivo = hstrArr_dat,  
         Pallet = bintNum_pal,
         Clave_producto = bstrCla_prod, Cantidad = bintCan_tid)%>%
  mutate(Fecha = as.Date(Fecha), Cantidad = as.integer(Cantidad))%>%
  merge(clientes, by.x = "Clave_cliente", by.y = "Clave_cliente" , all.x = TRUE)%>%
  merge(destinos, by.x = "Clave_destino", by.y = "Clave_destino", all.x = TRUE)%>%
  merge(productos, by.x = "Clave_producto", by.y = "Clave_producto", all.x = TRUE)
  

rm(hsalidas, bsalidas)

#--------------------------REEMBALES-------------------

reembales <- myfetch("tbReembalajes")%>%
  filter(strCan_cel == "NO")%>%
  transmute(Fecha = fecFec_mov,  Cantidad_anterior = intCan_ant, Numero_registro = intNum_reg,
         Producto_anterior = intProd_ant, Cantidad_nueva= intCan_nvo, Producto_nuevo = intProd_nvo,
         Registro_anterior = intRegA_ant, intNum_regO = intNum_regO, Numero_folio = intNum_fol,
         Registro_nuevo = intRegA_nvo)%>%
  mutate(Fecha = as.Date(Fecha))

