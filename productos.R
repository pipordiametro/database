source("funciones.R")

productos <- myfetch("tbProductos")

names(productos) <- paste0("p", names(productos))

productos <- productos%>%
  rename(Clave_presentacion = pintCla_pre, Clave_fruta = pintCla_fru, 
         Nombre_producto = pstrNom_bre)

presentaciones <- myfetch("tbPresentaciones")%>%
  rename(Clave_presentacion = intCla_pre, Clams = intCan_tid, 
         Peso = intPes_o, Unidad = strUni_med )%>%
  mutate(Presentacion = paste0(Clams,"X", Peso, Unidad))%>%
  filter(strCan_cel == "NO")

frutas <- myfetch("tbFrutas")%>%
  rename(Clave_fruta = intCla_fru)%>% 
  mutate(Fruta = paste(strNom_cto,strTip_cto))

productos <- merge(productos, presentaciones, all.x = TRUE)%>%
  merge(frutas, by = "Clave_fruta", all.x = TRUE)%>%
  rename(Clave_producto = pstrCla_prod,
         Fruta = Fruta)%>%
  mutate(Producto = paste0(Nombre_producto, Presentacion),
         Fraccion6oz = ifelse(Unidad == "G", Clams*Peso*0.035274/72  , Clams * Peso/72))


