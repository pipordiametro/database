source("funciones.R")

productos <- myfetch("tbProductos")

names(productos) <- paste0("p", names(productos))

productos <- productos%>%
  filter(pstrCan_cel == "NO")%>%
  transmute(Clave_presentacion = pintCla_pre, Clave_fruta = pintCla_fru, 
            Clave_producto = pstrCla_prod,Nombre_producto = pstrNom_bre)

presentaciones <- myfetch("tbPresentaciones")%>%
  filter(strCan_cel == "NO")%>%
  transmute(Clave_presentacion = intCla_pre, Clams = as.integer(intCan_tid), 
         Peso = as.numeric(intPes_o), Unidad = strUni_med )%>%
  mutate(Presentacion = paste0(Clams,"X", Peso, Unidad))
  

frutas <- myfetch("tbFrutas")%>%
  transmute(Clave_fruta = intCla_fru, Nombre = strNom_bre, Nombre_corto = strNom_cto, 
            Variedad = strVar_ied, Variedad_corto = strVar_cto, Tipo_fruta = strTip_o,
            Tipo_corto = strTip_cto)%>% 
  mutate(Fruta = paste(Nombre_corto,Tipo_corto))

productos <- merge(productos, presentaciones, all.x = TRUE)%>%
  merge(frutas, by = "Clave_fruta", all.x = TRUE)%>%
  mutate(Producto = paste0(Fruta,"_", Presentacion, "_", Nombre_producto),
         Fraccion6oz = ifelse(Unidad == "G", Clams*Peso*0.035274/72  , Clams * Peso/72))


