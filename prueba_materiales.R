#entradas material


entradas_material <- merge(myfetch("tbAlmEnt"),myfetch("tbAlmEntReg"), 
    by = "intNum_reg", all.x = TRUE, suffixes = c("h", "b"))%>%
    filter(strCan_celb == "NO")%>%
  transmute(Clave_acopio = intCla_alm, Tipo_entrada =  strTip_ent, Fecha = as.Date(fecFec_fac), 
            Tipo_proveedor = strTip_prov, Cantidad = intCan_tid , Clave_material = intCla_mat,
            Clave_proveedor = intCla_prov)%>%
  filter(Fecha > inicio_temporada)
  


entregas_material <- merge(myfetch("tbAlmSal"),myfetch("tbAlmsalReg"), 
                  by = "intNum_reg", all.x = TRUE, suffixes = c("h", "b"))%>%
  filter(strCan_celb == "NO")%>%
  transmute(Clave_acopio = intCla_alm, Tipo_salida = strTip_sal, Clave_productor = intCla_pro,
            Fecha = as.Date(fecFec_not), Cantidad = intCan_tid, Clave_material = intCla_mat)%>%
  filter(Fecha > inicio_temporada)

inventario_acopios <- rbind(entradas.material()%>%
                              select(Clave_acopio, Clave_material, Cantidad),
                            entregas.material()%>%
                              select(Clave_acopio, Clave_material, Cantidad))%>%
  ddply(.(Clave_acopio, Clave_material), summarize, Saldo = sum(Cantidad))%>%
  get.acopio()
  
        