source("materiales.R")

entregas <- entregas%>%
  ddply(.(Fecha, Clave_productor, Presentacion, Tipo_material), summarize, Flujo = sum(Cantidad))

devoluciones <- devoluciones%>%
  rename(Clave_productor = Clave_proveedor)%>%
  ddply(.(Fecha, Clave_productor, Presentacion, Tipo_material), summarize, Flujo = -sum(Cantidad))

recepcion <- recepcion%>%
  ddply(.(Fecha, Clave_productor, Presentacion, Tipo_material), summarize, Flujo = -sum(Cantidad))

movimientos <- rbind(entregas, devoluciones, recepcion)%>%
  filter(Tipo_material %in% c("CAJA EMP.", "CLAMSHELL"))%>%
  ddply(.(Fecha, Clave_productor, Presentacion, Tipo_material), 
        summarize, Flujo = sum(Flujo))%>%
  group_by(Clave_productor, Presentacion, Tipo_material)%>%
  arrange(Fecha)%>%
  mutate(Acumulado = cumsum(Flujo))%>%
  ungroup()%>%
  mutate(Acumulado = ifelse(Tipo_material == "CLAMSHELL", Acumulado/12, Acumulado))


ggplot(movimientos%>%
         filter(Clave_productor == 111), aes( x = Fecha, y = Acumulado, colour = Presentacion)) + geom_line()
