source("fruta.R")

entradas_fruta <- entradas_fruta%>%
  select(Idb_entradasFecha, Clave_producto, Cantidad, Rechazadas)

salidas_fruta <- salidas_fruta%>%
  select(Fecha, Clave_producto, Cantidad)

reembales <- reembales%>%
  select(Fecha, Producto_anterior, Cantidad_anterior, Producto_nuevo, Cantidad_nueva)

library(ggplot2) 
entradas_diarias <- entradas_fruta%>%
  ddply(.(Fecha, Clave_producto), summarize, Total = sum(Cantidad-Rechazadas))

salidas_diarias <- salidas_fruta%>%
  ddply(.(Fecha, Clave_producto), summarize, Total = -sum(Cantidad))

reembales <- rbind(reembales[,c("Fecha", "Producto_anterior"  ,"Cantidad_anterior")]%>%
                   rename(Clave_producto = Producto_anterior, Total = Cantidad_anterior)%>%
                   mutate(Total = -Total),
                   reembales[,c("Fecha", "Producto_nuevo"  ,"Cantidad_nueva")]%>%
                   rename(Clave_producto = Producto_nuevo, Total = Cantidad_nueva))%>%
  ddply(.(Fecha, Clave_producto), summarize, Total = sum(Total))

flujos <- rbind(entradas_diarias, salidas_diarias, reembales)%>%
  filter(Fecha > as.Date("2017-09-01"))%>%
  ddply(.(Fecha, Clave_producto), summarize, Total = sum(Total))%>%
  group_by(Clave_producto)%>%
  mutate(Acumulado = cumsum(Total))



ggplot(flujos, aes(x = Fecha, y = Acumulado, colour = as.factor(Clave_producto))) + 
  geom_line()


