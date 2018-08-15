## @knitr datos
library(plotly)
source("fruta.R")

entradas_fruta <- entradas_fruta%>%
  select(Idb_entradas, Fecha, Clave_producto, Cantidad, Rechazadas)

salidas_merma <- salidas_fruta%>%
  select(Fecha, Clave_producto)

salidas_fruta <- salidas_fruta%>%
  select(Fecha, Clave_producto, Cantidad, Cliente)

reembales <- reembales%>%
  select(Numero_registro, Fecha, Producto_anterior, Cantidad_anterior, Producto_nuevo, Cantidad_nueva)

library(ggplot2) 
entradas_diarias <- entradas_fruta%>%
  ddply(.(Fecha, Clave_producto), summarize, Total = sum(Cantidad-Rechazadas))

salidas_diarias <- salidas_fruta%>%
  ddply(.(Fecha, Clave_producto), summarize, Total = -sum(Cantidad))

reembales2 <- rbind(reembales[,c("Fecha", "Producto_anterior"  ,"Cantidad_anterior")]%>%
                   rename(Clave_producto = Producto_anterior, Total = Cantidad_anterior)%>%
                   mutate(Total = -Total),
                   reembales[,c("Fecha", "Producto_nuevo"  ,"Cantidad_nueva")]%>%
                   rename(Clave_producto = Producto_nuevo, Total = Cantidad_nueva))%>%
  ddply(.(Fecha, Clave_producto), summarize, Total = sum(Total))

flujos <- rbind(entradas_diarias, salidas_diarias, reembales2)%>%
  filter(Fecha > as.Date("2017-09-01"))%>%
  ddply(.(Fecha, Clave_producto), summarize, Total = sum(Total))%>%
  group_by(Clave_producto)%>%
  mutate(Acumulado = cumsum(Total))



plot <-  ggplotly(ggplot(flujos, aes(x = Fecha, y = Acumulado, colour = as.factor(Clave_producto))) + 
  geom_line())


totales <- flujos%>%
  ddply(.(Clave_producto), summarize, Total = sum(Total))%>%
  merge(productos[,c("Clave_producto", "Producto")],  by = "Clave_producto", all.x = TRUE)%>%
  select(Producto, Total )%>%
  filter(Total != 0)


merma_reembales <- merge(merge(reembales[,c("Numero_registro", "Fecha", "Producto_anterior", "Cantidad_anterior")],
                               productos[,c("Clave_producto","Fraccion6oz")], 
                               by.x = "Producto_anterior", by.y = "Clave_producto", all.x = TRUE)%>%
                           rename(Fraccion6oz_viejo = Fraccion6oz),
                         merge(reembales[,c("Numero_registro","Producto_nuevo", "Cantidad_nueva")],
                               productos[,c("Clave_producto","Fraccion6oz")],
                               by.x = "Producto_nuevo", by.y = "Clave_producto", all.x = TRUE)%>%
                           rename(Fraccion6oz_nueva = Fraccion6oz), by = "Numero_registro")%>%
  mutate(Merma6oz = Cantidad_anterior*Fraccion6oz_viejo - Cantidad_nueva*Fraccion6oz_nueva)%>%
  tiempos()%>%
  mutate(Clave_producto = Producto_anterior)%>%
  precio.promedio_diario()%>%
  precio.promedio_semanal()%>%
  mutate(Precio = ifelse(is.na(Precio_promedio), Precio_semanal, Precio_promedio))%>%
  select(-Precio_promedio, -Precio_semanal,- Clave_producto)%>%
  mutate(Merma_money = Merma6oz/Fraccion6oz_viejo*Precio)

  



merma_reembales <- merma_reembales%>%
  mutate(Merma6oz_2 = pmax(Merma6oz, 0), Merma_money_2 = pmax(Merma_money, 0))%>%
  ddply(.(Semana, Temporada), summarize, 
        Merma6oz = sum(Merma6oz), Merma_money = sum(Merma_money), 
        Merma6oz_2 = sum(Merma6oz_2), Merma_money_2 = sum(Merma_money_2))%>%
  merge(merge(data.frame(Semana = c(1:52)), 
              data.frame(Temporada = c("2013-2014","2014-2015","2015-2016", "2016-2017", 
                                       "2017-2018"))), 
        by = c( "Semana", "Temporada"), all.y = TRUE)


merma_reembales[is.na(merma_reembales$Merma6oz),c("Merma6oz", "Merma_money", "Merma6oz_2", "Merma_money_2")] <-  0

merma_reembales <- merma_reembales%>%
  ddply(.(Semana, Temporada), summarize, Merma6oz = sum(Merma6oz), Merma_money = sum(Merma_money),
        Merma6oz_2 = sum(Merma6oz_2), Merma_money_2 = sum(Merma_money_2))%>%
  group_by(Temporada)%>%
  arrange(Semana)%>%
  mutate(Acumulado6oz = cumsum(Merma6oz_2), Acumulado_money = cumsum(Merma_money_2))%>%
  ungroup()


merma.plot <-  ggplot(merma_reembales, aes(x = Semana, y = Acumulado_money, colour = Temporada, group = Temporada)) + geom_point() + geom_line()
  

merma_temporada <- merma_reembales%>%
  group_by(Temporada)%>%
  summarize(Total6oz = sum(Merma6oz), Total_money = sum(Merma_money),
            Total26oz = sum(Merma6oz_2), Total2_money = sum(Merma_money_2))


merma_cooler <- salidas_fruta%>%
  filter(Fecha > inicio_temporada, Cliente == "MERMA")%>%
  precio.promedio_diario()%>%
  mutate(Merma_money = Cantidad*Precio_promedio)%>%
  tiempos()%>%
  group_by(Temporada)%>%
  summarize(merma_temporada = sum(Merma_money))

