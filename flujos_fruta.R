## @knitr datos
library(plotly)
source("fruta.R")

entradas_fruta <- entradas_fruta%>%
  select(Idb_entradas, Fecha, Clave_producto, Cantidad, Rechazadas)

salidas_merma <- salidas_fruta%>%
  select(Fecha, Clave_producto)

salidas_fruta <- salidas_fruta%>%
  select(Fecha, Clave_producto, Cantidad)

reembales <- reembales%>%
  select(intNum_reg, Fecha, Producto_anterior, Cantidad_anterior, Producto_nuevo, Cantidad_nueva)

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


merma_reembales <- merge(merge(reembales[,c("intNum_reg", "Fecha", "Producto_anterior", "Cantidad_anterior")],productos[,c("Clave_producto","Fraccion6oz")], 
                               by.x = "Producto_anterior", by.y = "Clave_producto", all.x = TRUE)%>%
                           rename(Fraccion6oz_viejo = Fraccion6oz),
                         merge(reembales[,c("intNum_reg","Producto_nuevo", "Cantidad_nueva")],productos[,c("Clave_producto","Fraccion6oz")],
                               by.x = "Producto_nuevo", by.y = "Clave_producto", all.x = TRUE)%>%
                           rename(Fraccion6oz_nueva = Fraccion6oz), by = "intNum_reg")%>%
  mutate(Merma = Cantidad_anterior*Fraccion6oz_viejo - Cantidad_nueva*Fraccion6oz_nueva)%>%
  mutate(Semana = as.integer(format(Fecha, format = "%U")),
         Year = as.integer(format(Fecha, format = "%Y")),
         Temporada = ifelse((Fecha >= as.Date("2013-09-01") & Fecha < as.Date("2014-09-01")), "2013-2014",
                            ifelse((Fecha >= as.Date("2014-09-01") & Fecha < as.Date("2015-09-01")), "2014-2015",
                                   ifelse((Fecha >= as.Date("2015-09-01") & Fecha < as.Date("2016-09-01")), "2015-2016",
                                          ifelse((Fecha >= as.Date("2016-09-01") & Fecha < as.Date("2017-09-01")), "2016-2017",
                                                 ifelse((Fecha >= as.Date("2016-09-01") & Fecha < as.Date("2018-09-01")), "2017-2018",
                                                        ifelse((Fecha >= as.Date("2017-09-01") & Fecha < as.Date("2019-09-01")), "2018-2019",NA)))))))%>%
  ddply(.(Semana, Temporada), summarize, Merma = sum(Merma))%>%
  merge(merge(data.frame(Semana = c(0:52)), 
              data.frame(Temporada = c("2013-2014","2014-2015","2015-2016", "2016-2017", "2017-2018"))), 
        by = c( "Semana", "Temporada"), all.y = TRUE)%>%
  mutate(Semana = ifelse(Semana == 0, 52, Semana ))

merma_reembales[is.na(merma_reembales$Merma),]$Merma <-  0

merma_reemabales <- merma_reembales%>%
  ddply(.(Semana, Temporada), summarize, Merma = sum (Merma))

merma.plot <-  ggplot(merma_reembales, aes(x = Semana, y = Merma, colour = Temporada, group = Temporada)) + geom_point() + geom_line()
  

merma_temporada <- merma_reembales%>%
  group_by(Temporada)%>%
  summarize(Total = sum(Merma))
  