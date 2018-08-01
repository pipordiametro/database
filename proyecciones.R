source("fruta.R")

salidas_productos <- salidas_fruta%>%
  select(Fecha, Producto, Cantidad, Fruta)
  

tiempos(salidas_productos)

salidas_productos <- tiempos(salidas_productos)%>%
  ddply(.(Semana, Temporada, Producto), summarize, Total = sum(Cantidad))%>%
  filter(Temporada != "2013-2014")
  


plots.productos(salidas_productos)


acumulados_productos <- salidas_productos%>%
  group_by(Producto, Temporada)%>%
  summarize(Total = sum(Total))%>%
  ungroup()

acumulados.plot <- ggplotly(ggplot(acumulados_productos[acumulados_productos$Temporada != "2013-2014",], 
                aes(x= Temporada, y = Total, colour = Producto, group = Producto)) + geom_line() +
           labs(title = "Crecimiento de productos por temporada") + 
           scale_y_continuous(name="Cajas6oz", labels = scales::comma) +geom_smooth(method = "lm"))
proyecciones <- data.frame()

for (var in unique(acumulados_productos[acumulados_productos$Temporada == "2017-2018",]$Producto)){
  
  prod.df <- acumulados_productos%>%
    filter(Producto == var)
  
  prodfit <- lm(Total ~ as.numeric(Temporada), data = prod.df) 
  
  predict.var <- as.data.frame(predict(prodfit, newdata = data.frame(Temporada = 6),
          type = "response", interval = "confidence"))%>%
    mutate(Producto = var)
  proyecciones <- rbind(proyecciones, predict.var)
  
}

acumulados_gat <- acumulados_productos%>%
  spread(Temporada, Total)%>%
  filter(!is.na(`2017-2018`))
  
 acumulados_gat <- acumulados_gat[acumulados_gat$`2017-2018`]
 
proyecciones <- merge(acumulados_gat, proyecciones, by = "Producto")

write.csv(proyecciones, "proyecciones.csv")
