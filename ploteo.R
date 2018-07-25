

prueba6 <- registros%>%
  mutate( Temporada = ifelse((Fecha >= as.Date("2013-09-01") & Fecha < as.Date("2014-09-01")), "2013-2014",
                             ifelse((Fecha >= as.Date("2014-09-01") & Fecha < as.Date("2015-09-01")), "2014-2015",
                                    ifelse((Fecha >= as.Date("2015-09-01") & Fecha < as.Date("2016-09-01")), "2015-2016",
                                           ifelse((Fecha >= as.Date("2016-09-01") & Fecha < as.Date("2017-09-01")), "2016-2017",
                                                  ifelse((Fecha >= as.Date("2016-09-01") & Fecha < as.Date("2018-09-01")), "2017-2018",
                                                         ifelse((Fecha >= as.Date("2017-09-01") & Fecha < as.Date("2019-09-01")), "2018-2019",NA)))))))

prueba6 <- ddply(registros,.(Temporada,Semana,Producto), summarize, Total = sum(Cajas))

productos <- unique(registros[registros$Fecha > as.Date("2017-09-01") & registros$Fruta == "ZARZAMORA",]$Producto)

for (var in productos){
  assign(var, prueba6[prueba6$Producto == var,]%>%
           merge(merge(data.frame(Year= c(2014:2018)), data.frame(Semana = c(1:52))), 
                 all.y = TRUE)%>%
           mutate(Total = ifelse(is.na(Total), 0, Total)))
  
  assign(paste0(var,".plot"), ggplotly(ggplot(get(var), aes(x = Semana, y = Total, colour = as.factor(Year))) + 
                                        geom_line()))
}

pedidos_especiales <- registros%>%
  mutate(Especial = ifelse(Peso != 6.0, "Seisoz", "Otro"),
         Semanats = (Year - 2013)*52 + Semana)%>%
  select(Fecha, Temporada, Semanats, Aceptadas, Especial, Cajas)%>%
  ddply(.(Temporada, Semanats , Especial), summarize, Cajas6oz = sum(Cajas))%>%
  filter(!is.na(.$Especial))%>%
  spread(Especial, Cajas6oz, fill = 0)%>%
  mutate(Total6oz = Seisoz + Otro)%>%
  select(-Seisoz)%>%
  gather(Tipo, Total6oz, -Semanats, -Temporada)
  



pedidos_especiales.plot <- ggplot(pedidos_especiales, aes(x = Semanats, y = Total6oz, colour = Tipo)) + geom_line()

