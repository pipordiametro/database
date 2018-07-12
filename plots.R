source("registros.R",encoding="utf-8")
registros <- mutate(registros, Producto = paste(Producto, Clams, "x", Peso, Unidades))



#---------Funcion que devielve plots de serie temporal
#----------y acumulativos.

ploteo <- function(df = registros, producto = "ALPASA12x6OZ" , fecha = "Fecha" , cantidad = "Cajas"){
  df <- df[df$Producto == producto,]
  df <- data.frame(Fecha = df[,c(fecha)], Cantidad = df[,c(cantidad)])%>%
    ddply(.(Fecha), summarize, Total = sum(Cantidad))
  minyear <- min(as.integer(format(df$Fecha, format = "%Y")))
  
  df <- df%>% 
    merge(data.frame(Fecha = as.Date(c(min(df$Fecha):max(df$Fecha)))), all.y = TRUE)%>%
    mutate(Semana = as.integer(format(Fecha -244, format = "%U")),
           Year = as.integer(format(Fecha-244, format = "%Y")),
           Year = ifelse(Semana == 0, Year -1, Year),
           Semanats = Semana,
           Semanats = ifelse(Semana %in% c( 0,53), 52, Semana),
           Semanats = Semanats + (Year - minyear)*52)   
  
  
  df[is.na(df$Total),]$Total <- 0
  semanal <- ddply(df,.(Year, Semanats), summarize, Total = sum(Total))%>%
    arrange(Semanats)
  
  
  
  
  
  ts.semanal <- ts(semanal$Total, c(minyear, min(semanal$Semanats)), frequency = 52) 
  dec.semanal <- decompose(semanal.ts, "additive")
  
  
  semanal.cum <- semanal%>%
    group_by(Year)%>%
    mutate(Acumulado =cumsum(Total))
  
  ts.semanal.cum <- ts(semanal.cum$Acumulado, c(minyear, min(semanal$Semanats)), frequency = 52)
  dec.semanal.cum <- decompose(ts.semanal.cum, "additive")
  
  print(ggseasonplot(ts.semanal))
  
  dev.new()
  plot(dec.semanal)
  dev.new()
  print(ggseasonplot(ts.semanal.cum))
  dev.new()
  plot(dec.semanal.cum)
}




#_______________pruebas

registros <- mutate(registros, 
                    Six = ifelse(Peso == 6, TRUE, FALSE))%>%
  merge(data.frame(Fecha = as.Date(c(min(df$Fecha):max(df$Fecha)))), all.y = TRUE)%>%
  mutate(Semana = as.integer(format(Fecha -244, format = "%U")),
         Year = as.integer(format(Fecha-244, format = "%Y")),
         Year = ifelse(Semana == 0, Year -1, Year),
         Semanats = Semana,
         Semanats = ifelse(Semana %in% c( 0,53), 52, Semana),
         Semanats = Semanats + (Year - minyear)*52)   

registros[is.na(registros$Cajas),]$Cajas <- 0


gg <- ggplot(ddply(registros,.(Semanats), 
             summarize, 
             Total = sum(Cajas)), 
       aes(x = Semanats,  y = Total)) + 
  geom_line(colour = "blue") + geom_line(data = ddply(registros[registros$Six == FALSE,],
                               .(Semanats), 
                              summarize, 
                              Total = sum(Cajas)), 
                          aes(x = Semanats, y = Total), colour = "red")

ggplotly(gg)


#-------comparativo

comp <- merge(ddply(registros,
                    .(Semanats),
                    summarize,
                    Total = sum(Cajas)),
              ddply(registros[registros$Six == FALSE,],
                    .(Semanats), summarize, 
                    Especiales = sum(Cajas)), all.x = TRUE)%>%
  mutate(Ratio = Especiales/Total)
                  
ggplot(comp, aes( x= Semanats, y = Ratio)) + geom_line()


prueba6 <- ddply(registros,.(Year,Semana,Producto), summarize, Total = sum(Cajas))

productos <- unique(registros[registros$Fecha >= as.Date("2017-09-01)"),]$Producto)

for (var in productos){
  assign(var, prueba6[prueba6$Producto == var,]%>%
           merge(merge(data.frame(Year= c(2014:2018)), data.frame(Semana = c(1:52))), 
                 all.y = TRUE)%>%
           mutate(Total = ifelse(is.na(Total), 0, Total)))
  
  assign(paste0(var,".plot"), 
         ggplotly(ggplot(get(var),
                         aes(x = Semana,
                             y = Total, 
                             colour = as.factor(Year))) + 
                    geom_line() +
                    labs(title = paste0(var))))

  
}

dfgdf