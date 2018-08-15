source("fruta.R")

precio.mean_diario <- entradas_fruta%>%
  filter(Precio != 0)%>%
  ddply(.(Fecha, Clave_producto), summarize, 
        Precio_promedio = sum(Cantidad*Precio)/sum(Cantidad), 
        Desviacion = sqrt(sum((Cantidad*Precio - Cantidad*Precio_promedio)**2))/sum(Cantidad))
  
precio.mean_semanal <- precio.mean_diario%>%
  tiempos()%>%
  ddply(.(Semana, Temporada, Clave_producto), summarize, Precio_semanal = mean(Precio_promedio))

write.csv(precio.mean_diario, "csvs/precio.mean_diario.csv", row.names = FALSE)

write.csv(precio.mean_semanal, "csvs/precio.mean_semanal.csv", row.names = FALSE)

