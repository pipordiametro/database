## @knitr datos
source("funciones.R")
library(plotly)
library(ggplot2)
source("fruta.R")
library(forecast)
library(DT)

entradas_semanales <- entradas_fruta%>%
  select(Fecha, Cantidad, Rechazadas, Fraccion6oz, Fruta)%>%
  tiempos()%>%
  ddply(.(Semana, Temporada,Fruta), summarize, Total = sum((Cantidad-Rechazadas)*Fraccion6oz))

        
plots.productos(tiempos(entradas_fruta%>%mutate(Total = Cantidad -Rechazadas)))

source("precios_usda.R")

precios_semanales <- precios_diarios%>%
  gather(Fruta, Precio_usda, -Fecha)%>%
  mutate(Semana = as.integer(format(Fecha,format =  "%U")), 
         Year = as.integer(format(Fecha, format = "%Y" )),
         Semanats = (Year - 2010)*52 + Semana)%>%
  ddply(.(Fruta, Semanats), summarize, Promedio_usda = mean(Precio_usda))

precios_semanales[precios_semanales$Fruta == "ARA CN",]$Fruta <- "ARA BER"

zar.cn.ts <- ts(precios_semanales[precios_semanales$Fruta == "ZAR CN",]$Promedio_usda, c(2010, 1), frequency = 52)

ara.cn.ts <- ts(precios_semanales[precios_semanales$Fruta == "ARA BER",]$Promedio_usda, c(2010, 1), frequency = 52)

precios_zar.plot <- ggplotly(ggseasonplot(zar.cn.ts) + labs(title = "Precio promedio por semana en usda"))

precios_ara.plot <- ggplotly(ggseasonplot(ara.cn.ts))

precio.alpasa <- data.frame(Fecha = as.Date(c(min(entradas_fruta$Fecha):max(entradas_fruta$Fecha)), 
                                            origin = as.Date("1970-01-01")))%>%
  mutate(Clave_producto = "10201")%>%
  tiempos()%>%
  precio.promedio_semanal()



productores_fruta <- entradas_fruta%>%
  filter(Fecha >= max(Fecha)-20)%>%
  nombres.productores()%>%
  nombres.productos()%>%
  ddply(.(Fecha, Clave_productor,Nombre_completo, Fruta), 
        summarize, Aceptadas = sum((Cantidad - Rechazadas)*Fraccion6oz))%>%
  group_by(Clave_productor, Nombre_completo, Fruta)%>%
  summarize(Total = sum(Aceptadas),
            Mamimo = max(Aceptadas),
            Minimo = min(Aceptadas))


#source("merma.R")


