## @knitr datos
library(plotly)
library(forecast)
library(xts)
source("funciones.R")
source("fruta.R")
source("productos.R")

precios <- myfetch("tbPreciosEstimadosSalidas")%>%
  filter(strCan_cel == "NO")%>%
  transmute(Folio = intNum_fol, Clave_presentacion = intCla_pre, Clave_fruta = intCla_fru ,
             Precio_real = floPre_rea, Comision_aduanal = floCus_tom, 
            Manejo = floHan_dliD, Handlio = floHan_dliO, Comision_ventas = floSal_es,
            Caja = floCaj_a, Frio = floFri_o, Mano_obra = floMan_obr, Energia = floEne_rgi)%>%
  filter(Folio > 1672)%>%
  merge(frutas[,c("Clave_fruta", "Fruta")], by = c("Clave_fruta"), all.x = TRUE)%>%
  select(Folio, Precio_real, Fruta, Clave_presentacion, Clave_fruta)
 


salidas <- salidas_fruta%>% 
  filter(Fecha>= as.Date("2017-09-01"))%>%
  select(Folio,Fecha,Destino, Cliente, Pais, Clave_producto, Cantidad)%>%
  ddply(.(Folio,Fecha,Destino, Cliente, Pais, Clave_producto), 
        summarize, Cantidad = sum(Cantidad))%>%
  merge(productos, by = "Clave_producto", all.x = TRUE)%>%
  select(Folio, Destino, Cliente,  Clave_fruta, Clave_presentacion, 
         Clave_producto, Producto, Fraccion6oz, Cantidad, Fecha)

salidas[salidas$Destino == "FRSNKFURT",]$Destino <- "FRANKFURT"
salidas[salidas$Destino == "FRANFURT",]$Destino <- "FRANKFURT"
salidas[salidas$Destino %in%c("LOS ÁNGELES","LOS ÀNGELES","LOS ANGLES"),]$Destino <- "LOS ANGELES"

salidas[salidas$Destino == "MCALLEN TX",]$Destino <- "MCALLEN"
salidas[salidas$Destino == "MC ALLEN",]$Destino <- "MCALLEN"
salidas[salidas$Destino == "TOKIO",]$Destino <- "TOKYO"


precios_check <- merge(precios, salidas, by = c("Folio", "Clave_presentacion", "Clave_fruta"))

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

precios_zar.plot <- ggplotly(ggseasonplot(zar.cn.ts))

precios_ara.plot <- ggplotly(ggseasonplot(ara.cn.ts))



precios_cor <- merge(precios_check%>%
                       mutate(Semana = as.integer(format(Fecha,format =  "%U")), 
                              Year = as.integer(format(Fecha, format = "%Y" )),
                              Semanats = (Year - 2010)*52 + Semana), 
                     precios_semanales, by = c("Semanats", "Fruta"), all.x = TRUE)%>%
  filter(Fruta == "ZAR CN")%>%
  select(Folio, Fecha, Semanats, Fruta, Producto , Fraccion6oz, Cliente, Destino, Cantidad,  Promedio_usda, Precio_real)

pais.plot <- ggplotly(ggplot(precios_cor, aes(x = Promedio_usda, y = Precio_real, colour = Destino)) + 
           geom_point()  + geom_smooth(method = "lm", se = FALSE)+ geom_abline(slope = 1, linetype = "dotted"))

cliente.plot <- ggplotly(ggplot(precios_cor, aes(x = Promedio_usda, y = Precio_real, colour = Cliente)) + 
                        geom_point()  + geom_smooth(method = "lm") + geom_abline(slope = 1, linetype = "dotted"))

mix.plot <- ggplotly(ggplot(precios_cor, aes(x = Promedio_usda, y = Precio_real, colour = paste(Cliente, Destino))) + 
                       geom_point()  + geom_smooth(method = "lm", se = FALSE) + geom_abline(slope = 1, linetype = "dotted"))

global.plot <-  ggplotly(ggplot(precios_cor, aes(x = Promedio_usda, y = Precio_real),  colour = "Cyan") + 
                           geom_point(colour = "grey")  + geom_smooth(method = "lm", se = FALSE) + 
                           geom_abline(slope = 1, linetype = "dotted"))




#dolar
#hoy <- format(Sys.Date(),format ="%Y-%m-%d")
#dolarurl <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?chart_type=line&recession_bars=on&log_scales=&bgcolor=%23e1e9f0&graph_bgcolor=%23ffffff&fo=Open+Sans&ts=12&tts=12&txtcolor=%23444444&show_legend=yes&show_axis_titles=yes&drp=0&cosd=2016-09-01&coed=",hoy,"&height=450&stacking=&range=Custom&mode=fred&id=DEXMXUS&transformation=lin&nd=1993-11-08&ost=-99999&oet=99999&lsv=&lev=&mma=0&fml=a&fgst=lin&fgsnd=2009-06-01&fq=Daily&fam=avg&vintage_date=&revision_date=&line_color=%234572a7&line_style=solid&lw=2&scale=left&mark_type=none&mw=2&width=968")

#download.file(dolarurl,"dolar.csv",quiet = TRUE,method="libcurl")
dolar <- read.csv("dolar.csv",stringsAsFactors = FALSE)%>%
  as.data.frame()
names(dolar) <- c("Fecha","Dolar")
dolar$Fecha <- as.Date(dolar$Fecha, format = "%Y-%m-%d")
dolar[dolar$Dolar==".",c("Dolar")] <- NA
dias <- seq(as.Date("2016-09-01"),as.Date(Sys.Date()), by="days")%>%
  as.Date()%>%
  as.data.frame()
names(dias) <- c("Fecha")
dolar <- merge(dias,dolar,all.x = TRUE)
dolar$Dolar <- as.double(dolar$Dolar)
dolar.ts <- as.xts(x = dolar$Dolar, order.by= dolar$Fecha)
dolar.ts <- na.approx(dolar.ts)
dolar.ts <- as.data.frame(dolar.ts)
dolar <- data.frame(rownames(dolar.ts),coredata(dolar.ts))
names(dolar) <- c("Fecha","Cotizacion")
dolar$Fecha <- as.Date(dolar$Fecha)


row.names(dolar) <- c(1:length(dolar$Fecha))

dolar_promedio <- mutate(dolar, Semana = as.integer(format(Fecha, format = "%U")))%>%
  group_by(Semana)%>%
  summarize(Cotizacion_promedio = mean(Cotizacion))

source("registros.R", encoding = "utf-8")

zar_usda_semanal <- precios_diarios%>%
  select(Fecha, "ZAR CN")%>%
  rename(Precio_usda = "ZAR CN")%>%
  filter(Fecha > as.Date("2017-09-01"))%>%
  mutate(Semana = as.integer(format(Fecha, format = "%U")))%>%
  ddply(.(Semana), summarize, Promedio_usda = mean(Precio_usda))


precio_promedio6oz <- registros%>%
  filter(Fecha > as.Date("2017-09-01") & Fruta == "ZARZAMORA" & Peso == 6.0 & Precio != 0)%>%
  select(Semana, Precio, Aceptadas)%>%
  ddply(.(Semana), summarize, Precio_promedio = sum(Precio * Aceptadas)/sum(Aceptadas))%>%
  merge( dolar_promedio, by = "Semana", all.x = TRUE)%>%
  mutate(Alpasa = Precio_promedio/Cotizacion_promedio)%>%
  merge(zar_usda_semanal, by = "Semana", all.x = TRUE)

preciolm <- lm(precio_promedio6oz$Promedio_usda ~precio_promedio6oz$Alpasa)

preciolm2 <- lm(precio_promedio6oz$Alpasa ~precio_promedio6oz$Promedio_usda
                )  
precio_promedio6oz$Alpasa_regresion <- summary(preciolm)$coefficients[2,"Estimate"]*precio_promedio6oz$Alpasa + summary(preciolm)$coefficients[1,"Estimate"]
  
regresion.plot <- ggplotly(ggplot(precio_promedio6oz, aes(x = Promedio_usda, y = Alpasa)) + geom_point() +
           geom_smooth(method = "lm") + 
           geom_abline(slope = 1, intercept =  summary(preciolm2)$coefficients[1,"Estimate"], linetype = "dotted"))

precio_promedio6oz_gat <- precio_promedio6oz%>%
  select(Semana, Promedio_usda, Alpasa, Alpasa_regresion)%>%
  gather(Tipo, Precio,-Semana)
  
precios_regresion.plot<-  ggplotly(ggplot(precio_promedio6oz_gat,
                                          aes(x = Semana, y = Precio, colour = Tipo)) + geom_line()) 

histograma_precios <- registros %>%
  select(Producto, Aceptadas, Precio, Fecha, Fruta, Cajas)%>%
  filter(Fecha > as.Date("2017-09-01") & Fruta == "ZARZAMORA")%>%
  mutate(Precio6oz = Aceptadas*Precio/Cajas)%>%
  mutate(Precio6oz = cut(Precio6oz, ordered_result = TRUE, 
                         breaks = c(-1,50,70,80,90,100,110,120,130,140,150,175,251)))%>%
  ddply(.(Precio6oz), summarize, Total = sum(Aceptadas))


ggplot(histograma_precios, aes(x = Precio6oz, y = Total)) + geom_col()