library(tidyr)
library(imputeTS)

frutas.df <- data.frame(Fruta = c("Blackberries", "Blueberries"), 
                        Nombre_corto = c("ZAR","ARA"))

for (var in frutas.df$Fruta){
  precios.var <- read.csv(paste0("Precios/",var,".csv"))%>%
    rename(Presentacion_usda = packageDesc, Puerto =  cityName , Fecha = reportDate, 
           Precio_menor = lowPriceMin, Precio_mayor = highPriceMax, Frecuente_menor = mostlyLowMin,
           Frecuente_mayor = mostlyHighMax, Suministro = supplyTone, Demanda = demandTone, 
           Mercado = marketTone, Ciudad_reporte = reportingCity)%>%
    mutate(Fecha = as.Date(Fecha))%>%
    select(Puerto, Fecha, Precio_menor, Precio_mayor, Frecuente_menor, Frecuente_mayor,
           Ciudad_reporte, organic)
  
  assign(paste0(frutas.df[frutas.df$Fruta == var,]$Nombre_corto,".ORG"), 
         precios.var[precios.var$organic == "Organic",]%>%
           mutate(Fruta = paste(frutas.df[frutas.df$Fruta == var,]$Nombre_corto,"ORG")))
    
  
  
  
  assign(paste0(frutas.df[frutas.df$Fruta == var,]$Nombre_corto,".CN"), 
         precios.var[precios.var$organic != "Organic",]%>%
           mutate(Fruta = paste(frutas.df[frutas.df$Fruta == var,]$Nombre_corto,"CN")))

}
ara.cn.f <- ARA.CN%>%
  filter(!Puerto %in% c("CENTRAL & NORTH FLORIDA", 
                      "ARGENTINA IMPORTS - PORT OF ENTRY LOS ANGELES INTERNATIONAL AIRPORT")) 

ara.org.f <- ARA.ORG%>%
  filter(!Puerto %in% c("ARGENTINA/URUGUAY IMPORTS - PORTS OF ENTRY SOUTH FLORIDA",
                       "PERU IMPORTS - PORTS OF ENTRY PHILADELPHIA AREA AND NEW YORK CITY AREA"))

zar.cn.f <- ZAR.CN%>%
  filter(Puerto %in% c("CARIBBEAN BASIN IMPORTS - PORTS OF ENTRY SOUTH FLORIDA",
                       "CENTRAL AMERICA IMPORTS - PORTS OF ENTRY SOUTH FLORIDA",
                       "MEXICO CROSSINGS THROUGH ARIZONA, CALIFORNIA AND TEXAS"))

precios.df <- rbind(ara.cn.f,ara.org.f, zar.cn.f,ZAR.ORG)%>%
  ddply(.(Fruta, Fecha), summarize, 
        Promedio = mean((Precio_mayor+Precio_menor)/2, 
                        na.rm = TRUE))%>%
  spread(Fruta, Promedio)%>%
  filter(!is.na(Fecha))
 

rm(ARA.CN, ara.cn.f, ARA.ORG, ara.org.f, frutas.df,precios.var, 
   ZAR.cn, zar.cn.f, ZAR.ORG, var)
  
dias <- data.frame(Fecha = as.Date(c(min(precios.df$Fecha):max(precios.df$Fecha)), 
                                   origin = "1970-01-01"))

precios_diarios <-  merge(dias, precios.df, all.x = TRUE)%>%
  na.interpolation()

#rm(precios.df)

