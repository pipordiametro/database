library(reshape2)
library(scales)
library(plyr)
library(RODBC)
library(ggplot2)
library(dplyr)
library(plotly)
library(kableExtra)
library(knitr)
library(fpp)
library(PerformanceAnalytics)
library(RCurl)
library(XML)



con <- odbcConnect(dsn = "SQLProyecto08", uid = "francisco", pwd = "Alpasa2017")
emp <- sqlFetch(con, "tbRecFruEmp", as.is = TRUE)%>%
  filter(strCan_cel == "NO")%>%
  select(intNum_reg, fecFec_not, intCla_pro, intCen_aco)
reg <- sqlFetch(con, "tbRecFruEmpReg",as.is = TRUE)%>%
  select(intNum_reg, intCan_tid, intCan_rec, intCla_prod, intNum_sem, floPre_uni)
reg <- merge(emp, reg)
rm(emp) 

prod <- sqlFetch(con, "tbProductores", as.is = TRUE)%>%
  filter(strCan_cel == "NO")%>%
  select(intCla_pro, strNom_bre, strApe_pat, strApe_mat)


emb <- sqlFetch(con, "tbProductos", as.is = TRUE)%>%
  mutate(intCla_prod = strCla_prod)%>%
  select(intCla_prod, strNom_bre, intCla_pre, intCla_fru)

pres <- sqlFetch(con, "tbPresentaciones", as.is = TRUE)%>%
  select(intCla_pre, intCan_tid, intPes_o, strUni_med)

fruta <- sqlFetch(con, "tbFrutas")%>%
  select(intCla_fru,strNom_bre )%>%
  transmute(intCla_fru = intCla_fru,
            Fruta = strNom_bre)
acopios <- sqlFetch(con, "tbAlmacenes")%>%
  select(intCla_alm,strNom_bre)%>%
  transmute(intCla_alm = intCla_alm,
            Acopio = strNom_bre)

odbcClose(con)

productos <- merge(emb, pres)%>%
  merge(fruta)
rm(emb)
rm(pres)
rm(fruta)
productos$clams <- productos$intCan_tid
productos <- productos[, !names(productos) %in% c("intCan_tid")]
names(prod)[2] <- "Nombre"

registros <-  registros <- merge(reg, productos, all.x = TRUE, by.x = "intCla_prod", by.y = "intCla_prod")%>%
  merge(prod, all.x = TRUE, by.x = "intCla_pro", by.y = "intCla_pro")%>%
  merge(acopios, by.x= "intCen_aco", by.y = "intCla_alm")%>%
  transmute(Id = intNum_reg,
            Fecha = as.Date(format(fecFec_not, format = "%Y-%m-%d %h:%m:%s")),
            Semana = as.integer(ifelse(format(Fecha,format = "%W") == "00",52,format(Fecha,format = "%W"))),
            Year = ifelse(Semana == "00",as.integer(format(Fecha, format = "%Y")) -1,as.integer(format(Fecha, format = "%Y"))),
            Clave = intCla_pro,
            Nombre = paste(Nombre,strApe_pat,strApe_mat),
            Acopio = Acopio,
            Aceptadas = intCan_tid - intCan_rec,
            Peso = as.double(intPes_o),
            Unidades = strUni_med,
            Producto = paste0(strNom_bre,clams,"x",Peso,Unidades),
            Fruta = Fruta,
            Precio = as.double(floPre_uni),
            Clams = clams,
            Onzas = if_else(Unidades == "G",Peso*0.035274, Peso),
            Cajas = Onzas*Clams*Aceptadas/72,
            Temporada = ifelse((Fecha >= as.Date("2013-09-01") & Fecha < as.Date("2014-09-01")), "2013-2014",
                               ifelse((Fecha >= as.Date("2014-09-01") & Fecha < as.Date("2015-09-01")), "2014-2015",
                                      ifelse((Fecha >= as.Date("2015-09-01") & Fecha < as.Date("2016-09-01")), "2015-2016",
                                             ifelse((Fecha >= as.Date("2016-09-01") & Fecha < as.Date("2017-09-01")), "2016-2017",
                                                    ifelse((Fecha >= as.Date("2016-09-01") & Fecha < as.Date("2018-09-01")), "2017-2018",
                                                           ifelse((Fecha >= as.Date("2017-09-01") & Fecha < as.Date("2019-09-01")), "2018-2019",NA)
                                                    )
                                             )
                                      )
                               )
            )
  )


#load Anaberries
anab <- read.csv("Anaberries/anaberries.csv", stringsAsFactors = FALSE)%>%
  mutate(Fecha1 = as.Date(substr(Fecha, 3, 12), format = "%Y-%m-%d"),
         Fecha2 = as.Date(substr(Fecha, 16, 25), format = "%Y-%m-%d"),
         Year = as.integer(format(Fecha1, format = "%Y")),
         Semanats = Semana + (Year - 2013)*52,
         Temporada = ifelse((Fecha1 >= as.Date("2013-09-01") & Fecha1 < as.Date("2014-09-01")), "2013-2014",
                            ifelse((Fecha1 >= as.Date("2014-09-01") & Fecha1 < as.Date("2015-09-01")), "2014-2015",
                                   ifelse((Fecha1 >= as.Date("2015-09-01") & Fecha1 < as.Date("2016-09-01")), "2015-2016",
                                          ifelse((Fecha1 >= as.Date("2016-09-01") & Fecha1 < as.Date("2017-09-01")), "2016-2017",
                                                 ifelse((Fecha1 >= as.Date("2016-09-01") & Fecha1 < as.Date("2018-09-01")), "2017-2018",
                                                        ifelse((Fecha1 >= as.Date("2017-09-01") & Fecha1 < as.Date("2019-09-01")), "2018-2019",NA)
                                                 )
                                          )
                                   )
                            )))%>%
  transmute(Year = Year, 
            Semana = ifelse(Semana == 53 , 52, Semana),
            Semanats = ifelse(Year == 2015, Semanats - 1, Semanats), 
            Real = as.numeric(gsub(",", "", Enviado.Real..cajas., fixed = TRUE)),
            Pronostico = as.numeric(gsub(",", "", Pronóstico..cajas., fixed = TRUE)),
            USDA = as.numeric(gsub(",", "", USDA..cajas., fixed = TRUE)),
            Temporada = Temporada)
         

#filter(!(Cajasanab == 0))
rm(list = c("acopios", "productos", "reg"))
