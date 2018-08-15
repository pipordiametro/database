source("fruta.R")
getdolar()
dolar <- read.csv("dolar.csv", stringsAsFactors = FALSE)%>%
  mutate(Fecha = as.Date(Fecha), Dolar = as.numeric(Dolar))

inicio_temporada <- as.Date("2017-09-01")

compras_fruta <- entradas_fruta%>%
  filter(Fecha >= inicio_temporada)%>%
  merge(dolar, by = "Fecha", all.x = TRUE )%>%
  tiempos()%>%
  mutate(Importe = Cantidad *Precio/Dolar)%>%
  ddply(.(Semana, Temporada), summarize, Compras_fruta = sum(Importe))

precios <- myfetch("tbPreciosEstimadosSalidas")%>%
  transmute(Folio = intNum_fol, Clave_presentacion = intCla_pre, Clave_fruta = intCla_fru ,
            Cantidad = intCan_tid, Precio_real = floPre_rea, Comision_aduanal = floCus_tom, 
            Manejo = floHan_dliD, Handlio = floHan_dliO, Comision_ventas = floSal_es,
            Caja = floCaj_a, Frio = floFri_o, Mano_obra = floMan_obr, Energia = floEne_rgi)%>%
  merge(presentaciones[,c("Clave_presentacion", "Presentacion")], all.x = TRUE)%>%
  merge(frutas[,c("Clave_fruta","Fruta")], all.x = TRUE)%>%
  mutate(Producto = paste(Fruta, Presentacion))%>%
  select(-Clave_fruta, - Clave_presentacion, -Fruta, -Presentacion)%>%
  mutate(Importe = Cantidad*Precio_real)%>%
  ddply(.(Folio), summarize, 
        Liquidacion = sum(Cantidad*Precio_real - Comision_aduanal - Manejo - Handlio - 
                            Comision_ventas - Caja - Frio - Mano_obra - Energia))

liquidaciones <- salidas_fruta%>% 
  filter(Fecha>= inicio_temporada)%>%
  select(Folio, Fecha)%>%
  unique()%>%
  merge(precios, by = "Folio", all.x = TRUE)%>%
  tiempos()%>%
  ddply(.(Semana, Temporada), summarize, Liquidacion = sum(Liquidacion, na.rm = TRUE))


retornos <- merge(compras_fruta, liquidaciones, by = c("Semana", "Temporada"), all = TRUE)%>%
  mutate(Retorno = Liquidacion - Compras_fruta)


