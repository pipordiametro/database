library(RODBC)
library(plyr)
library(dplyr)

inicio_temporada <- as.Date("2017-09-01")

myfetch <- function(nombre,base = FALSE){
  if(unname(Sys.info()["nodename"] == "DESKTOP-LQ3B302") ){
    con <- odbcConnect(dsn = "SQLProyecto08", uid = "francisco", pwd = "Alpasa2017")
    var <- sqlFetch(con, nombre, as.is = TRUE) 
    odbcClose(con)
    write.csv(var, paste0("proyecto/", nombre, ".csv"))
    }else{
      var <- read.csv(paste0("proyecto/", nombre,".csv"))
   
  }
  return(var)
}

myfetch2 <- function(nombre,base = FALSE){
     var <- read.csv(paste0("proyecto/", nombre,".csv"))
    
  return(var)
}

get.precios <- function(){
  library(RCurl)
  library(XML)
  frutas.df <- data.frame(Fruta = c("Blackberries", "Blueberries"), 
                          Url = c("https://www.marketnews.usda.gov/mnp/fv-report-top-filters?&commAbr=BLKBERI-V&region=&repType=shipPriceDaily&portal=fv&locName=&type=shipPrice&navClass=&navClass=&navType=byComm&varName=&locAbr=&volume=&commName=BLACKBERRIES&dr=1&repDate=", 
                                  "http://www.marketnews.usda.gov/mnp/fv-report-top-filters?&commAbr=BLUBY&shipNavClass=&portal=fv&repType=shipPriceDaily&movNavClass=&Go=Go&locName=&type=shipPrice&locAbrAll=&navClass=FRUITS&navType=byComm&organic=&environment=&locAbr=&volume=&stateID=&commName=BLUEBERRIES&termNavClass=&repDate="))
  for(var in frutas.df$Fruta){
    
    fruta <- var
    
    purl <- frutas.df[frutas.df == var,]$Url
    
    fruit <- read.csv(paste0("Precios/",var,".csv"))%>%
      mutate(reportDate = as.Date(reportDate))
    
    dias <- as.Date(c(max(fruit$reportDate):Sys.Date()), origin = "1970-01-01")
    
    fruit <- fruit[,-c(1)]
    
    for(var2 in dias){
      
      tryCatch({    
        #var <- as.Date("2018-01-23")
        var2 <- as.Date(var2,origin="1970-01-01") 
        
        dia <- format(var2,"%d")
        
        mes <- format(var2,"%m")
        
        year <- format(var2,"%Y")
        
        url <- paste0(purl,mes,"%2F",dia,"%2F",year,"&endDate=",mes,"%2F",dia,"%2F",
                      year,"&format=xml&rebuild=false")
        
        dest <- paste0("Precios/files/",fruta,"/",format(var2,"%Y-%m-%d"),".xml")
        
        if(!file.exists(dest)){
          download.file(url,dest,quiet = TRUE,method="libcurl")
        }
      },error=function(e){})
      
    }
    
    for(var3 in dias){
      #var <- "2018-01-17"
      var3 <- as.Date(var3,origin="1970-01-01") 
      dia <- format(var3,"%d")
      mes <- format(var3,"%m")
      year <- format(var3,"%Y")
      dest <- paste0("Precios/files/",fruta,"/",format(var3,"%Y-%m-%d"),".xml")
      
      if(file.exists(dest)){
        data <- xmlParse(dest)
        #rootnode <- xmlRoot(data)
        table <- xmlToDataFrame(data)
        #table <- table[,-c(1)]
        table$reportDate <- as.Date(table$reportDate,format="%m/%d/%Y")
        fruit <- rbind(fruit,table)
        fruit <- unique(fruit)
      }
    }
    
    write.csv(fruit,paste0("Precios/files/",fruta,".csv"))
  }
}

dbbackup <- function(){
  con <- odbcConnect(dsn = "SQLProyecto08", uid = "francisco", pwd = "Alpasa2017")
  for (var in sqlTables(con)$TABLE_NAME){
    write.csv(sqlFetch(con,var, as.is = TRUE),paste0("proyecto/",var,".csv"))
  }
  odbcClose(con)
}

ploteos <- function(df, fecha = "Fecha" , cantidad = "Cantidad"){

  df <- data.frame(Fecha = df[,c(fecha)], Cantidad = df[,c(cantidad)])%>%
    ddply(.(Fecha), summarize, Total = sum(Cantidad))
  
  minyear <- min(as.integer(format(df$Fecha, format = "%Y")))
  
  df <- df%>% 
    merge(data.frame(Fecha = as.Date(c(min(df$Fecha):max(df$Fecha)), 
                                     origin = "1970-01-01")), all.y = TRUE)%>%
    tiempos()
  
  df[is.na(df$Total),]$Total <- 0
  
  semanal <- ddply(df,.(Temporada, Semanats, Semana), summarize, Total = sum(Total))%>%
    arrange(Semanats)
  
  semanal.ts <- ts(semanal$Total, c(minyear, min(semanal$Semanats)), frequency = 52) 
  
  semanal.ts <- decompose(semanal.ts, "additive")
  
  semanal.cum <- semanal%>%
    arrange(Semana)%>%
    group_by(Temporada)%>%
    mutate(Acumulado =cumsum(Total))
  
  ts.semanal.cum <- ts(semanal.cum$Acumulado, c(minyear, min(semanal$Semanats)), frequency = 52)
  
  
  
  print(ggseasonplot(semanal.ts))
  
  dev.new()
  plot(dec.semanal)
  dev.new()
  print(ggseasonplot(ts.semanal.cum))
  dev.new()
  plot(dec.semanal.cum)
}

tiempos <- function(df, Campos = c("Semana", "Semanats", "Year", "Temporada")){
  
  df_mod <- df%>%
    mutate(Semana = as.integer(format(Fecha, "%U")),
           Year = as.integer(format(Fecha , "%Y")),
           Year = ifelse(Semana == 0, Year - 1, Year),
           Semana = ifelse(Semana %in% c(0,53), 52, Semana),
           Semanats = min(Year - 2010)*52 + as.integer(Semana),
           Semana = factor(Semana, levels = (c(34:85)%%52 + 1), ordered = TRUE),
           Temporada = ifelse((Fecha >= as.Date("2013-09-01") & Fecha < as.Date("2014-09-01")), "2013-2014",
                              ifelse((Fecha >= as.Date("2014-09-01") & Fecha < as.Date("2015-09-01")), "2014-2015",
                                     ifelse((Fecha >= as.Date("2015-09-01") & Fecha < as.Date("2016-09-01")), "2015-2016",
                                            ifelse((Fecha >= as.Date("2016-09-01") & Fecha < as.Date("2017-09-01")), "2016-2017",
                                                   ifelse((Fecha >= as.Date("2017-09-01") & Fecha < as.Date("2018-09-01")), "2017-2018",
                                                          ifelse((Fecha >= as.Date("2018-09-01") & Fecha < as.Date("2019-09-01")), "2018-2019",NA)))))),
           Temporada = factor(Temporada, levels = c("2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                                                    "2017-2018", "2018-2019")))
  df_mod <- df_mod[,c(names(df), Campos)]   
  
  return(df_mod)
}

plots.productos <- function(df){
  library(plotly)
  productos <- unique(df[df$Temporada =="2017-2018",]$Producto)
  
  graficas <<- new.env()
  
  for (var in productos){
    
    assign(var, df[df$Producto == var,]%>%
             merge(merge(data.frame(Temporada = unique(df$Temporada)), 
                         data.frame(Semana = c(1:52))), 
                   all.y = TRUE)%>%
             mutate(Total = ifelse(is.na(Total), 0, Total))%>%
             ddply(.(Semana, Temporada), summarize, Total = sum(Total)), envir = graficas)
    
    assign(paste0(var,".plot"), ggplotly(ggplot(get(var, envir = graficas), 
                                                aes(x = Semana, y = Total, 
                                                    colour = Temporada, 
                                                    group = Temporada)) + 
                                           geom_line() + labs(title = var)), graficas)
  }
  
}

kable2 = function(...) {
  knitr::kable(..., format.args = list(decimal.mark = '.', big.mark = ","))
}

#dolar
getdolar <- function(){
  library(imputeTS)
  hoy <- format(Sys.Date(),format ="%Y-%m-%d")
  dolarurl <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?chart_type=line&recession_bars=on&log_scales=&bgcolor=%23e1e9f0&graph_bgcolor=%23ffffff&fo=Open+Sans&ts=12&tts=12&txtcolor=%23444444&show_legend=yes&show_axis_titles=yes&drp=0&cosd=2016-09-01&coed=",
                     hoy,"&height=450&stacking=&range=Custom&mode=fred&id=DEXMXUS&transformation=lin&nd=1993-11-08&ost=-99999&oet=99999&lsv=&lev=&mma=0&fml=a&fgst=lin&fgsnd=2009-06-01&fq=Daily&fam=avg&vintage_date=&revision_date=&line_color=%234572a7&line_style=solid&lw=2&scale=left&mark_type=none&mw=2&width=968")
  
  download.file(dolarurl,"DEXMXUS.csv",quiet = TRUE,method="libcurl")
  dolar <- read.csv("DEXMXUS.csv", stringsAsFactors = FALSE)%>%
    mutate(DATE =as.Date(DATE))
  dias <- data.frame(DATE = c(as.Date(c(min(dolar$DATE):max(dolar$DATE)), origin = "1970-01-01")))
  dolar <- merge(dias, dolar, all.x = TRUE, by = "DATE")%>%
    mutate(DEXMXUS = ifelse(DEXMXUS ==".", NA, DEXMXUS),
           DEXMXUS = as.numeric(DEXMXUS))%>%
    arrange(DATE)%>%
    na.interpolation(DEXMXUS, option = "linear")%>%
    rename(Fecha = DATE, Dolar = DEXMXUS)%>%
    write.csv("dolar.csv", row.names = FALSE)

}

get.productos <- function(df, Campos = c("Clave_fruta", "Clave_presentacion",  "Nombre_producto", "Clams",             
                                              "Peso", "Unidad", "Presentacion", "Nombre_fruta", "Nombre_corto_fruta", "Variedad", "Fruta",             
                                              "Producto", "Fraccion6oz")){
  
  df <- merge(myfetch("tbProductos")%>%
                       filter(strCan_cel == "NO"),
                     myfetch("tbPresentaciones")%>%
                       filter(strCan_cel == "NO"), 
                     by = "intCla_pre", suffixes = c("h", "b"))%>%
    transmute(Clave_presentacion = intCla_pre, intCla_fru = intCla_fru, 
              Clave_producto = strCla_prod, Nombre_producto = strNom_bre,
              Clams = as.integer(intCan_tid), Peso = as.numeric(intPes_o), 
              Unidad = strUni_med)%>%
    mutate(Presentacion = paste0(Clams,"X", Peso, Unidad))%>%
    merge(myfetch("tbFrutas"), by = "intCla_fru")%>%
    rename(Clave_fruta = intCla_fru, Nombre_fruta = strNom_bre, Nombre_corto_fruta = strNom_cto, 
              Variedad = strVar_ied, Variedad_corto = strVar_cto, Tipo_fruta = strTip_o,
              Tipo_corto = strTip_cto)%>% 
    mutate(Fruta = paste(Nombre_corto_fruta,Tipo_corto))%>%
    mutate(Producto = paste0(Fruta,"_", Presentacion, "_", Nombre_producto),
           Fraccion6oz = ifelse(Unidad == "G", Clams*Peso*0.035274/72  , Clams * Peso/72))%>%
    select(Clave_producto, one_of(Campos))%>%
    merge(df, by = "Clave_producto")
  
  return(df)
  

}

load.productos <- function(){

productos <<- merge(myfetch("tbProductos")%>%
                     filter(strCan_cel == "NO"),
                   myfetch("tbPresentaciones")%>%
                     filter(strCan_cel == "NO"), 
                   by = "intCla_pre", suffixes = c("h", "b"))%>%
  transmute(Clave_presentacion = intCla_pre, intCla_fru = intCla_fru, 
            Clave_producto = strCla_prod, Nombre_producto = strNom_bre,
            Clams = as.integer(intCan_tid), Peso = as.numeric(intPes_o), 
            Unidad = strUni_med)%>%
  mutate(Presentacion = paste0(Clams,"X", Peso, Unidad))%>%
  merge(myfetch("tbFrutas"), by = "intCla_fru")%>%
  rename(Clave_fruta = intCla_fru, Nombre_fruta = strNom_bre, Nombre_corto_fruta = strNom_cto, 
         Variedad = strVar_ied, Variedad_corto = strVar_cto, Tipo_fruta = strTip_o,
         Tipo_corto = strTip_cto)%>% 
  mutate(Fruta = paste(Nombre_corto_fruta,Tipo_corto))%>%
  mutate(Producto = paste0(Fruta,"_", Presentacion, "_", Nombre_producto),
         Fraccion6oz = ifelse(Unidad == "G", Clams*Peso*0.035274/72  , Clams * Peso/72))

}



precio.promedio_diario <- function(df){

  precio_diario <- read.csv("csvs/precio.mean_diario.csv", stringsAsFactors = FALSE)%>%
    mutate(Fecha = as.Date(Fecha), Precio_promedio = as.numeric(Precio_promedio))
  
  df <- merge(df,precio_diario, by = c("Fecha", "Clave_producto"))
  
  return(df)
  
}

precio.promedio_semanal <- function(df){
  
  precio_semanal <- read.csv("csvs/precio.mean_semanal.csv", stringsAsFactors = FALSE)%>%
    mutate(Precio_semanal = as.numeric(Precio_semanal))
  
  df <- merge(df, precio_semanal, by = c("Semana", "Temporada", "Clave_producto"), all.x = TRUE)
  
  return(df)
  
  
}    

nombres.productores <- function(df, Campos = c("Nombre_completo")){
  
  productores <- myfetch("tbProductores")%>%
    filter(strCan_cel == "NO")%>%
    transmute(Clave_productor = intCla_pro, Nombre = strNom_bre, Apellido_paterno = strApe_pat,
              Apellido_materno = strApe_mat, 
              Nombre_completo = paste(Apellido_paterno, Apellido_materno, Nombre))
  
  df <- merge(df, productores[,c("Clave_productor", Campos)], by ="Clave_productor", all.x =TRUE)
}

entradas.fruta <- function(Campos = c("Id_entradas","Idb_entradas", "Nota", "Folio", "Fecha", "Clave_productor", "Clave_acopio",   
                                      "Cantidad", "Rechazadas", "Clave_producto", "Pallet", "Precio",
                                      "Numero_pago", "Pagado", "Aceptadas")){

  entradas_fruta <- merge(myfetch("tbrecFruEmp"), myfetch("tbrecFruEmpReg"), 
                          by = "intNum_reg", suffixes = c("h","b"))%>%
    filter(strCan_celb == "NO")%>%
    transmute(Id_entradas = intNum_reg, Idb_entradas = intNum_regA, Folio = intNum_fol, Nota = intNum_not, 
              Fecha = as.Date(fecFec_not), Clave_productor = intCla_pro, 
              Clave_acopio = intCen_aco, Cantidad = as.integer(intCan_tid), Rechazadas = as.integer(intCan_rec), Clave_producto =  intCla_prod,
              Pallet = intNum_pal, Precio = as.numeric(floPre_uni), Numero_pago = intNum_pag, Pagado = strPag_ado)%>%
    filter(Fecha >= inicio_temporada)%>%
    mutate(Aceptadas = Cantidad - Rechazadas)%>%
    select(one_of(Campos))
 
 return(entradas_fruta)
}

get.acopio <- function(df){
  
  df <- merge(df,myfetch("tbAlmacenes")%>%
                filter(strCan_cel == "NO")%>%
                transmute(Clave_acopio = intCla_alm, Acopio = strNom_bre), 
              by = "Clave_acopio", suffixes = c("h", "b"))
  
  return(df)
}

entradas.material <- function(){
  
  entradas_material <- merge(myfetch("tbAlmEnt"),myfetch("tbAlmEntReg"), 
                             by = "intNum_reg", all.x = TRUE, suffixes = c("h", "b"))%>%
    filter(strCan_celb == "NO")%>%
    transmute(Clave_acopio = intCla_alm, Tipo_entrada =  strTip_ent, Fecha = as.Date(fecFec_fac), 
              Tipo_proveedor = strTip_prov, Cantidad = intCan_tid , Clave_material = intCla_mat,
              Clave_proveedor = intCla_prov)%>%
    filter(Fecha > inicio_temporada)
    

  
  return(entradas_material)
}

entregas.material <- function(Campos = c("Clave_acopio", "Tipo_salida", "Clave_productor", "Fecha", "Cantidad",
                                         "Clave_material", "Tipo_receptor" )){
  
  entregas_material <- merge(myfetch("tbAlmSal"),myfetch("tbAlmsalReg"), 
                             by = "intNum_reg", all.x = TRUE, suffixes = c("h", "b"))%>%
    filter(strCan_celb == "NO")%>%
    transmute(Clave_acopio = intCla_alm, Tipo_salida = strTip_sal, Clave_productor = intCla_pro,
              Fecha = as.Date(fecFec_not), Cantidad = intCan_tid, Clave_material = intCla_mat, 
              Tipo_receptor = strTip_pro )%>%
    filter(Fecha > inicio_temporada)%>%
    select(one_of(Campos))
  
  return(entregas_material)
}

get.material <- function(df, Campos = c("Clave_material", "Marca", "Tipo_material", "Presentacion",
                                        "Clave_presentacion", "Material", "Clams", "Peso", "Unidad")){
  
  df <- merge(df, merge(myfetch("tbMateriales")%>%
                          transmute(Clave_material = intCla_mat, Marca = strMar_ca,
                                    Tipo_material = strTip_o,
                                    Clave_presentacion = intCla_pre), 
                        myfetch("tbPresentaciones")%>%
                          transmute(Clave_presentacion = intCla_pre, Clams = as.integer(intCan_tid), 
                                    Peso = as.numeric(intPes_o), 
                                    Unidad = strUni_med, Cantidad = intCan_tid),
                        all.x = TRUE, by = "Clave_presentacion")%>%
                mutate(Presentacion = ifelse(Tipo_material == "CLAMSHELL", paste0(Peso, Unidad), 
                                             paste0(Cantidad, "X", Peso, Unidad)))%>%
                mutate(Material = paste(Marca, Presentacion, Tipo_material))%>%
                select(Clave_material, one_of(Campos)),
              by = "Clave_material", all.x = TRUE)
              
  return(df)              
   
}


load.materiales <- function( Campos = c("Clave_material", "Marca", "Tipo_material", "Presentacion",
                                        "Clave_presentacion", "Material")){
  
  materiales <<- myfetch("tbMateriales")%>%
    transmute(Clave_material = intCla_mat, Marca = strMar_ca,
              Tipo_material = strTip_o, Presentacion = strPre_sen,
              Clave_presentacion = intCla_pre,
              Material = paste(Marca, Presentacion, Tipo_material))%>%
    select(Clave_material, one_of(Campos))
 
}

load.presentaciones <- function(){
  
  presentaciones <<- myfetch("tbPresentaciones")
}



get.presentacion <- function(df, Campos = c("Clams", "Peso", "Unidad", "Presentacion")){
  
  df <- merge(df, myfetch("tbPresentaciones")%>%
                transmute(Clave_presentacion = intCla_pre, Clams = as.integer(intCan_tid), 
                          Peso = as.numeric(intPes_o), 
                          Unidad = strUni_med, Tipo_material = strTip_o)%>%
                mutate_if(Tipo_material == "CLAMSHELL", Presentacion = paste0(Peso, Unidad),
                          Presentacion = paste0(Clams,"X", Peso, Unidad))%>%
                select(Clave_presentacion, one_of(Campos)),
              all.x = TRUE, by = "Clave_presentacion")

}

load.saldos.material <- function(){
 
saldos_material <<-    rbind(
    entradas.fruta()%>%
    #  filter(Clave_productor == str_match(input$Clave_productor2, "^[[0-9]]{0,3}")[,1])%>%
      select(Fecha, Clave_productor, Presentacion,Total)%>%
      filter(Fecha >= inicio_temporada)%>%
      mutate(Tipo_material = "CAJA EMP.", Tipo = "CON FRUTA", Total = - Total),   #Cajas con fruta
    entradas.fruta()%>%
    #  filter(Clave_productor == str_match(input$Clave_productor2, "^[[0-9]]{0,3}")[,1])%>%
      get.productos(Campos = c( "Peso", "Unidad", "Clams"))%>%
      mutate(Total = Total*Clams,
             Presentacion = paste0(Peso,Unidad))%>%
      select(Fecha, Clave_productor, Presentacion, Total)%>%
      mutate(Tipo_material = "CLAMSHELL", Tipo = "CON FRUTA", Total = -Total),    #clams con fruta
    
    #prestamos
    entregas.material()%>%
    #  filter(Clave_productor == str_match(input$Clave_productor2, "^[[0-9]]{0,3}")[,1])%>%
      filter(Tipo_salida == "PRESTAMO", Tipo_receptor == "PRODUCTOR")%>%
      ddply(.(Fecha, Clave_material, Clave_productor), summarize, 
            Total = sum(Cantidad))%>%
      get.material(Campos = c("Presentacion", "Tipo_material")) %>%
      mutate(Tipo = "PRESTAMO")%>%
      select(Fecha, Clave_productor, Presentacion, Total, Tipo_material, Tipo),
    
    #devoluciones
    
    entradas_material%>%
      rename(Clave_productor = Clave_proveedor)%>%
   #   filter(Clave_productor == str_match(input$Clave_productor2, "^[[0-9]]{0,3}")[,1])%>%
      filter(Tipo_entrada == "DEVOLUCION", Tipo_proveedor == "PRODUCTOR")%>%
      rename(Tipo = Tipo_entrada)%>%
      get.material(Campos = c("Presentacion", "Tipo_material"))%>%
      ddply(.(Fecha, Clave_productor, Presentacion, Tipo_material, Tipo), 
            summarize, Total = -sum(Cantidad))%>%
      select(Fecha, Clave_productor, Presentacion, Total, Tipo_material, Tipo))%>%
    
  #  filter(Clave_productor == str_match(input$Clave_productor2, "^[[0-9]]{0,3}")[,1])%>%
    rename(Concepto = Tipo)%>%
    ddply(.(Fecha, Clave_productor, Concepto,Tipo_material, Presentacion), summarize, 
          Total = sum(Total))%>%
    tiempos(Campos = c("Semana"))
  
  
  
}

semanas <- function(df, Campos = list(Total = 0)){
  
  df <- merge(
              data.frame(Semana = factor(c(1:52), levels = (c(34:85)%%52 + 1), ordered = TRUE)),
              df, all.x = TRUE, by = "Semana")%>%
    replace_na(Campos)
  
}

dias <- function(df, Campos = list(Total = 0)){
  
  df <- merge(data.frame(Fecha = as.Date(c(inicio_temporada:Sys.Date()), origin = "1970-01-01")),
              df, all.x = TRUE, by = "Fecha")%>%
    replace_na(Campos)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

myhcsemana <- function(df, name = "Producto", data = "Total"){
  
  df2 <- data.frame(Semana = df$Semana)
  
  
  df2$Producto <- df[,c(name)]
  df2$Total <- df[,c(data)]
  
  x = list()
  for(var in  unique(df2$Producto)){
    y  <- df2%>%
      filter(Producto == var)%>%
      semanas(Campos = list(Total = 0))%>%
      arrange(Semana)
    
    x[[length(x)+1]] <-  list(name = var, data = y$Total)
  
  }
  
  highchart()%>%
    hc_add_series_list(x)%>%
    hc_chart(type = "column")%>%
    hc_chart(zoomType = "xy", panKey = "ctrl", panning = TRUE)%>%
    hc_xAxis(list(type = "category", 
                  categories =  sort(unique(data.frame(Semana = factor(c(1:52), 
                                                                       levels = (c(34:85)%%52 + 1), 
                                                                       ordered = TRUE))[,1])),
                  min =  min(as.numeric(df2[df2$Total != 0,]$Semana))-1, 
                  max = max(as.numeric(df2[df2$Total != 0,]$Semana))))
    
               
    
}
