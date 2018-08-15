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

tiempos <- function(df){
  
  df_mod <- df%>%
    mutate(Semana = as.integer(format(Fecha, "%U")),
           Year = as.integer(format(Fecha , "%Y")),
           Year = ifelse(Semana == 0, Year - 1, Year),
           Semana = ifelse(Semana %in% c(0,53), 52, Semana),
           Semanats = min(Year - 2010)*52 + as.integer(Semana),
           Semana = factor(Semana, levels = (c(34:85)%%52 + 1)),
           Temporada = ifelse((Fecha >= as.Date("2013-09-01") & Fecha < as.Date("2014-09-01")), "2013-2014",
                              ifelse((Fecha >= as.Date("2014-09-01") & Fecha < as.Date("2015-09-01")), "2014-2015",
                                     ifelse((Fecha >= as.Date("2015-09-01") & Fecha < as.Date("2016-09-01")), "2015-2016",
                                            ifelse((Fecha >= as.Date("2016-09-01") & Fecha < as.Date("2017-09-01")), "2016-2017",
                                                   ifelse((Fecha >= as.Date("2017-09-01") & Fecha < as.Date("2018-09-01")), "2017-2018",
                                                          ifelse((Fecha >= as.Date("2018-09-01") & Fecha < as.Date("2019-09-01")), "2018-2019",NA)))))),
           Temporada = factor(Temporada, levels = c("2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                                                    "2017-2018", "2018-2019")))
                  
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

nombres.productos <- function(df, Campos = c("Producto")){
  
  productos <- myfetch("tbProductos")
  
  names(productos) <- paste0("p", names(productos))
  
  productos <- productos%>%
    filter(pstrCan_cel == "NO")%>%
    transmute(Clave_presentacion = pintCla_pre, Clave_fruta = pintCla_fru, 
              Clave_producto = pstrCla_prod,Nombre_producto = pstrNom_bre)
  
  presentaciones <- myfetch("tbPresentaciones")%>%
    filter(strCan_cel == "NO")%>%
    transmute(Clave_presentacion = intCla_pre, Clams = as.integer(intCan_tid), 
              Peso = as.numeric(intPes_o), Unidad = strUni_med )%>%
    mutate(Presentacion = paste0(Clams,"X", Peso, Unidad))
  
  
  frutas <- myfetch("tbFrutas")%>%
    transmute(Clave_fruta = intCla_fru, Nombre = strNom_bre, Nombre_corto = strNom_cto, 
              Variedad = strVar_ied, Variedad_corto = strVar_cto, Tipo_fruta = strTip_o,
              Tipo_corto = strTip_cto)%>% 
    mutate(Fruta = paste(Nombre_corto,Tipo_corto))
  
  productos <- merge(productos, presentaciones, all.x = TRUE)%>%
    merge(frutas, by = "Clave_fruta", all.x = TRUE)%>%
    mutate(Producto = paste0(Fruta,"_", Presentacion, "_", Nombre_producto),
           Fraccion6oz = ifelse(Unidad == "G", Clams*Peso*0.035274/72  , Clams * Peso/72))
  
  
  productos <- productos[,c("Clave_producto", Campos)] 
  
  df <- merge(df, productos, by = "Clave_producto", all.x = TRUE)
  
  return(df)
  

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