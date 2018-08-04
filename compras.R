
source("funciones.R")
library(plyr)
library(dplyr)
library(XML)
library(tidyr)
library(stringr)

compras <- data.frame()

raros <- c("280763cb-6363-4d3d-93eb-afc8b473fa24.xml", "43bfdaf0-2fd4-4bca-a30f-ca5df0233fa3.xml",
           "7f24a4c7-522e-4094-aeba-6f6bd6dcbf8c.xml", "df214f34-d460-4723-b0fa-d447f0de6b4c.xml",
            "df214f34-d460-4723-b0fa-d447f0de6b4c.xml")


for (var in setdiff(list.files("xmls/"), raros )){
  
  var2 <- paste0("xmls/",var)
  
  data_xml <- xmlParse(var2, encoding = "UTF-8")
  
  data_list  <- xmlToList(data_xml)
  
   
  tryCatch({     
    if(data_list$.attrs[["Version"]] == 3.3){
      
      
      nombre <- data.frame(Valor = unlist(data_list$Emisor))
      
      nombre$Nombres <- row.names(nombre) 
      
      atributos <- data.frame(Valor = unlist(data_list$.attrs))
      
      atributos$Nombres <- rownames(atributos)
      
      conceptos <- data.frame()
        
        for(i in 1:length(data_list$Conceptos)){
        
        x <- data.frame(Valor = unlist(data_list$Conceptos[[i]][[".attrs"]]))                    
        
        x$Nombres <- rownames(x)
        
        vector <- rbind(nombre, atributos, x)
        
        vector$No_concepto <- i
        
        conceptos <- rbind(conceptos, vector)
        }
        
        
        
        
        
        conceptos$Archivo <- var
        
        compras <- rbind(compras, conceptos)
        
        
        
        
    }} ,error=function(e){})

    tryCatch({ 
  if(data_list$.attrs[["version"]] == 3.2){
  
  nombre <- data.frame(Valor = unlist(data_list$Emisor[[".attrs"]]))
  
  nombre$Nombres <- row.names(nombre) 
  
  atributos <- data.frame(Valor = unlist(data_list$.attrs))
  
  atributos$Nombres <- rownames(atributos)
  
  conceptos <- data.frame()

  
    
  for(i in 1:length(data_list$Conceptos)){
    
    x <- data.frame(Valor = unlist(data_list$Conceptos[[i]]))                    
    
    x$Nombres <- rownames(x)
    
    vector <- rbind(nombre, atributos, x)
    
    vector$No_concepto <- i
    
    conceptos <- rbind(conceptos, vector)
  
    
    }}
  
  conceptos$Archivo <- var
  
  compras <- rbind(compras, conceptos)
  
  }, error = function(e){})}  
  

compras$Nombres <- sub("(.attrs.)", "",compras$Nombres, perl=TRUE)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}  


compras$Nombres <- firstup(compras$Nombres) 
compras[compras$Nombres == "MetodoDePago",]$Nombres <- "MetodoPago"


compras <- unique(compras)
 
#setdiff(list.files("xmls/"), unique(compras$Archivo)) 

rownames(compras) <- 1:length(compras$Valor)

compras_spread<- spread(compras,Nombres, Valor)%>%
  transmute(Folio = Folio, Nombre = as.character(Nombre), Fecha = as.Date(Fecha), Descripcion = as.character(Descripcion), 
            Cantidad = as.numeric(as.character(Cantidad)), Unidad = as.character(Unidad), ClaveUnidad = as.character(ClaveUnidad), 
            ValorUnitario = as.numeric(as.character(ValorUnitario)), ClaveProdServ = as.character(ClaveProdServ), 
            TipoCambio = as.numeric(as.character(TipoCambio)), SubTotal = as.numeric(as.character(SubTotal)), 
            Total = as.numeric(as.character(Total)), Moneda = as.character(Moneda))
  

compras_spread[grep("Americano", compras_spread$Moneda, ignore.case = TRUE),]$Moneda <- "USD"
compras_spread[grep("mexicano", compras_spread$Moneda, ignore.case = TRUE),]$Moneda <- "MXN"

compras_spread$Material <- NA
compras_spread[is.na(compras_spread$ClaveProdServ),]$ClaveProdServ <- 0

compras_spread[grep("CLAM|7575215CMSCPET|MIXIM 18OZ CLM UNLABELD 270/CS|7256187CMSCPET|7256187CMSCPET", 
                    compras_spread$Descripcion, ignore.case = TRUE),]$Material <- "CLAMS"

compras_spread[grep("Clam", compras_spread$Descripcion),]$Material <- "CLAMS"

compras_spread[grep("CHAROLA", compras_spread$Descripcion),]$Material <- "CAJAS"
compras_spread[grep("CAJA|CJA", compras_spread$Descripcion, ignore.case = TRUE),]$Material <- "CAJAS"
compras_spread[compras_spread$Nombre == "International Baskets S.A. de C.V.",]$Material <- "CLAMS"
compras_spread[compras_spread$Nombre == "PENPACK S DE RL DE CV",]$Material <- "CLAMS"
compras_spread[grep("TARIMA", compras_spread$Descripcion),]$Material <- "TARIMA"
compras_spread[grep("JESUS", compras_spread$Nombre),]$Material <- "TARIMA MADERA"

compras_spread[compras_spread$Nombre == "NAMINSA S.A. DE C.V.",]$Material <- "CLAMS"
compras_spread[grep("PAD", compras_spread$Descripcion),]$Material <- "PADS"

compras_spread[grep("SEPARADOR PUNNET|SEPARADOR DE CARTON 150 GR PUNNET NEGRA", 
                    compras_spread$Descripcion, ignore.case = TRUE),]$Material <- "SEPARADOR PUNNET"

compras_spread[grep("ESQUINERO", compras_spread$Descripcion),]$Material <- "ESQUINERO"
compras_spread[grep("CUBIERTAS TERMICAS", compras_spread$Descripcion),]$Material <- "CUBIERTAS TERMICAS"
compras_spread[compras_spread$Descripcion == "BASES TERMICAS",]$Material <- "BASES TERMICAS"
compras_spread[compras_spread$Descripcion == "BASE TERMICA",]$Material <- "BASES TERMICAS"
compras_spread[grep("UNICEL", compras_spread$Descripcion),]$Material <- "UNICEL"
compras_spread[grep("FLEJE", compras_spread$Descripcion),]$Material <- "FLEJE"
compras_spread[grep("GEL", compras_spread$Descripcion),]$Material <- "GEL"
compras_spread[grep("CINTAS ADHESIVAS", compras_spread$Descripcion),]$Material <- "MASKING"
compras_spread[grep("CINTA ADHESIVA", compras_spread$Descripcion),]$Material <- "MASKING"
compras_spread[grep("CUBIERTAS TERMICAS", compras_spread$Descripcion),]$Material <- "CUBIERTAS TERMICAS"
compras_spread[grep("CUBIERTA TERMICA", compras_spread$Descripcion),]$Material <- "CUBIERTAS TERMICAS"
compras_spread[grep("TERMOGRAFO", compras_spread$Descripcion),]$Material <- "TERMOGRAFO"
compras_spread[grep("TAR NOVAPAL", compras_spread$Descripcion),]$Material <- "TARIMA"
compras_spread[grep("BOLSA PLAST NEGRA", compras_spread$Descripcion),]$Material <- "BOLSAS NEGRAS"
compras_spread[grep("PELICULA PLASTICA", compras_spread$Descripcion),]$Material <- "EMPLAYE"
compras_spread[grep("BASCULA", compras_spread$Descripcion),]$Material <- "BASCULA"
compras_spread[grep("PISTOLA", compras_spread$Descripcion),]$Material <- "PISTOLA"
compras_spread[grep("HERRAMIENTAS", compras_spread$Descripcion),]$Material <- "HERRAMIENTAS"
compras_spread[grep("ABSPD", compras_spread$Descripcion),]$Material <- "PADS"
#compras_spread[compras_spread$ClaveProdServ == "24121500",]$Material <- "CLAMS"
#compras_spread[compras_spread$ClaveProdServ == "24121503",]$Material <- "CLAMS"

compras_spread[grep("CAJA GRAPA", compras_spread$Descripcion),]$Material <- "CAJA GRAPA"
compras_spread[grep("CAJA DE GRAPA", compras_spread$Descripcion),]$Material <- "CAJA GRAPA"
compras_spread[grep("CAJAS DE GRAPA", compras_spread$Descripcion),]$Material <- "CAJA GRAPA"
compras_spread[grep("ETIQ X PIST", compras_spread$Descripcion),]$Material <- "TRAZABILIDAD"
compras_spread[grep("SERVICIO DE ENTREGA", compras_spread$Descripcion),]$Material <- "ENTREGA"
compras_spread[grep("GROWN FRESH", compras_spread$Descripcion),]$Material <- "CAJAS"
compras_spread[grep("Nota de CrÃ©dito", compras_spread$Descripcion),]$Material <- "NOTA DE CREDITO"
compras_spread[grep("HURST", compras_spread$Descripcion, ignore.case = TRUE), ]$Material <- "CLAMS"
compras_spread[grep("CESTO MORA 150G|BUSH TRAY", compras_spread$Descripcion, ignore.case = TRUE), ]$Material <- "CAJAS"
compras_spread[grep("CANDADO CLAVO O BOTELLA BCO KLICKER|PATIN HIDRAULICO", 
                    compras_spread$Descripcion, ignore.case = TRUE), ]$Material <- "OTROS"

compras_spread[intersect(grep("separador", 
                    compras_spread$Descripcion, ignore.case = TRUE),
                    grep("cat", 
                         compras_spread$Descripcion, ignore.case = TRUE)),]$Material <- "SEPARADOR PUNNET"

compras_spread[grep("INTERLOCK", compras_spread$Descripcion),]$Material <- "INTERLOCK"


#marca
compras_spread$Marca <- NA
compras_spread[grep("HURST", compras_spread$Descripcion),]$Marca <- "HURST"
compras_spread[grep("ROUGE", compras_spread$Descripcion),]$Marca <- "ROUGE"
compras_spread[grep("PUNNET", compras_spread$Descripcion),]$Marca <- "PUNNET"
compras_spread[grep("ALPASA|ALPAZA|alpasa", compras_spread$Descripcion, ignore.case = TRUE),]$Marca <- "ALPASA"
compras_spread[grep("DEL MONTE", compras_spread$Descripcion),]$Marca <- "DEL MONTE"
compras_spread[grep("HURTS", compras_spread$Descripcion),]$Marca <- "HURST"

compras_spread2 <- compras_spread%>%
    filter(is.na(Material)) 
compras_spread[grep("HURTS", compras_spread$Descripcion),]$Marca <- "HURST"
compras_spread[grep("CAT", compras_spread$Descripcion),]$Marca <- "PUNNET"


#Clave

compras_spread$Clave <- NA

compras_spread[grep("34187", compras_spread$Descripcion),]$Clave <- "34187"
compras_spread[grep("3407", compras_spread$Descripcion),]$Clave <- "3407"
compras_spread[grep("4004", compras_spread$Descripcion),]$Clave <- "4004"
compras_spread[grep("3720", compras_spread$Descripcion),]$Clave <- "3720"
compras_spread[grep("5011", compras_spread$Descripcion),]$Clave <- "5011"
compras_spread[grep("R. 274", compras_spread$Descripcion),]$Clave <- "R.274"
compras_spread[grep("3709", compras_spread$Descripcion),]$Clave <- "3709"
compras_spread[grep("3903", compras_spread$Descripcion),]$Clave <- "3903"
compras_spread[grep("3705", compras_spread$Descripcion),]$Clave <- "3705"
compras_spread[grep("4007", compras_spread$Descripcion),]$Clave <- "4007"
compras_spread[grep("3832", compras_spread$Descripcion),]$Clave <- "3832"


#UNDS
compras_spread$Unds <- NA

compras_spread$Unds<- str_match(compras_spread$Descripcion, "([[0-9]]{0,3}\\.{0,1}[[0-9]]{1,5})(\\sUNDS)")[,2]
compras_spread$Unds <- as.numeric(gsub("\\.{1}","",compras_spread$Unds))


#FRUTAS
compras_spread$Fruta <- NA

compras_spread[grep("arandano|blueberry|BLUEBERRIES", compras_spread$Descripcion, ignore.case = TRUE),]$Fruta <- "ARANDANO"
compras_spread[grep("blackberry|mora|zarzamora|BLACK BERRIES|BLACKBERRIES|ROUGE|PUNNET|PUNET|ROUGUE", compras_spread$Descripcion, ignore.case = TRUE),]$Fruta <- "ZARZAMORA"
compras_spread[grep("RASPBERY", compras_spread$Descripcion, ignore.case = TRUE),]$Fruta <- "FRAMBUESA"

compras_spread[intersect(grep("blackberry|mora|zarzamora|BLACK BERRIES|BLACKBERRIES|ROUGE|PUNNET|PUNET|ROUGUE|CAT", 
                    compras_spread$Descripcion, ignore.case = TRUE),
                    grep("arandano|blueberry|BLUEBERRIES", compras_spread$Descripcion, ignore.case = TRUE)),]$Fruta <- "MIXTO"

#embalajes

compras_spread$Embalaje <- NA

compras_spread[grep("6 OZ|6OZ|6 ONZAS", compras_spread$Descripcion, ignore.case = TRUE),]$Embalaje <-  "6OZ"
compras_spread[grep("12 ONZAS|12 OZ|12oz ", compras_spread$Descripcion, ignore.case = TRUE),]$Embalaje <-  "12OZ"  
compras_spread[grep("4x4|4.4", compras_spread$Descripcion, ignore.case = TRUE),]$Embalaje <-  "4.4OZ"
compras_spread[grep("18 OZ|18OZ|18 ONZAS", compras_spread$Descripcion, ignore.case = TRUE),]$Embalaje <-  "18OZ"
compras_spread[grep("PUNNET|150 GRS|PUNET|150G|CAT", compras_spread$Descripcion, ignore.case = TRUE),]$Embalaje <-  "PUNNET"
compras_spread[grep("ROUGE|ROUGUE", compras_spread$Descripcion, ignore.case = TRUE),]$Embalaje <-  "ROUGE"



compras_spread2 <- compras_spread%>%
    filter(is.na(Material)) 


var <- "280763cb-6363-4d3d-93eb-afc8b473fa24.xml"

