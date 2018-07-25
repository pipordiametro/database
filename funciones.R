library(RODBC)
library(plyr)
library(dplyr)
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

backup <- function(){
  con <- odbcConnect(dsn = "SQLProyecto08", uid = "francisco", pwd = "Alpasa2017")
  for (var in sqlTables(con)$TABLE_NAME){
    write.csv(sqlFetch(con,var, as.is = TRUE),paste0("proyecto/",var,".csv"))
  }
  odbcClose(con)
}
