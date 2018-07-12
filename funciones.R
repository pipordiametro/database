library(RODBC)
myfetch <- function(nombre,base = FALSE){
  if(base == TRUE){
    con <- odbcConnect(dsn = "SQLProyecto08", uid = "francisco", pwd = "Alpasa2017")
    var <- sqlfetch(con, nombre, stringsAsFactor = FALSE) 
    odbcClose(con)     
    }else{
      var <- read.csv(paste0("proyecto/", nombre,".csv"))
   
  }
  return(var)
}
