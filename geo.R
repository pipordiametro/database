## @knitr datos

library(RODBC)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(scales)
library(plotly)
library(kableExtra)
library(knitr)
library(fpp)
library(PerformanceAnalytics)
library(RCurl)
library(XML)


source("registros.R",encoding="utf-8")

#filter(!(Cajasanab == 0))

#plot todos los campos
anabmelt <- melt(anab, c("Year", "Semana"), value.name = "Factor")%>%
  filter(!variable == "Semanats")

#serie real

anab.ts <- ts(anab$Real, c(2014,47), frequency = 52)
anab.ts.plot<- ggplotly(ggseasonplot(anab.ts) + labs(title ="Enviado Reale Anaberries"))
anab.decomp <- decompose(anab.ts, "additive")
anab.seasonal.plot <- ggplotly(ggseasonplot(anab.decomp$seasonal))



#suma semanas alpasa

alp <- ddply(registros[!is.na(registros$Cajas),],.(Year, Semana), summarize, Total = sum(Cajas))
alp$Semana <- as.integer(alp$Semana)
alp$Semanats <- alp$Semana + (alp$Year - 2013)*52
#alp <- filter(alp,!Semanats == 128)

semanas <- data.frame(c(39:max(alp$Semanats)))
names(semanas) <- c("Semanats")
alp.ts <-   merge(alp, semanas, all.y = TRUE)%>%
  select(Total)

alp.ts[is.na(alp.ts$Total),] <- 0

alp.ts <-   ts(alp.ts$Total, c(2014, 5), frequency = 52)
alp.ts.plot <- ggplotly(ggseasonplot(alp.ts) +labs (title = "PRODUCCION ALPASA EN 6 OZ"))
alp.decompose <- decompose(alp.ts, "additive")

#alpasa acumulativo

alpcum <-  registros[!is.na(registros$Cajas),]%>%
  ddply(.(Temporada, Semana), summarize, Total = sum(Cajas))%>%
  mutate(Semanatemp =  (as.integer(Semana) + 18)%%52)%>%
  merge(merge(data.frame(Semanatemp = c(1:52)),
              data.frame(Temporada = unique(registros$Temporada))),
        all.y = TRUE)

alpcum[is.na(alpcum$Total),]$Total <- 0

alpcum <- alpcum%>%
  group_by(Temporada)%>%
  arrange(Semanatemp) %>%
  mutate(Acumulado = cumsum(Total))%>%
  ungroup()




alpcum.plot <- ggplot(alpcum, aes(x = Semanatemp, y = Acumulado, colour = Temporada, fill = Temporada)) + 
  geom_area(data =alpcum[alpcum$Temporada == "2013-2014",], alpha = 0.2) +
  geom_area(data =alpcum[alpcum$Temporada == "2014-2015",], alpha = 0.2) +
  geom_area(data =alpcum[alpcum$Temporada == "2015-2016",], alpha = 0.2) +
  geom_area(data =alpcum[alpcum$Temporada == "2016-2017",], alpha = 0.2) +
  geom_area(data =alpcum[alpcum$Temporada == "2017-2018",], alpha = 0.2) +
  geom_line()

alpcum.plot <- ggplotly(alpcum.plot)  
  #porcentajes dinero 

alpmoney <- ddply(registros[!is.na(registros$Aceptadas),],.(Year, Semana), summarize, Total = sum(Aceptadas*Precio))
alpmoney$Semana <- as.integer(alpmoney$Semana)
alpmoney$Semanats <- alpmoney$Semana + (alpmoney$Year - 2013)*52
#alp <- filter(alp,!Semanats == 128)

semanas <- data.frame(c(39:max(alpmoney$Semanats)))
names(semanas) <- c("Semanats")
alpmoney.ts <-   merge(alpmoney, semanas, all.y = TRUE)%>%
  select(Total)

alpmoney.ts[is.na(alpmoney.ts$Total),] <- 0

alpmoney.ts <-   ts(alpmoney.ts$Total, c(2014, 5), frequency = 52)
alpmoney.ts.plot <- ggplotly(ggseasonplot(alpmoney.ts) +labs (title = "PAGOS ALPASA"))


#alpasa acumulativo

alpmoneycum <-  registros[!is.na(registros$Aceptadas),]%>%
  ddply(.(Temporada, Semana), summarize, Total = sum(Aceptadas*Precio))%>%
  mutate(Semanatemp =  (as.integer(Semana) + 18)%%52)%>%
  merge(merge(data.frame(Semanatemp = c(1:52)),
              data.frame(Temporada = unique(registros$Temporada))),
        all.y = TRUE)

alpmoneycum[is.na(alpmoneycum$Total),]$Total <- 0

alpmoneycum <- alpmoneycum%>%
  group_by(Temporada)%>%
  arrange(Semanatemp) %>%
  mutate(Acumulado = cumsum(Total))%>%
  ungroup()




alpmoneycum.plot <- ggplot(alpmoneycum, aes(x = Semanatemp, 
                                            y = Acumulado, 
                                            colour = Temporada, fill = Temporada)) + 
  geom_area(data =alpmoneycum[alpmoneycum$Temporada == "2013-2014",], alpha = 0.2) +
  geom_area(data =alpmoneycum[alpmoneycum$Temporada == "2014-2015",], alpha = 0.2) +
  geom_area(data =alpmoneycum[alpmoneycum$Temporada == "2015-2016",], alpha = 0.2) +
  geom_area(data =alpmoneycum[alpmoneycum$Temporada == "2016-2017",], alpha = 0.2) +
  geom_area(data =alpmoneycum[alpmoneycum$Temporada == "2017-2018",], alpha = 0.2) +
  geom_line()

alpmoneycum.plot <- ggplotly(alpmoneycum.plot)

alpmoney.decompose <- decompose(alpmoney.ts)



#comparativo



comp <- merge(anab[,c( "Semanats","Real")],
              alp%>%
                mutate(Semanats = Semanats), by.x = c( "Semanats"), 
              by.y = c( "Semanats"),all.x = TRUE)%>%
  select(Semanats, Real, Total, Year)%>%
  filter(Total < Real)%>%
  merge(semanas, all.y = TRUE)%>%
  filter((Semanats >= 133) & !(Semanats %in% c(233,232,231,264)))%>%
  mutate(Ratio = Total/Real)
  



#ts ratio real

ratio.ts <- ts(comp$Ratio, c(1915,29), frequency = 52)


fitcomp <- lm(data = comp%>%
                filter(!is.na(Ratio)), formula = comp$Total~  0 + comp$Real)




comp.regre.plot <- ggplot(comp, aes( x = Real, y = Total)) +
  geom_point() +   geom_abline(intercept = 0, 
                               slope=fitcomp$coefficients[1],
                               color='red') +
  labs(title = paste("Porcentaje =", fitcomp$coefficients[1]))



ratio.ts.plot<- ggplotly(ggseasonplot(ratio.ts) + 
                           geom_hline(yintercept =  summary(fitcomp)$coefficients[1,"Estimate"],
                                      alpha = .75, colour = "grey"))




#comp.regre.plot <- ggplotly(ggplot(comp[!is.na(comp$Ratio),], 
#                                  aes(Semana, Ratio)) + 
#                             geom_point(aes(colour = Year)) + 
#                            geom_line(aes(colour = Year)))


#precios usda



fruta <- "Blackberries"

fruit <- read.csv(paste0("Precios/",fruta,".csv"))
fruit$reportDate <- as.Date(fruit$reportDate,format="%Y-%m-%d")
fruit <- fruit[,-c(1)]
conv <- fruit[!fruit$organic=="Organic",] #organica%>%
conv <- conv[conv$packageDesc=="flats 12 6-oz cups with lids",]
#conv <- conv[grep("MEXICO",conv$cityName),] #mexicana
#View(tail(conv[with(conv,order(reportDate)),c("reportDate","lowPriceMin","highPriceMax","mostlyLowMin","mostlyHighMax")]))
conv$precio <- (as.numeric(conv$highPriceMax)+as.numeric(conv$lowPriceMin))/2 #preciomedio
conv <- conv[conv$packageDesc=="flats 12 6-oz cups with lids",]
conv$origen <-"otro"
procedencia <- data.frame(cbind(c("MEXICO","CHILE","PERU","ARGENTINA","URUGUAY","CALIFORNIA","FLORIDA","MICHIGAN","GEORGIA","CAROLINA","OREGON","COLUMBIA","MEXICO"),c("MEXICO","CHILE","PERU","ARGENTINA","URUGUAY","USA","USA","USA","USA","USA","USA","USA","MEXICO")))
names(procedencia) <- c("origen","pais")
#tryCatch({
#  for(var in procedencia$origen){
#    conv[grep(var,conv$cityName),]$origen <- var
 # }
#},error=function(e){})
#conv <- merge(conv,procedencia)
conv <- conv[!is.na(conv$precio),]
#conv<-conv[conv$pais=="MEXICO",]
#data<-conv[conv$pais=="MEXICO",]
#ggplot(data,aes(data$reportDate,data$precio))+geom_point()

conv <- conv[,c("reportDate","precio")]
conv <- unique(conv)
conv <- conv[!is.na(conv$precio),]
prom <- ddply(conv,.(reportDate),summarize,preciomedio=mean(precio))
dias <-  as.Date(c(julian(min(prom$reportDate)):julian(max(prom$reportDate))))
dias <- as.data.frame(dias)
names(dias) <- c("reportDate")
ts <- merge(dias,prom,all.x = TRUE)
ts <- as.xts(x=ts$preciomedio,order.by = ts$reportDate)
ts <- na.approx(ts)
rs <- as.data.frame(cbind(as.Date(index(ts)),coredata(ts)))
names(rs) <- c("Fecha", "Precio")
rs$Fecha <- as.Date(rs$Fecha, origin = "1970-01-01")
rs$Semana <-  format(rs$Fecha, format = "%U")
rs$Year <- format(rs$Fecha, format = "%Y")
rs$Year <- as.integer(rs$Year)
rs$Semana <- ifelse(rs$Semana == "53", "52", rs$Semana)
rs$Semana <- ifelse(rs$Semana == "00", rs$Year - 1 , rs$Semana)
rs$Semana <- ifelse(rs$Semana == "00", rs$Semana == 52, rs$Semana)

#diario

prec.ts <- ts(rs$Precio, c(2010,113), frequency = 365.25) 
prec.ts.plot <- ggplotly(ggseasonplot(prec.ts))


#semanal

semprec <- ddply(rs,.(Year, Semana), summarize, Promedio = mean(Precio))
semprec$Semana <- as.integer(semprec$Semana)
semprec$Semanats <- semprec$Semana + (semprec$Year - 2010)*52
#semprec <- filter(semprec, Semanats > 28) 

prec.sem.ts <- ts(semprec$Promedio, c(2010, 16),frequency = 52)
prec.sem.ts.plot <- ggplotly(ggseasonplot(prec.ts))

# COntado, Financiados y Huertas Propias

temp20172018 <- registros[registros$Fecha > as.Date("2017-09-01"),]
prod20172018 <- unique(temp20172018$Clave) 

financeado <- data.frame(Clave = c(21,30,33,47,51,55,54,80,93,102,112,113,114,126,135,152,
                                   153,156,158,161,184,188,192,194,206,218,221,230,232,236,
                                   251,252,255,230,232,236,251,252,255,258,266,291,293,296,297,
                                   312,315,319,321,324,327, 375,388,389,391,395,397,405,414,
                                   429,433,440,445,448,450,453,454,455,456,457,459,460,461,
                                   476,479,480,485,486,495,541,548,559,569,316, 345, 347, 349,
                                   350, 358, 366, 376, 496, 525, 529, 537), Tipo = c("FINANCEADO"))


contado <- data.frame(Clave = c(24,84,270,292,343,354,355,356,357,359,360,362,363,365,
                                367,368,369,370,371,372,379,392,393,394,396,412,413,416,
                                417,418,419,426,430,432,434,435,436,437,438,439,441,442,
                                443,458,488,489,491,492,493,497,498,499,501,502,507,508,509,
                                512,513,514,515,516,518,521,522,526,527,532,534,535,536,539,
                                540,542,543,545,546,547,549,550,552,553,554,555,565,570,573,
                                574,576,577,578,579,581,584,585,586,587,588,589,590,594,595,
                                596,599,600,601,602,607,608,609,610,611,612,613,614,139), Tipo = "CONTADO")


alpasa <- data.frame(Clave = c(19,303,449,322,225,168,295,70,6,
                               179,181,503,504,505,506,328,116), Tipo = "GRUPO ALPASA")
confin <- union(union(contado$Clave, financeado$Clave), alpasa$Clave)

sinclasificar <- data.frame(Clave = setdiff(prod20172018, confin),Tipo = c("SIN CLASIFICAR"))


productores <- rbind(contado, financeado, sinclasificar,alpasa)%>%
  mutate(Tipo = factor(Tipo, 
                       levels = c("SIN CLASIFICAR", "GRUPO ALPASA", "FINANCEADO", "CONTADO")),
         ordered = TRUE)

cajasprod <- ddply(temp20172018,. (Clave), summarize, Total = sum(Cajas))%>%
  merge(productores, all.x = TRUE)

cajasprod <- cajasprod[!is.na(cajasprod$Tipo),]

porcconta <- ddply(cajasprod,.(Tipo), summarize, Total = sum(Total))%>%
  mutate(Porcentaje = Total/sum(Total))

#dev.new()  

# pie chart

pie.plot <- ggplot(porcconta, aes(x = "", y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity") +
  coord_polar("y") + 
  geom_text(aes(y = Porcentaje/3 + c(0, cumsum(Porcentaje)[-length(Porcentaje)])), 
            label = percent(porcconta$Porcentaje), size = 5)


#money por tipo

moneyprod <- ddply(temp20172018,. (Clave), summarize, Total = sum(Aceptadas*Precio))%>%
  merge(productores, all.x = TRUE)

moneyprod <- moneyprod[!is.na(moneyprod$Tipo),]

porcmoney <- ddply(moneyprod,.(Tipo), summarize, Total = sum(Total))%>%
  mutate(Porcentaje = Total/sum(Total))

#dev.new()  

# pie chart

piemoney.plot <- ggplot(porcmoney, aes(x = "", y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity") +
  coord_polar("y") + 
  geom_text(aes(y = Porcentaje/3 + c(0, cumsum(Porcentaje)[-length(Porcentaje)])), 
            label = percent(porcmoney$Porcentaje), size = 5)

#cajas por semana separadas por TIPO


cajasporsemana <- merge(productores,temp20172018, all.x= TRUE)%>%
  ddply(.(Tipo,Semana), summarize, Total = sum(Cajas))



cajasporsemana <- cajasporsemana[!is.na(cajasporsemana$Total),]%>%
  mutate(Semanatemp =  (as.integer(Semana) + 18)%%52,
         Semana = as.integer(Semana))%>%
  arrange(Semanatemp)%>%
  merge(merge(data.frame(Tipo = unique(productores$Tipo)), data.frame(Semanatemp = c(1:52))),
                           all.y = TRUE)

cajasporsemana[is.na(cajasporsemana$Total),]$Total <- 0

cajas.semana.plot <- ggplotly(ggplot(cajasporsemana, 
                                     aes(x =  factor(Semanatemp, 
                                                     levels = c(1:52), 
                                                     labels = (c(35 : 86)%%52),
                                                     ordered = TRUE)
                                         , y = Total, colour = Tipo, fill = Tipo)) + 
                                geom_col(alpha = .5) + 
                                geom_smooth(data = cajasporsemana,
                                            alpha = 1, se = FALSE,
                                            aes(x = cajasporsemana$Semanatemp,
                                                y = cajasporsemana$Total)) +
                                labs(x = "SEMANA", title = "Cajas por semana"))
# PORCENTAJES


cajasporsemana <- cajasporsemana %>%
  group_by(Semana)%>%
  mutate(Porcentaje = Total/sum(Total))%>%
  ungroup()%>%
  group_by(Tipo)%>%
  arrange(Semanatemp)%>%
  mutate(Acumulado = cumsum(Total))%>%
  ungroup()%>%
  as.data.frame() 

#plot porcenaje de cajas
porcentajescajas.plot<- ggplot(cajasporsemana, 
                               aes(x =  factor(Semanatemp, 
                                               levels = c(1:52), 
                                               labels = (c(35 : 86)%%52),
                                               ordered = TRUE)
                                   , y = Porcentaje, colour = Tipo, fill = Tipo)) + 
  geom_col(alpha = .5) + 
  geom_smooth(data = cajasporsemana,
              alpha = 1, se = FALSE,
              aes(x = cajasporsemana$Semanatemp,
                  y = cajasporsemana$Porcentaje)) +
  labs(x = "SEMANA", title = "Cajas por semana")

# Cajas Acumuladas

cajas.acumuladas.plot <- ggplotly(ggplot(cajasporsemana, 
                                         aes(x =  factor(Semanatemp, 
                                                         levels = c(1:52), 
                                                         labels = (c(35 : 86)%%52),
                                                         ordered = TRUE), y = Acumulado, colour = Tipo, fill = Tipo)) + 
                                    geom_col(data = cajasporsemana[cajasporsemana$Tipo == "CONTADO",],  alpha = .5) + 
                                    geom_col(data = cajasporsemana[cajasporsemana$Tipo == "FINANCEADO",],  alpha = .5) + 
                                    geom_col(data = cajasporsemana[cajasporsemana$Tipo == "SIN CLASIFICAR",],  alpha = .5) +
                                    geom_col(data = cajasporsemana[cajasporsemana$Tipo == "GRUPO ALPASA",],  alpha = .5))





#money por semana separadas por TIPO


moneyporsemana <- merge(productores,temp20172018, all.x= TRUE)%>%
  ddply(.(Tipo,Semana), summarize, Total = sum(Aceptadas*Precio))



moneyporsemana <- moneyporsemana[!is.na(moneyporsemana$Total),]%>%
  mutate(Semanatemp =  (as.integer(Semana) + 18)%%52,
         Semana = as.integer(Semana))%>%
  arrange(Semanatemp)%>%
  merge(merge(data.frame(Tipo = unique(productores$Tipo)), data.frame(Semanatemp = c(1:52))),
        all.y = TRUE)

moneyporsemana[is.na(moneyporsemana$Total),]$Total <- 0

money.semana.plot <- ggplot(moneyporsemana, 
                            aes(x =  factor(Semanatemp, 
                                            levels = c(1:52), 
                                            labels = (c(35 : 86)%%52),
                                            ordered = TRUE)
                                , y = Total, colour = Tipo, fill = Tipo)) + 
  geom_col(alpha = .5) + 
  geom_smooth(data = moneyporsemana,
              alpha = 1, se = FALSE,
              aes(x = moneyporsemana$Semanatemp,
                  y = moneyporsemana$Total)) +
  labs(x = "SEMANA", title = "Dinero por semana")
# PORCENTAJES


moneyporsemana <- moneyporsemana %>%
  group_by(Semana)%>%
  mutate(Porcentaje = Total/sum(Total))%>%
  ungroup()%>%
  group_by(Tipo)%>%
  arrange(Semanatemp)%>%
  mutate(Acumulado = cumsum(Total))%>%
  ungroup()%>%
  as.data.frame() 

#plot porcenaje de cajas
porcentajesmoney.plot<- ggplot(moneyporsemana, 
                               aes(x =  factor(Semanatemp, 
                                               levels = c(1:52), 
                                               labels = (c(35 : 86)%%52),
                                               ordered = TRUE)
                                   , y = Porcentaje, colour = Tipo, fill = Tipo)) + 
  geom_col(alpha = .5) + 
  geom_smooth(data = moneyporsemana,
              alpha = 1, se = FALSE,
              aes(x = moneyporsemana$Semanatemp,
                  y = moneyporsemana$Porcentaje)) +
  labs(x = "SEMANA", title = "Porcentaje Dinero por semana")

# Cajas Acumuladas

money.acumuladas.plot <- ggplotly(ggplot(moneyporsemana, 
                                         aes(x =  factor(Semanatemp, 
                                                         levels = c(1:52), 
                                                         labels = (c(35 : 86)%%52),
                                                         ordered = TRUE)
                                             , y = Acumulado, colour = Tipo, fill = Tipo)) + 
                                    geom_col(data = moneyporsemana[moneyporsemana$Tipo == "CONTADO",],  alpha = .5) +
                                    geom_col(data = moneyporsemana[moneyporsemana$Tipo == "FINANCEADO",],  alpha = .5) +
                                    geom_col(data = moneyporsemana[moneyporsemana$Tipo == "GRUPO ALPASA",],  alpha = .5) +
                                    geom_col(data = moneyporsemana[moneyporsemana$Tipo == "SIN CLASIFICAR",],  alpha = .5))

