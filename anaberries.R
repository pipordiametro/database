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




# -----------  ANABERRIES
anabmelt <- melt(anab, c("Year", "Semana"), value.name = "Factor")%>%
  filter(!variable == "Semanats")

#CONSTRUCCION DE LA SERIE TEMPORAL

anab.ts <- ts(anab$Real, c(2015,30), frequency = 52)
#PLOTEO DE LA SERIE

#SEASONAL PLOT  
anab.seasonal.plot<- ggplotly(ggseasonplot(anab.ts) + labs(title ="Enviado Reale Anaberries"))
#DECOMPOSE
anab.decomp <- decompose(anab.ts, "additive")
anab.seasonal.plot <- ggplotly(ggseasonplot(anab.decomp$seasonal))

#ACUMULATIVO
anab.cum <- anab%>%
  group_by(Year,Semana,Temporada)%>%
  summarize(Real = sum(Real))%>%
  ungroup()%>%
  mutate(Semanats = Semana +(Year - 2011)*52)%>%
  arrange(Semanats)%>%
  group_by(Temporada)%>%
  mutate(Acumulado_real = cumsum(Real))%>%
  ungroup()

anab.cum.plot <- ggplot(anab.cum, 
                        aes(x= factor(Semana, levels =  c(37:88)%%52 + 1) , 
                            y =Acumulado_real, colour = Temporada)) +
  geom_point()






# -------------------- COMPARATIVO ENTRE CRUZA REAL USDA Y PRODUCCION ALPASA


comp <- merge(anab[,c("Year", "Semanats","Real")],
              alp, by.x = c("Year", "Semanats"), 
              by.y = c("Year", "Semanats"),all.x = TRUE,
              all.y = TRUE)%>%
  select(Semanats, Real, Total, Year)%>%
  filter(Total < Real)%>%
  merge(semanas, all.y = TRUE)%>%
  mutate(Ratio = Total/Real)

comp[is.na(comp)] <- 0

#SERIE TEMPORAL


ratio.ts <- ts(comp$Ratio, c(2013,34), frequency = 52)
decompose(ratio.ts)

fitcomp <- lm(data = comp, formula = comp$Total~comp$Real-1 )


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       " Slope =",signif(fit$coef[[1]], 5)))
}

#Ploteo
comp.regre.plot <- ggplotRegression(fitcomp)



ratio.ts.plot<- ggplotly(ggseasonplot(ratio.ts) + 
                           geom_hline(yintercept =  summary(fitcomp)$coefficients[1,"Estimate"],
                                      alpha = .75, colour = "grey"))
