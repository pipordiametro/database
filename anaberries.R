
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
