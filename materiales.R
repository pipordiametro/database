source("funciones.R")
library(ggplot2)
library(plotly)

#list.files("proyecto/")


#---------------ENTRADAS---------------------
hentradas <- myfetch("tbAlmEnt")
bentradas <- myfetch("tbAlmEntReg")
names(hentradas) <- paste0("h",names(hentradas))
names(bentradas) <- paste0("b",names(bentradas))
entradas <- merge(hentradas,bentradas, 
                  by.x = "hintNum_reg", by.y= "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  select(hfecCre_aci, bintCla_mat, bintCan_tid)

#----------------------SALIDADAS---------------


hsalidas <- myfetch("tbAlmSal")
bsalidas <- myfetch("tbAlmsalReg")
names(hsalidas) = paste0("h",names(hsalidas))
names(bsalidas) = paste0("b",names(bsalidas))
salidas <- merge(hsalidas,bsalidas, by.x = "hintNum_reg", 
                 by.y = "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  select(hfecCre_aci, bintCla_mat, bintCan_tid)%>%
  mutate(bintCan_tid = -bintCan_tid)

materiales <- myfetch("tbMateriales")


#-------------fruta----------------
hfruta <- myfetch("tbRecFruEmp")
bfruta <- myfetch("tbRecFruEmpReg")
names(hfruta) <- paste0("h",names(hfruta))
names(bfruta) <- paste0("b",names(bfruta))
productos <- myfetch("tbProductos")%>%
###pendiente  select(strCla_prod, )

fruta <- merge(hfruta, bfruta, 
      by.x = "hintNum_reg", by.y =  "bintNum_reg")%>%
  filter(bstrCan_cel == "NO")%>%
  transmute(Fecha = as.Date(bfecCre_aci),
            intCla_prod = bintCla_prod,
            Recibidas = bintCan_tid,
            Rechazadas = bintCan_rec)%>%
  merge()

rm(hfruta,bfruta)


#-----------------ANALISIS---------------------

datos <- rbind(entradas,salidas)%>%
  transmute(Fecha = as.Date(hfecCre_aci),
             Clave = as.factor(bintCla_mat),
             Cantidad = as.integer(bintCan_tid))%>%
  filter(Fecha >= as.Date("2015-08-14"))

flujos <- ddply(datos,.(Fecha,Clave), 
                summarize, Total =sum(Cantidad))


dias.plot <-  ggplotly(ggplot(flujos, aes(x = Fecha, y = Total, colour = Clave)) +
  geom_line())

flujos.cum <- flujos%>%
  arrange(Fecha)%>%
  group_by(Clave)%>%
  mutate(Acumulado = cumsum(Total))%>%
  ungroup()

acumulado.plot <-  ggplotly(ggplot(flujos.cum, aes(x = Fecha, y = Acumulado, colour = Clave)) +
                         geom_line())
  
hentradas[hentradas$hstrTip_ent == "INV. INICIAL",]
