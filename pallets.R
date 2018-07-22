source("funciones.R")

hpallet <- myfetch("tbPallets")%>%
  filter(strCan_cel == "NO")

names(hpallet) <- paste0("h", names(hpallet))

bpallet <- myfetch("tbPalletsReg")%>%
  filter(strCan_cel == "NO")

names(bpallet) <- paste0("b", names(bpallet))

pallets <- merge(hpallet, bpallet, by.x = "hintNum_reg", by.y = "bintNum_reg", all.x= TRUE)

rm(hpallet, bpallet)
