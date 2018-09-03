prestamos <- entregas.material(Campos = c( "Tipo_salida", "Clave_productor", "Fecha", "Cantidad",
                                      "Clave_material", "Tipo_receptor" ))%>%
  filter(Tipo_salida == "PRESTAMO", Tipo_receptor == "PRODUCTOR")%>%
  ddply(.(Fecha, Clave_material, Clave_productor), summarize, Total = sum(Cantidad))%>%
  get.material(Campos = c("Presentacion", "Tipo_material"))%>%
  filter(Tipo_material %in% c("CAJA EMP.", "CLAMSHELL"))

  
prueba <-  rbind(entradas_fruta%>%
        select(Fecha, Clave_productor, Presentacion,Total)%>%
        filter(Fecha >= inicio_temporada)%>%
        mutate(Tipo_material = "CAJA EMP.", Tipo = "CON FRUTA", Total = - Total),   #Cajas con fruta
        entradas_fruta%>%
        get.productos(Campos = c( "Peso", "Unidad", "Clams"))%>%
        mutate(Total = Total*Clams,
               Presentacion = paste0(Peso,Unidad))%>%
        select(Fecha, Clave_productor, Presentacion, Total)%>%
        mutate(Tipo_material = "CLAMSHELL", Tipo = "CON FRUTA", Total = -Total),    #clams con fruta
      
      #prestamos
        entregas.material(Campos = c( "Tipo_salida", "Clave_productor", "Fecha", "Cantidad",
                                    "Clave_material", "Tipo_receptor" ))%>%
        filter(Tipo_salida == "PRESTAMO", Tipo_receptor == "PRODUCTOR")%>%
        ddply(.(Fecha, Clave_material, Clave_productor), summarize, Total = sum(Cantidad))%>%
        get.material(Campos = c("Presentacion", "Tipo_material")) %>%
        mutate(Tipo = "PRESTAMO")%>%
        select(Fecha, Clave_productor, Presentacion, Total, Tipo_material, Tipo),
      
              #devoluciones
      
      entradas_material%>%
        filter(Tipo_entrada == "DEVOLUCION", Tipo_proveedor == "PRODUCTOR")%>%
        rename(Tipo = Tipo_entrada, Clave_productor = Clave_proveedor)%>%
        get.material(Campos = c("Presentacion", "Tipo_material"))%>%
        ddply(.(Fecha, Clave_productor, Presentacion, Tipo_material, Tipo), 
              summarize, Total = sum(Cantidad))%>%
        select(Fecha, Clave_productor, Presentacion, Total, Tipo_material, Tipo))
      
  
  
  
prueba <- rbind(entradas_fruta%>%
                  select(Fecha, Clave_productor, Presentacion,Total)%>%
                  filter(Fecha >= inicio_temporada)%>%
                  mutate(Tipo_material = "CAJA EMP.", Tipo = "CON FRUTA", Total = - Total),   #Cajas con fruta
                entradas_fruta%>%
                  get.productos(Campos = c( "Peso", "Unidad", "Clams"))%>%
                  mutate(Total = Total*Clams,
                         Presentacion = paste0(Peso,Unidad))%>%
                  select(Fecha, Clave_productor, Presentacion, Total)%>%
                  mutate(Tipo_material = "CLAMSHELL", Tipo = "CON FRUTA", Total = -Total),    #clams con fruta
                
                #prestamos
                entregas.material(Campos = c( "Tipo_salida", "Clave_productor", 
                                              "Fecha", "Cantidad", "Clave_material",
                                              "Tipo_receptor" ))%>%
                  filter(Tipo_salida == "PRESTAMO", Tipo_receptor == "PRODUCTOR")%>%
                  ddply(.(Fecha, Clave_material, Clave_productor), summarize, 
                        Total = sum(Cantidad))%>%
                  get.material(Campos = c("Presentacion", "Tipo_material")) %>%
                  mutate(Tipo = "PRESTAMO")%>%
                  select(Fecha, Clave_productor, Presentacion, Total, Tipo_material, Tipo),
                
                #devoluciones
                
                entradas_material%>%
                  filter(Tipo_entrada == "DEVOLUCION", Tipo_proveedor == "PRODUCTOR")%>%
                  rename(Tipo = Tipo_entrada, Clave_productor = Clave_proveedor)%>%
                  get.material(Campos = c("Presentacion", "Tipo_material"))%>%
                  ddply(.(Fecha, Clave_productor, Presentacion, Tipo_material, Tipo), 
                        summarize, Total = sum(Cantidad))%>%
                  select(Fecha, Clave_productor, Presentacion, Total, Tipo_material, Tipo))%>%
  
  
  filter(Clave_productor == str_match(input$Clave_productor2, "^[[0-9]]{0,3}")[,1])%>%
  rename(Concepto = Tipo)%>%
  ddply(.(Fecha, Clave_productor, Concepto,Tipo_material, Presentacion), summarize, 
        Total = sum(Total))


                    