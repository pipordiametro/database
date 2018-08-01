clientes <-  myfetch("tbClientes")%>%
  transmute(Clave_cliente = intCla_cli, Cliente = strNom_bre)
  
destinos <-  myfetch("tbDestinos")%>%
  transmute(Pais = strPai_s, Clave_destino= intCla_des)
