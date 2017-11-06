library(lubridate)
library(dplyr)
library(tidyr)
set.seed(2345)


Naci <- seq(as.Date('1900/01/01'), as.Date('1905/12/31'), by="day")
Muerte <- seq(as.Date('1946/01/01'), as.Date('1990/12/31'), by="day")
prob_muerte <- rev(cumsum(rep(1,16436)))
Nacimientos <- data.frame(ID = 1:31243) %>% 
  rowwise() %>% 
  mutate(Fecha = as.character(sample(Naci, 1,prob = c(rep(1,800),200,rep(2,1390))))) %>% 
  mutate(Hora_Nac = sample(24,size = 1,replace = FALSE)-1) %>% 
  mutate(Fecha_Muerte = as.character(sample(Muerte, size = 1,prob = prob_muerte)),
         Hora_Muerte = sample(24,size = 1,replace =FALSE) - 1,
         Minuto_Muerte = sample(60,size = 1,replace = FALSE)-1) %>% 
  ungroup() %>% 
  separate(col = Fecha_Muerte,into = c('Anno_Muerte','Mes_Muerte','Dia_Muerte'),sep = '-')

  
  
Hijos_naci <- seq(as.Date('1926/01/01'), as.Date('1935/12/31'), by="day")
pesos_hijo <- dbinom(prob = 1/2,x = 1:3652,size = 3652)
Hijos <- Nacimientos %>% 
  sample_n(size = 50841,replace = TRUE) %>% 
  select(ID) %>% 
  mutate(Nacimiento_Hijo = sample(Hijos_naci,size = 50841,prob = pesos_hijo,replace = TRUE)) %>% 
  arrange(ID) %>% 
  mutate(Nacimiento_Hijo = as.character(Nacimiento_Hijo))




Fallecimientos <- Nacimientos %>% 
  select(ID,Anno_Muerte,Mes_Muerte,Dia_Muerte,Hora_Muerte,Minuto_Muerte)


Nacimientos <- Nacimientos %>% 
  select(ID,Fecha,Hora_Nac)


save(Fallecimientos,Nacimientos,Hijos,file = 'C:/Users/User/Dropbox/Clases 2017-2/Presentaciones_CA0202/Clase_Fechas/Fechas.RData')
