




library(lubridate)
library(dplyr)
set.seed(20171106)
fechas <- seq(as.Date('2020/01/01'), as.Date('2025/12/31'), by="day")


# montos <- rnorm(n = 100,mean = c(1500000,1250000),sd = c(1,0.5,0.5,1))

library(mvtnorm)

montos <- as.data.frame(rmvnorm(n = 100,mean = c(1500000,1450000),sigma = matrix(c(5000000,-3500000,-3500000,5000000),nrow = 2)))

names(montos) <- c('Monto_Adeudado','Monto_Remate')


Remates <- data_frame(ID_Remate = 1:100) %>% 
  mutate(Fecha_Fallo_prestamo = sample(size = 100,x = fechas,replace = FALSE)) %>% 
  mutate(Fecha_Juicio = Fecha_Fallo_prestamo + dweeks(x = rpois(n = 100,lambda = 15)) + ddays(rbinom(n = 100,size = 14,prob = 1/3))) %>% 
  mutate(Fecha_Remate = Fecha_Fallo_prestamo + dweeks(x = rpois(n = 100,lambda = 20)) + ddays(rbinom(n = 100,size = 14,prob = 1/3))) %>% 
  bind_cols(montos) %>% 
  mutate_at(vars(starts_with('Fecha')),as.character)


save(Remates,file = 'C:/Users/User/Dropbox/Clases 2017-2/Presentaciones_CA0202/Clase_Fechas/Remates.RData')
