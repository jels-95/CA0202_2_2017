library(randomNames)
library(dplyr)
library(evd)

set.seed(50)
total<- 100

datosSalario<- data.frame(Genero = sample(c('male','female'),total,replace = TRUE),
                          Antiguedad = rpois(total,15),
                          Provincia = sample(7,total,replace = TRUE),
                          Moneda = sample(c('CRC','USD'),total,replace = TRUE),
                          stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(Nombre = randomNames(gender = Genero,ethnicity = 'Hispanic')) %>%
  mutate(Edad = Antiguedad + 18 + rpois(n=1,5)) %>% 
  mutate(Casado = rbinom(n =1,size = 1,prob = 0.3)) %>%
  mutate(Annos_Casado = floor(Antiguedad/2)-2) %>% 
  mutate(Hijos = rbinom(n = 1,size = 1,prob = 0.29 + Casado/3)) %>%
  mutate(Regimen = sample(c('IVM', 'Magisterio','PJ'),size = 1,prob = c(0.6,0.3,0.1))) %>%
  ungroup() %>% 
  rowwise() %>%
  mutate(Cantidad_Hijos = ifelse(Hijos == 1,
                                 rbinom(size = 5,prob = 0.7,n = 1),
                                 rbinom(size = 1,prob =0.01,n=1))) %>%
  ungroup()

annos<- expand.grid(Nombre = datosSalario$Nombre,
                    Anno = 2018:2022,
                    Mes = 1:12,
                    stringsAsFactors = FALSE)

datosGastos<- datosSalario %>% 
  left_join(annos) %>%
  rowwise() %>%
  mutate(Gasto = rfrechet(1,scale = 750,loc = 1000000,shape = 7/8)) %>%
  ungroup() %>% 
  select(Anno,Mes,Nombre,Gasto)
rm(annos,total)


write.csv2(datosGastos,'C:/Users/User/Dropbox/Clases 2017-2/Presentaciones_CA0202/Clase_union_dataframes/Ejemplo_gastos.csv',row.names = FALSE)













