

library(tidyr)
library(dplyr)


data(EuStockMarkets,package = 'datasets')

t1 <- time(EuStockMarkets)
datos1 <- data.frame(EuStockMarkets) %>% 
  mutate(Tiempo = t1) %>% 
  mutate(Anno = floor(Tiempo)) %>% 
  gather(DAX:FTSE,key = Accion,value = Monto) %>% 
  group_by(Accion,Anno) %>% 
  mutate(Mayor = Monto > lag(Monto)) %>% 
  arrange(Tiempo) %>% 
  summarise(Veces = sum(Mayor,na.rm = TRUE),
            Porcentual = (last(Monto) - first(Monto))/first(Monto)) %>% 
  ungroup()

## Veces indica la cantidad de veces que crece en un año, 
## Porcentual indica el crecimientto porcentual que tiene 


datos2 <- datos1 %>% 
  gather(Veces:Porcentual,key  = tipo,value = Valor) %>% 
  arrange(Accion,Anno)
  

datos3 <- datos1 %>% 
  mutate(Veces_Porcentual = paste(Veces,sep = '/',Porcentual)) %>% 
  select(-Veces,-Porcentual)



# Veces
datos4a <- datos1 %>% 
  select(Accion,Anno,Veces) %>% 
  mutate(Anno = as.character(Anno)) %>% 
  spread(Anno,Veces)


# Porcentual
datos4b <- datos1 %>% 
  select(Accion,Anno,Porcentual) %>% 
  mutate(Anno = as.character(Anno)) %>% 
  spread(Anno,Porcentual)



datos5 <- datos1 %>% 
  mutate(Anno = as.character(Anno)) %>% 
  mutate(Siglo = substring(Anno,0,2),
         Anno = substring(Anno,3,4)) %>% 
  select(Accion,Siglo,everything())


rm(EuStockMarkets,t1)
save.image(file = 'C:/Users/User/Dropbox/Clases 2017-2/Presentaciones_CA0202/Ordenar_Dataframes/Datos_dataframes.RData')


library(randomNames)
library(dplyr)

set.seed(45)
total<- 5

datosSalario<- data.frame(Genero = sample(c('male','female'),total,replace = TRUE),
                          #Antiguedad = rpois(total,15),
                          #Provincia = sample(7,total,replace = TRUE),
                          #Moneda = sample(c('CRC','USD'),total,replace = TRUE),
                          stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(Nombre = randomNames(gender = Genero,ethnicity = 'Hispanic')) %>%
  #mutate(Edad = Antiguedad + 18 + rpois(n=1,5)) %>% 
  #mutate(Casado = rbinom(n =1,size = 1,prob = 0.3)) %>%
  ##mutate(Hijos = rbinom(n = 1,size = 1,prob = 0.27)) %>%
  #mutate(Regimen = sample(c('IVM', 'Magisterio','PJ'),size = 1,prob = c(0.6,0.3,0.1))) %>%
  #ungroup() %>% 
  #rowwise() %>%
  #mutate(Cantidad_Hijos = ifelse(Hijos == 1,rbinom(size = 5,prob = 0.5,n = 1),0)) %>%
  ungroup()

annos<- expand.grid(Nombre = datosSalario$Nombre,
                    Anno = 2018:2020,
                    Mes = 1:12,
                    stringsAsFactors = FALSE)

datosSalario<- datosSalario %>% 
  left_join(annos) %>%
  rowwise() %>%
  mutate(Salario = rnorm(n = 1,mean = 1500000,sd = 200000)) %>%
  ungroup() %>%
  group_by(Nombre) %>%
  arrange(Anno) %>%
  #mutate(Antiguedad = Anno - first(Anno) + Antiguedad) %>%
  #ungroup() %>%
  rowwise() %>%
  mutate(Volar = rbinom(n=1,size = 1,prob = 1/7)) %>%
  ungroup() %>%
  filter(Volar <1) %>%
  select(-Volar) %>% 
  filter(!((Nombre == 'Castillo, Omar') & (Anno == 2018) & (Mes == 2)))





save(datosSalario,file = 'C:/Users/User/Dropbox/Clases 2017-2/Presentaciones_CA0202/Ordenar_Dataframes/Datos_Salarios.RData')


