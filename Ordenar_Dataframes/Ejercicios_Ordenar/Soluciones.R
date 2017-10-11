library(readr)
library(tidyverse)


####

Life_tables_world <- read_csv("C:/Users/User/Dropbox/Clases 2017-2/Presentaciones_CA0202/Ordenar_Dataframes/Ejercicios_Ordenar/Life_tables_world.csv")

Life_tables_world <- Life_tables_world %>% 
  gather(key = Anno,value = Valor,`2015`:`2000_1`) %>% 
  mutate(Sexo = ifelse(Valor %in% c('Male','Female'),Valor, NA)) %>% 
  fill(Sexo,.direction = c('down'))

names(Life_tables_world)[1:2] <- c('Indicador','Grupo_Edad')

Life_tables_world <- Life_tables_world %>% 
  filter(!(Valor %in% c('Male','Female'))) %>% 
  separate(col = Indicador,into = c('Indicador','Descripcion'), sep = ' - ') %>% 
  select(-Descripcion) %>% 
  spread(key = Indicador,value = Valor) %>% 
  mutate(Anno = substring(Anno,first = 1,last = 4)) %>% 
  mutate(Anno = as.numeric(Anno)) %>% 
  mutate(Grupo_Edad = ifelse(Grupo_Edad == '&lt;1 year','0-1 years',Grupo_Edad)) %>% 
  mutate_at(vars(ex:Tx),as.numeric)

####
library(readr)
Population_growth <- read_delim("C:/Users/User/Dropbox/Clases 2017-2/Presentaciones_CA0202/Ordenar_Dataframes/Ejercicios_Ordenar/Population_growth.csv", 
                                "\t", escape_double = FALSE, trim_ws = TRUE, 
                                skip = 4)

Population_growth %>%
  gather(key = Anno,value = Porcentaje,`1960`:`2016`) %>% 
  select(-c(`Country Code`:`Indicator Code`)) %>% 
  rename(Country_name = `Country Name`)

#####
library(readxl)
Tasa_Basica_pasiva <- read_excel("C:/Users/User/Dropbox/Clases 2017-2/Presentaciones_CA0202/Ordenar_Dataframes/Ejercicios_Ordenar/Tasa_Basica_pasiva.xlsx")

Tasa_Basica_pasiva %>% 
  gather(key = Anno,value = Tasa,`2006`:`2017`) %>% 
  separate(col = X__1,into = c('Dia','Mes'),sep = ' ') %>%
  mutate(Tasa = as.numeric(sub(pattern= ',',replacement = '.',x = Tasa))) %>% 
  mutate(Anno = as.numeric(Anno))







