## Ejemplo más complejo de personas

library(dplyr)
set.seed(6)
total <- 12

apellidos <-  c('Gonzalez','Solano','Vargas','Solis','Jimenez','Sanchez','Gamboa', 'Chinchilla','Campos','Arias','Salazar','Blanco')
personas <- data.frame(Nombre = c('Pablo','Ana',
                      'Fernando','Maria','Jocelyn',
                      'Andrea','Monica','Magdalena',
                      'Javier','Jorge','Ernesto',
                      'Pedro'),
           Antiguedad = rpois(n = total,lambda =3) ,
           Sexo = c('M','F','M',rep('F',5),rep('M',4)),
           Provincia = sample(x = c('San Jose','Cartago',
                         'Limon','Heredia','Alajuela','Puntarenas','Guanacaste'),size = total,replace = TRUE),
           Asegurado = sample(x = c(FALSE,TRUE),size = total,replace = TRUE),
           Casado = sample(x = c(FALSE,TRUE),size = total,replace = TRUE),
           stringsAsFactors = FALSE) %>%
  mutate(Edad = Antiguedad + 18 + rpois(n=total,lambda = 3)) %>%
  mutate(Apellido = apellidos)

rm(apellidos,total)


## Carreras
# c('Farmacia','Actuariales','Nutricion','Farmacia','Actuariales','Ingenieria','Actuariales','Economia','Economia','Economia','Derecho','Ingenieria')










