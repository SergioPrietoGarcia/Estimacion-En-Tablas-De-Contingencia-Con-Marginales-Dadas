################################################################################
######## CÓDIGO EMPLEADO EN LA CONSTRUCCIÓN DEL TRABAJO FIN DE MÁSTER ##########
################################################################################




#### ---- OBJETIVO ESPECÍFICO:


# Demostrar que, en el caso de dos elecciones políticas consecutivas, la probabilidad condicional de votar
# por un partido en las segundas elecciones sabiendo lo que se votó en las primeras depende exclusivamente
# de lo que se votó en las primeras



######################################################
## ELECCIONES GENERALES NOVIEMBRE 2019 Y ABRIL 2019 ##
######################################################
library(dplyr)

# Seleccion del estudio 3269 del CIS que recopila el recuerdo de voto en las
# elecciones generales de noviembre de 2019 y el recuerdo de voto en las elecciones
# generales de abril de 2019.

# Cuenta con un tamaño muestral de 4804 individuos.

# El objetivo es contrastar la hipotesis de que la transferencia de votos de unas
# elecciones a otras depende de lo que se haya votado en las primeras y no de la
# zona donde residua el individuo

# Por esto mismo, se selecciona una zona que comparta las mismas opciones politicas
# como puede ser la Comunidad Autonoma de Analucia. Se procedera dividiendo la comunidad
# en dos partes que presenten un tamaño muestral similar y a continuacion se estudiara 
# en cada una de ellas las proporciones de transferencia del voto para posteriormente
# contrastar si estas proporciones son similares.



# -- Cargamos los datos del Estudio 3269 del CIS ya con las variables correspondientes seleccionadas
library(haven)
Estudio3269 <- read_sav("C:/Users/Lenovo/Desktop/Universidad/Master Tecnicas Estadisticas/TFM/Datos Transferencia De Votos/Estudio3269.sav")
head(Estudio3269, n = 300) 

# -- Renombramos las columnas
Estudio3269 <- Estudio3269 %>% 
  rename(Nov2019 = B22R, Abril2019 = RECUERDO)

Estudio3269 # Base de datos con las variables correspondientes
names(Estudio3269)

# -- Filtramos los datos por Comunidad Autonoma (ANDALUCIA) y creamos nuevo objeto con estos datos
VotoAndaluz <- Estudio3269 %>%
  filter(CCAA == 1)

print(VotoAndaluz, n = 15)
sum(is.na(VotoAndaluz)) # No presenta valores perdidos

# -- Observamos cuantos individuos existen por comunidad autonoma y creamos dos grupos (Bloque1 y Bloque2)
unique(VotoAndaluz$PROV) # (4) Almeria, (11) Cadiz, (14) Cordoba, (18) Granada, (21) Huelva, (23) Jaen, (29) Malaga, (41) Sevilla
table(VotoAndaluz$PROV)

# Bloque1: (11) Cadiz, (14) Cordoba, (23) Jaen, (29) Malaga. Total de 358 individuos
# Bloque2: (4) Almeria, (18) Granada, (21) Huelva, (41) Sevilla. Total de 341 individuos

Bloque1 <- VotoAndaluz %>%
  filter(PROV == 11 | PROV == 14| PROV == 23| PROV == 29 , Abril2019 != 77, Nov2019 != 77)

Bloque2 <- VotoAndaluz %>%
  filter(PROV == 4 | PROV == 18| PROV == 21| PROV == 41 , Abril2019 != 77, Nov2019 != 77)

Bloque1;Bloque2
# -- BLOQUE 1. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tablaA1 <- table(Bloque1$Nov2019, Bloque1$Abril2019)
rownames(tablaA1) <- c("PP", "PSOE", "Cs", "Pacma", "Vox", "UP", "Más País", "O.P.", "Blanco", "Abstención", "No recuerda", "N.C")
colnames(tablaA1) <- c("PP", "PSOE", "Cs", "Vox", "UP", "No derecho", "O.P.", "Blanco", "Abstención", "No recuerda", "N.C")
proporcionesA1 <- prop.table(tablaA1, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
round(proporcionesA1,3)

# -- BLOQUE 2. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tablaA2 <- table(Bloque2$Nov2019, Bloque2$Abril2019)
rownames(tablaA2) <- c("PP", "PSOE", "Cs", "Pacma", "Vox", "UP", "Más País", "O.P.", "Blanco", "Abstención", "No recuerda", "N.C")
colnames(tablaA2) <- c("PP", "PSOE", "Cs", "Vox", "UP", "No derecho", "O.P.", "Blanco", "Abstención", "No recuerda", "N.C")
proporcionesA2 <- prop.table(tablaA2, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
round(proporcionesA2,3)

diferenciaA = proporcionesA1 - proporcionesA2
sum2And = sum(diferenciaA^2); sum2And

sort(unique(VotoAndaluz$Nov2019))
sort(unique(VotoAndaluz$Abril2019))
# Una vez construidas ambas tablas de proporciones dividiendo Andalucia(1) en dos partes mas o menos iguales
# vamos a realizar el mismo proceso pero esta vez comparando la tabla de proporciones de Andalucia (1) con la 
# tabla de proporciones del conjunto de comunidades autonomas de Asturias (3), Castilla y Leon (7), Castilla-La Mancha (8) y
# Extremadura (11)

unique(Estudio3269$CCAA)
table(Estudio3269$CCAA)

# En Andalucia se recogen 699 individuos. En la suma de las otras 4 comunidades autonomas se recogen 813 individuos.
# Vamos a realizar el mismo proceso anterior y comparar ambas tablas de proporciones.

# -- Filtro del voto andaluz
VotoAndaluz <- Estudio3269 %>%
  filter(CCAA == 1)
nrow(VotoAndaluz)

# -- Filtro del conjunto de cuatro comunidades autonomas
VotoACCE <- Estudio3269 %>%
  filter(CCAA == 3 | CCAA == 7 | CCAA == 8 | CCAA == 11, Abril2019 != 94) 

# Prefiero eliminar las filas en las que los individuos no tuvieron edad para votar en Noviembre de 2018

nrow(VotoACCE)


# -- BLOQUE ANDALUCIA. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tabla1 <- table(VotoAndaluz$Nov2019, VotoAndaluz$Abril2019)
proporciones <- prop.table(tabla1, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
proporciones

# -- BLOQUE ACCE. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tabla2 <- table(VotoACCE$Nov2019, VotoACCE$Abril2019)
proporciones2 <- prop.table(tabla2, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
proporciones2

# -- Observacion de la diferencia entre una tabla de proporciones y otra
diferencia = proporciones - proporciones2
diferencia

# PROCESO BOOTSTRAP

# Pensamiento metodo bootstrap: como cuento con las proporciones "muestrales" en cada una de las tablas
# puedo emplear el metodo bootstrap para crear nuevas tablas a partir de esas proporciones y ver como 
# se comportan repitiendo el proceso un numero elevado de veces, por ejemplo B = 1000

dif = proporcionesA1 - proporcionesA2
dif
sum(dif^2)

# Para este proceso de empleara la Comunidad Autonoma de Andalucia:

# (1) --- Primer paso. Obtener las proporciones muestrales de los datos para la comunidad de ANDALUCIA en su conjunto

Andalucia <- Estudio3269 %>%
  filter(CCAA == 1, Nov2019 != 77, Abril2019 != 77)

t_andal <- table(Andalucia$Nov2019, Andalucia$Abril2019)
prop_andal <- prop.table(t_andal, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
prop_andal

# --- (2) Segundo paso: Simular las dos tablas fila por fila con tamaños muestrales iguales, fijando las proporciones muestrales
# ---     obtenidas en el paso anterior.

# Valores reales de las tablas
tablaA1
tablaA2

# Tamaño muestral por fila
sum(tablaA1[1,]) # Tamaño muestral de la fila 1 de la tabla 1
sum(tablaA2[1,]) # Tamaño muestral de la fila 1 de la tabla 2

# Proporciones de valores por fila
prop.table(tablaA1[1,]) # Proporciones de valores de la fila 1 de la tabla 1
prop.table(tablaA2[1,]) # Proporciones de valores de la fila 1 de la tabla 1

# Simular valores para dos vectores siguiendo estas proporciones y realizar la diferencia del resultado de sus proporciones

# Tabla 1
p <- as.numeric(prop.table(tablaA1[1,])) # Vector de probabilidades
n <- 1
fila1 <- as.numeric(rmultinom(1, size = sum(tablaA1[1,]), prob = p))
fila1 # Conjunto de valores para la tabla 1

p_fila1 <- as.numeric(prop.table(fila1))
p_fila1 # Conjunto de proporciones de los valores simulados para la tabla 1

# Tabla 2
p <- as.numeric(prop.table(tablaA2[1,])) # Vector de probabilidades
n <- 1
fila2 <- as.numeric(rmultinom(n, size = sum(tablaA1[2,]), prob = p))
fila2 # Conjunto de valores para la tabla 2

p_fila2 <- as.numeric(prop.table(fila2))
p_fila2



# --- (3) Tercer paso: Obtener la diferencia de proporciones cada par de tablas simuladas y realizar una suma de cuadrados para obtener
# ---     un valor

# Diferencia y suma de cuadrados
dif = p_fila1 - p_fila2
sum(dif^2)

# --- (4) Cuarto paso: Repetir este proceso 1000 veces y obtener 1000 valores diferentes.


# PROTOTIPO DE FUNCION PRE-BOOTSTRAP. ¡¡FUNCIONA!!

# Esta funcion calcula la suma de cuadrados de la diferencia entre las proporciones de dos tablas
# Los argumentos son: 

#           - p_base: aqui debe introducirse la matriz de proporciones a partir de las cuales se van a simular
#           las dos tablas

#           - t1: la tabla 1 a comparar de la que se extraen ciertas caracteristicas (tamaño de cada fila)

#           - t2: la tabla 2 a comparar de la que se extraen ciertas caracteristicas (tamaño de cada fila)

difsum2 <- function(p_base, t1, t2){
  
  p_f_t1 = matrix(NA, ncol = ncol(t1), nrow = nrow(t1))
  p_f_t2 = matrix(NA, ncol = ncol(t2), nrow = nrow(t2))
  tab1 = matrix(NA, ncol = ncol(t1), nrow = nrow(t1))
  tab2 = matrix(NA, ncol = ncol(t2), nrow = nrow(t2)) 
  nfil = 1 : nrow(t1)
  n = 1
  for (i in nfil){
    # Obtenemos las proporciones por fila y creamos dos tablas
    filas_t1 <- t(rmultinom(n, size = sum(t1[nfil,]), prob = p_base[nfil,]))
    tab1[nfil,] <- filas_t1
    p_f_t1[nfil,] <- as.matrix(as.matrix(prop.table(tab1[nfil,], margin = 1))) # Matriz de proporciones para la tabla 1
    filas_t2 <- t(rmultinom(n, size = sum(t2[nfil,]), prob = p_base[nfil,]))
    tab2[nfil,] <- filas_t2
    p_f_t2[nfil,] <- as.matrix(as.matrix(prop.table(tab2[nfil,], margin = 1))) # Matriz de proporciones para la tabla 2
  }
  # Obtenemos la resta de proporciones y su suma de cuadrados
  dif <- p_f_t1 - p_f_t2
  sum2 <- sum(dif^2)
  
  return(list(dif = dif, sum2 = sum2, p_f_t1 = p_f_t1, p_f_t2 = p_f_t2))
}

# Una vez creada la funcion es momento de ponerla a prueba con el proceso bootstrap
# para la comunidad de andalucia por un lado, y para la comparacion entre la comunidad
# de Andalucia y el conjunto de las otras 4 Comunidades Autonomas

# -- BLOQUE 1. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tablaA1 <- table(Bloque1$Nov2019, Bloque1$Abril2019)
proporcionesA1 <- prop.table(tablaA1, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
proporcionesA1

# -- BLOQUE 2. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tablaA2 <- table(Bloque2$Nov2019, Bloque2$Abril2019)
proporcionesA2 <- prop.table(tablaA2, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
proporcionesA2

diferenciaA = proporcionesA1 - proporcionesA2
sum2And = sum(diferenciaA^2)

## PRUEBA PARA LA COMUNIDAD DE ANDALUCIA
t_andal <- table(Andalucia$Nov2019, Andalucia$Abril2019)
prop_andal <- prop.table(t_andal, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
prop_andal

# Empleamos la funcion creada
x <- difsum2(p_base = prop_andal, t1 = tablaA1, t2 = tablaA2)
sum(x$p_f_t1[1,])
x$sum2
x$p_f_t1

# PROCESO BOOTSTRAP
valores = as.numeric()
b = 1:1000
for (i in b){
  valores[i] = difsum2(p_base = prop_andal, t1 = tablaA1, t2 = tablaA2)$sum2
}
hist(valores, xlim = c(0,3), main = " ", xlab = "Suma de cuadrados", ylab ="Frecuencias")
abline(v= sum2And, col = "red", lwd = 2.5)
max(valores)



## PRUEBA PARA LA COMPARACION ENTRE LA COMUNIDAD DE ANDALUCIA Y LAS OTRAS 4

# -- Filtro del conjunto de cuatro comunidades autonomas y Andalucia
Comunidades <- Estudio3269 %>%
  filter(CCAA == 1 | CCAA == 3 | CCAA == 7 | CCAA == 8 | CCAA == 11, Abril2019 != 94) 


# -- BLOQUE ANDALUCIA. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tabla1 <- table(VotoAndaluz$Nov2019, VotoAndaluz$Abril2019)
proporciones <- prop.table(tabla1, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
proporciones

# -- BLOQUE ACCE. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tabla2 <- table(VotoACCE$Nov2019, VotoACCE$Abril2019)
proporciones2 <- prop.table(tabla2, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
proporciones2

# -- Observacion de la diferencia entre una tabla de proporciones y otra
diferencia = proporciones - proporciones2
sum2 = sum(diferencia^2);sum2


# -- BLOQUE COMUNIDADES. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tabla3 <- table(Comunidades$Nov2019, Comunidades$Abril2019)
prop_com <- prop.table(tabla3, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
prop_com

# Empleamos la funcion creada
x <- difsum2(p_base = prop_com, t1 = tabla1, t2 = tabla2)
sum(x$p_f_t1[1,])
x$sum2
x$p_f_t1

# PROCESO BOOTSTRAP
valores = as.numeric()
b = 1:1000
for (i in b){
  valores[i] = difsum2(p_base = prop_com, t1 = tabla1, t2 = tabla2)$sum2
}


# --- (5) Quinto paso: Evaluar el valor obtenido con los datos originales de ambas tablas sobre el conjunto de valores
# ---     obtenidos.

hist(valores, xlim = c(0,3), main =" ", xlab = "Suma de cuadrados", ylab = "Frecuencias")
abline(v=sum2, col = "red", lwd = 2.5)
max(valores)

# COMENTARIO: Cae totalmente alejado de la distribucion de valores. Yo creo que este problema viene dado
# debido a la pequeña cantidad de casos que existen en ciertas variables que alteran gravemente las
# proporciones. Por lo tanto, voy a probar a llevar a cabo el mismo proceso pero reduciendo la tabla
# y eliminando ciertas categorias de respuesta de la variable 'Nov2019' y 'Abri2019' con tamaños
# muestrales demasiado pequeños y poco representativos. Unicamente se seleccionaran las respuestas
# correspondientes a los 5 grandes partidos del momento(PP(1), PSOE(2), CS(4), VOX(18), Unidas Podemos(21)) y a la 
# opcion de abstencion (97)

## -- ANDALUCIA.

table(Andalucia$Nov2019)
table(Andalucia$Abril2019)

# COMENTARIO: Las categorias 17, 50 y 95 de la variable 'Nov2019' seran eliminadas y las categorias
# 93, 95 y 96
Andalucia2 <- Estudio3269 %>%
  filter(CCAA == 1, !Nov2019 %in% c(77,17,50,95,96,98,99),
         !Abril2019 %in% c(77,93,95,96,98,99))

table(Andalucia2$Nov2019)
table(Andalucia2$Abril2019)

t_andal2 <- table(Andalucia2$Nov2019, Andalucia2$Abril2019)
prop_andal2 <- prop.table(t_andal2, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
prop_andal2

# Bloque1: (11) Cadiz, (14) Cordoba, (23) Jaen, (29) Malaga. Total de 358 individuos
# Bloque2: (4) Almeria, (18) Granada, (21) Huelva, (41) Sevilla. Total de 341 individuos

Bloque1 <- Andalucia2 %>%
  filter(PROV == 11 | PROV == 14| PROV == 23| PROV == 29 , 
         !Abril2019 %in% c(77,93,95,96,98,99), !Nov2019 %in% c(77,17,50,95,96,98,99))

Bloque2 <- Andalucia2 %>%
  filter(PROV == 4 | PROV == 18| PROV == 21| PROV == 41, 
         !Abril2019 %in% c(77,93,95,96,98,99), !Nov2019 %in% c(77,17,50,95,96,98,99))

# -- BLOQUE 1. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tablaA1 <- table(Bloque1$Nov2019, Bloque1$Abril2019)
proporcionesA1 <- prop.table(tablaA1, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
proporcionesA1

# -- BLOQUE 2. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tablaA2 <- table(Bloque2$Nov2019, Bloque2$Abril2019)
proporcionesA2 <- prop.table(tablaA2, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
proporcionesA2

diferencia = proporcionesA1 - proporcionesA2
sum2And = sum(diferencia^2)
sum2And

# Empleamos la funcion creada
x <- difsum2(p_base = prop_andal2, t1 = tablaA1, t2 = tablaA2)
sum(x$p_f_t1[1,])
x$sum2
x$p_f_t1

# PROCESO BOOTSTRAP
valores = as.numeric()
b = 1:1000
for (i in b){
  valores[i] = difsum2(p_base = prop_andal2, t1 = tablaA1, t2 = tablaA2)$sum2
}
hist(valores, main = " ", xlab = "Suma de cuadrados", ylab = "Frecuencias")
abline(v = sum2And, col = "red", lwd  = 2)
max(valores)

# Calculo del pvalor
# Contar cuántos valores bootstrap son mayores que el valor original
n.pvalor <- sum(valores > sum2And)

# Calcular la proporción de valores bootstrap mayores que el valor original
pvalor <- n.pvalor / length(valores);pvalor

# COMENTARIO: Despues de haber retirado las opciones de respuesta menos representativas de las variables
# que reunen el recuento de votos para las dos elecciones se puede ver que el valor real de la suma de cuadrados
# de la diferencia entre ambas tablas de proporciones, cae dentro del intervalo de valores obtenido por bootstrap.
# Para terminar, vamos a probar con muestras mas grandes. Esta vez lo haremos comparando la comunidad de Andalucia
# con el conjunto de comunidades ACCE.


## -- PRUEBA PARA LA COMPARACION ENTRE LA COMUNIDAD DE ANDALUCIA Y LAS OTRAS 4
## -- Esta vez retirando de los datos las siguientes opciones de voto:

## !Abril2019 %in% c(77,93,94,95,96,98,99),
## !Nov2019 %in% c(77,17,50,94,95,98,99))

# COMENTARIO: Se retiran debido a la pequeña cantidad de casos que presentan en
# la muestra.

sort(unique(Estudio3269$Nov2019))
sort(unique(Estudio3269$Abril2019))

# -- Filtro del conjunto de cuatro comunidades autonomas y Andalucia
Comunidades <- Estudio3269 %>%
  filter(CCAA == 1 | CCAA == 3 | CCAA == 7 | CCAA == 8 | CCAA == 11, 
         !Abril2019 %in% c(77,93,94,95,96,98,99),
         !Nov2019 %in% c(77,17,50,94,95,98,99)) 

# -- Filtro del voto andaluz
VotoAndaluz <- Estudio3269 %>%
  filter(CCAA == 1,
         !Abril2019 %in% c(77,93,94,95,96,98,99),
         !Nov2019 %in% c(77,17,50,94,95,98,99))


# -- Filtro del conjunto de cuatro comunidades autonomas
VotoACCE <- Estudio3269 %>%
  filter(CCAA == 3 | CCAA == 7 | CCAA == 8 | CCAA == 11,
         !Abril2019 %in% c(77,93,94,95,96,98,99),
         !Nov2019 %in% c(77,17,50,94,95,98,99)) 


# -- BLOQUE ANDALUCIA. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tabla1 <- table(VotoAndaluz$Nov2019, VotoAndaluz$Abril2019)
proporciones <- prop.table(tabla1, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
proporciones

# -- BLOQUE ACCE. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tabla2 <- table(VotoACCE$Nov2019, VotoACCE$Abril2019)
proporciones2 <- prop.table(tabla2, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
proporciones2

# -- Observacion de la diferencia entre una tabla de proporciones y otra
diferencia = proporciones - proporciones2
sum2 = sum(diferencia^2);sum2


# -- BLOQUE COMUNIDADES. Crear una tabla de contingencia y extraer tabla con las proporciones de fidelidad y transferencia de votos
tabla3 <- table(Comunidades$Nov2019, Comunidades$Abril2019)
prop_com <- prop.table(tabla3, margin = 1) # Calcular las proporciones de fidelidad y transferencia de votos
prop_com

# Empleamos la funcion creada
x <- difsum2(p_base = prop_com, t1 = tabla1, t2 = tabla2)
sum(x$p_f_t1[1,])
x$sum2
x$p_f_t1

# PROCESO BOOTSTRAP
valores = as.numeric()
b = 1:1000
for (i in b){
  valores[i] = difsum2(p_base = prop_com, t1 = tabla1, t2 = tabla2)$sum2
}


# --- Evaluar el valor obtenido con los datos originales de ambas tablas sobre el conjunto de valores
# --- obtenidos.

hist(valores, main = "", xlab = "Suma de cuadrados", ylab = "Frecuencias")
abline(v=sum2, col = "red", lwd  = 2)
max(valores)

# Calculo del pvalor
# Contar cuántos valores bootstrap son mayores que el valor original
n.pvalor <- sum(valores > sum2)

# Calcular la proporción de valores bootstrap mayores que el valor original
pvalor <- n.pvalor / length(valores);pvalor


