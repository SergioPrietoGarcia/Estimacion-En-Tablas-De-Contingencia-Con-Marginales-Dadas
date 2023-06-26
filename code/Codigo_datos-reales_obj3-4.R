################################################################################
######## CÓDIGO EMPLEADO EN LA CONSTRUCCIÓN DEL TRABAJO FIN DE MÁSTER ##########
################################################################################




#### ---- OBJETIVOS ESPECÍFICOS. En este apartado únicamente se trabaja con datos reales del Ayuntamiento de Torrent


# Diseñar un algoritmo de esperanza-maximización (EM) para la estimación de las probabilidades condicionales

# Estudiar la convergencia de las probabilidades condicionales a lo largo de las iteraciones del algoritmo y
# su tiempo de ejecución


############################################################
## -- CONJUNTO DE DATOS ELECCIONES MUNICIPALES TORRENT -- ##
############################################################
library(dplyr)
library(readxl)
library(tidyr)
tm <- read.csv2("C:/Users/Lenovo/Desktop/Universidad/Master Tecnicas Estadisticas/TFM/Municipales_Provisional_Torrent_2019.csv")
head(tm)
nrow(tm) # Numero de mesas electorales

# -- Seleccion de columnas con votos y codigo de mesa electoral
muni_Torr <- tm[,c(6:21)]
muni_Torr <- unite(muni_Torr, Mesa, c(Mesa.1, Mesa.2))
head(muni_Torr)
str(muni_Torr) # Las columnas tienen clase numerica menos la primera

# -- Creamos nueva columna "Abstencion"
muni_Torr <- mutate(muni_Torr, Abstencion = Censo - Votos.Totales)

# -- Creamos nueva columna "Otros Partidos", "Voto Blanco o Nulo" y seleccionamos las columnas correspondientes
muni_Torr <- mutate(muni_Torr, `Otros Partidos` = CONTIGO + GUANYANT + SOM.VALENCIANS + AVANT.LV)
muni_Torr <- mutate(muni_Torr, `Voto Blanco o Nulo` = Votos.Nulos + Votos.Blancos)
muni_Torr <- select(muni_Torr, Mesa, Censo, COMPROMIS, P.S.O.E., P.P., PODEM, C.s, VOX, `Otros Partidos`, `Voto Blanco o Nulo`, Abstencion)
head(muni_Torr)

##########################################################
## -- CONJUNTO DE DATOS ELECCIONES GENERALES TORRENT -- ##
##########################################################

tg <- read_excel("C:/Users/Lenovo/Desktop/Universidad/Master Tecnicas Estadisticas/TFM/Generales_Provisionales_Torrent_Abril_2019.xlsx")
tg <- as.data.frame(tg)
nrow(tg) # Numero de mesas electorales
tg <- unite(tg, Mesa, c(DISTRITO, SECCIÓN, MESA)) # Crear columna para identificar la mesa
head(tg)
str(tg) # Las columnas tienen clase numerica menos la primera

# -- Creamos nueva columna "Abstencion"
gene_Torr <- mutate(tg, Abstencion = CENSO - Votos)
head(gene_Torr)

# -- Creamos nueva columna "Otros Partidos", "Voto Blanco o Nulo" y seleccionamos las columnas correspondientes
gene_Torr <- mutate(gene_Torr, `Otros Partidos` = ERPV + PACMA + `UIG-SOM-CUIDES` + PCPE + `RECORTES CERO-GV` + `AVANT LOS VERDES`)
gene_Torr <- mutate(gene_Torr, `Voto Blanco o Nulo` = `Votos Nulos` + `Votos Blancos`)
gene_Torr <- select(gene_Torr, Mesa, CENSO, COMPROMÍS, PSOE, PP, `PODEMOS-EUPV`, Cs, VOX, `Otros Partidos`, `Voto Blanco o Nulo`, Abstencion)
head(gene_Torr)



####################################
## ALGORITMO EM DATOS DE TORRENT  ##
####################################

# CARGAMOS LOS DATOS DE AMBAS ELECCIONES
load("C:/Users/Lenovo/Desktop/Universidad/Master Tecnicas Estadisticas/TFM/DatosTorrent/datos_torr.RData")
# L1 <- muni_Torr[,3:11]; L2 <- gene_Torr[,3:11]
L2 <- muni_Torr[,3:11]; L1 <- gene_Torr[,3:11]
head(L1);head(L2)


### --- 1. Hacer $\hat n_{ij}^{(k)(0)}:= n··^{(k)} \hat P_i·^{(k)} P·_j^{(k)} = \frac {n_i·^{(k)} n·_j^{(k)}} {n··^{(k)}}$

K = nrow(L1)
J = ncol(L2)
I = ncol(L1)

# Calculo de n_ij_k0. Debe haber tantos n_ij_k0 como mesas electorales

# Definimos los objetos

n..k <- as.vector(round((rowSums(as.matrix(L1)) + rowSums(as.matrix(L2)))/2)) # En este caso coincide el numero de votos por mesa en ambas elecciones
ni.k <- as.matrix(L1) # Numero de votos para el partido i en la mesa k en las primeras elecciones
n.jk <- as.matrix(L2) # Numero de votos para el partido j en la mesa k en las segundas elecciones
n_ij_k0 <- list()

# Calculamos n_ij_k0

# FORMA (1)
for (k in 1:K){
  n_ij_k0[[k]] <- (matrix(ni.k[k,], ncol = 1, nrow = ncol(ni.k)) %*% matrix(n.jk[k,], ncol = ncol(n.jk), nrow = 1))/n..k[k]
}

# FORMA (2)
p_ik <- prop.table(ni.k, margin = 1)
p_jk <- prop.table(n.jk, margin = 1)
matriz <- matrix(0, ncol = J, nrow = I)
for (k in 1:K){
  for (i in 1:I){
    for (j in 1:J){
      matriz[i,j] <- n..k[k] * p_ik[k,i] * p_jk[k,j]
      n_ij_k0[[k]] <- matriz
    }}}

# COMENTARIO: Ambas formas de calcular n_ij_k0 devuelven el mismo resultado


### --- 2. Calcular $\hat n_{ij}^{(·)(0)}: = \sum_{k=1}^K \hat n_{ij}^{(k)(0)} \quad \text{y} \quad  \hat n_i·^{(·)(0)}=\sum_{s=1}^J \hat n_{is}^{(·)(s)}$

# Calculo de n_ij.0

n_ij.0 <- matrix(0, nrow=I, ncol=J) # Inicializar una matriz vacía para almacenar el resultado

# Recorrer la lista de matrices y sumarlas a la matriz resultado
for (i in 1:length(n_ij_k0)) {
  n_ij.0 <- n_ij.0 + n_ij_k0[[i]]
}

n_ij.0


# Calculo de ni.0
ni.0 = rowSums(n_ij.0)

# 3. Hacer $\hat C_{ij}^{(0)}:= \frac {\hat n_{ij}^{(·)(0)}} {\hat n_i·^{(·)(0)}} \quad i = 1, ..., I; \quad j = 1, ..., J$

# Calculo del iterante 0 C_ij

C_ij_0 = matrix(0, ncol = J, nrow = I)

for(i in 1:I){
  for(j in 1:J){
    C_ij_0[i,j] <- n_ij.0[i,j] / ni.0[i]
  }
}


#   Inicio del algoritmo EM
# 
# E4. Hacer $\tilde n_{ij}^{(k)(1)}:= n··^{(k)} \hat P_i·^{(k)} \hat C_{ij}^{(0)} = n_i·^{(k)}C_{ij}^{(0)} \quad i = 1, ..., I; \quad j = 1, ..., J; \quad k = 1, ..., K$

# E5. Calcular $\tilde n·_j^{(k)(1)}:= \sum_{i=1}^I \tilde n_{ij}^{(k)(1)}$ para $j=1,...,J$ y luego los cocientes $r_j = \frac{n·_j^{(k)}}{\tilde n·_j^{(k)(1)}}$, para $j = 1,...,J$

# E6. Obtener $\hat n_{ij}^{(k)(1)}:=r_j \tilde n_{ij}^{(k)(1)}$, para $i=1,...,I$, $j=1,...,J$, $k=1,...,K$

# E7. Calcular $\hat n_{ij}^{(·)(1)}:= \sum_{k=1}^K \hat n_{ij}^{(k)(1)}$ para $i = 1,...,I$, $j = 1,..., J$ y $\hat n_i·^{(·)(1)} := \sum_{s=1}^J \hat n_{is}^{(·)(1)}$ para $i = 1,...,I$

# M8. Hacer $\hat C_{ij}^{(1)} = \frac{\hat n_{ij}^{(·)(1)}}{\hat n_i·^{(·)(1)}}, \quad i = 1, ..., I; \quad j = 1, ..., J$

# 9.  Repetir los pasos 4-8 para obtener los $\hat n_{ij}^{(k)(l)}$ y $\hat C_{ij}^{(l)}$


# Objetos necesarios para ejecutar el bucle

p_ik <- prop.table(ni.k, margin = 1)
resultado <- list()
anterior = NULL

# Bucle algoritmo EM
M = 20000 # Objeto para iniciar el bucle
start_time = Sys.time()
for (m in 1:M){
  n_ij_kl <- list()
  matriz = matrix(0, ncol = J, nrow = I)
  # PASO 4E (A). Calculamos n_ij_kl para el primer iterante C_ij_0
  if(m == 1){
    for (k in 1:K){
      for (i in 1:I){
        for (j in 1:J){
          matriz[i,j] <- n..k[k]*(p_ik[k,i] * C_ij_0[i,j]) # Paso 4. Se emplea C_ij_0 como primer iterante
          n_ij_kl[[k]] <- matriz # Obtenemos tantas matrices como mesas electorales
        }}}}
  
  # PASO 4E (B). Calculamos n_ij_kl para sucesivos iterantes C_ij_l
  else{
    for (k in 1:K){
      for (i in 1:I){
        for (j in 1:J){
          matriz[i,j] <- n..k[k]*(p_ik[k,i] * C_ij_l[i,j]) # Paso 4. Se emplea C_ij_l como sucesivos iterantes
          n_ij_kl[[k]] <- matriz # Obtenemos tantas matrices como mesas electorales
        }}}}
  
  # PASO 5. 
  
  # Calculamos n.jkl (iteraccion l). Suma de todas las filas de las matrices de n_ij_kl
  n.jkl = list()
  for(k in 1:K){
    x = colSums(n_ij_kl[[k]])
    n.jkl[[k]] = x
  }
  
  # Calculamos los cocientes rj. Usamos n.jk y n.jkl
  rj <- vector("list", K)
  for(k in 1:K){
    for(j in 1:J){
      x = n.jk[k,j]/n.jkl[[k]][j]
      rj[[k]][j] = x # rj es una lista con 10 vectores
    }
  }
  # PASO 6. Obtener n_gorro_ij_kl
  n_gorro_ij_kl <- vector("list", K)
  for(k in 1:K){
    n_gorro_ij_kl[[k]] <- matrix(0, nrow = I, ncol = J)
    for(i in 1:I){
      for(j in 1:J){
        x = rj[[k]][j]*n_ij_kl[[k]][i,j]
        n_gorro_ij_kl[[k]][i,j] = x
      }
    }
  }
  
  # PASO 7. Calcular n_gorro_ij.l
  n_gorro_ij.l <- Reduce(`+`, n_gorro_ij_kl)
  
  # PASO 8. 
  # Calculamos las iteracciones de C_ij_l
  ni..l <- colSums(ni.k)
  C_ij_l <- matrix(0, ncol = J, nrow = I)
  for(i in 1:I){
    for(j in 1:J){
      C_ij_l[i,j] <- n_gorro_ij.l[i,j] / ni..l[i]
    }
  }
  # Normalizamos la matriz para que las filas sumen 1 cumpliendo la restriccion
  C_ij_l <- t(apply(C_ij_l, 1, function(x) x/sum(x)))
  
  # Almacenamos el resultado en una lista para ver como evoluciona C_ij_l en cada iteraccion
  resultado[[m]] <- C_ij_l
  
  # Verificar si se cumple la condición para detener el bucle
  if (m > 5) {
    max_diff <- max(abs(resultado[[m]] - resultado[[m-1]])) # se usa el maximo
    if (max_diff < 0.000001) {
      break
    }
  }
  
  # Actualizar la variable de la iteración anterior
  anterior <- C_ij_l
}
end_time = Sys.time()
end_time - start_time
length(resultado)
round(anterior,3)
rowSums(anterior)
colnames(anterior) <-  c("COMPROMIS", "PSOE", "PP","PODEM", "C.s", "VOX", "O.P.", "Blanco o Nulo", "Abstencion" )
rownames(anterior) <- c("COMPROMIS", "PSOE", "PP","PODEM", "C.s", "VOX", "O.P.", "Blanco o Nulo", "Abstencion" )

round(resultado[[1]], 4)
round(anterior,3)
round(anterior,4)


## GRAFICOS PARA OBSERVAR LA CONVERGENCIA DE LAS PROBABILIDADES CONDICIONALES

# En primer lugar, se extraen como vector los elementos [i,j] de cada una de las
# matrices del objeto "resultado"

C <- I*J # C de celdas
list_vec <- vector("list", C)

# Itera sobre las celdas de la matriz y extrae los valores correspondientes de cada matriz
for (c in 1:C) {
  i <- ((c - 1) %% I) + 1 # fila correspondiente a la celda c
  j <- ((c - 1) %/% I) + 1 # columna correspondiente a la celda c
  
  vector_ij <- sapply(resultado, function(x) x[i,j]) # extrae el valor i,j de cada matriz
  
  list_vec[[c]] <- vector_ij # agrega el vector a la lista
}

round(anterior,9)
length(resultado)
list_vec[[1]][6444];list_vec[[2]][6444] # Ordenado por columnas
length(list_vec)


# COMENTARIO: La representacion de la convergencia para este caso en concreto tendra
# la intencion de estudiar la convergencia entre aquellas probabilidades condicionales
# de la diagonal y aquellas probabilidades condicionales más próximas a cero

# Diagonal de la matriz de probabilidades condicionales
round(anterior,9) 
anterior[1,1]; anterior[2,2]; anterior[3,3]; anterior[4,4]; anterior[5,5]; anterior[6,6]; anterior[7,7]; anterior[8,8]; anterior[9,9]
diagonal <- c(which(anterior == anterior[1,1]),which(anterior == anterior[2,2]),which(anterior == anterior[3,3]),which(anterior == anterior[4,4]),which(anterior == anterior[5,5]),which(anterior == anterior[6,6]),which(anterior == anterior[7,7]),which(anterior == anterior[8,8]),which(anterior == anterior[9,9]))

# Valores proximos a cero
round(anterior,9)  # Valores: [2,1] <- 0| [6,1] <- 0.01902158 | [3,2] <- 0.031920028 | [2,3] <- 0.054355250 | [4,7] <- 0.101235404
anterior[2,1]; anterior[6,1]; anterior[3,2]; anterior[2,3]; anterior[4,7]
cercano_cero <- c(which(anterior == anterior[2,1])[1],which(anterior == anterior[6,1])[1],which(anterior == anterior[3,2])[1],which(anterior == anterior[2,3])[1],which(anterior == anterior[4,7])[1])

# Se procede con la creacion del grafico 
# para cada uno de los partidos

x_seq <- seq(from = 0, to = length(list_vec[[1]]), by = 100) # Secuencia de 100 en 100 para el eje x
R <- length(resultado)

## Probabilidades condicionales de la diagonal
diagonal;round(anterior,9)
# Grafico para la diagonal de la matriz de probabilidades condicional
plot(1:R, list_vec[[1]], type = "l", col = "brown", 
     ylim = c(-0.1, 1.6), xlim = c(1,R),
     xaxt = "n", lwd = 2.5, main = " Convergencia probabilidades condicionales de la diagonal ", 
     ylab = "Probabilidades Condicionales", xlab = "Iteraciones")
axis(1, at = x_seq) # Introducir nombres del eje x

lines(1:R, list_vec[[11]], col = "red", lwd = 2.5) # Representacion convergencia PSOE - PSOE
lines(1:R, list_vec[[21]], col = "blue", lwd = 2.5) # Representacion convergencia PP -PP 
lines(1:R, list_vec[[31]], col = "purple", lwd = 2.5) # Representacion convergencia PODEM - PODEM
lines(1:R, list_vec[[41]], col = "orange", lwd = 2.5) # Representacion convergencia Cs - Cs
lines(1:R, list_vec[[51]], col = "green", lwd = 2.5) # Representacion convergencia VOX - VOX
lines(1:R, list_vec[[61]], col = "pink", lwd = 2.5) # Representacion convergencia O.P - O.P
lines(1:R, list_vec[[71]], col = "grey", lwd = 2.5) # Representacion convergencia BlancoNulo - BlancoNulo
lines(1:R, list_vec[[81]], col = "cyan", lwd = 2.5) # Representacion convergencia Abstencion - Abstencion

# Agrega leyenda
legend("topright", legend = c("COMPROMIS - COMPROMIS", "PSOE - PSOE", "PP - PP", "PODEM - PODEM", "Cs - Cs", "VOX - VOX", "O.P - O.P", "BlancoNulo - BlancoNulo", "Abstencion - Abstencion" ),
       col = c("brown", "red", "blue", "purple", "orange", "green", "pink", "grey","cyan"  ), lty = 1, lwd = 2.5, cex = 0.65, bty = "n")

grafico_diag_grande <- recordPlot() # Guardamos el grafico
windows();grafico_diag_grande

# Reducido
plot(1:R, list_vec[[1]], type = "l", col = "brown", 
     ylim = c(-0.1, 1.5), xlim = c(1,500),
     xaxt = "n", lwd = 2.5, main = " Convergencia probabilidades condicionales de la diagonal ", 
     ylab = "Probabilidades Condicionales", xlab = "Iteraciones")
axis(1, at = x_seq) # Introducir nombres del eje x

lines(1:R, list_vec[[11]], col = "red", lwd = 2.5) # Representacion convergencia PSOE - PSOE
lines(1:R, list_vec[[21]], col = "blue", lwd = 2.5) # Representacion convergencia PP -PP 
lines(1:R, list_vec[[31]], col = "purple", lwd = 2.5) # Representacion convergencia PODEM - PODEM
lines(1:R, list_vec[[41]], col = "orange", lwd = 2.5) # Representacion convergencia Cs - Cs
lines(1:R, list_vec[[51]], col = "green", lwd = 2.5) # Representacion convergencia VOX - VOX
lines(1:R, list_vec[[61]], col = "pink", lwd = 2.5) # Representacion convergencia O.P - O.P
lines(1:R, list_vec[[71]], col = "grey", lwd = 2.5) # Representacion convergencia BlancoNulo - BlancoNulo
lines(1:R, list_vec[[81]], col = "cyan", lwd = 2.5) # Representacion convergencia Abstencion - Abstencion

# Agrega leyenda
legend("topright", legend = c("COMPROMIS - COMPROMIS", "PSOE - PSOE", "PP - PP", "PODEM - PODEM", "Cs - Cs", "VOX - VOX", "O.P - O.P", "BlancoNulo - BlancoNulo", "Abstencion - Abstencion" ),
       col = c("brown", "red", "blue", "purple", "orange", "green", "pink", "grey","cyan"  ), lty = 1, lwd = 2.5, cex = 0.65, bty = "n")

grafico_diag_reducido <- recordPlot() # Guardamos el grafico
windows();grafico_diag_reducido


## Probabilidades condicionales cercanas a 0.
cercano_cero;round(anterior,9)
windows()
# Grafico para el valor de probabilidad condicional cercano a 0
plot(1:R, list_vec[[cercano_cero[[1]]]], type = "l", col = "red", 
     ylim = c(0, 0.3), xlim = c(1,1000),
     xaxt = "n", lwd = 2.5, main = " Convergencia Probabilidades cercanas a 0", 
     ylab = "Probabilidades Condicionales", xlab = "Iteraciones")
axis(1, at = x_seq) # Introducir nombres del eje x

lines(1:R, list_vec[[cercano_cero[[2]]]], col = "blue", lwd = 2.5) # Representacion convergencia VOX - COMPROMIS
lines(1:R, list_vec[[cercano_cero[[3]]]], col = "green", lwd = 2.5) # Representacion convergencia PP - PSOE
lines(1:R, list_vec[[cercano_cero[[4]]]], col = "yellow", lwd = 2.5) # Representacion convergencia PSOE - PP
lines(1:R, list_vec[[cercano_cero[[5]]]], col = "purple", lwd = 2.5) # Representacion convergencia PODEM - O.P.


# Agrega leyenda
legend("topright", legend = c("PSOE - COMPROMIS", "VOX - COMPROMIS", "PP - PSOE", "PSOE - PP", "PODEM - O.P."),
       col = c("red", "blue", "green", "yellow", "purple"), lty = 1, lwd = 2.5, cex = 0.65, bty = "n")




## ESTIMACION PROBABILIDADES CONJUNTAS PARA LA MESA ELECTORAL 1

# P_ij = Pi· * C_ij

round(anterior,9)

mesa1 <- as.vector(p_ik[1,])
I <- 1:9
J <- 1:9

prob_conj_m1 <- matrix(NA, ncol = 9, nrow = 9)

for (j in J) {
  for (i in I) {
    celda <- mesa1[i] * anterior[i, j]
    prob_conj_m1[i, j] <- celda
  }
}

round(prob_conj_m1,4)


