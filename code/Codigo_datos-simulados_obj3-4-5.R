################################################################################
######## CÓDIGO EMPLEADO EN LA CONSTRUCCIÓN DEL TRABAJO FIN DE MÁSTER ##########
################################################################################



#### ---- OBJETIVOS ESPECÍFICOS. En este apartado únicamente se trabaja con datos simulados

library(dplyr)

# Diseñar un algoritmo de esperanza-maximización (EM) para la estimación de las probabilidades condicionales

# Estudiar la convergencia de las probabilidades condicionales a lo largo de las iteraciones del algoritmo y
# su tiempo de ejecución

# Estimar mediante Monte Carlo el error cuadrático medio (MSE) de estimación de las probabilidades
# condicionales para cada conjunto de datos simulados



######################################
## ALGORITMO EM CON DATOS SIMULADOS ## 
######################################



### EJEMPLO 1


## SIMULACION DE  DATOS PARA J = 3, I = 3 y K = 10 
## CON PROBABILIDADES CONDICIONALES MAYORES QUE 0.15 Y 
## PROBABILIDADES EN LAS PRIMERAS ELECCIONES MAYORES TAMBIEN QUE 0.15


# PROBABILIDADES CONDICIONALES:

#               PARTIDO 1   PARTIDO 2     PARTIDO 3
# PARTIDO 1     0.65        0.15          0.20
# PARTIDO 2     0.25        0.60          0.15
# PARTIDO 3     0.30        0.20          0.50

I = 3;J = 3; K = 10
partidos_L1  <- c("PARTIDO 1", "PARTIDO 2", "PARTIDO 3")
partidos_L2  <- c("PARTIDO 1", "PARTIDO 2", "PARTIDO 3")
cond_sim <- matrix(c(0.65,     0.15,       0.20,
                     0.25,     0.60,       0.15,
                     0.30,     0.20,       0.50), ncol = 3, nrow = 3, byrow = T)

colnames(cond_sim) <- partidos_L2
rownames(cond_sim) <- partidos_L1
rowSums(cond_sim)

# Como se reparten las probabilidades por mesa electoral para las primeras elecciones.
# Numero de mesas pequeño (K = 10). 

prob_L1 <- matrix(c(0.45, 0.25, 0.30,
                    0.55, 0.30, 0.15,
                    0.53, 0.27, 0.20,
                    0.39, 0.31, 0.30,
                    0.42, 0.35, 0.23,
                    0.33, 0.40, 0.27,
                    0.38, 0.29, 0.33,
                    0.25, 0.34, 0.41,
                    0.41, 0.34, 0.25,
                    0.30, 0.39, 0.31), ncol = 3, nrow = 10, byrow = T)
rowSums(prob_L1)

# Fijar numero de electores por mesa (n..^(k))
n_mesas <- c(400, 450, 500, 550, 600, 650, 700, 750, 800, 850);n_mesas

# Simulacion de las frecuencias de voto en las primeras elecciones en cada mesa a partir
# de los vectores de probabilidad que conforman la matriz "prob_L1"

set.seed(1) # Semilla para fijar la simulacion de los datos

elecciones1  <- matrix(0, ncol = I, nrow = K)
for(k in 1:K){
  x <- rmultinom(1, n_mesas[k], prob = prob_L1[k,])
  elecciones1[k,] <- x
}

elecciones1


# Simulacion de las frecuencias de voto conjuntas en las dos elecciones en cada mesa a partir
# de la matriz de probabilidades condicionales y los "resultados" en las primeras elecciones.
# Habra tantas matrices de frecuencias conjunta como mesas electorales

frec_conj <- vector("list", K)
matriz <- matrix(0, ncol = J, nrow = I)
for(k in 1:K){
  for(i in 1:I){
    for(j in 1:J){
      fila <- elecciones1[k,]
      x <- t(rmultinom(1, fila[i], prob = cond_sim[i,]))
      matriz[i,] <- x
      frec_conj[[k]] <- matriz
    }}}

frec_conj

# Lo que obtenemos con el objeto "frec_conj" son el conjunto de frecuencias que forman las transferencias
# de voto de unas elecciones a otras. Esto en la realidad es inobservable, por lo que a partir de este
# objeto debemos extraer las frecuencias marginales de las columnas para cada mesa electoral para asi 
# obtener una matriz similar a "elecciones1" pero esta vez para las segundas elecciones, "elecciones2"

elecciones2 <- matrix(0, ncol = J, nrow = K)
for(k in 1:K){
  x <- colSums(frec_conj[[k]])
  elecciones2[k,] <- x
}

elecciones2 # Matriz que se encuentra formada por el numero de votos en cada mesa electoral a cada partido en 
# las segundas elecciones


# DATOS SIMULADOS CON LOS QUE CALCULAR LAS PROBABILIDADES CONDICIONALES
colnames(elecciones1) <- partidos_L1; colnames(elecciones2) <- partidos_L2
elecciones1;elecciones2
L1 <- elecciones1; L2 <- elecciones2


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
    # mean_diff <- mean(abs(resultado[[m]] - resultado[[m-1]])) # se usa la media
    mean_diff <- max(abs(resultado[[m]] - resultado[[m-1]])) # se usa el maximo
    if (mean_diff < 0.0000001) {
      break
    }
  }
  
  # Actualizar la variable de la iteración anterior
  anterior <- C_ij_l
}

end_time = Sys.time()
end_time - start_time # Tiempo que ha tardado en ejecutarse el algoritmo
length(resultado)
round(anterior,9)
rowSums(anterior)

cond_sim;round(anterior,9)
round(cond_sim - anterior,9)

# COMENTARIO: Se puede observar que cuando se trabaja con datos simulados donde
# las probabilidades condicionales fijadas no tienen valores proximos a cero, sino
# todo lo contrario, presentan valores alejados de 0 (mayores a 0.15), además de que
# el algoritmo finalice con un número "pequeño" de iteracciones (si lo comparamos con
# datos simulados semejantes a la realidad y datos reales), las diferencias entre 
# la matriz inicial fijada de probabilidades condicionales y su estimación no comete un 
# error mayor a 0.05.


## GRAFICOS PARA OBSERVAR LA CONVERGENCIA DE LAS PROBABILIDADES CONDICIONALES

# En primer lugar, se extraen como vector los elementos [i,j] de cada una de las
# matrices del objeto "resultado"

C <- I*J # C de celdas
list_vec <- vector("list", C)

# Itera sobre las celdas de la matriz y extrae los valores correspondientes de cada matriz
for (c in 1:C) {
  i <- ((c - 1) %/% J) + 1 # fila correspondiente a la celda c
  j <- ((c - 1) %% J) + 1 # columna correspondiente a la celda c
  
  vector_ij <- sapply(resultado, function(x) x[i,j]) # extrae el valor i,j de cada matriz
  
  list_vec[[c]] <- vector_ij # agrega el vector a la lista
}


anterior
list_vec[[1]][1000];list_vec[[2]][1000];list_vec[[3]][1000]
list_vec[[4]][1000];list_vec[[5]][1000];list_vec[[6]][1000]
list_vec[[7]][1000];list_vec[[8]][1000];list_vec[[9]][1000]



# En segundo lugar, se procede con la creacion del grafico 
# para cada uno de los partidos

x_seq <- seq(from = 100, to = length(list_vec[[1]]), by = 100) # Secuencia de 100 en 100 para el eje x
R <- length(resultado)

## PARTIDO 1

# Grafico con linea de convergencia de la probabilidad condicional para PARTIDO 1 - PARTIDO 1
plot(1:R, list_vec[[1]], type = "l", col = "red", 
     ylim = c(0.1, 0.9), xlim = c(1,R),
     xaxt = "n", lwd = 2.5, main = " Convergencia Probabilidades condicionales PARTIDO 1", 
     ylab = "Probabilidades Condicionales", xlab = "Iteraciones")
axis(1, at = x_seq) # Introducir nombres del eje x

# Bucle for para ver donde empieza aproximadamente la convergencia en PARTIDO 1 - PARTIDO 1
for (i in 2:length(list_vec[[1]])) {
  diff <- abs(list_vec[[1]][i] - list_vec[[1]][i-1])
  if (diff <= 0.0001) {
    # Si la diferencia es menor o igual a 0.001, se imprime el índice y el valor correspondiente
    cat("La convergencia se inicia aproximadamente en el punto:", i-1, "con valor", list_vec[[1]][i-1], "\n")
    conv_p11 <- i-1 # Nombramos el indice donde comienza aproximadamente la convergencia
    break
  }
}

abline(v = conv_p11, col = "lightsalmon", lty = 2, lwd = 2)
text(conv_p11, list_vec[[1]][conv_p11],conv_p11 , pos=2) # Marcar punto donde aproximadamente empieza a converger
text(list_vec[[1]][1], list_vec[[1]][1],round(list_vec[[1]][1],3) , pos=2, col = "dimgrey") # valor inicial del proceso
text(length(list_vec[[1]]), tail(list_vec[[1]],1),round(tail(list_vec[[1]],1),3) , pos=1,  col = "dimgrey")


# Linea de convergencia de la probabilidad condicional para PARTIDO 1 - PARTIDO 2

# Bucle for para ver donde empieza aproximadamente la convergencia en PARTIDO 1 - PARTIDO 2
for (i in 2:length(list_vec[[2]])) {
  diff <- abs(list_vec[[2]][i] - list_vec[[2]][i-1])
  if (diff <= 0.0001) {
    # Si la diferencia es menor o igual a 0.001, se imprime el índice y el valor correspondiente
    cat("La convergencia se inicia aproximadamente en el punto:", i-1, "con valor", list_vec[[1]][i-1], "\n")
    conv_p12 <- i-1 # Nombramos el indice donde comienza aproximadamente la convergencia
    break
  }
}

lines(1:R, list_vec[[2]], col = "blue", lwd = 2.5)
abline(v = conv_p12, col = "skyblue", lty = 2, lwd = 2)
text(conv_p12, list_vec[[2]][conv_p12],conv_p12 , pos=2) # Marcar punto donde aproximadamente empieza a converger
text(list_vec[[2]][1], list_vec[[2]][1],round(list_vec[[2]][1],3) , pos=2, col = "dimgrey") # valor inicial del proceso
text(length(list_vec[[2]]), tail(list_vec[[2]],1),round(tail(list_vec[[2]],1),3) , pos=1, col = "dimgrey")


# Linea de convergencia de la probabilidad condicional para PARTIDO 1 - PARTIDO 3

# Bucle for para ver donde empieza aproximadamente la convergencia en PARTIDO 1 - PARTIDO 3
for (i in 2:length(list_vec[[3]])) {
  diff <- abs(list_vec[[3]][i] - list_vec[[3]][i-1])
  if (diff <= 0.0001) {
    # Si la diferencia es menor o igual a 0.001, se imprime el índice y el valor correspondiente
    cat("La convergencia se inicia aproximadamente en el punto:", i-1, "con valor", list_vec[[1]][i-1], "\n")
    conv_p13 <- i-1 # Nombramos el indice donde comienza aproximadamente la convergencia
    break
  }
}

lines(1:R, list_vec[[3]], col = "green", lwd = 2.5)
abline(v = conv_p13, col = "lightgreen", lty = 2, lwd = 2)
text(conv_p13, list_vec[[3]][conv_p13],conv_p13 , pos=2) # Marcar punto donde aproximadamente empieza a converger
text(list_vec[[3]][1], list_vec[[3]][1],round(list_vec[[3]][1],3) , pos=2, col = "dimgrey") # valor inicial del proceso
text(length(list_vec[[3]]), tail(list_vec[[3]],1),round(tail(list_vec[[3]],1),3) , pos=1, col = "dimgrey")

# Agrega leyenda
legend("topright", legend = c("PARTIDO 1 - PARTIDO 1", "PARTIDO 1 - PARTIDO 2", "PARTIDO 1 - PARTIDO 3", "Iteración", "Valor inicial y final", "Convergencia"),
       col = c("red", "blue", "green", "black", "dimgrey", "black"), lty = c(1,1,1,1,1,2), lwd = 2.5, cex = 0.75, bty = "n")

grafico_conv_p1 <- recordPlot() # Guardamos el grafico



## PARTIDO 2

# Grafico con linea de convergencia de la probabilidad condicional para PARTIDO 2 - PARTIDO 1
plot(1:R, list_vec[[4]], type = "l", col = "red", 
     ylim = c(0.1, 0.9), xlim = c(1,R),
     xaxt = "n", lwd = 2.5, main = " Convergencia Probabilidades condicionales PARTIDO 2", 
     ylab = "Probabilidades Condicionales", xlab = "Iteraciones")
axis(1, at = x_seq) # Introducir nombres del eje x

# Bucle for para ver donde empieza aproximadamente la convergencia en PARTIDO 2 - PARTIDO 1
for (i in 2:length(list_vec[[4]])) {
  diff <- abs(list_vec[[4]][i] - list_vec[[4]][i-1])
  if (diff <= 0.0001) {
    # Si la diferencia es menor o igual a 0.001, se imprime el índice y el valor correspondiente
    cat("La convergencia se inicia aproximadamente en el punto:", i-1, "con valor", list_vec[[4]][i-1], "\n")
    conv_p21 <- i-1 # Nombramos el indice donde comienza aproximadamente la convergencia
    break
  }
}

abline(v = conv_p21, col = "lightsalmon", lty = 2, lwd = 2)
text(conv_p21, list_vec[[4]][conv_p21],conv_p21 , pos=2) # Marcar punto donde aproximadamente empieza a converger
text(list_vec[[4]][1], list_vec[[4]][1],round(list_vec[[4]][1],3) , pos=2, col = "dimgrey") # valor inicial del proceso
text(length(list_vec[[4]]), tail(list_vec[[4]],1),round(tail(list_vec[[4]],1),3) , pos=1, col = "dimgrey")


# Linea de convergencia de la probabilidad condicional para PARTIDO 2 - PARTIDO 2

# Bucle for para ver donde empieza aproximadamente la convergencia en PARTIDO 2 - PARTIDO 2
for (i in 2:length(list_vec[[5]])) {
  diff <- abs(list_vec[[5]][i] - list_vec[[5]][i-1])
  if (diff <= 0.0001) {
    # Si la diferencia es menor o igual a 0.001, se imprime el índice y el valor correspondiente
    cat("La convergencia se inicia aproximadamente en el punto:", i-1, "con valor", list_vec[[5]][i-1], "\n")
    conv_p22 <- i-1 # Nombramos el indice donde comienza aproximadamente la convergencia
    break
  }
}

lines(1:R, list_vec[[5]], col = "blue", lwd = 2.5)
abline(v = conv_p22, col = "skyblue", lty = 2, lwd = 2)
text(conv_p22, list_vec[[5]][conv_p22],conv_p22 , pos=2) # Marcar punto donde aproximadamente empieza a converger
text(list_vec[[5]][1], list_vec[[5]][1],round(list_vec[[5]][1],3) , pos=2, col = "dimgrey") # valor inicial del proceso
text(length(list_vec[[5]]), tail(list_vec[[5]],1),round(tail(list_vec[[5]],1),3) , pos=1, col = "dimgrey")


# Linea de convergencia de la probabilidad condicional para PARTIDO 2 - PARTIDO 3

# Bucle for para ver donde empieza aproximadamente la convergencia en PARTIDO 2 - PARTIDO 3
for (i in 2:length(list_vec[[6]])) {
  diff <- abs(list_vec[[6]][i] - list_vec[[6]][i-1])
  if (diff <= 0.0001) {
    # Si la diferencia es menor o igual a 0.001, se imprime el índice y el valor correspondiente
    cat("La convergencia se inicia aproximadamente en el punto:", i-1, "con valor", list_vec[[6]][i-1], "\n")
    conv_p23 <- i-1 # Nombramos el indice donde comienza aproximadamente la convergencia
    break
  }
}

lines(1:R, list_vec[[6]], col = "green", lwd = 2.5)
abline(v = conv_p23, col = "lightgreen", lty = 2, lwd = 2)
text(conv_p23, list_vec[[6]][conv_p23],conv_p23 , pos=2) # Marcar punto donde aproximadamente empieza a converger
text(list_vec[[6]][1], list_vec[[6]][1],round(list_vec[[6]][1],3) , pos=2, col = "dimgrey") # valor inicial del proceso
text(length(list_vec[[6]]), tail(list_vec[[6]],1),round(tail(list_vec[[6]],1),3) , pos=1, col = "dimgrey")

# Agrega leyenda
legend("topright", legend = c("PARTIDO 2 - PARTIDO 1", "PARTIDO 2 - PARTIDO 2", "PARTIDO 2 - PARTIDO 3", "Iteración", "Valor inicial y final", "Convergencia"),
       col = c("red", "blue", "green", "black", "dimgrey", "black"), lty = c(1,1,1,1,1,2), lwd = 2.5, cex = 0.75, bty = "n")

grafico_conv_p2 <- recordPlot() # Guardamos el grafico


## PARTIDO 3

# Grafico con linea de convergencia de la probabilidad condicional para PARTIDO 3 - PARTIDO 1
plot(1:R, list_vec[[7]], type = "l", col = "red", 
     ylim = c(0.1, 0.9), xlim = c(1,R),
     xaxt = "n", lwd = 2.5, main = " Convergencia Probabilidades condicionales PARTIDO 3", 
     ylab = "Probabilidades Condicionales", xlab = "Iteraciones")
axis(1, at = x_seq) # Introducir nombres del eje x

# Bucle for para ver donde empieza aproximadamente la convergencia en PARTIDO 3 - PARTIDO 1
for (i in 2:length(list_vec[[7]])) {
  diff <- abs(list_vec[[7]][i] - list_vec[[7]][i-1])
  if (diff <= 0.0001) {
    # Si la diferencia es menor o igual a 0.001, se imprime el índice y el valor correspondiente
    cat("La convergencia se inicia aproximadamente en el punto:", i-1, "con valor", list_vec[[7]][i-1], "\n")
    conv_p31 <- i-1 # Nombramos el indice donde comienza aproximadamente la convergencia
    break
  }
}

abline(v = conv_p31, col = "lightsalmon", lty = 2, lwd = 2)
text(conv_p31, list_vec[[7]][conv_p31],conv_p31 , pos=2) # Marcar punto donde aproximadamente empieza a converger
text(list_vec[[7]][1], list_vec[[7]][1],round(list_vec[[7]][1],3) , pos=2, col = "dimgrey") # valor inicial del proceso
text(length(list_vec[[7]]), tail(list_vec[[7]],1),round(tail(list_vec[[7]],1),3) , pos=1, col = "dimgrey")


# Linea de convergencia de la probabilidad condicional para PARTIDO 3 - PARTIDO 2

# Bucle for para ver donde empieza aproximadamente la convergencia en PARTIDO 3 - PARTIDO 2
for (i in 20:length(list_vec[[8]])) {
  diff <- abs(list_vec[[8]][i] - list_vec[[8]][i-1])
  if (diff <= 0.0001) {
    # Si la diferencia es menor o igual a 0.001, se imprime el índice y el valor correspondiente
    cat("La convergencia se inicia aproximadamente en el punto:", i-1, "con valor", list_vec[[8]][i-1], "\n")
    conv_p32 <- i-1 # Nombramos el indice donde comienza aproximadamente la convergencia
    break
  }
}

lines(1:R, list_vec[[8]], col = "blue", lwd = 2.5)
abline(v = conv_p32, col = "skyblue", lty = 2, lwd = 2)
text(conv_p32, list_vec[[8]][conv_p32],conv_p32 , pos=2) # Marcar punto donde aproximadamente empieza a converger
text(list_vec[[8]][1], list_vec[[8]][1],round(list_vec[[8]][1],3) , pos=2, col = "dimgrey") # valor inicial del proceso
text(length(list_vec[[8]]), tail(list_vec[[8]],1),round(tail(list_vec[[8]],1),3) , pos=1, col = "dimgrey")


# Linea de convergencia de la probabilidad condicional para PARTIDO 3 - PARTIDO 3

# Bucle for para ver donde empieza aproximadamente la convergencia en PARTIDO 3 - PARTIDO 3
for (i in 2:length(list_vec[[9]])) {
  diff <- abs(list_vec[[9]][i] - list_vec[[9]][i-1])
  if (diff <= 0.0001) {
    # Si la diferencia es menor o igual a 0.001, se imprime el índice y el valor correspondiente
    cat("La convergencia se inicia aproximadamente en el punto:", i-1, "con valor", list_vec[[9]][i-1], "\n")
    conv_p33 <- i-1 # Nombramos el indice donde comienza aproximadamente la convergencia
    break
  }
}

lines(1:R, list_vec[[9]], col = "green", lwd = 2.5)
abline(v = conv_p33, col = "lightgreen", lty = 2, lwd = 2)
text(conv_p33, list_vec[[9]][conv_p33],conv_p33 , pos=2) # Marcar punto donde aproximadamente empieza a converger
text(list_vec[[9]][1], list_vec[[9]][1],round(list_vec[[9]][1],3) , pos=2, col = "dimgrey") # valor inicial del proceso
text(length(list_vec[[9]]), tail(list_vec[[9]],1),round(tail(list_vec[[9]],1),3) , pos=1, col = "dimgrey")

# Agrega leyenda
legend("topright", legend = c("PARTIDO 3 - PARTIDO 1", "PARTIDO 3 - PARTIDO 2", "PARTIDO 3 - PARTIDO 3", "Iteración", "Valor inicial y final", "Convergencia"),
       col = c("red", "blue", "green", "black", "dimgrey", "black"), lty = c(1,1,1,1,1,2), lwd = 2.5, cex = 0.75, bty = "n")

grafico_conv_p3 <- recordPlot() # Guardamos el gráfico



# Impresion de los 3 gráficos para cada uno de los valores en cada fila de la matriz de 
# probabilidades condicionales
windows();grafico_conv_p1
windows();grafico_conv_p2
windows();grafico_conv_p3


# COMENTARIO: En lineas generales se observa que la convergencia en todos
# los graficos se hace mas fuerte (es decir, los valores empiezan a ser cada
# vez mas proximos) antes de la iteración 405. Ademas, como era de esperar,
# los valores que consiguen converger antes son aquellos que se encuentran más
# proximos del valor con el que se inicia el algoritmo.


















### EJEMPLO 2

## SIMULACION DE  DATOS PARA J = 6, I = 7 y K = 10 


# PROBABILIDADES CONDICIONALES:

#         PP    VOX     PSOE     PODEM    CS      O.P.
# PP      0.80  0.10    0.02      0       0.05    0.03
# VOX     0.12  0.75     0        0.03    0.05    0.05 
# PSOE    0.05  0.05    0.70      0.15    0.03    0.02
# PODEM    0     0      0.30      0.65     0      0.05
# COMUN    0     0      0.45      0.50     0      0.05
# CS      0.15  0.10    0.05       0      0.65    0.05
# O.P.    0.15  0.10    0.20      0.05    0.15    0.35

I = 7;J = 6; K = 10
partidos_L1  <- c("PP", "VOX", "PSOE", "PODEM", "COMUN", "CS", "O.P.")
partidos_L2  <- c("PP", "VOX", "PSOE", "PODEM", "CS", "O.P.")
cond_sim <- matrix(c(0.80,  0.10,    0.02,      0,       0.05,    0.03,
                     0.12,  0.75,     0,        0.03,    0.05,    0.05,
                     0.05,  0.05,    0.70,      0.15,    0.03,    0.02,
                     0,     0,       0.30,      0.65,     0,      0.05,
                     0,     0,       0.45,      0.50,     0,      0.05,
                     0.15,  0.10,    0.05,       0,      0.65,    0.05,
                     0.15,  0.10,    0.20,      0.05,    0.15,    0.35), ncol = 6, nrow = 7, byrow = T)
colnames(cond_sim) <- partidos_L2
rownames(cond_sim) <- partidos_L1
rowSums(cond_sim)

# Como se reparten las probabilidades por mesa electoral para las primeras elecciones.
# Numero de mesas pequeño (K = 10). 

prob_L1 <- matrix(c(0.25, 0.15, 0.30, 0.10, 0.05, 0.10, 0.05,
                    0.20, 0.10, 0.35, 0.15, 0.10, 0.05, 0.05,
                    0.15, 0.05, 0.40, 0.15, 0.05, 0.15, 0.05,
                    0.20, 0.15, 0.25, 0.10, 0.05, 0.15, 0.10,
                    0.30, 0.20, 0.20, 0.10, 0.10, 0.05, 0.05,
                    0.18, 0.12, 0.33, 0.17, 0.04, 0.10, 0.06,
                    0.33, 0.17, 0.18, 0.12, 0.10, 0.06, 0.04,
                    0.30, 0.10, 0.25, 0.15, 0.10, 0.05, 0.05,
                    0.20, 0.10, 0.30, 0.10, 0.10, 0.05, 0.15,
                    0.25, 0.10, 0.30, 0.15, 0.10, 0.10, 0), ncol = 7, nrow = 10, byrow = T)
rowSums(prob_L1)
colnames(prob_L1) <- partidos_L1

# Fijar numero de electores por mesa (n..^(k))
set.seed(1)
n_mesas <- sample(400:888, 10, replace = T);n_mesas

# Simulacion de las frecuencias de voto en las primeras elecciones en cada mesa a partir
# de los vectores de probabilidad que conforman la matriz "prob_L1"

elecciones1  <- matrix(0, ncol = I, nrow = K)
for(k in 1:K){
  set.seed(1)
  x <- rmultinom(1, n_mesas[k], prob = prob_L1[k,])
  elecciones1[k,] <- x
}



# Simulacion de las frecuencias de voto conjuntas en las dos elecciones en cada mesa a partir
# de la matriz de probabilidades condicionales y los "resultados" en las primeras elecciones.
# Habra tantas matrices de frecuencias conjunta como mesas electorales

frec_conj <- vector("list", K)
matriz <- matrix(0, ncol = J, nrow = I)
for(k in 1:K){
  for(i in 1:I){
    for(j in 1:J){
      fila <- elecciones1[k,]
      set.seed(1)
      x <- t(rmultinom(1, fila[i], prob = cond_sim[i,]))
      matriz[i,] <- x
      frec_conj[[k]] <- matriz
    }}}

frec_conj

# Lo que obtenemos con el objeto "frec_conj" son el conjunto de frecuencias que forman las transferencias
# de voto de unas elecciones a otras. Esto en la realidad es inobservable, por lo que a partir de este
# objeto debemos extraer las frecuencias marginales de las columnas para cada mesa electoral para asi 
# obtener una matriz similar a "elecciones1" pero esta vez para las segundas elecciones, "elecciones2"

elecciones2 <- matrix(0, ncol = J, nrow = K)
for(k in 1:K){
  x <- colSums(frec_conj[[k]])
  elecciones2[k,] <- x
}

elecciones2 # Matriz que se encuentra formada por el numero de votos en cada mesa electoral a cada partido en 
# las segundas elecciones


# DATOS SIMULADOS CON LOS QUE CALCULAR LAS PROBABILIDADES CONDICIONALES
colnames(elecciones1) <- partidos_L1; colnames(elecciones2) <- partidos_L2
elecciones1;elecciones2
L1 <- elecciones1; L2 <- elecciones2



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
    # mean_diff <- mean(abs(resultado[[m]] - resultado[[m-1]])) # se usa la media
    mean_diff <- max(abs(resultado[[m]] - resultado[[m-1]])) # se usa el maximo
    if (mean_diff < 0.000001) {
      break
    }
  }
  
  # Actualizar la variable de la iteración anterior
  anterior <- C_ij_l
}
end_time = Sys.time()
end_time - start_time
length(resultado)
round(anterior,9)
rowSums(anterior)

cond_sim;round(anterior,9)
round(cond_sim - anterior,9)
max(round(cond_sim - anterior,9))

# Celdas que presentan un error mayor a 0.05:

# [4,3], error = -0.087763654. Probabilidad condicional real = 0.30
# [4,4], error = 0.12002894. Probabilidad condicional real = 0.65
# [5,4], error = 0.05031496. Probabilidad condicional real = 0.50
# [6,1], error = 0.072982828. Probabilidad condicional real = 0.15
# [6,2], error = -0.056900522. Probabilidad condicional real = 0.10
# [7,4], error = -0.07994075. Probabilidad condicional real = 0.05




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


anterior
list_vec[[1]][10000];list_vec[[2]][10000] # Ordenado por columnas
length(list_vec)
length(resultado)

# COMENTARIO: La representacion de la convergencia para este caso en concreto tendra
# la intencion de comparar la convergencia entre aquellas probabilidades condicionales
# con un valor más grande que cero (a partir de 0.15) y aquellas probabilidades 
# condicionales más proximas a 0 o que su valor sea directamente 0. A continuación,
# se procede a seleccionar 5 valores alejados de 0 y 5 valores próximos a 0.

# 5 probabilidades condicionales alejadas de 0. (>0.15)
cond_sim # Valores: [1,1] <- 0.80 | [3,3] <- 0.70 | [5,3] <- 0.45 | [7,6] <- 0.35 | [7,3] <- 0.20
cond_sim[1,1]; cond_sim[3,3]; cond_sim[5,3]; cond_sim[7,6]; cond_sim[7,3]
ubic_alejado <- c(which(cond_sim == cond_sim[1,1]),which(cond_sim == cond_sim[3,3]),which(cond_sim == cond_sim[5,3]),which(cond_sim == cond_sim[7,6]),which(cond_sim == cond_sim[7,3]))

# 5 probabilidades condicionales cercanas a 0. (>0.15)
cond_sim # Valores: [3,4] <- 0.15 | [2,1] <- 0.12 | [1,5] <- 0.05 | [1,3] <- 0.02 | [1,4] <- 0
cond_sim[3,4]; cond_sim[2,1]; cond_sim[1,5]; cond_sim[1,3]; cond_sim[1,4]
ubic_cercano <- c(which(cond_sim == cond_sim[3,4])[1],which(cond_sim == cond_sim[2,1])[1],which(cond_sim == cond_sim[1,5])[1],which(cond_sim == cond_sim[1,3])[1],which(cond_sim == cond_sim[1,4])[1])



# En segundo lugar, se procede con la creacion del grafico 
# para cada uno de los partidos

x_seq <- seq(from = 100, to = length(list_vec[[1]]), by = 100) # Secuencia de 100 en 100 para el eje x
R <- length(resultado)

## Probabilidades condicionales alejadas de 0. (>0.15)
ubic_alejado;cond_sim
# Grafico para el valor de probabilidad condicional 0.80
plot(1:R, list_vec[[1]], type = "l", col = "red", 
     ylim = c(-0.1, 1.1), xlim = c(1,R),
     xaxt = "n", lwd = 2.5, main = " Convergencia Probabilidades > 0.15", 
     ylab = "Probabilidades Condicionales", xlab = "Iteraciones")
axis(1, at = x_seq) # Introducir nombres del eje x

lines(1:R, list_vec[[17]], col = "blue", lwd = 2.5) # Representacion convergencia prob = 0.70
lines(1:R, list_vec[[19]], col = "green", lwd = 2.5) # Representacion convergencia prob = 0.45
lines(1:R, list_vec[[42]], col = "yellow", lwd = 2.5) # Representacion convergencia prob = 0.35
lines(1:R, list_vec[[21]], col = "purple", lwd = 2.5) # Representacion convergencia prob = 0.20
abline(v = 550, lty = 2, col = "grey", lwd = 2)
# Agrega leyenda
legend("topright", legend = c("C_ij = 0.80", "C_ij = 0.70", "C_ij = 0.45", "C_ij = 0.35", "C_ij = 0.20"),
       col = c("red", "blue", "green", "yellow", "purple", "black", "black"), lty = c(1,1,1,1,1,1,2), lwd = 2.5, cex = 0.65, bty = "n")

grafico_conv_grande <- recordPlot() # Guardamos el grafico
windows();grafico_conv_grande

# Grafico reducido para observar mejor las primeras iteraciones
plot(1:R, list_vec[[1]], type = "l", col = "red", 
     ylim = c(-0.1, 1.1), xlim = c(1,1000),
     xaxt = "n", lwd = 2.5, main = " Convergencia Probabilidades > 0.15", 
     ylab = "Probabilidades Condicionales", xlab = "Iteraciones")
axis(1, at = x_seq) # Introducir nombres del eje x
lines(1:R, list_vec[[17]], col = "blue", lwd = 2.5)
lines(1:R, list_vec[[19]], col = "green", lwd = 2.5)
lines(1:R, list_vec[[42]], col = "yellow", lwd = 2.5)
lines(1:R, list_vec[[21]], col = "purple", lwd = 2.5)
abline(v = 550, lty = 2, lwd = 2, col = "grey")
legend("topright", legend = c("C_ij = 0.80", "C_ij = 0.70", "C_ij = 0.45", "C_ij = 0.35", "C_ij = 0.20"),
       col = c("red", "blue", "green", "yellow", "purple"), lty = c(1,1,1,1,1,1,2), lwd = 2.5, cex = 0.65, bty = "n")

grafico_conv_grande_reducido <- recordPlot() # Guardamos el grafico
windows();grafico_conv_grande_reducido



## Probabilidades condicionales cercanas a 0. (<0.15)
ubic_cercano;cond_sim
# Grafico para el valor de probabilidad condicional 0.15
plot(1:R, list_vec[[6]], type = "l", col = "red", 
     ylim = c(-0.1, 0.5), xlim = c(1,R),
     xaxt = "n", lwd = 2.5, main = " Convergencia Probabilidades < 0.15", 
     ylab = "Probabilidades Condicionales", xlab = "Iteraciones")
axis(1, at = x_seq) # Introducir nombres del eje x

lines(1:R, list_vec[[2]], col = "blue", lwd = 2.5) # Representacion convergencia prob = 0.10
lines(1:R, list_vec[[3]], col = "green", lwd = 2.5) # Representacion convergencia prob = 0.05
lines(1:R, list_vec[[15]], col = "yellow", lwd = 2.5) # Representacion convergencia prob = 0.02
lines(1:R, list_vec[[4]], col = "purple", lwd = 2.5) # Representacion convergencia prob = 0
abline(v=2500, lty = 2, lwd = 2, col = "grey")

# Agrega leyenda
legend("topright", legend = c("C_ij = 0.15", "C_ij = 0.10", "C_ij = 0.05", "C_ij = 0.02", "C_ij = 0"),
       col = c("red", "blue", "green", "yellow", "purple", "black", "black"), lty = c(1,1,1,1,1,1,2), lwd = 2.5, cex = 0.65, bty = "n")

grafico_conv_peque <- recordPlot() # Guardamos el grafico
windows();grafico_conv_peque

# Grafico reducido para observar mejor las primeras iteraciones
plot(1:R, list_vec[[6]], type = "l", col = "red", 
     ylim = c(0, 0.4), xlim = c(1,4000),
     xaxt = "n", lwd = 2.5, main = " Convergencia Probabilidades < 0.15", 
     ylab = "Probabilidades Condicionales", xlab = "Iteraciones")
axis(1, at = x_seq) # Introducir nombres del eje x

lines(1:R, list_vec[[2]], col = "blue", lwd = 2.5) # Representacion convergencia prob = 0.10
lines(1:R, list_vec[[3]], col = "green", lwd = 2.5) # Representacion convergencia prob = 0.05
lines(1:R, list_vec[[15]], col = "yellow", lwd = 2.5) # Representacion convergencia prob = 0.02
lines(1:R, list_vec[[4]], col = "purple", lwd = 2.5) # Representacion convergencia prob = 0
abline(v=2500, lty = 2, lwd = 2, col = "grey")

# Agrega leyenda
legend("topright", legend = c("C_ij = 0.15", "C_ij = 0.10", "C_ij = 0.05", "C_ij = 0.02", "C_ij = 0"),
       col = c("red", "blue", "green", "yellow", "purple", "black", "black"), lty = c(1,1,1,1,1,1,2), lwd = 2.5, cex = 0.65, bty = "n")

grafico_conv_peque_reducido <- recordPlot() # Guardamos el grafico
windows();grafico_conv_peque_reducido

## COMPARACION DE AMBOS GRAFICOS
windows();grafico_conv_grande_reducido
windows();grafico_conv_peque_reducido 

# COMENTARIO: Se puede observar claramente que las probabilidades condicionales menores o iguales a 0.15
# tardan bastante mas en converger que las probabilidades condicionales mayores a 0.15.

# Aproximadamente, para el grafico de las probabilidades mayores a 0.15, los valores convergen entorno a  
# la iteracion 350, mientras que para el grafico con las probabilidades menores o iguales a 0.15, esto tarda
# un poco mas, sobre la iteracion 2500.














#######################################
## ESTIMACION POR MONTECARLO DEL MSE ##
#######################################

# A continuación, se va a diseñar un bucle para el ejemplo (1) 3 x 3 y  el ejemplo (2) 7 x 6, 
# en el que se simularan 100 muestras diferentes y se ejecutará la estimación de la matriz de probabilidades 100
# veces. Una vez hecho esto, de cada una de las simulaciones se calculará el Error Cuadrático
# Medio.

# Funcion para calcular el MSE

calcular_mse <- function(matriz_original, matriz_estimada) {
  diferencia <- matriz_original - matriz_estimada
  cuadrados <- diferencia^2
  mse <- mean(cuadrados)
  return(mse)
}


### EJEMPLO 1. MATRIZ 3 x 3


## SIMULACION DE  DATOS PARA J = 3, I = 3 y K = 10 
## CON PROBABILIDADES CONDICIONALES MAYORES QUE 0.15 Y 
## PROBABILIDADES EN LAS PRIMERAS ELECCIONES MAYORES TAMBIEN QUE 0.15


# PROBABILIDADES CONDICIONALES:

#               PARTIDO 1   PARTIDO 2     PARTIDO 3
# PARTIDO 1     0.65        0.15          0.20
# PARTIDO 2     0.25        0.60          0.15
# PARTIDO 3     0.30        0.20          0.50

I = 3;J = 3; K = 10
partidos_L1  <- c("PARTIDO 1", "PARTIDO 2", "PARTIDO 3")
partidos_L2  <- c("PARTIDO 1", "PARTIDO 2", "PARTIDO 3")
cond_sim <- matrix(c(0.65,     0.15,       0.20,
                     0.25,     0.60,       0.15,
                     0.30,     0.20,       0.50), ncol = 3, nrow = 3, byrow = T)

colnames(cond_sim) <- partidos_L2
rownames(cond_sim) <- partidos_L1
rowSums(cond_sim)

# Como se reparten las probabilidades por mesa electoral para las primeras elecciones.
# Numero de mesas pequeño (K = 10). 

prob_L1 <- matrix(c(0.45, 0.25, 0.30,
                    0.55, 0.30, 0.15,
                    0.53, 0.27, 0.20,
                    0.39, 0.31, 0.30,
                    0.42, 0.35, 0.23,
                    0.33, 0.40, 0.27,
                    0.38, 0.29, 0.33,
                    0.25, 0.34, 0.41,
                    0.41, 0.34, 0.25,
                    0.30, 0.39, 0.31), ncol = 3, nrow = 10, byrow = T)
rowSums(prob_L1)

# Fijar numero de electores por mesa (n..^(k))
n_mesas <- c(400, 450, 500, 550, 600, 650, 700, 750, 800, 850);n_mesas

# Simulacion de las frecuencias de voto en las primeras elecciones en cada mesa a partir
# de los vectores de probabilidad que conforman la matriz "prob_L1"

H = 1000 # Número de repeticiones
mse <- as.numeric() # Vector vacio para almacenar los mse
for (h in 1:H){
  
  set.seed(h) # Semilla para fijar la simulacion de los datos
  
  elecciones1  <- matrix(0, ncol = I, nrow = K)
  for(k in 1:K){
    x <- rmultinom(1, n_mesas[k], prob = prob_L1[k,])
    elecciones1[k,] <- x
  }
  
  elecciones1
  
  
  # Simulacion de las frecuencias de voto conjuntas en las dos elecciones en cada mesa a partir
  # de la matriz de probabilidades condicionales y los "resultados" en las primeras elecciones.
  # Habra tantas matrices de frecuencias conjunta como mesas electorales
  
  frec_conj <- vector("list", K)
  matriz <- matrix(0, ncol = J, nrow = I)
  for(k in 1:K){
    for(i in 1:I){
      for(j in 1:J){
        fila <- elecciones1[k,]
        x <- t(rmultinom(1, fila[i], prob = cond_sim[i,]))
        matriz[i,] <- x
        frec_conj[[k]] <- matriz
      }}}
  
  frec_conj
  
  # Lo que obtenemos con el objeto "frec_conj" son el conjunto de frecuencias que forman las transferencias
  # de voto de unas elecciones a otras. Esto en la realidad es inobservable, por lo que a partir de este
  # objeto debemos extraer las frecuencias marginales de las columnas para cada mesa electoral para asi 
  # obtener una matriz similar a "elecciones1" pero esta vez para las segundas elecciones, "elecciones2"
  
  elecciones2 <- matrix(0, ncol = J, nrow = K)
  for(k in 1:K){
    x <- colSums(frec_conj[[k]])
    elecciones2[k,] <- x
  }
  
  elecciones2 # Matriz que se encuentra formada por el numero de votos en cada mesa electoral a cada partido en 
  # las segundas elecciones
  
  
  # DATOS SIMULADOS CON LOS QUE CALCULAR LAS PROBABILIDADES CONDICIONALES
  colnames(elecciones1) <- partidos_L1; colnames(elecciones2) <- partidos_L2
  elecciones1;elecciones2
  L1 <- elecciones1; L2 <- elecciones2
  
  
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
      # mean_diff <- mean(abs(resultado[[m]] - resultado[[m-1]])) # se usa la media
      mean_diff <- max(abs(resultado[[m]] - resultado[[m-1]])) # se usa el maximo
      if (mean_diff < 0.0000001) {
        break
      }
    }
    
    # Actualizar la variable de la iteración anterior
    anterior <- C_ij_l
  }
  
  # Calculo del Error Cuadratico Medio
  error <- calcular_mse(cond_sim, anterior)
  mse[h] <- error
  
}

mse_ej1 <- mse
mse_sort_ej1 <- sort(mse_ej1)

head(round(mse_sort_ej1,9))

# Estadisticos descriptivos
round(summary(mse_sort_ej1),6)


# Crear grafica con la distribucion ordenada de los valores del MSE
plot(mse_sort_ej1, type = "l", xlab = "Índice", ylab = "MSE", main = "Gráfica de MSE")

# Crear histograma con los valores del MSE
hist(mse_sort_ej1, breaks = 10, xlab = "MSE", ylab = "Frecuencia", main = "Histograma de MSE",freq=FALSE, ylim = c(0,140))
lines(density(mse_sort_ej1), col = "red", lwd = 1.5)

# Estimacion de la funcion de densidad con los valores del MSE
densidad_ej1 <- density(mse_sort_ej1)
plot(densidad_ej1, main = "Función de Densidad de MSE", xlab = "MSE", ylab = "Densidad")

# COMENTARIO: A simple vista puede parecer que la función de densidad de los datos 
# equivale a una Chi-Cuadrado.

# Contraste bondad de ajuste
chisq.test(mse_sort_ej1)
shapiro.test(mse_sort_ej1)











#######################################
## ESTIMACION POR MONTECARLO DEL MSE ##
#######################################


# Funcion para calcular el MSE

calcular_mse <- function(matriz_original, matriz_estimada) {
  diferencia <- matriz_original - matriz_estimada
  cuadrados <- diferencia^2
  mse <- mean(cuadrados)
  return(mse)
}


### EJEMPLO 2

## SIMULACION DE  DATOS PARA J = 6, I = 7 y K = 10 


# PROBABILIDADES CONDICIONALES:

#         PP    VOX     PSOE     PODEM    CS      O.P.
# PP      0.80  0.10    0.02      0       0.05    0.03
# VOX     0.12  0.75     0        0.03    0.05    0.05 
# PSOE    0.05  0.05    0.70      0.15    0.03    0.02
# PODEM    0     0      0.30      0.65     0      0.05
# COMUN    0     0      0.45      0.50     0      0.05
# CS      0.15  0.10    0.05       0      0.65    0.05
# O.P.    0.15  0.10    0.20      0.05    0.15    0.35

I = 7;J = 6; K = 10
partidos_L1  <- c("PP", "VOX", "PSOE", "PODEM", "COMUN", "CS", "O.P.")
partidos_L2  <- c("PP", "VOX", "PSOE", "PODEM", "CS", "O.P.")
cond_sim <- matrix(c(0.80,  0.10,    0.02,      0,       0.05,    0.03,
                     0.12,  0.75,     0,        0.03,    0.05,    0.05,
                     0.05,  0.05,    0.70,      0.15,    0.03,    0.02,
                     0,     0,       0.30,      0.65,     0,      0.05,
                     0,     0,       0.45,      0.50,     0,      0.05,
                     0.15,  0.10,    0.05,       0,      0.65,    0.05,
                     0.15,  0.10,    0.20,      0.05,    0.15,    0.35), ncol = 6, nrow = 7, byrow = T)
colnames(cond_sim) <- partidos_L2
rownames(cond_sim) <- partidos_L1
rowSums(cond_sim)

# Como se reparten las probabilidades por mesa electoral para las primeras elecciones.
# Numero de mesas pequeño (K = 10). 

prob_L1 <- matrix(c(0.25, 0.15, 0.30, 0.10, 0.05, 0.10, 0.05,
                    0.20, 0.10, 0.35, 0.15, 0.10, 0.05, 0.05,
                    0.15, 0.05, 0.40, 0.15, 0.05, 0.15, 0.05,
                    0.20, 0.15, 0.25, 0.10, 0.05, 0.15, 0.10,
                    0.30, 0.20, 0.20, 0.10, 0.10, 0.05, 0.05,
                    0.18, 0.12, 0.33, 0.17, 0.04, 0.10, 0.06,
                    0.33, 0.17, 0.18, 0.12, 0.10, 0.06, 0.04,
                    0.30, 0.10, 0.25, 0.15, 0.10, 0.05, 0.05,
                    0.20, 0.10, 0.30, 0.10, 0.10, 0.05, 0.15,
                    0.25, 0.10, 0.30, 0.15, 0.10, 0.10, 0), ncol = 7, nrow = 10, byrow = T)
rowSums(prob_L1)

# Fijar numero de electores por mesa (n..^(k))
n_mesas <- c(400, 450, 500, 550, 600, 650, 700, 750, 800, 850);n_mesas

# Simulacion de las frecuencias de voto en las primeras elecciones en cada mesa a partir
# de los vectores de probabilidad que conforman la matriz "prob_L1"

H = 200 # Número de repeticiones
mse_ej2 <- as.numeric() # Vector vacio para almacenar los mse
for (h in 1:H){
  
  set.seed(h) # Semilla para fijar la simulacion de los datos
  
  elecciones1  <- matrix(0, ncol = I, nrow = K)
  for(k in 1:K){
    x <- rmultinom(1, n_mesas[k], prob = prob_L1[k,])
    elecciones1[k,] <- x
  }
  
  elecciones1
  
  
  # Simulacion de las frecuencias de voto conjuntas en las dos elecciones en cada mesa a partir
  # de la matriz de probabilidades condicionales y los "resultados" en las primeras elecciones.
  # Habra tantas matrices de frecuencias conjunta como mesas electorales
  
  frec_conj <- vector("list", K)
  matriz <- matrix(0, ncol = J, nrow = I)
  for(k in 1:K){
    for(i in 1:I){
      for(j in 1:J){
        fila <- elecciones1[k,]
        x <- t(rmultinom(1, fila[i], prob = cond_sim[i,]))
        matriz[i,] <- x
        frec_conj[[k]] <- matriz
      }}}
  
  frec_conj
  
  # Lo que obtenemos con el objeto "frec_conj" son el conjunto de frecuencias que forman las transferencias
  # de voto de unas elecciones a otras. Esto en la realidad es inobservable, por lo que a partir de este
  # objeto debemos extraer las frecuencias marginales de las columnas para cada mesa electoral para asi 
  # obtener una matriz similar a "elecciones1" pero esta vez para las segundas elecciones, "elecciones2"
  
  elecciones2 <- matrix(0, ncol = J, nrow = K)
  for(k in 1:K){
    x <- colSums(frec_conj[[k]])
    elecciones2[k,] <- x
  }
  
  elecciones2 # Matriz que se encuentra formada por el numero de votos en cada mesa electoral a cada partido en 
  # las segundas elecciones
  
  
  # DATOS SIMULADOS CON LOS QUE CALCULAR LAS PROBABILIDADES CONDICIONALES
  colnames(elecciones1) <- partidos_L1; colnames(elecciones2) <- partidos_L2
  elecciones1;elecciones2
  L1 <- elecciones1; L2 <- elecciones2
  
  
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
      # mean_diff <- mean(abs(resultado[[m]] - resultado[[m-1]])) # se usa la media
      mean_diff <- max(abs(resultado[[m]] - resultado[[m-1]])) # se usa el maximo
      if (mean_diff < 0.0000001) {
        break
      }
    }
    
    # Actualizar la variable de la iteración anterior
    anterior <- C_ij_l
  }
  
  # Calculo del Error Cuadratico Medio
  error <- calcular_mse(cond_sim, anterior)
  mse_ej2[h] <- error
  
}

mse_sort_ej2 <- sort(mse_ej2) # valores del MSE para el Conjunto 2
summary(mse_sort_ej2)

# Crear grafica con la distribucion ordenada de los valores del MSE
plot(mse_sort_ej2, type = "l", xlab = "Índice", ylab = "MSE", main = "Gráfica de MSE")

# Crear histograma con los valores del MSE
hist(mse_sort_ej2, breaks = 10, xlab = "MSE", ylab = "Frecuencia", main = "Histograma de MSE",freq=FALSE)
lines(density(mse_sort_ej2), col = "red", lwd = 1.5)

# Estimacion de la funcion de densidad con los valores del MSE
densidad_ej2 <- density(mse_sort_ej2)
plot(densidad_ej2, main = "Función de Densidad de MSE", xlab = "MSE", ylab = "Densidad")


