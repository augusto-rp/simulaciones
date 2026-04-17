library(tidyr)
library(tidyverse)
library(MASS)
library(dplyr)



set.seed(1289)


# 1. Crear la función COMPLETA que genera valores correlacionados
# (todos los cálculos deben estar DENTRO de la función)
ao_cor <- function(correlation_range = c(0.5, 0.8)) {
  # Valores posibles de -2 a 7 en pasos de 0.5
  posibles_valores <- seq(from = -2, to = 7, by = 0.5)
  n_posibles <- length(posibles_valores)  # 19 valores posibles
  
  # Seleccionar correlación aleatoria para este individuo
  rho <- runif(1, min = correlation_range[1], max = correlation_range[2])
  
  # Crear matriz de correlación para 4 mediciones
  cor_matrix <- matrix(rho, nrow = 4, ncol = 4)
  diag(cor_matrix) <- 1  # La diagonal debe ser 1
  
  # Generar datos normales multivariados con la correlación especificada
  datos_latentes <- mvrnorm(n = 1, mu = rep(0, 4), Sigma = cor_matrix)
  
  # Convertir a cuantiles (0-1) usando la CDF normal estándar
  cuantiles <- pnorm(datos_latentes)
  
  # Convertir cuantiles a índices en el vector de valores posibles
  indices <- ceiling(cuantiles * n_posibles)
  indices <- pmax(1, pmin(indices, n_posibles))  # Asegurar que estén dentro del rango
  
  # Devolver los valores ao correspondientes
  return(posibles_valores[indices])
}

# 2. Crear el data frame base de individuos
n_individuos <- 300
n_mujeres <- 150
n_hombres <- 150

individuos <- data.frame(
  id = 1:n_individuos,
  sexo = c(rep("Mujer", n_mujeres), rep("Hombre", n_hombres)),
  edad = sample(18:65, n_individuos, replace = TRUE),
  claves = sample(c("H", "NH"), n_individuos, replace = TRUE)
)

# 3. Generar valores correlacionados para cada individuo
database_long <- individuos %>%
  rowwise() %>%
  mutate(
    ao_values = list(ao_cor())  # Genera 4 valores correlacionados
  ) %>%
  ungroup() %>%
  # Expandir a formato largo (4 filas por individuo)
  unnest_wider(ao_values, names_sep = "_") %>%
  pivot_longer(cols = starts_with("ao_values_"),
               names_to = "medicion",
               values_to = "valor") %>%
  mutate(medicion = paste0("ao_", parse_number(medicion))) %>%
  arrange(id, medicion)


database_long$sexo[database_long$sexo == "Mujer"] <- "m"
database_long$sexo[database_long$sexo == "Hombre"] <- "h"

#Me molesta el nombre largo
df<- database_long
rm(database_long)

# ESTIMACION DE MODELO ----------------------------------------------------


options(scipen=0, digits=3)
#eliminar notacion cientifica (scipen=100)

#USANDO ezANOVA
library(ez)
library (psych)

#Exploracion de descriptivos
describe(df_larga)
str(df_larga)

#describir promedios de ao_value de acuerdo a niveles de group_topic

dif_promedios <- df_larga |>
  group_by(group_topic) |>
  summarise(
    mean_ao_value = mean(ao_value, na.rm = TRUE)
  )
print(dif_promedios )


#########Examinacion de criterios ANOVA
#Exploracion de mauchly sphericity test



###############ESTIMACION DE MODELO USANDO ezANOVA
#definir contrastes
c_humor<-c(1, -1) #contraste de humor vrs no humor
c_topico<-c(1, -1) #contraste de topico politico vrs no politico

contrasts(df_larga$group)<-cbind(c_humor)
contrasts(df_larga$topic)<-cbind(c_topico)

#hacer anova
modelo_anova <- ezANOVA(
  data = df_larga,
  dv = ao_value,
  wid = id,
  within = .(topic),
  between = .(group),
  type = 3,
  detailed = TRUE
)


modelo_anova

#como es de esperar nada es significativa because why would it be

pi


##########  USANDO GLM
#recordar que parte repetida topic se especifica en parte aleatoria
library(lme4)
library(reghelper)

#creamos un modelo base usando solo efecto aleatorio

#usando |id que mide variacion entre individuos
base <- lmer(ao_value ~ 1 + (1 |id),
             data = df_larga,
             REML = TRUE)

base
summary(base)

#ICC = var(intercepto sujeto)/(var(intercepto sujeto)+var(residual))
icc <- as.numeric(VarCorr(base)$id[1]) / (as.numeric(VarCorr(base)$id[1]) + attr(VarCorr(base), "sc")^2)
icc #0.649 es decir 64% varianza es explica por diferencias entre individuos, baselines muy disstintos individuales
reghelper::ICC(base) #MAS SENCILLO

#usando |id/topic que mide variacion entre individuos ESTE PASO NO ES NECESARIO
base_2 <- lmer(ao_value ~ 1 +(1 |topic),
               data = df_larga,
               REML = TRUE)
summary(base_2)
#inncesario agregar topic
#una vez que se considera individuos hay 0 varianza en topic


# y si quiero decir que efecto aleatorio de individuo puede variar e funcion de pol y fun_value
#base_3 <- lmer(ao_value ~ 1 + (1+fun_value |id),
#               data = df_larga,
#              REML = TRUE)  #numero de observaciones=a numero de parametros lo que hace inidentificable este modelo
#For every single observation, the model is trying to estimate two unique parameters associated with that individual.
#This leaves zero degrees of freedom for the model to estimate the residual variance—the error that is not explained by the fixed or random effects.


#Why This is Statistically Defensible:

# Brauer explicitly addresses this case: When you have only one observation per cell, random slopes and residual error become confounded, and the practical solution is to use only random intercepts.

#You're still controlling for the main source of non-independence: The random intercept (1 | id) accounts for the fact that multiple observations come from the same person.

#  Type I error control: While not "maximal", a random intercept model is far better than completely ignoring the non-independence (which would give you massively inflated Type I error rates).
#ver ds log del 11-11

#ahora agregar al modelo predictor fijo group
modelo1<-update(base, . ~ . + group)
summary(modelo1)

#y ahora agrega a modelo1 predictor pol
modelo2<-update(modelo1, . ~ . + pol)
summary(modelo2)

modelo3<-update(modelo2,.~.+topic)
summary(modelo3)

#y ahora comparar los modelos}

anova(base, modelo1, modelo2, modelo3)

model_recommended <- lmer(ao_value ~ topic * group + pol + (1 | id),
                          data = df_larga,
                          REML=T)
summary(model_full)




###################################3
###############################IGNORAR ESTO, nada de esto importa
library(dplyr)
library(purrr)
library(tidyverse)

set.seed(542)


#Parametros
n_individuos<-180
n_observaciones<-6 #observaciones por individuos
ambivalencia_min<- -2 
ambivalencia_max<- 7  #valores minimos y máximos de ambivalencia

#Datos a niviel individual, creaciond e df solo con individuos


claves_vector <- c(rep("H", 90), rep("NH", 90))
claves_shuffled <- sample(claves_vector, length(claves_vector), replace = FALSE)


datos_individuo<-tibble(
  id=1:n_individuos,
  claves=claves_shuffled,  #asignacion a condicion
  genero=sample(1:2, n_individuos, replace=TRUE), #asignacion de genero
  id_politica=round(rnorm(n_individuos, mean=4, sd=1.5)),  #asignacion de ideologia politica con distribucion normal
  extroversion=round(runif(n_individuos, 1, 7), digits = 1),
  apertura=round(runif(n_individuos, 1, 7), digits = 1),
  escrupulo=round(runif(n_individuos, 1, 7), digits = 1) #asignacion de rasgos redondeados
)



#Ahora generar datos de observaiones

df_simulada <- datos_individuo %>%
  uncount(n_observaciones) %>%
  group_by(id) %>%
  mutate(
    row_num = row_number(),
    # Asignar 3 Np y 3 P por indivudo
    contenido = rep(c("NP", "P"), each = 3)
  ) %>%
  ungroup() %>%
  arrange(id, contenido) %>%
  group_by(id, contenido) %>%
  mutate(
    # Asignar id des estimulo a tipo de contenido
    id_estimulo = row_number(),
    # Generar valores de ambivalencai simialres por grupo
    ambivalencia_base = ambivalencia_min + (ambivalencia_max - ambivalencia_min) * runif(1, 0, 1),
    ambivalencia = pmin(ambivalencia_max, pmax(ambivalencia_min, ambivalencia_base + rnorm(1, 0, 0.5))),
    ambivalencia = round(ambivalencia * 2) / 2
  ) %>%
  ungroup() %>%
  # Reordenar columnas
  select(
    id,
    claves,
    contenido,
    id_estimulo,
    ambivalencia,
    genero,
    id_politica,
    extroversion,
    apertura,
    escrupulo
  )



##############33ANALISIS

library(ez)
library(psych)
library(emmeans)

#Primero anova mixto

str(df_simulada)
table(df_simulada$claves, df_simulada$contenido)

df_simulada$id<-as.factor(df_simulada$id)
df_simulada$claves<-as.factor(df_simulada$claves)
df_simulada$contenido<-as.factor(df_simulada$contenido)
df_simulada$genero<-as.factor(df_simulada$genero)

aov_mixto<-ezANOVA(data=df_simulada,
                   dv=.(ambivalencia),
                   wid=.(id),
                   within =(contenido),
                   between = (claves),
                   detailed=TRUE)
aov_mixto


####Metodo de fields

library(pastecs)

#explorar datos

#describir valoers de ambivalencai segun niveles de claves y contenido
describeBy(df_simulada$ambivalencia, list(df_simulada$claves, df_simulada$contenido))

#no hay que definir contrastes pq son solo dos niveles pro factor

#CALCULO CON MULTINIVEL
library(nlme)

base<-lme(ambivalencia~1, random = ~1|id/contenido, data=df_simulada, method="ML") #modelo, el random effect no se modifica en updates psoterioers, solo los predictores
base  #aca solo s epredice valroes de ambivalencian con el gran promedio, cualquier variacion es solo por efectos aleatorios, es 


modelo1<-update(base, .~. + claves)
modelo1 #esto es igual a escribir lme(ambivalencia~claves, random = ~1|id/contenido, data=df_simulada, method="ML")
#este modelo asume que los promedios de ambivalencia varian tambien entre los valores de claves, cotnrolando por variabielida aleatoria

modelo2<-lme(ambivalencia~contenido, random = ~1|id/contenido, data=df_simulada, method="ML")
#este modelo se pregunta por como varian los nivieles indiviaules DENTRO del mismo individuo-->¿VALOR DE AMBIVALENCIA DEPENDE DE SI VEN NP O P?


modelo3<-update(modelo2, .~. *claves)#ambivalencia~contenido_claves. en ese
anova(base, modelo2, modelo3) 

modelo3
