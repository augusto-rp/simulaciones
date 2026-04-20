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


#Crear valores del numero de estimulo
levels_p<-c("p1", "p2", "p3", "p4")
pairs <- expand.grid(ao_1_stim = levels_p, ao_2_stim = levels_p) %>%
  filter(ao_1_stim != ao_2_stim)

##This function generates every possible combination between two (or more) vectors. Since you passed levels_p (which is $p1, p2, p3, p4$) twice, it creates a grid of $4 \times 4 = 16$ rows.
##This is the "Constraint Layer." It looks at that grid and removes the "diagonal"—the cases where the stimulus for $ao\_1$ is the same as $ao\_2$.

set.seed(532) 
balanced_assignments <- pairs %>%
  slice(rep(1:n(), each = 25)) %>%
  sample_frac(1) %>%
  mutate(id = 1:300)

df_f<- df %>%
  left_join(balanced_assignments, by = "id") %>%
  mutate(
    estimulo = case_when(
      medicion == "ao_1" ~ as.character(ao_1_stim),
      medicion == "ao_2" ~ as.character(ao_2_stim),
      medicion == "ao_3" ~ "np1",
      medicion == "ao_4" ~ "np2",
      TRUE ~ NA_character_
    )
  ) %>%
  # limpia tabla
  dplyr::select(-ao_1_stim, -ao_2_stim)

# 3. Verification
# Check counts for ao_1 and ao_2
df_f %>% 
  filter(medicion %in% c("ao_1", "ao_2")) %>% 
  group_by(medicion, estimulo) %>% 
  tally()
#Todo en orden


df<- df_f
rm(df_f, individuos, pairs, balanced_assignments)


#Ahora agregar otras covariables

set.seed(532)
individual_data <- data.frame(id = 1:300) %>%
  mutate(
    # Distribuion normal
    id_pol = round(rnorm(300, mean = 3, sd = 1)),
    id_pol = pmin(pmax(id_pol, 1), 5), # Ensures they stay in 1:5
    
    interes_pol = round(rnorm(300, mean = 3, sd = 1)),
    interes_pol = pmin(pmax(interes_pol, 1), 5),
    
    # Asegurarse que sean numeros enteros
    relevancia_1 = sample(1:4, 300, replace = TRUE),
    relevancia_2 = sample(1:4, 300, replace = TRUE),
    relevancia_3 = sample(1:4, 300, replace = TRUE),
    relevancia_4 = sample(1:4, 300, replace = TRUE)
  )

# Volver a unir
df <- df %>%
  left_join(individual_data, by = "id")


### Crear variable de cinismo
set.seed(532)

cinismo_data <- data.frame(id = 1:300) %>%
  mutate(
    # Create a "Latent Trait" (the individual's average cynicism level)
    # Using 1-5 distribution to keep it within scale bounds
    trait_base = runif(300, 1, 5),
    
    # Generate 4 measurements by adding a small amount of noise to the base trait
    # A smaller 'sd' in rnorm here results in a higher correlation
    cinismo_1 = round(trait_base + rnorm(300, mean = 0, sd = 0.5)),
    cinismo_2 = round(trait_base + rnorm(300, mean = 0, sd = 0.5)),
    cinismo_3 = round(trait_base + rnorm(300, mean = 0, sd = 0.5)),
    cinismo_4 = round(trait_base + rnorm(300, mean = 0, sd = 0.5))
  ) %>%
  # Clamp the results to ensure they stay strictly within 1-5
  mutate(across(starts_with("cinismo"), ~ pmin(pmax(.x, 1), 5))) %>%
  dplyr::select(-trait_base) # Remove the helper column

# 2. Join to your main dataframe
df <- df %>%
  left_join(cinismo_data, by = "id")

# 3. Verify the Correlation
# This should now show values in the 0.6 - 0.9 range
cor(cinismo_data[,-1])


#Finalmente agregar personalidad

set.seed(532)

perso_data <- data.frame(id = 1:300) %>%
  mutate(
    # Creo rasgo latente (valores promedio)
    # Usando rango de valores posible
    trait_base_p = runif(300, 1, 5),
    
    # Generate 4 measurements by adding a small amount of noise to the base trait
    # A smaller 'sd' in rnorm here results in a higher correlation
    per_1 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_2 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_3 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_4 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_5 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_6 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_7 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_8 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_9 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_10 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_11 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_12 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5)),
    per_13 = round(trait_base_p + rnorm(300, mean = 0, sd = 0.5))
  ) %>%
  # Clamp the results to ensure they stay strictly within 1-5
  mutate(across(starts_with("per"), ~ pmin(pmax(.x, 1), 5))) %>%
  dplyr::select(-trait_base_p) # Remove the helper column

# 2. Join to your main dataframe
df <- df %>%
  left_join(perso_data, by = "id")

# 3. Verify the Correlation
# This should now show values in the 0.6 - 0.9 range
cor(perso_data[,-1])

rm(cinismo_data, individual_data, perso_data)


##################CALCULO DE PODER ESTADISTICO##################
library(Superpower)

mu_ordinal <- c(0.484, 0.121,  # Grupo 1 entre condiciones :efecto de interaccion [primer nivel de primer factor, primer nivel segundo factor] 
                0.121, 0)      # Grupo 2 entre condiciones :sin interaccion, o mas leve que en grupo 1   [segundo nivel del primer factor, segundo nivel del segundo factor]  

#Definir diseño anona
diseno <- ANOVA_design(design = "2b*2w",
                           n = 300, # Calculo inicial, va a ir variando despues
                           mu = mu_ordinal,
                           sd = 1, #Asume homogeneidad de varianza, en LMM esto no se cumple. Pero esto es un calculo mas limitado pues aun no hay piloto
                           r = 0.5, #se pueden ver mas correlaciones, ver documentacion pero vamos a mantener solo una
                           labelnames = c("claves", "humor", "nohumor",
                                          "tema", "politico", "nopolitico"),
                           plot = TRUE)

#Sobre correlaciones The number of possible comparisons is the product of the levels of all factors squared minus the product of all factors, divided by two. 
#For a 2x2 design where each factor has two levels, this is: (((2*2)^2)-(2*2))/2

#Hay dos formas de calcular el poder usando esta libreria 
#ANOVA_power que toma modelo anterior y hace nsimulaciones
#ANOVA_exact que permite hacer estimaciones en base a un dataset :usar este más adelante

#En cualquiera de estos casos tendria que ir probando una y otra vez distintos N hasta encontrar el valor necesario.
#Peeeero tambien puedo plotear el poder y ver como varia

poder_resultados <- plot_power(diseno,
                            min_n = 150,
                            max_n = 500,
                            desired_power = 80,
                            exact = TRUE, 
                            plot = TRUE)










 ###### 
# ESTIMACION DE MODELO ----------------------------------------------------


options(scipen=0, digits=3)
#eliminar notacion cientifica (scipen=100)

#USANDO ezANOVA
library(ez)
library (psych)

#Exploracion de descriptivos
describe(df)
str(df)

#describir promedios de ao_value de acuerdo a niveles de group_topic


dif_promedios <- df |>
  # 1. Create a new factor column based on the 'estimulos' values
  mutate(factor_type = case_when(
    estimulo %in% c("p1", "p2", "p3", "p4") ~ "p",
    estimulo %in% c("np1", "np2")           ~ "np",
    TRUE                                     ~ "other" # Safety net
  )) |>
  # 2. Group by both the Participant and the new Factor
  group_by(id, factor_type) |> 
  # 3. Calculate the mean for those 2 measurements per person
  summarise(
    mean_ao_value = mean(valor, na.rm = TRUE),
    .groups = "drop"
  )

print(dif_promedios)


df <- df |>
  # Create the factor first
  mutate(contenido_tipo = if_else(estimulo %in% c("p1", "p2", "p3", "p4"), "p", "np")) |>
  # Group by ID and the NEW factor
  group_by(id, contenido_tipo) |>
  # Mutate adds the mean to every row without collapsing the df
  mutate(promedio_ao = mean(valor, na.rm = TRUE)) |>
  ungroup()

head(df)

#mover estas variables creadas mas cerca de inicio
df <- df |>
  relocate(31, .before = 8) |>
  relocate(32, .before = 9)

rm(dif_promedios)

#reordenar otras covariables
#df <- df |>
 # relocate(31:35, .before = 6)


#########CALCULO DE PODER ESTADISTICO




















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
