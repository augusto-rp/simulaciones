#comectar a git
usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::git_sitrep()
usethis::use_git()


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
