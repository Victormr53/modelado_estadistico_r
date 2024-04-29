# 1.En un ejido en Veracruz un grupo de familias ha montado un sistema de crianza de tilapias y desean
# conocer el efecto de la densidad de peces en el encierro y de la estación del año en el crecimiento de los
# individuos. Para probar el efecto de estos factores, realizan un experimento en encierros en donde colocan
# 10, 18 o 24 individuos (niveles dentro del factor densidad). Pesan 9 peces antes y después del experimento
# (marcándolos para recapturar al mismo pez) y registran el incremento en peso al cabo de dos semanas.
# Realizan estas mediciones en verano y en primavera (factor estación del año). ¿Cuál es el efecto de la época
# del año y de la densidad de peces en el encierro en el crecimiento de las tilapias? Utiliza todos los recursos
# vistos en clase para presentar los resultados de este ejercicio. NOTA: Las mediciones en cada estación vienen
# de peces distintos.


# Convertimos los datos a factores

getwd()
tilapias <- read.csv("tilapias.csv", header = TRUE)
tilapias

# Exploramos los datos

summary(tilapias)
head(tilapias)
str(tilapias)

# Convertimos a factor

tilapias$densidad <- as.factor(tilapias$densidad)
tilapias$estacion <- as.factor(tilapias$estacion)
str(tilapias)

# Realizamos un ANOVA de dos vías para poder determinar el efecto de los factores
# en el crecimiento de las tilapias

# Instalamos el paquete tidyverse
install.packages("tidyverse")

anova_tilapias <- aov(lm(aumento ~ densidad * estacion, data = tilapias))
anova_tilapias
summary(anova_tilapias)

# Los resultados mediante el ANOVA demuestran que los factores densidad y estación,
# así como en conjunto (densidad:estación) tienen un efecto altamente significativo
# en el crecimiento de las tilapias, especificamente los resultados fueron:
# densidad = F(2.48) 119.2, p< 0.001
# estación = F(1.48) 432.6, p< 0.001
# densidad:estación = F(2.48) 16.2, p< 0.001

# H0 Todas las medias son iguales y HA al menos una es diferente

# Histograma de los residuos
hist(residuals(anova_tilapias))
# Normalidad
shapiro.test(residuals(anova_tilapias)) # p-value = 0.607 confirma normalidad
# Homocedasticidad
boxplot(residuals(anova_tilapias))
# Utilizamos la prueba de Levin para evaluar si las varianzas de los grupos son iguales
library(car) # Cargamos la libreria car

# Realiza la prueba de Levene para probar la homocedasticidad
levene_tilapias <- leveneTest(residuos ~ densidad * estacion, data = tilapias)
print(levene_tilapias)

# 2.Analiza los mismos datos del ejercicio anterior pero probando el efecto de la estación y de la densidad por
# separado. ¿Qué diferencia encuentras en la interpretación de los resultados? Dado el diseño experimental
# y la pregunta central, ¿qué tipo de análisis es el más conveniente y por qué? No te olvides de revisar los
# supuestos.

t_densidad <- aov(lm(aumento ~ densidad, datos = tilapias))
summary(anova_densidad)

t_estacion <- aov(lm(aumento ~ estacion, datos = tilapias))
summary(anova_estacion)

# Los resultados para el ANOVA de densidad como de estación reflejan un efecto
# estadísticamente significativo para ambos factores con un valor de p< 0.001.

# Revisamos los supuestos de normalidad, homocedasticidad

hist(residuals(t_densidad))
hist(residuals(t_estacion))

# Normalidad
shapiro.test(residuals(t_densidad))
shapiro.test(residuals(t_estacion))

# Homocedasticidad

boxplot(residuals(t_densidad))
boxplot(residuals(t_estacion))

library(car) # Cargamos la libreria car

# Realiza la prueba de Levene para probar la homocedasticidad
levene_densidad <- leveneTest(residuos ~ densidad, data = tilapias)
print(levene_densidad)

levene_estacion <- leveneTest(residuos ~ estacion, data = tilapias)
print(levene_estacion)

# Debido a que los suspuestos del modelo con interacción fueron cumplidos de manera más satisfactoria
# a comparación de los modelos sin interacción podemos afirmar que el primer modelo es una mejor aproximación
# a lo que en realidad ocurre cuando los dos factores (densidad y estación) interactúan afectando el crecimiento
# de las tilapias. Por lo tanto, si se desea realizar una predicción sobre el crecimiento y la producción de tilapias
# aconsejamos utilizar el primer modelo con interacción por ser el más completo.

# 3.Descarga los datos toluca2.csv de la página del curso. Estos datos provienen de un estudio del maíz en
# un área periurbana. Aunque hay más variables en la base, para este ejercicio queremos construir un modelo
# que permita predecir la producción de 2009 (variable produccion_2009) a partir de las hectáreas sembradas
# (ha_maiz). ¿Qué tan bueno es este modelo para realizar predicciones? Utiliza todos los recursos vistos en
# clase para contestar. NOTA. Elimina el dato con producción de 2009 igual a cero, pues el entrevistador ha
# reportado que se trata de un error de captura.

# Cargamos nuestra base de datos

toluca <- read.csv("toluca2.csv", header = TRUE)

# Exploramos los datos de toluca
summary(toluca)
head(toluca)
str(toluca)
# Eliminamos el dato 0
toluca_sc <- subset(toluca, produccion_2009 != 0)
summary(toluca_sc)
str(toluca_sc)
toluca_sc

# Construimos un modelo que permita la predicción de la producción del 2009 (produccion_2009)
# a partir de las (ha_maiz).

pp09 <- lm(produccion_2009 ~ ha_maiz, data = toluca_sc)
summary(pp09)

# El modelo lineal refleja un intercepto de 3.408, un intervalo de confianza de 0.95
# Igualmente el modelo explica apenas el 57% de la variabilidad total y el efecto de 
# (ha_maiz) es estadísticamente significativo (p< 0.001)

# Residuals:
#  Min       1Q   Median       3Q      Max 
# -12733.2   -965.9    123.3   1026.6  16236.8 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    3.408    539.744   0.006    0.995    
# ha_maiz     1969.973    189.743  10.382   <2e-16 ***
  ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 3618 on 80 degrees of freedom
# Multiple R-squared:  0.574,	Adjusted R-squared:  0.5687 
# F-statistic: 107.8 on 1 and 80 DF,  p-value: < 2.2e-16

# Revisamos que cumpla los supuestos de normalidad y homocedasticidad

residuos <- resi(pp09)
residuos

# Gráfico QQ-plot
qqnorm(residuos)
qqline(residuos)

# Prueba de Shapiro-Wilk
shapiro.test(residuos)

# Homocedasticidad test de Breusch-Pagan
install.packages("lmtest")
library(lmtest)
bptest(pp09)

# studentized Breusch-Pagan test

# data:  pp09
# BP = 46.731, df = 1, p-value = 8.142e-12

# 4.Utilizando de nuevo los datos del estudio de maiz periurbano, realiza una comparación entre la producción
# del 2009 entre las diferentes comunidades. Sabemos que la producción está asociada con el área sembrada,
# por lo que quisiéramos tomar esto en cuenta en nuestro análisis al comparar las comunidades, i.e. queremos
# incorporar la covariable hectáreas sembradas de maíz (ha_maiz). ¿Hay diferencias en producción entre los
# sitios tomando en cuenta las hectáreas sembradas de maíz? Aunque no se detecte una diferencia significativa
# entre comunidades, ¿cuál sería la ecuación de regresión para cada comunidad (la finalidad es ejercitar el uso
# de las variables dummy)? Genera la gráfica correspondiente con tres rectas, una para cada comunidad a
# partir de las ecuaciones que generaste para cada comunidad.

# No existen diferencias en la producción entre los sitios 
(toluca_sc$comunidad)

pcomunidad <- lm(produccion_2009 ~ comunidad + ha_maiz, data = toluca_sc)
summary(pcomunidad)

# Residuals:
# Min       1Q   Median       3Q      Max 
# -12704.3  -1197.1    224.6   1079.8  16232.1 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              121.8      849.0   0.143    0.886    
# comunidadparedon         154.8     1209.1   0.128    0.898    
# comunidadsanfrancisco   -220.5     1000.5  -0.220    0.826    
# ha_maiz                 1936.4      223.6   8.659 4.96e-13 ***
  ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 3661 on 78 degrees of freedom
# Multiple R-squared:  0.5747,	Adjusted R-squared:  0.5583 
# F-statistic: 35.13 on 3 and 78 DF,  p-value: 1.799e-14

# Gráficamos
  
install.packages("ggplot2")
library(ggplot2)

ggplot(toluca_sc, aes(x = ha_maiz, y = produccion_2009)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Hectáreas de maíz", y = "Producción en 2009") +
  theme_minimal()


# 5.Entender qué aspectos del ambiente y de las actividades humanas afectan la abundancia de organismos es
# un aspecto muy importante para la conservación de áreas naturales. En 1987 se colectaron datos de aves en
# 56 parches de vegetación natural en Australia. En este estudio se registró la abundancia de aves (abundancia)
# y algunas variables que serán utilizadas como predictoras de esta abundancia, incluyendo el área del parche
# medido (area), el tiempo en años en que dicho parche ha quedado aislado del resto de la vegetación natural
# (anos.aislam), la distancia al parche de vegetación más cercano (dist), la distancia al parche más grande
# de vegetación en el área (dist.parche.grande), la cantidad de ganado presente en el parche (ganado, medido
# de 1 a 5 donde 1 es poco ganado y 5 es abundante ganado), y la altitud. Se tienen en total seis variables
# predictoras. OJO: Revisa que exista una relación lineal entre la variable de respuesta y las predictoras. Es
# posible que algunas requieran una transformación. Realiza lo siguiente:

aves <- read.csv("AbundanciaAves.csv", header = TRUE)
aves

# Exploramos los datos

summary(aves)
head(aves)
str(aves)

# Revisamos si existe una relación lineal entre la variable de respuesta y las predictoras
pairs(aves, pch = 18, col = "steelblue")
# Transformamos a log
aves_log <- log(aves)
pairs(aves_log, pch = 18, col = "steelblue")

# 5.1.Ajusta un modelo que permita predecir la abundancia de aves a partir de todas las seis variables predictoras.
# Interpreta los resultados (ajuste, pendientes, etc.)

aa <- lm(abund ~ area + anos.aislam + dist + dist.parche.grande + ganado + altitud, data = aves_log)
summary(aa)
plot(aa)

# Los resultados nos indican que los factores área y años de aislamiento son los más importantes
# para explicar la abundancia en el modelo ya que tienen un valo de p< 0.001 y p< 0.01 respectivamente.
# El intercepto del modelo es igualmente estadísticamente significativo (p< 0.01).
# El modelo solo explica el 58.03% de la variación. 

#5.2.Utiliza un procedimiento de eliminación backward comenzando con el modelo saturado (con las seis
# variables) para proponer un modelo múltiple que prediga la abundancia a partir de un subconjunto de las
# seis variables predictoras

aa_finalb <- step(aa, direction = "backward", trace = FALSE)
summary(aa_finalb)

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -187.86237   46.36228  -4.052 0.000170 ***
#  area           0.25736    0.04352   5.913 2.64e-07 ***
#  anos.aislam   24.78135    6.15219   4.028 0.000184 ***
#  altitud        0.46093    0.25145   1.833 0.072518 .  

# 5.3.Utiliza un procedimiento de selección forward comenzando con un modelo que solamente contenga al
# intercepto para proponer un modelo múltiple que prediga la abundancia a partir de un subconjunto de las
# seis variables predictoras

aa_finalf <- step(aa, direction = "forward", trace = FALSE)
summary(aa_finalf)

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -1.774e+02  5.629e+01  -3.152  0.00277 ** 
#  area                2.504e-01  5.832e-02   4.294 8.26e-05 ***
#   anos.aislam         2.344e+01  7.392e+00   3.171  0.00262 ** 
#   dist               -8.242e-03  1.084e-01  -0.076  0.93969    
# dist.parche.grande -4.226e-03  8.442e-02  -0.050  0.96028    
# ganado             -7.086e-02  1.956e-01  -0.362  0.71864    
# altitud             4.295e-01  2.948e-01   1.457  0.15144    
---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  # 5.4.Utiliza el procedimiento de selección de modelos basado en el AIC para seleccionar un grupo de modelos
# que predigan la abundancia a partir de un subconjunto de las variables predictoras. Puedes acudir a Burnham
# et al. 2011 y a Symonds & Moussalli 2011 y al ejercicio que realizamos en clase para revisar de nuevo esta
# aproximación. Para hacer el ejercicio más sencillo (es decir, para reducir el número de modelos a explorar),
# utiliza solamente el siguiente conjunto de cuatro variables predictoras en lugar de las seis de los modelos
# de los incisos previos: área, ganado, distancia al parche más grande y altitud. Explora todos los modelos
# posibles para estas cuatro variables y realiza las comparaciones que sean necesarias a través del valor de AIC
# de los modelos. ¿Cuál sería el conjunto que propondrías como buenos modelos? ¿Cuál es la importancia
# relativa de las diferentes variables tomando en cuenta su aparición en los diferentes modelos candidatos?

m1 <- lm(abund ~ area, data = aves_log)
m2 <- lm(abund ~ area + dist.parche.grande, data = aves_log)
m3 <- lm(abund ~ area + dist.parche.grande + ganado, data = aves_log)
m4 <- lm(abund ~ area + dist.parche.grande + ganado + altitud, data = aves_log)
m5 <- lm(abund ~ area + ganado + altitud, data = aves_log)
m6 <- lm(abund ~ dist.parche.grande + ganado + altitud, data = aves_log)
m7 <- lm(abund ~ ganado + altitud, data = aves_log)
m8 <- lm(abund ~ altitud, data = aves_log)
m9 <- lm(abund ~ ganado, data = aves_log)
m10 <- lm(abund ~ dist.parche.grande, data = aves_log)
m11 <- lm(abund ~ area + ganado, data = aves_log)
m12 <- lm(abund ~ area + altitud, data = aves_log)
m13 <- lm(abund ~ area + dist.parche.grande, data = aves_log)
m14 <- lm(abund ~ dist.parche.grande + ganado, data = aves_log)

AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)
AIC(m5)
AIC(m6)
AIC(m7)
AIC(m8)
AIC(m9)
AIC(m10)
AIC(m11)
AIC(m12)
AIC(m13)
AIC(m14)

# m5 <- lm(abund ~ area + ganado + altitud, data = aves_log) # > AIC(m5) [1] 112.7205

# 5.5.Compara los resultados obtenidos con los diferentes métodos de selección de modelos. ¿A qué conclusión
# llegamos que conteste la pregunta principal de investigación?

# Es importante área, el ganado y la altitud ya que son tres variables que explican mejor la abundancia de aves.