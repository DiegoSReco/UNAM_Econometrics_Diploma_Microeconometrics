########################################
#### Script: Modelo Tobit y Heckman #### 
########################################

#Librerías ----
if (!require(pacman, quietly = TRUE)) {install.packages('pacman')}
library(pacman)
p_load( 'wooldridge','tidyverse','modelsummary','skimr', 'dplyr', 'gtsummary')

#Cargamos datos 
data("mroz")
#Descripción
skim(mroz)
#Renombramos DF
df_mroz <- mroz  

#Distribución de variable de interés: horas anuales laboradas --
--
#Plot 1
plot_1 <- ggplot(df_mroz, aes(x = hours)) +
  geom_histogram(alpha = 0.6) +
  labs(
    title = "Distribución de las horas anuales de trabajo",
    x = "Horas",
    y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_1

# Modelo Tobit ----
#Cargamos censReg
p_load(censReg)

#Planteamiento de la estimación con Censreg 
modelo_tobit <-  censReg(hours ~ age + educ + exper + I(exper^2) + nwifeinc + kidslt6 + kidsge6, #especificación
                         left = 0, #Censura por la izquierda en este caso
                         data = mroz) #data set
summary(modelo_tobit)

# Modelo OLS ---- 
modelo_Olst <-  lm(hours ~ age + educ + exper + I(exper^2) + nwifeinc + kidslt6 + kidsge6, 
                   data = mroz) #data set

summary(modelo_Olst)

#Comparación de modelos  (OLS vs Tobit) ----
p_load("modelsummary")

modelsummary(
             list("OLS" = modelo_Olst, "Tobit" = modelo_tobit),
             stars = TRUE,
             statistic = "std.error",  # o "conf.int", "statistic"
             gof_omit = "IC|Log.Lik|F|R2"  # Opcional: para ocultar algunas estadísticas
            )


# Model Heckman o Heckit ----- 
#Carga sample selection que tiene la función selection para realizar Heckit 
p_load(sampleSelection)

# Estimación modelo Heckit ----
model_heckman_ex <- selection(selection = inlf ~ age + kidslt6 + kidsge6 + nwifeinc + educ + exper + I(exper^2),
                               outcome   = log(wage) ~ educ + exper + I(exper^2),
                               data = mroz,
                               method = "2step")
summary(model_heckman_ex)

#Estimación por OLS ----

ols_heck_modelc <- lm(log(wage) ~ educ + exper + I(exper^2), data = subset(mroz, inlf == 1))

summary(ols_heck_model)


# Comparacióm OLS vs Heckit 
modelsummary(list("OLS" = ols_heck_model, "Heckman (2step)" = model_heckman_ex))


# Estimación modelo Heckit por máxima verosimilitud ----

model_mle <- selection(
  selection = inlf ~ age + kidslt6 + kidsge6 + nwifeinc + educ + exper + I(exper^2),
  outcome = log(wage) ~ educ + exper + I(exper^2),
  data = mroz,
  method = "ml"
)
summary(model_mle)


#NOTAS ----
#1. El Heckit OLS (dos etapas) No proporciona  estimaciones de errores estándar para sigma y rho, lo que limita hacer inferencia formal sobre estos parámetros.
#2. Notar que en la estimación no se incluye explicitamente el inverso de Mills
#El modelo se estima de forma conjunta, es decir, la ecuación de selección y la de resultado se ajustan al mismo tiempo.
#El  efecto del inverso de Mills se incorpora directamente en la función de verosimilitud.
# En la estimación por máxima verosimilitud  sí incluye errores estándar para rho y sigma,
#porque todos los parámetros se estiman de forma conjunta a través de la maximización de la función de verosimilitud.





