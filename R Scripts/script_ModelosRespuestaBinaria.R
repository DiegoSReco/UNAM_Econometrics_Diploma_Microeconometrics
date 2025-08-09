######################################
#### Modelos de Respuesta Binaria #### 
######################################

#### Estimación de Modelo de Probabilidad Lineal en R (Manual) ----
## En R podremos realizar de forma "manual" o utilizando la funcion lm():

# Variables
x <- c(2, 3, 5, 4, 7, 8, 9, 6, 10, 11)
y <- c(0, 0, 1, 0, 1, 1, 1, 0, 1, 1)

# Cálculo matricial
X <- cbind(1, x)
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
print(beta_hat)

# Modelo lineal utilizando formula lm para comprobar
modelo_lpm <- lm(y ~ x)
print( modelo_lpm)


#Librerias
if (!require(pacman, quietly = TRUE)) {install.packages('pacman')}
library(pacman)
p_load( 'wooldridge','tidyverse','modelsummary','skimr', 'dplyr', 'gtsummary')

#Cargamos datos 
data("mroz")
#Descripción
skim(mroz)



#### Exploración del conjunto de datos ----
#Etiquetar 
df_mroz <- mroz  |> 
  mutate(inlf_1= factor(inlf, labels = c("No trabaja", "Trabaja"))) 

#Plot 1 
ggplot(df_mroz, aes(x = factor(inlf_1), fill = factor(inlf_1))) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', 
            aes(label = paste0(after_stat(count), "\n(", 
                               round(after_stat(count)/sum(after_stat(count))*100, 1), "%)")), 
            vjust = 1.5, size = 4) +
  
  labs(
    title = "Estado laboral de las mujeres en la muestra",
    x = "Estado laboral", 
    y = "Número de mujeres",
    caption = "inlf_1: 1 = trabaja fuera del hogar, 0 = no trabaja"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  


#Plot 2 
ggplot(df_mroz, aes(x = factor(inlf_1), fill = factor(inlf_1))) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', 
            aes(label = paste0(after_stat(count), "\n(", 
                               round(after_stat(count)/sum(after_stat(count))*100, 1), "%)")), 
            vjust = 1.5, size = 7) +
  
  labs(
    title = "Estado laboral de las mujeres en la muestra",
    x = "Estado laboral", 
    y = "Número de mujeres",
    caption = "inlf_1: 1 = trabaja fuera del hogar, 0 = no trabaja"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 24)) 

#Creamos dataframe para boxplots 

df_boxplots <- df_mroz |>  
  select(inlf_1, nwifeinc, age, educ, exper, kidslt6, kidsge6) |> 
  pivot_longer(cols = -inlf_1, names_to = "variable", values_to = "valor") |> 
  mutate(inlf_1 = factor(inlf_1, labels = c("No trabaja", "Trabaja")))

#Plot 3
ggplot(df_boxplots,
       aes(x = inlf_1, y = valor, fill = inlf_1)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  labs(title = "Distribución de variables por estado laboral",
       x = "Estado laboral",
       y = "Valor") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


#Plot 4
ggplot(df_mroz, aes(x = nwifeinc, fill = factor(inlf_1, labels = c("No trabaja", "Trabaja")))) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Distribución del ingreso no generado por la esposa",
    subtitle = "Por estado laboral",
    x = "Ingreso no-esposa (miles de dólares)",
    y = "Densidad",
    fill = "Estado laboral"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#Prueba t
t.test(nwifeinc ~ inlf_1, data = df_mroz, var.equal = TRUE)  


#### Ejemplo Modelo de Probabilidad Lineal  ----

modelo_mpl <- lm(inlf ~ age + educ + exper + I(exper^2) + nwifeinc + kidslt6 + kidsge6, data = mroz)


summary(modelo_mpl)


###### Predicciones fuera del rango 0 y 1----

preds <- predict(modelo_mpl)

out_range <- mean(preds < 0 | preds > 1)

print(out_range*100)


#La predicción de probabilidades del modelo se observa que 
#aproximadamente 4.4% de las predicciones del MPL están fuera del 
#intervalo válido es decir, son menores a 0 o mayores a 1.


###### Residuos vs. valores ajustados  ---

plot(modelo_mpl, which = 1, main = "Residuos vs Valores ajustados")


###### Gráfico Q-Q (cuantil-cuantil) ----


plot(modelo_mpl, which = 2, main = "Gráfico Q-Q de los residuos")



     
###### Colinealidad – VIF (Variance Inflation Factor) ----

p_load('car')

vif( modelo_mpl)



###### MPL con variable explicativa binaria  ---

modelo_mpl_bi <- lm(inlf ~ age + educ + exper + I(exper^2) + nwifeinc + kidslt6 + kidsge6 + city, data = mroz)


summary(modelo_mpl_bi)




#### Modelo PROBIT ----


# Modelo Probit con glm()
modelo_probit <- glm(inlf ~ age + educ + exper + I(exper^2) + nwifeinc + kidslt6 + kidsge6,
                     data = mroz,
                     family = binomial(link = "probit"))                    

# Resumen del modelo
summary(modelo_probit)

###### Bondad de ajuste ---- 

#Accuracy 
# Predicción de probabilidades
p_hat <- predict(modelo_probit, type = "response")

# Clasificación binaria con umbral 0.5
y_pred <- ifelse(p_hat > 0.5, 1, 0)

# Variable observada
y_obs <- mroz$inlf

# Accuracy
accuracy <- mean(y_pred == y_obs)
print(paste("Accuracy:", round(accuracy, 4)))



# Pseudo R^2
#Instalamos paquetería
p_load(pscl)

#Computo Pseudo R
pR2(modelo_probit)["McFadden"]

###### Cálculo de efectos marginales ----- 
#Cargamos paquetería
p_load(margins)

#Calculamos
efectos_marginales <- margins(modelo_probit)

summary(efectos_marginales)


#### Modelo LOGIT ----- 


# Modelo Logit con glm()
modelo_logit <- glm(inlf ~ age + educ + exper + I(exper^2) + 
                      nwifeinc + kidslt6 + kidsge6,
                    data = mroz,
                    family = binomial(link = "logit"))
#summary
summary(modelo_logit)



##### Bondad de ajuste ----
#Accuracy 

# Predicción de probabilidades
p_hat <- predict(modelo_logit, type = "response")

# Clasificación binaria con umbral 0.5
y_pred <- ifelse(p_hat > 0.5, 1, 0)

# Variable observada
y_obs <- mroz$inlf

# Accuracy
accuracy <- mean(y_pred == y_obs)
print(paste("Accuracy:", round(accuracy, 4)))

#Pseudo R^2
#Instalamos paquetería
p_load(pscl)


pR2(modelo_logit)["McFadden"]

#### Cálculo de Efectos Marginales ----

efectos_marginales_logit <- margins(modelo_logit)

summary(efectos_marginales_logit)

#### Gráfico Modelos MPL, PROBIT y LOGIT

set.seed(123)
n <- 200
x <- seq(-3, 3, length.out = n)
# Variable latente con función logística
prob_true <- 1 / (1 + exp(- (0.5 + 1.5 * x)))
y <- rbinom(n, 1, prob_true)
data <- data.frame(x = x, y = y)

# Ajustar MPL (modelo de probabilidad lineal)
mpl <- lm(y ~ x, data = data)
data$mpl_fit <- predict(mpl, newdata = data)

# Ajustar Probit
probit <- glm(y ~ x, data = data, family = binomial(link = "probit"))
data$probit_fit <- predict(probit, newdata = data, type = "response")

# Ajustar Logit
logit <- glm(y ~ x, data = data, family = binomial(link = "logit"))
data$logit_fit <- predict(logit, newdata = data, type = "response")

#PLOT
ggplot(data, aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.4, color = "black") +
  geom_line(aes(y = mpl_fit), color = "blue", size = 1, linetype = "dashed") +
  geom_line(aes(y = probit_fit), color = "lightblue", size = 1) +
  geom_line(aes(y = logit_fit), color = "salmon", size = 1) +
  labs(
    y = "Probabilidad estimada / Observada",
    x = "Variable explicativa (x)",
    title = "Comparación: MPL, Probit y Logit",
    subtitle = "Puntos: Observados (binarios) | Líneas: Modelos ajustados"
  ) +
  scale_y_continuous(limits = c(-0.1, 1.1)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_manual(
    name = "Modelo",
    values = c("MPL" = "blue", "Probit" = "lightblue", "Logit" = "salmon")
  ) +
  guides(color = guide_legend(override.aes = list(linetype = c(2, 1, 1)))) +
  annotate("text", x = 2, y = 0.9, label = "MPL (azul, punteado)") +
  annotate("text", x = 2, y = 0.8, label = "Probit (light blue)") +
  annotate("text", x = 2, y = 0.7, label = "Logit (salmon)")


