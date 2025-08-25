#Librearias ----
if (!require('pacman', character.only = TRUE)){ install.packages('pacman')}
library(pacman)
p_load("causaldata",  #Datos gapminder
       "tidyverse",   #Paqueterías para análisis de datos
       "wooldridge",  #Datos sindicatos
       "plm"          #Panel Linear Models
)

#Data Salarios 
data(wagepan)
head(wagepan)
glimpse(wagepan)

## Pooled OLS ----
#Estructura de datos panel con pdata.frame
pdata_wage <- pdata.frame(wagepan, index = c("nr", "year"))

pooled_plm_wage <- plm(lwage ~ union+
                         educ + exper + I(exper^2) , 
                       data = pdata_wage, model = "pooling")
summary(pooled_plm_wage)


#Modelo FE con plm ----
FE_plm_wage <- plm(lwage ~  union + educ + exper + I(exper^2) , 
                   data = pdata_wage,
                   model = "within", 
                   effect = 'individual')

summary(FE_plm_wage)

#Efectos fijos (FE) vs Pooled OLS (POOLS) ----
#F-test
f_test_wage <- pFtest(FE_plm_wage,pooled_plm_wage)
print(f_test_wage)

#TWFE ----
m_twfe_wage <- plm(lwage ~  union + educ + exper + I(exper^2),
                   data = pdata_wage, 
                   model = "within",
                   effect = "twoways")
summary(m_twfe_wage)



#Modelo FE con plm ----
FE_plm_wage <- plm(lwage ~  union + educ + exper + I(exper^2) , 
                   data = pdata_wage,
                   model = "within", 
                   effect = 'individual')

#Efectos fijos (FE) vs  Efectos fijos de dos vías (TWFE) ----
print(pFtest(m_twfe_wage , FE_plm_wage))




# Estimación Efectos Aleatorios -----
m_RE <- plm(lwage ~  union + educ + exper + I(exper^2),
            data = pdata_wage  ,
            model = "random")

summary(m_RE)

#Test Hausman: Efectos Fijos vs Efectos Aleatorios 
phtest(FE_plm_wage, m_RE)

# Test BP POLS vs Random Effects ----
plmtest(pooled_plm_wage, type = "bp")

# Tabla de todos los modelos ----
#Parquetería para utilizar la función tidy
p_load('broom')

tabla_modelos_wage <- bind_rows(
  tidy(pooled_plm_wage)  |>  mutate(Modelo = "Modelo pooled"),
  tidy(FE_plm_wage)      |>   mutate(Modelo = "Modelo FE plm") ,
  tidy(m_twfe_wage)      |>  mutate(Modelo = "Two-ways FE"), 
  tidy(m_RE)      |>  mutate(Modelo = "Random Effects"), 
  ) |>  
  filter(term == 'union' )

tabla_modelos_wage









