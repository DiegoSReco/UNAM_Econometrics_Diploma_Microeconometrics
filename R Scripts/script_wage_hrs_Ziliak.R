
-------------------------------------------------------------------------------
###### Modelos Dinámicos de Datos Panel: Método de estimación VI y GMM ----
-------------------------------------------------------------------------------

### Paqueterías ---
library(pacman)
p_load( "tidyverse",    "plm")
options(scipen = 999)
#-------------------------------------------------------------------------------
### Data  ----
#-------------------------------------------------------------------------------
  
  
file_path <- "C:/Users/Diego/OneDrive/Documentos/Diplomado Econometría UNAM/Data/MOM.dat"

columnas <- c("lnhr", "lnwg", "kids", "ageh", "agesq", "disab", "id", "year")

#log of annual hours worked, log of of hourly wage, number of children, age, quadratic age,
# = 1 if bad health, id , year
df_wage_hr <- read.table(file_path,  
                         col.names = columnas ,
                         sep = "",  
                         stringsAsFactors = FALSE)

head(df_wage_hr)
#Estadística Descriptiva
#Paquete para obtener tabla de estadística descriptiva
p_load('vtable')
st(df_wage_hr)  

#¿Cuántos períodos tenemos (T)? ¿Cuántas observaciones transversales (N)? 

T <- df_wage_hr  |> 
                    summarise(number_year = n_distinct(year))

print(T)

N <- df_wage_hr  |>  
                    summarise(n_ids = n_distinct(id))
print(N)
#--------------------------------------------------------------------------------
#### POLS ---- 
#--------------------------------------------------------------------------------
  
  
data_wage_hr <- pdata.frame(df_wage_hr, index = c("id", "year"))

pooled_plm_wage_hr <- plm(lnhr ~ lnwg + kids + ageh +  agesq + disab, 
                       data = data_wage_hr, model = "pooling")
summary(pooled_plm_wage_hr)

#Interpretación: 
#Un aumento del 1% en el salario se asocia con un 0.082% más de horas trabajadas
#Cada hijo adicional se asocia con un 0.8% más de horas trabajadas
#Tener alguna discapacidad reduce 9.1% las horas  trabajadas 

#### Efectos Fijos (FE) ----

FE_plm_wage_hr <- plm(lnhr ~ lnwg + kids + ageh +  agesq + disab  , 
                      data = data_wage_hr, model = "within", effect = 'individual')
summary(FE_plm_wage_hr)



#Interpretación 
#Un aumento del 1% en el salario se asocia con 0.165% más de horas trabajadas, 
#controlando por características individuales no observadas


#prueba Haussman
phtest(FE_plm_wage_hr, pooled_plm_wage_hr)

#Usamos efectos fijos 


#--------------------------------------------------------------------------------
#### Primera Diferencia FD ----
#--------------------------------------------------------------------------------
  
  
# Creación de primer diferencias 

p_load('dtplyr') #Instalo dtplyr para utilizar lazy_dt

#Cada observación es (i,t) y dentro de i los datos están ordenados por t
data_wage_hr_n <- data_wage_hr |> 
                  lazy_dt() |> 
                  group_by(id) |> 
                  arrange(year, .by_group = TRUE) |> 
                  mutate(Dlnhr =  lnhr - lag(lnhr,1),
                       Dlnwg = lnwg -  lag(lnwg, 1),
                       Dage = ageh  - lag(ageh, 1),
                       Dkids =  kids - lag(kids, 1),
                       Dagesq = agesq- lag(agesq, 1),
                       Ddisab = disab - lag(disab, 1)
                      ) |> 
  ungroup() |>  
  as_data_frame()
               
 View(data_wage_hr_n)
#Data frame eliminando 1979
data_wage_hr_f1979 <- data_wage_hr_n |>  
                      filter(year !=1979)

FD_wage_hr_sin1979 <- lm(Dlnhr ~ -1 +Dlnwg +  Dkids + Dage  + Dagesq + Ddisab  , 
                          data = data_wage_hr_f1979 )
summary(FD_wage_hr_sin1979)

#Interpretación
#Un aumento promedio de 1% en el salario se asocia con un aumento de 0.11% 
#en el cambio de un período a otro  de las horas trabajadas. 



#No comparable con FE
#Pocos períodos, cambios grandes en variables - FD           
# Muchos períodos, cambios pequeños, o tendencias lineales - FE                                        
# Maximizar eficiencia y se tienen datos balanceados   - FE                                  
# Solo quieres  eliminar heterogeneidad individual constante y enfocarte en cambios -FD         
  
--------------------------------------------------------------------------------
#### Método de estimación GMM ----
--------------------------------------------------------------------------------
  
  
# Formula de GMM: "Y(i,t) ~ X(i,t) | Z(i,t)"
# Y : variable dependiente
# X : variables endógenas;
# Z : conjunto de intrumentos

#Y(i,t)= lnhr
#X (i,t)= lnwg ,kids,age,agesq ,Disab 
#Z (i,t) = lnwg_lag2 ,kids_lag1 , kids_lag2 , ageh_lag1 , ageh_lag2 ,agesq_lag1 , agesq_lag2 ,disab_lag1 , disab_lag2 

# 1) Creación de intrumentos ----
# Utilizar egresores exógenos rezagados uno y dos períodos  y el nivel del regresor endógeno se rezagó dos períodos

##### Transformación de variables para obtener  intrumentos: lags (rezagos) -----
p_load('dtplyr')

data_wage_hr_n <- data_wage_hr_n |>  
                lazy_dt() |> 
                group_by(id) |> 
                arrange(year, .by_group = TRUE) |> 

           mutate(
                 lnwg_lag2 = lag(lnwg,2) ,    # 1. lnwg rezagado dos periodos
                 kids_lag1 = lag(kids, 1),    # 2. kids rezagado un periodo
                 kids_lag2 = lag(kids, 2),    # 3. kids rezagado dos periodos
                 ageh_lag1 = lag(ageh, 1),    # 4. age rezagado un periodo
                 ageh_lag2 = lag(ageh, 2),    # 5. age rezagado dos periodos
                 agesq_lag1 = lag(agesq, 1),  # 6. agesq rezagado un periodo
                 agesq_lag2 = lag(agesq, 2),  # 7. agesq rezagado dos periodos
                 disab_lag1 = lag(disab, 1),  # 8. disab rezagado un periodo
                 disab_lag2 = lag(disab, 2)   # 9. disab rezagado dos periodos
                ) |> 
  
      ungroup() |> 
      as.data.frame()

#Exploramos resultados
st(data_wage_hr)

#--------------------------------------------------------------------------------
# 2) Intrumentos apilados o en matriz Z Stacked -------
#--------------------------------------------------------------------------------
  
# Crear las variables z1y1 a z9y8 para cada año
for (i in 1:8) {
  
  year_var <- 1980 + i
  
  data_wage_hr_n <- data_wage_hr_n |> 
                  mutate(
                  !!paste0("z1y", i) := if_else(year == year_var, ageh_lag1, 0),
                  !!paste0("z2y", i) := if_else(year == year_var, agesq_lag1, 0),
                  !!paste0("z3y", i) := if_else(year == year_var, kids_lag1, 0),
                  !!paste0("z4y", i) := if_else(year == year_var, disab_lag1, 0),
                  !!paste0("z5y", i) := if_else(year == year_var, ageh_lag2, 0),
                  !!paste0("z6y", i) := if_else(year == year_var, agesq_lag2, 0),
                  !!paste0("z7y", i) := if_else(year == year_var, kids_lag2, 0),
                  !!paste0("z8y", i) := if_else(year == year_var, disab_lag2 , 0),
                  !!paste0("z9y", i) := if_else(year == year_var, lnwg_lag2, 0)
                  )
}


#Filtramos los períodos que tienen  1979 & 1980
data_wage_hr_filter <- data_wage_hr_n |>     
                       filter( !(year %in% c(1979,1980)))
View(data_wage_hr_filter)
#Condiciones de momentos 
# r = 9 
# T = 8 
# Condiciones de momentos  = 8 X 9 = 72

#Tabla de estadística descriptiva de nuestras variables 
st(data_wage_hr_filter)



# Definir las variables como vectores
#X(i,t)
X <- c("Dlnwg", "Dkids", "Dage", "Dagesq", "Ddisab")

#Z basic estrcuture -----
Z_b <- c("kids_lag1", "ageh_lag1", "agesq_lag1", "disab_lag1",  #t-1
         
         "ageh_lag2", "agesq_lag2", "kids_lag2",  "disab_lag2" ,"lnwg_lag2") #t-2

# Z_b = [kids_lag1₁   ageh_lag1₁   agesq_lag1₁   disab_lag1₁   ...   lnwg_lag2₁  ]
#       [kids_lag1₂   ageh_lag1₂   agesq_lag1₂   disab_lag1₂   ...   lnwg_lag2₂  ]
#       [kids_lag1₃   ageh_lag1₃   agesq_lag1₃   disab_lag1₃   ...   lnwg_lag2₃  ]
#       [    ⋮            ⋮             ⋮             ⋮         ⋱        ⋮       ]
#       [kids_lag1ₙ   ageh_lag1ₙ   agesq_lag1ₙ   disab_lag1ₙ   ...   lnwg_lag2ₙ  ]

#Z[i,t] 
Z_stacked <- c(paste0("z", 1:9, "y1"), paste0("z", 1:9, "y2"), paste0("z", 1:9, "y3"),
               paste0("z", 1:9, "y4"), paste0("z", 1:9, "y5"), paste0("z", 1:9, "y6"),
               paste0("z", 1:9, "y7"), paste0("z", 1:9, "y8") )#Vars 
# Z[i,t] = [Z₁   0    0    ...   0  ]
# [0    Z₂   0    ...   0  ]
# [0    0    Z₃   ...   0  ]
# [⋮    ⋮    ⋮     ⋱    ⋮  ]
# [0    0    0    ...   Z₈ ]
# 
# Donde cada Zₜ es:
# [kids_lag1₁ₜ   ageh_lag1₁ₜ   ...   lnwg_lag2₁ₜ]
# [kids_lag1₂ₜ   ageh_lag1₂ₜ   ...   lnwg_lag2₂ₜ]
# [     ⋮            ⋮         ⋱         ⋮      ]
# [kids_lag1ₙₜ   ageh_lag1ₙₜ   ...   lnwg_lag2ₙₜ]

data_wage_hr_filter |>  select(all_of(X))
data_wage_hr_filter |>  select(all_of(Z_b))
data_wage_hr_filter |>  select(all_of(Z_stacked))

#View(data_wage_hr_filter)
#Períodos perdidos 
T_trans <- data_wage_hr_filter  |>  
           summarise(T_trans = n_distinct(year))
print(T_trans - T)

#Observaciones pérdidas
 n_perdidas <- dim(data_wage_hr_filter )[1] - dim(data_wage_hr )[1]  
 print( n_perdidas)
 
 
 #--------------------------------------------------------------------------------
#### OLS (FD)  -----
 #--------------------------------------------------------------------------------
formula <- as.formula(paste("Dlnhr ~ - 1 + ", paste(X, collapse = "+")))
 
#Estimamos modelo de FD por OLS (aquí como calculamos lags de t-2 tenemos un período menos)
model_OLS  <- lm(formula, data = data_wage_hr_n)
summary(model_OLS)

#Interpretación  
#Un aumento del 1% en el salario de un período a otro se asocia 
#con un aumento de 0.11%en horas trabajadas en ese mismo período
# Buscamos respuestas temporales:
#la respuesta temporal a los cambios de salario.

#Errores robustos
p_load('sandwich', 'lmtest') 
# Calcular errores robustos
robust_se <- vcovHC(model_OLS , type = "HC1")  
sqrt(diag(robust_se))

### Table Robust Errors vs Normal
table_comparison_stderr <- data.frame(
                            Coefficient = coef(model_OLS),
                            SE_Normal = coef(summary(model_OLS))[, "Std. Error"],
                            SE_Robust = sqrt(diag(robust_se))
                                     )

--------------------------------------------------------------------------------
####  Variables Instrumentales: 2SLS ----
--------------------------------------------------------------------------------
  
p_load('AER')

#Formula 
iv_formula <- as.formula(paste("Dlnhr ~ - 1 +", paste(X, collapse = " + "), " |",
                               paste(Z_b, collapse = " + ")) )

# Estimar el modelo 2SLS sin constante
model_IV <- ivreg(iv_formula, data = data_wage_hr_n)


summary(model_IV, diagnostics = TRUE)

# Calcular errores robustos
robust_se_vi <- vcovHC(model_IV , type = "HC1")  

coeftest(model_IV, vcov. = robust_se_vi)

### Table Robust Errors vs Normal
table_comparison_stderr_VI <- data.frame(
  Coefficient = coef(model_IV),
  SE_Normal = coef(summary(model_IV))[, "Std. Error"],
  SE_Robust = sqrt(diag(robust_se_vi))
)


#Interpretación  
#Un aumento del 1% en el salario de un período a otro se asocia 
#con un aumento de 0.12%en horas trabajadas en ese mismo período

#--------------------------------------------------------------------------------
#Estimación por PGMM instrumentos apilados   ------
#--------------------------------------------------------------------------------
#utilizaremos pgmm() de plm 

mod_pgmm <- pgmm(lnhr ~ lag(lnwg, 1)+ kids + ageh + agesq + disab |
                        lag(kids, 1:2) + lag(ageh, 1:2) + lag(agesq, 1:2) +
                        lag(disab, 1:2) + lag(lnwg, 2:2) ,
                        data = data_wage_hr,
                        effect = "individual",
                        model = "onestep", 
                        transformation = "d")

summary(mod_pgmm, diagnostics = TRUE)

#Errores robustos
robust_se_pgmm <- vcovHC(mod_pgmm , type = "HC1")  
coeftest(mod_pgmm, vcov. = robust_se_pgmm)
#Coeficiente y errores
table_comparison_pgmmm <- data.frame(
  Coefficient = coef(mod_pgmm),
  SE_Normal = coef(summary(mod_pgmm))[, "Std. Error"],
  SE_Robust = sqrt(diag(robust_se_pgmm ))
)




###  Dlnwg  OLS, 2SLS caso base, 2SLS stacked -------

table_model_comparison <- bind_rows(table_comparison_stderr[1,]  |>  mutate(Modelo = "Primera Diferencia"),
                                      table_comparison_stderr_VI[1,] |> mutate(Modelo = "2SLS caso base") ,
                                      table_comparison_pgmmm[1,]  |>  mutate(Modelo = "2SLS stacked")) 

print(table_model_comparison)

