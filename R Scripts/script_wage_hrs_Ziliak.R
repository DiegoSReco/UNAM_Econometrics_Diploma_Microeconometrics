
-------------------------------------------------------------------------------
###### Modelos Dinámicos de Datos Panel: Método de estimación VI y GMM ----
-------------------------------------------------------------------------------

# Limpiar entorno
rm(list = ls())
gc()

# Paqueterías
library(pacman)
p_load("tidyverse",    # Manipulación de datos
       "plm",          # Modelos de panel
       "lmtest",       # Tests estadísticos
       "sandwich",     # Errores robustos
       "car",          # Tests adicionales
       "stargazer",    # Tablas de regresión
       "vtable",       # Estadísticas descriptivas
       "dtplyr")       # Manipulación eficiente

options(scipen = 999)

# ==============================================================================
# 1. CARGA Y EXPLORACIÓN DE DATOS
# ==============================================================================

# Cargar datos
file_path <- "C:/Users/Diego/OneDrive/Documentos/Diplomado Econometría UNAM/Data/MOM.dat"

columnas <- c("lnhr", "lnwg", "kids", "ageh", "agesq", "disab", "id", "year")

df_wage_hr <- read.table(file_path,  
                         col.names = columnas,
                         sep = "",  
                         stringsAsFactors = FALSE)

#log of annual hours worked, log of of hourly wage, number of children, age, quadratic age,
# = 1 if bad health, id , year

# Vista inicial
head(df_wage_hr, 15)
str(df_wage_hr)

# Dimensiones del panel
cat("\n=== DIMENSIONES DEL PANEL ===\n")
T <- df_wage_hr |> 
  summarise(periodos = n_distinct(year))
cat("Número de períodos (T):", T$periodos, "\n")

N <- df_wage_hr |> 
  summarise(individuos = n_distinct(id))
cat("Número de individuos (N):", N$individuos, "\n")

total_obs <- nrow(df_wage_hr)
cat("Total de observaciones:", total_obs, "\n")
cat("Panel balanceado:", total_obs == (T$periodos * N$individuos), "\n")

# Verificar estructura del panel
pdim(pdata.frame(df_wage_hr, index = c("id", "year")))


# ==============================================================================
# 2. POOLED OLS (MODELO BÁSICO - IGNORA ESTRUCTURA PANEL)
# ==============================================================================

# Convertir a panel data
data_wage_hr <- pdata.frame(df_wage_hr, index = c("id", "year"))
#Estimar POOLED
pooled_plm_wage_hr <- plm(lnhr ~ lnwg + kids + ageh +  agesq + disab, 
                          data = data_wage_hr, model = "pooling")

summary(pooled_plm_wage_hr)



cat("\n--- INTERPRETACIÓN POOLED OLS ---\n")
cat("• Un aumento del 1% en salario → 0.082% más horas trabajadas\n")
cat("• Cada hijo adicional → 0.8% más horas trabajadas\n")
cat("• Tener discapacidad → 9.5% menos horas trabajadas\n")
cat("\nPROBLEMA: Ignora heterogeneidad individual (αᵢ)\n")


# ------------------------------------------------------------------------------
# 2.1 Test de Heterocedasticidad (Breusch-Pagan modificado)
# ------------------------------------------------------------------------------

cat("\n\n---TEST DE HETEROCEDASTICIDAD ---\n")
cat("H0: Homocedasticidad (varianza constante)\n")
cat("H1: Heterocedasticidad\n\n")

# Test de heterocedasticidad para panel
bp_hetero <- bptest(lnhr ~ lnwg + kids + ageh + agesq + disab, 
                    data = df_wage_hr,
                    studentize = FALSE)
print(bp_hetero)

if (bp_hetero$p.value < 0.05) {
  cat("\n⚠️  CONCLUSIÓN: Rechazamos H0 → Hay heterocedasticidad\n")
  cat("   SOLUCIÓN: Usar errores robustos (HC1) o clusterizados\n")
} else {
  cat("\n✓ CONCLUSIÓN: No rechazamos H0 → Homocedasticidad\n")
}


# ------------------------------------------------------------------------------
#  2.2 Test de Correlación Contemporánea (Breusch-Pagan LM)
# ------------------------------------------------------------------------------

cat("\n---TEST DE CORRELACIÓN CONTEMPORÁNEA ---\n")
cat("H0: No hay correlación entre individuos en el mismo período\n")
cat("H1: Existe correlación contemporánea\n\n")

# Test de Breusch-Pagan para dependencia cross-sectional
pcdtest_result <- pcdtest(pooled_plm_wage_hr, test = "lm")
print(pcdtest_result)

if (pcdtest_result$p.value < 0.05) {
  cat("\n⚠CONCLUSIÓN: Rechazamos H0 → Hay correlación contemporánea\n")
  cat("   SOLUCIÓN: Modelos con efectos cross-section\n")
} else {
  cat("\n✓ CONCLUSIÓN: No hay correlación contemporánea\n")
}

# ------------------------------------------------------------------------------
# 2.3 Test de Autocorrelación Serial (Wooldridge)
# ------------------------------------------------------------------------------

cat("\n\n---TEST DE AUTOCORRELACIÓN SERIAL (Wooldridge) ---\n")
cat("H0: No hay correlación serial de primer orden en los residuales\n")
cat("H1: Existe correlación serial AR(1)\n\n")

# Test de Wooldridge para autocorrelación en panel
wooldridge_test <- pwartest(pooled_plm_wage_hr)
print(wooldridge_test)

if (wooldridge_test$p.value < 0.05) {
  cat("\nCONCLUSIÓN: Rechazamos H0 → Hay autocorrelación serial\n")
  cat("   SOLUCIÓN: Usar errores robustos HAC o modelos dinámicos\n")
} else {
  cat("\n✓ CONCLUSIÓN: No rechazamos H0 → No hay autocorrelación serial\n")
}

# ------------------------------------------------------------------------------
# 2.4 Errores Robustos
# ------------------------------------------------------------------------------

# Errores estándar robustos para Pooled
cat("\n--- Errores Estándar Robustos (HC1) ---\n")
coeftest(pooled_plm_wage_hr, vcov = vcovHC(pooled_plm_wage_hr, type = "HC1"))

# Errores clusterizados por individuo
cat("\n--- Errores Clusterizados por Individuo ---\n")
coeftest(pooled_plm_wage_hr, vcov = vcovHC(pooled_plm_wage_hr, type = "HC1", cluster = "group"))


# ==============================================================================
# 3.EFECTOS FIJOS INDIVIDUALES (FIXED EFFECTS - ONE WAY)
# ==============================================================================

cat("\n\n")
cat("="*80, "\n")
cat("MODELO 3: EFECTOS FIJOS INDIVIDUALES (FE)\n")
cat("="*80, "\n\n")

# Estimar modelo
FE_plm_wage_hr <- plm(lnhr ~ lnwg + kids + ageh +  agesq + disab  , 
                      data = data_wage_hr,
                      model = "within", 
                      effect = 'individual')
summary(FE_plm_wage_hr)

cat("\n--- INTERPRETACIÓN FE ---\n")
cat("• Un aumento del 1% en salario → 0.165% más horas (within)\n")
cat("• Controla por características individuales no observadas (αᵢ)\n")
cat("• Solo usa variación DENTRO de individuos\n")



# ------------------------------------------------------------------------------
# 3.1 Test de Autocorrelación Serial (Wooldridge)
# ------------------------------------------------------------------------------

cat("\n---TEST DE AUTOCORRELACIÓN SERIAL (Wooldridge) ---\n")
cat("H0: No hay correlación serial de primer orden en los residuales\n")
cat("H1: Existe correlación serial AR(1)\n\n")

# Test de Wooldridge para autocorrelación en panel
wooldridge_test <- pwartest(FE_plm_wage_hr)
print(wooldridge_test)

if (wooldridge_test$p.value < 0.05) {
  cat("\nCONCLUSIÓN: Rechazamos H0 → Hay autocorrelación serial\n")
  cat("   SOLUCIÓN: Usar errores robustos HAC o modelos dinámicos\n")
} else {
  cat("\n✓ CONCLUSIÓN: No rechazamos H0 → No hay autocorrelación serial\n")
}

# Errores clusterizados para FE
cat("\n--- Errores Clusterizados para FE ---\n")
coeftest(FE_plm_wage_hr, vcov = vcovHC(FE_plm_wage_hr, type = "HC1", cluster = "group"))


# ------------------------------------------------------------------------------
# 3.2 Test F para Efectos Fijos
# ------------------------------------------------------------------------------

cat("\n\n---TEST F: ¿Son necesarios los efectos fijos? ---\n")
cat("H0: αᵢ = 0 para todo i (Pooled OLS es adecuado)\n")
cat("H1: Existen efectos fijos individuales\n\n")

f_test_fe <- pFtest(FE_plm_wage_hr, pooled_plm_wage_hr)
print(f_test_fe)

if (f_test_fe$p.value < 0.05) {
  cat("\n✓ CONCLUSIÓN: Rechazamos H0 → Los efectos fijos son significativos\n")
} else {
  cat("\n✓ CONCLUSIÓN: No rechazamos H0 → Pooled OLS es suficiente\n")
}


# ==============================================================================
# 4.EFECTOS DOS VIAS (TWO WAYS)
# ==============================================================================

# Estimar efectos fijos individuales + temporales
FE_twoway <- plm(lnhr ~ lnwg + kids + ageh + agesq + disab, 
                 data = data_wage_hr,
                 model = "within", 
                 effect = 'twoways')

summary(FE_twoway)

cat("\n--- INTERPRETACIÓN TWOWAY FE ---\n")
cat("• Controla simultáneamente por:\n")
cat("  - Heterogeneidad individual (αᵢ)\n")
cat("  - Shocks temporales comunes (λₜ)\n")
cat("• Modelo: yᵢₜ = βXᵢₜ + αᵢ + λₜ + uᵢₜ\n")

# ------------------------------------------------------------------------------
# 4.1 Test F para Efectos Fijos temporales
# ------------------------------------------------------------------------------

#Prueba F  para efectos individuales ¿una vía o dos vías?

f_test_fe_2fe <- pFtest(FE_twoway , FE_plm_wage_hr)
print()

if (f_test_fe_2fe$p.value < 0.05) {
  cat("\nCONCLUSIÓN: Rechazamos H0 → Efecto fijo temporal significativo")
} else {
  cat("\n✓ CONCLUSIÓN: No rechazamos H0 → Efecto fijo temporal no significativo")
}


# ------------------------------------------------------------------------------
# 4.2 Test de Autocorrelación Serial (Wooldridge)
# ------------------------------------------------------------------------------

cat("\N---TEST DE AUTOCORRELACIÓN SERIAL (Wooldridge) ---\n")
cat("H0: No hay correlación serial de primer orden en los residuales\n")
cat("H1: Existe correlación serial AR(1)\n\n")

# Test de Wooldridge para autocorrelación en panel
wooldridge_test <- pwartest(FE_twoway)
print(wooldridge_test)

if (wooldridge_test$p.value < 0.05) {
  cat("\nCONCLUSIÓN: Rechazamos H0 → Hay autocorrelación serial\n")
  cat("   SOLUCIÓN: Usar errores robustos HAC o modelos dinámicos\n")
} else {
  cat("\n✓ CONCLUSIÓN: No rechazamos H0 → No hay autocorrelación serial\n")
}

# Errores clusterizados para FE
cat("\n--- Errores Clusterizados para FE ---\n")
coeftest(FE_twoway, vcov = vcovHC(FE_twoway, type = "HC1", cluster = "group"))

# ==============================================================================
# 5.EFECTOS ALEATORIOS (TWO WAYS)
# ==============================================================================
cat("MODELO: EFECTOS ALEATORIOS (RE)\n")

# Estimar modelo de efectos aleatorios
RE_plm <- plm(lnhr ~ lnwg + kids + ageh + agesq + disab, 
              data = data_wage_hr,
              model = "random", 
              effect = 'individual')

summary(RE_plm)

cat("\n--- INTERPRETACIÓN RE ---\n")
cat("• Asume que αᵢ ~ N(0, σ²α) y no está correlacionado con Xᵢₜ\n")
cat("• Más eficiente que FE si los supuestos se cumplen\n")


# ------------------------------------------------------------------------------
# 5.1 FE VS RE
# ------------------------------------------------------------------------------

cat("\n---TEST DE HAUSMAN (FE vs RE) ---\n")
cat("H0: Los efectos individuales no están correlacionados con los regresores\n")
cat("    (Efectos Aleatorios es consistente y eficiente)\n")
cat("H1: Los efectos individuales están correlacionados con los regresores\n")
cat("    (Solo Efectos Fijos es consistente)\n\n")

hausman_test <- phtest(FE_plm_wage_hr, RE_plm)
print(hausman_test)

if (hausman_test$p.value < 0.05) {
  cat("\n✓ CONCLUSIÓN: Rechazamos H0 → Usar EFECTOS FIJOS\n")
} else {
  cat("\n✓ CONCLUSIÓN: No rechazamos H0 → Usar EFECTOS ALEATORIOS\n")
}

hausman_test2 <- phtest(FE_twoway, RE_plm)
print(hausman_test2)

if (hausman_test2$p.value < 0.05) {
  cat("\n✓ CONCLUSIÓN: Rechazamos H0 → Usar EFECTOS FIJOS\n")
} else {
  cat("\n✓ CONCLUSIÓN: No rechazamos H0 → Usar EFECTOS ALEATORIOS\n")
}



# Errores robustos para RE
cat("\n--- Errores Clusterizados para RE ---\n")
coeftest(RE_plm, vcov = vcovHC(RE_plm, type = "HC1", cluster = "group"))


# ==============================================================================
# 6. RESUMEN DE DECISIONES (FE VS RE)
# ==============================================================================

cat("1. TEST F (Pooled vs FE):\n")
if (f_test_fe$p.value < 0.05) {
  cat("   ✓ Efectos fijos son significativos\n")
} else {
  cat("   • Pooled OLS es suficiente\n")
}

cat("\n2. TEST DE HAUSMAN (FE vs RE):\n")
if (hausman_test$p.value < 0.05) {
  cat("   ✓ Usar EFECTOS FIJOS (FE)\n")
} else {
  cat("   • Usar EFECTOS ALEATORIOS (RE)\n")
}
if (f_test_fe_2fe$p.value < 0.05) {
  cat("\nCONCLUSIÓN: Rechazamos H0 → Efecto fijo temporal significativo")
} else {
  cat("\n✓ CONCLUSIÓN: No rechazamos H0 → Efecto fijo temporal no significativo")
}

cat("\n3. AUTOCORRELACIÓN SERIAL:\n")
if (wooldridge_test$p.value < 0.05) {
  cat("   Presente → Considerar errores HAC o modelos dinámicos\n")
} else {
  cat("   ✓ No detectada\n")
}

cat("\n4. HETEROCEDASTICIDAD:\n")
if (bp_hetero$p.value < 0.05) {
  cat("   Presente → Usar errores robustos o clusterizados\n")
} else {
  cat("   ✓ No detectada\n")
}



# ==============================================================================
# 7. Estimación GMM para datos panel (PGMM)
# ==============================================================================
# ------------------------------------------------------------------------------
# 7.1 PRIMERA DIFERENCIA
# ------------------------------------------------------------------------------

# Creación de primer diferencias 
library(dtplyr)
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
  as_tibble()
              

View(data_wage_hr_n)
#Data frame eliminando 1979
data_wage_hr_f1979 <- data_wage_hr_n |>  
                      filter(year !=1979)

FD_wage_hr_sin1979 <- lm(Dlnhr ~ -1 +Dlnwg +  Dkids + Dage  + Dagesq + Ddisab  , 
                          data = data_wage_hr_f1979 )
summary(FD_wage_hr_sin1979)
phtest(FD_wage_hr_sin1979, pooled_plm_wage_hr)

#Interpretación

#Un aumento del 1% en el salario está asociado con un aumento de aproximadamente 0.11% 
#en las horas trabajadas, manteniendo constantes los cambios en las demás variables.

#Interpretación económica:
#La oferta laboral responde positivamente, pero de forma inelástica, a aumentos salariales
#Un 10% de aumento salarial → ≈ 1.1% más horas trabajadas

#Error 
#“Las personas con salarios altos trabajan más”
#Incorrecto: el modelo no compara personas, compara cambios dentro de la misma persona.

#Consideraciones: FE vs FD
#No comparable con FE
#Pocos períodos, cambios grandes en variables - FD           
# Muchos períodos, cambios pequeños, o tendencias lineales - FE                                        
# Maximizar eficiencia y se tienen datos balanceados   - FE                                  
# Solo quieres  eliminar heterogeneidad individual constante y enfocarte en cambios -FD         
  
# ------------------------------------------------------------------------------
# 7.2 Procedimiento método de estimación GMM
# ------------------------------------------------------------------------------

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
library('dtplyr')


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
View(data_wage_hr_n)


#--------------------------------------------------------------------------------
# 2) Intrumentos apilados o en matriz Z Stacked -------
#--------------------------------------------------------------------------------
  
# Crear las variables z1y1 a z9y8 para cada año
for (i in 1:8) {
  
  year_var <- 1980 + i
  
  data_wage_hr_n <- data_wage_hr_n |> 
                  mutate(
                  !!paste0("z1y", i) := ifelse(year == year_var, ageh_lag1, 0),
                  !!paste0("z2y", i) := ifelse(year == year_var, agesq_lag1, 0),
                  !!paste0("z3y", i) := ifelse(year == year_var, kids_lag1, 0),
                  !!paste0("z4y", i) := ifelse(year == year_var, disab_lag1, 0),
                  !!paste0("z5y", i) := ifelse(year == year_var, ageh_lag2, 0),
                  !!paste0("z6y", i) := ifelse(year == year_var, agesq_lag2, 0),
                  !!paste0("z7y", i) := ifelse(year == year_var, kids_lag2, 0),
                  !!paste0("z8y", i) := ifelse(year == year_var, disab_lag2 , 0),
                  !!paste0("z9y", i) := ifelse(year == year_var, lnwg_lag2, 0)
                  )
}


#Filtramos los períodos que tienen  1979 & 1980
data_wage_hr_filter <- data_wage_hr_n |>     
                       filter( !(year %in% c(1979,1980)))
View(data_wage_hr_filter)

#--------------------------------------------
#Condiciones de momentos 
# r = 9 
# T = 8 
# Condiciones de momentos  = 8 X 9 = 72

#--------------------------------------------
#Tabla de estadística descriptiva de nuestras variables 
st(data_wage_hr_filter)

# Definir las variables como vectores
#X(i,t)
X <- c("Dlnwg", "Dkids", "Dage", "Dagesq", "Ddisab")

#Z basica -----------------
Z_b <- c("kids_lag1", "ageh_lag1", "agesq_lag1", "disab_lag1",  #t-1
         
         "ageh_lag2", "agesq_lag2", "kids_lag2",  "disab_lag2" ,"lnwg_lag2") #t-2

# Z_b = [kids_lag1₁   ageh_lag1₁   agesq_lag1₁   disab_lag1₁   ...   lnwg_lag2₁  ]
#       [kids_lag1₂   ageh_lag1₂   agesq_lag1₂   disab_lag1₂   ...   lnwg_lag2₂  ]
#       [kids_lag1₃   ageh_lag1₃   agesq_lag1₃   disab_lag1₃   ...   lnwg_lag2₃  ]
#       [    ⋮            ⋮             ⋮             ⋮         ⋱        ⋮       ]
#       [kids_lag1ₙ   ageh_lag1ₙ   agesq_lag1ₙ   disab_lag1ₙ   ...   lnwg_lag2ₙ  ]

#Z[i,t]  Apilada -----------
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
 
 

 
 # ------------------------------------------------------------------------------
 # 7. FD OLS
 # ------------------------------------------------------------------------------
 
formula <- as.formula(paste("Dlnhr ~ - 1 + ", paste(X, collapse = "+")))
 
#Estimamos modelo de FD por OLS (aquí como calculamos lags de t-2 tenemos un período menos)
 
model_OLS  <- lm(formula, data = data_wage_hr_n)
summary(model_OLS)

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

print(table_comparison_stderr)

#Interpretación 
#Un aumento del 1% en el salario se asocia con un 0.11% de
#aumento en horas, usando solo variación temporal dentro del individuo.

# ------------------------------------------------------------------------------
# 7.4 2SLS (Instrumentos no apilados)
# ------------------------------------------------------------------------------

#Paqutería para estimar 2SLS
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

print(table_comparison_stderr_VI)


#Interpretación  

#Un aumento del 1% en el salario (predicho por su rezago)) está asociado con un aumento de aproximadamente 0.12% 
#en las horas trabajadas, manteniendo constantes los cambios en las demás variables.

# ------------------------------------------------------------------------------
# 7.5 Estimación por GMM (Instrumentos apilados)
# ------------------------------------------------------------------------------

#Utilizamos la función pgmm() de plm 
mod_pgmm <- pgmm(lnhr ~ lag(lnwg, 1) + kids + ageh + agesq + disab |
                  lag(kids, 1:2) + lag(ageh, 1:2) + lag(agesq, 1:2) +
                  lag(disab, 1:2) + lag(lnwg, 2) ,
                  data = data_wage_hr,
                  effect = "individual",
                  model = "onestep", 
                  transformation = "d")

summary(mod_pgmm, diagnostics = TRUE)

#Errores robustos
robust_se_pgmm <- vcovHC(mod_pgmm , type = "HC1")  
coeftest(mod_pgmm, vcov. = robust_se_pgmm)

#Coeficiente y errores
table_comparison_pgmmm <- data.frame(Coefficient = coef(mod_pgmm),
                                     SE_Normal = coef(summary(mod_pgmm))[, "Std. Error"],
                                     SE_Robust = sqrt(diag(robust_se_pgmm )))

# ------------------------------------------------------------------------------
# 7.6 Comparación de resultados
# ------------------------------------------------------------------------------


###  Dlnwg  OLS, 2SLS caso base, 2SLS stacked -------
table_model_comparison <- bind_rows(table_comparison_stderr[1,]  |>  mutate(Modelo = "Primera Diferencia"),
                                    table_comparison_stderr_VI[1,] |> mutate(Modelo = "2SLS caso base") ,
                                    table_comparison_pgmmm[1,]  |>  mutate(Modelo = "2SLS stacked")) 

print(table_model_comparison)






