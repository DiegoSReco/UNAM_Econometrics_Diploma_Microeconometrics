#Librearias ----
install.packages('pacman')
library(pacman)
p_load("causaldata",  #Datos gapminder
       "tidyverse",   #Paqueterías para análisis de datos
       "wooldridge",  #Datos sindicatos
       "plm"          #Panel Linear Models
       )

#Datos Esperanza de vida ---
data("gapminder", package = "causaldata")
head(gapminder)
## Pooled OLS ----

#Estructura de datos panel con pdata.frame
df_paneld <- pdata.frame(gapminder, index = c("country", "year"))
#Ïndice país-año
head(df_paneld)
df_paneld <- df_paneld |>  
             mutate(log_PIBper = log(gdpPercap))

# Estimación de modelos: Pooled OLS
pooled_plm <- plm(lifeExp ~ log_PIBper , data = df_paneld, model = 'pooling')

summary(pooled_plm)


#Con función lm()
#Estimación de modelos: Pooled OLS
pooled_lm <- lm(lifeExp ~ log(gdpPercap) , data = df_paneld)
#El resultados es el mismo que con plm
summary(pooled_lm)


# Plot México, Turkia, Argentina ----

plot_mex_turk_arg  <- gapminder |>  
  filter(country %in% c('Mexico', 'Turkey', 'Argentina')) |>   #Filtramos 

 ggplot(aes(x = gdpPercap, y = lifeExp, color = country)) +
       geom_point(alpha = 0.6) +
       scale_x_log10() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Relación entre PIB per cápita y esperanza de vida por continente",
       x = "PIB per cápita (escala logarítmica)", y = "Esperanza de vida") + 
     theme_minimal() 

print(plot_mex_turk_arg )

# Estimación de modelos: Effectos Fijos por substracción de media  ----
df_gapmind <- causaldata::gapminder
#Substracción de media a esperanza de vida y log percapita
df_gapmind  <- df_gapmind  |> 
               mutate(log_PIBper = log(gdpPercap)) |> 
               group_by(country) |> 
               mutate(#media esperanza de vida
                      life_exp_aver = mean(lifeExp),
                      #Esperanza de vida within
                      lifeExp_within = lifeExp - mean(lifeExp),
                      #media log(pib_percapita)
                      log_PIBper_mean = mean(log_PIBper) , 
                      #PIB within
                      log_PIBper_within = log_PIBper  - mean(log_PIBper )) |>  
               
               ungroup() |> 
               arrange(country)
View(df_gapmind)
head(df_gapmind)
             
#Estimación del modelo transformado por OLS con función lm() 
m_FE1 <- lm(lifeExp_within ~ log_PIBper_within, data = df_gapmind)

summary(m_FE1)


#Estimación con función plm() (Nos ahorramos el proceso de la substracción de la media)
#Datos con índice por país y año
df_pdata <- pdata.frame(df_gapmind , index = c("country", "year"))
#Modelo FE con plm
m_FE2 <- plm(lifeExp ~ log_PIBper , data = df_pdata , model = "within")
summary(m_FE2)

# Estimación de Effectos Fijos con variable  dummy por observación
m_FE3 <- lm(lifeExp ~ factor(country) +  log_PIBper, data = df_gapmind)
summary(m_FE3)

# Compración POOLS vs Effectos Fijos (FE) ----
# Test F para efectos individuales (Pooled vs EF)
f_test <- pFtest(m_FE2,pooled_plm)
print(f_test)


###  R-sq within y  overall ---

#### R-sq Within ----

df_gapmind <- df_gapmind |>
               mutate(residuos =  residuals(m_FE2) ,
                    life_exp_fit = fitted(m_FE2))

# r2 = 1 - (rss/tss)
r2_within <- 1 - sum(df_gapmind$residuos^2) / sum(df_gapmind$lifeExp_within^2) 
r.squared(m_FE2)

#Overall
m_overall <- plm(lifeExp ~ log_PIBper , data = df_pdata , model = "pooling")

summary(m_overall)

# 
# #### R-sq between  ----
# m_between <- plm(lifeExp ~ log_PIBper , data = df_pdata , model = "between")
# 
# summary(m_between)


# Estimación efectos fijos dos vías ---- 

## Substracción de media doble (Doubledemeaning ) ----
  
#Media por año y media global 
df_gapmind <- df_gapmind  |> 
group_by(year) |> 
mutate(lifeExp_year_mean = mean(lifeExp),
       logPIBper_year_mean = mean(log_PIBper))  |> 
ungroup() |> 
mutate(lifeExp_global_mean = mean(lifeExp),
         logPIBper_global_mean = mean(log_PIBper))

#Creación de variables Double Demeaning  yit​−yˉ​i⋅​−yˉ​⋅t​+yˉ​⋅⋅​
df_gapmind <- df_gapmind  |> 
  mutate( 
    lifeExp_within_2way = lifeExp - life_exp_aver - lifeExp_year_mean + lifeExp_global_mean,
    logPIBper_within_2way = log_PIBper - log_PIBper_mean  - logPIBper_year_mean + logPIBper_global_mean
  )

## Estimación por  TWFE por transformación Doubledemeneaning ------
m_FE2way_double <- lm(lifeExp_within_2way ~ logPIBper_within_2way, data = df_gapmind)
summary(m_FE2way_double)

## Two-way FE con plm -----
m_twfe <- plm(lifeExp ~   log_PIBper, data = df_pdata, model = "within", effect = "twoways")
summary(m_twfe)

## Two-way FE con feols ----
#Paquetería
p_load('fixest')

m1_twfe <- feols(lifeExp ~ log_PIBper | country + year,
                 data = df_gapmind)
summary(m1_twfe)


## Comparación FE una vía vs FE dos vía (TWFE) ------
#FE
m_FE2 <- plm(lifeExp ~ log_PIBper , data = df_pdata  , model = "within", effect = 'individual')

#TWFE
m_twfe <- plm(lifeExp ~   log_PIBper, data = df_pdata, model = "within",
              effect = "twoways")


#Prueba F  para efectos individuales ¿una vía o dos vías?
print(pFtest(m_twfe , m_FE2))





#####################################
# Estimación Efectos Aleatorios -----
#####################################

m_RE <- plm(lifeExp ~ log_PIBper , data = df_pdata  , model = "random")

#Test Hausman: Efectos Fijos vs Efectos Aleatorios ----
phtest(m_FE2, m_RE)

#Test LM de Breusch-Pagan
# Modelo pooled (OLS)
pooled_plm <- plm(lifeExp ~ log_PIBper, data = df_pdata, model = "pooling")
summary(pooled_plm)
# Modelo de efectos aleatorios
m_RE <- plm(lifeExp ~ log_PIBper, data = df_pdata, model = "random")
summary(m_RE)

# Test BP POLS vs Random Effects ----
plmtest(pooled_plm, type = "bp")


# Tabla de todos los modelos ----
#Parquetería para utilizar la función tidy
p_load('broom')
tabla_modelos <- bind_rows(
                 tidy(pooled_plm) |>  mutate(Modelo = "Modelo pooled"),
                 tidy(m_FE2)      |>   mutate(Modelo = "Modelo FE plm") ,
                 tidy(m_twfe)     |>  mutate(Modelo = "Two-ways FE"),
                 tidy(m_RE)       |>  mutate(Modelo = "Random Effects") ) |>  
                 filter(term != '(Intercept)' )






