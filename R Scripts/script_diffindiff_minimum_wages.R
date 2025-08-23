#### Estimador de Diferencias en Diferencias aplicado ####
##### Minimum Wages and Employment: A Case Study of the Fast-Food Industry in New Jersey and Pennsylvania ####
#David Card; Alan B. Krueger The American Economic Review, Vol. 84, No. 4. (Sep., 1994), pp. 772-793 ####

#Carga de datos ----
#Instalación de paquetería para obtener Card & Krueger (1994):
#remotes::install_github("b-rodrigues/diffindiff")
library(pacman)
p_load('tidyverse' ,'diffindiff', 'kableExtra')
#Datos
data("njmin")
str(njmin)


#Exploración ---- 

#Restaurantes por estado y año
df_count <-  njmin |>  
  group_by(state, observation) %>%
  summarise(n_restaurantes = n(), 
            .groups = "drop")
#Plot
ggplot(df_count, aes(x = observation, y = n_restaurantes, fill = state)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n_restaurantes), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3.5) +
  labs(
    x = "Año",
    y = "Número de restaurantes",
    fill = "Estado",
    title = "Cantidad de restaurantes por Estado y Año"
  ) +
  scale_fill_manual(values = c("New Jersey" = "#1f77b4", "Pennsylvania" = "lightblue")) +
  theme_minimal()


#Distribución de restaurantes 

table_dist_rest  <- njmin |> 
  group_by( state, chain) |>
  summarise(n = n()) |> 
  mutate(prop = n / sum(n)) |> 
  select(-n) |> 
  pivot_wider(names_from = state, values_from = prop, values_fill = 0) 

kable(table_dist_rest, digits = 2, caption = "Participación de cadenas por estados")



# Estadísticas Descriptiva: Medias de empleo, salario, horas pretratamiento y post ------

#Creación de variables:
#emptot : los equivalentes de tiempo completo (FTE, por sus siglas en inglés) 
#consisten en empleados de tiempo completo, gerentes y empleados de medio tiempo (multiplicado por 0.5) .
#prop_fte: proporción de empleados de tiempo completo (empft/emptot)

njmin <- njmin  |> 
         mutate(emptot =  empft + nmgrs +  (emppt * 0.5), 
                prop_fte = (empft/emptot)*100 )


#Pre-tratamiento
table_medias_pretrat <- njmin   |> 
                        filter(observation == "February 1992")  |> 
                        group_by(state) |> 
                        summarise(emptot = mean(emptot, na.rm = TRUE),
                                  proptaff  = mean(prop_fte, na.rm = TRUE),
                                  wage_st = mean(wage_st, na.rm = TRUE),
                                  hrsopen = mean(hrsopen, na.rm = TRUE))  |> 
                        pivot_longer(cols=-state, names_to = "variable")  |> 
                       
                       mutate(periodo = "Pre")  
#Post-tratamiento    
table_medias_posttrat    <-  njmin   |> 
                             filter(observation == "November 1992")  |> 
                             group_by(state) |> 
                             summarise(emptot = mean(emptot, na.rm = TRUE),
                                       proptaff  = mean(prop_fte, na.rm = TRUE),
                                       wage_st = mean(wage_st, na.rm = TRUE),
                                       hrsopen = mean(hrsopen, na.rm = TRUE)) |> 
                             pivot_longer(cols=-state, names_to = "variable") |>
                            mutate(periodo = "Post") 
                             
tabla_post_pre <- bind_rows(table_medias_pretrat, table_medias_posttrat) |>
                  pivot_wider(names_from = c(state, periodo), values_from = value)

tabla_post_pre                

kable(tabla_post_pre  , digits = 2, caption = "Medias")


# Diferencias y ATE ------

mean_emptot  <- njmin |>  
                      group_by(observation, state) |>  
                      summarise(emptotal = mean(emptot , na.rm = TRUE))

# pre-tratamiento  (NJ)
mean_njfeb <- mean_emptot[1,3]
# pre-tratamiento  (PA) Control
mean_pafeb <- mean_emptot[2,3]
# post_tratamiento (NJ)
mean_njnov <- mean_emptot[3,3]
# post_tratamiento (PA) Control
mean_panov <- mean_emptot[4,3]



#Diferencia de grupo tratado vs control pre
diff_pre <- mean_njfeb -  mean_pafeb 
#Diferencia de grupo tratado vs control post
diff_post <-  mean_njnov - mean_panov

#ATE
ate <-  diff_post - diff_pre 
2.753606	

# Estimación del efecto por Diff-in- Diff ------

#####1. Es necesario crear las variables dummy's de tratamiento (GrupoTratado_i) y tiempo (PostTrat_t) ----

njmin  <- njmin |> 
          mutate(PostTrat = ifelse(observation == "November 1992", 1, 0),
                 GrupoTratado = ifelse(state == "New Jersey", 1, 0)  )


#####2. La Estimación  ----

#Podemos hacerla por OLS planteando una interacción de tiempo y tratamiento o por efectos fijos de dos vías (TWFE) o p

###### 2.1) Interacción  ----

model_did_inter <- lm(emptot ~ PostTrat + GrupoTratado + PostTrat:GrupoTratado,
                      data = njmin)

summary(model_did_inter)
 

###### 2.2) TWFE ------ 
#Recordando: Le damos estructura de panel al df 
p_load('plm')
df_panel_njmin <- pdata.frame(njmin, 
                              index = c("sheet" ,"PostTrat"))
#Model TWFE
model_did_twfe <- plm( emptot ~ GrupoTratado*PostTrat,
                       data = df_panel_njmin,
                       model = "within",   # efectos fijos de unidad
                       effect = "twoways")  # también efectos fijos de tiempo


summary(model_did_inter)




##### Plots de salario ------
plot_hist_pre <- njmin |> 
  filter(observation == "February 1992") |> 
  ggplot(aes(wage_st, fill = state)) +
  geom_histogram(aes(y=c(..count..[..group..==1]/sum(..count..[..group..==1]),
                         ..count..[..group..==2]/sum(..count..[..group..==2]))*100),
                 alpha=0.5, position = "dodge", bins = 20) +
  labs(title = "Febrero 1992", x = "rango de salario", y = "% de tiendas", fill = "") +
  scale_fill_grey()

plot_hist_post <- njmin |> 
  filter(observation == "November 1992") %>%
  ggplot(aes(wage_st, fill = state)) +
  geom_histogram(aes(y=c(..count..[..group..==1]/sum(..count..[..group..==1]),
                         ..count..[..group..==2]/sum(..count..[..group..==2]))*100),
                 alpha = 0.5, position = "dodge", bins = 23) +
  labs(title = "Noviembre 1992", x="rango de salario", y = "% de tiendas", fill="") +
  scale_fill_grey()

plot_hist_post














