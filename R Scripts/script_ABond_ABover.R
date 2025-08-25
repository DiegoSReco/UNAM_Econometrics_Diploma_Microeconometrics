#Paqueterías
library(pacman)
p_load( "tidyverse",   
        "plm", 
        'vtable')

#Data ----
data(EmplUK)
str(EmplUK)

#Estadística Descriptiva
st(EmplUK)  

#Creación de variables ----
data_EmplUK <-  EmplUK |>  
                mutate(lwage = log(wage), 
                       lemp =  log(emp), 
                       lcapital = log(capital), 
                       loutput = log(output))

#Creamos dummy por año  de forma manual 
for (yr in unique(data_EmplUK$year)) {
    
  data_EmplUK[[paste0("year_", yr)]] <- if_else(data_EmplUK$year == yr, 1, 0)
}


#data_EmplUK[[paste0("year_", "1977_2")]] <- if_else(data_EmplUK$year == 1977, 1, 0)

#Pueden usar la paquetería fastdummys

pgmm()
# Estimación de Modelo Dinámico ----

# 1) dynformula(): Genera la fórmula para un modelo dinámico en pgmm()

# 2) Especifica los rezagos para cada variable en la fórmul en una lista  (1,0,2)
#Primera variable un rezago, segunda variable sin rezagso y tercer variable con dos rezagos

#  3) log = TRUE  Tomar logarítmo de las variables 

#  4 ) DataFrame 

# 5)  effect = "twoways"  : Incluye efectos individuales y temporales no observados.

# 6)  model = "twosteps": Mejora la eficiencia usando una matriz robusta en la segunda paso.


# 7) gmm.inst = ~log(emp): Aquí, se usa el logaritmo de emp como instrumento.

#  8) c(2, 99) indica que se utilizan los rezagos de emp desde 2 hasta un número muy alto (99 como un máximo grande).

#
data_EmplUK <- pdata.frame(data_EmplUK, 
                           index = c("firm", "year"))


model_AB <- pgmm(  dynformula(log(emp) ~ log(wage) + log(capital) + log(output)  ,
                            
                            lag = list(c(1,2), c(0,1), c(0,1,2), c(0,1,2)),
                            
                            log = FALSE) ,
                 
                            data_EmplUK,
                 
                            effect = "twoways", 
                 
                            model = "twosteps",
                 
                            gmm.inst = ~log(emp),
                            #iv.inst = ~ log(wag),  
                            
                            lag.gmm = list(c(2, 99))
                   )


summary(model_AB)

#Un aumento del 1% en el empleo del periodo anterior
#está asociado con un aumento del 0.63% en el empleo del periodo actual.


# Estimador Arellano-BOver ----

#Usa la transformación en diferencias en niveles, requerida para Arellano-Bover.
# Por lo que agrego un argumetno  transformation = "ld",

#system = TRUE: Activa el modelo de sistema GMM. 
#En este enfoque, se estiman tanto la ecuación en diferencias primeras
#(differenced equation) como la ecuación en niveles (level equation) de manera conjunta.              


model_ABover <- pgmm(dynformula(log(emp) ~ log(wage) + log(capital) + log(output), 
                     lag = list(c(1,2), c(0,1), c(0,1,2), c(0,1,2)), 
                     log = FALSE),
                     data_EmplUK,
                     effect = "twoways",   
                     model = "twosteps",
                     transformation = "ld", #Argumento adicional Arellano-Bond
                     gmm.inst = ~log(emp),
                     #iv.inst = ~ log(wag),  
                     lag.gmm = list(c(2, 99)) ,
                     system = TRUE         #Argumento adicional Arellano-Bond             
                    )




summary(model_ABover)










