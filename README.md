# Diplomado de Economía UNAM - Módulo 4: Microeconometría
## Modelos de Respuesta Binaria y Análisis de Datos Panel

[![License: CC BY-NC-SA 4.0](https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/)
[![R Version](https://img.shields.io/badge/R-%E2%89%A5%204.0.0-blue.svg)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-%E2%89%A5%201.3-75AADB.svg)](https://quarto.org/)

**Instructor:** Mtro. Diego Sánchez Rojas  
**Institución:** Universidad Nacional Autónoma de México (UNAM)  
**Programa:** Diplomado en Econometría  
**Módulo:** Etapa 4 - Microeconometría 

---

## 📚 Descripción del Curso

Este repositorio contiene materiales de enseñanza para el **Módulo 4** del Diplomado de Economía de la UNAM, cubriendo técnicas microeconométricas avanzadas con enfoque en:

- **Modelos de Respuesta Binaria** (MPL, Logit, Probit)
- **Variables Dependientes Limitadas** (Tobit, Heckit/Selección Muestral)
- **Métodos de Datos Panel** (Efectos Fijos, Efectos Aleatorios, Primeras Diferencias,Diferencias en Diferencias)
- **Variables Instrumentales en Datos Panel** (2SLS, GMM, PGMM)
- **Modelos Panel Dinámicos** (Arellano-Bond, Arellano-Bover)

Todos los materiales están implementados en **R** con **Quarto** para presentaciones reproducibles.

---

## 📂 Estructura del Repositorio

```
.
├── README.md                          # Este archivo
├── CITATION.cff                       # Información de citación
├── LICENSE                            # Información de licencia
│
├── slides/                            # Presentaciones del curso (archivos .qmd)
│   ├── 01_modelos_respuesta_binaria.qmd
│   ├── 02_modelo_probabilidad_lineal.qmd
│   ├── 03_tobit_seleccion.qmd
│   ├── 04_datos_panel_introduccion.qmd
│   ├── 05_panel_dinamico_I.qmd
│   ├── 06_panel_dinamico_II.qmd
│   ├── 07_microeconometria_intermedia.qmd
│   └── referencias.bib                # Bibliografía
│
├── R Scripts/                              # Scripts de R para aplicaciones empíricas
│   ├── script_ModelosRespuestaBinaria.R
│   ├── script_Tobit_Heckman.R
│   ├── script_plm_gdp_le.R
│   ├── script_plm_wage_union.R
│   ├── script_diffindiff_minium_wages.R
│   ├── script_wage_hrs_Ziliak.R
│   └── script_ABond_ABover.R
│
├── data/                              # Conjuntos de datos
│   └── MOM.dat                        # Datos de mujeres en mercado laboral
├── exercises/                         # Ejercicios (por agregar)
│   └── README.md
│
└── references/                        # Recursos adicionales
    ├── articulos_clave.md
    └── recursos_software.md
```

---

## 🎯 Objetivos de Aprendizaje

Al finalizar este módulo, los estudiantes serán capaces de:

1. ✅ **Estimar e interpretar** modelos de elección binaria (Logit, Probit, Multinomial)
2. ✅ **Aplicar** técnicas de variables dependientes limitadas (Tobit, Heckit)
3. ✅ **Analizar datos panel** usando efectos fijos, efectos aleatorios y primeras diferencias
4. ✅ **Abordar endogeneidad** en datos panel usando estimadores IV y GMM
5. ✅ **Implementar modelos panel dinámicos** (Arellano-Bond, GMM Sistémico)
6. ✅ **Realizar inferencia causal** usando Diferencias en Diferencias
7. ✅ **Interpretar y validar** resultados econométricos críticamente

---

## 🚀 Primeros Pasos

### Requisitos Previos

**Requisitos de Software:**
- [R](https://www.r-project.org/) (≥ 4.0.0)
- [RStudio](https://posit.co/download/rstudio-desktop/) (recomendado)
- [Quarto](https://quarto.org/docs/get-started/) (≥ 1.3) para renderizar presentaciones

**Paquetes de R Requeridos:**

```r
# Paquetes básicos
install.packages(c(
  "tidyverse",      # Manipulación y visualización de datos
  "plm",            # Modelos de datos panel
  "lmtest",         # Pruebas diagnósticas
  "sandwich",       # Errores estándar robustos
  "AER",            # Econometría Aplicada con R
  "sampleSelection" # Modelos Tobit y Heckit
))

# Datos panel avanzados
install.packages(c(
  "pdynmc",         # Modelos panel dinámicos (Arellano-Bond)
  "panelView",      # Visualización de datos panel
  "fixest",         # Efectos fijos rápidos
  "did"             # Diferencias en Diferencias
))

# Utilidades adicionales
install.packages(c(
  "stargazer",      # Tablas LaTeX/HTML
  "modelsummary",   # Tablas de regresión modernas
  "ggplot2",        # Gráficos
  "knitr",          # Reportes dinámicos
  "kableExtra"      # Tablas mejoradas
))
```

### Inicio Rápido

1. **Clonar el repositorio:**
```bash
git clone https://github.com/DiegoSReco/UNAM_Econometrics_Diploma_Microeconometrics.git
cd UNAM_Econometrics_Diploma_Microeconometrics
```

2. **Abrir RStudio y establecer el directorio de trabajo:**
```r
setwd("ruta/a/UNAM_Econometrics_Diploma_Microeconometrics")
```

3. **Ejecutar scripts de ejemplo:**
```r
# Ejemplo de modelos binarios
source("code/script_ModelosRespuestaBinaria.R")

# Ejemplo de datos panel
source("code/script_plm_gdp_le.R")
```

4. **Renderizar presentaciones (requiere Quarto):**
```bash
quarto render slides/02_ppt_Modulo4_MPL.qmd
```

---

## 📖 Contenido del Curso

### 1: Introducción la microeconometría (enfoque de Inferencia Causal
**Presentación:** `01_Modulo4_IntMicroeconometrics.qmd`  

- ¿Qué es la Microeconometría?
- ¿Qué es la Inferencia Causal?
- Marco de Resultados Potenciales
  
### 2: Modelos de Respuesta Binaria
**Presentación:** `02_ppt_Modulo4_MPL.qmd`  
**Código:** `script_ModelosRespuestaBinaria.R`

- Modelo de Probabilidad Lineal (MPL)
- Modelos Logit y Probit
- Efectos marginales e interpretación
- Comparación y diagnósticos de modelos

### 3: Variables Dependientes Limitadas
**Presentación:** `03_ppt_Modulo4_Tobit.qmd`  
**Código:** `script_Tobit_Heckman.R` 

- Datos censurados y truncados
- Modelos Tobit (Tipo I)
- Modelos de selección muestral (Heckman dos etapas)
- Aplicaciones: oferta laboral, salarios

### 4: Modelos de Datos Panel 
**Presentación:** `04_ppt_Modulo4_DatoPanel.qmd`  y `05_ppt_Modulo4_DiffinDiff.qmd` 
**Código:** `script_plm_gdp_le.R`, `script_plm_wage_union.R`, `script_diffindiff_minimum_wages.R` 

- Estructura y ventajas de datos panel
- OLS Agrupado (`Pooled OLS`) vs. Estimadores Panel
- Efectos Fijos (FE) y Efectos Aleatorios (RE)
- Prueba de Hausman
- Primeras Diferencias
- Diferencias en Diferencia (`DiD`)
- Aplicaciones: PIB y esperanza de vida


### Módulo 5: Modelos Panel Dinámicos I
**Presentación:** `06_ppt_Modulo4_PanelDinamico.qmd`  
**Código:** `script_wage_hrs_Ziliak.R`

- Variables instrumentales en datos panel
- Mínimos Cuadrados en Dos Etapas (2SLS)
- Método Generalizado de Momentos (GMM)
- Pruebas de instrumentos débiles
- Sesgo en panel dinámico
- Estimador  `PGMM` 
- GMM en primeras diferencias
- Pruebas de validez de instrumentos (Sargan, Hansen)
- Aplicaciones: ecuaciones salariales, afiliación sindical

### Módulo 6: Modelos Panel Dinámicos II
**Presentación:** `07_ppt_Modulo4_PanelDinamicoII.qmd`  
**Código:** `script_ABond_ABover.R` 

- Estimador Arrellano-Bond
- Estimador Arellano-Bover (Combinación de niveles y diferencias)
- Aplicaciones empíricas

---

## 📊 Conjuntos de Datos

### `mroz` - Conjunto de datos de mujeres en el mercado laboral
**Fuente:**  Wooldridge Source: T.A. Mroz (1987). Descargue en paquetería `wooldridge`.
**Descripción:** Participación laboral y salarios de mujeres casadas  
 
### `gapminder` - Conjunto datos de esperanza de vida y PIB per cápita
**Fuente:**  CausalData: [Causaldata](https://github.com/NickCH-K/causaldata)
**Descripción:** Relación de esperanza de vida y PIB per cápita   

### `wagepan` - Conjunto datos de hombres en el mercado laboral y sindicalismo
**Fuente:**  Wooldridge Source: F. Vella and M. Verbeek (1998). Descargue en paquetería `wooldridge`.
**Descripción:** Relación de salario con el estado de pertenecer a un sindicato.

### `MOM.dat` - Conjunto de de datos de la oferta laboral de trabajadores en EU.
**Fuente:**  `Panel Survey of Income Dynamics J. Ziliak (1997), "Efficient Estimation With Panel Data when Instruments are Predetermined: An Empirical Comparison of Moment-Condition Estimators,"`
**Descripción:** Relación intertemporal de la oferta de trabajo con el salari.

### `EmplUK` - Empleo y salarios en Inglaterra
**Fuente:**  Descargue en paquetería `plm`.
**Descripción:** Relación intertemporal de la oferta de trabajo con el salari.


---

## 🛠️ Solución de Problemas

### Problemas Comunes

**1. Quarto no encontrado:**
```bash
# Instalar Quarto desde: https://quarto.org/docs/get-started/
```

**2. Errores de instalación de paquetes:**
```r
# Actualizar R a la última versión
# Para Ubuntu/Debian:
sudo apt-get update
sudo apt-get install r-base-dev

# Instalar dependencias del sistema para paquetes
sudo apt-get install libxml2-dev libcurl4-openssl-dev libssl-dev
```

**3. Falla el renderizado de presentaciones:**
```r
# Verificar instalación de Quarto
system("quarto check")

# Renderizar desde terminal en su lugar
system("quarto render slides/01_modelos_respuesta_binaria.qmd")
```

---

## 📚 Referencias y Recursos

### Libros de Texto Clave
- **Wooldridge, J.M.** (2010). *Econometric Analysis of Cross Section and Panel Data* (2da ed.). MIT Press.
- **Cameron, A.C. & Trivedi, P.K.** (2005). *Microeconometrics: Methods and Applications*. Cambridge University Press.
- **Greene, W.H.** (2018). *Econometric Analysis* (8va ed.). Pearson.
- **Angrist, J.D. & Pischke, J.S.** (2009). *Mostly Harmless Econometrics*. Princeton University Press.

### Artículos Clave
- Arellano, M., & Bond, S. (1991). Some tests of specification for panel data: Monte Carlo evidence and an application to employment equations. *Review of Economic Studies*, 58(2), 277-297.
- Blundell, R., & Bond, S. (1998). Initial conditions and moment restrictions in dynamic panel data models. *Journal of Econometrics*, 87(1), 115-143.
- Card, D., & Krueger, A.B. (1994). Minimum wages and employment: A case study of the fast-food industry in New Jersey and Pennsylvania. *American Economic Review*, 84(4), 772-793.
- Heckman, J.J. (1979). Sample selection bias as a specification error. *Econometrica*, 47(1), 153-161.

### Recursos en Línea
- [Documentación de Quarto](https://quarto.org/docs/guide/)
- [Viñeta del Paquete plm](https://cran.r-project.org/web/packages/plm/vignettes/plmPackage.html)
- [Econometría de Datos Panel en R](https://www.princeton.edu/~otorres/Panel101R.pdf)
- [Recursos de Diferencias en Diferencias](https://asjadnaqvi.github.io/DiD/)

*Ver carpeta `references/` para listas curadas.*

---

## 📝 ¿Cómo citar?

Si utiliza estos materiales en su investigación o enseñanza, por favor cite:

```bibtex
@misc{reco2025microeconometrics,
  author = {Sánchez-Rojas, Diego R.},
  title = {Diplomado de Economía UNAM - Módulo 4: Modelos Binarios y Datos Panel},
  year = {2025},
  publisher = {GitHub},
  url = {https://github.com/DiegoSReco/UNAM_Econometrics_Diploma_Microeconometrics}
}
```

Ver `CITATION.cff` para formato de citación legible por máquina.

---

## 📄 Licencia

Este trabajo está licenciado bajo [Creative Commons Atribución-NoComercial-CompartirIgual 4.0 Internacional](https://creativecommons.org/licenses/by-nc-sa/4.0/).

**Usted es libre de:**
- ✅ Compartir — copiar y redistribuir el material
- ✅ Adaptar — remezclar, transformar y construir sobre el material

**Bajo los siguientes términos:**
- 📌 Atribución — Debe dar crédito apropiado
- 🚫 NoComercial — No puede usar para fines comerciales
- 🔄 CompartirIgual — Debe distribuir bajo la misma licencia

---

## 🤝 Contribuciones

¡Las contribuciones son bienvenidas! Si encuentra errores, tiene sugerencias o desea agregar materiales:

1. Haga un fork del repositorio
2. Cree una rama de característica (`git checkout -b feature/mejora`)
3. Haga commit de sus cambios (`git commit -m 'Agregar mejora'`)
4. Haga push a la rama (`git push origin feature/mejora`)
5. Abra un Pull Request

---

## 📧 Contacto

**Mtro. Diego Sánchez Rojas**  
- 📧 Email: [diegosreco@gmail.com]
- 🐙 GitHub: [@DiegoSReco](https://github.com/DiegoSReco)

Para preguntas sobre el contenido del curso, por favor use GitHub Issues o contacte vía email.

---

## 🙏 Agradecimientos

- Departamento de Economía de la UNAM por apoyar este programa
- Estudiantes del Diplomado de Economía de la UNAM por retroalimentación valiosa

---

**Última Actualización:** Enero 2026
**Versión:** 1.0.0

---
