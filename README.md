# UNAM Economics Diploma - Module 4: Microeconometrics
## Binary Response Models & Panel Data Analysis

[![License: CC BY-NC-SA 4.0](https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/)
[![R Version](https://img.shields.io/badge/R-%E2%89%A5%204.0.0-blue.svg)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-%E2%89%A5%201.3-75AADB.svg)](https://quarto.org/)

**Instructor:** Diego Sánchez Rojas
**Institution:** Universidad Nacional Autónoma de México (UNAM)  
**Program:** Economics Diploma - Econometrics Specialization  
**Module:** Stage 4 - Microeconometrics

---

## 📚 Course Overview

This repository contains teaching materials for **Module 4** of the UNAM Economics Diploma, covering advanced microeconometric techniques with focus on:

- **Binary Response Models** (LPM, Logit, Probit)
- **Limited Dependent Variables** (Tobit, Heckit)
- **Panel Data Methods** (Fixed Effects, Random Effects, First Differences, Difference-in-Difference)
- **Instrumental Variables in Panel Data** (2SLS, GMM)
- **Dynamic Panel Models** (Arellano-Bond, Arellano-Bover)

All materials are implemented in **R** with **Quarto** for reproducible presentations.

---

## 📂 Repository Structure

```
.
├── README.md                          # This file
├── CITATION.cff                       # Citation information
├── LICENSE                            # License information
│
├── slides/                            # Course presentations (.qmd files)
│   ├── 01_binary_response_models.qmd
│   ├── 02_linear_probability_model.qmd
│   ├── 03_tobit_selection.qmd
│   ├── 04_panel_data_intro.qmd
│   ├── 05_dynamic_panel_I.qmd
│   ├── 06_dynamic_panel_II.qmd
│   ├── 07_intermediate_microeconometrics.qmd
│   └── references.bib                 # Bibliography
│
├── code/                              # R scripts for empirical applications
│   ├── 01_binary_models.R
│   ├── 02_tobit_heckman.R
│   ├── 03_panel_basics.R
│   ├── 04_panel_gdp_lifeexpectancy.R
│   ├── 05_panel_wage_union.R
│   ├── 06_panel_wage_hours.R
│   ├── 07_diff_in_diff_minwage.R
│   ├── 08_arellano_bond.R
│   └── README.md                      # Code documentation
│
├── data/                              # Datasets
│   ├── MOM.dat                        # Mothers dataset
│   └── README.md                      # Data documentation
│
├── exercises/                         # Problem sets (to be added)
│   └── README.md
│
├── output/                            # Generated outputs
│   ├── figures/
│   └── tables/
│
└── references/                        # Additional resources
    ├── key_papers.md
    └── software_resources.md
```

---

## 🎯 Learning Objectives

By the end of this module, students will be able to:

1. ✅ **Estimate and interpret** binary choice models (Logit, Probit, Multinomial)
2. ✅ **Apply** limited dependent variable techniques (Tobit, Heckit)
3. ✅ **Analyze panel data** using fixed effects, random effects, and first differences
4. ✅ **Address endogeneity** in panel data using IV and GMM estimators
5. ✅ **Implement dynamic panel models** (Arellano-Bond, System GMM)
6. ✅ **Conduct causal inference** using Difference-in-Differences
7. ✅ **Interpret and validate** econometric results critically

---

## 🚀 Getting Started

### Prerequisites

**Software Requirements:**
- [R](https://www.r-project.org/) (≥ 4.0.0)
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended)
- [Quarto](https://quarto.org/docs/get-started/) (≥ 1.3) for rendering presentations

**Required R Packages:**

```r
# Core packages
install.packages(c(
  "tidyverse",      # Data manipulation and visualization
  "plm",            # Panel data models
  "lmtest",         # Diagnostic tests
  "sandwich",       # Robust standard errors
  "AER",            # Applied Econometrics with R
  "sampleSelection" # Tobit and Heckit models
))

# Advanced panel data
install.packages(c(
  "pdynmc",         # Dynamic panel models (Arellano-Bond)
  "panelView",      # Panel data visualization
  "fixest",         # Fast fixed effects
  "did"             # Difference-in-Differences
))

# Additional utilities
install.packages(c(
  "stargazer",      # LaTeX/HTML tables
  "modelsummary",   # Modern regression tables
  "ggplot2",        # Graphics
  "knitr",          # Dynamic reports
  "kableExtra"      # Enhanced tables
))
```

### Quick Start

1. **Clone the repository:**
```bash
git clone https://github.com/DiegoSReco/UNAM_Econometrics_Diploma_Microeconometrics.git
cd UNAM_Econometrics_Diploma_Microeconometrics
```

2. **Open RStudio and set working directory:**
```r
setwd("path/to/UNAM_Econometrics_Diploma_Microeconometrics")
```

3. **Run example scripts:**
```r
# Binary models example
source("code/01_binary_models.R")

# Panel data example
source("code/03_panel_basics.R")
```

4. **Render slides (requires Quarto):**
```bash
quarto render slides/01_binary_response_models.qmd
```

---

## 📖 Course Content

### Module 1: Binary Response Models
**Slides:** `01_binary_response_models.qmd`  
**Code:** `01_binary_models.R`

- Linear Probability Model (LPM)
- Logit and Probit models
- Marginal effects and interpretation
- Multinomial and ordered models
- Model comparison and diagnostics

### Module 2: Limited Dependent Variables
**Slides:** `03_tobit_selection.qmd`  
**Code:** `02_tobit_heckman.R`

- Censored and truncated data
- Tobit models (Type I, II, III)
- Sample selection models (Heckman two-step)
- Applications: labor supply, wages

### Module 3: Panel Data Fundamentals
**Slides:** `04_panel_data_intro.qmd`  
**Code:** `03_panel_basics.R`, `04_panel_gdp_lifeexpectancy.R`

- Panel data structure and advantages
- Pooled OLS vs. Panel estimators
- Fixed Effects (FE) and Random Effects (RE)
- Hausman test
- First Differences
- Applications: GDP and life expectancy

### Module 4: Panel Data with Endogeneity
**Slides:** `04_panel_data_intro.qmd` (advanced sections)  
**Code:** `05_panel_wage_union.R`, `06_panel_wage_hours.R`

- Instrumental Variables in panel data
- Two-Stage Least Squares (2SLS)
- Generalized Method of Moments (GMM)
- Testing for weak instruments
- Applications: wage equations, union membership

### Module 5: Dynamic Panel Models I
**Slides:** `05_dynamic_panel_I.qmd`  
**Code:** `08_arellano_bond.R`

- Dynamic panel bias
- First-difference GMM
- GMM estimation
- PGMM estimator
- Instrument validity tests (Sargan, Hansen)

### Module 6: Dynamic Panel Models II
**Slides:** `06_dynamic_panel_II.qmd`  
**Code:** `08_arellano_bond.R` (System GMM section)

- Arellano-Bond estimator
- Arellano-Bover estimator
- Combining levels and differences
- Empirical applications

### Module 7: Causal Inference - Difference-in-Differences
**Slides:** `07_intermediate_microeconometrics.qmd`  
**Code:** `07_diff_in_diff_minwage.R`

- DiD framework and assumptions
- Parallel trends assumption
- Card-Krueger minimum wage study
- Event study designs
- Robust standard errors

---

## 📊 Datasets

### `MOM.dat` - Mothers Dataset
**Source:** Mroz (1987), via Wooldridge textbooks  
**Description:** Labor force participation and wages of married women  
**Variables:**
- `hours`: Annual hours worked
- `wage`: Hourly wage rate
- `educ`: Years of education
- `exper`: Years of work experience
- `age`: Age in years
- `kidslt6`: Number of children < 6 years old
- `kidsge6`: Number of children ≥ 6 years old
- `nwifeinc`: Non-wife household income

**Applications:**
- Binary models (labor force participation)
- Tobit models (hours worked, wages)
- Sample selection models (Heckit)

*See `data/README.md` for complete data documentation.*

---

## 🛠️ Troubleshooting

### Common Issues

**1. Quarto not found:**
```bash
# Install Quarto from: https://quarto.org/docs/get-started/
```

**2. Package installation errors:**
```r
# Update R to latest version
# For Ubuntu/Debian:
sudo apt-get update
sudo apt-get install r-base-dev

# Install system dependencies for packages
sudo apt-get install libxml2-dev libcurl4-openssl-dev libssl-dev
```

**3. Rendering slides fails:**
```r
# Check Quarto installation
system("quarto check")

# Render from terminal instead
system("quarto render slides/01_binary_response_models.qmd")
```

---

## 📚 References & Resources

### Key Textbooks
- **Wooldridge, J.M.** (2010). *Econometric Analysis of Cross Section and Panel Data* (2nd ed.). MIT Press.
- **Cameron, A.C. & Trivedi, P.K.** (2005). *Microeconometrics: Methods and Applications*. Cambridge University Press.
- **Greene, W.H.** (2018). *Econometric Analysis* (8th ed.). Pearson.
- **Angrist, J.D. & Pischke, J.S.** (2009). *Mostly Harmless Econometrics*. Princeton University Press.

### Key Papers
- Arellano, M., & Bond, S. (1991). Some tests of specification for panel data: Monte Carlo evidence and an application to employment equations. *Review of Economic Studies*, 58(2), 277-297.
- Blundell, R., & Bond, S. (1998). Initial conditions and moment restrictions in dynamic panel data models. *Journal of Econometrics*, 87(1), 115-143.
- Card, D., & Krueger, A.B. (1994). Minimum wages and employment: A case study of the fast-food industry in New Jersey and Pennsylvania. *American Economic Review*, 84(4), 772-793.
- Heckman, J.J. (1979). Sample selection bias as a specification error. *Econometrica*, 47(1), 153-161.

### Online Resources
- [Quarto Documentation](https://quarto.org/docs/guide/)
- [plm Package Vignette](https://cran.r-project.org/web/packages/plm/vignettes/plmPackage.html)
- [Panel Data Econometrics in R](https://www.princeton.edu/~otorres/Panel101R.pdf)
- [Difference-in-Differences Resources](https://asjadnaqvi.github.io/DiD/)

*See `references/` folder for curated lists.*

---

## 📝 Citation

If you use these materials in your research or teaching, please cite:

```bibtex
@misc{reco2025microeconometrics,
  author = {Sánchez-Rojas, Diego},
  title = {UNAM Economics Diploma - Module 4: Binary Models and Panel Data},
  year = {2025},
  publisher = {GitHub},
  url = {https://github.com/DiegoSReco/UNAM_Econometrics_Diploma_Microeconometrics}
}
```

See `CITATION.cff` for machine-readable citation format.

---

## 📄 License

This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-nc-sa/4.0/).

**You are free to:**
- ✅ Share — copy and redistribute the material
- ✅ Adapt — remix, transform, and build upon the material

**Under the following terms:**
- 📌 Attribution — You must give appropriate credit
- 🚫 NonCommercial — You may not use for commercial purposes
- 🔄 ShareAlike — You must distribute under the same license

---

## 🤝 Contributing

Contributions are welcome! If you find errors, have suggestions, or want to add materials:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/improvement`)
3. Commit your changes (`git commit -m 'Add improvement'`)
4. Push to the branch (`git push origin feature/improvement`)
5. Open a Pull Request

---

## 📧 Contact

**Diego S. Reco**  
- 📧 Email: [diegosreco@gmail.com]
- 🐙 GitHub: [@DiegoSReco](https://github.com/DiegoSReco)
- 🏛️ Institution: Universidad Nacional Autónoma de México (UNAM)

For questions about the course content, please use GitHub Issues or contact via email.

---

## 🙏 Acknowledgments

- UNAM Economics Department for supporting this program
- Students of the UNAM Economics Diploma for valuable feedback
- Open-source R community for excellent econometric packages
- Quarto team for making reproducible presentations accessible

---

**Last Updated:** January 2025  
**Version:** 1.0.0

---
