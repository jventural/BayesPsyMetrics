<p align="center">
 <img src="https://github.com/jventural/BayesPsyMetrics/blob/master/logo_bayes.png" alt="BayesPsyMetrics" width="200" height="200"/>

</p>

<h1 align="center">BayesPsyMetrics</h1>

<p align="center">
    Una paquetería en R para análisis psicométricos desde un enfoque bayesiano.
    <br />
    <a href="https://joseventuraleon.com/"><strong>Explorar la página web del autor »</strong></a>
    <br />
    <br />
</p>

<!-- BADGES -->
<p align="center">
  <img src="https://www.r-pkg.org/badges/version/BayesPsyMetrics" alt="CRAN version"/>
</p>

## Installation

You can install the latest version of BayesPsyMetrics from GitHub with the help of the devtools package:

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("jventural/BayesPsyMetrics")
```

## Examples

### Boxplot de cargas factoriales bayesianas
```r
library(BayesPsyMetrics)
plot_bfactor_loadings(fit)
```

---

### Histogramas de cargas por ítem
```r
plot_bhist_loadings(fit)
```

---

### Intervalos de credibilidad 95% para ítems
```r
plot_binterval_loadings(fit)
```

---

### Proporción de draws que superan el umbral de 0.50
```r
# Para un ítem específico
calc_prob_gt(fit, "F1=~bif2", 0.80)

# Para todos los ítems
calc_prob_gt(fit, "All", 0.80)
```

---

### Barras apiladas con proporciones por ítem
```r
plot_bayes_bar_proportion(fit)
```

---

### Histogramas para índices BRMSEA y BCFI
```r
plot_bayes_indices(bfit, col_brmsea = "#009E73", col_bcfI = "#E69F00")
```

---

## Author

📦 Desarrollado por **José Ventura León**  
🔗 https://joseventuraleon.com/  
📧 jose.ventura@example.com
