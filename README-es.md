# Herramientas para Análisis de Zonas Climáticas Locales e Islas de Calor Urbanas en R

<img align="right" src="https://github.com/ByMaxAnjos/LCZ4r/blob/main/inst/figures/logo.png?raw=true" alt="Logotipo LCZ4r" width="140">
  
<!-- badges: start -->
[![Ciclo de vida: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

El [**paquete LCZ4r**](https://github.com/ByMaxAnjos/LCZ4r) proporciona un conjunto integral de herramientas para analizar y visualizar **Zonas Climáticas Locales (LCZ)** e **Islas de Calor Urbanas (UHI)** en R. Diseñado para investigadores, planificadores urbanos y científicos del clima, LCZ4r simplifica el proceso de descarga, procesamiento e interpretación de datos de LCZ.

---

# 1. Instalación

El paquete `LCZ4r` está disponible en GitHub y se puede instalar de dos formas.  
Recomendamos la **Opción 1** para la mayoría de los usuarios.

## Opción 1: Instalar desde GitHub (Recomendado)

Esta es la forma más rápida de obtener la versión más reciente del paquete.

::: callout-tip
**Requisito previo:** Si ya tiene instalados los paquetes `remotes` o `devtools`, puede omitir este paso.
:::

```r
if (!require("remotes")) { install.packages("remotes")}
# Instalar o actualizar directamente desde GitHub
remotes::install_github("ByMaxAnjos/climasus4r", upgrade = "never")
```

::: callout-warning
Actualizaciones: `LCZ4r` está en desarrollo activo. Para actualizar, simplemente ejecute nuevamente el comando anterior.
R sobrescribirá automáticamente la versión anterior. Después de actualizar, se recomienda reiniciar la sesión de R
(Sesión > Reiniciar R).
:::

## Opción 2: Instalar desde Archivo Local (.zip)

Esta opción es recomendada para: conexiones de internet inestables, redes restringidas (ej.: entornos institucionales) y sesiones de capacitación con múltiples usuarios.

Pasos:

1. **Descargar el paquete**
👉 https://github.com/ByMaxAnjos/LCZ4r/archive/refs/heads/main.zip

2. **Extraer el archivo**
👉 Después de la descarga, extraiga el archivo .zip a una carpeta local (ej.: Descargas o Escritorio)

3. **Instalar localmente**

```r
# IMPORTANTE: Ajuste la ruta a donde extrajo la carpeta
remotes::install_local(
  "C:/Ruta/a/su/carpeta/LCZ4r-main", 
  upgrade = "never"
)
```

::: callout-tip
Ejemplo: Si descargó y extrajo el archivo en su carpeta de Descargas:

```r
remotes::install_local(
   "/Users/maxanjos/Downloads/LCZ4r-main",
  upgrade = "never"
)
```
:::

## Cargando el paquete

Después de la instalación, cargue el paquete cada vez que inicie una nueva sesión de R:

```r
library(LCZ4r)
```

---

## [Ejecute LCZ4r en Posit Cloud, sin necesidad de instalar RStudio.](https://bymaxanjos.github.io/LCZ4r/en/articles/posit_cloud.html)

<a href="https://posit.cloud/content/9921467" target="_blank">
  <p style="text-align:center;">
    <img src="https://github.com/ByMaxAnjos/LCZ4r/raw/main/inst/figures/posit_cloud.png" 
         alt="Posit Cloud" 
         width="60%" 
         style="border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); transition: transform 0.3s ease;">
  </p>
  <p style="text-align:center;">
    (Haga clic en la imagen para explorar LCZ4r en Posit Cloud.)
  </p>
</a>

---

## Inspiración

El paquete **LCZ4r** está inspirado en los siguientes trabajos:

- **Stewart, I., and T. Oke, 2012**: [Local Climate Zones for Urban Temperature Studies](https://doi.org/10.1175/BAMS-D-11-00019.1).

- **Ching, J., et al., 2018**: [WUDAPT: An Urban Weather, Climate, and Environmental Modeling Infrastructure for the Anthropocene](https://doi.org/10.1175/BAMS-D-16-0236.1).
- **Demuzere, M., et al., 2019**: [Mapping Europe into Local Climate Zones](https://doi.org/10.1371/journal.pone.0214474).
- **Demuzere, M., et al., 2020**: [Combining Expert and Crowd-Sourced Training Data to Map Urban Form and Functions for the Continental US](https://doi.org/10.1038/s41597-020-00605-z).
- **Demuzere, M., et al., 2022**: [A Global Map of Local Climate Zones to Support Earth System Modelling and Urban-Scale Environmental Science](https://doi.org/10.5194/essd-14-3835-2022).

---

## ¿Tiene Comentarios o Sugerencias?

Valoramos su opinión. Si tiene ideas para mejorar o detecta algún problema, por favor, háganoslo saber abriendo un issue en GitHub.

<button type="button" class="btn" style="background-color: #008000; color: white; padding: .5rem 1rem; font-size: 1rem; border: none; border-radius: .25rem;">
  <a href='https://github.com/ByMaxAnjos/LCZ4r/issues/new' style="text-decoration: none; color: white;">
    Abrir un Issue en GitHub
  </a>
</button>
```
