# Tools for Local Climate Zone and Urban Heat Island Analysis in R

<img align="right" src="https://github.com/ByMaxAnjos/LCZ4r/blob/main/inst/figures/logo.png?raw=true" alt="LCZ4r Logo" width="140">
  
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The [**LCZ4r package**](https://github.com/ByMaxAnjos/LCZ4r) provides a comprehensive suite of tools for analyzing and visualizing **Local Climate Zones (LCZ)** and **Urban Heat Islands (UHI)** in R. Designed for researchers, urban planners, and climate scientists, LCZ4r simplifies the process of downloading, processing, and interpreting LCZ data.

---

# 1. Installation

The `LCZ4r` package is available on GitHub and can be installed in two ways.  
We recommend **Option 1** for most users.

---

## Option 1: Install from GitHub (Recommended)

This is the fastest way to get the latest version of the package.

::: callout-tip
**Prerequisite:** If you already have `remotes` or `devtools` installed, you can skip this step.
:::

```r
if (!require("remotes")) { install.packages("remotes")}
# Install or update directly from GitHub
remotes::install_github("ByMaxAnjos/LCZ4r", upgrade = "never")
```
::: callout-warning
Updates: `LCZ4r` is under active development. To update, simply run the command above again.
R will automatically overwrite the previous version. After updating, it is recommended to restart your R session
(Session > Restart R).
:::

## Option 2: Install from Local File (.zip)

This option is recommended for: unstable internet connections, restricted networks (e.g., institutional environments), and training sessions with multiple users.

Steps:

1. **Download the package**
👉 https://github.com/ByMaxAnjos/LCZ4r/archive/refs/heads/main.zip

2. **Extract the file**
👉 After downloading, extract the .zip file to a local folder (e.g., Downloads or Desktop)

3. **Install locally**

```r
# IMPORTANT: Adjust the path to where you extracted the folder
remotes::install_local(
  "C:/Path/to/your/folder/LCZ4r-main", 
  upgrade = "never"
)
```
::: callout-tip
Example: If you downloaded and extracted the file to your Downloads folder:

```r
remotes::install_local(
   "/Users/maxanjos/Downloads/LCZ4r-main",
  upgrade = "never"
)
```
:::

## Loading the package

After installation, load the package whenever you start a new R session:
```r
library(LCZ4r)
```

---

## [Run LCZ4r in Posit Cloud, no RStudio installation required!](https://bymaxanjos.github.io/LCZ4r/articles/posit_cloud.html)

<a href="https://posit.cloud/content/9921467" target="_blank">
  <p style="text-align:center;">
    <img src="https://github.com/ByMaxAnjos/LCZ4r/raw/main/inst/figures/posit_cloud.png" 
         alt="Posit Cloud" 
         width="60%" 
         style="border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); transition: transform 0.3s ease;">
  </p>
  <p style="text-align:center;">
    (Click the image to explore the LCZ4r in Posit Cloud.)
  </p>
</a>

---

## LCZ4r-QGIS Plugin: Multilingual Integration

The [**LCZ4r-QGIS plugin**](https://bymaxanjos.github.io/LCZ4r/en/articles/examples.html) integrates the LCZ4r package with **QGIS**, enabling users to analyze Local Climate Zones and urban heat islands directly within the QGIS environment. The plugin supports multiple languages, making it accessible to a global audience.

<img width="1217" alt="LCZ4r-QGIS Plugin Screenshot" src="https://github.com/user-attachments/assets/68cdca10-c1d5-4755-8d73-351af809552a">


---

## Inspiration

The **LCZ4r** package is inspired by the following works:

- **Stewart, I., and T. Oke, 2012**: [Local Climate Zones for Urban Temperature Studies](https://doi.org/10.1175/BAMS-D-11-00019.1).

- **Ching, J., et al., 2018**: [WUDAPT: An Urban Weather, Climate, and Environmental Modeling Infrastructure for the Anthropocene](https://doi.org/10.1175/BAMS-D-16-0236.1).
- **Demuzere, M., et al., 2019**: [Mapping Europe into Local Climate Zones](https://doi.org/10.1371/journal.pone.0214474).
- **Demuzere, M., et al., 2020**: [Combining Expert and Crowd-Sourced Training Data to Map Urban Form and Functions for the Continental US](https://doi.org/10.1038/s41597-020-00605-z).
- **Demuzere, M., et al., 2022**: [A Global Map of Local Climate Zones to Support Earth System Modelling and Urban-Scale Environmental Science](https://doi.org/10.5194/essd-14-3835-2022).

---

## Have Feedback or Suggestions?

We value your input! If you have ideas for improvement or spot any issues, please let us know by opening an issue on GitHub.

<button type="button" class="btn" style="background-color: #008000; color: white; padding: .5rem 1rem; font-size: 1rem; border: none; border-radius: .25rem;">
  <a href='https://github.com/ByMaxAnjos/LCZ4r/issues/new' style="text-decoration: none; color: white;">
    Open an Issue on GitHub
  </a>
</button>
