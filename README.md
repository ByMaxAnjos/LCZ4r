# Tools for Local Climate Zone and Urban Heat Island Analysis in R

<img align="right" src="https://github.com/ByMaxAnjos/LCZ4r/blob/main/inst/figures/logo.png?raw=true" alt="LCZ4r Logo" width="140">
  
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The [**LCZ4r package**](https://github.com/ByMaxAnjos/LCZ4r) provides a comprehensive suite of tools for analyzing and visualizing **Local Climate Zones (LCZ)** and **Urban Heat Islands (UHI)** in R. Designed for researchers, urban planners, and climate scientists, LCZ4r simplifies the process of downloading, processing, and interpreting LCZ data.

---

## Scientific publication

The **LCZ4r** package is supported by the following peer-reviewed publication:

[![Nature Scientific Reports](https://img.shields.io/badge/Published%20in-Nature%20Scientific%20Reports-2A6B92)](https://www.nature.com/scientificreports)

<a href="https://www.nature.com/articles/s41598-025-92000-0" target="_blank">
  <p style="text-align:center;">
    <img src="https://github.com/ByMaxAnjos/LCZ4r/raw/main/inst/figures/paper_nature.png" 
         alt="LCZ4r Publication in Nature Scientific Reports" 
         width="60%" 
         style="border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); transition: transform 0.3s ease;">
  </p>
  <p style="text-align:center;">
    (Click the image to explore the full publication in Nature Scientific Reports.)
  </p>
</a>

---

## 🗃️ Installation in R

To install the **LCZ4r package**, use the development version from GitHub. This ensures you have access to the latest features and improvements.

### Step 1: Install the remotes Package

If you don’t already have the remotes package installed, run the following command:

**if(!require(remotes)) install.packages("remotes")**

### Step 2: Install LCZ4r from GitHub

Use the remotes::install_github() function to install the LCZ4r package:

**remotes::install_github("ByMaxAnjos/LCZ4r", upgrade = "never")**

Once installed, load the package to start using its functions:

**library(LCZ4r)**

---

## Package Overview

The **LCZ4r** package includes the following key functions:

- **Data Download**:
  - `lcz_get_map()`: Download LCZ maps from the global dataset.
  - `lcz_get_map_euro()`: Download LCZ maps from the European dataset.
  - `lcz_get_map_usa()`: Download LCZ maps from the Continental United States dataset.
  - `lcz_get_map_generator()`: Download LCZ maps from the LCZ Generator Platform.

- **Visualization**:
  - `lcz_plot_map()`: Visualize LCZ maps.
  - `lcz_plot_parameters()`: Visualize LCZ parameter maps.
  - `lcz_plot_interp()`: Visualize LCZ interpolation results.

- **Analysis**:
  - `lcz_cal_area()`: Calculate the area of LCZ classes.
  - `lcz_get_parameters()`: Retrieve LCZ parameters.
  - `lcz_ts()`: Analyze LCZ time series.
  - `lcz_anomaly()`: Calculate LCZ thermal anomalies.
  - `lcz_anomaly_map()`: Map LCZ thermal anomalies.
  - `lcz_interp_map()`: Map LCZ interpolation results.
  - `lcz_interp_eval()`: Evaluate LCZ interpolation accuracy.
  - `lcz_uhi_intensity()`: Assess urban heat island intensity using LCZ data.

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

The [**LCZ4r-QGIS plugin**](https://bymaxanjos.github.io/LCZ4r/articles/Introd_QGIS_LCZ4r.html) integrates the LCZ4r package with **QGIS**, enabling users to analyze Local Climate Zones and urban heat islands directly within the QGIS environment. The plugin supports multiple languages, making it accessible to a global audience.

<img width="1217" alt="LCZ4r-QGIS Plugin Screenshot" src="https://github.com/user-attachments/assets/68cdca10-c1d5-4755-8d73-351af809552a">

---

## People

The development of the **LCZ4r** package is led by [**Dr. Max Anjos**](https://www.researchgate.net/profile/Max-Anjos/research) and supported by a team of researchers:

With the following contributors:

- **Dr. Fred Meier**, Chair of Climatology, Institute of Ecology, Technische Universität Berlin (fred.meier@tu-berlin.de).
- **Dr. Francisco Castelhano**, Center for Climate Crisis Studies, Department of Geography, Federal University of Rio Grande do Norte, Brazil (fjcastelhano@gmail.com).
- **Dayvid Carlos de Medeiros**, Center for Climate Crisis Studies, Department of Geography, Federal University of Rio Grande do Norte, Brazil (Dayvid.medeiros.123@ufrn.edu.br).
- **Antônio Campos Neto**, Center for Climate Crisis Studies, Department of Geography, Federal University of Rio Grande do Norte, Braziln (antoniocamposneto9@gmail.com).
- **José Felipe da Costa Neto**, Department of Geography, Federal University of Rio Grande do Norte, Braziln (jose.felipe.124@ufrn.edu.br).

---

## Funding

This project is supported by:
- **Coordenação de Aperfeiçoamento de Pessoal de Nível Superior (CAPES)** – Finance Code 001.
- **Alexander von Humboldt Foundation**.

---

## Contact

For questions, suggestions, or collaboration opportunities, please contact us at [maxanjos@campus.ul.pt](mailto:maxanjos@campus.ul.p). We welcome contributions to the development of this R package!

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
