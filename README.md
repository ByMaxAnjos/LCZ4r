# Tools for Local Climate Zones Analysis and Urban Heat Islands in R

## Introduction

<img align="right" src="https://github.com/ByMaxAnjos/LCZ4r/blob/main/inst/figures/logo.png?raw=true" alt="logo" width="140"> 

As part of **Zoom City Carbon Model (ZCCM)**, we present the **LCZ4r**, a set of R functions which models Urban Heat Island at high-definition using Local Climate Zone classification and local air temperature readings.

Please note that [LCZ4r package R](https://bymaxanjos.github.io/LCZ4r/) is currently undergoing develop, and caution is advised when interpreting its outcomes. Our methodology is based on Anjos M., Madeiros D, Meier F, Castelhano F. LCZ4r, an R package for Urban Local Climate Zones and Heat Islands Analysis (in preparation).

Happy coding!

## Install 

To install LCZ4r use the development version with latest features:

```{r, include=FALSE}

install.packages("devtools")
devtools::install_github("ByMaxAnjos/LCZ4r")

library(LCZ4r)

```

## Overview of the package

To date, the **LCZ4r** package includes 5 functions:

- `lcz_get_map()` - Get you LCZ map
- `lcz_plot_map()` - Plot you LCZ map
- `lcz_cal_area()` - Calculate the LCZ area
- `lcz_get_parameters()` - Get LCZ parameters
- `lcz_plot_parameters()` - Plot LCZ parameters


## Basic Usage

This function gets LCZ for your city or specified region of interest (ROI) using global LCZ mapping from Stewart and Oke (2012) and Demuzere et al., (2022).

```{r, include=FALSE}

lcz_map <- lcz_get_map(city="Berlin")

lcz_plot_map(lcz_map)

```
![lcz_PlotMap](https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/d1f3e0b8-bd05-464b-b52d-932fa5cf77a2)


This nice function gets all LCZ parameters (including, min, max, and mean) from Stewart and Oke (2012) and converts them to shapefile or raster stack.

```{r, include=FALSE}

LCZpar <- lcz_get_parameters(lcz_map, iStack = TRUE)

lcz_plot_parameters(LCZpar, iselect = "SVF1")

```
<img width="1439" alt="Screenshot 2023-08-13 at 19 19 06" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/e9006776-a336-4303-bc35-f787090a1caf">


This function calculates the LCZ area like this:

```{r, include=FALSE}

LCZarea <- lcz_cal_area(lcz_map, iplot = TRUE)

LCZarea

```
<img width="1440" alt="Screenshot 2023-08-13 at 19 24 47" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/2f2fcd9f-1744-47a9-850a-5ad0630aae3b">


## UHI analysis

To ensure the model runs correctly, it is necessary to load the following inputs:

1.  Air temperature data .csv (required) with a minimum of four columns labeled *date*, *Latitude*, *Longitude*, *airT*.
2.  Other variables (optional) should have the same date column recommendation.

Note that the model converts the date-time into a R-formatted version, e.g., "2023-03-13 11:00:00" or "2023-03-13".

The following dataframe is presented as follows:

```{r, include=FALSE}

air_UCON %>% head(10)

```

## Calculate thermal anomaly between LCZ

<img width="1440" alt="Screenshot 2023-08-19 at 13 44 11" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/3594539f-0cfd-4671-9a1f-fc65375bc442">


## Interpolate air temperature with LCZ

it's coming soon... but have look at this:

<img width="1367" alt="Screenshot 2023-08-13 at 19 33 02" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/ecc209b1-0ef5-4554-a141-3feb26d0a623">


## Calculate UHI intensity

it's coming soon...


### People

The development of the **LCZ4r** has been led by [Dr. Max Anjos](https://www.researchgate.net/profile/Max-Anjos/research) and joined by Dr.Fred Meier. It is hosted at the [Chair of Climatology, Institute of Ecology, Technische Universität Berlin](https://www.klima.tu-berlin.de/index.php?show=home_start&lan=en). People engaged in it:

–> Dayvid Carlos de Medeiros, Center for Climate Crisis Studies, Departament of Geography, Federal University of Rio Grande do Norte, Brazil.

–> Dr. Francisco Castelhano, Center for Climate Crisis Studies, Departament of Geography, Federal University of Rio Grande do Norte, Brazil.

### Funding

This project is was financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior (CAPES) – Finance Code 001, and by the Alexander von Humboldt Foundation.

### Contact

Please feel free to contact us if you have any questions or suggestions by emailing [maxanjos\@campus.ul.pt](mailto:maxanjos@campus.ul.pt). If you are interested in contributing to the development of this R package, we welcome you to join our team.
