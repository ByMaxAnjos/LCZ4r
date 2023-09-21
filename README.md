# ZCCM::Tools for Urban Heat Islands and Local Climate Zones Analysis in R

## Introduction

As part of **Zoom City Carbon Model (ZCCM)**, we present the **LCZ4r**, a set of R functions which models Urban Heat Island at high-definition using Local Climate Zone classification and local air temperature readings.

Please note that **LCZ4r package R** is currently undergoing develop, and caution is advised when interpreting its outcomes. Our methodology is based on Anjos M., Madeiros D, Meier F, Castelhano F. Tools for Urban Heat Islands and Local Climate Zones Analysis in R(in preparation).

### People

The development of the **LCZ4r** has been led by [Dr. Max Anjos](https://www.researchgate.net/profile/Max-Anjos/research) and joined by Dr.Fred Meier. It is hosted at the [Chair of Climatology, Institute of Ecology, Technische Universität Berlin](https://www.klima.tu-berlin.de/index.php?show=home_start&lan=en). People engaged in it:

–> Dayvid Carlos de Medeiros, Center for Climate Crisis Studies, Departament of Geography, Federal University of Rio Grande do Norte, Brazil.

–> Dr. Francisco Castelhano, Center for Climate Crisis Studies, Departament of Geography, Federal University of Rio Grande do Norte, Brazil.

### Funding

This project is was financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior (CAPES) – Finance Code 001, and by the Alexander Von Humboldt Foundation.

### Contact

Please feel free to contact us if you have any questions or suggestions by emailing [maxanjos\@campus.ul.pt](mailto:maxanjos@campus.ul.pt). If you are interested in contributing to the development of this R package, we welcome you to join our team.

Happy coding!

## Install 

To install LCZ4r:

```{r setup, include=TRUE}
#Apply the function
devtools::install_github("ByMaxAnjos/LCZ4r")

```

## Get your LCZ map

This function gets LCZ for a specified region of interest (ROI) using global LCZ mapping.

```{r setup, include=TRUE}
#Apply the function
lcz_map <- getLCZmap(city="Berlin")
plotLCZmap(lcz_map)

```
<img width="1217" alt="Screenshot 2023-08-13 at 18 27 13" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/6ca761e5-5fd4-4148-9a6f-c22b5306c10b">


## Hands-on 34 LCZ parameters

This nice function gets all LCZ parameters (including, min, max, and mean) from Stewart and Oke (2012) and converts them to shapefile or raster stack.

```{r setup, include=TRUE}

#Apply the function
LCZpar <- getLCZparameters(lcz_map, iStack = TRUE)

```
<img width="1439" alt="Screenshot 2023-08-13 at 19 19 06" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/e9006776-a336-4303-bc35-f787090a1caf">


## Calculate the LCZ area

This function calculates the LCZ area like this:

```{r setup, include=TRUE}
#Apply the function
LCZarea <- LCZarea(lcz_map, iplot = TRUE)
LCZarea

```
<img width="1440" alt="Screenshot 2023-08-13 at 19 24 47" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/2f2fcd9f-1744-47a9-850a-5ad0630aae3b">


## UHI analysis

To ensure the model runs correctly, it is necessary to load the following inputs:

1.  Air temperature data .csv (required) with a minimum of four columns labeled *date*, *Latitude*, *Longitude*, *airT*.
2.  Other variables (optional) should have the same date column recommendation.

Note that the model converts the date-time into a R-formatted version, e.g., "2023-03-13 11:00:00" or "2023-03-13".

The following dataframe is presented as follows:

```{r setup, include=TRUE}
air_UCON %>% head(10)
```

## Calculate thermal anomaly between LCZ

<img width="1440" alt="Screenshot 2023-08-19 at 13 44 11" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/3594539f-0cfd-4671-9a1f-fc65375bc442">


## Interpolate air temperature with LCZ

it's coming soon... but have look at this:

<img width="1367" alt="Screenshot 2023-08-13 at 19 33 02" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/ecc209b1-0ef5-4554-a141-3feb26d0a623">


## Calculate UHI intensity

it's coming soon...

