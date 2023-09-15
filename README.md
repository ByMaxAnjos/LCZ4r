# ZCCM::Tools for Urban Heat Islands and Local Climate Zones Analysis in R

## Introduction

As part of **Zoom City Carbon Model (ZCCM)**, we present the **ZCCM::UHI**, a set of R functions which models Urban Heat Island at high-definition using Local Climate Zone classification and local air temperature readings.

Please note that **ZCCM::UHI** is currently undergoing peer-review, and caution is advised when interpreting its outcomes. Our methodology is based on Anjos, M.; Meier, F. City Carbon Budget and hourly net CO2 fluxes at 0.00025º resolution (30-meter grid cell) for informed climate action(in preparation).

### People

The development of the ZCCM::UHI was led by [Dr. Max Anjos](https://www.researchgate.net/profile/Max-Anjos/research) and joined by Dr.Fred Meier, and it is hosted at the [Chair of Climatology, Institute of Ecology, Technische Universität Berlin](https://www.klima.tu-berlin.de/index.php?show=home_start&lan=en). It also has been joined by:

–> Dayvid Carlos de Medeiros, Center for Climate Crisis Studies, Departament of Geography, Federal University of Rio Grande do Norte, Brazil.

–> Dr. Francisco Castelhano, Center for Climate Crisis Studies, Departament of Geography, Federal University of Rio Grande do Norte, Brazil.

### Funding

This project is was financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior (CAPES) – Finance Code 001, and by the Alexander Von Humboldt Foundation.

### Contact

Please feel free to contact us if you have any questions or suggestions by emailing [maxanjos\@campus.ul.pt](mailto:maxanjos@campus.ul.pt). If you are interested in contributing to the development of the model, we welcome you to join our team.

Happy coding!

## Get your LCZ map

This function gets LCZ for a specified region of interest (ROI) using global LCZ mapping.

```{r setup, include=TRUE}
#Apply the function
lcz_map <- getLCZmap(city="Berlin")
plotLCZmap(lcz_map)

```
<img width="1217" alt="Screenshot 2023-08-13 at 18 27 13" src="https://github.com/ByMaxAnjos/Urban-Heat-Islands/assets/94705218/b9c69544-205e-4c0c-a9e4-3a2591f9aeeb">

## Hands-on 34 LCZ parameters

This nice function gets all LCZ parameters (including, min, max, and mean) from Stewart and Oke (2012) and converts them to shapefile or raster stack.

```{r setup, include=TRUE}

#Apply the function
LCZpar <- getLCZparameters(lcz_map, iStack = TRUE)

```

<img width="1439" alt="Screenshot 2023-08-13 at 19 19 06" src="https://github.com/ByMaxAnjos/Urban-Heat-Islands/assets/94705218/2f7c2bb0-547f-4a82-99d0-97d87cddbbf6">

## Calculate the LCZ area

This function calculates the LCZ area like this:

```{r setup, include=TRUE}
#Apply the function
LCZarea <- calLCZarea(lcz_map, iplot = TRUE)
LCZarea

```
<img width="1440" alt="Screenshot 2023-08-13 at 19 24 47" src="https://github.com/ByMaxAnjos/Urban-Heat-Islands/assets/94705218/dd5ff7f8-4bce-41e3-a841-6f72ca7bb7ba">

## UHI analysis

To ensure the model runs correctly, it is necessary to load the following inputs:

1.  Air temperature data .csv (required) with a minimum of four columns labeled *date*, *Latitude*, *Longitude*, *airT*.
2.  Other variables (optional) should have the same date column recommendation.

Note that the model converts the date-time into a R-formatted version, e.g., "2023-03-13 11:00:00" or "2023-03-13".

The following dataframe is presented as follows:

```{r setup, include=TRUE}
air_UCON %>% head(10)
```
<img src="https://user-images.githubusercontent.com/94705218/235909499-82427b94-5f35-4e58-b08b-0418d6fb4f44.png" alt="Screenshot 2023-05-03 at 08 57 51" width="691"/>

## Calculate thermal anomaly between LCZ

<img width="1440" alt="Screenshot 2023-08-19 at 13 44 11" src="https://github.com/ByMaxAnjos/Urban-Heat-Islands/assets/94705218/e641827f-da32-473b-bb4d-1861337ecb47">


## Interpolate air temperature with LCZ

it's coming soon... but have look at this:

<img width="1367" alt="Screenshot 2023-08-13 at 19 33 02" src="https://github.com/ByMaxAnjos/Urban-Heat-Islands/assets/94705218/7fbd74c3-bf91-4c0e-b273-9d5103313966">

## Calculate UHI intensity

it's coming soon...

