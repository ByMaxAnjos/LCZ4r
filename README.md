# Tools for Local Climate Zones Analysis and Urban Heat Islands in R

<img align="right" src="https://github.com/ByMaxAnjos/LCZ4r/blob/main/inst/figures/logo.png?raw=true" alt="logo" width="140"> 

The **LCZ4r**, a suite of R functions for analyzing and visualizing Local Climate Zones and Urban Heat Islands.






## Package Overview

The **LCZ4r** package is a comprehensive toolset, featuring a total of functions. 

* `lcz_get_map()` - Obtain the LCZ map 
* `lcz_get_map2()` - Obtain the LCZ map (without internet)
* `lcz_plot_map()` - Visualize the LCZ map 
* `lcz_cal_area()` - Calculate LCZ areas 
* `lcz_get_parameters()` - Retrieve LCZ parameters
* `lcz_plot_parameters()` - Visualize LCZ parameter map
* `lcz_ts()` - Analyze LCZ Time Series
* `lcz_anomaly()` - Calculate LCZ Thermal Anomalies
* `lcz_anomaly_map()` - Visualize LCZ Thermal Anomaly map
* `lcz_krige_map()` - Perform LCZ Interpolation with Kriging
* `lcz_krige_eval()` - Evaluate LCZ Interpolation with Kriging
* `lcz_idw_map()` - Perform LCZ Interpolation with IDW
* `lcz_idw_eval()` - Evaluate LCZ Interpolation with IDW
* `lcz_uhi()` - Assess LCZ for Urban Heat Island Intensity

## Instalation 

To [Install to the LCZ4r](https://bymaxanjos.github.io/LCZ4r/articles/instalation_lcz4r.html) use the development version on GitHub.


## Function types and tutorials 

These functions are categorized into two main types: general functions and local functions. The general functions are tailored for circumstances where specific LCZ data is not readily available. In contrast, the local functions are designed to operate with more extensive data inputs, such as urban meteorological network readings, including air temperature.

We highly recommend that users explore the following tutorials to gain a deeper understanding of the capabilities and applications of the LCZ4r package:

* [Introduction to the General Functions](https://bymaxanjos.github.io/LCZ4r/articles/Introd_general_LCZ4r.html)

* [Introduction to the Local Functions](https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html) 


## Obtain and visualize the LCZ map

![lcz_PlotMap](https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/d1f3e0b8-bd05-464b-b52d-932fa5cf77a2)

## Calculate LCZ areas 

![lczarea_berlin](https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/46b57b89-f05d-4f60-95d4-a93d9f54ad93)

## Retrieve and visualize LCZ parameters

<img width="1439" alt="Screenshot 2023-08-13 at 19 19 06" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/e9006776-a336-4303-bc35-f787090a1caf">

## Time serires of air tempearture from LCZ classes

<img width="1440" alt="ts_berlin" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/8b86432e-a372-4c05-82b0-60c1e231596f">


## Calculate thermal anomaly between LCZ

<img width="1440" alt="thermal_berlin" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/3fdb3509-eaa1-43a0-832c-bfbc8b38ac52">


## Interpolate air temperature with LCZ

it's coming soon... but have look at this:

<img width="1367" alt="Screenshot 2023-08-13 at 19 33 02" src="https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/ecc209b1-0ef5-4554-a141-3feb26d0a623">


## Calculate UHI intensity

it's coming soon...


### People

The development of the package R **LCZ4r** has been led by [Dr. Max Anjos](https://www.researchgate.net/profile/Max-Anjos/research) and joined by:


–> Dayvid Carlos de Medeiros, Center for Climate Crisis Studies, Departament of Geography, Federal University of Rio Grande do Norte, Brazil.

–> Dr. Francisco Castelhano, Center for Climate Crisis Studies, Departament of Geography, Federal University of Rio Grande do Norte, Brazil.

–> Dr.Fred Meier, Chair of Climatology, Institute of Ecology, Technische Universität Berlin.

### Funding

This project was financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior (CAPES) – Finance Code 001, and by the Alexander von Humboldt Foundation.

### Contact

Please feel free to contact us if you have any questions or suggestions by emailing [maxanjos\@campus.ul.pt](mailto:maxanjos@campus.ul.pt). If you are interested in contributing to the development of this R package, we welcome you to join our team.
