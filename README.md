# Tools for Local Climate Zone and Urban Heat Island Analysis in R

<img align="right" src="https://github.com/ByMaxAnjos/LCZ4r/blob/main/inst/figures/logo.png?raw=true" alt="logo" width="140"> 

  <!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  <!-- badges: end -->

The [LCZ4r package](https://github.com/ByMaxAnjos/LCZ4r) offers a suite of functions designed to analyze and visualize Local Climate Zones and Urban Heat Islands in R.


## Package Overview

The **LCZ4r** is a comprehensive toolkit, including the following functions: 

* `lcz_get_map()`  Obtain your LCZ map from Global LCZ map
* `lcz_get_map_euro()`  Obtain your LCZ map from European LCZ map
* `lcz_get_map_usa()`  Obtain your LCZ map from Continental United States LCZ map
* `lcz_plot_map()`  Visualize the LCZ map 
* `lcz_cal_area()`  Calculate LCZ areas 
* `lcz_get_parameters()`  Retrieve LCZ parameters
* `lcz_plot_parameters()`  Visualize LCZ parameter map
* `lcz_ts()`  Analyze LCZ Time Series
* `lcz_anomaly()`  Calculate LCZ Thermal Anomalies
* `lcz_anomaly_map()`  Map LCZ Thermal Anomaly
* `lcz_interp_map()`  Map LCZ Interpolation
* `lcz_plot_interp()`  Visualize LCZ Interpolation
* `lcz_uhi_intensity()`  Assess LCZ for Urban Heat Island Intensity

## Instalation 

To [Install to the LCZ4r](https://bymaxanjos.github.io/LCZ4r/articles/instalation_lcz4r.html) use the development version on GitHub.


## Function types and tutorials 

These functions are categorized into two main types: general functions and local functions. The general functions are tailored for circumstances where specific LCZ data is not readily available. In contrast, the local functions are designed to operate with more extensive data inputs, such as urban meteorological network readings, including air temperature.

We highly recommend that users explore the following tutorials to gain a deeper understanding of the capabilities and applications of the LCZ4r package:

* [Introduction to the General Functions](https://bymaxanjos.github.io/LCZ4r/articles/Introd_general_LCZ4r.html)

[Click to Whatch Demo Video](https://github.com/ByMaxAnjos/LCZ4r/assets/94705218/a534cfaa-37d2-4c1d-8cec-e5e2a17e8806)

* [Introduction to the Local Functions](https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html) 


### People

The development of the package R **LCZ4r** has been led by [Dr. Max Anjos](https://www.researchgate.net/profile/Max-Anjos/research) and joined by:

–> Dayvid Carlos de Medeiros, Center for Climate Crisis Studies, Department of Geography, Federal University of Rio Grande do Norte, Brazil.

–> Dr. Francisco Castelhano, Center for Climate Crisis Studies, Department of Geography, Federal University of Rio Grande do Norte, Brazil.

–> Dr.Fred Meier, Chair of Climatology, Institute of Ecology, Technische Universität Berlin.

### Funding

This project was financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior (CAPES) – Finance Code 001, and by the Alexander von Humboldt Foundation.

### Contact

Please feel free to contact us if you have any questions or suggestions by emailing [maxanjos\@campus.ul.pt](mailto:maxanjos@campus.ul.pt). If you are interested in contributing to the development of this R package, we welcome you to join our team.

### Inspiration

* Stewart, I., and T. Oke, 2012: Local climate zones for urban temperature studies. Bull. Amer. Meteor. Soc., 93, 1879–1900[DOI](https://doi.org/10.1175/BAMS-D-11-00019.1)
* Ching, J., Mills, G., Bechtel, B., See, L., Feddema, J., Wang, X., … Theeuwes, N. (2018). WUDAPT: An Urban Weather, Climate, and Environmental Modeling Infrastructure for the Anthropocene. Bulletin of the American Meteorological Society, 99(9), 1907–1924[DOI](https://doi.org/10.1175/BAMS-D-16-0236.1)
* Demuzere, M., Bechtel, B., Middel, A., & Mills, G. (2019). Mapping Europe into local climate zones. PLOS ONE, 14(4), e0214474[DOI](https://doi.org/10.1371/journal.pone.0214474)
* Demuzere, M., Hankey, S., Mills, G., Zhang, W., Lu, T., & Bechtel, B. (2020). Combining expert and crowd-sourced training data to map urban form and functions for the continental US. Scientific Data, 7(1), 264[DOI](https://doi.org/10.1038/s41597-020-00605-z)
* Demuzere, M., Kittner, J., Martilli, A., Mills, G., Moede, C., Stewart, I. D., van Vliet, J., and Bechtel, B. (2022). A global map of Local Climate Zones to support earth system modelling and urban scale environmental science, Earth Syst. Sci. Data 14(8) 3835-3873.[DOI](https://doi.org/10.5194/essd-14-3835-2022)

