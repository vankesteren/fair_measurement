## Fair inference on error-prone outcomes

_L. Boeschoten, E.-J. van Kesteren, A. Bagheri, & D.L. Oberski_

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

This is a reproducible code repository supporting the manuscript. By running the `R` scripts in this repository, all the figures and tables can be reproduced. This repository follows a CC-BY 4.0 license.

The dataset `data/data_new.csv` was obtained directly without making changes from the [dissecting bias](https://gitlab.com/labsysmed/dissecting-bias) repository on GitLab and is subject to their license terms. 


### Setup
Ensure you have `R` and `RStudio` installed and run the following lines to install the dependencies
- `install.packages("tidyverse", "glmnet", "patchwork", "lavaan", "remotes")`
- `remotes::install_github("vankesteren/firatheme")`

### Running the code
The structure of the folder follows loosely the structure of the _Experiments_ section in the manuscript. Clone this repository, open the `fair_measurement.Rproj` file and run the following files in order:


| Section | Main R file | Main output |
| :------ | :------ | :----- |
| Data preparation and feature selection | `01_feature_selection.R` | `output/01_feature_selection.png`|
| Fair inference on cost as a proxy of health | `02_parity_correction.R`| `output/02_parity_correction.pdf` |
| Fair inference on latent health | `03_measurement_model.R` | `output/03_measurement_model.pdf` |
| Investigating unfairness in proxies | `04_differential_item_functioning.R` | `Table 1 (in console)` |


These main R files depend on the remaining R files and output files in this repository. The main figures will appear in the `output/` folder.
