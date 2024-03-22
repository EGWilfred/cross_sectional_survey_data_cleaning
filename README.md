# Malaria Multilevel Spatial Analysis in Urban Areas of Nigeria

## Overview
This project aims to conduct a multilevel spatial analysis of malaria prevalence in urban areas of Nigeria, focusing on the impact of environmental factors, health seeking behaviours, human movement and socio-economic status on malaria distribution. The project is structured into three main folders, each containing code for different aspects of the analysis: data cleaning and weight calculation, extraction of environmental covariates, and calculation of the Socioeconomic Demographic Index (SDI).

## Project Structure
The project is organized into three main folders:

### 1. Wet Season
This folder contains R scripts and notebooks for data cleaning and weight calculations specific to the wet season's data. Key operations include preprocessing of malaria incidence data and calculation of weights for spatial analysis.

- **Main Files:**
  - `DataCleaning.R`: Script for initial data cleaning steps.
  - `WeightCalculation.R`: Script for calculating weights used in the spatial analysis.

### 2. Raster Covariates
Includes all code related to the extraction of environmental covariates from Google Earth Engine. These covariates include land temperature, humidity levels, and vegetation indices which are crucial for understanding the environmental factors influencing malaria distribution.

- **Main Files:**
  - `ExtractCovariates.R`: Script to extract and process raster data from Google Earth Engine.
  - `CovariateAnalysis.R`: Script for the analysis of environmental covariates and their correlation with malaria incidence.

### 3. SDI
Contains code for the calculation of the Socioeconomic Demographic Index (SDI), which is used to assess the socio-economic status of different settlements within the urban areas under study. The SDI is calculated based on various indicators such as household goods, access to clean water, and type of sanitation facilities.

- **Main Files:**
  - `CalculateSDI.R`: Script for calculating the Socioeconomic Demographic Index.
  - `SDIAnalysis.R`: Script for analyzing the impact of SDI on malaria distribution.

## Getting Started
To run the scripts and notebooks in this project, ensure you have R and any necessary packages installed. Each folder contains a separate README with specific instructions on how to execute the code within that directory.

1. Clone this repository to your local machine.
2. Navigate to each folder and follow the instructions in the README file for setup and execution details.
3. Data required for this project is assumed to be pre-downloaded and structured according to the paths specified in the scripts.

## Contributing
We welcome contributions and suggestions to improve the analysis. Please follow the standard pull request process to propose changes to the codebase.

## License
This project is open-sourced under the MIT License. See the LICENSE file for more details.

## Contact
For any questions or inquiries related to this project, please contact the project team at [laurette@aims.ac.tz].

