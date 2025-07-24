# Projections of temperature-related suicide mortality under climate change: A multi-country time-series study

This repository contains R scripts for analyzing and projecting the burden of suicide related to climate change across multiple countries and regions. The analysis is structured into five main scripts:

---

## Scripts Overview

### `Setting.R`
- **Purpose**: Basic environment setup
- **Contents**: Loads required R packages, sets global options, and defines working directories or common functions.

---

### `0.Calibration.R`
- **Purpose**: Calibration of modeled temperature
- **Contents**: Applies the bias-correction method developed within ISI-MIP (Hempel et al. 2013), using "fhempel.R".

---

### `1.FirstStage.R`
- **Purpose**: First-stage estimation
- **Contents**: Fits location-specific temperature–suicide associations using conditional Poisson regression and distributed lag non-linear models (DLNM).

---

### `2.SecondStage.R`
- **Purpose**: Second-stage mixed meta-analysis
- **Contents**: Pools location-specific estimates using multivariate mixed meta-regression to obtain region- and country-level exposure–response relationships.

---

### `3.ModeledSuicide.R`
- **Purpose**: Generate modeled baseline suicide risk
- **Contents**: Computes modeled suicide risk using historical observed temperatures for each location, based on estimated temperature–suicide relationships.

---

### `4.ProjectionANAF.R`
- **Purpose**: Project future attributable suicide mortality
- **Contents**: Projects future temperature-related suicide mortality under different climate scenarios using modeled data (SSP1-2.6, SSP2-4.5, SSP5-8.5). Includes Monte Carlo simulations for uncertainty quantification.

---

## Notes
- The actual dataset used in the study is confidential and cannot be shared publicly. However, the data files included in this repository (under the `data/` directory) contain **simulated datasets** that were generated to match the structure and format of the original data. These simulated datasets allow the code to run without modification and reproduce the analysis workflow, although they do not reflect real-world values.
- All scripts are designed to be run sequentially from 0 to 4.
- Please ensure all necessary packages are installed before running `0.Setting.R`.


