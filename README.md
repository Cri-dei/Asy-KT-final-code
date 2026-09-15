# Asymmetric Dependence in Hydrological Extremes

This repository contains the R code supporting the analyses presented in:

**Deidda, C., Engelke, S., & De Michele, C. (2023).**
*Asymmetric Dependence in Hydrological Extremes.*
**Water Resources Research, 59(12), e2023WR034512.**
https://doi.org/10.1029/2023WR034512

## Overview

Dependence between extreme events is commonly described using symmetric dependence measures. However, many environmental and hydrological processes are inherently directional.

For example, extreme discharge at an upstream river station can directly influence discharge downstream, whereas the reverse relationship does not necessarily hold.

In this study, we introduce the **asymmetric tail Kendall's τ**, a conditional version of Kendall's τ designed to identify asymmetric dependence in extreme observations.

The method can be used to:

* quantify directional dependence between extremes;
* identify dependence structures that may be missed by symmetric measures;
* explore potential causal structures;
* assess the behavior of multivariate extreme-value models.

The methodology is evaluated through simulation experiments and applied to extreme river discharge in catchments in the United Kingdom.

## Repository contents

The repository contains the R scripts used for the hydrological application, simulation experiments, and figures presented in the paper.

### Hydrological analysis

`0_POT_daily_data_v3.R`
Processes daily discharge data and extracts peak-over-threshold (POT) extreme events for the selected river basins.

`1_Water_direction_ selected_couples.R`
Defines and analyses the directional relationships between selected pairs of river stations.

`2. Processing matrix.R`
Processes the pairwise dependence results and constructs the matrices used in the subsequent analysis.

### Figures

`3_ Plot.R`
Produces exploratory plots from the hydrological analysis.

`4_Plot_finali paper.R`
`4_Plot_finali paperV2.R`
Generate figures used for the final analysis and manuscript.

`4_1_ Plot finali paper AsyCopula.R`
Produces figures related to the asymmetric copula analysis.

### Simulation experiments

`5_ Simulation_AsyCopula_update0906.R`
Simulation experiments investigating the behavior of the asymmetric tail Kendall's τ under asymmetric copula models.

`7_ Simulation Causality_ improved.R`
`7_ Simulation Causality_ improved2.R`
`7_ Simulation Causality_ 6 cases_SensitivityAnalysis_sym.R`
Simulation experiments investigating the relationship between asymmetric extremal dependence and different causal structures.

## Method

For two variables \(X\) and \(Y\), the proposed measure evaluates Kendall's τ conditionally on extreme observations of one variable at a time.

This produces two directional measures,

$$
\tau_{XY}
\qquad \text{and} \qquad
\tau_{YX},
$$

which allow asymmetry in extremal dependence to be identified.

When the two coefficients differ, the dependence structure contains directional information that cannot be captured by a conventional symmetric dependence coefficient.

Further methodological details and theoretical results are provided in the paper.

## Case study

The methodology is applied to extreme river discharge in UK catchments.

The hydrological application illustrates how asymmetric extremal dependence can reveal information about connections between upstream and downstream river stations that would be lost when considering only symmetric dependence measures.

## Data

The hydrological analysis uses UK river discharge data from the **National River Flow Archive (NRFA)**.

The raw observational data are not distributed in this repository. Users wishing to reproduce the hydrological analysis should obtain the corresponding discharge and station information from the original data provider and adapt the input paths in the R scripts.

## Requirements

The analyses are implemented in **R**.

The scripts use several R packages depending on the analysis, including packages for data manipulation, hydrological data processing, spatial analysis, extreme-value analysis, and visualization.

Users should check the `library()` calls at the beginning of each script for the packages required for the corresponding analysis.

## Citation

If you use the methodology or code from this repository, please cite:

> Deidda, C., Engelke, S., & De Michele, C. (2023). Asymmetric Dependence in Hydrological Extremes. *Water Resources Research*, **59**(12), e2023WR034512. https://doi.org/10.1029/2023WR034512

### BibTeX

```bibtex
@article{Deidda2023Asymmetric,
  author  = {Deidda, Cristina and Engelke, Sebastian and De Michele, Carlo},
  title   = {Asymmetric Dependence in Hydrological Extremes},
  journal = {Water Resources Research},
  volume  = {59},
  number  = {12},
  pages   = {e2023WR034512},
  year    = {2023},
  doi     = {10.1029/2023WR034512}
}
```

## Authors

**Cristina Deidda**
Politecnico di Milano / Vrije Universiteit Brussel

**Sebastian Engelke**
University of Geneva

**Carlo De Michele**
Politecnico di Milano
