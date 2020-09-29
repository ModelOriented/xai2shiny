# xai2shiny

<!-- badges: start -->
  [![R build status](https://github.com/ModelOriented/xai2shiny/workflows/R-CMD-check/badge.svg)](https://github.com/ModelOriented/xai2shiny/actions)
  <!-- badges: end -->

## Overview

The `xai2shiny` R package creates a **Shiny application** for Explainers (adapters for machine learning models created using the `DALEX` package). Turn your model into an interactive application containing model's prediction, performance and many **XAI** methods with just **one function**.

## Examples

- [x] [Titanic dataset - GLM and Random Forest models](https://adamr.shinyapps.io/xai2shiny/)

## Installation

```
# Install the development version from GitHub:
devtools::install_github("ModelOriented/xai2shiny")
```
## Functionality

The main function is called **xai2shiny** which creates the Shiny **app.R** file and runs it converting your models into an interactive application. 

At the time it supports such functionalities for **multiple models in one application**:

1. **Model prediction**
2. **Model performance** (with text descriptions of measures)
3. **Local explanations:** (with text descriptions)
   * Break Down plot
   * SHAP values plot
   * Ceteris Paribus plot
4. **Global explanations:**
   * Feature importance plots
   * Partial Dependence plots
