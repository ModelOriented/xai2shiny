# xai2shiny

<!-- badges: start -->
  [![R build status](https://github.com/ModelOriented/xai2shiny/workflows/R-CMD-check/badge.svg)](https://github.com/ModelOriented/xai2shiny/actions)
  [![Coverage Status](https://codecov.io/gh/ModelOriented/xai2shiny/branch/master/graph/badge.svg)](https://codecov.io/github/ModelOriented/xai2shiny?branch=master)
  <!-- badges: end -->

## Overview

The `xai2shiny` R package creates a **Shiny application** for Explainers (adapters for machine learning models created using the `DALEX` package). Turn your model into an interactive application containing model's prediction, performance and many **XAI** methods with just **one function**. Furthermore, with `xai2shiny` you can simply export your application to the cloud and share it with others.

## Installation

```
# Install the development version from GitHub:
devtools::install_github("ModelOriented/xai2shiny")
```

## Example

Package usage example will be based on the *titanic* dataset, including GLM and Random Forest models.
[The final application created using the scipt below.](http://206.189.209.54:2211/Xai2shinyWorkingExample/)
First, it is necessary to have any explainers created whatsoever:

```
library("xai2shiny")
library("ranger")
library("DALEX")

# Creating ML models
model_rf <- ranger(survived ~ .,
                   data = titanic_imputed,
                   classification = TRUE, 
                   probability = TRUE)
model_glm <- glm(survived ~ .,
                 data = titanic_imputed,
                 family = "binomial")

# Creating DALEX explainers
explainer_rf <- explain(model_rf,
                     data = titanic_imputed[,-8],
                     y = titanic_imputed$survived)

explainer_glm <- explain(model_glm,
                     data = titanic_imputed[,-8],
                     y = titanic_imputed$survived)
```

Then all is left to do is to run:

```
xai2shiny::xai2shiny(explainer_glm, explainer_rf, 
                     directory = 'D:/Studia/test',
                     selected_variables = c('gender', 'age'),
                     run = FALSE)
```

Above, in `xai2shiny` function, apart from explainers, following attributes were provided:

* `directory` - a location indicator where to create whole `xai2shiny` directory and place there required files (an app and explainers),
* `selected_variables` - a vector containing variables list chosen at an app start-up (used for modification and local explanations research),
* `run` - whether to run an app immediately after creating.

## Cloud deployment

Further cloud deployment can be performed. In order to do so, there are just three steps necessary to enjoy your new *xai2shiny* application in the cloud.

1. If you don't have an account on DigitalOcean, create one [here](https://m.do.co/c/c07558eaca11) and get $100 free credit.
2. [Create an SSH key](https://docs.github.com/en/enterprise/2.17/user/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent?fbclid=IwAR3E66nCkq5cS6BSSHvgv-tzFa9MjWL37bUgRz3DKwglTO8Zn_t6tmKwvRo) if you don't have one yet.
3. [Deploy the SSH key to DigitalOcean](https://www.digitalocean.com/docs/droplets/how-to/add-ssh-keys/to-account/)

And that's it, you are ready to get back to R and deploy your application. In order to create a new cloud instance, called a *droplet* by DigitalOcean, running Docker on Ubuntu with all prerequisities installed, just run:

```
xai2shiny::cloud_setup(size)
```

* `size` - ram size desired for the droplet, defaults to 1GB. It can be modified later through [DigitalOceans website](https://www.digitalocean.com/).

Now that your droplet is setup, just deploy the created *xai2shiny* application with *one function*.

```
deploy_shiny(droplet = <your_droplet_id>, path = './xai2shiny', packages = "ranger")
```


* `droplet` - the droplet object/droplet's ID that can be read from running `analogsea::droplets()`.
* `path` - path to the *xai2shiny* application
* `packages` - packages used to create or run the model, they will be installed on the droplet.

And that's it, the *xai2shiny* application is running and will automatically open in your default web browser, now all that's left is to share it!

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
