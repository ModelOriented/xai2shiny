% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xai2shiny.R
\name{xai2shiny}
\alias{xai2shiny}
\title{Create Shiny app from an explainer}
\usage{
xai2shiny(
  ...,
  directory = NULL,
  selected_variables = NULL,
  run = TRUE,
  override = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{...}{one or more explainers created with \code{DALEX::explain()} function. They can be switched in top right corner of the application.}

\item{directory}{path to the directory the application files will be created in. If \code{NULL} the application will be created in a temporary directory.}

\item{selected_variables}{choosen variables for application start-up. There can be more added in the application interface through an input.}

\item{run}{whether to run the Shiny application instantly}

\item{override}{how to respond to a directory overriding case}

\item{verbose}{whether to log in console internal function's steps}
}
\description{
This function creates a \code{Shiny} application for explainers which are adapters for models created using the \code{DALEX} package.
The application contains model performance and explanations to fully explore the model.
}
\examples{
# Create models
library("ranger")
library("DALEX")
model_rf <- ranger(survived ~ .,
                   data = titanic_imputed,
                   classification = TRUE,
                   probability = TRUE)
model_glm <- glm(survived ~ .,
                 data = titanic_imputed,
                 family = "binomial")

# Create DALEX explainers
explainer_rf <- explain(model_rf,
                     data = titanic_imputed[,-8],
                     y = titanic_imputed$survived)

explainer_glm <- explain(model_glm,
                     data = titanic_imputed[,-8],
                     y = titanic_imputed$survived)

# Create and run the application

\dontrun{
xai2shiny(explainer_rf, explainer_glm)
}
}
