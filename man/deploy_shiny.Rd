% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cloud_setup.R
\name{deploy_shiny}
\alias{deploy_shiny}
\title{Deploy Shiny applications to the cloud}
\usage{
deploy_shiny(
  droplet,
  directory,
  packages = "stats",
  port = 80,
  browse = TRUE,
  ssh_user = "root"
)
}
\arguments{
\item{droplet}{the droplet's id or droplet's object. The IP can be checked by running \code{analogsea::droplets()}.}

\item{directory}{path to the directory containing \code{Shiny} application (\code{xai2shiny} application).}

\item{packages}{vector of packages (package names) that are needed for the application to run.}

\item{port}{port at which the application will run.}

\item{browse}{a boolean, which indicates whether open the app on the web after deploying}

\item{ssh_user}{the name of ssh console user, should NOT be modified when using the default xai2shiny cloud_setup}
}
\description{
This function deploys a selected \code{Shiny} application (\code{xai2shiny} application) to the created droplet.
}
