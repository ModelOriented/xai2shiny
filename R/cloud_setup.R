#' Create DigitalOcean droplet setup to deploy \code{Shiny} apps
#'
#' This function creates a new DigitalOcean droplet with Docker 19.03.12 on Ubuntu 18.04 with every required package and service to run xai2shiny applications in the cloud.
#'
#' @param size initial RAM (in GBs) for the droplet. Available values: 1, 2, 4, 8. You can always resize the droplet later on.
#' @param ... additional params to function analogsea::docklet_create
#' @export
#' @importFrom analogsea droplet
#' @importFrom analogsea docklet_create
#' @importFrom analogsea docklet_shinyserver
cloud_setup <- function(size = 1, ...){
  if(!size %in% c(1,2,4,8)){
    stop("The droplet's size (RAM) can be 1, 2, 4, or 8 GB. Please select one of these values.")
  }
  size_do <- paste0("s-1vcpu-",size,"gb")
  # Create new droplet with Docklet 19.03.12 on Ubuntu 18.04 and selected size
  docklet <- docklet_create(size = getOption("do_size", size_do))
  docklet <- droplet(docklet$id)
  # Install Shiny Server and all prerequisities from xai2shiny Docker image
  docklet %>% docklet_shinyserver(img = 'adamoso/xai2shiny')
}

#' Deploy Shiny applications to the cloud
#'
#' This function deploys a selected \code{Shiny} application (\code{xai2shiny} application) to the created droplet.
#'
#' @param droplet the droplet's id or droplet's object. The IP can be checked by running \code{analogsea::droplets()}.
#' @param path path to the \code{Shiny} application (\code{xai2shiny} application).
#' @param port port at which the application will run.
#' @param packages vector of packages (package names) that are needed for the application to run.
#' @export
#' @importFrom analogsea docklet_shinyapp
deploy_shiny <- function(droplet, path, port, packages){
  if (missing(droplet) || is.null(droplet) || class(droplet) != "droplet" && class(droplet) != "numeric"){
    stop("You must create a droplet using xai2shiny::cloud_setup() before deploying your application.\n  After doing so, provide your droplet's id/droplet's object as the droplet parameter.")
  }
  if (class(droplet) == "droplet"){
    docklet <- droplet
  }
  if (class(droplet) == "numeric"){
    docklet <- droplet(id = droplet)
  }
  docklet_shinyapp(droplet = docklet, path = path, port = port, img = 'adamoso/xai2shiny')
}
