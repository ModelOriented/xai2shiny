# It is impossible to test DigitalOcean droplet's operation, hence no cov.
# nocov start

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
  if(!size %in% c(1,2,4,8)) {
    stop("The droplet's size (RAM) can be 1, 2, 4, or 8 GB. Please select one of these values.")
  }

  size_do <- paste0("s-1vcpu-",size,"gb")

  # Create new droplet with Docklet 19.03.12 on Ubuntu 18.04 and selected size
  docklet <- docklet_create(size = getOption("do_size", size_do), ...)
  Sys.sleep(15) # Wait for the droplet to initialize
  docklet <- droplet(docklet$id)

  # Install Shiny Server and all prerequisities from xai2shiny Docker image
  docklet_shinyserver(droplet = docklet, img = 'adamoso/xai2shiny')
}

#' Deploy Shiny applications to the cloud
#'
#' This function deploys a selected \code{Shiny} application (\code{xai2shiny} application) to the created droplet.
#'
#' @param droplet the droplet's id or droplet's object. The IP can be checked by running \code{analogsea::droplets()}.
#' @param directory path to the directory containing \code{Shiny} application (\code{xai2shiny} application).
#' @param packages vector of packages (package names) that are needed for the application to run.
#' @param port port at which the application will run.
#' @param browse a boolean, which indicates whether open the app on the web after deploying
#' @param ssh_user the name of ssh console user, should NOT be modified when using the default xai2shiny cloud_setup
#' @export
#' @importFrom analogsea docklet_shinyapp
#' @importFrom analogsea docklet_run
#' @importFrom analogsea droplet_ssh
#' @importFrom analogsea droplet_upload
#' @importFrom readr read_file
#' @importFrom whisker whisker.render
deploy_shiny <- function(droplet, directory, packages = "stats", port = 80, browse = TRUE, ssh_user = "root"){

  # Check if droplet exists
  if (missing(droplet) || is.null(droplet) || class(droplet) != "droplet" && class(droplet) != "numeric"){
    stop("You must create a droplet using xai2shiny::cloud_setup() before deploying your application.\n  After doing so, provide your droplet's id/droplet's object as the droplet parameter.")
  }
  if (class(droplet) == "droplet"){
    docklet <- droplet
  }
  if (class(droplet) == "numeric"){
    docklet <- droplet(id = droplet)
  }

  # Create folder for Dockerfile in droplet
  droplet_ssh(docklet, "mkdir -p /home/docker_setup")

  # Prepare the Dockerfile and data to fill it
  path_to_dockerfile <- system.file("docker", "Dockerfile", package="xai2shiny")
  dockerfile <- readr::read_file(path_to_dockerfile)
  pkgs <- paste("'", packages, "'", sep = "", collapse = ",")
  pkgs <- paste0("c(", pkgs, ")")
  packages_needed <- list(packages = pkgs)

  # Filling Dockefile with whisker
  text_to_file <- whisker::whisker.render(dockerfile, packages_needed)
  temp_directory <- tempdir()
  file_path <- paste0(temp_directory, "/Dockerfile")
  file.create(file_path)
  file_conn <- file(file_path)
  writeLines(text_to_file, file_conn)
  close(file_conn)

  # Upload Dockerfile to droplet
  droplet_upload(docklet, file_path, "/home/docker_setup/")

  # Build the image
  image_name <- tolower(packages[1])
  cmd <- paste0("docker build -t ", image_name, " /home/docker_setup/")
  droplet_ssh(docklet, cmd)

  # Upload Shiny app files to droplet
  droplet_ssh(docklet, "mkdir -p /srv/shinyapps")
  droplet_upload(docklet, directory, "/srv/shinyapps/")

  # Run the application
  docklet_run(docklet, " -d", " -p ", paste0(port, ":3838"),
              cn(" -v ", '/srv/shinyapps/:/srv/shiny-server/'),
              cn(" -w", ''), " ", image_name, ssh_user = ssh_user)

  # Open the application in web browser
  url <- sprintf("http://%s:%s/", droplet_ip(docklet), port)
  if (browse) {
    Sys.sleep(5) # give Shiny Server a few seconds to start up
    browseURL(url)
  }
}

# nocov end
