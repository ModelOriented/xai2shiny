#' Create DigitalOcean droplet setup to deploy shiny apps
#'
#' @param ... additional params to function analogsea::droplet_create
#' @param droplet droplet object created with analogsea package, if creating a new droplet, leave empty
#' @export
#' @import analogsea
cloud_setup <- function(droplet, ...){

  if (missing(droplet) || is.null(droplet)){

    # No droplet provided; create a new droplet
    message("Starting a basic DigitalOcean droplet")
    message("This actions costs you money: $5/month billed by DigitalOcean")

    # Check if DO has ssh keys configured
    if (!length(analogsea::keys())) {
      stop("Please add an ssh key to your Digital Ocean account before using this method. See `analogsea::key_create` method.")
    }


    createArgs <- list(...)
    createArgs$tags <- c(createArgs$tags, "xai2shiny")
    createArgs$image <- "ubuntu-20-04-x64"

    droplet <- do.call(analogsea::droplet_create, createArgs)

    # Wait for the droplet to come online
    analogsea::droplet_wait(droplet)

    # Wait a little more to ensure it's online
    Sys.sleep(25)

    # Refresh the droplet; sometimes the original one doesn't yet have a network interface.
    droplet <- analogsea::droplet(id=droplet$id)
  }

  # Provision

  message("This can take up to 20 minutes, so grab yourself a coffee.")

  lines <- droplet_capture(droplet, 'swapon | grep "/swapfile" | wc -l')
  if (lines != "1"){
    analogsea::debian_add_swap(droplet)
  }
  install_new_r(droplet)
  install_r_packages(droplet)
  install_nginx(droplet)
  install_firewall(droplet)
  install_shiny_server(droplet)

  invisible(droplet)
}

#' Captures the output from running some command via SSH
#' @noRd
droplet_capture <- function(droplet, command){
  tf <- tempdir()
  randName <- paste(sample(c(letters, LETTERS), size=10, replace=TRUE), collapse="")
  tff <- file.path(tf, randName)
  on.exit({
    if (file.exists(tff)) {
      file.remove(tff)
    }
  })
  analogsea::droplet_ssh(droplet, paste0(command, " > /tmp/", randName))
  analogsea::droplet_download(droplet, paste0("/tmp/", randName), tf)
  analogsea::droplet_ssh(droplet, paste0("rm /tmp/", randName))
  lin <- readLines(tff)
  lin
}

install_firewall <- function(droplet){
  analogsea::droplet_ssh(droplet, "ufw allow http")
  analogsea::droplet_ssh(droplet, "ufw allow ssh")
  analogsea::droplet_ssh(droplet, "ufw -f enable")
}

install_nginx <- function(droplet){
  analogsea::debian_apt_get_install(droplet, "nginx")
  analogsea::droplet_ssh(droplet, "rm -f /etc/nginx/sites-enabled/default") # Disable the default site
  analogsea::droplet_ssh(droplet, "systemctl reload nginx")
}

install_new_r <- function(droplet){
  analogsea::debian_apt_get_install(droplet, c("dirmngr", "gnupg","apt-transport-https", "ca-certificates", "software-properties-common"))
  analogsea::droplet_ssh(droplet, "apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9")
  analogsea::droplet_ssh(droplet, "add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'")
  analogsea::debian_apt_get_update(droplet)
  analogsea::debian_install_r(droplet)
}

install_r_packages <- function(droplet){
  # install cran versions of required packages
  analogsea::install_r_package(droplet, "DALEX")
  analogsea::install_r_package(droplet, "iBreakDown")
  analogsea::install_r_package(droplet, "ingredients")
  analogsea::install_r_package(droplet, "shiny")
  analogsea::install_r_package(droplet, "shinyjs")
  analogsea::install_r_package(droplet, "shinydashboard")
  analogsea::install_r_package(droplet, "shinyWidgets")
  analogsea::install_r_package(droplet, "shinycssloaders")
}

install_shiny_server <- function(droplet){
  analogsea::droplet_ssh(droplet, "sudo apt-get install gdebi-core")
  # Allow default shiny server port in firewall
  analogsea::droplet_ssh(droplet, "sudo ufw allow 3838")
  analogsea::droplet_ssh(droplet, "wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.9.923-amd64.deb")
  analogsea::droplet_ssh(droplet, "sudo gdebi --non-interactive shiny-server-1.5.9.923-amd64.deb")
}
