#' Create shiny app from an explainer
#'
#' This function creates a \code{Shiny} application for explainers which are adapters for models created using the \code{DALEX} package.
#' The application contains model performance and explanations to fully explore the model.
#'
#' @param ... one or more explainers created with \code{DALEX::explain()} function. They can be switched in top right corner of the application.
#' @param directory path to the folder the application files will be created in. If \code{NULL} the application will be created in a temporary directory.
#' @param selected_variables choosen variables for application start-up. There can be more added in the application interface through an input.
#' @param run whether to run the shiny application instantly
#' @export
#' @import shiny
#' @import shinydashboard
#' @import DALEX
#' @importFrom shinyjs useShinyjs show hide
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom whisker whisker.render
#' @importFrom readr read_file
#' @examples
#' # Create models
#' library("ranger")
#' library("DALEX")
#' model_rf <- ranger(survived ~ .,
#'                    data = titanic_imputed)
#' model_glm <- glm(survived ~ .,
#'                  data = titanic_imputed,
#'                  family = "binomial")
#'
#' # Create DALEX explainers
#' explainer_rf <- explain(model_rf,
#'                      data = titanic_imputed[,-8],
#'                      y = titanic_imputed$survived)
#'
#' explainer_glm <- explain(model_glm,
#'                      data = titanic_imputed[,-8],
#'                      y = titanic_imputed$survived)
#'
#' # Create and run the application
#'
#'\dontrun{
#' xai2shiny(explainer_rf, explainer_glm)
#' }
xai2shiny <- function(..., directory = NULL, selected_variables = NULL, run = TRUE) {


  # Obtaining explainers and template
  args <- list(..., version = 1.0)
  options <- args[names(args) != ""]

  explainers <- args[names(args) == ""]

  # Reading template file to
  path_to_template <- system.file("templates", "default_template.txt", package="xai2shiny")
  template <- readr::read_file(path_to_template)

  # Creating necessary directory in order to drop generated app there
  if(is.null(directory)) directory <- tempdir()
  directory <- file.path(directory, 'xai2shiny')
  if(!dir.exists(directory)) dir.create(directory)

  # Fetching explainers data (assuming that all are based on the same frames)
  data <- explainers[[1]]$data

  # Checking for variables with just one unique value and ignoring them
  # `cols` objects is being created while `selected_variables`, if NULL, is first 7 elements of `cols`
  if(length(which(apply(data, 2, function(x) length(unique(x))) == 1)) > 0) {
    cols <- colnames(data)[- which(apply(data, 2, function(x) length(unique(x))) == 1)]
  }
  else {
    cols <- colnames(data)
  }

  cols <- paste0("'", cols, "'")
  cols <- paste(cols, sep = "", collapse = ",")
  cols <- paste0("c(", cols, ")")

  if(is.null(selected_variables)){
    if(length(cols) < 7) selected_variables <- cols
    else selected_variables <- cols[1:7]
  } else {
    temp_variables <- paste0("'", selected_variables, "'")
    temp_variables <- paste(temp_variables, sep = "", collapse = ",")
    selected_variables <- paste0("c(", temp_variables, ")")
  }

  # Observation string used further in template
  obs <- .create_observation(data)

  buttons <- ''
  explainers_reactive <- ''
  explainers_static <- ''
  libs <- ''

  # Looping through all explainers to save them into files
  for(i in 1:length(explainers)){
    saveRDS(explainers[[i]], file = paste0(directory,"/exp", i, ".rds"))
    packages <- explainers[[i]]$model_info$package

    # Checking if model package has been read correctly
    if(length(packages) > 1) {
      for(package in packages) {
        if(!grepl("\\s", package)) {
          lib <- paste0("library(",package,")\n")
        }
      }
    }
    else {
      if(grepl("\\s", packages)){
        if(grepl("H2O", packages, fixed = TRUE)){
          lib <- paste0("library('h2o')\n")
        }
        else{
          stop(paste0("The package used to create the model was not read correctly.\n",
                      "Set it manually to the correct value by using:\nexplainer$model_info$package <- 'name of package'"))
        }
      }
      else{
        lib <- paste0("library(",packages,")\n")
      }
    }

    # Setting up proper values
    libs <- paste0(libs, lib)
    button <- paste0('tags$li(class = "dropdown", actionBttn("exp', i, '", explainer', i, '$label, style = "fill", block = TRUE))')
    buttons <- paste0(buttons, ", ", button)
    explainer_reactive <- paste0('observeEvent(input$exp', i, ', { exp$data <- explainer', i, ' })')
    explainers_reactive <- paste0(explainers_reactive, "\n\t", explainer_reactive)
    explainer_static <- paste0('explainer',i,' <- readRDS("exp',i,'.rds")')
    explainers_static <- paste0(explainers_static, "\n", explainer_static)
  }

  template_data <- list(obs = obs,
                        cols = cols,
                        libs = libs,
                        explainers_static = explainers_static,
                        selected_variables = selected_variables,
                        explainers_reactive = explainers_reactive,
                        buttons = buttons)

  # Filling template values with whisker
  text_to_file <- whisker::whisker.render(template, template_data)

  # Saving filled template as Shiny application
  file_path <- paste0(directory, "/app.R")
  file.create(file_path)
  file_conn <- file(file_path)
  writeLines(text_to_file, file_conn)
  close(file_conn)

  # Additional app running
  if(run) shiny::runApp(directory)
}

# Auxiliary function to create expression string used in Shiny application data input
.create_observation <- function(data){

  vars <- lapply(data, class)
  t_vars <- as.data.frame(cbind(names = names(vars), type = vars))
  t_vars$levels <- apply(t_vars, 1, function(x) paste0(', levels = levels(data$', x$`names`, ')'))
  t_vars$levels[t_vars$type != 'factor']  <- ''
  t_vars$as  <- ''
  t_vars$as[t_vars$type != 'factor']  <- 'as.'

  t <- apply(t_vars, 1, function(x) paste0(x$`names`, ' = ', x$`as` , x$`type` , '(input$', x$`names`, x$`levels`, ')'))

  obstr <- paste(t, collapse = ", ", '\n\t\t\t')
  paste0("list(", obstr,")")
}
