#' Create Shiny app from an explainer
#'
#' This function creates a \code{Shiny} application for explainers which are adapters for models created using the \code{DALEX} package.
#' The application contains model performance and explanations to fully explore the model.
#'
#' @param ... one or more explainers created with \code{DALEX::explain()} function. They can be switched in top right corner of the application.
#' @param directory path to the folder the application files will be created in. If \code{NULL} the application will be created in a temporary directory.
#' @param selected_variables choosen variables for application start-up. There can be more added in the application interface through an input.
#' @param run whether to run the Shiny application instantly
#' @export
#' @import shiny
#' @import shinydashboard
#' @import DALEX
#' @importFrom shinyjs useShinyjs show hide
#' @importFrom shinyBS bsPopover
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom whisker whisker.render
#' @importFrom readr read_file
#' @importFrom utils browseURL read.csv
#' @examples
#' # Create models
#' library("ranger")
#' library("DALEX")
#' model_rf <- ranger(survived ~ .,
#'                    data = titanic_imputed,
#'                    classification = TRUE,
#'                    probability = TRUE)
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

  # Obtaining explainers
  args <- list(..., version = 1.0)
  explainers <- args[names(args) == ""]

  # Creating necessary directory in order to drop generated app there
  directory <- create_directory(directory)

  # Fetching explainers data
  data <- get_explainers_data(explainers)

  # Creating `cols` and `selected_variables` objects (if not specified)
  objects_to_template <- generate_cols_and_variables(data, selected_variables)

  # Observation string used further in template
  objects_to_template <- create_observation(data, objects_to_template)

  # Saving all explainers
  objects_to_template <- save_explainers(explainers, directory, objects_to_template)

  # Filling template
  template_text_filled <- generate_template(objects_to_template)

  # Saving filled template as Shiny application and .html file with XAI tab content
  save_files(directory, template_text_filled)

  # Additional app running
  if(run) shiny::runApp(directory)
}



# Creating directory at a given location. If not provided --- create temporary directory.
create_directory <- function(directory) {

  if(is.null(directory)) directory <- tempdir()
  directory <- file.path(directory, 'xai2shiny')
  if(!dir.exists(directory)) dir.create(directory)

  return(directory)
}



# Providing explainers data for further calculations.
# Additionally, function checks whether all explainers are based on the same frame.
get_explainers_data <- function(explainers) {

  if(length(unique(lapply(explainers, function(x) { x$data }))) != 1) {
    stop("Explainers unique datasets amount does not equal 1. You have to base explainers on the same data!")
  }

  return(explainers[[1]]$data)
}



# Creating `cols` and `selected_variables` objects for further calculations.
# Checking for variables with just one unique value and ignoring them.
# `cols` objects is being created while `selected_variables`, if NULL, is first element of `cols`.
generate_cols_and_variables <- function(data, selected_variables) {
  if(length(which(apply(data, 2, function(x) length(unique(x))) == 1)) > 0) {
    cols <- colnames(data)[- which(apply(data, 2, function(x) length(unique(x))) == 1)]
  }
  else {
    cols <- colnames(data)
  }

  temp_cols <- cols
  cols <- paste0("'", cols, "'")
  cols <- paste(cols, sep = "", collapse = ",")
  cols <- paste0("c(", cols, ")")

  if(is.null(selected_variables)) {
    # if(length(cols) < 7) selected_variables <- cols
    # else selected_variables <- cols[1:7]
    selected_variables <- paste0("c('", temp_cols[1], "')")
  } else {
    temp_variables <- paste0("'", selected_variables, "'")
    temp_variables <- paste(temp_variables, sep = "", collapse = ",")
    selected_variables <- paste0("c(", temp_variables, ")")
  }

  objects_to_template <- list(cols = cols, selected_variables = selected_variables)

  return(objects_to_template)
}



# Auxiliary function to create expression string used in Shiny application data input.
create_observation <- function(data, objects_to_template) {

  vars <- lapply(data, class)
  t_vars <- as.data.frame(cbind(names = names(vars), type = vars))
  t_vars$levels <- apply(t_vars, 1, function(x) paste0(', levels = levels(data$', x$`names`, ')'))
  t_vars$levels[t_vars$type != 'factor']  <- ''
  t_vars$as  <- ''
  t_vars$as[t_vars$type != 'factor']  <- 'as.'

  t <- apply(t_vars, 1, function(x) paste0(x$`names`, ' = ', x$`as` , x$`type` , '(input$', x$`names`, x$`levels`, ')'))

  obstr <- paste(t, collapse = ", ", '\n\t\t\t')

  objects_to_template['obs'] <- paste0("list(", obstr,")")

  return(objects_to_template)
}



# Looping through all explainers to save them into files.
save_explainers <- function(explainers, directory, objects_to_template) {

  buttons <- ''
  explainers_reactive <- ''
  explainers_static <- ''
  libs <- ''

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
      # Additional logic for h2o-based models
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

  objects_to_template['libs'] <- libs
  objects_to_template['buttons'] <- buttons
  objects_to_template['explainers_reactive'] <- explainers_reactive
  objects_to_template['explainers_static'] <- explainers_static

  return(objects_to_template)
}



# Generating template text by filling all the necessary placeholders with created data.
generate_template <- function(objects_to_template) {

  # Reading necessary files: template and static text for prediction description
  static_text <- read.csv(system.file("extdata", "app_static_text.csv", package="xai2shiny"), sep = ';')
  prediction_text <- ifelse(explainer_glm$model_info$type == "classification",
                            paste0("'",as.character(static_text$text[static_text$text_destination == 'prediction_classification']),"'"),
                            paste0("'",as.character(static_text$text[static_text$text_destination == 'prediction_regression']),"'"))
  path_to_template <- system.file("templates", "default_template.txt", package = "xai2shiny")
  template_text <- readr::read_file(path_to_template)

  # Adding further template objects
  objects_to_template['text_prediction'] <- prediction_text

  # Filling template values with whisker
  template_text_filled <- whisker::whisker.render(template_text, objects_to_template)

  return(template_text_filled)
}



# Saving filled template text to the file as a Shiny app and XAI tab content
save_files <- function(directory, template_text_filled) {
  # Saving Shiny app
  template_file_path <- paste0(directory, "/app.R")
  file.create(template_file_path)
  template_file_conn <- file(template_file_path)
  writeLines(template_text_filled, template_file_conn)
  close(template_file_conn)
}
