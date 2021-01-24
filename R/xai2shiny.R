#' Create Shiny app from an explainer
#'
#' This function creates a \code{Shiny} application for explainers which are adapters for models created using the \code{DALEX} package.
#' The application contains model performance and explanations to fully explore the model.
#'
#' @param ... one or more explainers created with \code{DALEX::explain()} function. They can be switched in top right corner of the application.
#' @param directory path to the folder the application files will be created in. If \code{NULL} the application will be created in a temporary directory.
#' @param selected_variables choosen variables for application start-up. There can be more added in the application interface through an input.
#' @param run whether to run the Shiny application instantly
#' @param override how to respond to a directory overriding case
#' @param verbose whether to log in console internal function's steps
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
#' @importFrom ggplot2 ggtitle labs
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
xai2shiny <- function(..., directory = NULL, selected_variables = NULL, run = TRUE, override = FALSE, verbose = TRUE) {

  if(verbose == TRUE) {
    cat("Setting up new Shiny XAI application\n")
  }

  # Obtaining explainers
  args <- list(..., version = 1.0)
  explainers <- args[names(args) == ""]

  # Creating necessary directory in order to drop generated app there
  directory <- create_directory(directory, override, verbose)

  # Fetching explainers data
  data <- get_explainers_data(explainers)

  # Creating `cols` and `selected_variables` objects (if not specified)
  objects_to_template <- generate_cols_and_variables(data, selected_variables, verbose)

  # Observation string used further in template
  objects_to_template <- create_observation(data, objects_to_template, verbose)

  # Saving all explainers
  objects_to_template <- save_explainers(explainers, directory, objects_to_template, verbose)

  # Filling template
  template_text_filled <- fill_template(objects_to_template, explainers, verbose)

  # Saving filled template as Shiny application and .html file with XAI tab content
  save_files(directory, template_text_filled, verbose)

  if(verbose == TRUE) {
    cat("Application setup ended\n")
  }

  # Additional app running
  if(run) shiny::runApp(directory)
}


#' @noRd
#' @title create_directory
#' @description This funciton creates a directory at a given location.
#' If not provided, the directory will be created in temporary directory.
#' @param directory string - path to the desired directory location
#' @param override bool - whether to override the directory if it already exists
#' @param verbose bool - if text information should be displayed in console
create_directory <- function(directory, override, verbose) {

  if(is.null(directory) | length(nchar(directory)) == 0) {

    cat("You passed no explicit directory location. If you want to specify it, please pass it now.\n")
    cat("In case of an empty string, temporary directory is going to be set up\n")
    directory <- readline("Please provide the final location: ")

    if(nchar(directory) == 0) {
      directory <- file.path(tempdir(), 'xai2shiny')
    }
  }

  if(verbose == TRUE) {
    cat(paste0("\tApplication is setting up at: ", directory, "\n"))
  }

  if(dir.exists(directory)) {
    if(override) {
      warning("Overiding existing directory with the newest application")
      cat(paste0("Caution! You are about to delete the directory content (", directory, ").\n"))
      if(readline("Are you sure? [y / n]: ") == "y") {
        suppressWarnings({
          unlink(directory, recursive = TRUE)
          dir.create(directory)
        })
      } else {
        stop("Specified directory exists. Please delete it by yourself or point to a different location.")
      }
    } else {
      stop('Specified directory exists and override is set to FALSE. Set it to TRUE or change xai2shiny files destination')
    }
  } else {
    dir.create(directory)
  }

  return(directory)
}


#' @noRd
#' @title get_explainers_data
#' @description This funciton provides explainers data for further calculations.
#' Additionally, it checks whether all explainers are based on the same data frame (same columns, data can be split into train/test/validate).
#' @param explainers explainer list - list of explainers input for the function
get_explainers_data <- function(explainers) {

  if(length(unique(lapply(explainers, function(x) { colnames(x$data) }))) != 1) {
    stop("Explainers unique datasets amount does not equal 1.\nYou have to base explainers on data with the same columns!")
  }

  return(explainers[[1]]$data)
}


#' @noRd
#' @title generate_cols_and_variables
#' @description This funciton creates `cols` - data columns and `selected_variables` - variables chosen by the user objects for further calculations.
#' Additionally, it checks for variables with just one unique value and ignores them.
#' @param data data selected while creating the explainer
#' @param selected_variables variables selected by the user. If not specified, will choose all variables or if there are more than 8 variables, will choose the first 8.
#' @param verbose bool - if text information should be displayed in console
generate_cols_and_variables <- function(data, selected_variables, verbose) {

  if(verbose == TRUE) {
    cat("\tGenerating internal Shiny app objects, part 1.\n")
  }

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


#' @noRd
#' @title create_observation
#' @description Auxiliary function to create expression string used in Shiny application data input.
#' @param data data selected while creating the explainer
#' @param objects_to_template list - information to fill the template, generated by previous functions
#' @param verbose bool - if text information should be displayed in console
create_observation <- function(data, objects_to_template, verbose) {

  if(verbose == TRUE) {
    cat("\tGenerating internal Shiny app objects, part 2.\n")
  }

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


#' @noRd
#' @title save_explainers
#' @description This function loops through all explainers to save them into files.
#' @param explainers explainer list - list of explainers input for the function
#' @param directory string - path to the desired directory location
#' @param objects_to_template list - information to fill the template, generated by previous functions
#' @param verbose bool - if text information should be displayed in console
save_explainers <- function(explainers, directory, objects_to_template, verbose) {

  if(verbose == TRUE) {
    cat("\tSaving explainers files to the final directory\n")
    cat("\tGenerating internal Shiny app objects, part 3.\n")
  }

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

  if(verbose == TRUE) {
    cat("\tExplainers saved properly\n")
  }

  objects_to_template['libs'] <- libs
  objects_to_template['buttons'] <- buttons
  objects_to_template['explainers_reactive'] <- explainers_reactive
  objects_to_template['explainers_static'] <- explainers_static

  return(objects_to_template)
}


#' @noRd
#' @title fill_template
#' @description This function generates template text by filling all the necessary placeholders with created data.
#' @param objects_to_template list - information to fill the template, generated by previous functions
#' @param explainers explainer list - list of explainers input for the function
#' @param verbose bool - if text information should be displayed in console
fill_template <- function(objects_to_template, explainers, verbose) {

  if(verbose == TRUE) {
    cat("\tGenerating Shiny application by filling the template file\n")
  }

  # Reading necessary files: template and static text for prediction description
  static_text <- read.csv(system.file("extdata", "app_static_text.csv", package="xai2shiny"), sep = ';')
  prediction_text <- ifelse(explainers[[1]]$model_info$type == "classification",
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


#' @noRd
#' @title save_files
#' @description This function saves filled template text to the file as a Shiny app and XAI tab content
#' @param directory string - path to the desired directory location
#' @param template_text_filled template text - shiny application text that will be writen to the file
#' @param verbose bool - if text information should be displayed in console
save_files <- function(directory, template_text_filled, verbose) {

  if(verbose == TRUE) {
    cat("\tSaving application file\n")
  }

  # Saving Shiny app
  template_file_path <- paste0(directory, "/app.R")
  file.create(template_file_path)
  template_file_conn <- file(template_file_path)
  writeLines(template_text_filled, template_file_conn)
  close(template_file_conn)
}
