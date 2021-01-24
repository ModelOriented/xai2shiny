library(shinytest)

shinytest::installDependencies()

data <- DALEX::titanic_imputed

mod_glm <- glm(survived ~ ., data, family = "binomial")
explainer_glm <- DALEX::explain(mod_glm, data = data[,-8], y=data$survived)

xai2shiny(explainer_glm, directory = paste0(getwd(), '/xai2shiny'), run = FALSE)

test_that("All files are created",{
  expect_true("xai2shiny" %in% list.files())
  expect_true("app.R" %in% list.files("xai2shiny"))
  expect_true("exp1.rds" %in% list.files("xai2shiny"))
})

app <- ShinyDriver$new("xai2shiny/")

test_that("The application runs",{
  output_pred <- app$getValue(name = "text_pred")
  pred_base <- substr(output_pred, 9, 12)
  expect_equal(pred_base, "0.08")

  output_bd_description <- app$getValue(name = "text_predictprofile")
  expect_equal(output_bd_description, "")

  output_cp_description <- app$getValue(name = "text_ceterisparibus")
  expect_equal(output_cp_description, "")

  output_performance <- app$getValue(name = "text_performance")
  expect_equal(output_performance, "")

  # Modyfing text checkbox
  app$setInputs(selected_features = c("Local explanations", "Model performance", "Text description"))

  output_bd_description <- app$getValue(name = "text_predictprofile")
  bd_description_base <- substr(output_bd_description, 1, 2)
  expect_equal(bd_description_base, "Lm")

  output_cp_description <- app$getValue(name = "text_ceterisparibus")
  cp_description_base <- substr(output_cp_description, 1, 3)
  expect_equal(cp_description_base, "For")

  output_performance <- app$getValue(name = "text_performance")
  performance_base <- substr(output_performance, 1, 11)
  expect_equal(performance_base, "Performance")
})

app$stop()

test_that("The selected_variables parameter functions properly",{
  expect_true({
    xai2shiny(explainer_glm, selected_variables = "age", directory = paste0(getwd(), '/xai2shiny_test2'), run = FALSE)
    TRUE
  })
})

test_that("Two explainers with different datasets will produce an error",{
  explainer_glm2 <- DALEX::explain(mod_glm, data = data[,c(-7,-8)], y=data$survived)
  expect_error({
    xai2shiny(explainer_glm, explainer_glm2, directory = paste0(getwd(), '/xai2shiny_test3'), run = FALSE)
  })
})

test_that("Two model packages load properly",{
  explainer_glm$model_info$package <- c("stats", "base")
  expect_true({
    xai2shiny(explainer_glm, directory = paste0(getwd(), '/xai2shiny_test4'), run = FALSE)
    TRUE
  })
})

test_that("Models with model packages including space but created using H2O load properly",{
  explainer_glm$model_info$package <- "H2O with space"
  expect_true({
    xai2shiny(explainer_glm, directory = paste0(getwd(), '/xai2shiny_test5'), run = FALSE)
    TRUE
  })
})

test_that("Other models with model packages including space provide an error",{
  explainer_glm$model_info$package <- "stats with space"
  expect_error({
    xai2shiny(explainer_glm, directory = paste0(getwd(), '/xai2shiny_test6'), run = FALSE)
  })
})



unlink("xai2shiny", recursive = TRUE)
unlink("xai2shiny_test2", recursive = TRUE)
unlink("xai2shiny_test3", recursive = TRUE)
unlink("xai2shiny_test4", recursive = TRUE)
unlink("xai2shiny_test5", recursive = TRUE)
unlink("xai2shiny_test6", recursive = TRUE)
