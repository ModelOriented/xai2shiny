library(ranger)
library(shinytest)

shinytest::installDependencies()

data <- DALEX::titanic_imputed

mod_glm <- glm(survived ~ ., data, family = "binomial")
explainer_glm <- DALEX::explain(mod_glm, data = data[,-8], y=data$survived)

xai2shiny(explainer_glm, directory = getwd(), run = FALSE)

test_that("All files are created",{
  expect_true("xai2shiny" %in% list.files())
  expect_true("app.R" %in% list.files("xai2shiny"))
  expect_true("exp1.rds" %in% list.files("xai2shiny"))
})

app <- ShinyDriver$new("xai2shiny/")

test_that("The application runs",{
  output_pred <- app$getValue(name = "textPred")
  expect_equal(output_pred, "<strong>0.0602054929559495</strong>")
  output_performance <- app$getValue(name = "textid3")
  expect_equal(output_performance, "")
  output_bd_description <- app$getValue(name = "textid1")
  expect_equal(output_bd_description, "")
  output_cp_description <- app$getValue(name = "textid2")
  expect_equal(output_cp_description, "")
  app$setInputs(text_yesno = TRUE)
  output_performance <- app$getValue(name = "textid3")
  expect_equal(output_performance, "Performance measures for classification:<br>Recall: 0.574<br>Precision: 0.747<br>F1: 0.649<br>Accuracy: 0.8")
  output_bd_description <- app$getValue(name = "textid1")
  expect_equal(output_bd_description, "Lm predicts, that the prediction for the selected instance is 0.06 which is lower than the average model prediction. The most important variable that decrease the prediction is class. Other variables are with less importance. The contribution of all other variables is 0.")
  output_cp_description <- app$getValue(name = "textid2")
  expect_equal(output_cp_description, "For the selected instance, prediction estimated by Lm is equal to 0.06. Model's prediction would increase substantially if the value of gender variable would change to \"female\". The largest change would be marked if gender variable would change to \"female\". All the variables were displayed.")
})

app$stop()

unlink("xai2shiny", recursive = TRUE)
