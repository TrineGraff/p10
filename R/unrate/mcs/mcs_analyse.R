mcs80 <- readRDS("mcs80.rds")
mcs90 <- readRDS("mcs90.rds")

models <- c("ridge regressin",
            "lasso",
            "EN", 
            "group lasso",
            "adaptive lasso med OLS vægte",
            "adaptive lasso med ridge vægte")



# fra 90% til 80% confidence
for (loss in c("abs", "sq")) {
    mcs90_models <- mcs90[[loss]]@Info$model.names
    mcs80_models <- mcs80[[loss]]@Info$model.names
    diffs <- setdiff(mcs90_models, mcs80_models)
    cat(length(mcs90_models), " vs ", length(mcs80_models), " models\n -",
        paste0(diffs, collapse = "\n- "), "\n", sep = "")
}










# Overensstemmelse mellem abs og sq tab mål
for (area in areas) {
  mcs_abs_models <- mcs80$abs[[area]]@Info$model.names
  mcs_sq_models <- mcs80$sq[[area]]@Info$model.names
  common <- intersect(mcs_sq_models, mcs_abs_models)
  cat("--- ", area, ": ", length(mcs_abs_models), " vs ", length(mcs_sq_models), " models\n",
      "Models in common (", length(common), "):\n- ",
      paste0(common, collapse = "\n- "), "\n\n", sep = "")
}

# Number of times each model is included in the SSM
ranks <- setNames(vector("integer", length(models)), models)
for (loss in c("abs", "sq")) {
    ranks <- ranks + (models %in% mcs80[[loss]]@Info$model.names)
}
round(data.frame(Rate = sort(ranks, decreasing = TRUE) / 12), 2)
