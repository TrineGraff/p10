mcs80_TR <- readRDS("mcs80.rds")
mcs90_TR <- readRDS("mcs90.rds")
mcs95_TR <- readRDS("mcs95.rds")

mcs80_Tmax <- readRDS("mcs80_Tmax.rds")
mcs90_Tmax <- readRDS("mcs90_Tmax.rds")
mcs95_Tmax <- readRDS("mcs95_Tmax.rds")

models <- c("AR(4)",
            "Faktor model (IC1)", "Faktor model (IC2)", "Faktor model (IC3)",
            "lasso (CV)", "lasso (BIC)",
            "ridge regression (CV)", "ridge regression (BIC)",
            "Group lasso (CV)", "Group lasso (BIC)",
            "Adaptive lasso m. OLS (CV)", "Adaptive lasso m. OLS (BIC)",
            "Adaptive lasso m. lasso (CV)", "Adaptive lasso m. lasso (BIC)",
            "LARS u. lasso modifikation (CV)", "LARS u. lasso modifikation (BIC)",
            "LARS m. lasso modifikation (CV)", "LARS m. lasso modifikation (BIC)")



# fra 90% til 80% confidence
for (loss in c("abs", "sq")) {
    mcs90_models <- mcs90_Tmax[[loss]]@Info$model.names
    mcs80_models <- mcs80_Tmax[[loss]]@Info$model.names
    diffs <- setdiff(mcs90_models, mcs80_models)
    cat(length(mcs90_models), " vs ", length(mcs80_models), " models\n -",
        paste0(diffs, collapse = "\n- "), "\n", sep = "")
}



# Overensstemmelse mellem abs og sq tab mål
  mcs_abs_models <- mcs80_Tmax$abs[[area]]@Info$model.names
  mcs_sq_models <- mcs80_Tmax$sq[[area]]@Info$model.names
  common <- intersect(mcs_sq_models, mcs_abs_models)
  cat("--- ", area, ": ", length(mcs_abs_models), " vs ", length(mcs_sq_models), " models\n",
      "Models in common (", length(common), "):\n- ",
      paste0(common, collapse = "\n- "), "\n\n", sep = "")

# Number of times each model is included in the SSM
ranks <- setNames(vector("integer", length(models)), models)
for (loss in c("abs", "sq")) {
    ranks <- ranks + (models %in% mcs80[[loss]]@Info$model.names)
}
round(data.frame(Rate = sort(ranks, decreasing = TRUE) / 12), 2)
