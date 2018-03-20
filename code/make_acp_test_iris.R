
rm(list = ls())
source('code/packages.R')
source('code/func_moda.R')
source('code/make_acp.R')


data(iris)

index <- list(
    id = NULL,
    main = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    quali.sup = "Species"
)

data <- iris
variables <- index$main
id <- index$id
var_sup <- index$quali.sup
ind_sup <- c(1)

# debugonce(make_pca)
pca <- make_pca(iris, variables, id, var_sup, ind_sup = ind_sup)

output_eigens_pca(pca)

output_pca_plot_inds(pca, axes = c(1, 2))
output_pca_plot_correl(pca, axes = c(1, 2))
output_pca_plot_quali(pca, axes = c(1, 2))



