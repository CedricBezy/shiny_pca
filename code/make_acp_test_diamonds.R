##-----------------------------------
## Cedric Bezy
## 
##-----------------------------------

rm(list = ls())

##==================================================
## Import Packages and functions
##==================================================
source('code/packages.R')
source('code/make_acp.R')
source('code/func_moda.R')

##==================================================
## Make ACP
##==================================================
##---------------------------------
# Import Data
##---------------------------------
data('diamonds')

diamonds <- diamonds %>%
    tibble::add_column(id = paste0("D", 1:nrow(diamonds)), .before = 1) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("id")

diamonds <- diamonds %>%
    tibble::add_column(id = paste0("D", 1:nrow(diamonds)), .before = 1)
summary(diamonds)

# dims
N <- min(nrow(diamonds), 5000)
diamonds <- diamonds[1:N,]

##---------------------------------
# ACP
##---------------------------------

ind_sup <- c(2, 6, 12, 14, 16)

list_vars <- list(
    main = c("depth", "table", "price", "x", "y", "z"),
    quanti.sup = "carat",
    quali.sup = c("cut", "color", "clarity")
)

pca <- make_pca(
    data = diamonds,
    variables = list_vars$main,
    id = "id",
    var_sup = c(list_vars$quanti.sup, list_vars$quali.sup),
    ind_sup = ind_sup
)
Pca <- pca
pca$call$index

##==================================================
## Eigens
##==================================================

output_eigens_pca(pca)

##==================================================
## Individuals
##==================================================

choice <- "ind.sup"
axes <- c(1, 2)
shows <- c("main", "sup")
pca_color <- "cut"
pca_size <- 1


# debugonce(output_data_pca)
dfinds_axe1 <- output_pca_data(pca, "ind", axes = c(1, 2))[[1]]

# debugonce(output_plot_pca_inds)
plot_12 <- output_pca_plot_inds(
    pca,
    axes = c(1, 2),
    pca_color = "cut",
    with_plotly = FALSE
)
plot_12

# debugonce(output_plot_pca_inds)
plot_12 <- output_pca_plot_inds(
    pca,
    c(1, 2),
    pca_color = "cut",
    pca_size = NULL,
    with_plotly = TRUE
)
plot_12


output_pca_plot_correl(
    pca,
    c(1, 2),
    with_plotly = FALSE
)

cor_12 <- output_pca_plot_correl(
    pca,
    c(1, 2),
    with_plotly = TRUE
)
cor_12


quali_13 <- output_pca_plot_quali(
    pca,
    c(1, 3),
    with_plotly = TRUE
)
quali_13


lplots <- output_pca_plots(pca)(1, 2)$ind(selection = c("active", "sup"), pca_color = "cut")


# dfInds <- output_data_pca(pca, results = c("ind", "ind.sup")) %>%
#     dplyr::bind_rows() %>%
#     dplyr::mutate(
#         sum_qlt = rowSums(.[grep("qlt_", names(.))], na.rm = TRUE)
#     )
# plot_ly(
#     data = dfInds,
#     x = ~coord_1,
#     y = ~coord_2,
#     mode = "markers",
#     text = ~paste0(
#         "Id: ", id, "<br>",
#         "Dim_1: (Ctr: ", ctr_1, "; Qlt: ", qlt_1, ')', "<br>",
#         "Dim_2: (Ctr: ", ctr_2, "; Qlt: ", qlt_2, ')', "<br>"
#     )
# )

##==================================================
## Variables
##==================================================

# dfVars <- bind_rows(
#     output_pca("var") %>%
#         tibble::add_column(
#             type = "main",
#             .before = 1
#         ),
#     output_pca("quanti.sup") %>%
#         tibble::add_column(
#             type = "quanti.sup",
#             .before = 1
#         ),
#     output_pca("quali.sup") %>%
#         tibble::add_column(
#             type = "quali.sup",
#             .before = 1
#         )
# )
# 
# list_results <- lapply(names(useful$PCA), output_pca)
# names(list_results) <- names(useful$PCA)



