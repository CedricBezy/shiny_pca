##===============================================================
## function de verification
##===============================================================

.check_axes <- function(axes){
    if(length(axes) != 2){
        stop(paste('"axes" must be of length 2, instead of', length(axes)))
    }
}

.check_variables <- function(varname,
                             data,
                             func = NULL,
                             msg = "func(%s) returns FALSE",
                             ...)
{
    if(!(varname %in% colnames(data))){
        return(sprintf("%s is not a variable of data! Removed!", varname))
    }else if(is.function(func)){
        if(!func(data[[varname]], ...)){
            return(sprintf(msg, varname))
        }else{
            return(TRUE)
        }
    }else{
        return(TRUE)
    }
}

##===============================================================
## Select Vars Name
##===============================================================

select_varnames <- function(data, func = NULL, ...){
    if(!is.function(func)){
        res <- colnames(data)
    }else{
        res <- names(which(sapply(data, func, ...)))
    }
    return(res)
}

##===============================================================
## BUILD PCA
##===============================================================

useful <- list(
    'PCA' = list(
        'ind' = c('coord', 'contrib', 'cos2'),
        'ind.sup' = c('coord', 'cos2'),
        'var' = c('coord', 'contrib', 'cos2', 'cor'),
        'quanti.sup' = c('coord', 'cos2', 'cor'),
        'quali.sup' = c('coord', 'cos2', 'v.test')
    )
)

.get_prefix_pca <- function(prefix){
    switch(
        prefix,
        "coord" = "coord",
        "contrib" = "ctr",
        "cos2" = "qlt",
        "cor" = "cor",
        "v.test" = "vtest",
        "eta2" = "eta2",
        "dim"
    )
}

make_pca <- function(data,
                     variables = setdiff(
                         names(which(sapply(data, is.numeric))),
                         c(id, var_sup)
                     ),
                     id = NULL,
                     var_sup = NULL,
                     ind_sup = NULL,
                     ...)
{
    # This function realizes a PCA.
    ##-----------------------.
    ## VERIFICATIONS
    ##-----------------------.
    if(!is.data.frame(data)){
        stop("Data is not a data.frame")
    }
    if(length(variables)<=1){
        stop("variable needs at least 2 names of numeric variables!")
    }
    ok_main <- sapply(
        variables,
        .check_variables,
        data = data,
        func = is.numeric,
        msg = "%s is not numeric! Converted as a qualitative sup."
    )
    # Warning Message for main variables
    varnotnum <- ok_main[which(ok_main != TRUE)]
    invisible(sapply(varnotnum, warning))
    variables <- variables[which(variables %in% colnames(data))]
    variables <- setdiff(variables, names(varnotnum))
    var_sup <- c(var_sup, names(varnotnum))
    
    # check var_sup
    ok_sup <- sapply(var_sup, .check_variables,  data = data)
    invisible(sapply(ok_sup[which(ok_sup != TRUE)], warning))
    var_sup <- var_sup[which(var_sup %in% colnames(data))]
    
    # check id variable
    N <- nrow(data)
    if(is.null(id)){
        id <- "rownames"
        data <- data %>%
            tibble::rownames_to_column(id)
    }else if(!(id[1] %in% colnames(data))){
        id <- "rownames"
        data <- data %>%
            tibble::rownames_to_column(id)
    }else{
        data <- data[c(id, setdiff(colnames(data)), id)]
    }
    vid <- data[id]
    
    index_inds <- list(
        "active" = setdiff(1:N, ind_sup),
        "sup" = ind_sup
    )
    
    if(length(var_sup) >= 1){
        ok_nunmsup <- sapply(var_sup, function(x){is.numeric(data[[x]])})
        index_vars <- list(
            id = id,
            main = variables,
            quanti.sup = var_sup[which(ok_nunmsup)],
            quali.sup = var_sup[which(!ok_nunmsup)]
        )
    }else{
        index_vars <- list(
            id = id,
            main = variables
        )
    }
    
    ## make data for pca
    data_for_pca <- data[,unlist(index_vars[-1])]
    data_for_pca <- tibble::remove_rownames(data_for_pca)
    data_for_pca <- data_for_pca[sort(unlist(index_inds)),]
    
    data_for_pca_relevels <- data_for_pca
    namefacts <- names(which(sapply(data_for_pca_relevels, is.factor)))
    for(iname in namefacts){
        ifact <- data_for_pca[[iname]]
        data_for_pca_relevels[[iname]] <- factor(
            ifact,
            levels = levels(ifact),
            labels = paste(iname, levels(ifact), sep = "_//_")
        )
    }
          
    ## get index num for variables
    indexnum_vars <- lapply(
        index_vars[-1],
        function(x){
            if(length(x) >= 1){
                match(x, colnames(data_for_pca))
            }else{
                NULL
            }
        }
    )
    
    pca <- FactoMineR::PCA(
        data_for_pca_relevels,
        ncp = length(variables),
        ind.sup = index_inds[["sup"]],
        quanti.sup = indexnum_vars[["quanti.sup"]],
        quali.sup = indexnum_vars[["quali.sup"]],
        graph = FALSE
    )
    pca$call$X <- data_for_pca
    pca$call$index <- list(
        inds = index_inds,
        vars = index_vars
    )
    pca$call$id <- vid
    
    for(i in names(useful$PCA)){
        for(j in useful$PCA[[i]]){
            imat <- pca[[i]][[j]]
            if(is.matrix(imat)){
                colnames(imat) <- sub(
                    "^Dim.",
                    paste0(.get_prefix_pca(j), "_"),
                    colnames(imat)
                )
                pca[[i]][[j]] <- imat
            }
        }
    }
    return(pca)
}

##===============================================================
## Eigen Values
##===============================================================

output_eigens_pca <- function(pca,
                              digits = c(3, 2, 2))
{
    ## fonction pour obtenir la table d'inertie
    matEig <- pca$eig
    N <- nrow(matEig)
    PC <- factor(
        seq(1, N),
        levels = seq(1,N),
        labels = paste("Dim", seq(1,N), sep = " ")
    )
    dfEig <- cbind(PC, as.data.frame(matEig))
    colnames(dfEig) <- c('DIM', 'EIGEN_VALUE', 'PCT_INERTIA', 'CUM_INERTIA')
    rownames(dfEig) <- seq(1, N)
    if(is.numeric(digits)){
        ndig <- length(digits)
        dfEig <- within(dfEig, {
            EIGEN_VALUE = round(EIGEN_VALUE, digits[min(1, ndig)])
            PCT_INERTIA = round(PCT_INERTIA, digits[min(2, ndig)])
            CUM_INERTIA = round(CUM_INERTIA, digits[min(3, ndig)])
        })
    }
    return(dfEig)
}

##===============================================================
## OUTPUT DATA
##===============================================================
##------------------------------
# Make Data
##------------------------------

output_pca_data <- function(
    pca,
    results = names(useful$PCA),
    axes = seq(1, pca$call$ncp),
    digits = 3
){
    # Function for each matrix
    .output_matrix_pca <- function(iname, list_pca){
        # Get Matrix
        imat <- list_pca[[iname]]
        ## Rownames
        if(!is.matrix(imat)){
            imat <- as.matrix(t(imat))
            rownames(imat) <- switch(
                choice,
                "quanti.sup" = colnames(pca$call$quanti.sup),
                "ind.sup" = rownames(pca$call$X[pca$call$ind.sup,])
            )
        }
        # Format
        imat <- round(imat, digits)
        colnames(imat) <- sub(
            "^Dim.",
            paste0(.get_prefix_pca(iname), "_"),
            colnames(imat)
        )
        iDf <- as.data.frame(imat)[axes]
        return(iDf)
    }
    
    # Function for each choosen feature
    .make_data_choice <- function(choice){
        # Existence of results
        list_pca <- pca[[choice]]
        if(!is.null(list_pca)){
            # Init
            indicators <- useful$PCA[[choice]]
            index <- pca$call$index
            # Init Result DF
            resDf <- switch(
                choice,
                "ind" = data.frame(
                    pca_id = index$inds$active,
                    pca_type = "active",
                    stringsAsFactors = FALSE
                ),
                "ind.sup" = data.frame(
                    pca_id = index$inds$sup,
                    pca_type = "sup",
                    stringsAsFactors = FALSE
                ),
                "var" = data.frame(
                    pca_variable = rownames(pca$var$coord),
                    pca_type = "main",
                    stringsAsFactors = FALSE
                ),
                "quanti.sup" = data.frame(
                    pca_variable = rownames(pca$quanti.sup$coord),
                    pca_type = "quanti_sup",
                    stringsAsFactors = FALSE
                ),
                "quali.sup" = {
                    xrows <- strsplit(rownames(pca$quali.sup$coord), "_//_")
                    data.frame(
                        pca_variable = sapply(xrows, function(x){x[1]}),
                        pca_moda = sapply(xrows, function(x){x[2]}),
                        pca_type = "quali_sup",
                        stringsAsFactors = FALSE
                    )
                }
            )
            # Function for each matrix
            res_list <- lapply(indicators, .output_matrix_pca, list_pca = list_pca)
            pcadf <- bind_cols(resDf, res_list)
            return(pcadf)
        }else{
            return(invisible())
        }
        return(pcadf)
    }
    ## RESULT : apply previous function
    names(results) <- results
    reslist <- lapply(results, .make_data_choice)
    return(reslist)
}


##===============================================================
## OUTPUT PLOT
##===============================================================

get_palette_default <- function(palette, n, direction = 1, default = "grey",
                                grey_min = 0.2, grey_max = 0.8){
    .default_col <- function(){
        switch(
            default,
            "hue" = hue_pal(direction = direction)(n),
            "grey" = if(direction == 1){
                grey_pal(grey_min, grey_max)(n)
            }else{
                grey_pal(grey_max, grey_min)(n)
            }
        )
    }
    palDf <- brewer.pal.info
    palnames <- rownames(palDf)
    
    res_pal <- switch(
        palette,
        "hue" = hue_pal(direction = direction)(n),
        "grey" = if(direction == 1){
            grey_pal(grey_min, grey_max)(n)
        }else{
            grey_pal(grey_max, grey_min)(n)
        },
        if(palette %in% palnames){
            npal <- palDf[which(palnames == palette), "maxcolors"]
            if(n <= npal){
                brewer_pal(palette = palette, direction = direction)(n)
            }else{
                .default_col()
            }
        }else if(tolower(palette) %in% tolower(palnames)){
            w <- which(tolower(palnames) == tolower(palette))
            warning(sprintf("%s doesnt exist ! Do you mean \"%s\" ?",
                            palette, palnames[w]))
            npal <- palDf[w, "maxcolors"]
            if(n <= npal){
                brewer_pal(palette = palnames[w], direction = direction)(n)
            }else{
                .default_col()
            }
        }else{
            warning(sprintf("%s doesnt exist ! Default %s has been built",
                            palette, default))
            .default_col()
        }
    )
    return(res_pal)
}

## Fonctions pour obtenir un cercle
getCircle <- function(rayon = 1, centre = c(0,0), nbpoints = 100){
    tt <- seq(0, 2*pi, length.out = nbpoints)
    xx <- centre[1] + rayon * cos(tt)
    yy <- centre[2] + rayon * sin(tt)
    return(data.frame(x = xx, y = yy))
}

##===============================================================
## OUTPUT PLOT IND
##===============================================================

output_pca_plot_inds <- function(pca,
                                  axes = c(1, 2),
                                  selection = c("active", "sup"),
                                  pca_color = NULL,
                                  pca_size = NULL,
                                  with_plotly = TRUE,
                                  palette = "hue",
                                  ...)
{
    # PCA
    data <- pca$call$X
    variables <- colnames(data)
    index_inds <- pca$call$index$inds
    vinds <- unlist(index_inds)
    results <- c(if("active" %in% selection){"ind"},
                 if("sup" %in% selection){"ind.sup"})
    ##----------------------.
    # Make Data
    ##----------------------.
    dfInds <- output_pca_data(pca, results = results, axes = axes) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(
            sum_qlt = rowSums(.[grep("qlt_", names(.))], na.rm = TRUE)
        )
    
    ##----------------------.
    # Make Data
    ##----------------------.
    if(!is.null(pca_color)){
        if(pca_color %in% variables){
            dfInds[[pca_color]] <- data[vinds, pca_color]
            pal_col <- get_palette_default(
                palette = palette,
                n = nlevels(dfInds[[pca_color]]),
                direction = -1
            )
        }else{
            warning(sprintf("Unknown Color Variable %s", pca_color))
            pca_color <- "pca_type"
            pal_col <- c("blue", "purple")
        }
    }
    if(length(pca_size) == 1){
        if(is.numeric(pca_size)){
            dfInds[["pca_size"]] <- rep(pca_size, N)
        }else if(pca_size %in% variables){
            dfInds[["pca_size"]] <- data[vinds, pca_size]
        }else if(pca_size == "sum_qlt"){
            dfInds[["pca_size"]] <- dfInds[["sum_qlt"]]
        }else{
            warning(sprintf("Unknown Color Variable %s", as.character(pca_size)))
            pca_size <- NULL
        }
    }else{
        pca_size <- NULL
    }
    
    formatText <- paste0(
        'paste0(',
        '"<i>(", pca_type, ")</i>", "<br>",',
        '"<b>Id: ", pca_id, "</b>", "<br>",',
        if(!is.null(pca_color)){
            sprintf('"%1$s: ", %1$s, "<br>",', pca_color)
        },
        '"***<br>",',
        '"<b>Dim %1$i:</b><br>",',
        '"Coord: ", coord_%1$i, "<br>",',
        '"(Ctr: ", ctr_%1$i, "; Qlt: ", qlt_%1$i, ")", "<br>",',
        '"***<br>",',
        '"<b>Dim %2$i:</b><br>",',
        '"Coord: ", coord_%2$i, "<br>",',
        '"(Ctr: ", ctr_%2$i, "; Qlt: ", qlt_%2$i, ")", "<br>"',
        ')'
    )
    dfInds <- dfInds %>%
        dplyr::mutate_(
            pca_text = sprintf(formatText, axes[1], axes[2])
        )
    
    ##----------------------.
    # make plot
    ##----------------------.
    a1 <- axes[1]
    a2 <- axes[2]
    # Initialise Plot
    resplot <- ggplot(
        mapping = aes_string(
            x = sprintf("coord_%i", a1),
            y = sprintf("coord_%i", a2)
        ),
        data = dfInds
    ) +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        theme_bw()
    
    suppressWarnings({
        resplot <- resplot +
            geom_point(
                mapping = aes_string(
                    shape = "pca_type",
                    col = pca_color,
                    size = if(!is.null(pca_size)){"pca_size"}else{NULL},
                    text = "pca_text"
                )
            )
    })
    
    ## Legend
    resplot <- resplot +
        scale_color_manual(values = pal_col) +
        scale_shape_manual(values = c(16, 1)) +
        guides(shape = FALSE, size = FALSE)
    
    ## Axes
    resplot <- resplot +
        scale_x_continuous(
            name = sprintf(
                "Dim %i (%s%%)", 
                a1, 
                formatC(pca$eig[a1,2], digits = 2, format = "f")
            )
        ) +
        scale_y_continuous(
            name = sprintf(
                "Dim %i (%s%%)",
                a2, 
                formatC(pca$eig[a2,2], digits = 2, format = "f")
            )
        )
    
    ## Title
    resplot <- resplot +
        ggtitle(
            "PCA : individuals plot",
            sprintf("Axes %i - %i", a1, a2)
        )
    
    if(with_plotly){
        suppressMessages({
            resplot <- ggplotly(resplot, tooltip = c("text"))
        })
    }
    return(resplot)
}



##===============================================================
## OUTPUT PLOT CORREL
##===============================================================

output_pca_plot_correl <- function(pca,
                                   axes = c(1, 2),
                                   selection = c("main", "sup"),
                                   with_plotly = TRUE,
                                   palette = "hue",
                                   ...)
{
    ##----------------------.
    # Make Data
    ##----------------------.
    vselect <- c(if("main" %in% selection){"var"},
                 if("sup" %in% selection){"quanti.sup"})
    
    index_vars <- pca$call$index$vars
    dfVars <- output_pca_data(pca, results = vselect, axes = axes) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(
            pca_variable = factor(
                pca_variable,
                levels = c(index_vars$main, index_vars$quanti.sup)
            ),
            sum_qlt = rowSums(.[grep("qlt_", names(.))], na.rm = TRUE)
        )
    
    vok <- (dfVars$pca_type == "main")
    vpal <- c(get_palette_default("hue", sum(vok)),
              get_palette_default("grey", sum(!vok), grey_min = 0.3, grey_max = 0.7))
    
    ## Cercle
    dfCercle <- getCircle(nbpoints = 360)
    
    ##----------------------.
    # Make Data
    ##----------------------.
    
    formatText <- paste0(
        'paste0(',
        '"<i>(", pca_type, ")</i>", "<br>",',
        '"Variable: <b>", pca_variable, "</b>", "<br>",',
        '"***<br>",',
        '"<b>Dim %1$i:</b><br>",',
        '"Coord: ", coord_%1$i, "<br>",',
        '"(Ctr: ", ctr_%1$i, "; Qlt: ", qlt_%1$i, ")", "<br>",',
        '"***<br>",',
        '"<b>Dim %2$i:</b><br>",',
        '"Coord: ", coord_%2$i, "<br>",',
        '"(Ctr: ", ctr_%2$i, "; Qlt: ", qlt_%2$i, ")", "<br>"',
        ')'
    )
    dfVars <- dfVars %>%
        dplyr::mutate_(
            pca_text = sprintf(formatText, axes[1], axes[2])
        )
    
    ##----------------------.
    # make plot
    ##----------------------.
    a1 <- axes[1]
    a2 <- axes[2]
    # Initialise Plot
    resplot <- ggplot(
        data = dfVars
    ) +
        coord_fixed(ratio = 1) +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        geom_path(
            data = dfCercle,
            mapping = aes(x, y),
            col = "black",
            lwd = 1
        ) +
        theme_bw()
        
    suppressWarnings({
        resplot <- resplot +
            geom_segment(
                x = 0,
                y = 0, 
                mapping = aes_string(
                    xend = sprintf("cor_%i", a1),
                    yend = sprintf("cor_%i", a2),
                    linetype = "pca_type",
                    col = "pca_variable",
                    text = "pca_text"
                ),
                arrow = arrow(),
                lwd = 1
            ) 
    })
    
    ## Legend
    resplot <- resplot +
        scale_color_manual(values = vpal) +
        guides(linetype = FALSE)
    
    ## Axes
    resplot <- resplot +
        scale_x_continuous(
            name = sprintf(
                "Dim %i (%s%%)", 
                a1, 
                formatC(pca$eig[a1,2], digits = 2, format = "f")
            )
        ) +
        scale_y_continuous(
            name = sprintf(
                "Dim %i (%s%%)",
                a2, 
                formatC(pca$eig[a2,2], digits = 2, format = "f")
            )
        )
    
    ## Title
    resplot <- resplot +
        ggtitle(
            "PCA : correlation plot",
            sprintf("Axes %i - %i", a1, a2)
        )
    
    if(with_plotly){
        suppressMessages({
            resplot <- ggplotly(resplot, tooltip = c("text"))
        })
    }
    return(resplot)
}


##===============================================================
## OUTPUT QUALI
##===============================================================

output_pca_plot_quali <- function(pca,
                                  axes = c(1, 2),
                                  variables = pca$call$index$vars$quali.sup,
                                  with_plotly = TRUE,
                                  palette = "hue",
                                  ...)
{
    ##----------------------.
    # Initialize Plot
    ##----------------------.
    a1 <- axes[1]
    a2 <- axes[2]
    resplot <- ggplot() +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        theme_bw()
    
    if(length(pca$call$index$vars$quali.sup >= 1)){
        ##----------------------.
        # Make Data
        ##----------------------.
        dfQuali <- output_pca_data(pca, results = "quali.sup", axes = axes) %>%
            bind_rows() %>%
            dplyr::mutate(
                sum_qlt = rowSums(.[grep("qlt_", names(.))], na.rm = TRUE)
            )
        dfQuali$pca_variable <- factor(
            dfQuali$pca_variable,
            levels = pca$call$index$vars$quali.sup
        )
        dfQuali <- dfQuali %>%
            dplyr::filter(pca_variable %in% variables)
        
        pal_col <- get_palette_default("hue", nlevels(dfQuali$pca_variable))
        ##----------------------.
        # Make Text
        ##----------------------.
        formatText <- paste0(
            'paste0(',
            '"<i>(", pca_type, ")</i>", "<br>",',
            '"<b>", pca_variable, "</b> // <b>", pca_moda, "<br>",',
            '"***<br>",',
            '"<b>Dim %1$i:</b><br>",',
            '"Coord: ", coord_%1$i, "<br>",',
            '"(Qlt: ", qlt_%1$i, ")", "<br>",',
            '"***<br>",',
            '"<b>Dim %2$i:</b><br>",',
            '"Coord: ", coord_%2$i, "<br>",',
            '"(Qlt: ", qlt_%2$i, ")", "<br>"',
            ')'
        )
        dfQuali <- dfQuali %>%
            dplyr::mutate_(
                pca_text = sprintf(formatText, axes[1], axes[2])
            )
        
        ##----------------------.
        # Make plot
        ##----------------------.
        # Initialise Plot
        suppressWarnings({
            resplot <- resplot +
                geom_point(
                    data = dfQuali,
                    mapping = aes_string(
                        x = sprintf("coord_%i", a1),
                        y = sprintf("coord_%i", a2),
                        col = "pca_variable",
                        text = "pca_text"
                    ),
                    shape = 17,
                    size = 3
                ) +
                ## Legend
                scale_color_manual(values = pal_col) +
                guides(shape = FALSE, size = FALSE)
            
            ## Axes
            resplot <- resplot +
                scale_x_continuous(
                    name = sprintf(
                        "Dim %i (%s%%)", 
                        a1, 
                        formatC(pca$eig[a1,2], digits = 2, format = "f")
                    )
                ) +
                scale_y_continuous(
                    name = sprintf(
                        "Dim %i (%s%%)",
                        a2, 
                        formatC(pca$eig[a2,2], digits = 2, format = "f")
                    )
                )
        })
    }
    
    ## Title
    resplot <- resplot +
        ggtitle(
            "PCA : Qualitative Suppl",
            sprintf("Axes %i - %i", a1, a2)
        )
    
    if(with_plotly){
        suppressMessages({
            resplot <- ggplotly(resplot, tooltip = c("text"))
        })
    }
    return(resplot)
}

