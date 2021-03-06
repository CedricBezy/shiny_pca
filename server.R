##-----------------------------.
# Cedric Bezy
# 06 / 03 / 2018
# Shiny : Server
##-----------------------------.
##==============================================.
## SERVER
##==============================================.

shinyServer(function(input, output, session)
{
    ##==============================================.
    ## UPLOAD DATA
    ##==============================================.
    ## This reactive function consists on importing Data.
    ## The user can choose an available data or import a csv file.
    
    ImportData <- reactive({
        ## path
        style_upload <- input$style_upload
        if(style_upload == "exists"){
            Data <- switch(
                input$upload_df,
                "diamonds" = diamonds,
                "iris" = iris,
                "mtcars" = mtcars,
                "rock" = rock,
                data.frame(stringsAsFactors = FALSE)
            )
        }else if(style_upload == "csv"){
            path <- input$upload_file
            ## Read CSV
            if(is.null(path)){ 
                Data <- NULL
            }else{
                ## importation
                Data <- read.csv2(
                    path$datapath,
                    header = TRUE,
                    sep = input$file_sep,
                    quote = "\"",
                    na.strings = "",
                    stringsAsFactors = FALSE
                )
            }
        }
        varsid <- names(which(sapply(Data, nb_values) == nrow(Data)))
        updateSelectizeInput(
            session = session,
            inputId = "pca_id",
            choices = c("rownames", varsid)
        )
        # RESULT
        return(Data)
    })
    
    # Output Data
    output$dataOutput <- renderTable(ImportData())
    output$dataname <- renderText(input$upload_df)
    
    ##==============================================.
    ## UPLOAD DATA
    ##==============================================.
    ##---------------------
    ## VARIABLE
    ##---------------------
    Selectors_Pca_Vars <- reactive({
        # features columns
        oknum <- sapply(ImportData(), is.numeric)
        varsnum <- names(which(oknum))
        # vars_selector
        ui_vars <- selectizeInput(
            inputId = "pca_main_vars",
            label = "Select Variables",
            choices = setdiff(
                varsnum,
                input$pca_id
            ),
            multiple = TRUE,
            options = list(
                placeholder = 'Select main variables'
            )
        )
        return(ui_vars)
    })
    output$select_pca_vars <- renderUI(Selectors_Pca_Vars())
    
    ##---------------------
    ## QUANTI SUP
    ##---------------------
    Selectors_Pca_QuantiSup <- reactive({
        # features columns
        oknum <- sapply(ImportData(), is.numeric)
        still_selected <- c(input$pca_id, input$pca_main_vars)
        varsnum <- setdiff(names(which(oknum)), still_selected)
        
        # var_sup selector
        ui_quantisup <- selectizeInput(
            inputId = "pca_quanti_sup",
            label = "Numeric Suppl.",
            choices = varsnum,
            multiple = TRUE,
            options = list(
                placeholder = 'Select quanti suppl.'
            )
        )
        return(ui_quantisup)
    })
    output$select_pca_quanti_sup <- renderUI(Selectors_Pca_QuantiSup())
    
    ##---------------------
    ## QUALI SUP
    ##---------------------
    Selectors_Pca_QualiSup <- reactive({
        # features columns
        data <- ImportData()
        oknum <- sapply(data, is.numeric)
        okid <- sapply(data, nb_values) == nrow(data)
        varsfact <- names(which(!oknum & !okid))
        
        # quali_sup selector
        ui_qualisup <- selectizeInput(
            inputId = "pca_quali_sup",
            label = "Quali Suppl.",
            choices = setdiff(
                varsfact,
                input$pca_id
            ),
            multiple = TRUE,
            options = list(
                placeholder = 'Select quali suppl.'
            )
        )
        return(ui_qualisup)
    })
    output$select_pca_quali_sup <- renderUI(Selectors_Pca_QualiSup())
    
    ##---------------------
    ## IND SUP
    ##---------------------
    Selectors_Pca_IndSup <- reactive({
        vchoices <- if(input$pca_id == "rownames"){
            rownames(ImportData())
        }else{
            ImportData()[[input$pca_id]]
        }
        ui_indsup <- selectizeInput(
            inputId = "pca_ind_sup",
            label = "Ind Sup",
            choices = vchoices,
            multiple = TRUE,
            options = list(
                placeholder = 'Select suppl. individuals'
            )
        )
        return(ui_indsup)
    })
    output$select_pca_ind_sup <- renderUI(Selectors_Pca_IndSup())
    
    ##==============================================.
    ## Test
    ##==============================================.
    
    output$test_output <- renderPrint({
        list(
            id = input$pca_id,
            main = input$pca_main_vars,
            quanti_sup = input$pca_quanti_sup,
            quali_sup = input$pca_quali_sup,
            ind_sup = input$pca_ind_sup
        )
    })
    
    ##==============================================.
    ## Make PCA
    ##==============================================.
    
    MakePca <- eventReactive(input$button_pca, {
        # update axis
        updateActionButton(
            session = session,
            inputId = "button_pca",
            label = "Update PCA !"
        )
        vid <- input$pca_id
        vsups <- c(input$pca_quanti_sup,
                   input$pca_quali_sup)
        if(length(vsups) == 0){vsups <- NULL}
        
        indsups <- input$pca_ind_sup
        if(vid == "rownames"){indsups <- as.numeric(indsups)}
        if(length(indsups) == 0){indsups <- NULL}
        
        Pca <- make_pca(
            data = ImportData(),
            variables = input$pca_main_vars,
            id = switch(vid, "rownames" = NULL, vid),
            var_sup = vsups,
            ind_sup = indsups
        )
        dfEigs <- output_eigens_pca(Pca, c(3, 2, 2))
        
       
        ncp <- Pca$call$ncp
        axisNames <- sprintf("PC.%i (%.2f%%)", 1:ncp, dfEigs$PCT_INERTIA)
        updateSelectizeInput(
            session = session,
            inputId = "axis_x",
            choices = setNames(1:ncp, axisNames),
            selected = 1
        )
        updateSelectizeInput(
            session = session,
            inputId = "axis_y",
            choices = setNames(1:ncp, axisNames),
            selected = 2
        )
        # result
        reslist <- list(
            pca = Pca,
            eigs = dfEigs
        )
        return(reslist)
    })
    
    ##==============================================.
    ## Output Eigs
    ##==============================================.
    
    output$eigens_data <- renderTable({
        MakePca()$eigs
    })
    
    ##==============================================.
    ## Output Plots
    ##==============================================.
    
    OutputPlots <- reactive({
        pca <- MakePca()$pca
        axes <- c(as.integer(input$axis_x), as.integer(input$axis_y))
        with_plotly <- TRUE
        reslist <- list(
            ind = output_pca_plot_inds(
                pca,
                axes,
                with_plotly = with_plotly
            ),
            correl = output_pca_plot_correl(
                pca,
                axes,
                with_plotly = with_plotly
            ),
            quali = output_pca_plot_quali(
                pca,
                axes,
                with_plotly = with_plotly
            )
        )
    })
    output$ind_plot <- renderPlotly({
        p <- OutputPlots()$ind
        p$elementId <- NULL
        p <- layout(p, autosize=TRUE, showlegend = TRUE)
        p
    })
    output$correl_plot <- renderPlotly({
        p <- OutputPlots()$correl
        p$elementId <- NULL
        p <- layout(p, autosize=TRUE, showlegend = TRUE)
        p
    })
    output$quali_plot <- renderPlotly({
        p <- OutputPlots()$quali
        p$elementId <- NULL
        p <- layout(p, autosize=TRUE, showlegend = TRUE)
        p
    })
    
    
    #####################################################################.
    ## END
    #####################################################################.
})