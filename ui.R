##-----------------------------.
# Cedric Bezy
# 06 / 03 / 2018
# Shiny : UI
##-----------------------------.

##==============================================.
## SHINY UI
##==============================================.

shinyUI(
    navbarPage(
        ##===================================.
        ## NavBar
        ##===================================.
        title = "Shiny-PCA",
        theme = shinytheme("flatly"),
        tabPanel(
            title = "Upload Data",
            fluidPage(
                useShinyjs(),
                ##------------------------.
                ## TabSetPanel
                ##------------------------.
                h4("UPLOAD DATA"),
                sidebarLayout(
                    sidebarPanel(
                        radioButtons(
                            inputId = "style_upload",
                            label = "Upload Data",
                            choices = c(
                                "from available data" = "exists"
                            ),
                            selected = "exists"
                        ),
                        conditionalPanel(
                            condition = "input.style_upload === 'exists'",
                            selectInput(
                                inputId = "upload_df",
                                label = "Choose Available data",
                                choices = c("diamonds", "iris", "mtcars", "rock")
                            )
                        ),
                        conditionalPanel(
                            condition = "input.style_upload === 'csv'",
                            fileInput(
                                inputId = "upload_file",
                                label = "Data",
                                accept = c(
                                    "text/csv", 
                                    "text/comma-separated-values,text/plain", 
                                    ".csv"
                                )
                            ),
                            textInput(
                                inputId = "file_sep",
                                label = "Separation",
                                value = ";"
                            )
                        )
                    ),
                    ##--------------------------------.
                    ## Main : View Importation
                    ##--------------------------------.
                    mainPanel(
                        h6("View uploaded data / matrix"),
                        tabsetPanel(
                            id = "tabset_data",
                            tabPanel(
                                title = "Data",
                                tableOutput("dataOutput")
                            ),
                            tabPanel(
                                title = "Metadata" #,
                                # tableOutput("metadataOutput")
                            )
                        )
                    )
                )
            )
        ),
        ##========================================.
        ## End NavbarPANEL
        ##========================================.
        tabPanel(
            title = "PCA",
            fluidPage(
                useShinyjs(),
                ##------------------------.
                ## TabSetPanel
                ##------------------------.
                textOutput("dataname"),
                sidebarLayout(
                    sidebarPanel(
                        width = 12,
                        fluidRow(
                            column(
                                width = 4,
                                selectizeInput(
                                    "pca_id",
                                    label = "Select ID column",
                                    choices = "rownames",
                                    selected = "rownames",
                                    options = list(
                                        placeholder = 'Select id column'
                                    ),
                                    multiple = FALSE
                                ),
                                uiOutput("select_pca_vars")
                            ),
                            column(
                                width = 4,
                                uiOutput("select_pca_quanti_sup"),
                                uiOutput("select_pca_quali_sup")
                            ),
                            column(
                                width = 4,
                                uiOutput("select_pca_ind_sup"),
                                actionButton(inputId = "button_pca",
                                             label = "Make PCA !")
                            )
                        )
                    ),
                    mainPanel(
                        tabsetPanel(
                            id = "set_pca",
                            tabPanel(
                                title = "Plots",
                                sidebarLayout(
                                    sidebarPanel(
                                        width = 4,
                                        selectizeInput(
                                            inputId = "axis_x",
                                            label = "Component X",
                                            choices = NULL,
                                            multiple = FALSE,
                                            options = list(
                                                placeholder = '---'
                                            )
                                        ),
                                        selectizeInput(
                                            inputId = "axis_y",
                                            label = "Component Y",
                                            choices = NULL,
                                            multiple = FALSE,
                                            options = list(
                                                placeholder = '---'
                                            )
                                        )
                                    ),
                                    mainPanel(
                                        plotlyOutput("ind_plot"),
                                        plotlyOutput("correl_plot"),
                                        plotlyOutput("quali_plot")
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tables"
                            ),
                            tabPanel(
                                title = "Eigens",
                                tableOutput("eigens_data")
                            ),
                            tabPanel(
                                title = "Tests",
                                verbatimTextOutput("test_output")
                            ),
                            selected = "Tests"
                        )
                    )
                )
            )
        ),
        ##========================================.
        ## End NavbarPANEL
        ##========================================.
        selected = "Upload Data"
    )
)
