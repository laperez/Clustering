# Load library
library(shiny)
library(shinyjs)
library(dplyr)
library(shinyalert)
library(ggplot2)

# Function that manages the combo data with the external metrics

fillComboExternalGraphs <- function(metrics) {
    '%not in%' <- Negate('%in%')

    result <- metrics

    if (length(metrics) > 0) {
        for (m in 1:length(metrics)) {
            if (tolower(metrics[m]) %not in% tolower(c(
                "Entropy",
                "Precision",
                "Recall",
                "Variation_information",
                "F_measure",
                "Fowlkes_mallows_index"
            ))) {
                result <- result[result != metrics[m]]
            }
        }
    }

    return (result)
}

# Function that manages the combo data with the internal metrics

fillComboInternalGraphs <- function(metrics) {
    '%not in%' <- Negate('%in%')

    result <- metrics

    if (length(metrics) > 0) {
        for (m in 1:length(metrics)) {
            if (tolower(metrics[m]) %not in% tolower(c("Connectivity",
                                      "Dunn",
                                      "Silhouette"))) {
                result <- result[result != metrics[m]]
            }
        }
    }

    return (result)
}

df_result <- NULL

# Define server logic required to manages the processing of user interface components.
shinyServer(function(input, output, session) {

    df_result <<- NULL

    # File management component
    shinyDirChoose(
        input,
        'dir',
        roots = c(home = '~'),
        filetypes = c('arff', 'dat', 'csv')
    )

    global <- reactiveValues(datapath = getwd())

    dir <- reactive(input$dir)

    output$dir <- renderText({
        global$datapath
    })

    # Reactive event for the management of external image loading
    observeEvent(input$image1, {
        if (!is.null(input$image1) &&
            input$image1 != "" && !is.null(df_result)) {
            shinyalert(input$image1,"q3423")
            output$plotImage1 <- renderPlot({
                Clustering::plot_clustering(df_result, input$image1)
            })
        }
    })

    # Reactive event for the management of internal image loading
    observeEvent(input$image2, {
        if (!is.null(input$image2) &&
            input$image2 != "" && !is.null(df_result)) {
            output$plotImage2 <- renderPlot({
                Clustering::plot_clustering(df_result, input$image2)
            })
        }
    })

    # Reactive event to manage how the data will be loaded, i.e. if we are going to work with a dataframe or a file directory
    observeEvent(input$typeExecution, {
        if (input$typeExecution == "data") {
            shinyjs::disable("dir")
            shinyjs::enable("datasetTest")
        } else {
            shinyjs::disable("datasetTest")
            shinyjs::enable("dir")
        }
    })


    # Reactive event to manage the algorithm combo, that is, if we select an algorithm through this
    # event we will select the package that contains that algorithm.
    observeEvent(
        ignoreNULL = F,
        ignoreInit = T,
        eventExpr = {
            input$algorithm
        },
        handlerExpr = {
            if (length(input$algorithm) > 0) {
                if (is.null(input$packages)) {
                    selectedPackage = c()
                } else {
                    selectedPackage = c(input$packages)
                }

                algorithm_clustering <-
                    c(
                        "apclusterK",
                        "agnes",
                        "clara",
                        "daisy",
                        "diana",
                        "fanny",
                        "gmm",
                        "fuzzy_cm",
                        "fuzzy_gg",
                        "fuzzy_gk",
                        "hclust",
                        "kmeans_arma",
                        "kmeans_rcpp",
                        "mini_kmeans",
                        "mona",
                        "pam",
                        "pvclust"
                    )

                advclust_algorithm <-
                    c("fuzzy_cm", "fuzzy_gg", "fuzzy_gk")
                amap_algorithm <- c("hclust")
                apcluster_algorithm <- c("apclusterK")
                clusterr_algorithm <-
                    c("agnes",
                      "clara",
                      "daisy",
                      "diana",
                      "fanny",
                      "mona",
                      "pam")
                cluster_algorithm <-
                    c("gmm",
                      "kmeans_arma",
                      "kmeans_rcpp",
                      "mini_kmeans")

                pvclcust_algorithm <- c("pvclust")

                '%notin%' <- Negate('%in%')

                countAlgorithmAdvclust <- 0
                countAlgorithmAmap <- 0
                countAlgorithmApcluster <- 0
                countAlgorithmCluster <- 0
                countAlgorithmClusterR <- 0
                countAlgorithmPvclust <- 0

                for (a in 1:length(algorithm_clustering)) {
                    if (algorithm_clustering[a] %notin% input$algorithm) {
                        if (algorithm_clustering[a] %in% advclust_algorithm) {
                            countAlgorithmAdvclust <- countAlgorithmAdvclust + 1
                        }

                        if (algorithm_clustering[a] %in% amap_algorithm) {
                            countAlgorithmAmap <- countAlgorithmAmap + 1
                        }

                        if (algorithm_clustering[a] %in% apcluster_algorithm) {
                            countAlgorithmApcluster <- countAlgorithmApcluster + 1
                        }

                        if (algorithm_clustering[a] %in% cluster_algorithm) {
                            countAlgorithmCluster <- countAlgorithmCluster + 1
                        }

                        if (algorithm_clustering[a] %in% clusterr_algorithm) {
                            countAlgorithmClusterR <- countAlgorithmClusterR + 1
                        }

                        if (algorithm_clustering[a] %in% pvclcust_algorithm) {
                            countAlgorithmPvclust <- countAlgorithmPvclust + 1
                        }
                    }
                }

                if (countAlgorithmAdvclust == length(advclust_algorithm)) {
                    selectedPackage = selectedPackage[selectedPackage != 'advclust']
                }

                if (countAlgorithmAmap == length(amap_algorithm)) {
                    selectedPackage = selectedPackage[selectedPackage != 'amap']
                }

                if (countAlgorithmApcluster == length(apcluster_algorithm)) {
                    selectedPackage = selectedPackage[selectedPackage != 'apcluster']
                }

                if (countAlgorithmCluster == length(cluster_algorithm)) {
                    selectedPackage = selectedPackage[selectedPackage != 'cluster']
                }

                if (countAlgorithmClusterR == length(clusterr_algorithm)) {
                    selectedPackage = selectedPackage[selectedPackage != 'clusterr']
                }

                if (countAlgorithmPvclust == length(pvclcust_algorithm)) {
                    selectedPackage = selectedPackage[selectedPackage != 'pvclust']
                }

                for (a in 1:length(input$algorithm)) {
                    if (input$algorithm[a] %in% advclust_algorithm) {
                        if (!is.null(input$packages)) {
                            if ("advclust" %notin% input$packages) {
                                selectedPackage = c(selectedPackage, "advclust")
                            }
                        } else {
                            selectedPackage = c(selectedPackage, "advclust")
                        }
                    }

                    if (input$algorithm[a] %in% amap_algorithm) {
                        if (!is.null(input$packages)) {
                            if ("amap" %notin% input$packages) {
                                selectedPackage = c(selectedPackage, "amap")
                            }
                        } else {
                            selectedPackage = c(selectedPackage, "amap")
                        }
                    }

                    if (input$algorithm[a] %in% apcluster_algorithm) {
                        if (!is.null(input$packages)) {
                            if ("apcluster" %notin% input$packages) {
                                selectedPackage = c(selectedPackage, "apcluster")
                            }
                        } else {
                            selectedPackage = c(selectedPackage, "apcluster")
                        }
                    }

                    if (input$algorithm[a] %in% cluster_algorithm) {
                        if (!is.null(input$packages)) {
                            if ("cluster" %notin% input$packages) {
                                selectedPackage = c(selectedPackage, "cluster")
                            }
                        } else {
                            selectedPackage = c(selectedPackage, "cluster")
                        }
                    }

                    if (input$algorithm[a] %in% clusterr_algorithm) {
                        if (!is.null(input$packages)) {
                            if ("clusterr" %notin% input$packages) {
                                selectedPackage = c(selectedPackage, "clusterr")
                            }
                        } else {
                            selectedPackage = c(selectedPackage, "clusterr")
                        }
                    }

                    if (input$algorithm[a] %in% pvclcust_algorithm) {
                        if (!is.null(input$packages)) {
                            if ("pvclust" %notin% input$packages) {
                                selectedPackage = c(selectedPackage, "pvclust")
                            }
                        } else {
                            selectedPackage = c(selectedPackage, "pvclust")
                        }
                    }
                }

            } else {
                selectedPackage = ""
            }


            updatePickerInput(session = session,
                              inputId = "packages",
                              selected = selectedPackage)
        }
    )

    # Event that will launch the execution of the algorithm every time it detects a change in any component.
    observe({
        generate_information()
    })

    # Event that manages the package component. Each vex that selects a bundle will mark
    # the algorithms corresponding to the bundle.
    observeEvent(
        ignoreNULL = F,
        ignoreInit = T,
        eventExpr = {
            input$packages
        },
        handlerExpr = {
            if (length(input$packages) > 0) {
                if (is.null(input$algorithm)) {
                    selectedAlgorithm = c()
                } else {
                    selectedAlgorithm = c(input$algorithm)
                }

                advclust_algorithm <-
                    c("fuzzy_cm", "fuzzy_gg", "fuzzy_gk")
                amap_algorithm <- c("hclust")
                apcluster_algorithm <- c("apclusterK")
                clusterr_algorithm <-
                    c("agnes",
                      "clara",
                      "daisy",
                      "diana",
                      "fanny",
                      "mona",
                      "pam")
                cluster_algorithm <-
                    c("gmm",
                      "kmeans_arma",
                      "kmeans_rcpp",
                      "mini_kmeans")
                pvclcust_algorithm <- c("pvclust")

                '%notin%' <- Negate('%in%')
                packages_clustering <-
                    c(
                        "advclust",
                        "amap",
                        "apcluster",
                        "clusterr",
                        "cluster",
                        "pvclust"
                    )

                #' Limpiamos aquellos algoritmos que no está en el paquete indicado.
                for (p in 1:length(packages_clustering)) {
                    if (packages_clustering[p] %notin% input$packages) {
                        if (tolower(packages_clustering[p]) == tolower("advclust")) {
                            for (alg in 1:length(advclust_algorithm)) {
                                if (advclust_algorithm[alg] %in% input$algorithm) {
                                    selectedAlgorithm <-
                                        selectedAlgorithm[selectedAlgorithm != advclust_algorithm[alg]]
                                }
                            }
                        }
                        if (tolower(packages_clustering[p]) == tolower("amap")) {
                            for (alg in 1:length(amap_algorithm)) {
                                if (amap_algorithm[alg] %in% input$algorithm) {
                                    selectedAlgorithm <-
                                        selectedAlgorithm[selectedAlgorithm != amap_algorithm[alg]]
                                }
                            }
                        }
                        if (tolower(packages_clustering[p]) == tolower("apcluster")) {
                            for (alg in 1:length(apcluster_algorithm)) {
                                if (apcluster_algorithm[alg] %in% input$algorithm) {
                                    selectedAlgorithm <-
                                        selectedAlgorithm[selectedAlgorithm != apcluster_algorithm[alg]]
                                }
                            }
                        }
                        if (tolower(packages_clustering[p]) == tolower("cluster")) {
                            for (alg in 1:length(cluster_algorithm)) {
                                if (cluster_algorithm[alg] %in% input$algorithm) {
                                    selectedAlgorithm <-
                                        selectedAlgorithm[selectedAlgorithm != cluster_algorithm[alg]]
                                }
                            }
                        }
                        if (tolower(packages_clustering[p]) == tolower("clusterr")) {
                            for (alg in 1:length(clusterr_algorithm)) {
                                if (clusterr_algorithm[alg] %in% input$algorithm) {
                                    selectedAlgorithm <-
                                        selectedAlgorithm[selectedAlgorithm != clusterr_algorithm[alg]]
                                }
                            }
                        }
                        if (tolower(packages_clustering[p]) == tolower("pvclust")) {
                            for (alg in 1:length(pvclcust_algorithm)) {
                                if (pvclcust_algorithm[alg] %in% input$algorithm) {
                                    selectedAlgorithm <-
                                        selectedAlgorithm[selectedAlgorithm != pvclcust_algorithm[alg]]
                                }
                            }
                        }
                    }
                }

                for (n in 1:length(input$packages)) {
                    if (tolower(input$packages[n]) == tolower("advclust")) {
                        if (length(input$algorithm) > 0) {
                            for (alg in 1:length(advclust_algorithm)) {
                                if (advclust_algorithm[alg] %notin% input$algorithm) {
                                    selectedAlgorithm <- c(selectedAlgorithm,
                                                           advclust_algorithm[alg])
                                }
                            }
                        }  else {
                            selectedAlgorithm <- c(selectedAlgorithm,
                                                   advclust_algorithm)
                        }
                    }

                    if (tolower(input$packages[n]) == tolower("amap")) {
                        if (length(input$algorithm) > 0) {
                            for (alg in 1:length(amap_algorithm)) {
                                if (amap_algorithm[alg] %notin% input$algorithm) {
                                    selectedAlgorithm <- c(selectedAlgorithm,
                                                           amap_algorithm[alg])
                                }
                            }
                        } else {
                            selectedAlgorithm <- c(selectedAlgorithm,
                                                   amap_algorithm)
                        }
                    }

                    if (tolower(input$packages[n]) == tolower("apcluster")) {
                        if (length(input$algorithm) > 0) {
                            for (alg in 1:length(apcluster_algorithm)) {
                                if (apcluster_algorithm[alg] %notin% input$algorithm) {
                                    selectedAlgorithm <- c(selectedAlgorithm,
                                                           apcluster_algorithm[alg])
                                }
                            }
                        } else {
                            selectedAlgorithm <- c(selectedAlgorithm,
                                                   apcluster_algorithm)
                        }
                    }

                    if (tolower(input$packages[n]) == tolower("cluster")) {
                        if (length(input$algorithm) > 0) {
                            for (alg in 1:length(cluster_algorithm)) {
                                if (cluster_algorithm[alg] %notin% input$algorithm) {
                                    selectedAlgorithm <- c(selectedAlgorithm,
                                                           cluster_algorithm[alg])
                                }
                            }
                        } else {
                            selectedAlgorithm <- c(selectedAlgorithm,
                                                   cluster_algorithm)
                        }
                    }

                    if (tolower(input$packages[n]) == tolower("clusterr")) {
                        if (length(input$algorithm) > 0) {
                            for (alg in 1:length(clusterr_algorithm)) {
                                if (clusterr_algorithm[alg] %notin% input$algorithm) {
                                    selectedAlgorithm <- c(selectedAlgorithm,
                                                           clusterr_algorithm[alg])
                                }
                            }
                        } else {
                            selectedAlgorithm <- c(selectedAlgorithm,
                                                   clusterr_algorithm)
                        }
                    }

                    if (tolower(input$packages[n]) == tolower("pvclust")) {
                        if (length(input$algorithm) > 0) {
                            for (alg in 1:length(pvclcust_algorithm)) {
                                if (pvclcust_algorithm[alg] %notin% input$algorithm) {
                                    selectedAlgorithm <- c(selectedAlgorithm,
                                                           pvclcust_algorithm[alg])
                                }
                            }
                        } else {
                            selectedAlgorithm <- c(selectedAlgorithm,
                                                   pvclcust_algorithm)
                        }
                    }
                }
            } else {
                selectedAlgorithm = ""
            }
        }
    )

    # Renders the text containing the directory name
    renderText({
        input$typeExecution
    })

    # Event managed by the component that controls the management of directories
    observeEvent(
        ignoreNULL = T,
        eventExpr = {
            input$dir
        },
        handlerExpr = {
            if (!"path" %in% names(dir()))
                return()
            home <- normalizePath("~")
            global$datapath <-
                file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
        },
        ignoreInit = T
    )


    # Main function in charge of obtaining the values needed to launch the clustering algorithm
    generate_information <- function() {

        # Initialization of variables

        output$tableClustering <- NULL

        output$best_evaluation1 <- NULL

        output$best_evaluation2 <- NULL

        output$image1 <- NULL

        output$plotImage1 <- NULL

        output$image2 <- NULL

        output$plotImage2 <- NULL

        shinyjs::hide("image1")
        shinyjs::hide("image2")
        shinyjs::hide("plotImage1")
        shinyjs::hide("plotImage2")
        shinyjs::hide("best_evaluation2")

        # If the required fields are not marked, this triggers an exception

        if ((is.null(input$packages) ||
             input$packages == "") ||
            (is.null(input$algorithm) ||
             input$algorithm == "") ||
            (is.null(input$metrics) || input$metrics == "")) {
            shinyalert("The field packages,algorithm and metrics must be filled",
                       type = "error")
        } else {

            printFirstTable = F

            # We check if the user has marked that the data should be uploaded from a directory
            # or a dataframe

            if (input$typeExecution == "data") {


                data = NULL

                if (input$datasetTest == "basketball") {
                    data = Clustering::basketball

                } else if (input$datasetTest == "bolts") {
                    data = Clustering::bolts

                } else if (input$datasetTest == "stock") {
                    data = Clustering::stock

                } else if (input$datasetTest == "stulong") {
                    data = Clustering::stulong

                } else {
                    data = Clustering::weather

                }

                tryCatch({

                    # Execute clustering algorithm to dataset indicate

                    df_result <<-
                        Clustering::clustering(
                            df = data,
                            algorithm = input$algorithm,
                            min = input$clustering[1],
                            max = input$clustering[2],
                            metrics = input$metrics
                        )


                    if (df_result$has_external_metrics) {

                        result_external <-
                            Clustering::best_ranked_external_metrics(df_result)

                        output$best_evaluation1 <-
                            DT::renderDataTable(DT::datatable(result_external$result,
                                                              extensions = c('Buttons','ColReorder'),
                                                              options = list(
                                                                  colReorder = TRUE,
                                                                  scrollX = T,
                                                                  lengthChange = F,
                                                                  scroller = T,
                                                                  dom = 'Bfrtip',
                                                                  buttons =
                                                                      list(
                                                                          list(
                                                                              extend = 'copy',
                                                                              buttons = c('copy'),
                                                                              filename = 'External'
                                                                          ),
                                                                          list(
                                                                              extend = 'csv',
                                                                              buttons = c('csv'),
                                                                              filename = 'External'
                                                                          ),
                                                                          list(
                                                                              extend = 'pdf',
                                                                              buttons = c('pdf'),
                                                                              filename = 'External'
                                                                          ),
                                                                          list(
                                                                              extend = 'excel',
                                                                              buttons = c('excel'),
                                                                              filename = 'External'
                                                                          ))
                                                              )))

                        printFirstTable = T

                        shinyjs::show("plotImage1")
                        output$plotImage1 <- renderPlot({
                            shinyjs::show("image1")
                            result <-
                                fillComboExternalGraphs(input$metrics)
                            updateSelectInput(
                                session = session,
                                inputId = "image1",
                                choices = result,
                                selected = result[1]
                            )

                            Clustering::plot_clustering(df_result, result[1])

                        })
                    }

                    if (df_result$has_internal_metrics) {

                        result_internal <-
                            Clustering::best_ranked_internal_metrics(df_result)

                        if (printFirstTable) {

                            output$best_evaluation2 <-
                                DT::renderDataTable(DT::datatable(result_internal$result,
                                                                  extensions = c('Buttons','ColReorder'),
                                                                  options = list(
                                                                      colReorder = TRUE,
                                                                      scrollX = T,
                                                                      lengthChange = F,
                                                                      scroller = T,
                                                                      dom = 'Bfrtip',
                                                                      buttons =
                                                                          list(
                                                                              list(
                                                                                  extend = 'copy',
                                                                                  buttons = c('copy'),
                                                                                  filename = 'Internal'
                                                                              ),
                                                                              list(
                                                                                  extend = 'csv',
                                                                                  buttons = c('csv'),
                                                                                  filename = 'Internal'
                                                                              ),
                                                                              list(
                                                                                  extend = 'pdf',
                                                                                  buttons = c('pdf'),
                                                                                  filename = 'Internal'
                                                                              ),
                                                                              list(
                                                                                  extend = 'excel',
                                                                                  buttons = c('excel'),
                                                                                  filename = 'Internal'
                                                                              ))
                                                                  )))

                            shinyjs::show("best_evaluation2")

                        } else {

                            output$best_evaluation1 <-
                                DT::renderDataTable(
                                    result_internal$result,
                                    options = list(
                                        scroller = T,
                                        scrollX = T,
                                        lengthChange = F,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                    )
                                )
                        }

                        shinyjs::show("plotImage2")
                        output$plotImage2 <- renderPlot({
                            shinyjs::show("image2")
                            result <-
                                fillComboInternalGraphs(input$metrics)
                            updateSelectInput(
                                session = session,
                                inputId = "image2",
                                choices = result,
                                selected = result[1]
                            )

                            Clustering::plot_clustering(df_result, result[1])

                        })
                    }
                },

                # When you raise an exception, it initializes the table and hides the load component

                error = function(e) {


                    output$best_evaluation1 <-
                        DT::renderDataTable(NULL,
                                            options = list(
                                                scroller = T,
                                                scrollX = T,
                                                lengthChange = F,
                                                dom = 'Bfrtip',
                                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                            ))
                    output$best_evaluation2 <-
                        DT::renderDataTable(NULL,
                                            options = list(
                                                scroller = T,
                                                scrollX = T,
                                                lengthChange = F,
                                                dom = 'Bfrtip',
                                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                            ))
                    shinyjs::show("best_evaluation2")

                    messageError <- ""

                    if (e$message == "subíndice fuera de  los límites") {
                        messageError <- "out-of-bounds sub-index"
                    } else {
                        messageError <- e$message
                    }

                    shinyalert(messageError, type = "error")
                })

            } else {

                # Execute this part when the user select a directory with the files a execute

                tryCatch({
                    df_result <<-
                        Clustering::clustering(
                            path = global$datapath,
                            algorithm = input$algorithm,
                            min = input$clustering[1],
                            max = input$clustering[2],
                            metrics = input$metrics
                        )

                    columnnames <- colnames(df_result$result)

                    #Exclude ranking column

                    columnnames <-
                        columnnames[columnnames != "Var"]

                    # Verify if contain internal or external values.

                    columnnames[columnnames != c("Time","TimeAtt")]

                    result <-
                        dplyr::select(as.data.frame(df_result$result),
                                      columnnames)

                    # Render tables and plots.

                    output$tableClustering <-
                        DT::renderDataTable(result,
                                            options = list(
                                                scroller = T,
                                                scrollX = T,
                                                lengthChange = F,
                                                dom = 'Bfrtip',
                                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                            ))

                    printFirstTable = F

                    if (df_result$has_external_metrics) {
                        result_external <-
                            Clustering::evaluate_best_validation_external_by_metrics(df_result)

                        output$best_evaluation1 <-
                            DT::renderDataTable(
                                result_external$result,
                                options = list(
                                    scroller = T,
                                    scrollX = T,
                                    lengthChange = F,
                                    dom = 'Bfrtip',
                                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                )
                            )

                        printFirstTable = T

                        shinyjs::show("plotImage1")
                        output$plotImage1 <- renderPlot({
                            shinyjs::show("image1")
                            result <-
                                fillComboExternalGraphs(input$metrics)
                            updateSelectInput(
                                session = session,
                                inputId = "image1",
                                choices = result,
                                selected = result[1]
                            )

                            Clustering::plot_clustering(df_result, result[1])

                        })
                    }


                    if (df_result$has_internal_metrics) {
                        result_internal <-
                            Clustering::evaluate_best_validation_internal_by_metrics(df_result)

                        if (printFirstTable) {
                            output$best_evaluation2 <-
                                DT::renderDataTable(
                                    result_internal$result,
                                    options = list(
                                        scroller = T,
                                        scrollX = T,
                                        lengthChange = F,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                    )
                                )
                            shinyjs::show("best_evaluation2")
                        } else {
                            output$best_evaluation1 <-
                                DT::renderDataTable(
                                    result_internal$result,
                                    options = list(
                                        scroller = T,
                                        scrollX = T,
                                        lengthChange = F,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                    )
                                )
                        }

                        shinyjs::show("plotImage2")
                        output$plotImage2 <- renderPlot({
                            shinyjs::show("image2")
                            result <-
                                fillComboInternalGraphs(input$metrics)
                            updateSelectInput(
                                session = session,
                                inputId = "image2",
                                choices = result,
                                selected = result[1]
                            )

                            Clustering::plot_clustering(df_result, result[1])

                        })
                    }
                },

                error = function(e) {
                    output$tableClustering <- DT::renderDataTable(NULL,
                                                                  options = list(
                                                                      scroller = T,
                                                                      scrollX = T,
                                                                      lengthChange = F,
                                                                      dom = 'Bfrtip',
                                                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                                                  ))
                    output$best_evaluation1 <-
                        DT::renderDataTable(NULL,
                                            options = list(
                                                scroller = T,
                                                scrollX = T,
                                                lengthChange = F,
                                                dom = 'Bfrtip',
                                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                            ))
                    output$best_evaluation2 <-
                        DT::renderDataTable(NULL,
                                            options = list(
                                                scroller = T,
                                                scrollX = T,
                                                lengthChange = F,
                                                dom = 'Bfrtip',
                                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                            ))
                    shinyjs::show("best_evaluation2")

                    messageError <- ""

                    if (e$message == "subíndice fuera de  los límites") {
                        messageError <- "out-of-bounds sub-index"
                    } else {
                        messageError <- e$message
                    }

                    shinyalert(messageError, type = "error")
                })
            }
        }
    }
})
