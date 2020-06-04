#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(dplyr)
library(shinyalert)
library(ggplot2)

fillComboExternalGraphs <- function(metrics) {
    '%not in%' <- Negate('%in%')

    result <- metrics

    if (length(metrics) > 0) {
        for (m in 1:length(metrics)) {
            if (metrics[m] %not in% c(
                "entropy",
                "precision",
                "recall",
                "variation_information",
                "f_measure",
                "fowlkes_mallows_index"
            )) {
                result <- result[result != metrics[m]]
            }
        }
    }

    return (result)
}

fillComboInternalGraphs <- function(metrics) {
    '%not in%' <- Negate('%in%')

    result <- metrics

    if (length(metrics) > 0) {
        for (m in 1:length(metrics)) {
            if (metrics[m] %not in% c("connectivity",
                                      "dunn",
                                      "silhouette")) {
                result <- result[result != metrics[m]]
            }
        }
    }

    return (result)
}

df_result <- NULL

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    df_result <<- NULL

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


    observeEvent(input$image1, {
        if (!is.null(input$image1) &&
            input$image1 != "" && !is.null(df_result)) {
            output$plotImage1 <- renderPlot({
                plot(df_result, input$image1)
            })
        }
    })

    observeEvent(input$image2, {
        if (!is.null(input$image2) &&
            input$image2 != "" && !is.null(df_result)) {
            output$plotImage2 <- renderPlot({
                plot(df_result, input$image2)
            })
        }
    })

    observeEvent(input$typeExecution, {
        if (input$typeExecution == "data") {
            shinyjs::disable("dir")
            shinyjs::enable("datasetTest")
        } else {
            shinyjs::disable("datasetTest")
            shinyjs::enable("dir")
        }
    })

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
                        "gama",
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
                gama_algorithm <- c("gama")
                pvclcust_algorithm <- c("pvclust")

                '%notin%' <- Negate('%in%')

                countAlgorithmAdvclust <- 0
                countAlgorithmAmap <- 0
                countAlgorithmApcluster <- 0
                countAlgorithmCluster <- 0
                countAlgorithmClusterR <- 0
                countAlgorithmGama <- 0
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

                        if (algorithm_clustering[a] %in% gama_algorithm) {
                            countAlgorithmGama <- countAlgorithmGama + 1
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

                if (countAlgorithmGama == length(gama_algorithm)) {
                    selectedPackage = selectedPackage[selectedPackage != 'gama']
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

                    if (input$algorithm[a] %in% gama_algorithm) {
                        if (!is.null(input$packages)) {
                            if ("gama" %notin% input$packages) {
                                selectedPackage = c(selectedPackage, "gama")
                            }
                        } else {
                            selectedPackage = c(selectedPackage, "gama")
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

    observe({
        generate_information()
    })

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
                gama_algorithm <- c("gama")
                pvclcust_algorithm <- c("pvclust")

                '%notin%' <- Negate('%in%')
                packages_clustering <-
                    c(
                        "advclust",
                        "amap",
                        "apcluster",
                        "clusterr",
                        "cluster",
                        "gama",
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
                        if (tolower(packages_clustering[p]) == tolower("gama")) {
                            for (alg in 1:length(gama_algorithm)) {
                                if (gama_algorithm[alg] %in% input$algorithm) {
                                    selectedAlgorithm <-
                                        selectedAlgorithm[selectedAlgorithm != gama_algorithm[alg]]
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

                    if (tolower(input$packages[n]) == tolower("gama")) {
                        if (length(input$algorithm) > 0) {
                            for (alg in 1:length(gama_algorithm)) {
                                if (gama_algorithm[alg] %notin% input$algorithm) {
                                    selectedAlgorithm <- c(selectedAlgorithm,
                                                           gama_algorithm[alg])
                                }
                            }
                        } else {
                            selectedAlgorithm <- c(selectedAlgorithm,
                                                   gama_algorithm)
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

    renderText({
        input$typeExecution
    })

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



    generate_information <- function() {

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

        if ((is.null(input$packages) ||
             input$packages == "") ||
            (is.null(input$algorithm) ||
             input$algorithm == "") ||
            (is.null(input$metrics) || input$metrics == "")) {
            shinyalert("The field packages,algorithm and metrics must be filled",
                       type = "error")
        } else {

            visible = F

            printFirstTable = F

            if (input$visible == "TRUE")
                visible = T


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
                    df_result <<-
                        Clustering::clustering(
                            df = data,
                            algorithm = input$algorithm,
                            min = input$clustering[1],
                            max = input$clustering[2],
                            metrics = input$metrics,
                            variables = visible
                        )

                    columnnames <- colnames(df_result$result)

                    columnnames <-
                        columnnames[columnnames != "Ranking"]

                    if (isFALSE(df_result$hasInternalMetrics)) {
                        columnnames <- columnnames[columnnames != "timeInternal"]
                    }

                    if (isFALSE(df_result$hasExternalMetrics)) {
                        columnnames <- columnnames[columnnames != "timeExternal"]
                    }

                    result <-
                        dplyr::filter(as.data.frame(df_result$result),
                                      Ranking == 1) %>% select(columnnames)

                    output$tableClustering <-
                        DT::renderDataTable(result,
                                            options = list(
                                                scrollX = T,
                                                lengthChange = F,
                                                scroller = T
                                            ))


                    if (df_result$hasExternalMetrics) {
                        result_external <-
                            Clustering::evaluate_best_validation_external_by_metrics(df_result)

                        output$best_evaluation1 <-
                            DT::renderDataTable(
                                result_external$result,
                                options = list(
                                    scroller = T,
                                    scrollX = T,
                                    lengthChange = F
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
                            plot(df_result, result[1])

                        })
                    }

                    if (df_result$hasInternalMetrics) {
                        result_internal <-
                            Clustering::evaluate_best_validation_internal_by_metrics(df_result)

                        if (printFirstTable) {
                            output$best_evaluation2 <-
                                DT::renderDataTable(
                                    result_internal$result,
                                    options = list(
                                        scroller = T,
                                        scrollX = T,
                                        lengthChange = F
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
                                        lengthChange = F
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
                            plot(df_result, result[1])

                        })
                    }
                },

                error = function(e) {
                    output$tableClustering <- DT::renderDataTable(NULL,
                                                                  options = list(
                                                                      scroller = T,
                                                                      scrollX = T,
                                                                      lengthChange = F
                                                                  ))

                    output$best_evaluation1 <-
                        DT::renderDataTable(NULL,
                                            options = list(
                                                scroller = T,
                                                scrollX = T,
                                                lengthChange = F
                                            ))
                    output$best_evaluation2 <-
                        DT::renderDataTable(NULL,
                                            options = list(
                                                scroller = T,
                                                scrollX = T,
                                                lengthChange = F
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
                tryCatch({
                    df_result <<-
                        Clustering::clustering(
                            path = global$datapath,
                            algorithm = input$algorithm,
                            min = input$clustering[1],
                            max = input$clustering[2],
                            metrics = input$metrics,
                            variables = visible
                        )

                    columnnames <- colnames(df_result$result)

                    columnnames <-
                        columnnames[columnnames != "Ranking"]

                    if (isFALSE(df_result$hasInternalMetrics)) {
                        columnnames <- columnnames[columnnames != "timeInternal"]
                    }

                    if (isFALSE(df_result$hasExternalMetrics)) {
                        columnnames <- columnnames[columnnames != "timeExternal"]
                    }

                    result <-
                        dplyr::select(as.data.frame(df_result$result),
                                      columnnames)

                    output$tableClustering <-
                        DT::renderDataTable(result,
                                            options = list(
                                                scroller = T,
                                                scrollX = T,
                                                lengthChange = F
                                            ))

                    printFirstTable = F

                    if (df_result$hasExternalMetrics) {
                        result_external <-
                            Clustering::evaluate_best_validation_external_by_metrics(df_result)

                        output$best_evaluation1 <-
                            DT::renderDataTable(
                                result_external$result,
                                options = list(
                                    scroller = T,
                                    scrollX = T,
                                    lengthChange = F
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
                            plot(df_result, result[1])

                        })
                    }


                    if (df_result$hasInternalMetrics) {
                        result_internal <-
                            Clustering::evaluate_best_validation_internal_by_metrics(df_result)

                        if (printFirstTable) {
                            output$best_evaluation2 <-
                                DT::renderDataTable(
                                    result_internal$result,
                                    options = list(
                                        scroller = T,
                                        scrollX = T,
                                        lengthChange = F
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
                                        lengthChange = F
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
                            plot(df_result, result[1])

                        })
                    }
                },

                error = function(e) {
                    output$tableClustering <- DT::renderDataTable(NULL,
                                                                  options = list(
                                                                      scroller = T,
                                                                      scrollX = T,
                                                                      lengthChange = F
                                                                  ))
                    output$best_evaluation1 <-
                        DT::renderDataTable(NULL,
                                            options = list(
                                                scroller = T,
                                                scrollX = T,
                                                lengthChange = F
                                            ))
                    output$best_evaluation2 <-
                        DT::renderDataTable(NULL,
                                            options = list(
                                                scroller = T,
                                                scrollX = T,
                                                lengthChange = F
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
