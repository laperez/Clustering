#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyFiles)
library(shinyWidgets)
library(shinyjs)
library(waiter)
library(DT)
library(shinyalert)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        shinyjs::useShinyjs(),
        theme = shinythemes::shinytheme("superhero"),
        use_waiter(),
        useShinyalert(),

        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$script(src = "myjs.js"),


        fluidRow(
            column(
                3,
                radioButtons(
                    "typeExecution",
                    label = h3(" Do you want to use test data or a file directory ? "),
                    choices = list("File Directory" = "directory", "Test Data" = "data"),
                    inline = TRUE,
                    selected = "data"
                )
            ),
            column(
                3,
                h3("Directory Datasets"),
                shinyDirButton(id = "dir", label = "Directory Datasets", title = 'Select a directory')
            ),
            column(4, h3("Path"), verbatimTextOutput("dir", placeholder = TRUE)),
            column(
                2,
                selectInput(
                    inputId = "datasetTest",
                    label = h3("Dataset Test"),
                    choices = c(
                        "Basketball" = "basketball",
                        "Bolts" = "bolts",
                        "Stock" = "stock",
                        "Stulong" = "stulong",
                        "Weather" = "weather"
                    ),
                    multiple = FALSE,
                    selected = "basketball"
                )

            )

        ),

        # Sidebar with a slider input for number of bins
        fluidRow(
            column(
                2,
                pickerInput(
                    inputId = "packages",
                    label = h3("Packages"),
                    choices = list(
                        "Advclust" = "advclust",
                        "Amap" = "amap",
                        "Apcluster" = "apcluster",
                        "ClusterR" = "clusterr",
                        "Cluster" = "cluster",
                        "Gama" = "gama",
                        "Pvclust" = "pvclust"
                    ),
                    options = list(
                        `actions-box` = TRUE,
                        size = 5,
                        `selected-text-format` = "count > 3"
                    ),
                    multiple = TRUE,
                    selected = "cluster"
                )
            ),
            column(
                2,
                pickerInput(
                    inputId = "algorithm",
                    label = h3("Algorithms"),
                    choices = list(
                        "ApclusterK" = "apclusterK",
                        "Agnes" = "agnes",
                        "Clara" = "clara",
                        "Daisy" = "daisy",
                        "Diana" = "diana",
                        "Fanny" = "fanny",
                        "Gama" = "gama",
                        "Gmm" = "gmm",
                        "Fuzzy CM" = "fuzzy_cm",
                        "Fuzzy GG" = "fuzzy_gg",
                        "Fuzzy GK" = "fuzzy_gk",
                        "Hclust" = "hclust",
                        "Kmeans Arma" = "kmeans_arma",
                        "Kmeans Rcpp" = "kmeans_rcpp",
                        "Mini Kmeans" = "mini_kmeans",
                        "Mona" = "mona",
                        "Pam" = "pam",
                        "Pvclust" = "pvclust"
                    ),
                    options = list(
                        `actions-box` = TRUE,
                        size = 5,
                        `selected-text-format` = "count > 3"
                    ),
                    selected = c("kmeans_arma", "kmeans_rcpp", "mini_kmeans", "gmm"),
                    multiple = TRUE
                )

            ),
            column(
                3,
                sliderInput(
                    "clustering",
                    label = h3("Number of Clustering"),
                    min = 1,
                    max = 10,
                    value = c(3, 4)
                )
            ),
            column(
                2,
                pickerInput(
                    "metrics",
                    label = h3("Metrics"),
                    choices = list(
                        "Connectivity" = "connectivity",
                        "Dunn" = "dunn",
                        "Entropy" = "entropy",
                        "Fowlkes Mallows Index" = "fowlkes_mallows_index",
                        "F-measure" = "f_measure",
                        "Precision" = "precision",
                        "Recall" = "recall",
                        "Silhouette" = "silhouette",
                        "Variation Information" = "variation_information"
                    ),
                    options = list(
                        `actions-box` = TRUE,
                        size = 5,
                        `selected-text-format` = "count > 3"
                    ),
                    multiple = TRUE,
                    selected = "precision"
                )
            ),
            column(
                3,
                radioButtons(
                    "visible",
                    label = h3("Do you want to show the variable?"),
                    choices = list("Yes" = TRUE, "No" = FALSE),
                    inline = TRUE
                )
            ),
        ),

        fluidRow(column(10),
                 column(
                     1, actionButton("generate", "Generate Information")
                 )),
        tags$br(),
        fluidRow(
            column(4,
                   DT::dataTableOutput("tableClustering")),
            column(4, DT::dataTableOutput("best_evaluation1")),
            column(4, DT::dataTableOutput("best_evaluation2"))
        ),
        tags$br(),
        fluidRow(column(
            2,
            hidden(
            selectInput(
                "image1",
                h3("Metrics External"),
                choices = c(),
                multiple = FALSE
            ))
        ),
        column(4, plotOutput("plotImage1")),
        column(
            2,
            hidden(
                selectInput(
                    "image2",
                    h3("Metrics Internal"),
                    choices = c(),
                    multiple = FALSE
                ))
        ),
        column(4, plotOutput("plotImage2"))


        )
    )





)
