# library load

library(shiny)
library(shinythemes)
library(shinyFiles)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(shinyalert)
library(shinycssloaders)

# Define UI for application which allows us to run the clustering algorithm without having to do it from the console
shinyUI(
    fluidPage(

        #load javascript y themes
        shinyjs::useShinyjs(),
        theme = shinythemes::shinytheme("superhero"),
        useShinyalert(),

        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$script(src = "myjs.js"),

        #layout left
        sidebarLayout(
            sidebarPanel(
                radioButtons(
                    "typeExecution",
                    label = h3(" Do you want to use test data or a file directory ? "),
                    choices = list("File Directory" = "directory", "Test Data" = "data"),
                    inline = T,
                    selected = "data"
                ),
                shinyDirButton(id = "dir", label = "Directory Datasets", title = 'Select a directory'),
                tags$br(),
                tags$br(),
                verbatimTextOutput("dir", placeholder = T),
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
                    multiple = F,
                    selected = "basketball"
                ),
                pickerInput(
                    inputId = "packages",
                    label = h3("Packages"),
                    choices = list(
                        "Advclust" = "advclust",
                        "Amap" = "amap",
                        "Apcluster" = "apcluster",
                        "ClusterR" = "clusterr",
                        "Cluster" = "cluster",
                        "Pvclust" = "pvclust"
                    ),
                    options = list(
                        `actions-box` = T,
                        size = 5,
                        `selected-text-format` = "count > 3"
                    ),
                    multiple = T,
                    selected = "cluster"
                ),
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
                        `actions-box` = T,
                        size = 5,
                        `selected-text-format` = "count > 3"
                    ),
                    selected = c("kmeans_arma", "kmeans_rcpp", "mini_kmeans", "gmm"),
                    multiple = T
                ),
                sliderInput(
                    "clustering",
                    label = h3("Number of Clustering"),
                    min = 1,
                    max = 10,
                    value = c(3, 4)
                ),
                pickerInput(
                    "metrics",
                    label = h3("Metrics"),
                    choices = list(
                        "Connectivity" = "Connectivity",
                        "Dunn" = "Dunn",
                        "Entropy" = "Entropy",
                        "Fowlkes Mallows Index" = "Fowlkes_mallows_index",
                        "F-measure" = "F_measure",
                        "Precision" = "Precision",
                        "Recall" = "Recall",
                        "Silhouette" = "Silhouette",
                        "Variation Information" = "Variation_information"
                    ),
                    options = list(
                        `actions-box` = T,
                        size = 5,
                        `selected-text-format` = "count > 3"
                    ),
                    choicesOpt = list(
                        style = "height: 15px;"
                    ),
                    multiple = T,
                    selected = "Precision"
                )

            ),

            #layout middle
            mainPanel(
                tabsetPanel(
                    tabPanel("Summary",
                             tags$br(),
                             withSpinner(DT::dataTableOutput("tableClustering"),color = "#4e5d6c"),
                             tags$br(),
                             withSpinner(DT::dataTableOutput("best_evaluation1"),color = "#4e5d6c"),
                             tags$br(),
                             DT::dataTableOutput("best_evaluation2")),

                    tabPanel("Plot",hidden(
                        selectInput(
                            "image1",
                            h3("Metrics External", class="h3-clustering"),
                            choices = c(),
                            multiple = F
                        )),
                        withSpinner(plotOutput("plotImage1"),color = "#4e5d6c"),
                        hidden(
                            selectInput(
                                "image2",
                                h3("Metrics Internal",class="h3-clustering"),
                                choices = c(),
                                multiple = F
                            )),
                        plotOutput("plotImage2")
                    )
                )
            )
        )
    )
)
