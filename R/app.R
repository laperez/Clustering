#'
#' Method running the web interface of the clustering package
#'
#' @importFrom
#' shiny runApp dataTableOutput renderDataTable runExample
#'
#' @export
#' appClustering
#'
appClustering <- function() {


  appDir <- system.file("shiny", "clustering", package = "Clustering")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `clustering`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


#'
#' Execute a list of datasets from a route or a dataframe
#'
#' @param path  path where the datasets are located.
#' @param df data matrix or data frame, or dissimilarity matrix, depending on the value of the argument.
#' @param packages array defining the clustering package. The seven packages implemented are: cluster, ClusterR, advclust, amap, apcluster,
#'        gama, pvclust. \cr By default runs all packages.
#' @param algorithm array with the algorithms that implement the package. The algorithms implemented are: fuzzy_cm,fuzzy_gg,fuzzy_gk,
#'        hclust,apclusterK,agnes,clara,daisy, \cr diana,fanny,mona,pam,gmm,kmeans_arma,kmeans_rcpp,mini_kmeans,gama,\cr pvclust.
#' @param min minimum number of clusters. at least one must be.
#' @param max maximum number of clusters. cluster_max must be greater or equal cluster_min.
#' @param metrics array defining the metrics avalaible in the package. The night metrics implemented are: entropy, variation_information,\cr
#' precision,recall,f_measure,fowlkes_mallows_index,connectivity,dunn,silhouette.
#' @param variables accepts Boolean values. If true as a result it shows the variable that behaves best otherwise it shows the value of the executed metric.
#'
#' @return returns a matrix with the result of running all the metrics of the algorithms contained in the packages we indicated.
#'
#' @import
#' glue
#'
#' @importFrom
#' apcluster aggExCluster cutree apclusterK negDistMat
#'
#' @importFrom
#' cluster agnes clara daisy diana fanny silhouette mona pam
#'
#' @importFrom
#' ClusterR GMM predict_GMM distance_matrix KMeans_rcpp predict_KMeans KMeans_arma MiniBatchKmeans predict_MBatchKMeans center_scale
#'
#' @importFrom
#' advclust fuzzy.CM fuzzy.GG fuzzy.GK
#'
#' @importFrom
#' pvclust pvclust pvpick
#'
#' @importFrom
#' gama gama
#'
#' @importFrom
#' amap hcluster
#'
#' @importFrom
#' stats dist hclust filter lag
#'
#' @importFrom
#' pracma strcmp
#'
#' @importFrom
#' data.table data.table
#'
#' @importFrom
#' tools file_path_sans_ext file_ext
#'
#' @importFrom
#' gmp asNumeric chooseZ as.bigz
#'
#' @importFrom
#' utils read.csv data
#'
#' @importFrom
#' xtable print.xtable xtable
#'
#' @importFrom
#' sqldf sqldf
#'
#' @export
#' clustering
#'
#' @examples
#'
#' clustering(df = cluster::agriculture, min = 4, max = 5, algorith='gmm', variables = TRUE)
#'
clustering <- function(path = CONST_NULL,
                               df = CONST_NULL,
                               packages = CONST_NULL,
                               algorithm = CONST_NULL,
                               min = CONST_NULL,
                               max = CONST_NULL,
                               metrics = CONST_NULL,
                               variables = CONST_NULL) {
  if (is.null(path) && is.null(df)) {
    stop("You must fill in at least one of the fields: path or df")
  }

  if (!is.null(path) && !dir.exists(path)) {
    stop("The path field must be a directory")
  }


  if (!is.null(df) && !is.data.frame(df)) {
    stop("The field df must be a data frame")
  }

  if (!is.null(packages) && !is.null(algorithm)) {
    stop("You must fill in the algorithm or packages field")
  }

  if (!is.null(packages) && !is.vector(packages)) {
    stop("The field package must be a array")
  }

  if (!is.null(algorithm) && !is.vector(algorithm)) {
    stop("The field package must be a array")
  }

  if (!is.null(packages) && length(packages) > CONST_ZERO) {
    for (i in 1:length(packages)) {
      if (!tolower(packages[i]) %in% packages())
        stop("The packages indicated is not among the packages used by the")
    }
  }

  if (!is.null(algorithm) && length(algorithm) > CONST_ZERO) {
    for (i in 1:length(algorithm)) {
      if (!tolower(algorithm[i]) %in% tolower(algorithms()))
        stop("The indicated algorithm is not implemented in the package")
    }
  }

  if (is.null(min)) {
    stop("It is necessary to indicate the minimum number of clusters")
  }

  if (is.null(max)) {
    stop("It is necessary to indicate the maximum number of clusters")
  }

  if (min < CONST_ONE) {
    stop("The cluster_min number must be equal to or greater than 1")
  }

  if (min > max) {
    stop("The minimum cluster number must be less than the maximum cluster number")
  }

  if (!is.null(metrics) && !is.vector(metrics)) {
    stop("The field metrics must be a array")
  }

  if (!is.null(metrics) && length(metrics) > CONST_ZERO) {
    for (i in 1:length(metrics)) {
      if (!tolower(metrics[i]) %in% metrics())
        stop("The metric indicated is not among the metrics used by the")
    }
  }

  execute_datasets(path,
                   df,
                   packages,
                   algorithm,
                   min,
                   max,
                   metrics,
                   variables)
}



#' Method of performing information processing
#'
#' @param path  path where the datasets are located.
#' @param df data matrix or data frame, or dissimilarity matrix, depending on the value of the argument.
#' @param packages array defining the clustering package. The seven packages implemented are: cluster, ClusterR, advclust, amap, apcluster, gama, pvclust. By default runs all packages.
#' @param algorithm array with the algorithms that implement the package. The algorithms implemented are: fuzzy_cm,fuzzy_gg,fuzzy_gk,hclust,apclusterK,agnes,clara,daisy,diana,fanny,mona,pam,gmm,kmeans_arma,kmeans_rcpp,mini_kmeans,gama,pvclust.
#' @param cluster_min minimum number of clusters. at least one must be.
#' @param cluster_max maximum number of clusters. cluster_max must be greater or equal cluster_min.
#' @param metrics array defining the metrics avalaible in the package. The night metrics implemented are: entropy, variation_information,precision,recall,f_measure,fowlkes_mallows_index,connectivity,dunn,silhouette.
#' @param variables accepts Boolean values. If true as a result it shows the variable that behaves best otherwise it shows the value of the executed metric.
#'
#' @return returns a matrix with the result of running all the metrics of the algorithms contained in the packages we indicated. It also provides two files called internal and external in latex format with the results of the execution.
#'
#' @keywords internal
#'
execute_datasets <- function (path,
                              df = CONST_NULL,
                              packages = CONST_NULL,
                              algorithm = CONST_NULL,
                              cluster_min = CONST_NULL,
                              cluster_max = CONST_NULL,
                              metrics = CONST_NULL,
                              variables = CONST_NULL) {

  on.exit(options(scipen = 999))

  formals(print.data.frame)$row.names <- FALSE

  if (is.null(algorithm)) {
    algorithms_execute <- algorithms_package(packages)

    measures_execute = measure_package(packages)
  } else {

    algorithms_execute <- algorithm

    measures_execute = measure_calculate(algorithm)
  }

  metrics_execute = metrics_calculate(metrics)

  is_metric_internal <- is_Internal_Metrics(metrics)

  is_metric_external <- is_External_Metrics(metrics)

  tableInternal <- CONST_INITIALIZER_STRING

  tableExternal <- CONST_INITIALIZER_STRING

  numberDataSets <- CONST_ZERO

  numberColumns <-
    length(metrics_execute) + CONST_COLUMN_DEFAULT_TABLE

  numberClusters <- (cluster_max - cluster_min) + CONST_ONE

  ficheros <- CONST_NULL

  if (!is.null(path)) {
    ficheros <- path_dataset(path)
  }

  if (!is.null(ficheros) &&
      length(ficheros) > CONST_ZERO) {
    numberDataSets <-
      numberDataSets + number_variables_dataset(path)
  }

  if (!is.null(df)) {
    numberDataSets <-
      numberDataSets + ncol(df)
  }

  number_algorithms <- CONST_ONE

  if (!is.null(algorithms_execute)) {
    number_algorithms <- number_algorithms + length(algorithms_execute)
  }

  df_result <-
    matrix(
      nrow = number_algorithms * length(measures_execute) * numberClusters * numberDataSets,
      ncol = numberColumns
    )

  colnames(df_result) <-
    c(
      CONST_ALGORITHM,
      CONST_DISTANCE,
      CONST_CLUSTERS,
      CONST_DATASET,
      CONST_RANKING,
      metrics_execute
    )

  df_external <- CONST_NULL
  df_internal <- CONST_NULL

  if (is_metric_external) {
    df_external <- data.frame(
      matrix(
        nrow = number_algorithms * length(measures_execute) * numberClusters * numberDataSets,
        ncol = number_columnas_external(metrics_execute) + CONST_EXTERNAL_METRICS_DEFAULT
      ),
      row.names = CONST_NULL
    )

    colnames(df_external) <-
      row_name_df_external(metrics_execute)

  }

  if (is_metric_internal) {
    df_internal <- data.frame(
      matrix(
        nrow = number_algorithms * length(measures_execute) * numberClusters * numberDataSets,
        ncol = number_columnas_internal(metrics_execute) + CONST_INTERNAL_METRICS_DEFAULT
      ),
      row.names = CONST_NULL
    )

    colnames(df_internal) <-
      row_name_df_internal(metrics_execute)
  }

  rowCount = CONST_ONE

  rowCountLatex  = CONST_ONE

  if (!is.null(ficheros) || !is.null(df)) {
    changeAlgorithm = CONST_ZERO

    for (i in 1:length(algorithms_execute)) {

      changeAlgorithm = CONST_ONE

      name_package = gsub("\\_", "\\\\_", algorithms_execute[i])

      countMeasureAlgorithm = CONST_ZERO

      for (n in 1:length(measures_execute)) {
        name_measure = substr(measures_execute[n],
                              CONST_ZERO,
                              nchar(algorithms_execute[i]))
        if (strcmp(name_measure, algorithms_execute[i])) {
          countMeasureAlgorithm = countMeasureAlgorithm + CONST_ONE
        }
      }

      changeMeasure = CONST_ZERO
      nameMeasureUsing = ''
      for (j in 1:length(measures_execute)) {
        name_measure = substr(measures_execute[j],
                              CONST_ZERO,
                              nchar(algorithms_execute[i]))
        name_measure_latex = gsub("\\_", "\\\\_", measures_execute[j])
        countRow = CONST_ZERO
        if (strcmp(name_measure, algorithms_execute[i])) {
          if (strcmp(measures_execute[j], nameMeasureUsing) == CONST_FALSE) {
            nameMeasureUsing = measures_execute[j]
            changeMeasure = CONST_ONE
          }

          countDifferentCluster = cluster_max - cluster_min

          countDifferentCluster = countDifferentCluster + CONST_ONE

          changeCluster = CONST_ZERO

          for (k in cluster_min:cluster_max) {
            changeCluster = CONST_ONE

            number_files = ifelse(!is.null(df),
                                  CONST_ONE,
                                  length(ficheros))

            for (m in 1:number_files) {
              entropy = CONST_ZERO_DOUBLE
              variation_information = CONST_ZERO_DOUBLE
              precision = CONST_ZERO_DOUBLE
              recall = CONST_ZERO_DOUBLE
              f_measure = CONST_ZERO_DOUBLE
              fowlkes_mallows_index = CONST_ZERO_DOUBLE
              timeExternal = CONST_ZERO_DOUBLE
              timeInternal = CONST_ZERO_DOUBLE
              dunn = CONST_ZERO_DOUBLE
              connectivity = CONST_ZERO_DOUBLE
              silhouette = CONST_ZERO_DOUBLE

              if (!is.null(df)) {
                resultado = evaluate_all_column_dataset(
                  as.matrix(df),
                  measures_execute[j],
                  k,
                  CONST_TIME_DF,
                  metrics_execute
                )

              } else {


                resultado = evaluate_all_column_dataset(
                  read_file(ficheros[m]),
                  measures_execute[j],
                  k,
                  ficheros[m],
                  metrics_execute
                )
              }

              name_file = ifelse(!is.null(df),
                                 CONST_TIME_DF,
                                 ficheros[m])

              entropy = resultado$external$entropy
              variation_information = resultado$external$variation_information
              precision = resultado$external$precision
              recall = resultado$external$recall
              f_measure = resultado$external$f_measure
              fowlkes_mallows_index = resultado$external$fowlkes_mallows_index
              timeExternal = resultado$external$time
              timeInternal = resultado$internal$time
              dunn = resultado$internal$dunn
              connectivity = resultado$internal$connectivity
              silhouette = resultado$internal$silhouette

              for (c in 1:length(entropy)) {
                info =     calculate_result(
                  name_measure,
                  measures_execute[j],
                  k,
                  name_file,
                  c,
                  timeExternal,
                  if (CONST_ENTROPY_METRIC %in% metrics_execute)
                    entropy
                  else
                    CONST_NULL,
                  if (CONST_VARIATION_INFORMATION_METRIC %in% metrics_execute)
                    variation_information
                  else
                    CONST_NULL,
                  if (CONST_PRECISION_METRIC %in% metrics_execute)
                    precision
                  else
                    CONST_NULL,
                  if (CONST_RECALL_METRIC %in% metrics_execute)
                    recall
                  else
                    CONST_NULL,
                  if (CONST_FOWLKES_MALLOWS_INDEX_METRIC %in% metrics_execute)
                    fowlkes_mallows_index
                  else
                    CONST_NULL,
                  if (CONST_F_MEASURE_METRIC %in% metrics_execute)
                    f_measure
                  else
                    CONST_NULL,
                  if (CONST_DUNN_METRIC %in% metrics_execute)
                    dunn
                  else
                    CONST_NULL,
                  if (CONST_CONNECTIVITY_METRIC %in% metrics_execute)
                    connectivity
                  else
                    CONST_NULL,
                  if (CONST_SILHOUETTE_METRIC %in% metrics_execute)
                    silhouette
                  else
                    CONST_NULL,
                  if (CONST_TIME_INTERNAL %in% metrics_execute)
                    timeInternal
                  else
                    CONST_NULL,
                  variables
                )

                result_info <- (as.vector(unlist(info)))

                for (pos in 6:length(info)) {
                  option_format <- options(digits=4)
                  on.exit(options(option_format))
                  result_info[pos] = format(round(x = as.numeric(result_info[pos]),
                                           digits = 4),scientific = FALSE)
                }

                df_result[rowCount, ] = result_info

                if (is_metric_external) {
                  df_external[rowCountLatex, ] = info_external(
                    metrics_execute,
                    info,
                    CONST_EXTERNAL_METRICS_DEFAULT + number_columnas_external(metrics_execute),
                    variables
                  )
                }

                if (is_metric_internal) {
                  df_internal[rowCountLatex, ] = info_internal(
                    metrics_execute,
                    info,
                    CONST_INTERNAL_METRICS_DEFAULT + number_columnas_internal(metrics_execute),
                    variables
                  )
                }

                rowCount = rowCount + CONST_ONE

                rowCountLatex  = rowCountLatex + CONST_ONE
              }
            }
          }
        }
      }
    }
  }

  rowCountLatex = rowCountLatex - CONST_ONE

  if (is_metric_external) {
    tableExternal <-
      xtable(xtable(df_external[1:rowCountLatex,]), include.rownames = FALSE)
  }

  if (is_metric_internal) {
    tableInternal <-
      xtable(xtable(df_internal[1:rowCountLatex,]), include.rownames = FALSE)
  }

  rowCount  = rowCount - CONST_ONE

  r <- list(result = df_result[1:rowCount, ], hasInternalMetrics = is_metric_internal, hasExternalMetrics = is_metric_external,
            algorithms_execute = algorithms_execute, measures_execute = measures_execute, tableExternal = tableExternal, tableInternal = tableInternal)
  r
}

#' Method that shows on screen the result of the clustring execution
#'
#' @param x list containing the result of the clustering run
#' @param ... other params
#'
#' @keywords internal
#'

print.clustering <- function(x, ...)
{
  cat("Result:	\n");		print(x$result, ...)
  cat("Has Internal Metrics: \t ", print(x$hasInternalMetrics, ...),"\n")
  cat("Has External Metrics: \t ", print(x$is_metric_external, ...),"\n")
  cat("Algorithms Execute: \t ", print(x$algorithms_execute, ...),"\n")
  cat("Measures Execute: \t ", print(x$measures_execute, ...),"\n")
  cat("External Table:\n");	print(x$tableExternal, ...)
  cat("\nInternal Table:\n");	print(x$tableInternal, ...)
  invisible(x)
}

#' Method that calculates the best rated external metrics
#'
#' @param df  data matrix or data frame
#'
#' @return returns a table with the external metrics that has the best rating
#'
#' @export
#' best_ranked_external_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' best_ranked_external_metrics(df$result)
#'

best_ranked_external_metrics <- function (df) {
  r <- list("result" = calculate_best_external_variables_by_metrics(df))
  r
}

#' Method that shows on screen the result of best rated external metrics
#'
#' @param x list containing the result of the best_ranked_external_metrics run
#' @param ... other params
#'
#' @keywords internal
#'

print.best_ranked_external_metrics <- function(x)
{
  cat("Result:	\n");		print(x$result)
  invisible(x)
}

#' Method that calculates the best rated internal metrics
#'
#' @param df  data matrix or data frame
#'
#' @return returns a table with the internal metrics that has the best rating
#'
#' @export
#'
#' best_ranked_internal_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' best_ranked_internal_metrics(df$result)
#'

best_ranked_internal_metrics <- function (df) {
  r <- list("result" = calculate_best_internal_variables_by_metrics(df))
  r
}

#' Method that shows on screen the result of best rated internal metrics
#'
#' @param x list containing the result of the best_ranked_internal_metrics run
#' @param ... other params
#'
#' @keywords internal
#'

print.best_ranked_internal_metrics <- function(x, ...)
{
  cat("Result:	\n");		print(x$result, ...)
  invisible(x)
}

#' Method that calculates which algorithm behaves best for the datasets provided
#'
#' @param df data matrix or data frame
#'
#' @return returns a table with the best performing algorithm for the provided datasets
#'
#' @export
#'
#' evaluate_validation_external_by_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' evaluate_validation_external_by_metrics(df$result)
#'

evaluate_validation_external_by_metrics <- function (df) {

  df <- best_ranked_external_metrics(df)
  r <- list("result" = calculate_validation_external_by_metrics(df$result))
  r
}

#' Method that shows on screen the result of algorithm behaves best for the datasets provided
#'
#' @param x list containing the result of the evaluate_validation_external_by_metrics run
#' @param ... other params
#'
#' @keywords internal
#'

print.evaluate_validation_external_by_metrics <- function(x, ...)
{
  cat("Result:	\n");		print(x$result, ...)
  invisible(x)
}

#' Method that calculates which algorithm behaves best for the datasets provided
#'
#' @param df data matrix or data frame
#'
#' @return returns a table with the best performing algorithm for the provided datasets
#'
#' @export
#'
#' evaluate_validation_internal_by_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' evaluate_validation_internal_by_metrics(df$result)
#'

evaluate_validation_internal_by_metrics <- function (df) {
  df <- best_ranked_internal_metrics(df)
  r <- list("result" = calculate_validation_internal_by_metrics(df$result))
  r
}

#' Method that shows on screen the result of algorithm behaves best for the datasets provided
#'
#' @param x list containing the result of the evaluate_validation_internal_by_metrics run
#' @param ... other params
#'
#' @keywords internal
#'

print.evaluate_validation_internal_by_metrics <- function(x, ...)
{
  cat("Result:	\n");		print(x$result, ...)
  invisible(x)
}

#' Method that calculates which algorithm and which metric behaves best for the datasets provided
#'
#' @param df data matrix or data frame
#'
#' @return returns a table with the algorithm and the best performing metric for the datasets
#'
#' @export
#'
#' evaluate_best_validation_external_by_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' evaluate_best_validation_external_by_metrics(df$result)
#'

evaluate_best_validation_external_by_metrics <- function(df) {
  df <- best_ranked_external_metrics(df)
  r <- list("result" = calculate_best_validation_external_by_metrics(df$result))
  r
}

#' Method that shows on screen which algorithm and which metric behaves best for the datasets provided
#'
#' @param x list containing the result of the evaluate_best_validation_external_by_metrics run
#' @param ... other params
#'
#' @keywords internal
#'

print.evaluate_best_validation_external_by_metrics <- function(x, ...)
{
  cat("Result:	\n");		print(x$result, ...)
  invisible(x)
}

#' Method that calculates which algorithm and which metric behaves best for the datasets provided
#'
#' @param df data matrix or data frame
#'
#' @return returns a table with the algorithm and the best performing metric for the datasets
#'
#' @export
#'
#' evaluate_best_validation_internal_by_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' evaluate_best_validation_internal_by_metrics(df$result)
#'

evaluate_best_validation_internal_by_metrics <- function(df) {
  df <- best_ranked_internal_metrics(df)
  r <- list("result" = calculate_best_validation_internal_by_metrics(df$result))
  r
}

#' Method that shows on screen which algorithm and which metric behaves best for the datasets provided
#'
#' @param x list containing the result of the evaluate_best_validation_internal_by_metrics run
#' @param ... other params
#'
#' @keywords internal
#'

print.evaluate_best_validation_internal_by_metrics <- function(x, ...)
{
  cat("Result:	\n");		print(x$result, ...)
  invisible(x)
}

#' Method that returns a table with the algorithm and the metric indicated as parameters
#'
#' @param df data matrix or data frame
#' @param algorithm on which we will calculate the results
#'
#' @return returns a table with the algorithm and the metric indicated as parameter
#'
#' @export
#'
#' result_external_algorithm_by_metric
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' result_external_algorithm_by_metric(df$result,'daisy')
#'

result_external_algorithm_by_metric <- function(df, algorithm) {
  df <- best_ranked_external_metrics(df)
  r <- list("result" = show_result_external_algorithm_by_metric(df$result, algorithm))
  r
}

#' Method that shows on screen the table with the algorithm and the metric indicated as parameters
#'
#' @param x list containing the result of the result_external_algorithm_by_metric run
#' @param ... other params
#'
#' @keywords internal
#'

print.result_external_algorithm_by_metric <- function(x, ...)
{
  cat("Result:	\n");		print(x$result, ...)
  invisible(x)
}

#' Method that returns a table with the algorithm and the metric indicated as parameters
#'
#' @param df data matrix or data frame
#' @param algorithm on which we will calorder(v1)culate the results
#'
#' @return returns a table with the algorithm and the metric indicated as parameter
#'
#' @export
#'
#' result_internal_algorithm_by_metric
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' result_internal_algorithm_by_metric(df$result,'gmm')
#'

result_internal_algorithm_by_metric <- function(df, algorithm) {

  df <- best_ranked_internal_metrics(df)
  r <- list("result" = show_result_internal_algorithm_by_metric(df$result, algorithm))
  r
}

#' Method that shows on screen the table with the algorithm and the metric indicated as parameters
#'
#' @param x list containing the result of the result_internal_algorithm_by_metric run
#' @param ... other params
#'
#' @keywords internal
#'

print.result_internal_algorithm_by_metric <- function(x, ...)
{
  cat("Result:	\n");		print(x$result, ...)
  invisible(x)
}


#' Method that graphically compares external evaluation metrics
#'
#' @param df df data matrix or data frame
#' @param metric string with the name of the metric select to evaluate
#'
#' @export
#'
#' plot_external_validation
#'
#' @importFrom
#'
#' ggplot2 ggplot aes_string geom_point xlab ylab labs scale_y_continuous
#'
#' @examples
#'
#' df <- clustering(df = cluster::agriculture, min = 4, max = 5, algorith='gmm')
#'
#' plot_external_validation(df,"precision")
#'

plot_external_validation <- function(df, metric) {

  df_best_ranked <- best_ranked_external_metrics(df$result)

  hasExternalMetrics <- ifelse(df$hasExternalMetrics, 1, 0)

  maximum <- as.numeric(max_value_metric(df$result,metric))

  maximum <-  ifelse(is.infinite(maximum),10000,maximum)

  interval <- maximum / 4

  break_points <- (seq(0,maximum,by=interval))

  if (hasExternalMetrics == 0)
    stop("There are no external metrics to represent")

  exits_metric = FALSE


  for (col in colnames(df_best_ranked$result)) {
    if (col == metric) {
      exits_metric = TRUE
    }
  }

  if (exits_metric) {

    name_metric <- metric
    metric <- paste("as.numeric(",metric,")")

    ggplot(df_best_ranked$result,
           aes_string(x = "Clusters", y = metric, color = "Algorithm")) + ggplot2::geom_point() + xlab(toupper("Clustering"))+ ylab(toupper(name_metric)) + labs(color='Algorithm') + ggplot2::scale_y_continuous(breaks=as.numeric(break_points),limits=c(0, maximum))
  } else
    stop("the metric indicates does not exist in the dataframe")
}



#' Method that graphically compares internal evaluation metrics
#'
#' @param df df data matrix or data frame
#' @param metric string with the name of the metric select to evaluate
#'
#' @export
#'
#' plot_internal_validation
#'
#' @examples
#'
#' df <- clustering(df = cluster::agriculture, min = 4, max = 5, algorith='gmm')
#'
#' plot_internal_validation(df,"dunn")
#'

plot_internal_validation <- function(df, metric) {

  df_best_ranked <- best_ranked_internal_metrics(df$result)

  hasInternalMetrics <- ifelse(df$hasInternalMetrics, 1, 0)

  maximum <- as.numeric(max_value_metric(df$result,metric))

  maximum <-  ifelse(is.infinite(maximum),10000,maximum)

  interval <- maximum / 4

  break_points <- (seq(0,maximum,by=interval))

  if (hasInternalMetrics == 0)
    stop("There are no internal metrics to represent")

  exits_metric = FALSE


  for (col in colnames(df_best_ranked$result)) {
    if (col == metric) {
      exits_metric = TRUE
    }
  }

  if (exits_metric) {

    name_metric <- metric
    metric <- paste("as.numeric(",metric,")")

    ggplot(df_best_ranked$result,
           aes_string(x = "Clusters", y = metric, color = "Algorithm")) + ggplot2::geom_point() + xlab(toupper("Clustering"))+ ylab(toupper(name_metric)) + labs(color='Algorithm') + ggplot2::scale_y_continuous(breaks=as.numeric(break_points),limits=c(0, maximum))
  } else
    stop("the metric indicates does not exist in the dataframe")
}
