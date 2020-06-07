#'
#' Method that allows us to execute the main algorithm in graphic interface mode instead of through the console.
#'
#' @importFrom
#' shiny runApp dataTableOutput renderDataTable runExample
#'
#' @export
#' appClustering
#'
#' @examples
#'
#' appClustering()
#'
appClustering <- function() {
  appDir <- system.file("shiny", "clustering", package = "Clustering")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `clustering`.",
         call. = F)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

#'
#' @title Clustering algorithm.
#' @description Discovering the behavior of variables in a set of clustering packages based on evaluation metrics.
#'
#' @param path The path of file. \code{NULL} It is only allowed to use path or df but not both at the same time. Only files in .dat, .csv or arff format are allowed.
#' @param df data matrix or data frame, or dissimilarity matrix. \code{NULL} If you want to use training and test \code{basketball} variables.
#' @param packages character vector with the packets running the algorithm. \code{NULL} The seven packages implemented are: cluster, ClusterR, advclust, amap, apcluster,
#'        gama, pvclust. \cr By default runs all packages.
#' @param algorithm character vector with the algorithms implemented within the package. \code{NULL} The algorithms implemented are: fuzzy_cm,fuzzy_gg,fuzzy_gk,
#'        hclust,apclusterK,agnes,clara,daisy, \cr diana,fanny,mona,pam,gmm,kmeans_arma,kmeans_rcpp,mini_kmeans,gama,\cr pvclust.
#' @param min An integer with the minimum number of clusters This data is necessary to indicate the minimum number of clusters when grouping the data. The default value is \code{3}.
#' @param max An integer with the maximum number of clusters. This data is necessary to indicate the maximum number of clusters when grouping the data. The default value is \code{4}.
#' @param metrics Character vector with the metrics implemented to evaluate the distribution of the data in clusters. \code{NULL} The night metrics implemented are: entropy, variation_information,\cr
#' precision,recall,f_measure,fowlkes_mallows_index,connectivity,dunn,silhouette.
#' @param variables an boolean which indicates that if we want to show as a result the variables of the datasets or the numerical value of the calculation of the metrics. The default value is \code{F}.
#'
#' @details The operation of this algorithm is to evaluate how the variables of a dataset or a set of datasets behave in
#' different grouping algorithms. To do this, it is necessary to indicate the type of evaluation you want to make on the
#' distribution of the data. To be able to execute the algorithm it is necessary to indicate the number of clusters
#' \code{min} and \code{max}, the algorithms \code{algorithm} or packages \code{packages} that we want to cluster,
#' the metrics \code{metrics} and if we want that the results of evaluation are the own classified variables or numerical values \code{variables}.
#'
#' @section How does this algorithm work?:
#' This algorithm improves and complements existing implementations of clustering algorithms.
#'
#' The approaches that exist, are many algorithms that run parallel to the algorithms, without being able to be compared
#' between them. In addition, it was necessary to indicate which variable of the dataset is required to be executed.
#' In addition, depending on the package there are some implementations or others to evaluate the groupings of data,
#' so it is sometimes complicated to compare the groupings between different packages.
#'
#' With this algorithm we can solve the problems mentioned above and determine which algorithm has the best behavior
#' for the set of variables as well as the most efficient number of clusters.
#'
#'
#' @return returns a matrix with the result of running all the metrics of the algorithms contained in the packages we indicated. We also obtain information with the types of metrics, algorithms and packages executed in addition to being able to export the results in latex format.
#' \enumerate{
#'     \itemize{
#'        \item result It is a list with the algorithms, metrics and variables defined in the execution of the algorithm.
#'        \item hasInternalMetrics Boolean field to indicate if there are internal metrics such as: dunn, silhoutte and connectivity.
#'        \item hasExternalMetrics Boolean field to indicate if there are external metrics such as: precision, recall, f-measure, entropy, variation information and fowlkes-mallows.
#'        \item algorithms_execute Vector de caracteres con los algoritmos ejecutados. Dichos algoritmos han sido mencionados en la definición de los parámetros.
#'        \item measures_execute Character vector with the measures executed. These measures have been mentioned in the definition of the parameters.
#'        \item tableExternal It's a string of characters. It contains the results of the external evaluation measures in latex format. This table can be exported to a file using the \code{export_file_external}.
#'        \item tableInternal It's a string of characters. It contains the results of the internal evaluation measures in latex format. This table can be exported to a file using the \code{export_file_internal}.
#'     }
#' }
#'
#'
#' @importFrom
#' apcluster aggExCluster cutree apclusterK negDistMat
#'
#' @importFrom
#' foreach foreach "%:%" "%do%"
#'
#' @importFrom
#' doParallel registerDoParallel
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
#' @import
#' future
#'
#' @export
#' clustering
#'
#' @examples
#'
#' clustering(
#'     df = cluster::agriculture,
#'     min = 4,
#'     max = 5,
#'     algorith='gmm',
#'     variables = TRUE
#' )
#'
#' clustering(
#'    df = Clustering::weather,
#'    min = 2
#'    max = 3,
#'    algorithm = c("gmm","kmeans_rcpp",
#'    metrics = c("precision","recall")
#' )
#'
clustering <- function(path = NULL,
                       df = NULL,
                       packages = NULL,
                       algorithm = NULL,
                       min = 3,
                       max = 4,
                       metrics = NULL,
                       variables = F) {
  ## Validation of input parameters

  if (is.null(path) && is.null(df)) {
    stop("You must fill in at least one of the fields: path or df")
  }

  if (!is.null(path) && !dir.exists(path)) {
    stop("The path field must be a directory")
  }

  if (!is.null(df) && !is.data.frame(df)) {
    stop("The field df must be a data frame")
  }

  if (!is.null(path) && !is.null(df)) {
    stop("You can only fill in df or path")
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

  if (!is.numeric(min))
    stop ("The min field must be numeric")

  if (!is.numeric(max))
    stop ("The max field must be numeric")

  if (min > max) {
    stop("The minimum cluster number must be less than the maximum cluster number")
  }

  if (!is.null(metrics) && !is.vector(metrics)) {
    stop("The field metrics must be a array")
  }

  if (!is.null(metrics) && length(metrics) > CONST_ZERO) {
    for (iterateMetrics in 1:length(metrics)) {
      if (!tolower(metrics[iterateMetrics]) %in% metrics())
        stop("The metric indicated is not among the metrics used by the")
    }
  }

  if (is.null(variables) || !is.logical(variables)) {
    stop("The variable field must be boolean")
  }

  if (length(variables) > 1) {
    stop("The variable field must have only one element")
  }

  #Start of the main method that executes the datasets.

  execute_datasets(path,
                   df,
                   packages,
                   algorithm,
                   min,
                   max,
                   metrics,
                   variables)
}



#' Method of performing information processing.
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
                              df,
                              packages,
                              algorithm,
                              cluster_min,
                              cluster_max,
                              metrics,
                              variables) {
  # Initialization of the parameter format

  on.exit(options(scipen = 999))

  formals(print.data.frame)$row.names <- F

  if (is.null(algorithm)) {
    # We calculate the algorithms to be executed depending on the package

    algorithms_execute <- algorithms_package(packages)

    # Depending on the algorithms we determine the measures of dissimilarity

    measures_execute = measure_package(packages)

  } else {
    # In the case of indicating the algorithm parameter, we carry out the calculation to obtain the dissimilarity measurements

    algorithms_execute <- algorithm

    measures_execute = measure_calculate(algorithm)
  }

  # Character vector with the clustering evaluation measures

  metrics_execute = metrics_calculate(metrics)

  # We calculate if you have internal evaluation measures

  is_metric_internal <- is_Internal_Metrics(metrics)

  # We calculate if you have external evaluation measures

  is_metric_external <- is_External_Metrics(metrics)

  # Variable initialization

  tableInternal <- CONST_INITIALIZER_STRING

  tableExternal <- CONST_INITIALIZER_STRING

  numberDataSets <- CONST_ZERO

  numberClusters <- (cluster_max - cluster_min) + CONST_ONE

  directory_files <- CONST_NULL

  # If we have indicated the path field, we read all the existing files in that directory.

  if (!is.null(path)) {
    directory_files <- path_dataset(path)
  }

  if (!is.null(directory_files) &&
      length(directory_files) > CONST_ZERO) {
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

  if (!is.null(directory_files) || !is.null(df)) {
    # We carry out the calculations of all the algorithms and metrics for the files or datatsets
    # indicated at the beginning of the algorithm.

    result <-
      execute_package_parallel(
        directory_files,
        df,
        algorithms_execute,
        measures_execute,
        cluster_min,
        cluster_max,
        metrics_execute,
        variables,
        number_algorithms,
        numberClusters,
        numberDataSets,
        is_metric_external,
        is_metric_internal
      )
  }

  # If it contains external metrics we create the table in latex format with the external results obtained.

  if (is_metric_external) {
    tableExternal <-
      xtable(xtable(result$df_external), include.rownames = F)
  }

  # If it contains internal metrics we create the table in latex format with the internal results obtained.

  if (is_metric_internal) {
    tableInternal <-
      xtable(xtable(result$df_internal), include.rownames = F)
  }

  #We return a list with the result of the execution, algorithms, packages, tables in latex format with the separation of the results in internal and external

  res <-
    list(
      result = result$df_result,
      hasInternalMetrics = is_metric_internal,
      hasExternalMetrics = is_metric_external,
      algorithms_execute = algorithms_execute,
      measures_execute = measures_execute,
      tableExternal = tableExternal,
      tableInternal = tableInternal
    )

  class(res) = "clustering"

  res
}


execute_package_parallel <-
  function (directory_files,
            df,
            algorithms_execute,
            measures_execute,
            cluster_min,
            cluster_max,
            metrics_execute,
            variables,
            number_algorithms,
            numberClusters,
            numberDataSets,
            is_metric_external,
            is_metric_internal) {

    # We start the process of creating clusters to perform parallel runs.

    cl <- parallel::makeCluster(availableCores(), timeout = 60)
    plan(cluster, workers = cl)

    numberColumns <-
      length(metrics_execute) + CONST_COLUMN_DEFAULT_TABLE

    # We create the different matrixes and vectors needed to return the results.

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

    # Initialization of variables

    rowCount = CONST_ONE

    rowCountLatex  = CONST_ONE

    changeAlgorithm = CONST_ZERO

    number_files = ifelse(!is.null(df),
                          CONST_ONE,
                          length(directory_files))

    # We're going through each of the algorithms

    for (i in 1:length(algorithms_execute)) {
      changeAlgorithm = CONST_ONE

      # From the algorithm we get the package

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

      # We're going through each of the measures("euclidean","manhattan")

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

          #We'll go through each of the clusters

          for (k in cluster_min:cluster_max) {
            changeCluster = CONST_ONE

            #We'll go through each of the files

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

              # We initialize the evaluation metrics

              if (!is.null(df)) {

                #If the indicated parameter is the dataframe, we make the appropriate calculations.

                result <-
                  value(
                    evaluate_all_column_dataset(
                      as.matrix(df),
                      measures_execute[j],
                      k,
                      CONST_TIME_DF,
                      metrics_execute
                    )
                  )

              } else {

                #We perform the calculations for each of the files.

                result <-
                  value(
                    evaluate_all_column_dataset(
                      read_file(directory_files[m]),
                      measures_execute[j],
                      k,
                      directory_files[m],
                      metrics_execute
                    )
                  )
              }

              name_file = ifelse(!is.null(df),
                                 CONST_TIME_DF,
                                 directory_files[m])

              entropy = result$external$entropy
              variation_information = result$external$variation_information
              precision = result$external$precision
              recall = result$external$recall
              f_measure = result$external$f_measure
              fowlkes_mallows_index = result$external$fowlkes_mallows_index
              timeExternal = result$external$time
              timeInternal = result$internal$time
              dunn = result$internal$dunn
              connectivity = result$internal$connectivity
              silhouette = result$internal$silhouette

              # We carry out the assignment of the calculations for each of the variables.

              for (c in 1:length(entropy)) {
                information <- value(
                  calculate_result(
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
                )

                result_information <-
                  as.vector(unlist(information))

                #We checked that the format of the numbers is 4 decimals.

                for (pos in 6:length(information)) {
                  option_format <- options(digits = 4)
                  on.exit(options(option_format))
                  result_information[pos] = format(round(
                    x = as.numeric(result_information[pos]),
                    digits = 4
                  ), scientific = F)
                }

                # We assign the value in the matrix

                df_result[rowCount, ] = result_information

                # From the result obtained we parse the external information

                if (is_metric_external) {
                  df_external[rowCountLatex, ] <- information_external(
                    metrics_execute,
                    information,
                    CONST_EXTERNAL_METRICS_DEFAULT + number_columnas_external(metrics_execute),
                    variables
                  )
                }

                # From the result obtained we parse the internal information

                if (is_metric_internal) {
                  df_internal[rowCountLatex, ] <-  information_internal(
                    metrics_execute,
                    information,
                    CONST_INTERNAL_METRICS_DEFAULT + number_columnas_internal(metrics_execute),
                    variables
                  )
                }

                # we increase the position of the matrix

                rowCount = rowCount + CONST_ONE

                rowCountLatex  = rowCountLatex + CONST_ONE

              }
            }
          }
        }
      }
    }

    rowCount  = rowCount - CONST_ONE

    rowCountLatex = rowCountLatex - CONST_ONE

    result = list(
      "df_result" = df_result[1:rowCount, ],
      "df_external" = df_external[1:rowCountLatex,],
      "df_internal" = df_internal[1:rowCountLatex,]
    )

    # We stop the clusters created in the parallel execution.
    on.exit(parallel::stopCluster(cl))

    return (result)
  }



#' Method that shows on screen the result of the clustring execution.
#'
#' @param x list containing the result of the clustering run.
#' @param ... other params.
#'
#'
print.clustering <- function(x, ...)
{
  cat("Result:	\n")
  print(x$result)
  cat("\n")
  cat("Internal Metrics: \n")
  print(x$hasInternalMetrics)
  cat("\n")
  cat("External Metrics: \n")
  print(x$hasExternalMetrics)
  cat("\n")
  cat("Algorithms Execute: \n ")
  print(x$algorithms_execute)
  cat("\n")
  cat("Measures Execute: \n ")
  print(x$measures_execute)
  cat("\n")
  cat("External Table:\n")
  print(x$tableExternal)
  cat("\n")
  cat("\nInternal Table:\n")
  print(x$tableInternal)
  cat("\n")

  invisible(x)
}


#' Method that calculates the best rated external metrics.
#'
#' @param df  data matrix or data frame with the result of running the clustering algorithm.
#'
#' @return returns a table with the external metrics that has the best rating.
#'
#' @export
#' best_ranked_external_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = T)
#'
#' best_ranked_external_metrics(df)
#'
#'
#' df = clustering(df = Clustering::weather, min = 4, max = 5, algorithm = c("gmm", "kmeans_rcpp"), variables = F)
#'
#' best_ranked_external_metrics(df)
#'

best_ranked_external_metrics <- function (df) {
  result <-
    list("result" = calculate_best_external_variables_by_metrics(df$result))

  class(result) <- "best_ranked_external_metrics"

  result
}

#' Method that shows on screen the result of best rated external metrics.
#'
#' @param x list containing the result of the best_ranked_external_metrics run.
#' @param ... other params.
#'
#' @keywords internal
#'

print.best_ranked_external_metrics <- function(x)
{
  cat("Result:	\n")
  print(x$result)
  invisible(x)
}

#' Method that calculates the best rated internal metrics.
#'
#' @param df data matrix or data frame with the result of running the clustering algorithm.
#'
#' @return returns a table with the internal metrics that has the best rating.
#'
#' @export
#'
#' best_ranked_internal_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = T)
#'
#' best_ranked_internal_metrics(df)
#'
#' df = clustering(df = Clustering::weather, min = 4, max = 5, algorithm = c("gmm", "kmeans_rcpp"), variables = F)
#'
#' best_ranked_internal_metrics(df)
#'

best_ranked_internal_metrics <- function (df) {
  result <-
    list("result" = calculate_best_internal_variables_by_metrics(df$result))

  class(result) <- "best_ranked_internal_metrics"

  result
}

#' Method that shows on screen the result of best rated internal metrics.
#'
#' @param x list containing the result of the best_ranked_internal_metrics run.
#' @param ... other params.
#'
#' @keywords internal
#'

print.best_ranked_internal_metrics <- function(x, ...)
{
  cat("Result:	\n")
  print(x$result, ...)
  invisible(x)
}

#' Method that calculates which algorithm behaves best for the datasets provided.
#'
#' @param df data matrix or data frame with the result of running the clustering algorithm.
#'
#' @return returns a table with the best performing algorithm for the provided datasets.
#'
#' @export
#'
#' evaluate_validation_external_by_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' evaluate_validation_external_by_metrics(df)
#'
#' df = clustering(df = Clustering::weather, min = 4, max = 5, algorithm = c("gmm", "kmeans_rcpp"), variables = F)
#'
#' evaluate_validation_external_by_metrics(df)
#'

evaluate_validation_external_by_metrics <- function (df) {
  df_best_ranked <- best_ranked_external_metrics(df)
  result <-
    list("result" = calculate_validation_external_by_metrics(df_best_ranked$result))

  class(result) <- "evaluate_validation_external_by_metrics"

  result
}

#' Method that shows on screen the result of algorithm behaves best for the datasets provided.
#'
#' @param x list containing the result of the evaluate_validation_external_by_metrics run.
#' @param ... other params.
#'
#' @keywords internal
#'

print.evaluate_validation_external_by_metrics <- function(x, ...)
{
  cat("Result:	\n")
  print(x$result, ...)
  invisible(x)
}

#' Method that calculates which algorithm behaves best for the datasets provided.
#'
#' @param df data matrix or data frame with the result of running the clustering algorithm.
#'
#' @return returns a table with the best performing algorithm for the provided datasets.
#'
#' @export
#'
#' evaluate_validation_internal_by_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' evaluate_validation_internal_by_metrics(df)
#'
#' df = clustering(df = Clustering::weather, min = 4, max = 5, algorithm = c("gmm", "kmeans_rcpp"), variables = F)
#'
#' evaluate_validation_internal_by_metrics(df)
#'

evaluate_validation_internal_by_metrics <- function (df) {
  df_best_ranked <- best_ranked_internal_metrics(df)
  result <-
    list("result" = calculate_validation_internal_by_metrics(df_best_ranked$result))

  class(result) <- "evaluate_validation_internal_by_metrics"

  result
}

#' Method that shows on screen the result of algorithm behaves best for the datasets provided.
#'
#' @param x list containing the result of the evaluate_validation_internal_by_metrics run.
#' @param ... other params.
#'
#' @keywords internal
#'

print.evaluate_validation_internal_by_metrics <- function(x, ...)
{
  cat("Result:	\n")
  print(x$result, ...)
  invisible(x)
}

#' Method that calculates which algorithm and which metric behaves best for the datasets provided.
#'
#' @param df data matrix or data frame with the result of running the clustering algorithm.
#'
#' @return returns a table with the algorithm and the best performing metric for the datasets.
#'
#' @export
#'
#' evaluate_best_validation_external_by_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' evaluate_best_validation_external_by_metrics(df)
#'
#' df = clustering(df = Clustering::weather, min = 4, max = 5, algorithm = c("gmm", "kmeans_rcpp"), variables = F)
#'
#' evaluate_best_validation_external_by_metrics(df)
#'

evaluate_best_validation_external_by_metrics <- function(df) {
  df_best_ranked <- best_ranked_external_metrics(df)
  result <-
    list("result" = calculate_best_validation_external_by_metrics(df_best_ranked$result))

  class(result) <- "evaluate_best_validation_external_by_metrics"

  result
}

#' Method that shows on screen which algorithm and which metric behaves best for the datasets provided.
#'
#' @param x list containing the result of the evaluate_best_validation_external_by_metrics run.
#' @param ... other params.
#'
#' @keywords internal
#'

print.evaluate_best_validation_external_by_metrics <-
  function(x, ...)
  {
    cat("Result:	\n")
    print(x$result, ...)
    invisible(x)
  }

#' Method that calculates which algorithm and which metric behaves best for the datasets provided.
#'
#' @param df data matrix or data frame with the result of running the clustering algorithm.
#'
#' @return returns a table with the algorithm and the best performing metric for the datasets.
#'
#' @export
#'
#' evaluate_best_validation_internal_by_metrics
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' evaluate_best_validation_internal_by_metrics(df)
#'
#' df = clustering(df = Clustering::weather, min = 4, max = 5, algorithm = c("gmm", "kmeans_rcpp"), variables = F)
#'
#' evaluate_best_validation_internal_by_metrics(df)

evaluate_best_validation_internal_by_metrics <- function(df) {
  df_best_ranked <- best_ranked_internal_metrics(df)
  result <-
    list("result" = calculate_best_validation_internal_by_metrics(df_best_ranked$result))

  class(result) <- "evaluate_best_validation_internal_by_metrics"

  result
}

#' Method that shows on screen which algorithm and which metric behaves best for the datasets provided.
#'
#' @param x list containing the result of the evaluate_best_validation_internal_by_metrics run.
#' @param ... other params.
#'
#' @keywords internal
#'

print.evaluate_best_validation_internal_by_metrics <-
  function(x, ...)
  {
    cat("Result:	\n")
    print(x$result, ...)
    invisible(x)
  }

#' Method that returns a table with the algorithm and the metric indicated as parameters.
#'
#' @param df data matrix or data frame with the result of running the clustering algorithm.
#' @param algorithm on which we will calculate the results.
#'
#' @return returns a table with the algorithm and the metric indicated as parameter.
#'
#' @export
#'
#' result_external_algorithm_by_metric
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' result_external_algorithm_by_metric(df,'daisy')
#'

result_external_algorithm_by_metric <- function(df, algorithm) {
  df_ranked <- best_ranked_external_metrics(df)
  result <-
    list("result" = show_result_external_algorithm_by_metric(df_ranked$result, algorithm))

  class(result) <- "result_external_algorithm_by_metric"

  result
}

#' Method that shows on screen the table with the algorithm and the metric indicated as parameters.
#'
#' @param x list containing the result of the result_external_algorithm_by_metric run.
#' @param ... other params.
#'
#' @keywords internal
#'

print.result_external_algorithm_by_metric <- function(x, ...)
{
  cat("Result:	\n")
  print(x$result, ...)
  invisible(x)
}

#' Method that returns a table with the algorithm and the metric indicated as parameters.
#'
#' @param df data matrix or data frame with the result of running the clustering algorithm.
#' @param algorithm on which we will calorder(v1)culate the results.
#'
#' @return returns a table with the algorithm and the metric indicated as parameter.
#'
#' @export
#'
#' result_internal_algorithm_by_metric
#'
#' @examples
#'
#' df = clustering(df = cluster::agriculture, min = 4, max = 5, algorithm='gmm', variables = TRUE)
#'
#' result_internal_algorithm_by_metric(df,'gmm')
#'

result_internal_algorithm_by_metric <- function(df, algorithm) {
  df_ranked <- best_ranked_internal_metrics(df)
  result <-
    list("result" = show_result_internal_algorithm_by_metric(df_ranked$result, algorithm))

  class(result) <- "result_internal_algorithm_by_metric"

  result
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
  cat("Result:	\n")
  print(x$result, ...)
  invisible(x)
}


#' Method that graphically compares external evaluation metrics
#'
#' @param df df data matrix or data frame
#' @param metric string with the name of the metric select to evaluate
#'
#' @export
#'
#' plot.clustering
#'
#' @importFrom
#'
#' ggplot2 ggplot aes_string geom_point xlab ylab labs scale_y_continuous
#'
#' @examples
#'
#' df <- clustering(df = cluster::agriculture, min = 4, max = 5, algorith='gmm')
#'
#' plot.clustering(df,"precision")
#'
#' df <- clustering(df = cluster::agriculture, min = 4, max = 5, algorith='gmm')
#'
#' plot.clustering(df,"recall")
#'

plot.clustering <- function(df, metric) {
  if (is.null(metric))
    stop("Metric field must be filled in")

  if (!is.null(metric) &&
      length(metric) > 1)
    stop("The metric field only accepts one element")

  if (!is.null(metric) &&
      !is.character(metric))
    stop("Metric field must be a string")

  '%not in%' <- Negate('%in%')

  if (metric %not in% colnames(df$result))
    stop("The metric indicate does not exist in the dataframe")

  isExternalMetrics <- is_External_Metrics(metric)

  isInternalMetrics <- is_Internal_Metrics(metric)

  if (isExternalMetrics == F &&
      isInternalMetrics == F)
    stop("The metric field indicated is not correct")

  df_best_ranked <- NULL

  if (is_External_Metrics(metric)) {
    df_best_ranked <- best_ranked_external_metrics(df)
  } else {
    df_best_ranked <- best_ranked_internal_metrics(df)
  }

  #We calculate the maximum value of l metric. In case the value is infinite we set a limit.

  maximum <- as.numeric(max_value_metric(df$result, metric))

  maximum <-  ifelse(is.infinite(maximum), 10000, maximum)

  interval <- maximum / 4

  break_points <- (seq(0, maximum, by = interval))

  exits_metric = F

  for (col in colnames(df_best_ranked$result)) {
    if (col == metric) {
      exits_metric = T
    }
  }

  if (exits_metric) {
    name_metric <- metric
    metric <- paste("as.numeric(", metric, ")")

    ggplot(df_best_ranked$result,
           aes_string(x = "Clusters", y = metric, color = "Algorithm")) + ggplot2::geom_point() + xlab(toupper("Clustering")) + ylab(toupper(name_metric)) + labs(color =                                                                                                                                                                   'Algorithm') + ggplot2::scale_y_continuous(breaks = as.numeric(break_points),
                                                                                                                                                                                                                                                                                                                                                                                       limits = c(0, maximum))
  } else
    stop("The metric indicate does not exist in the dataframe")
}

#'
#' Method that exports the results of external measurements in latex format to a file
#'
#' @param df is a dataframe that contains as a parameter a table in latex format with the results of the external validations
#' @param path is the path to a directory where a file is to be stored in latex format
#'
#' @export
#'
#' export_file_external
#'
#' @example
#'
#' export_file_external(df,'/Users')
#'
#'

export_file_external <- function(df, path) {
  '%not in%' <- Negate('%in%')

  if (is.null(df) ||
      !is.list(df))
    stop ("The df field must be a list")

  if (!df$hasExternalMetrics)
    stop("The df field must have external evaluation metrics")

  if (is.null(df$tableExternal) ||
      !(is.list(df)))
    stop ("The df field must be a list")

  if ("xtable" %not in% class(df$tableExternal))
    stop ("The df$tableExternal field must be a dataframe or xtable")

  if (is.null(path) ||
      length(path) < 1)
    stop ("The path field must be filled in")

  if (!is.null(path) &&
      !dir.exists(path))
    stop ("The path must be a valid directory")

  print(df$tableExternal,
        file = paste(path, CONST_NAME_EXTERNAL_FILA_LATEX))

}

#'
#' Method that exports the results of internal measurements in latex format to a file
#'
#' @param df is a dataframe that contains as a parameter a table in latex format with the results of the internal validations
#' @param path is the path to a directory where a file is to be stored in latex format
#'
#' @export
#'
#' export_file_internal
#'
#' @example
#'
#' export_file_internal(df,'/Users')
#'
#'

export_file_internal <- function(df, path) {
  '%not in%' <- Negate('%in%')

  if (is.null(df) ||
      !is.list(df))
    stop ("The df field must be a list")

  if (!df$hasInternalMetrics)
    stop("The df field must have internal evaluation metrics")

  if (is.null(df$tableInternal) ||
      !(is.list(df)))
    stop ("The df field must be a list")

  if ("xtable" %not in% class(df$tableInternal))
    stop ("The df$tableInternal field must be a dataframe or xtable")

  if (is.null(path) ||
      length(path) < 1)
    stop ("The path field must be filled in")

  if (!is.null(path) &&
      !dir.exists(path))
    stop ("The path must be a valid directory")

  print(df$tableInternal,
        file = paste(path, CONST_NAME_INTERNAL_REPORT_FILE))

}
