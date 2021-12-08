#' @title Clustering GUI.
#'
#' @description Method that allows us to execute the main algorithm in graphic
#' interface mode instead of through the console.
#'
#' @details The operation of this method is to generate a graphical user.
#' interface to be able to execute the clustering algorithm without knowing the
#' parameters.
#' Its operation is very simple, we can change the values and see the behavior
#' quickly.
#'
#' @return GUI with the parameters of the algorithm and their representation in
#' tables and graphs.
#'
#' @importFrom
#' shiny runApp dataTableOutput renderDataTable runExample
#'
#' @importFrom
#' utils install.packages
#'
#' @export
#' appClustering
#'

appClustering <- function() {

  # Verify that the following packages are installed

  if (!requireNamespace("shinycssloaders")) {
    install.packages("shinycssloaders")
  }

  if (!requireNamespace("shinyalert")) {
    install.packages("shinyalert")
  }

  if (!requireNamespace("DT")) {
    install.packages("DT")
  }

  if (!requireNamespace("shinyjs")) {
    install.packages("shinyjs")
  }

  if (!requireNamespace("shinyWidgets")) {
    install.packages("shinyWidgets")
  }

  if (!requireNamespace("shinyFiles")) {
    install.packages("shinyFiles")
  }

  if (!requireNamespace("shinythemes")) {
    install.packages("shinythemes")
  }

  appDir <-
    system.file("shiny", "clustering", package = "Clustering")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `clustering`.",
         call. = F)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

#'
#' @title Clustering algorithm.
#'
#' @description Discovering the behavior of attributes in a set of clustering
#' packages based on evaluation metrics.
#'
#' @param path The path of file. \code{NULL} It is only allowed to use path or
#' df but not both at the same time. Only files in .dat, .csv or arff format are
#' allowed.
#'
#' @param df data matrix or data frame, or dissimilarity matrix. \code{NULL} If
#' you want to use training and test \code{basketball} attributes.
#'
#' @param packages character vector with the packets running the algorithm.
#' \code{NULL} The seven packages implemented are: cluster, ClusterR, advclust,
#' amap, apcluster, pvclust. \cr By default runs all packages.
#'
#' @param algorithm character vector with the algorithms implemented within the
#' package. \code{NULL} The algorithms implemented are: fuzzy_cm,fuzzy_gg,
#' fuzzy_gk,hclust,apclusterK,agnes,clara,daisy, \cr diana,fanny,mona,pam,gmm,
#' kmeans_arma,kmeans_rcpp,mini_kmeans,\cr pvclust.
#'
#' @param min An integer with the minimum number of clusters This data is
#' necessary to indicate the minimum number of clusters when grouping the data.
#' The default value is \code{3}.
#'
#' @param max An integer with the maximum number of clusters. This data is
#' necessary to indicate the maximum number of clusters when grouping the data.
#' The default value is \code{4}.
#'
#' @param metrics Character vector with the metrics implemented to evaluate the
#' distribution of the data in clusters. \code{NULL} The night metrics
#' implemented are: Entropy, Variation_information,\cr
#' Precision,Recall,F_measure,Fowlkes_mallows_index,Connectivity,Dunn and Silhouette.
#'
#' @details The operation of this algorithm is to evaluate how the attributes of
#' a dataset or a set of datasets behave in different clustering algorithms. To do
#' this, it is necessary to indicate the type of evaluation you want to make on
#' the distribution of the data. To be able to execute the algorithm it is necessary
#' to indicate the number of clusters.
#'
#' \code{min} and \code{max}, the algorithms \code{algorithm} or packages#'
#' \code{packages} that we want to cluster and the metrics \code{metrics}.
#'
#'
#' @return A matrix with the result of running all the metrics of the algorithms
#' contained in the packages indicated. We also obtain information with the
#' types of metrics, algorithms and packages executed.
#'
#' \itemize{
#'        \item result It is a list with the algorithms, metrics and variables
#'        defined in the execution of the algorithm.
#'        \item has_internal_metrics Boolean field to indicate if there are
#'        internal metrics such as: dunn, silhoutte and connectivity.
#'        \item has_external_metrics Boolean field to indicate if there are
#'        external metrics such as: precision, recall, f-measure, entropy,
#'        variation information and fowlkes-mallows.
#'        \item algorithms_execute Character vector with the algorithms
#'        executed. These algorithms have been mentioned in the definition of
#'        the parameters.
#'        \item measures_execute Character vector with the measures executed.
#'        These measures have been mentioned in the definition of the
#'        parameters.
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
#' ClusterR GMM predict_GMM distance_matrix KMeans_rcpp predict_KMeans
#' KMeans_arma MiniBatchKmeans predict_MBatchKMeans center_scale
#'
#' @importFrom
#' advclust fuzzy.CM fuzzy.GG fuzzy.GK
#'
#' @importFrom
#' pvclust pvclust pvpick
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
#' @import
#' toOrdinal
#'
#' @export
#' clustering
#'
#' @examples
#'
#' clustering(
#'      df = cluster::agriculture,
#'      min = 3,
#'      max = 3,
#'      algorithm='clara',
#'      metrics=c('Precision')
#' )
#'
#'
#'

clustering <- function(path = NULL,
                       df = NULL,
                       packages = NULL,
                       algorithm = NULL,
                       min = 3,
                       max = 4,
                       metrics = NULL) {

  ## Validation of input parameters

  on.exit(options(scipen = 999))

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
    stop("The min field must be numeric")

  if (!is.numeric(max))
    stop("The max field must be numeric")

  if (min > max) {
    stop("The minimum cluster number must be less than the maximum cluster
         number")
  }

  if (!is.null(metrics) && !is.vector(metrics)) {
    stop("The field metrics must be a array")
  }

  if (!is.null(metrics) && length(metrics) > CONST_ZERO) {
    for (iterateMetrics in 1:length(metrics)) {
      if (!metrics[iterateMetrics] %in% metrics_validate())
        stop("The metric indicated is not among the metrics used by the")
    }
  }

  attributes <- T

  name_dataframe <- CONST_TIME_DF

  if (!is.null(df)){
    name_dataframe <- unlist(strsplit(x = toString(match.call()),split = ","))[2]
    name_dataframe <- gsub("Clustering::", "", name_dataframe)
  }

  #Start of the main method that executes the datasets.

  execute_datasets(path,
                   df,
                   packages,
                   algorithm,
                   min,
                   max,
                   metrics,
                   attributes,
                   name_dataframe)
}



#' @title Evaluation clustering algorithm.
#'
#' @description Method of performing information processing
#'
#' @param path Path where the datasets are located.
#'
#' @param df Data matrix or data frame, or dissimilarity matrix, depending on
#' the value of the argument.
#'
#' @param packages Array defining the clustering package. The seven packages
#' implemented are: cluster, ClusterR, advclust, amap, apcluster, pvclust.
#' By default runs all packages.
#'
#' @param algorithm Array with the algorithms that implement the package.
#' The algorithms implemented are: fuzzy_cm,fuzzy_gg,fuzzy_gk,hclust,apclusterK,
#' agnes,clara,daisy,diana,fanny,mona,pam,gmm,kmeans_arma,kmeans_rcpp,
#' mini_kmeans, pvclust.
#'
#' @param cluster_min Minimum number of clusters. at least one must be.
#'
#' @param cluster_max Maximum number of clusters. cluster_max must be greater or
#' equal cluster_min.
#'
#' @param metrics Array defining the metrics avalaible in the package. The night
#' metrics implemented are: Entropy, Variation_information, Precision, Recall,
#' F_measure, Fowlkes_mallows_index, Connectivity, Dunn and Silhouette.
#'
#' @param name_dataframe Name of data.frame when df is fill.
#'
#' @return Returns a matrix with the result of running all the metrics of the
#' algorithms contained in the packages we indicated.
#'
#' @keywords internal
#'

execute_datasets <- function(path,
                             df,
                             packages,
                             algorithm,
                             cluster_min,
                             cluster_max,
                             metrics,
                             attributes,
                             name_dataframe) {

  # Initialization of the parameter format

  formals(print.data.frame)$row.names <- F

  results <- NULL

  if (is.null(algorithm)) {
    # We calculate the algorithms to be executed depending on the package

    algorithms_execute <- algorithms_package(packages)

    # Depending on the algorithms we determine the measures of dissimilarity

    measures_execute = measure_package(packages)

  } else {
    # In the case of indicating the algorithm parameter, we carry out the
    # calculation to obtain the dissimilarity measurements

    algorithms_execute <- algorithm

    measures_execute = measure_calculate(algorithm)
  }

  # We calculate if you have internal evaluation measures

  is_metric_internal <- is_Internal_Metrics(metrics)

  # We calculate if you have external evaluation measures

  is_metric_external <- is_External_Metrics(metrics)

  if (!is_metric_external)
    stop("Must have external metrics")

  # Character vector with the clustering evaluation measures

  metrics_execute = metrics_calculate(metrics, attributes, is_metric_internal, is_metric_external)

  # Variable initialization

  numberDataSets <- CONST_ZERO

  numberClusters <- (cluster_max - cluster_min) + CONST_ONE

  directory_files <- CONST_NULL

  # If we have indicated the path field, we read all the existing files in that
  # directory.

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

    # We carry out the calculations of all the algorithms and metrics for the
    # files or datatsets indicated at the beginning of the algorithm.

    results <-
      execute_package_parallel(
        directory_files,
        df,
        algorithms_execute,
        measures_execute,
        cluster_min,
        cluster_max,
        metrics_execute,
        attributes,
        number_algorithms,
        numberClusters,
        numberDataSets,
        is_metric_external,
        is_metric_internal,
        name_dataframe
      )
  }

  results <- as.data.frame(results$df_result, stringsAsFactors = F)

  # Convert column to numeric

  for (parse_column in 6:ncol(results)) {
    results[, parse_column] = as.numeric(results[, parse_column])

  }

  # We return a list with the result of the execution, algorithms, packages,
  # tables in latex format with the separation of the results in internal and
  # external

  res <-
    list(
      result = results,
      has_internal_metrics = is_metric_internal,
      has_external_metrics = is_metric_external,
      algorithms_executed = algorithms_execute,
      measures_executed = measures_execute
    )

  class(res) = "clustering"

  res
}

#' @title Evaluation clustering algorithm.
#'
#' @description Method that evaluates clustering algorithm from a file directory
#' or dataframe.
#'
#' @param directory_files It's a string with the route where the datasets are
#' located.
#'
#' @param df Data matrix or data frame, or dissimilarity matrix, depending on
#' the value of the argument.
#'
#' @param algorithms_execute Character vector with the algorithms to be
#' executed. The algorithms implemented are: fuzzy_cm,fuzzy_gg,fuzzy_gk,hclust,
#' apclusterK,agnes,clara,daisy,diana,fanny,mona,pam,gmm,kmeans_arma,
#' kmeans_rcpp,mini_kmeans, pvclust.
#'
#' @param measures_execute Character array with the measurements of
#' dissimilarity to be executed. Depending on the algorithm, one or the other is
#' implemented. Among them we highlight: Euclidena, Manhattan, etc.
#'
#' @param cluster_min Minimum number of clusters.
#'
#' @param cluster_max Maximum number of clusters. cluster_max must be greater or
#' equal cluster_min.
#'
#' @param metrics_execute Character array defining the metrics to be executed.
#' The night metrics implemented are: Entropy, Variation_information, Precision,
#' Recall, F_measure, Fowlkes_mallows_index, Connectivity, Dunn and Silhouette.
#'
#' @param number_algorithms It's a numeric field with the number of algorithms.
#'
#' @param numberClusters It's a numeric field with the difference between clusters.
#'
#' @param numberDataSets It's a numeric field with the number of datasets.
#'
#' @param is_metric_external Boolean field to indicate whether to run external metrics.
#'
#' @param is_metric_internal Boolean field to indicate whether to run internal metrics.
#'
#' @param name_dataframe Name of data.frame when is fill.
#'
#' @return Returns a list with the result matrix of evaluating the data from the
#' indicated algorithms, metrics and number of clusters.
#'
#' @keywords internal
#'

execute_package_parallel <-
  function(directory_files,
           df,
           algorithms_execute,
           measures_execute,
           cluster_min,
           cluster_max,
           metrics_execute,
           attributes,
           number_algorithms,
           numberClusters,
           numberDataSets,
           is_metric_external,
           is_metric_internal,
           name_dataframe) {

    # We start the process of creating clusters to perform parallel runs.

    cl <-
      parallel::makeCluster(availableCores() - 1,
                            timeout = 60,
                            setup_strategy = "sequential")
    registerDoParallel(cl)

    numberColumns <-
      length(metrics_execute) + CONST_COLUMN_DEFAULT_TABLE

    # We create the different matrixes and vectors needed to return the results.

    df_result <-
      matrix(
        nrow = number_algorithms * length(measures_execute) * numberClusters *
          numberDataSets,
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

      #name_package = gsub("\\_", "\\\\_", algorithms_execute[i])

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

                # If the indicated parameter is the dataframe, we make the
                # appropriate calculations.

                result <-
                  value(
                    evaluate_all_column_dataset(
                      as.matrix(df),
                      measures_execute[j],
                      k,
                      name_dataframe,
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
                                 name_dataframe,
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
              measuFormat = refactorName(measures_execute[j])

              # We carry out the assignment of the calculations for each of the
              # attributes.

              for (c in 1:length(entropy)) {
                information <- value(
                  calculate_result(
                    name_measure,
                    measuFormat,
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
                    timeInternal,
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
                    F
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

                if (attributes) {
                  information <- value(
                    calculate_result(
                      name_measure,
                      measuFormat,
                      k,
                      name_file,
                      c,
                      timeExternal,
                      if (CONST_ENTROPY_METRIC %in% metrics_execute)
                        entropy
                      else
                        CONST_NULL,
                      if (CONST_VARIATION_INFORMATION_METRIC %in%
                          metrics_execute)
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
                      if (CONST_FOWLKES_MALLOWS_INDEX_METRIC %in%
                          metrics_execute)
                        fowlkes_mallows_index
                      else
                        CONST_NULL,
                      if (CONST_F_MEASURE_METRIC %in% metrics_execute)
                        f_measure
                      else
                        CONST_NULL,
                      timeInternal,
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
                      T
                    )
                  )

                  result_information_aux <-
                    as.vector(unlist(information))

                  pos_aux <- 6

                  pos_init_aux <- length(information) + 1

                  for (pos in pos_init_aux:numberColumns) {
                    option_format <- options(digits = 4)
                    on.exit(options(option_format))
                    result_information[pos] = format(
                      round(
                        x = as.numeric(result_information_aux[pos_aux]),
                        digits = 4
                      ),
                      scientific = F
                    )
                    pos_aux = pos_aux + 1
                  }
                }

                # We assign the value in the matrix

                df_result[rowCount, ] = result_information

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

    result = list("df_result" = df_result[1:rowCount, ])

    # We stop the clusters created in the parallel execution.
    on.exit(parallel::stopCluster(cl))

    return(result)
  }



print.clustering <- function(x, ...)
{
  cat("Result:	\n")
  data <- resultClustering(x$result)
  data <- convert_toOrdinal(data)
  print(data)
  cat("\n")
  invisible(x)
}

#'
#' @title Returns the clustering result sorted by a set of metrics.
#'
#' @description This function receives a clustering object and sorts the
#' columns by parameter. By default it performs sorting by the algorithm field.
#'
#' @param x It's an \code{clustering} object.
#'
#' @param decreasing A logical indicating if the sort should be increasing or
#' decreasing. By default, decreasing.
#'
#' @param ... Additional parameters as "by", a String with the name of the
#' evaluation measure to order by. Valid values are: \code{Algorithm, Distance,
#' Clusters, Data, Var, Time, Entropy, Variation_information,
#' Precision, Recall, F_measure, Fowlkes_mallows_index, Connectivity, Dunn,
#' Silhouette and TimeAtt}.
#'
#' @details The additional argument in "..." is the 'by' argument, which is a
#' array with the name of the evaluation measure to order by. Valid value are:
#' \code{Algorithm, Distance, Clusters, Data, Var, Time, Entropy,
#' Variation_information, Precision, Recall, F_measure, Fowlkes_mallows_index,
#' Connectivity, Dunn, Silhouette, TimeAtt}.
#'
#' @return Another \code{clustering} object with the evaluation measures sorted
#'
#' @importFrom
#'
#' dplyr arrange_at %>% select desc
#'
#' @examples
#'
#' library(Clustering)
#'
#' result <-
#' clustering(df = cluster::agriculture,min = 4, max = 4,algorithm='gmm',
#' metrics='Recall')
#'
#' sort(result, FALSE, 'Recall')
#'

sort.clustering <- function(x, decreasing = TRUE, ...) {

  if (is.null(x))
    stop("The x field must be filled")

  if (class(x) != "clustering")
    stop("The x field must be clustering type")

  #Catch dots arguments
  by <- c(...)

  if (length(by) == 0) {
    by <- "Algorithm"
  }

  if (decreasing) {
    result_sort <-  tryCatch({
      resultClustering(x$result) %>% arrange_at(by, desc)
    },
    error = function(cond) {
      stop("The ... received as a parameter is not correct")
    })
  } else {
    result_sort <- resultClustering(x$result) %>% arrange_at(by)
  }

  result <-
    list(
      result = result_sort,
      has_internal_metrics = x$has_internal_metrics,
      has_external_metrics = x$has_external_metrics,
      algorithms_executed = x$algorithms_execute,
      measures_executed = x$measures_execute
    )

  class(result) <- "clustering"

  return(result)
}

#'
#' @title Filter metrics in a \code{clustering} object returning a new
#' \code{clustering} object.
#'
#' @description Generates a new filtered \code{clustering} object.
#'
#' @param clustering The \code{clustering} object to filter.
#'
#' @param condition Expression to filter the \code{clustering} object.
#'
#' @details This function allows you to filter the data set for a given
#' evaluation metric. The evaluation metrics available are:
#' \code{Algorithm, Distance, Clusters, Data, Var, Time, Entropy,
#' Variation_information, Precision, Recall, F_measure, Fowlkes_mallows_index,
#' Connectivity, Dunn, Silhouette and TimeAtt}.
#'
#' @examples
#'
#' library(Clustering)
#'
#' result <- clustering(df = Clustering::basketball, algorithm = 'clara',
#' min=3, max=4, metrics = c('Precision','Recall'))
#'
#' result[Precision > 0.14 & Recall > 0.11]
#'

"[.clustering" <- function(clustering, condition = T) {

  if (is.null(clustering))
    stop("The clustering field must be filled")

  if (class(clustering) != "clustering")
    stop("The clustering field must be clustering type")

  filter <- substitute(condition, )

  if (is.call(filter)) {
    rulesToKeep <-  tryCatch({
      dplyr::filter(resultClustering(clustering$result), eval(filter))
    },
    error = function(cond) {
      stop("The condition received as a parameter is not correct")
    })

  } else {
    rulesToKeep <- resultClustering(clustering$result)
  }

  result <-
    list(
      result = rulesToKeep,
      has_internal_metrics = clustering$has_internal_metrics,
      has_external_metrics = clustering$has_external_metrics,
      algorithms_executed = clustering$algorithms_execute,
      measures_executed = clustering$measures_execute
    )

  class(result) <- "clustering"

  return(result)

}

summary.clustering <- function(object, ...)
{
  class(object) <- "summary.clustering"
  object
}

print.summary.clustering <- function(x, ...) {

  cat("Object of class 'clustering'")
  cat("Result:	\n")
  print(convert_toOrdinal(resultClustering(x$result)))
  cat("\n")
  cat("Internal Metrics:	\n")
  print(x$has_internal_metrics)
  cat("\n")
  cat("External Metrics:	\n")
  print(x$has_external_metrics)
  cat("\n")
  cat("Number of Algorithms:	\n")
  print(length(x$algorithms_executed))
  cat("\n")
  cat("Number of Measures:	\n")
  print(length(x$measures_executed))
  cat("\n")
  cat("Total elements:	\n")
  print(NROW(x$result))
  cat("\n")

  columns <- colnames(resultClustering(x$result))

  if (CONST_TIME_EXTERNAL %in% columns) {
    cat("Mean time for evaluation of external metrics:	\n")
    print(format(round(
      mean(x$result$Time), digits = 4
    ), scientific = F))
    cat("\n")
  }

  if (CONST_ENTROPY_METRIC %in% columns) {
    cat("Metric mean Entropy:	\n")
    print(format(round(mean(
      x$result$Entropy
    ), digits = 4), scientific = F))
    cat("\n")
  }

  if (CONST_VARIATION_INFORMATION_METRIC %in% columns) {
    cat("Metric mean Variation_information:	\n")
    print(format(round(
      mean(x$result$Variation_information), digits = 4
    ), scientific = F))
    cat("\n")
  }

  if (CONST_PRECISION_METRIC %in% columns) {
    cat("Metric mean Precision:	\n")
    print(format(round(mean(
      x$result$Precision
    ), digits = 4), scientific = F))
    cat("\n")
  }

  if (CONST_RECALL_METRIC %in% columns) {
    cat("Metric mean Recall:	\n")
    print(format(round(mean(
      x$result$Recall
    ), digits = 4), scientific = F))
    cat("\n")
  }

  if (CONST_F_MEASURE_METRIC %in% columns) {
    cat("Metric mean F_measure:	\n")
    print(format(round(mean(
      x$result$F_measure
    ), digits = 4), scientific = F))
    cat("\n")
  }

  if (CONST_FOWLKES_MALLOWS_INDEX_METRIC %in% columns) {
    cat("Metric mean Fowlkes_mallows_index:	\n")
    print(format(round(
      mean(x$result$Fowlkes_mallows_index), digits = 4
    ), scientific = F))
    cat("\n")
  }

  invisible(x)
}

print.clustering_sort <- function(x, ...) {
  cat("Result:	\n")
  print(convert_toOrdinal(x$result))
  cat("\n")
  invisible(x)
}

#'
#' @title Best rated external metrics.
#'
#' @description Method in charge of searching for each algorithm those that have
#' the best external classification.
#'
#' @param df Matrix or data frame with the result of running the clustering
#' algorithm.
#'
#' @description Method that looks for those external attribute that are better
#' classified, making use of the var column. In this way of discard attribute and
#' only work with those that give the best response to the algorithm in question.
#'
#' @return Returns a data.frame with the best classified external attribute.
#'
#' @export
#' best_ranked_external_metrics
#'
#' @examples
#'
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 4,
#'                algorithm='gmm',
#'                metrics=c("Recall")
#'          )
#'
#' best_ranked_external_metrics(df = result)
#'

best_ranked_external_metrics <- function(df) {

  result <-
    list("result" = calculate_best_external_variables_by_metrics(df$result))

  class(result) <- "best_ranked_external_metrics"

  result
}

print.best_ranked_external_metrics <- function(x, ...)
{
  cat("Result:	\n")
  print(convert_toOrdinal(x$result))
  invisible(x)
}

#'
#' @title Best rated internal metrics.
#'
#' @description Method in charge of searching for each algorithm those that have
#' the best internal classification.
#'
#' @param df Matrix or data frame with the result of running the clustering
#' algorithm.
#'
#' @description Method that looks for those internal attributes that are better
#' classified, making use of the Var column. In this way we discard the attributes
#' and only work with those that give the best response to the algorithm in question.
#'
#' @return Returns a data.frame with the best classified internal attributes.
#'
#' @export
#' best_ranked_internal_metrics
#'
#' @examples
#'
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 5,
#'                algorithm='gmm',
#'                metrics=c("Recall")
#'          )
#'
#' best_ranked_internal_metrics(df = result)
#'
#'

best_ranked_internal_metrics <- function(df) {

  result <-
    list("result" = calculate_best_internal_variables_by_metrics(df$result))

  class(result) <- "best_ranked_internal_metrics"

  result
}

print.best_ranked_internal_metrics <- function(x, ...)
{
  cat("Result:	\n")
  print(convert_toOrdinal(x$result, ...))
  invisible(x)
}

#'
#' @title Evaluate external validations by algorithm.
#'
#' @description Method that calculates which algorithm behaves best for the
#' datasets provided.
#'
#' @param df data matrix or data frame with the result of running the clustering
#' algorithm.
#'
#' @details It groups the results of the execution by algorithms.
#'
#' @return A data.frame with all the algorithms that obtain the best results
#' regardless of the dissimilarity measure used.
#'
#' @export
#'
#' evaluate_validation_external_by_metrics
#'
#' @examples
#'
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 4,
#'                algorithm='kmeans_arma',
#'                metrics=c("Precision")
#'          )
#'
#' evaluate_validation_external_by_metrics(result)
#'
#'

evaluate_validation_external_by_metrics <- function(df) {

  df_best_ranked <- best_ranked_external_metrics(df)

  result <-
    list("result" = calculate_validation_external_by_metrics(df_best_ranked$result))

  class(result) <- "evaluate_validation_external_by_metrics"

  result
}

print.evaluate_validation_external_by_metrics <- function(x, ...)
{
  cat("Result:	\n")
  print(convert_toOrdinal(x$result, ...))
  invisible(x)
}

#'
#' @title Evaluate internal validations by algorithm.
#'
#' @description Method that calculates which algorithm behaves best for the
#' datasets provided.
#'
#' @param df data matrix or data frame with the result of running the clustering
#' algorithm.
#'
#' @details It groups the results of the execution by algorithms.
#'
#' @return A data.frame with all the algorithms that obtain the best results
#' regardless of the dissimilarity measure used.
#'
#' @export
#'
#' evaluate_validation_internal_by_metrics
#'
#' @examples
#'
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 5,
#'                algorithm='kmeans_rcpp',
#'                metrics=c("Recall","Silhouette")
#'          )
#'
#' evaluate_validation_internal_by_metrics(result)
#'
#' \dontrun{
#' evaluate_validation_internal_by_metrics(result$result)
#' }
#'

evaluate_validation_internal_by_metrics <- function(df) {
  df_best_ranked <- best_ranked_internal_metrics(df)
  result <-
    list("result" =
           calculate_validation_internal_by_metrics(df_best_ranked$result))

  class(result) <- "evaluate_validation_internal_by_metrics"

  result
}

print.evaluate_validation_internal_by_metrics <- function(x, ...)
{
  cat("Result:	\n")
  print(x$result, ...)
  invisible(x)
}


#'
#' @title Evaluates algorithms by measures of dissimilarity based on a metric.
#'
#' @description Method that calculates which algorithm and which metric behaves
#' best for the datasets provided.
#'
#' @param df Data matrix or data frame with the result of running the clustering
#' algorithm.
#'
#' @param metric String with the metric.
#'
#' @details Method groups the data by algorithm and distance measure, instead
#' of obtaining the best attribute from the data set.
#'
#' @return A data.frame with the algorithms classified by measures of
#' dissimilarity.
#'
#' @export
#'
#' evaluate_best_validation_external_by_metrics
#'
#' @examples
#'
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 5,
#'                algorithm='kmeans_rcpp',
#'                metrics=c("F_measure"))
#'
#' evaluate_best_validation_external_by_metrics(result,'F_measure')
#'

evaluate_best_validation_external_by_metrics <- function(df,metric) {

  if (is.null(metric))
    stop("The metric field must be filled in")

  if (length(metric) != 1) {
    stop("The metric field cannot contain more than one metric")
  }

  if (!is.character(metric)) {
    stop("The metric field must be a string")
  }

  if (!is_External_Metrics(metric))
    stop("The indicated metric is not external")

  df_best_ranked <- best_ranked_external_metrics(df)

  result <-
    list("result" =
           calculate_best_validation_external_by_metrics(df_best_ranked$result,metric))

  class(result) <- "evaluate_best_validation_external_by_metrics"

  result
}


print.evaluate_best_validation_external_by_metrics <-
  function(x, ...)
  {
    cat("Result:	\n")
    print(convert_toOrdinal(x$result, ...))
    invisible(x)
  }

#'
#' @title Evaluates algorithms by measures of dissimilarity based on a metric.
#'
#' @description Method that calculates which algorithm and which metric behaves
#' best for the datasets provided.
#'
#' @param df Data matrix or data frame with the result of running the clustering
#' algorithm.
#'
#' @param metric It's a string with the metric to evaluate.
#'
#' @details This method groups the data by algorithm and distance measure,
#' instead of obtaining the best attribute from the data set.
#'
#' @return A data.frame with the algorithms classified by measures of
#' dissimilarity.
#'
#' @export
#'
#' evaluate_best_validation_internal_by_metrics
#'
#' @examples
#'
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 5,
#'                algorithm='gmm',
#'                metrics=c("Precision","Connectivity")
#'          )
#'
#' evaluate_best_validation_internal_by_metrics(result,"Connectivity")
#'

evaluate_best_validation_internal_by_metrics <- function(df,metric) {

  if (is.null(metric))
    stop("The metric field must be filled in")

  if (length(metric) != 1) {
    stop("The metric field cannot contain more than one metric")
  }

  if (!is.character(metric)) {
    stop("The metric field must be a string")
  }

  if (!is_Internal_Metrics(metric))
    stop("The indicated metric is not internal")

  df_best_ranked <- best_ranked_internal_metrics(df)

  result <-
    list("result" =
           calculate_best_validation_internal_by_metrics(df_best_ranked$result,metric))

  class(result) <- "evaluate_best_validation_internal_by_metrics"

  result
}

print.evaluate_best_validation_internal_by_metrics <-
  function(x, ...)
  {
    cat("Result:	\n")
    print(convert_toOrdinal(x$result, ...))
    invisible(x)
  }

#'
#' @title External results by algorithm.
#'
#' @description It is used for obtaining the results of an algorithm indicated
#' as a parameter grouped by number of clusters.
#'
#' @param df data matrix or data frame with the result of running the clustering
#' algorithm.
#'
#' @param metric It's a string with the metric to evaluate.
#'
#' @return A data.frame with the results of the algorithm indicated as parameter.
#'
#' @export
#'
#' result_external_algorithm_by_metric
#'
#' @examples
#'
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 5,
#'                algorithm='gmm',
#'                metrics=c("Precision")
#'          )
#'
#' result_external_algorithm_by_metric(result,'Precision')
#'

result_external_algorithm_by_metric <- function(df, metric) {

  if (is.null(metric))
    stop("The metric field must be filled in")

  if (length(metric) != 1) {
    stop("The metric field cannot contain more than one metric")
  }

  if (!is.character(metric)) {
    stop("The metric field must be a string")
  }

  if (!is_External_Metrics(metric))
    stop("The indicated metric is not external")

  df_ranked <- best_ranked_external_metrics(df)

  result <-
    list("result" = show_result_external_algorithm_by_metric(df_ranked$result,
                                                             metric))

  class(result) <- "result_external_algorithm_by_metric"

  result
}

print.result_external_algorithm_by_metric <- function(x, ...)
{
  cat("Result:	\n")
  print(convert_toOrdinal(x$result, ...))
  invisible(x)
}

#'
#' @title Internal results by algorithm
#'
#' @description It is used for obtaining the results of an algorithm indicated
#' as a parameter grouped by number of clusters.
#'
#' @param df data matrix or data frame with the result of running the clustering
#' algorithm.
#'
#' @param metric It's a string with the metric we want to evaluate your results.
#'
#' @return A data.frame with the results of the algorithm indicated as parameter.
#'
#' @export
#'
#' result_internal_algorithm_by_metric
#'
#' @examples
#'
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 5,
#'                algorithm='gmm',
#'                metrics=c("Recall","Silhouette")
#'          )
#'
#' result_internal_algorithm_by_metric(result,'Silhouette')
#'

result_internal_algorithm_by_metric <- function(df, metric) {

  if (is.null(metric))
    stop("The metric field must be filled in")

  if (length(metric) != 1) {
    stop("The metric field cannot contain more than one metric")
  }

  if (!is.character(metric)) {
    stop("The metric field must be a string")
  }

  if (!is_Internal_Metrics(metric))
    stop("The indicated metric is not internal")

  df_ranked <- best_ranked_internal_metrics(df)

  result <-
    list("result" = show_result_internal_algorithm_by_metric(df_ranked$result,
                                                             metric))

  class(result) <- "result_internal_algorithm_by_metric"

  result
}

print.result_internal_algorithm_by_metric <- function(x, ...)
{
  cat("Result:	\n")
  print(convert_toOrdinal(x$result, ...))
  invisible(x)
}


#'
#' @title Graphic representation of the evaluation measures.
#'
#' @description Graphical representation of the evaluation measures grouped by
#' cluster.
#'
#' @param df data matrix or data frame with the result of running the clustering
#' algorithm.
#'
#' @param metric it's a string with the name of the metric select to evaluate.
#'
#' @details In certain cases the review or filtering of the data is necessary to
#' select the data, that is why thanks to the graphic representations this task
#' is much easier. Therefore with this method we will be able to filter the data
#' by metrics and see the data in a graphical way.
#'
#' @importFrom
#'
#' ggplot2 ggplot aes_string geom_point xlab ylab labs scale_y_continuous
#'
#' @export
#'
#' plot_clustering
#'
#' @examples
#'
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 5,
#'                algorithm='gmm',
#'                metrics=c("Precision")
#'          )
#'
#' plot_clustering(result,c("Precision"))
#'

plot_clustering <- function(df, metric) {

  if (is.null(metric))
    stop("Metric field must be filled in")

  if (!is.null(metric) &&
      length(metric) > 1)
    stop("The metric field only accepts one element")

  if (!is.null(metric) &&
      !is.character(metric))
    stop("Metric field must be a string")

  '%not in%' <- Negate('%in%')

  if (tolower(metric) %not in% tolower(colnames(df$result)))
    stop("The metric indicate does not exist in the dataframe")

  isExternalMetrics <- is_External_Metrics(metric)

  if (!isExternalMetrics)
    stop("Must have external metrics")

  isInternalMetrics <- is_Internal_Metrics(metric)

  if (isExternalMetrics == F &&
      isInternalMetrics == F)
    stop("The metric field indicated is not correct")

  df_best_ranked <- NULL

  if (isExternalMetrics) {
    df_best_ranked <-
      show_result_external_algorithm_group_by_clustering(df$result)
  } else {
    df_best_ranked <-
      show_result_internal_algorithm_group_by_clustering(df$result)
  }

  # We calculate the maximum value of l metric. In case the value is infinite we
  # set a limit.

  maximum <- as.numeric(max_value_metric(df$result, metric,isExternalMetrics))

  maximum <-  ifelse(is.infinite(maximum), 10000, maximum)

  interval <- maximum / 4

  break_points <- (seq(0, maximum, by = interval))

  exits_metric = F

  for (col in colnames(df_best_ranked)) {
    if (tolower(col) == tolower(metric)) {
      exits_metric = T
    }
  }

  if (exits_metric) {
    name_metric <- metric
    metric <- paste("as.numeric(", metric, ")")

    ggplot(df_best_ranked,
           aes_string(x = "Clusters", y = metric, fill = 'Algorithm')) +
      ggplot2::geom_bar(position = 'dodge2', stat = 'identity') +
      ggplot2::theme_minimal() + xlab(toupper("CLUSTERS")) +
      ylab(toupper(name_metric)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
                     axis.title.x = ggplot2::element_text(size = 25),
                     axis.title.y = ggplot2::element_text(size = 25),
                     axis.text.y = ggplot2::element_text(size = 15),
                     legend.text = ggplot2::element_text(size = 15));

  } else
    stop("The metric indicate does not exist in the dataframe")

}

#'
#' @title Export result of external metrics in latex.
#'
#' @description Method that exports the results of external measurements in
#' latex format to a file.
#'
#' @param df It's a dataframe that contains as a parameter a table in latex format
#' with the results of the external validations.
#'
#' @param path It's a string with the path to a directory where a file is to be
#' stored in latex format.
#'
#' @details When we work in latex format and we need to create a table to export
#' the results, with this method we can export the results of the clustering
#' algorithm to latex.
#'
#' @details
#'
#' @export
#'
#' export_file_external
#'
#' @examples
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 5,
#'                algorithm='gmm',
#'                metrics=c("Precision")
#'          )
#'
#' export_file_external(result)
#' file.remove("external_data.tex")
#'

export_file_external <- function(df, path = NULL) {

  '%not in%' <- Negate('%in%')

  if (is.null(df) ||
      !is.list(df))
    stop("The df field must be a list")

  if (!df$has_external_metrics)
    stop("The df field must have external evaluation metrics")

  if (!is.null(path) &&
      !dir.exists(path))
    stop("The path must be a valid directory")

  tableExternal <- dataframe_by_metrics_evaluation(df$result)

  tableExternal <-
    xtable(xtable(
      tableExternal,
      include.rownames = F,
      digits = 4
    ))

  if (!is.null(path)) {
    print(tableExternal,
          file = paste(path, CONST_NAME_EXTERNAL_FILA_LATEX))
  } else
    print(tableExternal,
          file = CONST_NAME_EXTERNAL_FILA_LATEX)
}

#'
#' @title Export result of internal metrics in latex.
#'
#' @description Method that exports the results of internal measurements in
#' latex format to a file.
#'
#' @param df It's a dataframe that contains as a parameter a table in latex format
#' with the results of the internal validations.
#'
#' @param path It's a string with the path to a directory where a file is to be
#' stored in latex format.
#'
#' @details When we work in latex format and we need to create a table to export
#' the results, with this method we can export the results of the clustering
#' algorithm to latex.
#'
#' @export
#'
#' export_file_internal
#'
#' @examples
#' result = clustering(
#'                df = cluster::agriculture,
#'                min = 4,
#'                max = 5,
#'                algorithm='gmm',
#'                metrics=c("Recall","Dunn")
#'          )
#'
#' export_file_internal(result)
#' file.remove("internal_data.tex")
#'

export_file_internal <- function(df, path = NULL) {
  '%not in%' <- Negate('%in%')

  if (is.null(df) ||
      !is.list(df))
    stop("The df field must be a list")

  if (!df$has_internal_metrics)
    stop("The df field must have internal evaluation metrics")

  if (!is.null(path) &&
      !dir.exists(path))
    stop("The path must be a valid directory")

  tableInternal <- dataframe_by_metrics_evaluation(df$result, F)

  tableInternal <-
    xtable(xtable(
      tableInternal,
      include.rownames = F,
      digits = 4
    ))

  if (!is.null(path)) {
    print(tableInternal,
          file = paste(path, CONST_NAME_INTERNAL_FILA_LATEX))
  } else {
    print(tableInternal,
          file = CONST_NAME_INTERNAL_FILA_LATEX)
  }
}
