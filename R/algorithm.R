#' Method that returns the list of used packages
#'
#' @return package listing array
#'
#' @keywords internal
#'

packages <- function() {
  return (
    c(
      CONST_ALGORITHM_AMAP,
      CONST_ALGORITHM_APCLUSTER,
      CONST_ALGORITHM_CLUSTER,
      CONST_ALGORITHM_CLUSTERR,
      CONST_ALGORITHM_PVCLUST
    )
  )
}

#' Method that returns the list of used algorithms
#'
#' @return algorithm listing array
#'
#' @keywords internal
#'

algorithms <- function() {
  return (
    c(
      CONST_HCLUST,
      CONST_APCLUSTERK,
      CONST_AGNES,
      CONST_CLARA,
      CONST_DAISY,
      CONST_DIANA,
      CONST_FANNY,
      CONST_MONA,
      CONST_PAM,
      CONST_GMM,
      CONST_KMEANS_ARMA,
      CONST_KMEANS_RCPP,
      CONST_MINI_KMEANS,
      CONST_PVCLUST
    )
  )
}

#' Method that returns the list of used metrics
#'
#' @return metrics listing array
#'
#' @keywords internal
#'

metrics_validate <- function() {
  return (
    c(
      CONST_ENTROPY_METRIC,
      CONST_VARIATION_INFORMATION_METRIC,
      CONST_PRECISION_METRIC,
      CONST_RECALL_METRIC,
      CONST_F_MEASURE_METRIC,
      CONST_FOWLKES_MALLOWS_INDEX_METRIC,
      CONST_CONNECTIVITY_METRIC,
      CONST_DUNN_METRIC,
      CONST_SILHOUETTE_METRIC
    )
  )

}

#' Method that returns the list of used external metrics
#'
#' @return external metrics listing array
#'
#' @keywords internal
#'
#'

metrics_external <- function() {
  return (
    c(
      CONST_ENTROPY_METRIC,
      CONST_VARIATION_INFORMATION_METRIC,
      CONST_PRECISION_METRIC,
      CONST_RECALL_METRIC,
      CONST_F_MEASURE_METRIC,
      CONST_FOWLKES_MALLOWS_INDEX_METRIC
    )
  )

}

#' Method that returns the list of used internal metrics
#'
#' @return internal metrics listing array
#'
#' @keywords internal
#'

metrics_internal <- function() {
  return (c(
    CONST_CONNECTIVITY_METRIC,
    CONST_DUNN_METRIC,
    CONST_SILHOUETTE_METRIC
  ))

}


#' Method that checks for external metrics
#'
#' @param metrics array with the metrics used in the execution of the package
#'
#' @return true if it exists and false otherwise
#'
#' @keywords internal
#'

is_External_Metrics <- function(metrics) {
  if (is.null(metrics))
    return (T)


  for (iterate in 1:length(metrics)) {
    if (tolower(metrics[iterate]) %in% tolower(metrics_external()))
      return (T)

  }

  return (F)
}

#' Method that checks for internal metrics
#'
#' @param metrics array with the metrics used in the execution of the package
#'
#' @return true if it exists and false otherwise
#'
#' @keywords internal
#'

is_Internal_Metrics <- function(metrics) {
  if (is.null(metrics))
    return (T)


  for (iterate in 1:length(metrics)) {
    if (tolower(metrics[iterate]) %in% tolower(metrics_internal()))
      return (T)
  }

  return (F)


}

#' Method that returns how many external metrics there are in the array of
#' metrics used in the calculation
#'
#' @param metrics array with the metrics used in the execution of the package
#'
#' @return returns the number of occurrences
#'
#' @keywords internal
#'

number_columnas_external <- function(metrics) {
  numberColumnasExternal <- 0

  for (iterate in 1:length(metrics)) {
    if (tolower(metrics[iterate] %in% tolower(metrics_external()))) {
      numberColumnasExternal <- numberColumnasExternal + 1
    }

  }

  return (numberColumnasExternal)

}


#' Method that returns an array with the internal information of the cluster
#'
#' @param metrics array with the metrics used in the execution of the package
#' @param info list with internal clustering information
#' @param size internal number of columns
#' @param variables Null returns the position of the variable, otherwise it
#' returns the value of the variable
#'
#' @return array with the information from the calculation of the internal
#' evaluation of the clustering
#'
#' @keywords internal
#'

information_internal <- function (metrics, information, size, variables) {
  result <- array(data = NA, dim = size)

  result[1] <- information$algorith
  result[2] <- information$distance
  result[3] <- information$cluster
  result[4] <- information$dataset
  result[5] <- information$ranking

  if (!is.null(variables))
    result[6] <- information$timeInternal
  else
    result[6] <- format(round(as.numeric(information$timeInternal), digits = 4),
                        scientific = FALSE)

  position <- 7

  for (i in 1:length(metrics)) {

    if (tolower(metrics[i] == CONST_CONNECTIVITY_METRIC)) {
      if (variables)
        result[position] <- information$connectivity
      else
        result[position] <-
          as.numeric(format(as.numeric(information$connectivity), digits = 4))
      position <- position + 1
    }

    if (tolower(metrics[i] == CONST_DUNN_METRIC)) {
      if (variables)
        result[position] <- information$dunn
      else
        result[position] <-
          as.numeric(format(as.numeric(information$dunn), digits = 4))
      position <- position + 1
    }

    if (tolower(metrics[i] == CONST_SILHOUETTE_METRIC)) {
      if (variables)
        result[position] <- information$silhouette
      else
        result[position] <-
          as.numeric(format(as.numeric(information$silhouette), digits = 4))
      position <- position + 1
    }
  }

  return(result)
}

#' Method that returns an array with the external information of the cluster
#'
#' @param metrics array with the metrics used in the execution of the package
#' @param info list with external clustering information
#' @param size external number of columns
#' @param variables Null returns the position of the variable, otherwise it
#' returns the value of the variable
#'
#' @return array with the information from the calculation of the external
#' evaluation of the clustering
#'
#' @keywords internal
#'

information_external <- function (metrics, information, size, variables) {
  result <- array(data = NA, dim = size)

  result[1] <- information$algorith
  result[2] <- information$distance
  result[3] <- information$cluster
  result[4] <- information$dataset
  result[5] <- information$ranking

  if (!is.null(variables))
    result[6] <- information$timeExternal
  else
    result[6] <- format(round(as.numeric(information$timeExternal), digits = 4),
                        scientific = FALSE)

  position <- 7

  for (iterate in 1:length(metrics)) {
    if (tolower(metrics[iterate] == CONST_ENTROPY_METRIC)) {
      if (variables)
        result[position] <- information$entropy
      else
        result[position] <-
          as.numeric(format(as.numeric(information$entropy), digits = 4))
      position <- position + 1
    }

    if (tolower(metrics[iterate] == CONST_VARIATION_INFORMATION_METRIC)) {
      if (variables)
        result[position] <- information$variation_information
      else
        result[position] <-
          as.numeric(format(as.numeric(information$variation_information),
                            digits = 4))
      position <- position + 1
    }

    if (tolower(metrics[iterate] == CONST_PRECISION_METRIC)) {
      if (variables)
        result[position] <- information$precision
      else
        result[position] <-
          as.numeric(format(as.numeric(information$precision), digits = 4))
      position <- position + 1
    }

    if (tolower(metrics[iterate] == CONST_RECALL_METRIC)) {
      if (variables)
        result[position] <- information$recall
      else
        result[position] <-
          as.numeric(format(as.numeric(information$recall), digits = 4))

      position <- position + 1
    }

    if (tolower(metrics[iterate] == CONST_FOWLKES_MALLOWS_INDEX_METRIC)) {
      if (variables)
        result[position] <- information$fowlkes_mallows_index
      else
        result[position] <-
          as.numeric(format(as.numeric(information$fowlkes_mallows_index),
                            digits = 4))

      position <- position + 1
    }

    if (tolower(metrics[iterate] == CONST_F_MEASURE_METRIC)) {
      if (variables)
        result[position] <- information$f_measure
      else
        result[position] <-
          as.numeric(format(as.numeric(information$f_measure), digits = 4))

      position <- position + 1
    }
  }

  return(result)
}

#' Method that returns how many internal metrics there are in the array of
#' metrics used in the calculation
#'
#' @param metrics array with the metrics used in the execution of the package
#'
#' @return returns the number of occurrences
#'
#' @keywords internal
#'

number_columnas_internal <- function(metrics) {
  numberColumnasInternal <- 0

  for (iterate in 1:length(metrics)) {
    if (tolower(metrics[iterate] %in% metrics_internal())) {
      numberColumnasInternal <- numberColumnasInternal + 1
    }

  }

  return (numberColumnasInternal)

}


#' Method that returns all the algorithms executed by the package
#'
#' @param packages package array
#'
#' @return array with the algorithms we're going to run
#'
#' @keywords internal
#'

algorithms_package <- function(packages) {
  algorithms <- vector()

  if (is.null(packages)) {
    # All algorithm
    algorithms <- c(algorithms, algorithm_amap())
    algorithms <- c(algorithms, algorithm_apcluster())
    algorithms <- c(algorithms, algorithm_cluster())
    algorithms <- c(algorithms, algorithm_clusterr())
    algorithms <- c(algorithms, algorithm_pvclust())

  } else {
    for (iterate in 1:length(packages)) {

      if (tolower(packages[iterate]) == CONST_ALGORITHM_AMAP) {
        algorithms <- c(algorithms, algorithm_amap())
      }

      if (tolower(packages[iterate]) == CONST_ALGORITHM_APCLUSTER) {
        algorithms <- c(algorithms, algorithm_apcluster())
      }

      if (tolower(packages[iterate]) == CONST_ALGORITHM_CLUSTER) {
        algorithms <- c(algorithms, algorithm_cluster())
      }

      if (tolower(packages[iterate]) == CONST_ALGORITHM_CLUSTERR) {
        algorithms <- c(algorithms, algorithm_clusterr())
      }

      if (tolower(packages[iterate]) == CONST_ALGORITHM_PVCLUST) {
        algorithms <- c(algorithms, algorithm_pvclust())
      }
    }
  }

  return(algorithms)

}

#' Method that returns all the measures executed by the package from the
#' indicated algorithms
#'
#' @param algorithm algorithms array
#'
#' @return array with the measures we're going to run
#'
#' @keywords internal
#'

measure_calculate <- function(algorithm) {
  result <- vector()

  for (iterate in 1:length(algorithm)) {

    if (tolower(algorithm[iterate]) == tolower(CONST_HCLUST)) {
      result <- c(result, CONST_HCLUST_EUCLIDEAN)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_APCLUSTERK)) {
      result <- c(result, CONST_APCLUSTERK_EUCLIDEAN)
      result <- c(result, CONST_APCLUSTERK_MANHATTAN)
      result <- c(result, CONST_APCLUSTERK_MINKOWSKI)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_AGNES)) {
      result <- c(result, CONST_AGNES_EUCLIDEAN)
      result <- c(result, CONST_AGNES_MANHATTAN)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_CLARA)) {
      result <- c(result, CONST_CLARA_EUCLIDEAN)
      result <- c(result, CONST_CLARA_MANHATTAN)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_DAISY)) {
      result <- c(result, CONST_DAISY_MANHATTAN)
      result <- c(result, CONST_DAISY_GOWER)
      result <- c(result, CONST_DAISY_EUCLIDEAN)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_DIANA)) {
      result <- c(result, CONST_DIANA_EUCLIDEAN)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_FANNY)) {
      result <- c(result, CONST_FANNY_EUCLIDEAN)
      result <- c(result, CONST_FANNY_MANHATTAN)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_MONA)) {
      result <- c(result, CONST_MONA)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_PAM)) {
      result <- c(result, CONST_PAM_EUCLIDEAN)
      result <- c(result, CONST_PAM_MANHATTAN)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_GMM)) {
      result <- c(result, CONST_GMM_EUCLIDEAN)
      result <- c(result, CONST_GMM_MANHATTAN)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_KMEANS_ARMA)) {
      result <- c(result, CONST_KMEANS_ARMA)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_KMEANS_RCPP)) {
      result <- c(result, CONST_KMEANS_RCPP)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_MINI_KMEANS)) {
      result <- c(result, CONST_MINI_KMEANS)
    }

    if (tolower(algorithm[iterate]) == tolower(CONST_PVCLUST)) {
      result <- c(result, CONST_PVCLUST_EUCLIDEAN)
      result <- c(result, CONST_PVLCUST_CORRELATION)
    }
  }


  return(result)
}


#' Method that returns all the measures executed by the package
#'
#' @param package package array
#'
#' @return array with the measures we're going to run
#'
#' @keywords internal
#'

measure_package <- function(package) {
  result <- vector()

  if (is.null(package)) {
    # All measures
    result <- c(result, measure_amap())
    result <- c(result, measure_apcluster())
    result <- c(result, measure_cluster())
    result <- c(result, measure_clusterr())
    result <- c(result, measure_pvclust())
  } else {
    for (iterate in 1:length(package)) {

      if (tolower(package[iterate]) == CONST_ALGORITHM_AMAP) {
        result <- c(result, measure_amap())
      }

      if (tolower(package[iterate]) == CONST_ALGORITHM_APCLUSTER) {
        result <- c(result, measure_apcluster())
      }

      if (tolower(package[iterate]) == CONST_ALGORITHM_CLUSTER) {
        result <- c(result, measure_cluster())
      }

      if (tolower(package[iterate]) == CONST_ALGORITHM_CLUSTERR) {
        result <- c(result, measure_clusterr())
      }

      if (tolower(package[iterate]) == CONST_ALGORITHM_PVCLUST) {
        result <- c(result, measure_pvclust())
      }
    }
  }

  return(result)

}


#' Method in charge of verifying the implemented metrics
#'
#' @param metrics array with the metrics used in the execution of the package
#'
#' @param variables boolean field that indicates if it should show the results
#' of the variables
#'
#' @return list of metrics
#'
#' @keywords internal
#'

metrics_calculate <- function(metrics,variables,internal,external) {
  result <- vector()

  if (is.null(metrics)) {
    # All metrics
    if (variables){
      result <-
        c(
          CONST_TIME_EXTERNAL,
          CONST_ENTROPY_METRIC,
          CONST_VARIATION_INFORMATION_METRIC,
          CONST_PRECISION_METRIC,
          CONST_RECALL_METRIC,
          CONST_F_MEASURE_METRIC,
          CONST_FOWLKES_MALLOWS_INDEX_METRIC,
          CONST_TIME_INTERNAL,
          CONST_CONNECTIVITY_METRIC,
          CONST_DUNN_METRIC,
          CONST_SILHOUETTE_METRIC,
          CONST_TIME_EXTERNAL_ATTR,
          CONST_ENTROPY_METRIC_ATTR,
          CONST_VARIATION_INFORMATION_METRIC_ATTR,
          CONST_PRECISION_METRIC_ATTR,
          CONST_RECALL_METRIC_ATTR,
          CONST_F_MEASURE_METRIC_ATTR,
          CONST_FOWLKES_MALLOWS_INDEX_METRIC_ATTR,
          CONST_TIME_INTERNAL_ATTR,
          CONST_CONNECTIVITY_METRIC_ATTR,
          CONST_DUNN_METRIC_ATTR,
          CONST_SILHOUETTE_METRIC_ATTR
        )
    } else {
      result <-
        c(
          CONST_TIME_EXTERNAL,
          CONST_ENTROPY_METRIC,
          CONST_VARIATION_INFORMATION_METRIC,
          CONST_PRECISION_METRIC,
          CONST_RECALL_METRIC,
          CONST_F_MEASURE_METRIC,
          CONST_FOWLKES_MALLOWS_INDEX_METRIC,
          CONST_TIME_INTERNAL,
          CONST_CONNECTIVITY_METRIC,
          CONST_DUNN_METRIC,
          CONST_SILHOUETTE_METRIC
        )
    }

  } else {

    if(external) {
      result <- c(result, CONST_TIME_EXTERNAL)
    }

    exitsInternal <- 0;

    if (length(metrics) > 0){

      if (tolower(CONST_ENTROPY_METRIC) %in% tolower(metrics)) {
        result <- c(result, CONST_ENTROPY_METRIC)
      }

      if (tolower(CONST_VARIATION_INFORMATION_METRIC) %in% tolower(metrics)) {
        result <- c(result, CONST_VARIATION_INFORMATION_METRIC)
      }

      if (tolower(CONST_PRECISION_METRIC)  %in% tolower(metrics)) {
        result <- c(result, CONST_PRECISION_METRIC)
      }

      if (tolower(CONST_RECALL_METRIC)  %in% tolower(metrics)) {
        result <- c(result, CONST_RECALL_METRIC)
      }

      if (tolower(CONST_F_MEASURE_METRIC)  %in% tolower(metrics)) {
        result <- c(result, CONST_F_MEASURE_METRIC)
      }

      if (tolower(CONST_FOWLKES_MALLOWS_INDEX_METRIC)  %in% tolower(metrics)) {
        result <- c(result, CONST_FOWLKES_MALLOWS_INDEX_METRIC)
      }

      if (tolower(CONST_CONNECTIVITY_METRIC) %in% tolower(metrics)) {
        if (exitsInternal == 0) {
          result <- c(result, CONST_TIME_INTERNAL)
          exitsInternal = 1
        }
        result <- c(result, CONST_CONNECTIVITY_METRIC)
      }

      if (tolower(CONST_DUNN_METRIC) %in% tolower(metrics)) {
        if (exitsInternal == 0) {
          result <- c(result, CONST_TIME_INTERNAL)
          exitsInternal = 1
        }
        result <- c(result, CONST_DUNN_METRIC)
      }

      if (tolower(CONST_SILHOUETTE_METRIC) %in% tolower(metrics)) {
        if (exitsInternal == 0) {
          result <- c(result, CONST_TIME_INTERNAL)
          exitsInternal = 1
        }
        result <- c(result, CONST_SILHOUETTE_METRIC)
      }

    }

    if (variables) {

      if(external) {
        result <- c(result, CONST_TIME_EXTERNAL_ATTR)
      }

      exitsInternal <- 0;

      if (length(metrics) > 0){

        if (tolower(CONST_ENTROPY_METRIC) %in% tolower(metrics)) {
          result <- c(result, CONST_ENTROPY_METRIC_ATTR)
        }

        if (tolower(CONST_VARIATION_INFORMATION_METRIC) %in% tolower(metrics)) {
          result <- c(result, CONST_VARIATION_INFORMATION_METRIC_ATTR)
        }

        if (tolower(CONST_PRECISION_METRIC)  %in% tolower(metrics)) {
          result <- c(result, CONST_PRECISION_METRIC_ATTR)
        }

        if (tolower(CONST_RECALL_METRIC)  %in% tolower(metrics)) {
          result <- c(result, CONST_RECALL_METRIC_ATTR)
        }

        if (tolower(CONST_F_MEASURE_METRIC)  %in% tolower(metrics)) {
          result <- c(result, CONST_F_MEASURE_METRIC_ATTR)
        }

        if (tolower(CONST_FOWLKES_MALLOWS_INDEX_METRIC)  %in% tolower(metrics)) {
          result <- c(result, CONST_FOWLKES_MALLOWS_INDEX_METRIC_ATTR)
        }

        if (tolower(CONST_CONNECTIVITY_METRIC) %in% tolower(metrics)) {
          if (exitsInternal == 0) {
            result <- c(result, CONST_TIME_INTERNAL_ATTR)
            exitsInternal = 1
          }
          result <- c(result, CONST_CONNECTIVITY_METRIC_ATTR)
        }

        if (tolower(CONST_DUNN_METRIC) %in% tolower(metrics)) {
          if (exitsInternal == 0) {
            result <- c(result, CONST_TIME_INTERNAL_ATTR)
            exitsInternal = 1
          }
          result <- c(result, CONST_DUNN_METRIC_ATTR)
        }

        if (tolower(CONST_SILHOUETTE_METRIC) %in% tolower(metrics)) {
          if (exitsInternal == 0) {
            result <- c(result, CONST_TIME_INTERNAL_ATTR)
            exitsInternal = 1
          }
          result <- c(result, CONST_SILHOUETTE_METRIC_ATTR)
        }
      }
    }

  }

  return (result)

}

#' Metrics of the amap algorithm
#'
#' @return list with the metrics
#'
#' @keywords internal
#'

measure_amap <- function() {
  return(c(CONST_HCLUST_EUCLIDEAN))

}

#' Metrics of the apcluster algorithm
#'
#' @return list with the metrics
#'
#' @keywords internal
#'

measure_apcluster <- function() {
  return(c(
    CONST_APCLUSTERK_EUCLIDEAN,
    CONST_APCLUSTERK_MANHATTAN,
    CONST_APCLUSTERK_MINKOWSKI
  ))

}

#' Metrics of the cluster algorithm
#'
#' @return list with the metrics
#'
#' @keywords internal
#'

measure_cluster <- function() {
  return(
    c(
      CONST_AGNES_EUCLIDEAN,
      CONST_AGNES_MANHATTAN,
      CONST_CLARA_EUCLIDEAN,
      CONST_CLARA_MANHATTAN,
      CONST_DAISY_MANHATTAN,
      CONST_DAISY_GOWER,
      CONST_DAISY_EUCLIDEAN,
      CONST_DIANA_EUCLIDEAN,
      CONST_FANNY_EUCLIDEAN,
      CONST_FANNY_MANHATTAN,
      CONST_PAM_EUCLIDEAN,
      CONST_PAM_MANHATTAN,
      CONST_MONA
    )
  )

}

#' Metrics of the ClusterR algorithm
#'
#' @return list with the metrics
#'
#' @keywords internal
#'

measure_clusterr <- function() {
  return(c(
    CONST_GMM_EUCLIDEAN,
    CONST_GMM_MANHATTAN,
    CONST_KMEANS_ARMA,
    CONST_KMEANS_RCPP,
    CONST_MINI_KMEANS
  ))

}

#' Metrics of the pvclust algorithm
#'
#' @return list with the metrics
#'
#' @keywords internal
#'

measure_pvclust <- function() {
  return(c(CONST_PVCLUST_EUCLIDEAN,
           CONST_PVLCUST_CORRELATION))

}

#' amap package algorithms
#'
#' @return list with the algorithms
#'
#' @keywords internal
#'

algorithm_amap <- function() {
  return(c(CONST_HCLUST))

}

#' apcluster package algorithms
#'
#' @return list with the algorithms
#'
#' @keywords internal
#'

algorithm_apcluster <- function() {
  return(c(CONST_APCLUSTERK))

}

#' cluster package algorithms
#'
#' @return list with the algorithms
#'
#' @keywords internal
#'

algorithm_cluster <- function() {
  return(c(CONST_AGNES,
           CONST_CLARA,
           CONST_DAISY,
           CONST_DIANA,
           CONST_FANNY,
           CONST_MONA,
           CONST_PAM))

}

#' ClusterR package algorithms
#'
#' @return list with the algorithms
#'
#' @keywords internal
#'

algorithm_clusterr <- function() {
  return(c(CONST_GMM,
           CONST_KMEANS_ARMA,
           CONST_KMEANS_RCPP,
           CONST_MINI_KMEANS))

}

#' pvclust package algorithms
#'
#' @return list with the algorithms
#'
#' @keywords internal
#'

algorithm_pvclust <- function() {
  return(c(CONST_ALGORITHM_PVCLUST))

}
