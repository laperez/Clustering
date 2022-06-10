#'
#' Method that runs the kmeans_rcpp algorithm using the Euclidean metric to
#' make an external or internal validation of the cluster.
#'
#' @param dt Matrix or data frame with the set of values to be applied to the
#' algorithm.
#'
#' @param clusters It's an integer that indexes the number of clusters we want to
#' create.
#'
#' @param metric It's a characters vector with the metrics avalaible in the
#' package. The metrics implemented are: Entropy, Variation_information,
#' Precision,Recall,F_measure,Fowlkes_mallows_index,Connectivity,Dunn,
#' Silhouette.
#'
#' @return Return a list with both the internal and external evaluation of the
#' grouping.
#'
#' @keywords internal
#'

kmeans_rcpp_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters), 1, 0)

  if (sum(numeric_cluster) > 0)
    stop('The field clusters must be a numeric')

  kmeans_rcpp <- tryCatch({
    KMeans_rcpp(data = dt, clusters = clusters)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(kmeans_rcpp)) {
    ev_kmeans_rcpp <-
      tryCatch({
        external_validation(
          column_dataset_label = c(dt[, columnClass]),
          clusters_vector = kmeans_rcpp$clusters,
          metric
        )
      },

      error = function(cond) {
        ev_kmeans_rcpp = initializeExternalValidation()
      })

    iv_kmeans_rcpp <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = kmeans_rcpp$clusters,
        dataf = dt,
        method = CONST_EUCLIDEAN,
        metric
      )
    },

    error = function(cond) {
      iv_kmeans_rcpp = initializeInternalValidation()
    })

  } else {
    ev_kmeans_rcpp = initializeExternalValidation()
    iv_kmeans_rcpp = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_kmeans_rcpp$time = time - iv_kmeans_rcpp$time
  iv_kmeans_rcpp$time = time - ev_kmeans_rcpp$time

  result = list("external" = ev_kmeans_rcpp,
                "internal" = iv_kmeans_rcpp)

  return (result)

}

#'
#' Method that runs the kmeans_arma algorithm using the Euclidean metric to
#' make an external or internal validation of the cluster.
#'
#' @param dt Matrix or data frame with the set of values to be applied to the
#' algorithm.
#'
#' @param clusters It's an integer that indexes the number of clusters we want to
#' create.
#'
#' @param metric It's a characters vector with the metrics avalaible in the
#' package. The metrics implemented are: Entropy, Variation_information,
#' Precision,Recall,F_measure,Fowlkes_mallows_index,Connectivity,Dunn,
#' Silhouette.
#'
#' @return Return a list with both the internal and external evaluation of the
#' grouping.
#'
#' @keywords internal
#'


kmeans_arma_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters), 1, 0)

  if (sum(numeric_cluster) > 0)
    stop('The field clusters must be a numeric')

  kmeans_arma <- tryCatch({
    KMeans_arma(data = dt, clusters = clusters)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(kmeans_arma)) {
    pr_kmeans_arma <- tryCatch({
      predict_KMeans(data = dt, CENTROIDS = kmeans_arma)
    },

    error = function(cond) {
      return(CONST_NULL)
    })

    if (!is.null(pr_kmeans_arma)) {
      ev_kmeans_arma <-
        tryCatch({
          external_validation(
            column_dataset_label = c(dt[, columnClass]),
            clusters_vector = as.vector(pr_kmeans_arma, metric)
          )
        },

        error = function(cond) {
          ev_kmeans_arma = initializeExternalValidation()
        })

      iv_kmeans_arma <- tryCatch({
        internal_validation(
          distance = CONST_NULL,
          clusters_vector = as.vector(pr_kmeans_arma),
          dataf = dt,
          method = CONST_EUCLIDEAN,
          metric
        )
      },

      error = function(cond) {
        iv_kmeans_arma = initializeInternalValidation()
      })
    } else {
      ev_kmeans_arma = initializeExternalValidation()

      iv_kmeans_arma = initializeInternalValidation()
    }

  } else {
    ev_kmeans_arma = initializeExternalValidation()

    iv_kmeans_arma = initializeInternalValidation()

  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_kmeans_arma$time = time - iv_kmeans_arma$time
  iv_kmeans_arma$time = time - ev_kmeans_arma$time

  result = list("external" = ev_kmeans_arma,
                "internal" = iv_kmeans_arma)

  return (result)

}

#'
#' Method that runs the mini_kmeans algorithm using the Euclidean metric to
#' make an external or internal validation of the cluster.
#'
#' @param dt Matrix or data frame with the set of values to be applied to the
#' algorithm.
#'
#' @param clusters It's an integer that indexes the number of clusters we want to
#' create.
#'
#' @param metric It's a characters vector with the metrics avalaible in the
#' package. The metrics implemented are: Entropy, Variation_information,
#' Precision,Recall,F_measure,Fowlkes_mallows_index,Connectivity,Dunn,
#' Silhouette.
#'
#' @return Return a list with both the internal and external evaluation of the
#' grouping.
#'
#' @keywords internal
#'

mini_kmeans_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters), 1, 0)

  if (sum(numeric_cluster) > 0)
    stop('The field clusters must be a numeric')

  mini_kmeans <- tryCatch({
    MiniBatchKmeans(data = dt, clusters = clusters)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(mini_kmeans)) {
    pr_mini_kmeans <-
      tryCatch({
        predict_MBatchKMeans(data = dt, CENTROIDS = mini_kmeans$centroids)

      },

      error = function(cond) {
        return(CONST_NULL)
      })

    if (!is.null(pr_mini_kmeans)) {
      ev_mini_kmeans <-
        tryCatch({
          external_validation(
            column_dataset_label = c(dt[, columnClass]),
            clusters_vector = as.vector(pr_mini_kmeans, metric)
          )
        },

        error = function(cond) {
          ev_mini_kmeans = initializeExternalValidation()
        })

      iv_mini_kmeans <- tryCatch({
        internal_validation(
          distance = CONST_NULL,
          clusters_vector = as.vector(pr_mini_kmeans),
          dataf = dt,
          method = CONST_EUCLIDEAN,
          metric
        )
      },

      error = function(cond) {
        iv_mini_kmeans = initializeInternalValidation()
      })
    } else{
      ev_mini_kmeans = initializeExternalValidation()
      iv_mini_kmeans = initializeInternalValidation()
    }

  } else {
    ev_mini_kmeans = initializeExternalValidation()
    iv_mini_kmeans = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_mini_kmeans$time = time - iv_mini_kmeans$time
  iv_mini_kmeans$time = time - ev_mini_kmeans$time

  result = list("external" = ev_mini_kmeans,
                "internal" = iv_mini_kmeans)

  return (result)
}
