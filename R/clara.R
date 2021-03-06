#' Method that runs the clara algorithm using the euclidean metric to make an
#' external or internal validation of the cluster.
#'
#' @param dt matrix or data frame with the set of values to be applied to the
#' algorithm.
#' @param clusters is an integer that indexes the number of clusters we want to
#' create.
#' @param columnClass is an integer with the number of columns, for example if a
#'  dataset has five column, we can select column four to calculate validation.
#' @param metric is a characters vector with the metrics avalaible in the
#' package. The metrics implemented are: entropy, variation_information,
#' precision,recall,f_measure,fowlkes_mallows_index,connectivity,dunn,
#' silhouette.
#'
#' @return returns a list with both the internal and external evaluation of the
#' grouping.
#'
#' @keywords internal
#'

clara_euclidean_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  on.exit(options(warn = -1))

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters), 1, 0)

  if (sum(numeric_cluster) > 0)
    stop('The field clusters must be a numeric')

  clara_euclidean <- tryCatch({
    clara(x = dt, k = clusters, metric = 'euclidean')
  },

  error = function(cond) {
    return(NULL)
  })

  if (!is.null(clara_euclidean)) {
    ev_clara_euclidean <-
      tryCatch({
        external_validation(c(dt[, columnClass]),
                            clara_euclidean$clustering, metric)

      },

      error = function(cond) {
        ev_clara_euclidean = initializeExternalValidation()
      })

    iv_clara_euclidean <- tryCatch({
      internal_validation(
        distance = NULL,
        clusters_vector = clara_euclidean$clustering,
        dataf = dt,
        method = "euclidean",
        metric
      )
    },

    error = function(cond) {
      iv_clara_euclidean = initializeInternalValidation()
    })


  } else {
    ev_clara_euclidean = initializeExternalValidation()
    iv_clara_euclidean = initializeInternalValidation()

  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_clara_euclidean$time = time - iv_clara_euclidean$time
  iv_clara_euclidean$time = time - ev_clara_euclidean$time

  result = list("external" = ev_clara_euclidean,
                "internal" = iv_clara_euclidean)

  return (result)
}

#' Method that runs the clara algorithm using the manhattan metric to make an
#' external or internal validation of the cluster.
#'
#' @param dt matrix or data frame with the set of values to be applied to the
#' algorithm.
#' @param clusters is an integer that indexes the number of clusters we want to
#' create.
#' @param columnClass is an integer with the number of columns, for example if a
#' dataset has five column, we can select column four to calculate validation.
#' @param metric is a characters vector with the metrics avalaible in the
#' package. The metrics implemented are: entropy, variation_information,
#' precision,recall,f_measure,fowlkes_mallows_index,connectivity,dunn,
#' silhouette.
#'
#' @return returns a list with both the internal and external evaluation of the
#' grouping.
#'
#' @keywords internal
#'

clara_manhattan_method  = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  on.exit(options(warn = -1))

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters), 1, 0)

  if (sum(numeric_cluster) > 0)
    stop('The field clusters must be a numeric')

  clara_manhattan <- tryCatch({
    clara(x = dt, k = clusters, metric = 'manhattan')
  },

  error = function(cond) {
    return(NULL)
  })

  if (!is.null(clara_manhattan)) {
    ev_clara_manhattan <-
      tryCatch({
        external_validation(c(dt[, columnClass]),
                            clara_manhattan$clustering, metric)

      },

      error = function(cond) {
        ev_clara_manhattan = initializeExternalValidation()
      })

    iv_clara_manhattan <- tryCatch({
      internal_validation(
        distance = NULL,
        clusters_vector = clara_manhattan$clustering,
        dataf = dt,
        method = "manhattan",
        metric
      )

    },

    error = function(cond) {
      iv_clara_manhattan = initializeInternalValidation()
    })

  } else {
    ev_clara_manhattan = initializeExternalValidation()
    iv_clara_manhattan = initializeInternalValidation()

  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_clara_manhattan$time = time - iv_clara_manhattan$time
  iv_clara_manhattan$time = time - ev_clara_manhattan$time

  result = list("external" = ev_clara_manhattan,
                "internal" = iv_clara_manhattan)

  return (result)
}
