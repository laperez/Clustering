#'
#' Method that runs the fanny algorithm using the Euclidean metric to
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

fanny_euclidean_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  fanny_euclidean <- tryCatch({
    fanny(
      x = dt,
      k = clusters,
      metric = CONST_EUCLIDEAN,
      maxit = 100,
      trace.lev = 0
    )
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(fanny_euclidean)) {
    ev_fanny_euclidean <-
      tryCatch({
        external_validation(c(dt[, columnClass]),
                            fanny_euclidean$clustering,metric)

      },

      error = function(cond) {
        ev_fanny_euclidean = initializeExternalValidation()
      })

    iv_fanny_euclidean <- tryCatch({
      internal_validation(
        distance = as.vector(fanny_euclidean$diss),
        clusters_vector = fanny_euclidean$clustering,
        dataf = dt,
        method = CONST_EUCLIDEAN,
        metric
      )

    },

    error = function(cond) {
      iv_fanny_euclidean = initializeInternalValidation()

    })

  } else {
    ev_fanny_euclidean = initializeExternalValidation()
    iv_fanny_euclidean = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_fanny_euclidean$time = time - iv_fanny_euclidean$time
  iv_fanny_euclidean$time = time - ev_fanny_euclidean$time

  result = list("external" = ev_fanny_euclidean,
                "internal" = iv_fanny_euclidean)

  return (result)
}

#'
#' Method that runs the fanny algorithm using the Manhattan metric to
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

fanny_manhattan_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  fanny_manhattan <- tryCatch({
    fanny(
      x = dt,
      k = clusters,
      metric = CONST_MANHATTAN,
      maxit = 100,
      trace.lev = 0
    )
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(fanny_manhattan)) {
    ev_fanny_manhattan <-
      tryCatch({
        external_validation(c(dt[, columnClass]),
                            fanny_manhattan$clustering,metric)

      },

      error = function(cond) {
        ev_fanny_manhattan = initializeExternalValidation()
      })

    iv_fanny_manhattan <- tryCatch({
      internal_validation(
        distance = as.vector(fanny_manhattan$diss),
        clusters_vector = fanny_manhattan$clustering,
        dataf = dt,
        method = CONST_MANHATTAN,
        metric
      )

    },

    error = function(cond) {
      iv_fanny_manhattan = initializeInternalValidation()
    })

  } else {
    ev_fanny_manhattan = initializeExternalValidation()
    iv_fanny_manhattan = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_fanny_manhattan$time = time - iv_fanny_manhattan$time
  iv_fanny_manhattan$time = time - ev_fanny_manhattan$time

  result = list("external" = ev_fanny_manhattan,
                "internal" = iv_fanny_manhattan)

  return (result)
}
