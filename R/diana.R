#' Method that runs the diana algorithm using the euclidean metric to make an
#' external or internal validation of the cluster.
#'
#' @param dt matrix or data frame with the set of values to be applied to the
#' algorithm.
#' @param clusters is an integer that indexes the number of clusters we want to
#' create.
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

diana_euclidean_method = function(dt, clusters, metric) {
  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  diana_euclidean <- tryCatch({
    diana(x = dt,
                   metric = CONST_EUCLIDEAN,
                   stand = F)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(diana_euclidean)) {
    ev_diana_euclidean <-
      tryCatch({
        external_validation(diana_euclidean$order,
                            cutree(diana_euclidean, k = clusters),metric)

      },

      error = function(cond) {
        ev_diana_euclidean = initializeExternalValidation()
      })

    iv_diana_euclidean <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = cutree(diana_euclidean, k = clusters),
        dataf = dt,
        method = CONST_EUCLIDEAN,
        metric
      )

    },

    error = function(cond) {
      iv_diana_euclidean = initializeInternalValidation()
    })

  } else {
    ev_diana_euclidean = initializeExternalValidation()
    iv_diana_euclidean = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_diana_euclidean$time = time - iv_diana_euclidean$time
  iv_diana_euclidean$time = time - ev_diana_euclidean$time

  result = list("external" = ev_diana_euclidean,
                "internal" = iv_diana_euclidean)

  return (result)

}
