#'
#' Method that runs the pam algorithm using the Euclidean metric to
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

pam_euclidean_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  on.exit(options(warn = -1))

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  pam_euclidean <- tryCatch({
    pam(x = dt, k = clusters, metric = CONST_EUCLIDEAN)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(pam_euclidean)) {
    ev_pam_euclidean <-
      tryCatch({
        external_validation(c(dt[, columnClass]),
                            pam_euclidean$clustering,metric)

      },

      error = function(cond) {
        ev_pam_euclidean = initializeExternalValidation()
      })

    iv_pam_euclidean <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = pam_euclidean$clustering,
        dataf = dt,
        method = CONST_EUCLIDEAN,
        metric
      )

    },

    error = function(cond) {
      ev_pam_euclidean = initializeExternalValidation()
    })

  } else {
    ev_pam_euclidean = initializeExternalValidation()
    iv_pam_euclidean = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_pam_euclidean$time = time - iv_pam_euclidean$time
  iv_pam_euclidean$time = time - ev_pam_euclidean$time

  result = list("external" = ev_pam_euclidean,
                "internal" = iv_pam_euclidean)

  return (result)

}

#'
#' Method that runs the pam algorithm using the Manhattan metric to
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

pam_manhattan_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  on.exit(options(warn = -1))

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  pam_manhattan <- tryCatch({
    pam(x = dt, k = clusters, metric = CONST_MANHATTAN)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(pam_manhattan)) {
    ev_pam_manhattan <-
      tryCatch({
        external_validation(c(dt[, columnClass]),
                            pam_manhattan$clustering,metric)
      },

      error = function(cond) {
        ev_pam_manhattan = initializeExternalValidation()
      })

    iv_pam_manhattan <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = pam_manhattan$clustering,
        dataf = dt,
        method = CONST_MANHATTAN,
        metric
      )

    },

    error = function(cond) {
      iv_pam_manhattan = initializeInternalValidation()
    })


  } else {
    ev_pam_manhattan = initializeExternalValidation()
    iv_pam_manhattan = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_pam_manhattan$time = time - iv_pam_manhattan$time
  iv_pam_manhattan$time = time - ev_pam_manhattan$time

  result = list("external" = ev_pam_manhattan,
                "internal" = iv_pam_manhattan)

  return (result)
}
