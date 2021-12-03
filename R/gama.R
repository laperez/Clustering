#'
#' Method that runs the gama algorithm using the Euclidean metric to
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

gama = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  on.exit(options(warn = -1))

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  gama <- tryCatch({
    gama(dataset = as.data.frame(dt), k = clusters)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(gama)) {
    ev_gama <-
      tryCatch({
        external_validation(c(dt[, columnClass]),
                            gama@cluster,metric)

      },

      error = function(cond) {
        ev_gama = initializeExternalValidation()
      })

    iv_gama <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = gama@cluster,
        dataf = dt,
        method = CONST_EUCLIDEAN,
        metric
      )

    },

    error = function(cond) {
      iv_gama = initializeInternalValidation()

    })

  } else {
    ev_gama = initializeExternalValidation()
    iv_gama = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_gama$time = time - iv_gama$time
  iv_gama$time = time - ev_gama$time

  result = list("external" = ev_gama,
                "internal" = iv_gama)

  return (result)
}
