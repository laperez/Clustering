#' Method that runs the gama algorithm using the Euclidean metric to make an external or internal validation of the cluster.
#'
#' @param dt matrix or data frame with the set of values to be applied to the algorithm.
#' @param clusters is an integer that indexes the number of clusters we want to create.
#' @param columnClass is an integer with the number of columns, for example if a dataset has five column,
#' we can select column four to calculate alidation.
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

gama = function(dt, clusters, columnClass, metric) {
  start.time <- Sys.time()

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
