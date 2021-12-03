#'
#' Method that runs the mona algorithm using external or internal validation of the cluster.
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

mona_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  on.exit(options(warn = -1))

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  mona <- tryCatch({
    mona(x = dt)
  },

  error = function(cond) {
    return(NULL)
  })

  if (!is.null(mona)) {
    ev_mona <- tryCatch({
      external_validation(c(dt[, columnClass]),
                          cutree(mona$order, k = clusters),metric)
    },

    error = function(cond) {
      return(NULL)
    })

    iv_mona <- tryCatch({
      internal_validation(
        distance = NULL,
        clusters_vector = cutree(mona$order, k = clusters),
        dataf = dt,
        method = "",
        metric
      )
    },

    error = function(cond) {
      return(NULL)
    })

  } else {
    ev_mona = initializeExternalValidation()
    iv_mona = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_mona$time = time - iv_mona$time
  iv_mona$time = time - ev_mona$time

  result = list("external" = ev_mona,
                "internal" = iv_mona)

  return (result)

}
