#'
#' Method that runs the pvpick algorithm using an external or internal validation of the cluster.
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

pvpick_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  pvpick <- tryCatch({
    pvpick(x = dt)
  },

  error = function(cond) {
    return(NULL)
  })

  if (!is.null(pvpick)) {
    ev_pvpick <- tryCatch({
      external_validation(c(dt[, columnClass]),
                          pvpick$clusters[[1]],metric)

    }, error = function(cond) {
      ev_pvpick = initializeExternalValidation()
    })

    iv_pvpick <- tryCatch({
      internal_validation(
        distance = NULL,
        clusters_vector = pvpick$clusters[[1]],
        dataf = dt,
        method = CONST_PEARSON_CORRELATION,
        metric
      )
    }, error = function(cond) {
      iv_pvpick = initializeInternalValidation()
    })

  } else {
    ev_pvpick = initializeExternalValidation()
    iv_pvpick = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_pvpick$time = time - iv_pvpick$time
  iv_pvpick$time = time - ev_pvpick$time

  result = list("external" = ev_pvpick,
                "internal" = iv_pvpick)

  return (result)
}
