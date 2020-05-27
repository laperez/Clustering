#' Method that runs the pvpick algorithm to make an external or internal validation of the cluster
#'
#' @param data matrix or data frame
#' @param clusters number of clusters
#' @param columnClass number of column, for example if a dataset has five column,
#' we can select column four to calculate alidation
#' @param metric metrics avalaible in the package. The metrics implemented are: entropy, variation_information,precision,recall,f_measure,fowlkes_mallows_index,connectivity,dunn,silhouette.
#'
#' @return returns a list with both the internal and external evaluation of the grouping
#'
#' @keywords internal
#'

pvpick_method = function(data, clusters, columnClass, metric) {
  start.time <- Sys.time()

  if ('data.frame' %in% class(data))
    data = as.matrix(data)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  pvpick <- tryCatch({
    pvpick(x = data)
  },

  error = function(cond) {
    return(NULL)
  })

  if (!is.null(pvpick)) {
    ev_pvpick <- tryCatch({
      external_validation(c(data[, columnClass]),
                          pvpick$clusters[[1]],metric)

    }, error = function(cond) {
      ev_pvpick = initializeExternalValidation()
    })

    iv_pvpick <- tryCatch({
      internal_validation(
        distance = NULL,
        clusters_vector = pvpick$clusters[[1]],
        data = data,
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
