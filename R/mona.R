#' Method that runs the mona algorithm to make an external or internal
#' validation of the cluster.
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

mona_method = function(dt, clusters, columnClass, metric) {
  start.time <- Sys.time()

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
