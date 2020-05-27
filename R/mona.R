#' Method that runs the mona algorithm to make an external or internal validation of the cluster
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

mona_method = function(data, clusters, columnClass, metric) {
  start.time <- Sys.time()

  if ('data.frame' %in% class(data))
    data = as.matrix(data)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  mona <- tryCatch({
    mona(x = data)
  },

  error = function(cond) {
    return(NULL)
  })

  if (!is.null(mona)) {
    ev_mona <- tryCatch({
      external_validation(c(data[, columnClass]),
                          cutree(mona$order, k = clusters),metric)
    },

    error = function(cond) {
      return(NULL)
    })

    iv_mona <- tryCatch({
      internal_validation(
        distance = NULL,
        clusters_vector = cutree(mona$order, k = clusters),
        data = data,
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
