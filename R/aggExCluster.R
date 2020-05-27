
#' Method that runs the aggExcluster algorithm using the Euclidean metric to make an external or internal validation of the cluster
#'
#' @param data matrix or data frame
#' @param clusters number of clusters
#' @param metric metrics avalaible in the package. The metrics implemented are: entropy, variation_information,precision,recall,f_measure,fowlkes_mallows_index,connectivity,dunn,silhouette.
#'
#' @return returns a list with both the internal and external evaluation of the grouping
#'
#' @keywords internal

aggExCluster_euclidean = function(data, clusters, metric) {
  start.time <- Sys.time()


  if ('data.frame' %in% class(data))
    data = as.matrix(data)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  aggExCluster_euclidean <- tryCatch({
    aggExCluster(s = negDistMat(r = CONST_TWO, method = CONST_EUCLIDEAN),
                            x = data)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(aggExCluster_euclidean)) {
    ev_aggExCluster_euclidean <-
      tryCatch({
        external_validation(
          aggExCluster_euclidean@order,
          cutree(tree = aggExCluster_euclidean, k = clusters),metric
        )
      },

      error = function(cond) {
        ev_aggExCluster_euclidean = initializeExternalValidation()
      })

    iv_aggExCluster_euclidean <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = cutree(tree = aggExCluster_euclidean, k = clusters),
        data = data,
        method = CONST_EUCLIDEAN,
        metric
      )

    },

    error = function(cond) {
      iv_aggExCluster_euclidean = initializeInternalValidation()
    })

  } else {
    ev_aggExCluster_euclidean = initializeExternalValidation()
    iv_aggExCluster_euclidean = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_aggExCluster_euclidean$time = time - iv_aggExCluster_euclidean$time
  iv_aggExCluster_euclidean$time = time - ev_aggExCluster_euclidean$time

  result = list("external" = ev_aggExCluster_euclidean,
                "internal" = iv_aggExCluster_euclidean)

  return (result)

}
