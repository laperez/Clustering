
#'
#' Method that runs the aggExcluster algorithm using the Euclidean metric to
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

aggExCluster_euclidean = function(dt, clusters, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  aggExCluster_euclidean <- tryCatch({
    aggExCluster(s = negDistMat(r = CONST_TWO, method = CONST_EUCLIDEAN),
                            x = dt)
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
        dataf = dt,
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
