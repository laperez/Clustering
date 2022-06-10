#'
#' Method that runs the hcluster algorithm using the Euclidean metric to
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

hclust_euclidean = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  hclust_euclidean <- tryCatch({
    hcluster(
      x = as.matrix(dt),
      method = CONST_EUCLIDEAN,
      diag = TRUE,
      upper = TRUE
    )
  },

  error = function(cond) {
    return(NULL)
  })

  if (!is.null(hclust_euclidean)) {
    if (!is.null(hclust_euclidean)) {
      ev_hclust_euclidean <-
        tryCatch({
          external_validation(hclust_euclidean$order,
                              cutree(hclust_euclidean, k = clusters),metric)

        },

        error = function(cond) {
          ev_hclust_euclidean = initializeExternalValidation()
        })

      iv_hclust_euclidean <- tryCatch({
        internal_validation(
          distance = NULL,
          clusters_vector = cutree(hclust_euclidean, k = clusters),
          dataf = dt,
          method = CONST_EUCLIDEAN,
          metric
        )

      },

      error = function(cond) {
        iv_hclust_euclidean = initializeInternalValidation()
      })

    } else {
      ev_hclust_euclidean = initializeExternalValidation()
      iv_hclust_euclidean = initializeInternalValidation()
    }


  } else {
    ev_hclust_euclidean = initializeExternalValidation()
    iv_hclust_euclidean = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_hclust_euclidean$time = time - iv_hclust_euclidean$time
  iv_hclust_euclidean$time = time - ev_hclust_euclidean$time

  result = list("external" = ev_hclust_euclidean,
                "internal" = iv_hclust_euclidean)

  return (result)

}
