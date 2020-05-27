#' Method that runs the gama hcluster using the Euclidean metric to make an external or internal validation of the cluster
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

hclust_euclidean = function(data, clusters, columnClass, metric) {
  start.time <- Sys.time()

  if ('data.frame' %in% class(data))
    data = as.matrix(data)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  hclust_euclidean <- tryCatch({
    hcluster(
      x = as.matrix(data),
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
          data = data,
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
