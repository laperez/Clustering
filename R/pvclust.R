#'
#' Method that runs the pvclust algorithm using the Euclidean metric to
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

pvclust_euclidean_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  pvclust_euclidean <- tryCatch({
    pvclust(data = dt, method.dist = CONST_EUCLIDEAN)
  },

  error = function(cond) {
    return(NULL)
  })

  if (!is.null(pvclust_euclidean)) {
    ev_pvclust_euclidean <- tryCatch({
      external_validation(
        pvclust_euclidean$hclust$order,
        cutree(pvclust_euclidean$hclust, k = clusters),metric
      )
    },

    error = function(cond) {
      ev_pvclust_euclidean = initializeExternalValidation()
    })

    iv_pvclust_euclidean <- tryCatch({
      internal_validation(
        distance = NULL,
        clusters_vector = cutree(pvclust_euclidean$hclust, k = clusters),
        dataf = dt,
        method = CONST_EUCLIDEAN,
        metric
      )
    },

    error = function(cond) {
      iv_pvclust_euclidean = initializeInternalValidation()
    })

  } else {
    ev_pvclust_euclidean = initializeExternalValidation()
    iv_pvclust_euclidean = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_pvclust_euclidean$time = time - iv_pvclust_euclidean$time
  iv_pvclust_euclidean$time = time - ev_pvclust_euclidean$time

  result = list("external" = ev_pvclust_euclidean,
                "internal" = iv_pvclust_euclidean)

  return (result)

}


#'
#' Method that runs the pvclust algorithm using the Correlation metric to
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

pvclust_correlation_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  pvclust_correlation <- tryCatch({
    pvclust(data = dt, method.dist = CONST_CORRELATION)
  },

  error = function(cond) {
    return(NULL)
  })

  if (!is.null(pvclust_correlation)) {
    ev_pvclust_correlation <- tryCatch({
      external_validation(
        pvclust_correlation$hclust$order,
        cutree(pvclust_correlation$hclust, k = clusters),
        metric
      )
    }, error = function(cond) {
      ev_pvclust_correlation = initializeExternalValidation()
    })

    iv_pvclust_correlation <- tryCatch({
      internal_validation(
        distance = NULL,
        clusters_vector = cutree(pvclust_correlation$hclust, k = clusters),
        dataf = dt,
        method = CONST_PEARSON_CORRELATION,
        metric
      )
    }, error = function(cond) {
      iv_pvclust_correlation = initializeInternalValidation()
    })


  } else {
    ev_pvclust_correlation = initializeExternalValidation()
    iv_pvclust_correlation = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_pvclust_correlation$time = time - iv_pvclust_correlation$time
  iv_pvclust_correlation$time = time - ev_pvclust_correlation$time

  result = list("external" = ev_pvclust_correlation,
                "internal" = iv_pvclust_correlation)

  return (result)

}
