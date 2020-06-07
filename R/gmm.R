#' Method that runs the GMM algorithm using the Euclidean metric to make an external or internal validation of the cluster.
#'
#' @param dt matrix or data frame with the set of values to be applied to the algorithm.
#' @param clusters is an integer that indexes the number of clusters we want to create.
#' @param columnClass is an integer with the number of columns, for example if a dataset has five column,
#' we can select column four to calculate alidation.
#' @param metric is a characters vector with the metrics avalaible in the package. The metrics implemented are: entropy, variation_information,precision,recall,f_measure,fowlkes_mallows_index,connectivity,dunn,silhouette.
#'
#' @return returns a list with both the internal and external evaluation of the grouping.
#'
#' @keywords internal
#'

gmm_euclidean_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  gmm_euclidean <- tryCatch({
    GMM(
      data = dt,
      km_iter = 10,
      dist_mode = CONST_EUCLIDEAN_DIST,
      seed_mode = CONST_RANDOM_SUBSET,
      gaussian_comps = clusters,
      em_iter = 50
    )
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(gmm_euclidean)) {
    pr_gmm_euclidean <- tryCatch({
      predict_GMM(
        dt,
        gmm_euclidean$centroids,
        gmm_euclidean$covariance_matrices,
        gmm_euclidean$weights
      )
    },

    error = function(cond) {
      return(CONST_NULL)
    })

    if (!is.null(pr_gmm_euclidean)) {
      ev_gmm_euclidean <-
        tryCatch({
          external_validation(
            column_dataset_label = c(dt[, columnClass]),
            clusters_vector = pr_gmm_euclidean$cluster_labels + 1,metric
          )
        },

        error = function(cond) {
          ev_gmm_euclidean = initializeExternalValidation()
        })

      iv_gmm_euclidean <- tryCatch({
        internal_validation(
          distance = CONST_NULL,
          clusters_vector = pr_gmm_euclidean$cluster_labels + 1,
          dataf = dt,
          method = CONST_EUCLIDEAN,
          metric
        )
      },

      error = function(cond) {
        iv_gmm_euclidean = initializeInternalValidation()
      })


    } else {
      ev_gmm_euclidean = initializeExternalValidation()
      iv_gmm_euclidean = initializeInternalValidation()
    }

  } else {
    ev_gmm_euclidean = initializeExternalValidation()
    iv_gmm_euclidean = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time


  ev_gmm_euclidean$time = time - iv_gmm_euclidean$time
  iv_gmm_euclidean$time = time - ev_gmm_euclidean$time

  result = list("external" = ev_gmm_euclidean,
                "internal" = iv_gmm_euclidean)

  return (result)

}

#' Method that runs the GMM algorithm using the Manhattan metric to make an external or internal validation of the cluster.
#'
#' @param dt matrix or data frame with the set of values to be applied to the algorithm.
#' @param clusters is an integer that indexes the number of clusters we want to create.
#' @param columnClass is an integer with the number of columns, for example if a dataset has five column,
#' we can select column four to calculate alidation.
#' @param metric is a characters vector with the metrics avalaible in the package. The metrics implemented are: entropy, variation_information,precision,recall,f_measure,fowlkes_mallows_index,connectivity,dunn,silhouette.
#'
#' @return returns a list with both the internal and external evaluation of the grouping.
#'
#' @keywords internal
#'

gmm_manhattan_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')


  gmm_manhattan <- tryCatch({
    GMM(
      data = dt,
      km_iter = 10,
      dist_mode = CONST_MANHATTAN_DIST,
      seed_mode = CONST_RANDOM_SUBSET,
      gaussian_comps = clusters
    )
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(gmm_manhattan)) {
    pr_gmm_manhattan <- tryCatch({
      predict_GMM(
        dt,
        gmm_manhattan$centroids,
        gmm_manhattan$covariance_matrices,
        gmm_manhattan$weights
      )

    },

    error = function(cond) {
      return(CONST_NULL)
    })

    if (!is.null(pr_gmm_manhattan)) {
      ev_gmm_manhattan <-
        tryCatch({
          external_validation(
            column_dataset_label = c(dt[, columnClass]),
            clusters_vector = pr_gmm_manhattan$cluster_labels + 1,metric
          )
        },

        error = function(cond) {
          ev_gmm_manhattan = initializeExternalValidation()
        })


      iv_gmm_manhattan <- tryCatch({
        internal_validation(
          distance = CONST_NULL,
          clusters_vector = pr_gmm_manhattan$cluster_labels + 1,
          dataf = dt,
          method = CONST_MANHATTAN,
          metric
        )
      },

      error = function(cond) {
        iv_gmm_manhattan = initializeInternalValidation()
      })

    } else {
      ev_gmm_manhattan = initializeExternalValidation()

      iv_gmm_manhattan = initializeInternalValidation()
    }

  } else {
    ev_gmm_manhattan = initializeExternalValidation()

    iv_gmm_manhattan = initializeInternalValidation()

  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_gmm_manhattan$time = time - iv_gmm_manhattan$time
  iv_gmm_manhattan$time = time - ev_gmm_manhattan$time

  result = list("external" = ev_gmm_manhattan,
                "internal" = iv_gmm_manhattan)

  return (result)
}

