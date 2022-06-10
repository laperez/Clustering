#'
#' Method that runs the daisy algorithm using the Euclidean metric to
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

daisy_euclidean_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  daisy_euclidean <- tryCatch({
    daisy(x = dt, metric = CONST_EUCLIDEAN)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(daisy_euclidean)) {
    daisy_euclidean_clust = hclust(dist(daisy_euclidean),
                                   method = CONST_CENTROID)

    if (!is.null(daisy_euclidean_clust)) {
      ev_daisy_euclidean <-
        tryCatch({
          external_validation(c(dt[, columnClass]),
                              cutree(daisy_euclidean_clust, k = clusters)
                              ,metric)

        },

        error = function(cond) {
          ev_daisy_euclidean = initializeExternalValidation()
        })

      iv_daisy_euclidean <- tryCatch({
        internal_validation(
          distance = CONST_NULL,
          clusters_vector = cutree(daisy_euclidean_clust, k = clusters),
          dataf = dt,
          method = CONST_EUCLIDEAN,
          metric
        )

      },

      error = function(cond) {
        iv_daisy_euclidean = initializeInternalValidation()
      })

    } else {
      ev_daisy_euclidean = initializeExternalValidation()
      iv_daisy_euclidean = initializeInternalValidation()
    }


  } else {
    ev_daisy_euclidean = initializeExternalValidation()
    iv_daisy_euclidean = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_daisy_euclidean$time = time - iv_daisy_euclidean$time
  iv_daisy_euclidean$time = time - ev_daisy_euclidean$time

  result = list("external" = ev_daisy_euclidean,
                "internal" = iv_daisy_euclidean)

  return (result)

}

#'
#' Method that runs the daisy algorithm using the Manhattan metric to
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

daisy_manhattan_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  daisy_manhattan <- tryCatch({
    daisy(x = dt, metric = CONST_MANHATTAN)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(daisy_manhattan)) {
    daisy_manhattan_clust <-
      tryCatch({
        hclust(dist(daisy_manhattan), method = CONST_SINGLE)

      },

      error = function(daisy_manhattan_clust) {
        return(CONST_NULL)
      })

    if (!is.null(daisy_manhattan_clust)) {
      ev_daisy_manhattan <-
        tryCatch({
          external_validation(c(dt[, columnClass]),
                              cutree(daisy_manhattan_clust, k = clusters)
                              ,metric)

        },

        error = function(daisy_manhattan_clust) {
          ev_daisy_manhattan = initializeExternalValidation()
        })

      iv_daisy_manhattan <- tryCatch({
        internal_validation(
          distance = CONST_NULL,
          clusters_vector = cutree(daisy_manhattan_clust, k = clusters),
          dataf = dt,
          method = CONST_MANHATTAN,
          metric
        )

      },

      error = function(daisy_manhattan_clust) {
        iv_daisy_manhattan = initializeInternalValidation()
      })

    } else {
      ev_daisy_manhattan = initializeExternalValidation()
      iv_daisy_manhattan = initializeInternalValidation()
    }




  } else {
    ev_daisy_manhattan = initializeExternalValidation()
    iv_daisy_manhattan = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_daisy_manhattan$time = time - iv_daisy_manhattan$time
  iv_daisy_manhattan$time = time - ev_daisy_manhattan$time

  result = list("external" = ev_daisy_manhattan,
                "internal" = iv_daisy_manhattan)

  return (result)

}

#'
#' Method that runs the daisy algorithm using the Gower metric to
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

daisy_gower_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  daisy_gower <- tryCatch({
    daisy(x = dt, metric = CONST_GOWER)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(daisy_gower)) {
    daisy_gower_clust <-
      tryCatch({
        hclust(dist(daisy_gower), method = CONST_SINGLE)
      },

      error = function(cond) {
        return(CONST_NULL)
      })


    if (!is.null(daisy_gower_clust)) {
      ev_daisy_gower <-
        tryCatch({
          external_validation(c(dt[, columnClass]),
                              cutree(daisy_gower_clust, k = clusters),metric)

        },

        error = function(cond) {
          ev_daisy_gower = initializeExternalValidation()
        })

      iv_daisy_gower <- tryCatch({
        internal_validation(
          distance = as.matrix(daisy_gower),
          clusters_vector = cutree(daisy_gower_clust, k = clusters),
          dataf = dt,
          method = CONST_NULL,
          metric
        )
      },

      error = function(cond) {
        iv_daisy_gower = initializeInternalValidation()
      })

    } else {
      ev_daisy_gower = initializeExternalValidation()
      iv_daisy_gower = initializeInternalValidation()
    }

  } else {
    ev_daisy_gower = initializeExternalValidation()
    iv_daisy_gower = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_daisy_gower$time = time - iv_daisy_gower$time
  iv_daisy_gower$time = time - ev_daisy_gower$time

  result = list("external" = ev_daisy_gower,
                "internal" = iv_daisy_gower)

  return (result)
}
