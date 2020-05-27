#' Method that runs the apclusterK algorithm using the euclidean metric to make an external or internal validation of the cluster
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

apclusterK_euclidean = function(data, clusters, columnClass, metric) {
  start.time <- Sys.time()

  if ('data.frame' %in% class(data))
    data = as.matrix(data)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  appcluster_euclidean <- tryCatch({
    apclusterK(negDistMat(r = CONST_TWO, method = CONST_EUCLIDEAN),
                          x = data,
                          K = clusters)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(appcluster_euclidean)) {

    cluster_appcluster_euclidean = fill_cluster_vector(data, appcluster_euclidean)

    ev_apcluster_eu <-
      tryCatch({
        external_validation(c(data[, columnClass]),
                            cluster_appcluster_euclidean,metric)

      },

      error = function(cond) {
        ev_apcluster_eu = initializeExternalValidation()
      })

    iv_apcluster_eu <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = cluster_appcluster_euclidean,
        data = data,
        method = CONST_EUCLIDEAN,
        metric
      )

    },

    error = function(cond) {
      iv_apcluster_eu = initializeInternalValidation()

    })

  } else {
    ev_apcluster_eu = initializeExternalValidation()
    iv_apcluster_eu = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_apcluster_eu$time = time - iv_apcluster_eu$time
  iv_apcluster_eu$time = time - ev_apcluster_eu$time

  result = list("external" = ev_apcluster_eu,
                "internal" = iv_apcluster_eu)

  return (result)
}

#' Method that runs the apclusterK algorithm using the manhattan metric to make an external or internal validation of the cluster
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

apclusterK_manhattan = function(data, clusters, columnClass, metric) {
  start.time <- Sys.time()

  if ('data.frame' %in% class(data))
    data = as.matrix(data)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  appcluster_manhattan <- tryCatch({
    apclusterK(negDistMat(r = CONST_TWO, method = CONST_MANHATTAN),
                          x = data,
                          K = clusters)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(appcluster_manhattan)) {

    cluster_appcluster_manhattan = fill_cluster_vector(data, appcluster_manhattan)

    ev_apcluster_ma <-
      tryCatch({
        external_validation(c(data[, columnClass]),
                            cluster_appcluster_manhattan,metric)

      },

      error = function(cond) {
        ev_apcluster_ma = initializeExternalValidation()
      })

    iv_apcluster_ma <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = cluster_appcluster_manhattan,
        data = data,
        method = CONST_MANHATTAN,
        metric
      )

    },

    error = function(cond) {
      iv_apcluster_ma = initializeInternalValidation()

    })

  } else {
    ev_apcluster_ma = initializeExternalValidation()
    iv_apcluster_ma = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_apcluster_ma$time = time - iv_apcluster_ma$time
  iv_apcluster_ma$time = time - ev_apcluster_ma$time

  result = list("external" = ev_apcluster_ma,
                "internal" = iv_apcluster_ma)

  return (result)
}


#' Method that runs the apclusterK algorithm using the minkowski metric to make an external or internal validation of the cluster
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

apclusterK_minkowski = function(data, clusters, columnClass, metric) {
  start.time <- Sys.time()

  if ('data.frame' %in% class(data))
    data = as.matrix(data)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  appcluster_minkowski <- tryCatch({
    apclusterK(negDistMat(r = CONST_TWO, method = CONST_MINKOWSKI),
                          x = data,
                          K = clusters)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(appcluster_minkowski)) {

    cluster_appcluster_minkowski = fill_cluster_vector(data, appcluster_minkowski)

    ev_apcluster_mi <-
      tryCatch({
        external_validation(c(data[, columnClass]),
                            cluster_appcluster_minkowski,metric)

      },

      error = function(cond) {
        ev_apcluster_mi = initializeExternalValidation()
      })

    iv_apcluster_mi <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = cluster_appcluster_minkowski,
        data = data,
        method = CONST_MINKOWSKI,
        metric
      )

    },

    error = function(cond) {
      iv_apcluster_mi = initializeInternalValidation()

    })

  } else {
    ev_apcluster_mi = initializeExternalValidation()
    iv_apcluster_mi = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_apcluster_mi$time = time - iv_apcluster_mi$time
  iv_apcluster_mi$time = time - ev_apcluster_mi$time

  result = list("external" = ev_apcluster_mi,
                "internal" = iv_apcluster_mi)

  return (result)
}
