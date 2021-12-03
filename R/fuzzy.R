#'
#' Method that runs the fuzzy_cm algorithm using the Euclidean metric to
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

fuzzy_cm_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  on.exit(options(warn = -1))

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  fuzzy_cm <- tryCatch({
    fuzzy.CM(X = dt,
                       K = clusters,
                       max.iteration = 20)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(fuzzy_cm)) {
    ev_fuzzy_cm <-
      tryCatch({
        external_validation(c(dt[, columnClass]),
                            as.vector(fuzzy_cm@hard.label),metric)
      },

      error = function(cond) {
        ev_fuzzy_cm = initializeExternalValidation()
      })

    iv_fuzzy_cm <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = as.vector(fuzzy_cm@hard.label),
        dataf = dt,
        method = CONST_EUCLIDEAN,
        metric
      )
    },

    error = function(cond) {
      iv_fuzzy_cm = initializeInternalValidation()
    })

  } else {
    ev_fuzzy_cm = initializeExternalValidation()
    iv_fuzzy_cm = initializeInternalValidation()

  }


  end.time <- Sys.time()
  time <- end.time - start.time

  ev_fuzzy_cm$time = time - iv_fuzzy_cm$time
  iv_fuzzy_cm$time = time - ev_fuzzy_cm$time

  result = list("external" = ev_fuzzy_cm,
                "internal" = iv_fuzzy_cm)

  return (result)
}

#'
#' Method that runs the fuzzy_gg algorithm using the Euclidean metric to
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

fuzzy_gg_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  on.exit(options(warn = -1))

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  fuzzy_gg <- tryCatch({
    fuzzy.GG(X = dt,
                       K = clusters,
                       max.iteration = 20)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(fuzzy_gg)) {
    ev_fuzzy_gg <-
      tryCatch({
        external_validation(c(dt[, columnClass]),
                            as.vector(fuzzy_gg@hard.label),metric)

      },

      error = function(cond) {
        ev_fuzzy_gg = initializeExternalValidation()
      })

    iv_fuzzy_gg <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = as.vector(fuzzy_gg@hard.label),
        dataf = dt,
        method = CONST_EUCLIDEAN,
        metric
      )
    },

    error = function(cond) {
      iv_fuzzy_gg = initializeInternalValidation()
    })

  } else {
    ev_fuzzy_gg = initializeExternalValidation()
    iv_fuzzy_gg = initializeInternalValidation()
  }


  end.time <- Sys.time()
  time <- end.time - start.time

  ev_fuzzy_gg$time = time - iv_fuzzy_gg$time
  iv_fuzzy_gg$time = time - ev_fuzzy_gg$time

  result = list("external" = ev_fuzzy_gg,
                "internal" = iv_fuzzy_gg)

  return (result)
}

#'
#' Method that runs the fuzzy_gk algorithm using the Euclidean metric to
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

fuzzy_gk_method = function(dt, clusters, columnClass, metric) {

  start.time <- Sys.time()

  on.exit(options(warn = -1))

  if ('data.frame' %in% class(dt))
    dt = as.matrix(dt)

  numeric_cluster <- ifelse(!is.numeric(clusters),1,0)

  if (sum(numeric_cluster)>0)
    stop('The field clusters must be a numeric')

  fuzzy_gk <- tryCatch({
    fuzzy.GK(X = dt,
                       K = clusters,
                       max.iteration = 20)
  },

  error = function(cond) {
    return(CONST_NULL)
  })

  if (!is.null(fuzzy_gk)) {
    ev_fuzzy_gk <-
      tryCatch({
        external_validation(c(dt[, columnClass]),
                            as.vector(fuzzy_gk@hard.label),metric)

      },

      error = function(cond) {
        ev_fuzzy_gk = initializeExternalValidation()
      })

    iv_fuzzy_gk <- tryCatch({
      internal_validation(
        distance = CONST_NULL,
        clusters_vector = as.vector(fuzzy_gk@hard.label),
        dataf = dt,
        method = CONST_MAHALANOBIS,
        metric
      )

    },

    error = function(cond) {
      iv_fuzzy_gk = initializeInternalValidation()

    })

  } else {
    ev_fuzzy_gk = initializeExternalValidation()
    iv_fuzzy_gk = initializeInternalValidation()
  }

  end.time <- Sys.time()
  time <- end.time - start.time

  ev_fuzzy_gk$time = time - iv_fuzzy_gk$time
  iv_fuzzy_gk$time = time - ev_fuzzy_gk$time

  result = list("external" = ev_fuzzy_gk,
                "internal" = iv_fuzzy_gk)

  return (result)
}
