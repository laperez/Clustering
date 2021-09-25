#' Method in charge of calculating the average for all datasets using all the
#' algorithms defined in the application.
#'
#' @param data is a data frame or matrix
#' @param method described the metrics used by each of the algorithms
#' @param cluster number of clusters
#' @param nameDataset specify the name of dataset like information
#' @param metrics array with internal or external metrics
#'
#' @return a list with result of external and internal validation applying on
#' algorithms
#'
#' @keywords internal
#'

evaluate_all_column_dataset = function(datas,
                                       method,
                                       cluster,
                                       nameDataset,
                                       metrics) {
  if ('data.frame' %in% class(datas))
    datas = as.matrix(datas)

  nColumnDataSet = ncol(datas)

  resultado = list()
  entropy = list()
  variation_information = list()
  precision = list()
  recall = list()
  f_measure = list()
  fowlkes_mallows_index = list()
  timeExternal = list()

  timeInternal = list()
  connectivity = list()
  dunn = list()
  silhouette = list()

  if (anyNA(datas)) {
    minValue <- as.numeric(which.min(datas) - 1)
    datas[is.na(datas)] <- minValue
  }

  datas <- convert_numeric_matrix(datas)
  mode(datas) = "numeric"

  datas = center_scale(datas)

  calculate_result <- list()

  existsInfinitive <- ifelse(is.finite(datas), 1, 0)

  if (sum(existsInfinitive) > 0) {
    datas <- datas[is.finite(rowSums(datas)),]
  }


  if (!method %in% c(
    'agnes_euclidean',
    'agnes_manhattan',
    'pvclust_euclidean',
    'pvclust_euclidean',
    'diana_euclidean',
    'aggExCluster_euclidean',
    'hclust_euclidean'
  )) {
    for (i in 1:nColumnDataSet) {
      resultado = CONST_NULL

      if (method == 'gmm_euclidean') {
        resultado = gmm_euclidean_method(datas, cluster, i, metrics)
      }


      if (method == 'gmm_manhattan') {
        resultado = gmm_manhattan_method(datas, cluster, i, metrics)

      }

      if (method == 'kmeans_arma') {
        resultado = kmeans_arma_method(datas, cluster, i, metrics)

      }

      if (method == 'kmeans_rcpp') {
        resultado = kmeans_rcpp_method(datas, cluster, i, metrics)
      }

      if (method == 'mini_kmeans') {
        resultado = mini_kmeans_method(datas, cluster, i, metrics)

      }

      if (method == 'clara_euclidean') {
        resultado = clara_euclidean_method(datas, cluster, i, metrics)
      }

      if (method == 'clara_manhattan') {
        resultado = clara_manhattan_method(datas, cluster, i, metrics)
      }

      if (method == 'daisy_gower') {
        resultado = daisy_gower_method(datas, cluster, i, metrics)
      }

      if (method == 'daisy_euclidean') {
        resultado = daisy_euclidean_method(datas, cluster, i, metrics)
      }

      if (method == 'daisy_manhattan') {
        resultado = daisy_manhattan_method(datas, cluster, i, metrics)
      }

      if (method == 'fanny_euclidean') {
        resultado = fanny_euclidean_method(datas, cluster, i, metrics)
      }

      if (method == 'fanny_manhattan') {
        resultado = fanny_manhattan_method(datas, cluster, i, metrics)
      }

      if (method == 'mona') {
        resultado = mona_method(datas, cluster, i, metrics)
      }

      if (method == 'pam_euclidean') {
        resultado = pam_euclidean_method(datas, cluster, i, metrics)
      }

      if (method == 'pam_manhattan') {
        resultado = pam_manhattan_method(datas, cluster, i, metrics)
      }

      if (method == 'fuzzy_cm') {
        resultado = fuzzy_cm_method(datas, cluster,  i, metrics)
      }

      if (method == 'fuzzy_gg') {
        resultado = fuzzy_gg_method(datas, cluster, i, metrics)
      }

      if (method == 'fuzzy_gk') {
        resultado = fuzzy_gk_method(datas, cluster, i, metrics)
      }

      if (method == 'pvpick') {
        resultado = pvpick_method(datas, cluster, metrics)
      }

      if (method == 'pvclust_correlation') {
        resultado = pvclust_correlation_method(datas, cluster, i, metrics)
      }

      if (method == 'pvclust_euclidean') {
        resultado = pvclust_euclidean_method(datas, cluster, i, metrics)
      }

      if (method == 'apclusterK_euclidean') {
        resultado = apclusterK_euclidean(datas, cluster, i, metrics)
      }

      if (method == 'apclusterK_manhattan') {
        resultado = apclusterK_manhattan(datas, cluster, i, metrics)
      }

      if (method == 'apclusterK_minkowski') {
        resultado = apclusterK_minkowski(datas, cluster, i, metrics)
      }

      result <- list(
        "entropy" = ifelse(
          !is.nan(resultado$external$entropy),
          resultado$external$entropy,
          0.0
        ),
        "variation_information" = ifelse(
          !is.nan(resultado$external$variation_information),
          resultado$external$variation_information,
          0.0
        ),
        "precision" =
          ifelse(
            !is.nan(resultado$external$precision),
            resultado$external$precision,
            0.0
          ),
        "recall" = ifelse(
          !is.nan(resultado$external$recall),
          resultado$external$recall,
          0.0
        ),
        "f_measure" =
          ifelse(
            !is.nan(resultado$external$f_measure),
            resultado$external$f_measure,
            0.0
          ),
        "fowlkes_mallows_index" = ifelse(
          !is.nan(resultado$external$fowlkes_mallows_index),
          resultado$external$fowlkes_mallows_index,
          0.0
        ),
        "timeExternal" = ifelse(
          !is.nan(resultado$external$time),
          resultado$external$time,
          0.0
        ),

        "connectivity" = ifelse(
          !is.nan(resultado$internal$connectivity),
          resultado$internal$connectivity,
          0.0
        ),
        "dunn" = ifelse(
          !is.nan(resultado$internal$dunn),
          resultado$internal$dunn,
          0.0
        ),
        "silhouette" =
          ifelse(
            !is.nan(resultado$internal$silhouette),
            resultado$internal$silhouette,
            0.0
          ),
        "timeInternal" =
          ifelse(
            !is.nan(resultado$internal$time),
            resultado$internal$time,
            0.0
          )
      )

      calculate_result[[i]] = result

    }

  } else {
    if (method == 'agnes_euclidean') {
      resultado = agnes_euclidean_method(datas, cluster, metrics)
    }

    if (method == 'agnes_manhattan') {
      resultado = agnes_manhattan_method(datas, cluster, metrics)
    }

    if (method == 'pvclust_euclidean') {
      resultado = pvclust_euclidean_method(datas, cluster, metrics)
    }

    if (method == 'pvclust_correlation') {
      resultado = pvclust_correlation_method(datas, cluster, metrics)
    }

    if (method == 'diana_euclidean') {
      resultado = diana_euclidean_method(datas, cluster, metrics)
    }

    if (method == 'aggExCluster_euclidean') {
      resultado = aggExCluster_euclidean(datas, cluster, metrics)
    }

    if (method == 'hclust_euclidean') {
      resultado = hclust_euclidean(datas, cluster, i, metrics)
    }

    result <- list(
      "entropy" = ifelse(
        !is.nan(resultado$external$entropy),
        resultado$external$entropy,
        0.0
      ),
      "variation_information" = ifelse(
        !is.nan(resultado$external$variation_information),
        resultado$external$variation_information,
        0.0
      ),
      "precision" =
        ifelse(
          !is.nan(resultado$external$precision),
          resultado$external$precision,
          0.0
        ),
      "recall" = ifelse(
        !is.nan(resultado$external$recall),
        resultado$external$recall,
        0.0
      ),
      "f_measure" =
        ifelse(
          !is.nan(resultado$external$f_measure),
          resultado$external$f_measure,
          0.0
        ),
      "fowlkes_mallows_index" = ifelse(
        !is.nan(resultado$external$fowlkes_mallows_index),
        resultado$external$fowlkes_mallows_index,
        0.0
      ),
      "timeExternal" = ifelse(
        !is.nan(resultado$external$time),
        resultado$external$time,
        0.0
      ),

      "connectivity" = ifelse(
        !is.nan(resultado$internal$connectivity),
        resultado$internal$connectivity,
        0.0
      ),
      "dunn" = ifelse(
        !is.nan(resultado$internal$dunn),
        resultado$internal$dunn,
        0.0
      ),
      "silhouette" =
        ifelse(
          !is.nan(resultado$internal$silhouette),
          resultado$internal$silhouette,
          0.0
        ),
      "timeInternal" =
        ifelse(
          !is.nan(resultado$internal$time),
          resultado$internal$time,
          0.0
        )
    )

    calculate_result[[1]] = result

  }

  for (i in 1:length(calculate_result)) {
    entropy[i] <- calculate_result[[i]]$entropy
    variation_information[i]  <-
      calculate_result[[i]]$variation_information
    precision[i]  <- calculate_result[[i]]$precision
    recall[i]  <- calculate_result[[i]]$recall
    f_measure[i]  <- calculate_result[[i]]$f_measure
    fowlkes_mallows_index[i]  <-
      calculate_result[[i]]$fowlkes_mallows_index
    timeExternal[i]  <- calculate_result[[i]]$timeExternal
    connectivity[i]  <- calculate_result[[i]]$connectivity
    dunn[i]  <- calculate_result[[i]]$dunn
    silhouette[i]  <- calculate_result[[i]]$silhouette
    timeInternal[i]  <- calculate_result[[i]]$timeInternal
  }

  external = list(
    "entropy" = entropy,
    "variation_information" =
      variation_information,
    "precision" = precision,
    "recall" = recall,
    "f_measure" = f_measure,
    "fowlkes_mallows_index" = fowlkes_mallows_index,
    "timeExternal" = timeExternal
  )

  internal = list(
    "connectivity" = connectivity,
    "dunn" = dunn,
    "silhouette" = silhouette,
    "timeInternal" = timeInternal
  )

  result = list("external" = external, "internal" = internal)

  return (result)
}
