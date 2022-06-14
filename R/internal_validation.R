#'
#' Method in charge of obtaining those metrics that are internal from those
#' indicated.
#'
#' @param metrics Array with the metrics used in the calculation.
#'
#' @return Return an array with the metrics that are internal.
#'
#' @keywords internal
#'

row_name_df_internal = function(metrics) {
  result = c()

  result = c(result, CONST_ALGORITHM)
  result = c(result, CONST_DISTANCE)
  result = c(result, CONST_CLUSTERS)
  result = c(result, CONST_DATASET)
  result = c(result, CONST_RANKING)
  result = c(result, CONST_TIME_INTERNAL)

  for (iterate in 1:length(metrics)) {
    if (tolower(metrics[iterate]) == tolower(CONST_CONNECTIVITY_METRIC)) {
      result = c(result, CONST_CONNECTIVITY_METRIC)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_DUNN_METRIC)) {
      result = c(result, CONST_DUNN_METRIC)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_SILHOUETTE_METRIC)) {
      result = c(result, CONST_SILHOUETTE_METRIC)
    }

    #Variable

    if (tolower(metrics[iterate]) == tolower(CONST_TIME_INTERNAL_ATTR)) {
      result = c(result, CONST_TIME_INTERNAL_ATTR)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_CONNECTIVITY_METRIC_ATTR)) {
      result = c(result, CONST_CONNECTIVITY_METRIC_ATTR)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_DUNN_METRIC_ATTR)) {
      result = c(result, CONST_DUNN_METRIC_ATTR)
    }

    if (tolower(metrics[iterate]) == tolower(CONST_SILHOUETTE_METRIC_ATTR)) {
      result = c(result, CONST_SILHOUETTE_METRIC_ATTR)
    }

  }

  return (result)
}


#'
#' Method that applicate differents internal metrics about a data frame or
#' matrix, for example dunn, connectivity etc.
#'
#' @param distance Dissimilarity matrix.
#'
#' @param clusters_vector Array that containe tha data grouped in cluster.
#'
#' @param dataf Dataframe with original data.
#'
#' @param method Indicates the method for calculating distance between points.
#'
#' @param metric Array with external metric types.
#'
#' @return Return a list of the internal results initialized to zero.
#'
#' @keywords internal
#'

internal_validation = function(distance = NULL,
                               clusters_vector,
                               dataf = NULL,
                               method = CONST_EUCLIDEAN,
                               metric = NULL) {
  start.time = Sys.time()

  connectivity = 0.0

  dunn = 0.0

  silhouette = 0.0

  distance_null <- ifelse (!is.null(distance), 0, 1)

  if (distance_null == 0) {
    method = ""
  } else {
    distance = distance_matrix(
      data =  dataf,
      method = method,
      upper = TRUE,
      diagonal = TRUE
    )
    method = ""
    dataf = NULL

  }

  metrics_nula <- ifelse(is.null(metric), 0, 1)

  if (sum(metrics_nula) == 0) {
    connectivity =
      connectivity_metric (distance, clusters_vector, dataf, method)

    dunn = dunn_metric (distance, clusters_vector, dataf, method)

    silhouette = silhouette_metric (clusters_vector, distance)

  } else {
    for (me in metric) {
      if (me == CONST_DUNN_METRIC) {
        dunn = dunn_metric (distance, clusters_vector, dataf, method)
      }
      if (me == CONST_SILHOUETTE_METRIC)
        silhouette = silhouette_metric (clusters_vector, distance)
      if (me == CONST_CONNECTIVITY_METRIC)
        connectivity =
          connectivity_metric (distance, clusters_vector, dataf, method)
    }

  }


  end.time = Sys.time()
  time = end.time - start.time

  time_internal = round(as.numeric(time), 4)

  if (is.infinite(as.numeric(dunn))) dunn <- 0
  if (is.na(as.numeric(dunn))) dunn <- 0

  if (is.infinite(as.numeric(connectivity))) connectivity <- 0
  if (is.na(as.numeric(connectivity))) connectivity <- 0

  if (is.infinite(as.numeric(silhouette))) silhouette <- 0
  if (is.na(as.numeric(silhouette))) silhouette <- 0

  result = list(
    "connectivity" = format(round(as.numeric(connectivity), digits = 4),
                            scientific = FALSE),
    "dunn" = format(round(as.numeric(dunn), digits = 4),scientific = FALSE),
    "silhouette" = format(round(
      as.numeric(silhouette),
      digits = 4),
      scientific = FALSE
    ),
    "time" = round(time_internal, digits = 4)
  )

  return (result)

}


#'
#' Method to calculate the connectivity.
#'
#' @param distance Dissimilarity matrix.
#'
#' @param clusters_vector Array that containe tha data grouped in cluster.
#'
#' @param dt Dataframe with original data.
#'
#' @param method Indicates the method for calculating distance between points.
#'
#' @return Return a double with the result of the connectivity calculation.
#'
#' @keywords internal
#'

connectivity_metric =
  function(distance, clusters_vector, dt, method) {
    connectivity = 0.0

    connectivity = tryCatch({
      calculate_connectivity(
        distance = distance,
        clusters = clusters_vector,
        datadf = dt,
        method = method
      )
    },

    error = function(cond) {
      connectivity = 0
    })

    return (connectivity)

  }

#'
#' Method to calculate the dunn.
#'
#' @param distance Dissimilarity matrix.
#'
#' @param clusters_vector Array that containe tha data grouped in cluster.
#'
#' @param dt Dataframe with original data.
#'
#' @param method Indicates the method for calculating distance between points.
#'
#' @return Return a double with the result of the dunn calculation.
#'
#' @keywords internal
#'

dunn_metric = function(dist, clusters_vector, dt, me) {
  dunn = 0.0

  dunn = tryCatch({
    calculate_dunn(dist, clusters_vector, dt, me)
  },

  error = function(cond) {
    stop(cond)
    dunn = 0
  })

  return (dunn)
}

#'
#' Method to calculate the silhouette.
#'
#' @param distance Dissimilarity matrix.
#'
#' @param clusters_vector Array that containe tha data grouped in cluster.
#'
#' @return Return a double with the result of the silhouette calculation.
#'
#' @keywords internal
#'

silhouette_metric = function (clusters_vector, distance) {
  silhouette = 0.0

  silhouette = tryCatch({
    si = silhouette(clusters_vector, distance)
    round(sum(si[, 3]) / length(si[, 3]), digits = 2)
  },

  error = function(cond) {
    silhouette = 0
  })

  return (silhouette)
}

#'
#'Method that return a list of external validation initialized to zero.
#'
#'@return A list of all values set to zero.
#'
#'@keywords internal
#'

initializeInternalValidation = function() {
  connectivity = 0

  start.time = Sys.time()

  dunn = 0

  silhouette = 0

  end.time = Sys.time()
  time = end.time - start.time


  result = list(
    "connectivity" = format(round(as.numeric(connectivity), digits = 4),
                            scientific = FALSE),
    "dunn" = format(round(as.numeric(dunn), digits = 4), scientific = FALSE),
    "silhouette" = format(round(as.numeric(silhouette), digits = 4),
                          scientific = FALSE),
    "time" = round(as.numeric(time), digits = 4)
  )

  return (result)

}

#'
#' Method to calculate the dunn.
#'
#' @param distance Dissimilarity matrix.
#'
#' @param clusters Array that containe tha data grouped in cluster.
#'
#' @param datadf Dataframe with original data.
#'
#' @param method Indicate the method for calculating distance between points.
#'
#' @return Return a double with the result of the dunn calculation
#'
#' @keywords internal
#'

calculate_dunn <-
  function(distance = NULL,
           clusters,
           datadf = NULL,
           method = "euclidean") {
    distance_null <- ifelse (is.null(distance), 1, 0)
    data_null <- ifelse (is.null(datadf), 1, 0)

    if (distance_null == 1 &&
        data_null == 1)
      stop("The distance or datadf field must be filled in")
    if (distance_null == 1)
      distance <- as.matrix(dist(datadf, method = method))

    type_distance <- ifelse ('dist' %in% class(datadf), 1, 0)

    if (type_distance == 1)
      distance <- as.matrix(distance)
    nc <- max(clusters)
    interClust <- matrix(NA, nc, nc)
    intraClust <- rep(NA, nc)

    for (i in 1:nc) {
      c1 <- which(clusters == i)
      for (j in i:nc) {
        if (j == i)
          intraClust[i] <- max(distance[c1, c1])
        if (j > i) {
          c2 <- which(clusters == j)
          interClust[i, j] <- min(distance[c1, c2])
        }
      }
    }
    dunn <- min(interClust, na.rm = TRUE) / max(intraClust)
    return(dunn)
  }


#'
#' Method to calculate the Connectivity
#'
#' @param distance Dissimilarity matrix.
#'
#' @param clusters Array that containe tha data grouped in cluster.
#'
#' @param datdf Dataframe with original data.
#'
#' @param neighbSize Number of neighbours.
#'
#' @param method Indicates the method for calculating distance between points.
#' Default is euclidean.
#'
#' @return Return a double with the result of the connectivity calculation.
#'
#' @keywords internal
#'

calculate_connectivity <-
  function(distance = NULL,
           clusters,
           datadf = NULL,
           neighbSize = 12,
           method = "euclidean") {
    distance_null <- ifelse (is.null(distance), 1, 0)
    data_null <- ifelse (is.null(datadf), 1, 0)

    if (distance_null == 1 &&
        data_null == 1)
      stop("The distance or datadf field must be filled in")
    if (distance_null == 1)
      distance <- as.matrix(dist(datadf, method = method))

    type_distance <- ifelse ('dist' %in% class(datadf), 1, 0)
    if (type_distance == 1)
      distance <- as.matrix(distance)
    nearest <-
      apply(distance, 2, function(x)
        sort(x, ind = TRUE)$ix[2:(neighbSize + 1)])
    nr <- nrow(nearest)
    nc <- ncol(nearest)
    same <-
      matrix(clusters,
             nrow = nr,
             ncol = nc,
             byrow = TRUE) != matrix(clusters[nearest], nrow = nr, ncol = nc)
    conn <- sum(same * matrix(1 / 1:neighbSize, nrow = nr, ncol = nc))
    return(conn)
  }

#'
#'Method for filtering internal columns of a dataset.
#'
#'@param df data frame with clustering results.
#'
#'@return dafa frame filtered with the columns of the internal measurements.
#'

transform_dataset_internal = function(df) {

  nameColumns <- colnames(df)

  numberOcurrenceTime <- which(nameColumns == CONST_TIME_EXTERNAL)

  numberOcurrenceTimeAttr <- which(nameColumns == CONST_TIME_EXTERNAL_ATTR)

  if (length(numberOcurrenceTime) > 1){

    c <- c()

    #' Exists internal measure

    for (iterate in 1:length(nameColumns)) {

      if (nameColumns[iterate] %in% c(CONST_ALGORITHM,CONST_DISTANCE,CONST_CLUSTERS,CONST_DATASET,CONST_RANKING)){
        c <- append(c,iterate)
      } else {

        if (iterate >= numberOcurrenceTime[2] && iterate < numberOcurrenceTimeAttr[1]){
          c <- append(c,iterate)
        } else {
          if (iterate >= numberOcurrenceTimeAttr[2]){
            c <- append(c,iterate)
          }
        }
      }
    }

    nameColumns <- c
  }

  return (select(df, nameColumns))
}
