#' Dataset with training data. The data is obtained from running the clustering algorithm with the basketball dataset. Of all the packages we use the clusterr package and all the metrics. The number of clusters is between 3 and 4.
#'
#' @docType data
#'
#' @usage data(datasetTest)
#'
#' @format It is a \code{clustering} object:
#' \describe{
#' This dataset contains the information to run the clustering algorithm. The parameters are as follows:
#' \item{result}{It is a list with the algorithms, metrics and variables defined in the execution of the algorithm}
#' \item{hasInternalMetrics}{Boolean field to indicate if there are internal metrics such as: dunn, silhoutte and connectivity}
#' \item{hasExternalMetrics}{Boolean field to indicate if there are external metrics such as: precision, recall, f-measure, entropy, variation information and fowlkes-mallows}
#' \item{algorithms_execute}{Character vector with the algorithms executed. These algorithms have been mentioned in the definition of the parameters}
#' \item{measures_execute}{Character vector with the measures executed. These measures have been mentioned in the definition of the parameters}
#' }
#'
#' @keywords datasets
#'
"datasetTest"
