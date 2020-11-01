#'Data from an experiment on the affects of machine adjustments on the time to count bolts.
#'
#'A manufacturer of automotive accessories provides hardware, e.g. nuts, bolts, washers and screws, to fasten the accessory to the car or truck. Hardware is counted and packaged automatically. Specifically, bolts are dumped into a large metal dish. A plate that forms the bottom of the dish rotates counterclockwise. This rotation forces bolts to the outside of the dish and up along a narrow ledge. Due to the vibration of the dish caused by the spinning bottom plate, some bolts fall off the ledge and back into the dish. The ledge spirals up to a point where the bolts are allowed to drop into a pan on a conveyor belt. As a bolt drops, it passes by an electronic eye that counts it. When the electronic counter reaches the preset number of bolts, the rotation is stopped and the conveyor belt is moved forward
#'
#'There are several adjustments on the machine that affect its operation. These include; a speed setting that controls the speed of rotation (SPEED1Integer) of the plate at the bottom of the dish, a total number of bolts (TOTAL) to be counted, a second speed setting (SPEED2Integer) that is used to change the speed of rotation (usually slowing it down) for the last few bolts, the number of bolts to be counted at this second speed (NUMBER2Integer), and the sensitivity of the electronic eye (SENSInteger). The sensitivity setting is to insure that the correct number of bolts are counted. Too few bolts packaged causes customer complaints. Too many bolts packaged increases costs. For each run conducted in this experiment the correct number of bolts was counted. From an engineering standpoint if the correct number of bolts is counted, the sensitivity should not affect the time to count bolts. The measured response is the time (TIMEReal), in seconds, it takes to count the desired number of bolts. In order to put times on a equal footing the response to be analyzed is the time to count 20 bolts (T20BOLTReal). Below are the data for 40 combinations of settings. RUNinteger is the order in which the data were collected.
#'
#' @docType data
#'
#' @usage data(bolts)
#'
#' @format A data frame with 40 observations on 8 variables:
#' \describe{
#'   A manufacturer of automotive accessories provides hardware, e.g. nuts,
#'   bolts, washers and screws, to fasten the accessory to the car or truck.
#'   Hardware is counted and packaged automatically. Specifically, bolts are
#'   dumped into a large metal dish. A plate that forms the bottom of the dish
#'   rotates counterclockwise. This rotation forces bolts to the outside of the
#'   dish and up along a narrow ledge. Due to the vibration of the dish caused
#'   by the spinning bottom plate, some bolts fall off the ledge and back into
#'   the dish. The ledge spirals up to a point where the bolts are allowed to
#'   drop into a pan on a conveyor belt. As a bolt drops, it passes by an
#'   electronic eye that counts it. When the electronic counter reaches the
#'   preset number of bolts, the rotation is stopped and the conveyor belt
#'   is moved forward
#'   \item{RUNInteger}{is the order in which the data were collected}
#'   \item{SPEED1Integer}{a speed setting that controls the speed of rotation of
#'    the plate at the bottom of the dish}
#'   \item{TOTALInteger}{total number of bolts (TOTAL) to be counted}
#'   \item{SPEED2Integer}{a second speed setting hat is used to change the speed
#'    of rotation (usually slowing it down) for the last few bolts}
#'   \item{NUMBER2Integer}{the number of bolts to be counted at this second
#'   speed}
#'   \item{SENSInteger}{the sensitivity of the electronic eye}
#'   \item{TIMEReal}{The measured response is the time, in seconds}
#'   \item{T20BOLTReal}{n order to put times on a equal footing the response to
#'   be analyzed is the time to count 20 bolts}
#' }
#'
#' @keywords datasets
#'
#' @source KEEL, <http://www.keel.es/>
#'
"bolts"
