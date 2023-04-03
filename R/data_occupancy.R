#' Occupancy dataset
#'
#' Experimental data used for binary classification (room occupancy) from
#' Temperature,Humidity,Light and CO2. Ground-truth occupancy was obtained
#' from time stamped pictures that were taken every minute. We've removed the
#' date time column of the measurement as it is unnecessary for the training model.
#'
#'
#' @format A data frame with 20560 rows and 6 variables:
#' \describe{
#'   \item{Temperature}{numeric, Temperature in Celsius}
#'   \item{Humidity}{numeric, Relative Humidity in %}
#'   \item{Light}{numeric, Light in Lux}
#'   \item{CO2}{numeric, CO2 in ppm}
#'   \item{HumidityRatio}{numeric, Derived quantity from temperature and relative humidity, in kgwater-vapor/kg-air}
#'   \item{Occupancy}{integer, 0 for not occupied, 1 for occupied status}
#' }
#' @name occupancy
#' @docType data
#' @usage data(occupancy)
#'
#' @source Data from UCL \url{https://archive.ics.uci.edu/ml/datasets/Occupancy+Detection+}
#'
NULL
