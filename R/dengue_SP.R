#' Dengue cases in *Sao Paulo*
#'
#' Data frame containing the weekly number of notified dengue cases in
#' the municipality of *Sao Paulo*, as well as a set of climatic covariates.
#' 
#' @docType data
#' @format 
#' A data frame with 678 rows and 8 columns:
#' \describe{
#'   \item{date}{First day of the week in date format.}
#'   \item{geocode}{Unique ID code for Sao Paulo microregion.}
#'   \item{cases}{Number of notified dengue cases.}
#'   \item{year}{Year (2010 - 2022).}
#'   \item{temp_med}{Weekly average daily mean temperature.}
#'   \item{precip_tot}{Weekly cumulative precipitation.}
#'   \item{enso}{El Niño-Southern Oscillation index.}
#'   \item{pop}{Number of inhabitants.}
#' }
#' @source <https://info.dengue.mat.br/services/api>
#' @usage data(dengue_SP)
"dengue_SP"