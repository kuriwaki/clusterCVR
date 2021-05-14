#' Simulated data (some races not contested)
#'
#' Simulated dataset that can be used for clustering
#'
#' @format A list with several parameters
#' \describe{
#'   \item{y}{Numeric full matrix}
#'   \item{uy}{Same numeric full matrix, unique profiles}
#'   \item{m}{Indicator for the missingness in each cell of \code{y}. See writeup}
#'   ...
#' }
"simdata_miss"


#' Simulated data (all races are contested)
#'
#' Simulated dataset that can be used for clustering
#'
#' @format A list with several parameters
#' \describe{
#'   \item{y}{Numeric full matrix}
#'   \item{uy}{Same numeric full matrix, unique profiles}
#'   \item{m}{Indicator for the missingness in each cell of \code{y}. See writeup}
#'   ...
#' }
"simdata_full"


#' varying choice set mlogit replication
#'
#' Replication data from the \code{mlogit} package, which is available
#' by \code{ModeCanada}. This data shows the mode of transportation 14,000+
#' respondents in Canada took.
#'
#' @details \code{canada_mlogit_y} is a length 4,324 vector of the outcome.
#' It is coded numerically with the reference category at 0. 0 is "train",
#' 1 is "air", 2 is "bus", and 3 is "car".
#'
#' \code{canada_mlogit_m} is a 4,324 - by - 4 matrix indicating the availability
#' of the choice on the menu. Cell (i, l) is 1 is individual i had option l
#' available on the menu, 0 otherwise. Because this is a R matrix, the columns are
#' 1-based.
#'
#' \code{canada_mlogit_w} is a length 4,324 vector of the weights, which are
#' are necessary in a clustering task. These are arbitrarily the individual-specific
#' variable "dist" in the ModeCanada dataset.
#'
#'
"canada_mlogit_y"


#' @rdname canada_mlogit_y
"canada_mlogit_w"
#' @rdname canada_mlogit_y
"canada_mlogit_m"


#' Sample CCES
#'
#' @description Sample from the CCES 2018, Massachusetts, Wisconsin, and Michigan.
#' Includes vote choice for President, Governor, Congress, Attorney General, Secreatary
#' of State, and demographics, which were all up for election in these states.
#' President is retrospective from 2016.
#'
#'
#' @format
#' \describe{
#'   \item{case_id}{Unique CCES respondent identifier}
#'   \item{st}{State abbreviation}
#'   \item{cd}{congressional district (for US House)}
#'   \item{pid}{Partisan identification. Leaners based on pid7 in the CCES
#'    are coded as partisans}
#'   \item{race}{Race as coded by the cumulative common content}
#'   \item{newsint}{News interest  as coded by the cumulative common content}
#'   \item{ideo5}{Ideology as coded by the cumulative common content}
#'   \item{PRS_party, USS_party, GOV_party, USH_party, ATG_party,  SOS_party}{
#'   Party vote for several offices. PRS = President, USS = US Senate,
#'   GOV = Governor, USH = US House, ATG = Attorney General, SOS = Secretary of State}
#' }
#'
#' @importFrom tibble tibble
#' @import haven
#'
#' @examples
#' cces18_samp
"cces18_samp"
