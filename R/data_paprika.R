#' Paprika Dataset
#'
#' Data from a study on the growth rate of paprika plants grown under small-scale farming
#' conditions in southern Africa.
#'
#' @format A tibble with 1070 rows and 5 variables:
#' \describe{
#'   \item{rep}{replicate number}
#'   \item{treatment}{fertilizer treatment}
#'   \item{variety}{plant variety}
#'   \item{plant}{plant number}
#'   \item{height}{height of the plant (cm)}
#'   ...
#' }
#'
#' @details The study was conducted in 2007 and 2008 in Malawi using a split plot design
#' with five replicates. The researchers compared 4 fertilizer treatments ((1) control,
#' (2) inorganic fertilizer, (3) Gliricidia biomass, and (4) Gliricidia biomass plus
#' half the recommended dose of fertilizer for paprika) and 6 plant varieties
#' ((1) Papri-King, (2) Papri-Queen, (3) Papri-Excel, (4) Papri-Supreme, (5) Papri-Ace,
#' and (6) PX1140 4601). The fertilizers were applied to the main plots, and the
#' varieties were assigned to the subplots within a main plot. For each replicate,
#' 10 plants were randomly chosen from each treatment and variety treatment combination
#' and observed over 20 weeks. This dataset contains observations from week 4
#' of the data in 2008.
#'
#' @source The data was obtained from \url{https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/25747},
#' and the original paper on the study can be found at \url{https://link.springer.com/article/10.1007\%2Fs10457-011-9415-2}.

"paprika"


