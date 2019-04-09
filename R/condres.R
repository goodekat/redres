#' condress
#'
#' This is Function that is used to identify conditional residule of the model.
#'
#' @param model input mixed model

#' @import Matrix
#' @import lme4
#'
#' @export condres
#'
#' @examples
#'
#' # read data from stat510 of Iowa State University
#' d = read.delim("http://dnett.github.io/S510/SeedlingDryWeight2.txt")
#' d$Genotype = factor(d$Genotype)
#' # fit the mixed model
#' model <- lmer(SeedlingWeight ~ Genotype + (1|Tray), REML = F,data = d)
#' # using the condres function to get coditional residule of the model
#' condres(model)

condres <- function(model) {
  BLUPs <- coef(model)[[1]][,1]                          ## BlupS for each subject
  subj <- table(unlist(model@flist[1]))                  ## number of levels for each subject
  blups_y <- rep(BLUPs, c(as.numeric(as.matrix(subj))))  ## BLUP for each observation
  res <- model@resp$y - blups_y                          ## conditional residule
  res
}





