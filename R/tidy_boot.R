#' Tidy bootstrap results
#'
#' @param fit_boot
#'
#' @return percentile bootstrap 95% CI
#' @export
#'
#' @examples
tidy_boot <- function(fit_boot){
  est <- fit_boot$t0
  lower <- apply(fit_boot$t,2,function(x)quantile(x,probs=0.025))
  upper <- apply(fit_boot$t,2,function(x)quantile(x,probs=0.975))
  res <- cbind(est,lower,upper)
  colnames(res) <- c("Estimate","Lower 95% CI","Upper 95% CI")
  rownames(res) <- names(est)
  return(res)
}
