#' Function to obtain CIF, difference in CIF, and Restricted Mean Time Lost (RMTL) from a stpm2 Stata model
#'
#' @param i Internal argument used by the boot function
#' @param dat The dataset to be analyzed
#' @param stata_code Stata code to run the stpm2 model and generate CIF predictions
#' @param idvar Character string specifying the ID variable in the dataset
#' @param cif Logical (TRUE or FALSE), indicating whether to return CIF estimates
#' @param diff Numeric vector of time points at which to compute differences in CIF (default is NULL; no differences are calculated)
#' @param rmtl Numeric vector of time horizons for calculating RMTL (default is NULL; RMTL is not calculated)
#' @param group_names Character vector specifying the names of the groups for CIF comparison
#' @param event The event of interest for CIF and RMTL calculations
#' @param ref Reference group for comparisons (default is 1)
#'
#' @return
#' @export
#'
#' @examples
stpm2boot <- function(data,
                      i,
                      dat,
                      stata_code,
                      idvar,
                      cif = F,
                      diff = NULL,
                      rmtl = NULL,
                      group_names = c(),
                      event = c("dementia"),
                      ref = 1) {
  # checks
  if (!((stringr::str_match(
    sub(".*cause1\\(([^)]+)\\).*", "\\1", stata_code), event
  ) == event) |
    (stringr::str_match(
      sub(".*cause2\\(([^)]+)\\).*", "\\1", stata_code), event
    ) == event))) {
    stop("event needs to be either cause1 or cause2 in stata_code")
  }


  datai <- dat %>% dplyr::filter(.data[[idvar]] %in% data[i])



  invisible({
    suppressMessages({
      # we execute the STATA code within R
      get_cif <- RStata::stata(stata_code, data.in = datai, data.out = T)

      select_cols <- c("_newt", paste0("CIF_", as.vector(outer(
        event, group_names, paste0
      ))))
      get_cif %<>% dplyr::select(tidyselect::any_of(select_cols))

      if (!is.null(diff)) {
        diffCIF <- c()
        for (e in 1:length(event)) {
          for (d in 1:length(diff)) {
            for (r in 1:length(ref)) {
              thisdiff <- get_cif %>%
                dplyr::filter(`_newt` >= diff[d]) %>%
                dplyr::slice(1) %>%
                dplyr::select(-`_newt`) %>%
                dplyr::transmute(dplyr::across(
                  .data[[paste0("CIF_", event[e], ref[r])]],
                  ~ .data[[paste0("CIF_", event[e], group_names)]] - .x,
                  .names = paste0("diff", diff[d], "_{.col}")
                )) %>%
                dplyr::mutate(dplyr::across(tidyselect::everything(), ~ as.numeric(.)))

              diffCIF <- c(diffCIF, thisdiff)
            }
          }
        }
        res <- unlist(diffCIF)
      } else {
        res <- NULL
      }


      if (!is.null(rmtl)) {
        rtmlCIF <- c()
        rmtlDIFF <- c()
        for (e in 1:length(event)) {
          for (d in 1:length(rmtl)) {
            for (r in 1:length(ref)) {
              thisrmtl <- get_cif %>%
                dplyr::filter(`_newt` <= rmtl[d]) %>%
                dplyr::select(-`_newt`) %>%
                purrr::map_dbl(~ trapz(na.omit(get_cif$`_newt`[get_cif$`_newt` <=
                  rmtl[d]]), .))
              names(thisrmtl) <- paste0("RMTL", rmtl[d], "_", event[e], group_names)

              thisrmtldiff <- thisrmtl - thisrmtl[paste0("RMTL", rmtl[d], "_", event[e], ref[r])]

              names(thisrmtldiff) <- paste0("diff", rmtl[d], "_RMTL", event[e], group_names)
              thisrmtldiff <- thisrmtldiff[setdiff(names(thisrmtldiff), paste0("diff", rmtl[d], "_RMTL", event[e], ref[r]))]

              rtmlCIF <- c(rtmlCIF, thisrmtl)
              rmtlDIFF <- c(rmtlDIFF, thisrmtldiff)
              res <- c(res, rtmlCIF, rmtlDIFF)
            }
          }
        }
      }
      if (cif) {
        cif_v <- get_cif %>%
          dplyr::select(-`_newt`) %>%
          as.matrix() %>%
          as.vector()
        res <- c(res, cif_v)
      }
    })
  })

  return(res)
}



utils::globalVariables(c("_newt"))
