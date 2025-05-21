cattab <- function(x,
                   y,
                   weights = NULL,
                   percent = "column",
                   robust = TRUE,
                   show.n = TRUE,
                   show.asso = TRUE,
                   digits = c(1,1),
                   na.rm = TRUE,
                   na.value = "NAs") {
  
  if (!requireNamespace("gtsummary", quietly = TRUE))
    stop("gtsummary package should be installed to use this function")
  
  if (!requireNamespace("gt", quietly = TRUE))
    stop("gt package should be installed to use this function")
  
  if (!requireNamespace("survey", quietly = TRUE))
    stop("survey package should be installed to use this function")  
  
  if(is.null(weights)) weights <- rep(1, times = nrow(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  my_cramer <- function(data, variable, by, ...) {
    weighted.cramer(data$variables[[variable]], data$variables[[by]], weights = weights)
  }
  
  my_eta <- function(data, variable, by, ...) {
    stats::summary.lm(stats::aov(data$variables[[variable]] ~ data$variables[[by]], weights = weights))$r.squared
  }
  
  # x_name <- gsub("$", "_", deparse(substitute(x)), fixed = TRUE)
  y_name <- gsub("$", "_", deparse(substitute(y)), fixed = TRUE)
  
  df <- data.frame(y,x)
  names(df)[1] <- y_name
  
  dfw <- survey::svydesign(ids = ~ 1, data = df, weights = ~ weights)
  
  res <- gtsummary::tbl_svysummary(dfw,
                                   by = gtsummary::all_of(y_name),
                                   percent = percent,
                                   type = list(gtsummary::all_continuous() ~ "continuous2"),
                                   statistic = list(gtsummary::all_categorical() ~ paste("{p}%",ifelse(show.n," ({n})","")),
                                                    gtsummary::all_continuous2() ~ c(ifelse(robust,"{median}","{mean} ({sd})"),"({p25} - {p75})")),
                                   digits = list(gtsummary::all_categorical() ~ c(digits[1],0),
                                                 gtsummary::all_continuous2() ~ digits[2]),
                                   missing = ifelse(na.rm, "no", "ifany"),
                                   missing_text = na.value)
  res <- gtsummary::add_overall(res, last = TRUE)
  res <- gtsummary::modify_header(res,
                                  label ~ "",
                               gtsummary::all_stat_cols(stat_0 = FALSE) ~ "{level} <br> _(n={n})_",
                               stat_0 ~ "**Total** <br> _(n={n})_")
  res <- gtsummary::modify_footnote(res, gtsummary::all_stat_cols() ~ NA)
  res <- gtsummary::modify_spanning_header(res, gtsummary::all_stat_cols(stat_0 = FALSE) ~ paste0("**",
                                                                                          ifelse(is.null(attr(y,"label")), y_name, attr(y,"label")),
                                                                                          "**"))
  res <- gtsummary::bold_labels(res)
  
  if(show.asso) {
    res  <- gtsummary::add_stat(res, fns = list(gtsummary::all_categorical() ~ my_cramer,
                                     gtsummary::all_continuous2() ~ my_eta))
    res <- gtsummary::modify_header(res, add_stat_1 ~ "**Association**")
    res <- gtsummary::modify_footnote(res, gtsummary::all_stat_cols() ~ NA,
                                 add_stat_1 ~ "Cramer's V (categorical var.) or eta-squared (continuous var.)")
  }
  
  res <- gtsummary::as_gt(res)
  
  res <- gt::text_replace(
      res,
      locations = gt::cells_body(),
      pattern = "Median",
      replacement = "median")
  res <- gt::text_replace(
      res,
      locations = gt::cells_body(),
      pattern = "Mean \\(SD\\)",
      replacement = "mean (sd)")
  res <- gt::text_replace(
      res,
      locations = gt::cells_body(),
      pattern = "IQR",
      replacement = "p25 - p75")
  res <- gt::tab_style(
      res,
      style = list(gt::cell_text(style = "italic",
                                 weight = 350,
                                 color = "grey25"),
                   gt::cell_fill(color = "white")),
      locations = list(
        gt::cells_body(columns = gt::starts_with("stat_0")),
        gt::cells_column_labels(columns = gt::starts_with("stat_0")))
    )
  
  return(res)
    
}
