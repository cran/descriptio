# x = Movies[, c("Genre", "Budget", "Festival", "Critics")]
# y = Movies$BoxOffice
# # y = factor(Movies$BoxOffice >= 200000)
# weights = NULL
# continuous = "slopes"
# show.ci = TRUE
# conf.level = 0.95

regtab <- function(x, y, weights = NULL, continuous = "slopes", show.ci = TRUE, conf.level = 0.95) {
  
  if (!requireNamespace("gtsummary", quietly = TRUE))
    stop("gtsummary package should be installed to use this function")
  
  if (!requireNamespace("gt", quietly = TRUE))
    stop("gt package should be installed to use this function")
  
  gtsummary::set_gtsummary_theme(
    suppressMessages(gtsummary::theme_gtsummary_language(language = "en", decimal.mark = ".", big.mark = " ", ci.sep =" , ")))
  
  if(is.null(weights)) weights <- rep(1, times = nrow(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  ww <- weights
  
  # y_name <- gsub("$", "_", deparse(substitute(y)), fixed = TRUE)
  facs <- names(x)[sapply(x, is.factor)]
  nums <- names(x)[sapply(x, function(x) is.numeric(x) | is.integer(x))]
  
  df <- data.frame(outcome = y, x)
  # names(df)[1] <- y_name
  
  # Predictions marginales univariees
  if(is.numeric(y) | is.integer(y)) {
    tb1b <- 
    gtsummary::tbl_uvregression(data = data.frame(df,ww),
                                method = stats::lm,
                                method.args = list(weights = ww),
                                y = "outcome",
                                include = -ww,
                                hide_n = TRUE,
                                conf.level = conf.level,
                                add_estimate_to_reference_rows = TRUE,
                                tidy_fun = broom.helpers::tidy_marginal_predictions)
  } else if(is.factor(y) & nlevels(y) == 2) {
    tb1b <- 
    gtsummary::tbl_uvregression(data = data.frame(df,ww),
                                method = stats::glm,
                                method.args = list(family = stats::binomial, weights = ww),
                                y = "outcome",
                                include = -ww,
                                type = "response",
                                estimate_fun = scales::label_percent(accuracy = 0.1),
                                hide_n = TRUE,
                                conf.level = conf.level,
                                add_estimate_to_reference_rows = TRUE,
                                tidy_fun = broom.helpers::tidy_marginal_predictions)
  } else {
    stop("y should be numeric or a dichotomous (a factor with two levels)")
  }
  
  # Ajout des pentes (si besoin)
  if(continuous == "predictions") {
      tb1c <- tb1b
  } else if(continuous == "slopes") {
      if(is.numeric(y) | is.integer(y)) {
        tb1a <- gtsummary::tbl_uvregression(data = data.frame(df,ww),
                                            method = stats::lm,
                                            method.args = list(weights = ww),
                                            y = "outcome",
                                            include = -ww,
                                            hide_n = TRUE,
                                            conf.level = conf.level,
                                            add_estimate_to_reference_rows = TRUE,
                                            tidy_fun = broom.helpers::tidy_parameters)
        bod1a <- tb1a$table_body[tb1a$table_body$var_type == "continuous",]
        bod1b <- tb1b$table_body[tb1b$table_body$var_type != "continuous",]
        anotb = names(bod1a)[!(names(bod1a) %in% names(bod1b))]
        bnota = names(bod1b)[!(names(bod1b) %in% names(bod1a))]
        addtoa = setNames(data.frame(matrix(ncol = length(bnota), nrow = nrow(bod1a))), bnota)
        addtob = setNames(data.frame(matrix(ncol = length(anotb), nrow = nrow(bod1b))), anotb)
        bod1a = cbind.data.frame(bod1a, addtoa)
        bod1b = cbind.data.frame(bod1b, addtob)
        newbody <- rbind.data.frame(bod1a, bod1b)
        newbody <- newbody[order(newbody$tbl_id1),]
        tb1c <- tb1a
        tb1c$table_body <- newbody
      } else if(is.factor(y) & nlevels(y) == 2) {
        tb1a <- gtsummary::tbl_uvregression(data = data.frame(df,ww),
                                            method = stats::glm,
                                            method.args = list(family = stats::binomial, weights = ww),
                                            y = "outcome",
                                            include = -ww,
                                            type = "response",
                                            hide_n = TRUE,
                                            conf.level = conf.level,
                                            add_estimate_to_reference_rows = TRUE,
                                            tidy_fun = broom.helpers::tidy_parameters)
        tb1d <- gtsummary::tbl_uvregression(data = data.frame(df,ww),
                                            method = stats::glm,
                                            method.args = list(family = stats::binomial, weights = ww),
                                            y = "outcome",
                                            include = -ww,
                                            type = "response",
                                            hide_n = TRUE,
                                            conf.level = conf.level,
                                            add_estimate_to_reference_rows = TRUE,
                                            tidy_fun = broom.helpers::tidy_avg_slopes)
        amp <- tb1b$table_body[tb1b$table_body$var_class == "factor" & tb1b$table_body$row_type == "level", c("variable", "label", "estimate", "conf.low", "conf.high", "ci")]
        names(amp)[3:6] <- paste0("amp_", names(amp)[3:6])
        body <- tb1a$table_body
        body$id <- 1:nrow(body)
        body <- merge(body, amp, by = c("variable", "label"), all.x = TRUE, sort = FALSE)
        body[body$var_class == "factor" & body$row_type == "level", c("estimate", "conf.low", "conf.high", "ci")] <- body[body$var_class == "factor" & body$row_type == "level", c("amp_estimate", "amp_conf.low", "amp_conf.high", "amp_ci")]
        body$amp_estimate <- body$amp_conf.low <- body$amp_conf.high <- body$amp_ci <- NULL
        slo <- tb1d$table_body[tb1d$table_body$var_type == "continuous" & tb1d$table_body$row_type == "level", c("variable", "estimate", "conf.low", "conf.high", "ci")]
        names(slo)[2:5] <- paste0("slo_", names(slo)[2:5])
        body <- merge(body, slo, by = "variable", all.x = TRUE, sort = FALSE)
        body[body$var_type == "continuous", c("estimate", "conf.low", "conf.high", "ci")] <- body[body$var_type == "continuous", c("slo_estimate", "slo_conf.low", "slo_conf.high", "slo_ci")]
        body$slo_estimate <- body$slo_conf.low <- body$slo_conf.high <- body$slo_ci <- NULL
        body <- body[order(body$id), ]
        body$id <- NULL
        tb1c <- tb1a
        tb1c$table_body <- body
        tb1c <- 
          gtsummary::modify_fmt_fun(
            tb1c,
            gtsummary::starts_with("estimate") ~ scales::label_percent(accuracy = 0.1),
            gtsummary::starts_with("conf") ~ scales::label_percent(accuracy = 0.1),
            rows = .data$variable %in% facs)
        tb1c <- 
          gtsummary::modify_fmt_fun(
            tb1c,
            gtsummary::starts_with("estimate") ~ scales::label_percent(accuracy = 0.1, style_positive = "plus", suffix = " pp"),
            gtsummary::starts_with("conf") ~ scales::label_percent(accuracy = 0.1, style_positive = "plus", suffix = " pp"),
            rows = .data$variable %in% nums)
      }

  }
  
  # masquage des colonnes p.value etc.
  if(isTRUE(show.ci)) {
    tb1c <- gtsummary::modify_column_hide(tb1c, columns = c("p.value", "std.error"))
  } else if(isFALSE(show.ci)) {
    tb1c <- gtsummary::modify_column_hide(tb1c, columns = c("ci", "p.value", "std.error"))
  }

  # Ajustement du modele multivarie
  if(is.numeric(y) | is.integer(y)) {
    mod <- stats::lm(outcome ~ ., data = df, weights = ww)
    tb2b <- gtsummary::tbl_regression(mod,
                                      conf.level = conf.level,
                                      add_estimate_to_reference_rows = TRUE,
                                      tidy_fun = broom.helpers::tidy_marginal_predictions,
                                      vcov = "HC")
  } else if(is.factor(y) & nlevels(y) == 2) {
    mod <- stats::glm(outcome ~ ., data = df, weights = ww, family = stats::binomial)
    tb2b <- gtsummary::tbl_regression(mod,
                                      type = "response",
                                      conf.level = conf.level,
                                      estimate_fun = scales::label_percent(accuracy = 0.1),
                                      add_estimate_to_reference_rows = TRUE,
                                      tidy_fun = broom.helpers::tidy_marginal_predictions)
  }
  
  # Ajout des pentes (si besoin)
  if(continuous == "predictions") {
    tb2c <- tb2b
  } else if(continuous == "slopes") {
    if(is.numeric(y) | is.integer(y)) {
      tb2a <- gtsummary::tbl_regression(mod,
                                        conf.level = conf.level,
                                        add_estimate_to_reference_rows = TRUE,
                                        tidy_fun = broom.helpers::tidy_parameters)
      bod2a <- tb2a$table_body[tb2a$table_body$var_type == "continuous",]
      bod2b <- tb2b$table_body[tb2b$table_body$var_type != "continuous",]
      anotb = names(bod2a)[!(names(bod2a) %in% names(bod2b))]
      bnota = names(bod2b)[!(names(bod2b) %in% names(bod2a))]
      addtoa = setNames(data.frame(matrix(ncol = length(bnota), nrow = nrow(bod2a))), bnota)
      addtob = setNames(data.frame(matrix(ncol = length(anotb), nrow = nrow(bod2b))), anotb)
      bod2a = cbind.data.frame(bod2a, addtoa)
      bod2b = cbind.data.frame(bod2b, addtob)
      newbody <- rbind.data.frame(bod2a, bod2b)
      newbody <- do.call("rbind.data.frame", split(newbody, factor(newbody$variable))[names(x)])
      tb2c <- tb2a
      tb2c$table_body <- newbody
    } else if(is.factor(y) & nlevels(y) == 2) {
      tb2a <- gtsummary::tbl_regression(mod,
                                        type = "response",
                                        conf.level = conf.level,
                                        add_estimate_to_reference_rows = TRUE,
                                        tidy_fun = broom.helpers::tidy_parameters)
      tb2d <- gtsummary::tbl_regression(mod,
                                        type = "response",
                                        conf.level = conf.level,
                                        add_estimate_to_reference_rows = TRUE,
                                        tidy_fun = broom.helpers::tidy_avg_slopes)
      amp <- tb2b$table_body[tb2b$table_body$var_class == "factor" & tb2b$table_body$row_type == "level", c("variable", "label", "estimate", "conf.low", "conf.high", "ci")]
      names(amp)[3:6] <- paste0("amp_", names(amp)[3:6])
      body <- tb2a$table_body
      body$id <- 1:nrow(body)
      body <- merge(body, amp, by = c("variable", "label"), all.x = TRUE, sort = FALSE)
      body[body$var_class == "factor" & body$row_type == "level", c("estimate", "conf.low", "conf.high", "ci")] <- body[body$var_class == "factor" & body$row_type == "level", c("amp_estimate", "amp_conf.low", "amp_conf.high", "amp_ci")]
      body$amp_estimate <- body$amp_conf.low <- body$amp_conf.high <- body$amp_ci <- NULL
      slo <- tb2d$table_body[tb2d$table_body$var_type == "continuous" & tb2d$table_body$row_type == "level", c("variable", "estimate", "conf.low", "conf.high", "ci")]
      names(slo)[2:5] <- paste0("slo_", names(slo)[2:5])
      body <- merge(body, slo, by = "variable", all.x = TRUE, sort = FALSE)
      body[body$var_type == "continuous", c("estimate", "conf.low", "conf.high", "ci")] <- body[body$var_type == "continuous", c("slo_estimate", "slo_conf.low", "slo_conf.high", "slo_ci")]
      body$slo_estimate <- body$slo_conf.low <- body$slo_conf.high <- body$slo_ci <- NULL
      body <- body[order(body$id), ]
      body$id <- NULL
      tb2c <- tb2a
      tb2c$table_body <- body
      tb2c <- 
        gtsummary::modify_fmt_fun(
          tb2c,
          gtsummary::starts_with("estimate") ~ scales::label_percent(accuracy = 0.1),
          gtsummary::starts_with("conf") ~ scales::label_percent(accuracy = 0.1),
          rows = .data$variable %in% facs)
      tb2c <- 
        gtsummary::modify_fmt_fun(
          tb2c,
          gtsummary::starts_with("estimate") ~ scales::label_percent(accuracy = 0.1, style_positive = "plus", suffix = " pp"),
          gtsummary::starts_with("conf") ~ scales::label_percent(accuracy = 0.1, style_positive = "plus", suffix = " pp"),
          rows = .data$variable %in% nums)
    }
  }

  # Masquage des colonnes p.value, etc.
  if(isTRUE(show.ci)) {
    tb2c <- gtsummary::modify_column_hide(tb2c, columns = c("p.value", "std.error"))
  } else if(isFALSE(show.ci)) {
    tb2c <- gtsummary::modify_column_hide(tb2c, columns = c("ci", "p.value", "std.error"))
  }
  
  # Fusion des tableaux
  tb1c <- gtsummary::remove_abbreviation(tb1c)
  tb2c <- gtsummary::remove_abbreviation(tb2c)
  res <- gtsummary::tbl_merge(list(tb1c, tb2c))
  res <- gtsummary::bold_labels(res)
  res <- 
    gtsummary::modify_post_fmt_fun(
      res,
      fmt_fun = ~ gsub("^(.*)$", "\\[\\1\\]", .),
      columns = gtsummary::starts_with("conf.low"),
      rows = !is.na(.data$conf.low_1)
  )

  # Intitules des colonnes
  if(isTRUE(show.ci)) {
    res <- 
      gtsummary::modify_header(res,
                               label ~ "",
                               # gtsummary::starts_with("ci") ~ "95% CI",
                               gtsummary::starts_with("estimate_") ~ "**AME**")
    # res <- gtsummary::modify_footnote(res, gtsummary::everything() ~ NA, abbreviation = TRUE)
    res <- gtsummary::modify_spanning_header(
                                        res,
                                        gtsummary::ends_with("_1") ~ "**univariate**",
                                        gtsummary::ends_with("_2") ~ "**multivariate**")
  } else if(isFALSE(show.ci)) {
    res <- gtsummary::modify_header(res,
                                    label ~ "",
                                    estimate_1 ~ "univariate",
                                    estimate_2 ~ "multivariate")
    res <- gtsummary::modify_spanning_header(res, gtsummary::starts_with("estimate") ~ "**average marginal effects**")
  }
  
  res <- gtsummary::remove_abbreviation(res)
  # show_header_names(res)
  # gtsummary::reset_gtsummary_theme()
  return(res)
  
}
