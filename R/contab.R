contab <- function(x,
                   y,
                   strata = NULL,
                   weights = NULL,
                   robust = TRUE,
                   digits = c(1,3),
                   na.rm = TRUE,
                   na.value = "NAs") {
  
  if (!requireNamespace("gt", quietly = TRUE))
    stop("gt package should be installed to use this function")
  
  if(is.null(weights)) weights <- rep(1, times = nrow(x))
  if(any(is.na(weights))) stop("There are empty values in weights.")
  
  my_eta <- function(data, variable, by, ...) {
    summary.lm(aov(data$variables[[variable]] ~ data$variables[[by]], weights = weights))$r.squared
  }
  
  compute1 <- function(y, x, weights) {
    x_name <- gsub("$", "_", deparse(substitute(x)), fixed = TRUE)
    if(is.factor(x)) {
      df <- data.frame(x, y, weights)
      spl <- split(df, df$x)
      if(robust) {
        centr <- round(sapply(spl, function(data) with(data, ifelse(nrow(data)>0, weighted.quantile(y, weights, probs = 0.5, na.rm = TRUE), NaN))), digits[1])
        disp <- round(sapply(spl, function(data) with(data, ifelse(nrow(data)>0, weighted.mad(y, weights, na.rm = TRUE), NaN))), digits[1])
      } else {
        centr <- round(sapply(spl, function(data) with(data, weighted.mean(y, weights, na.rm = TRUE))), digits[1])
        disp <- round(sapply(spl, function(data) with(data, weighted.sd(y, weights, na.rm = TRUE))), digits[1])
      }
      eta <- round(stats::summary.lm(stats::aov(y ~ x, weights = weights))$r.squared,3)
      labs <- c(ifelse(is.null(attr(x,"label")), x_name, attr(x,"label")),
                levels(x))
      stat <- c("", paste0(centr, " (", disp, ")"))
      asso <- c(eta, rep("", nlevels(x)))
      res <- data.frame(labs, stat.1 = stat, asso)
      return(res)
    } else if(is.numeric(x)) {
      cor <- assoc.twocont(x,y,weights,na.rm = TRUE)
      labs <- ifelse(is.null(attr(x,"label")), x_name, attr(x,"label"))
      stat <- round(stats::lm(y ~ x, weights = weights)$coefficients[2], digits[2])
      asso <- paste(round(cor$pearson,3), "/", round(cor$kendall,3))
      res <- data.frame(labs, stat.1 = stat, asso)
      return(res)
    }
  }
  
  computeAll <- function(y, x, weights) {
    res <- list()
    for(i in 1:length(x)) {
      res[[i]] <- compute1(y, x[[i]], weights)
      res[[i]]$labs[1] <- names(x)[i]
    }
    res <- do.call("rbind.data.frame", res)
    return(res)
  }
  
  if(is.null(strata)) {
    tab0 <- computeAll(y,x,weights)
  } else {
    df <- data.frame(y = y , weights = weights, x)
    spl <- split(df, strata)
    tab0 <- lapply(spl, function(data) computeAll(y = data$y, x = data[,c(-1,-2)], weights = data$weights))
    tab0 <- do.call("cbind.data.frame", tab0)
    names(tab0)[1] <- "labs"
    names(tab0)[seq(from=2, by=3, length.out=nlevels(strata))] <- paste("stat", 1:nlevels(strata), sep=".")
    names(tab0)[seq(from=3, by=3, length.out=nlevels(strata))] <- paste("asso", 1:nlevels(strata), sep=".")
    tab0[, seq(from=4, by=3, length.out=nlevels(strata)-1)] <- NULL
  }
  
  tab1 <- gt::gt(tab0)
  tab1 <- gt::tab_style(
                  tab1,
                  style = list(gt::cell_text(weight = "bold", indent = gt::pct(0))),
                  locations = gt::cells_body(columns = labs,
                                             rows = !grepl("\\(", .data$stat.1)))
  tab1 <- gt::tab_style(
                  tab1,
                  style = list(gt::cell_text(indent = gt::pct(20))),
                  locations = gt::cells_body(columns = labs,
                                             rows = grepl("\\(", .data$stat.1)))
  tab1 <- gt::tab_style(
                  tab1,
                  style = list(gt::cell_text(align = "center",
                                             v_align = "middle")),
                  locations = list(gt::cells_body(columns = gt::starts_with(c("stat","asso"))),
                                   gt::cells_column_labels(columns = gt::starts_with(c("stat","asso")))))
  tab1 <- gt::text_replace(
                     tab1,
                     locations = gt::cells_body(columns = gt::starts_with("stat")),
                     pattern = " \\(",
                     replacement = "<br>(")
  tab1 <- gt::cols_label(
                   tab1,
                   labs = "",
                   gt::starts_with("asso") ~ "association")
  tab1 <- gt::cols_width(tab1, labs ~ px(150))
  tab1 <- gt::tab_style(
                  tab1,
                  style = gt::cell_borders(sides = "top",
                                           color = "gray80",
                                           weight = gt::px(2),
                                           style = "solid"),
                  locations =  list(gt::cells_body(rows = !grepl("\\(", .data$stat.1))))

  if(robust) {
    tab1 <- gt::cols_label(
                     tab1,
                     labs = "",
                     gt::starts_with("stat") ~ gt::html("median<br>(mad)"))
  } else {
    tab1 <- gt::cols_label(
                     tab1,
                     labs = "",
                     gt::starts_with("stat") ~ gt::html("mean<br>(sd)"))
  }
  
  if(!is.null(strata)) {
    strata_name <- gsub("$", "_", deparse(substitute(strata)), fixed = TRUE)
    for(i in 1:nlevels(strata)) {
      tab1 <- gt::tab_spanner(tab1, 
                              label = gt::md(paste0("**",levels(strata)[i],"**")),
                              columns = gt::ends_with(as.character(i)))
      }
    tab1 <- gt::tab_spanner(
                       tab1,
                       label = gt::md(paste0("**",ifelse(is.null(attr(strata,"label")), strata_name, attr(strata,"label")),"**")),
                       spanners = paste0("**",levels(strata),"**"))
  }
  
  return(tab1)

}
