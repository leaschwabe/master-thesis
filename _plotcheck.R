setwd("c:/github/master-thesis")
ok <- TRUE
msg <- character()
tryCatch({
  suppressPackageStartupMessages({library(dplyr); library(tidyr); library(zoo); library(ggplot2)})
  source("code/plot_theme.R")
  FIGDIR <- tempdir()

  qs <- as.yearqtr("2017 Q1", "%Y Q%q") + (0:11)/4
  quarterly_prices <- tibble(quarter = rep(qs,3),
    series = rep(c("Composite","EFH","ETW"), each=12),
    value = runif(36, 1000, 5000)) %>% group_by(series) %>% arrange(quarter) %>%
    mutate(index = 100*value/first(value)) %>% ungroup()
  quarterly_prices_thesis <- quarterly_prices %>%
    mutate(series = recode(series, Composite="Composite", EFH="House", ETW="Apartment"),
           series = factor(series, levels=c("Composite","House","Apartment")))
  p1 <- ggplot(quarterly_prices_thesis, aes(quarter, index, colour=series)) + geom_line(linewidth=.9) +
    scale_x_yearqtr(format="%Y", n=5) + scale_colour_thesis() +
    labs(x=NULL, y="Index (Q1 2017 = 100)", colour=NULL) + theme_thesis()
  ggsave_thesis(file.path(FIGDIR,"t1.pdf"), p1)

  shock_panel <- tibble(quarter=qs, shock_neg_ois20y=-runif(12,0,3), shock_pos_ois20y=runif(12,0,3),
                        shock_net_ois20y=runif(12,-2,2))
  shock_components <- shock_panel %>% select(quarter, Easing=shock_neg_ois20y, Tightening=shock_pos_ois20y) %>%
    pivot_longer(-quarter, names_to="component", values_to="value")
  shock_net_thesis <- shock_panel %>% select(quarter, net=shock_net_ois20y)
  p2 <- ggplot() + geom_hline(yintercept=0, colour="grey50") +
    geom_col(data=shock_components, aes(quarter, value, fill=component), position="identity", alpha=.85, width=.18) +
    geom_line(data=shock_net_thesis, aes(quarter, net), colour="grey25", linewidth=.4) +
    geom_point(data=shock_net_thesis, aes(quarter, net, shape="Net shock"), colour="grey15", size=1.6) +
    scale_x_yearqtr(format="%Y", n=5) + scale_fill_thesis() + scale_shape_manual(values=c("Net shock"=16)) +
    labs(x=NULL, y="Quarterly shock sum (OIS 20Y)", fill=NULL, shape=NULL) + theme_thesis()
  ggsave_thesis(file.path(FIGDIR,"t2.pdf"), p2)

  key_corr_vars <- c("dQ_composite","d_lincome","d_lpop","d_unemp","shock","shock_ease","shock_tight","blr_14","w_house","shk_x_inel")
  m <- matrix(runif(100,-1,1), 10, dimnames=list(key_corr_vars, key_corr_vars)); m[lower.tri(m)] <- t(m)[lower.tri(m)]; diag(m) <- 1
  cor_long <- function(cmat) as.data.frame(as.table(cmat), stringsAsFactors=FALSE) %>% rename(variable_1=Var1, variable_2=Var2, correlation=Freq)
  corr_labels <- c(dQ_composite="Composite price growth", d_lincome="Income growth", d_lpop="Population growth",
    d_unemp="Unemployment change", shock="Monetary policy shock", shock_ease="Easing surprise", shock_tight="Tightening surprise",
    blr_14="Building-land reserve", w_house="House weight", shk_x_inel="Shock \u00d7 inelastic")
  relabel_corr <- function(x) ifelse(is.na(corr_labels[x]), x, corr_labels[x])
  d <- cor_long(m) %>% mutate(
    variable_1=factor(relabel_corr(variable_1), levels=unname(corr_labels[key_corr_vars])),
    variable_2=factor(relabel_corr(variable_2), levels=unname(corr_labels[key_corr_vars])))
  p3 <- ggplot(d, aes(variable_1, variable_2, fill=correlation)) + geom_tile(colour="white", linewidth=.3) +
    geom_text(aes(label=sprintf("%.2f",correlation)), size=2.4) +
    scale_fill_gradient2(low="#2166ac", mid="white", high="#b2182b", limits=c(-1,1)) + coord_equal() +
    labs(x=NULL,y=NULL,fill="Correlation") + theme_thesis() +
    theme(axis.text.x=element_text(angle=45,hjust=1), panel.grid.major=element_blank(), legend.position="right")
  ggsave_thesis(file.path(FIGDIR,"t3.pdf"), p3, height=13)

  msg <- c(msg, paste("t1 exists:", file.exists(file.path(FIGDIR,"t1.pdf"))))
  msg <- c(msg, paste("t2 exists:", file.exists(file.path(FIGDIR,"t2.pdf"))))
  msg <- c(msg, paste("t3 exists:", file.exists(file.path(FIGDIR,"t3.pdf"))))
}, error=function(e){ ok <<- FALSE; msg <<- c(msg, paste("ERROR:", conditionMessage(e))) })
writeLines(c(paste("OK:", ok), msg), "c:/github/master-thesis/_plotcheck.txt")
