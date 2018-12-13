test2groupsCov <- function(df, varlist, grVar, gr1, gr2, covar) {
  
  tab_tmp <- psyedutools::test2groups(df, varlist, group = grVar, group2 = gr1, group1 = gr2)
  colnames(tab_tmp)[2:5] <- c("gr1Mean", "gr1SD", "gr2Mean", "gr2SD")
  
  gr1name <- paste0(grVar,"-",gr1)
  gr2name <- paste0(grVar,"-",gr2)
  
  out <- data_frame(Variable = varlist, gr1name = NA, gr2name = NA, F = NA, p = NA, df = NA)
  for (i in 1:length(varlist)) {
    v <- varlist[i]
    f <- formula(paste0(v, "~ ", grVar," * ", covar))
    
    res <- lm(f, data = df) %>% car::Anova() %>% broom::tidy()
    out[[i, "gr1name"]] <- paste0(tab_tmp$gr1Mean[i], " (",tab_tmp$gr1SD[i],")")
    out[[i, "gr2name"]] <- paste0(tab_tmp$gr2Mean[i], " (",tab_tmp$gr2SD[i],")")
    
    out[[i, "F"]] <- res$statistic[1] %>% round(2)
    out[[i, "p"]] <- res$p.value[1] %>% round(3)
    out[[i, "df"]] <- paste0(res$df[1], ", ", res$df[4])
    #out[[i, "eta_sq_p"]] <- res$eta.sq.p[1]
    #out[[i, "eta_sq_p_CI"]] <- paste0("[", res$eta.sq.p.lo[1], ", ", 
    #                                  res$eta.sq.p.hi[1], "]")
    
  }
  return(out)
}

test4groups <- function(df, varlist, grVar = "culture", gr1 = "CM", gr2 = "CZ", gr3 = "DE", gr4 = "HK") {
  n <- length(varlist)
  df_out <- data_frame(varlist, m1 = numeric(n), sd1 = numeric(n), 
                       m2 = numeric(n), sd2 = numeric(n),
                       m3 = numeric(n), sd3 = numeric(n),
                       m4 = numeric(n), sd4 = numeric(n),
                       na1 = numeric(n), na2 = numeric(n), na3 = numeric(n), na4 = numeric(n),  
                       F = numeric(n), df = numeric(n), p_val = numeric(n), eta_p = numeric(n))
  
  gr1 <- df[df[[grVar]] == gr1,]
  gr2 <- df[df[[grVar]] == gr2,]
  gr3 <- df[df[[grVar]] == gr3,]
  gr4 <- df[df[[grVar]] == gr4,]
  
  for(i in 1:length(varlist)){
    v <- varlist[i]
    
    if (is.numeric(df[[v]])) {
      res <- lm(formula(paste0(v,"~culture")), df) %>% car::Anova() %>% broom::tidy()
      
      df_out$m1[i]  <- mean(gr1[[v]], na.rm = T) %>% round(2)
      df_out$sd1[i] <- sd(gr1[[v]], na.rm = T) %>% round(2)
      df_out$na1[i] <- sum(is.na(gr1[[v]]))
      df_out$m2[i]  <- mean(gr2[[v]], na.rm = T) %>% round(2)
      df_out$sd2[i] <- sd(gr2[[v]], na.rm = T) %>% round(2)
      df_out$na2[i] <- sum(is.na(gr2[[v]]))
      df_out$m3[i]  <- mean(gr3[[v]], na.rm = T) %>% round(2)
      df_out$sd3[i] <- sd(gr3[[v]], na.rm = T) %>% round(2)
      df_out$na3[i] <- sum(is.na(gr3[[v]]))
      df_out$m4[i]  <- mean(gr4[[v]], na.rm = T) %>% round(2)
      df_out$sd4[i] <- sd(gr4[[v]], na.rm = T) %>% round(2)
      df_out$na4[i] <- sum(is.na(gr4[[v]]))
      df_out$F[i]  <- res$statistic[1] %>% round(2)
      df_out$p_val[i] <- res$p.value[1] %>% round(3)
      df_out$df[i] <- paste0(res$df[1], ", ", res$df[2])
      df_out$eta_p[i] <- (res$sumsq[1]/(res$sumsq[1] + res$sumsq[2])) %>% round(2)
      
    } else {
      res <- chisq.test(table(df[[grVar]], df[[v]]), simulate.p.value = T) %>% broom::tidy()
      x1 <- table(gr1[[v]])[1]/sum(table(gr1[[v]]))
      x2 <- table(gr2[[v]])[1]/sum(table(gr2[[v]]))
      x3 <- table(gr3[[v]])[1]/sum(table(gr3[[v]]))
      x4 <- table(gr4[[v]])[1]/sum(table(gr4[[v]]))
      df_out$m1[i]  <- mean(x1, na.rm = T) %>% round(2)
      df_out$sd1[i] <- NA
      df_out$na1[i] <- sum(is.na(x1))
      df_out$m2[i]  <- mean(x2, na.rm = T) %>% round(2)
      df_out$sd2[i] <- NA
      df_out$na2[i] <- sum(is.na(x2))
      df_out$m3[i]  <- mean(x3, na.rm = T) %>% round(2)
      df_out$sd3[i] <- NA
      df_out$na3[i] <- sum(is.na(x3))
      df_out$m4[i]  <- mean(x4, na.rm = T) %>% round(2)
      df_out$sd4[i] <- NA
      df_out$na4[i] <- sum(is.na(x4))
      df_out$F[i]  <- res$statistic[1] %>% round(2)
      df_out$p_val[i] <- res$p.value[1] %>% round(3)
      df_out$df[i] <- NA
      df_out$eta_p[i] <- NA
    }
  }
  return(df_out)
}

print_table1 <- function(){
  varlist <- c("age", "sex", "university", "working")
  df_out <- test4groups(df, varlist)
  df_out %>% transmute(var = varlist,
                       CM = paste0(m1,"(",sd1,") /", na1),
                       CZ = paste0(m2,"(",sd2,") /", na2),
                       DE = paste0(m3,"(",sd3,") /", na3),
                       HK = paste0(m4,"(",sd4,") /", na4)) %>%
    return()
}

interaction_plot <- function(df, v, culture_var, ggtit = culture_var) {
  df$newx <- as.character(df[[v]])
  df %>% filter(culture == culture_var) %>% 
    filter(!is.na(newx)) %>% 
    ggplot(aes(y=rheis,x=university, col=newx, group = newx)) +
    stat_summary(fun.data = "mean_cl_boot",position=position_dodge(0.1)) + 
    stat_summary(fun.y = "mean", geom = "line") +
    scale_color_discrete(v) + 
    ggtitle(ggtit)
}

interaction_plot2 <- function(v, culture_var, ggtit = culture_var) {
  df$newx <- as.character(df[[v]])
  df %>% filter(culture == culture_var) %>% 
    filter(!is.na(newx)) %>% 
    ggplot(aes(y=rheis,x=university, col=newx, group = newx)) +
    stat_summary(fun.data = "mean_cl_boot",position=position_dodge(0.1)) + 
    stat_summary(fun.y = "mean", geom = "line") +
    scale_color_discrete(v) + 
    ggtitle(ggtit)
}

print_correlation_table <- function(df, varlist) {
  rc <- df %>% select_at(varlist) %>% as.matrix() %>% Hmisc::rcorr()
  
  
  m <- matrix("", nrow = nrow(rc$r), ncol = ncol(rc$r))
  
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      m[i,j] <- sprintf("%.2f (%d)%s", rc$r[i,j],rc$n[i,j], print_stars(rc$P[i,j]))
    }  
  }
  m[upper.tri(m,diag = T)] <- ""
  colnames(m)<-rownames(m)<-varlist
  m
}

print_stars<-function(pv) {
  
  Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                   symbols = c("***", "**", "*", ".", "")) 
  return(Signif)
}

impute_missing_years <- function(df_life_exp) {
  
  df_CZ <- df_life_exp %>% filter(culture == "CZ")
  df_HK <- df_life_exp %>% filter(culture == "HK")
  df_DE <- df_life_exp %>% filter(culture == "DE")
  df_CM <- df_life_exp %>% filter(culture == "CM")
  
  imputed_CZ <- Hmisc::approxExtrap(df_CZ$birth_year,df_CZ$life_exp, 1900:2000)$y
  imputed_DE <- Hmisc::approxExtrap(df_DE$birth_year,df_DE$life_exp, 1900:2000)$y
  
  imputed_HK <- Hmisc::approxExtrap(df_HK$birth_year,df_HK$life_exp, 1900:2000)$y
  imputed_CM <- Hmisc::approxExtrap(df_CM$birth_year,df_CM$life_exp, 1900:2000)$y
  
  df_life_exp2 <- tibble(culture=rep(c("CZ","HK","DE","CM"), each = 101), 
         birth_year = rep(1900:2000, times = 4),
         life_exp = c(imputed_CZ, imputed_HK, imputed_DE, imputed_CM)
         )
  
  return(df_life_exp2)
  
}
