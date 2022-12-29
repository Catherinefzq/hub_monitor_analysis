#################################################
### Updated December 29 by Catherine           ##
#################################################

# Version 3.0 updates: 
# update to the matching method to Coarsened Exact Matching (CEM)
## IMPORTANT ##
# pls keep a column for K6 score (for tier define use) and name it as `k6_tier_sc` 
# keep ageby_sub and gender (with the value 1/2) [should name in this way]
# e.g. df %>% mutate(k6_tier_sc = k6_score)

# Six lists in the output:
# 1) the matched data (out2[[1]])
# 2) mean & sample size table of matched sample (out2[[2]]) 
# 3) mean & sample size table of crude sample (out2[[3]]) 
# 4) separate paired non-parametric test (out2[[4]])
# 5) mixed ANOVA (out2[[5]])
# 6) the plot (out2[[6]])

# DEMO #
#out2 <- 
# stat_test_cem(hub_first = df_2m_first,
#               hub_last = df_2m_last,
#               control_first = icy_first,
#               control_last = icy_last,
#               var.to.test = c("k6_first", "k6_last"),
#               tier.is = c("T0", "T1", "T2", "T3"),
# the option is removed
#               num_pmt = 5, 
# the option is removed
#               cluster.n = 2,
# CART tiers option, T is using CART tier, F is using old tier
#               cart = F) 

### matching (no longer use this) ###
match_dem_reduct <- function(df_join, match.var, seed_num, cluster.n) {
  # bin by bin matching
  require(dplyr)
  set.seed(seed_num)
  df_join_1 <- df_join %>% 
    select(as.name(match.var), everything()) %>% 
    as.data.frame()
  uni_score <- unique(df_join_1[ ,1])
  # Dimension Reduction Using kmeans
  cl <- kmeans(df_join_1[,1], round(length(uni_score)/cluster.n), nstart = 25)
  uni_bins <- unique(cl$cluster)
  uni_score <- uni_bins
 # Lapply replace for loop
  out <- lapply(1:length(uni_score),
              function(x){
  df_k6 <- df_join_1[cl$cluster == uni_score[x],]
  nsmall <- min(c(sum(df_k6$project == 'Hub'), sum(df_k6$project == 'Control')))
  nsmall[is.na(nsmall) == T] <- 0
  if (nsmall > 0) {
    df_new_hub <- sample_n(df_k6[df_k6$project == 'Hub',],
                           nsmall, replace = FALSE)
    df_new_control <- sample_n(df_k6[df_k6$project == 'Control',],
                               nsmall, replace = FALSE)
    df_new_match <- bind_rows(df_new_control, df_new_hub)
    } else{
    df_new_match <- data.frame()
    }
  return(df_new_match)
  })
  join_out <- 
    do.call(rbind.data.frame, lapply(out, as.data.frame))
  return(join_out)
}


### statistical testing with permutation ###
stat_test_cem <- function(hub_first, hub_last, control_first, control_last,
                      var.to.test, tier.is, num_pmt = 100, cluster.n = 4,
                      cart) {
  require(MatchIt);require(tidyverse);require(gtsummary);
  require(rstatix);require(ggpubr)
 
    # identify which variable are we testing
  if (cart == T){
    # union first last data between hub and control
    df_join_pre <- 
      hub_last %>% 
      janitor::clean_names() %>% 
      select(subject_code, var.to.test[2]) %>% 
      left_join(hub_first %>% 
                  janitor::clean_names() %>% 
                  select(subject_code, var.to.test[1], k6_tier_sc, #sf6_z_first,
                         ageby_sub, gender),
                by = "subject_code") %>% 
      mutate(treated = as.factor(1)) %>% 
      union_all(
        control_last %>% 
          janitor::clean_names() %>% 
          select(subject_code, var.to.test[2]) %>% 
          left_join(control_first %>% 
                      janitor::clean_names() %>% 
                      select(subject_code, var.to.test[1], k6_tier_sc, #sf6_z_first
                             ageby_sub, gender),
                    by = "subject_code") %>% 
          mutate(treated = as.factor(0)) 
      ) %>% 
      ungroup()
    df_join <- 
      df_join_pre %>%
      mutate(tier = case_when(k6_tier_sc < 12 & sf6_z_first >= 0.82 ~ "T0",
                              k6_tier_sc < 12 & sf6_z_first < 0.82 ~ "T1",
                              k6_tier_sc >= 12 & k6_tier_sc < 15 ~ "T2",
                              k6_tier_sc >= 15 & k6_tier_sc <= 24 ~ "T3"),
             hub = gsub("[^a-zA-Z]", "", subject_code),
             project = if_else(treated == 1, "Hub","Control") %>% 
               factor(., levels = c("Hub","Control"))) %>% 
      filter(complete.cases(.) & (tier %in% tier.is)) %>% 
      arrange(project, desc(k6_first))
  }
  else if (cart == F){
    # union first last data between hub and control
      df_join_pre <- 
          hub_last %>% 
          janitor::clean_names() %>% 
          select(subject_code, var.to.test[2]) %>% 
          left_join(hub_first %>% 
                        janitor::clean_names() %>% 
                        select(subject_code, var.to.test[1], k6_tier_sc, 
                               ageby_sub, gender),
                    by = "subject_code") %>% 
          mutate(treated = as.factor(1)) %>% 
          union_all(
              control_last %>% 
                  janitor::clean_names() %>% 
                  select(subject_code, var.to.test[2]) %>% 
                  left_join(control_first %>% 
                                janitor::clean_names() %>% 
                                select(subject_code, var.to.test[1], k6_tier_sc,
                                       ageby_sub, gender),
                            by = "subject_code") %>% 
                  mutate(treated = as.factor(0)) 
          ) %>% 
          ungroup()
    df_join <- 
      df_join_pre %>%
      mutate(tier = case_when(k6_tier_sc >= 0 & k6_tier_sc < 9 ~ "T0",
                              k6_tier_sc >= 9 & k6_tier_sc < 11 ~ "T1",
                              k6_tier_sc >= 11 & k6_tier_sc < 15 ~ "T2",
                              k6_tier_sc >= 15 & k6_tier_sc <= 24 ~ "T3"),
             hub = gsub("[^a-zA-Z]", "", subject_code),
             project = if_else(treated == 1, "Hub","Control") %>% 
               factor(., levels = c("Hub","Control"))) %>% 
      filter(complete.cases(.) & (tier %in% tier.is)) %>% 
      arrange(project, desc(k6_tier_sc))
  }
    else{
      stop("Error in cart: cart is an logical statement")
    }
    ## CEM matching ##
    m.out <- matchit(treated ~ k6_first + gender + ageby_sub,
                     data = as.data.frame(df_join), 
                     method = "cem")
    
    df_match <- df_join %>% 
        mutate(subclass = m.out$subclass) %>% 
        filter(!is.na(subclass))
      
      tb1 <- tbl_summary(df_match %>% dplyr::select(var.to.test[1],
                                                    var.to.test[2], project), 
                         by = project,
                         type =  c(var.to.test[1] ~ "continuous",
                                   var.to.test[2] ~ "continuous"),
                         statistic = all_continuous() ~ "{mean}",
                         digits = all_continuous() ~ 3) %>% 
        modify_header(label = "") %>% 
        modify_footnote(c(all_stat_cols()) ~ NA) 
      
      
      # crude and matched sample size
      tb2 <- tbl_summary(df_join %>% dplyr::select(var.to.test[1], 
                                                   var.to.test[2], project), 
                         by = project,
                         type =  c(var.to.test[1] ~ "continuous",
                                   var.to.test[2] ~ "continuous"),
                         statistic = all_continuous() ~ "{mean}",
                         digits = all_continuous() ~ 3) %>% 
        modify_header(label = "") %>% 
        modify_footnote(c(all_stat_cols()) ~ NA) 
      
      df_plot <- 
        df_match %>% 
        pivot_longer(var.to.test[1]:var.to.test[2],
                     names_to = "visit", values_to = "values") 
      
      # Mixed ANOVA
      res.aov <- anova_test(
        data = df_plot,
        dv = values, wid = subject_code, 
        within = visit, between = project)
      #summary(res.aov)
      
      pwc <- df_plot %>%
        group_by(project) %>%
        pairwise_wilcox_test(
          as.formula(values ~ visit),
          paired = T,
          p.adjust.method = "bonferroni"
        ) %>% 
        add_xy_position(x = "project")
      
      
      list(tb1, tb2, res.aov, pwc, df_match)
    
  # result visualization
  #plotting position
  label.position <- min(pwc$y.position)
  pwc$y.position <- c(label.position, 
                           (label.position - label.position/10))
  pwc$xmin <- 1
  pwc$xmax <- 2
  df_p <- tb1 %>% 
      as_tibble(col_labels = T) %>% 
      t() %>% 
      cbind(sample.size = rownames(.)) %>% 
      as.data.frame() %>% 
      slice(-1) %>% 
      rename(first = V1, last = V2) %>% 
      pivot_longer(first:last, names_to = "visit", values_to = "values") %>% 
      mutate(values = as.numeric(values))
  p <- ggline(
    df_p, x = "visit", y = "values",
    color = "sample.size", palette = "jco") +
    stat_pvalue_manual(pwc, label = "{project}: p = {p}") +
    labs(x = "", y = str_split(var.to.test[1], fixed("_"))[[1]][1],
         subtitle = paste("ANOVA, p =", res.aov$p[3]),
         title = str_c(unique(unlist(tier.is)), collapse = ","),
         caption = paste("p(project) = ", res.aov$p[1], "; ",
                         "p(time) = ", res.aov$p[2],
                         sep = "")) +
    theme(legend.title = element_text(size = 0)) + 
    coord_cartesian(ylim = c(0, label.position + label.position/10))  

  return(list(df_match, tb1, tb2, pwc, res.aov, p))
}

