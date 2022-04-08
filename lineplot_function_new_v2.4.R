#################################################
### Updated April 07 by Catherine and Charlton ##
#################################################

# Version 2.4 updates: 
# apply to average K6 score as first visit
## IMPORTANT ##
# pls keep a column for K6 score (for tier define use) and name it as `k6_tier_sc` 
# e.g. df %>% mutate(k6_tier_sc = k6_score)

# Six lists in the output:
# 1) mean & sample size table (out2[[1]])
# 2) mixed ANOVA (out2[[2]])
# 3) separate paired non-parametric test (out2[[3]])
# 4) match data frame in each permutation (out2[[4]])

# DEMO #
#out2 <- 
# stat_test_perm(hub_first = df_2m_first,
#               hub_last = df_2m_last,
#               control_first = icy_first,
#               control_last = icy_last,
#               var.to.test = c("k6_first", "k6_last"),
#               tier.is = c("T0", "T1", "T2", "T3"),
# number of permutation, please test run with small number first
#               num_pmt = 5, 
# number of unique score in each cluster, suggest 4 for sf6, 2 for K6
#               cluster.n = 2,
# CART tiers option, T is using CART tier, F is using old tier
#               cart = F) 

### matching ###
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
stat_test_perm <- function(hub_first, hub_last, control_first, control_last,
                      var.to.test, tier.is, num_pmt, cluster.n = 4,
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
                  select(subject_code, var.to.test[1], k6_tier_sc, sf6_z_first),
                by = "subject_code") %>% 
      mutate(project = 1) %>% 
      union_all(
        control_last %>% 
          janitor::clean_names() %>% 
          select(subject_code, var.to.test[2]) %>% 
          left_join(control_first %>% 
                      janitor::clean_names() %>% 
                      select(subject_code, var.to.test[1], k6_tier_sc, sf6_z_first),
                    by = "subject_code") %>% 
          mutate(project = 0) 
      ) %>% 
      ungroup()
    df_join <- 
      df_join_pre %>%
      mutate(tier = case_when(k6_tier_sc < 12 & sf6_z_first >= 0.82 ~ "T0",
                              k6_tier_sc < 12 & sf6_z_first < 0.82 ~ "T1",
                              k6_tier_sc >= 12 & k6_tier_sc < 15 ~ "T2",
                              k6_tier_sc >= 15 & k6_tier_sc <= 24 ~ "T3"),
             hub = gsub("[^a-zA-Z]", "", subject_code),
             project = if_else(project == 1, "Hub","Control") %>% 
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
                  select(subject_code, var.to.test[1], k6_tier_sc),
                by = "subject_code") %>% 
      mutate(project = 1) %>% 
      union_all(
        control_last %>% 
          janitor::clean_names() %>% 
          select(subject_code, var.to.test[2]) %>% 
          left_join(control_first %>% 
                      janitor::clean_names() %>% 
                      select(subject_code, var.to.test[1], k6_tier_sc),
                    by = "subject_code") %>% 
          mutate(project = 0) 
      ) %>% 
      ungroup()
    df_join <- 
      df_join_pre %>%
      mutate(tier = case_when(k6_tier_sc >= 0 & k6_tier_sc < 9 ~ "T0",
                              k6_tier_sc >= 9 & k6_tier_sc < 11 ~ "T1",
                              k6_tier_sc >= 11 & k6_tier_sc < 15 ~ "T2",
                              k6_tier_sc >= 15 & k6_tier_sc <= 24 ~ "T3"),
             hub = gsub("[^a-zA-Z]", "", subject_code),
             project = if_else(project == 1, "Hub","Control") %>% 
               factor(., levels = c("Hub","Control"))) %>% 
      filter(complete.cases(.) & (tier %in% tier.is)) %>% 
      arrange(project, desc(k6_tier_sc))
  }
    else{
      stop("Error in cart: cart is an logical statement")
    }
    res.aov_pmt = data.frame(); pwc_pmt = data.frame(); 
    df_match_pmt = data.frame()
    
    require(foreach);require(doParallel)
    registerDoParallel(4)
    out <- 
    foreach (i = 1:num_pmt) %dopar% {
      
      if (length(unique(df_join$project)) < 2) {
        return(as.list(rep(c(NA),6)))
      }
      else {
        df_match <- match_dem_reduct(
          df_join, match.var= var.to.test[1], seed_num = i,
          cluster.n = cluster.n) %>% 
          select(subject_code, var.to.test[1]:var.to.test[2], everything())
      }
      df_match$cnt_pmt <- i
      df_match_pmt <- rbind(df_match_pmt, df_match)
      
      tb1 <- tbl_summary(df_match %>% dplyr::select(var.to.test[1],
                                                    var.to.test[2], project), 
                         by = project,
                         type =  c(var.to.test[1] ~ "continuous",
                                   var.to.test[2] ~ "continuous"),
                         statistic = all_continuous() ~ "{mean}",
                         digits = all_continuous() ~ 3) %>% 
        modify_header(label = "") %>% 
        modify_footnote(c(all_stat_cols()) ~ NA) 
      
      tb1_pmt <- as_tibble(tb1)
      
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
      
      tb2_pmt <- as_tibble(tb2)
      
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
      res.aov_pmt <- rbind(res.aov_pmt,res.aov)
      
      pwc <- df_plot %>%
        group_by(project) %>%
        pairwise_wilcox_test(
          as.formula(values ~ visit),
          paired = T,
          p.adjust.method = "bonferroni"
        ) %>% 
        add_xy_position(x = "project")
      
      pwc_pmt = rbind(pwc_pmt,pwc)
      
      list(tb1_pmt, tb2_pmt, res.aov_pmt, pwc_pmt, df_match_pmt)
    }
    
  out.trans <- purrr::transpose(out)
  proc_fun <- function(x){t(x)[-1,]}
  
  anova.out <- do.call(rbind.data.frame, lapply(out.trans[[3]], as.data.frame))
  pwr.test <- do.call(rbind.data.frame, lapply(out.trans[[4]], as.data.frame))
  mean.all <- tibble(sample.size = rep(names(out.trans[[1]][[1]])[-1], num_pmt),
                     first = do.call(
                       rbind.data.frame, lapply(out.trans[[1]], proc_fun))$V1 %>% 
                       as.numeric(),
                     last = do.call(
                       rbind.data.frame, lapply(out.trans[[1]], proc_fun))$V2 %>% 
                       as.numeric()) %>% 
    bind_rows(tibble(sample.size = rep(paste(names(out.trans[[2]][[1]])[-1], "crude"),
                                       num_pmt),
                     first = do.call(
                       rbind.data.frame, lapply(out.trans[[2]], proc_fun))$V1 %>% 
                       as.numeric(),
                     last = do.call(
                       rbind.data.frame, lapply(out.trans[[2]], proc_fun))$V2 %>% 
                       as.numeric()))
  tb1 <- aggregate(cbind(first, last) ~ sample.size, data = mean.all, mean)
  anova.sum <- aggregate(cbind(p, ges) ~ Effect, data = anova.out, mean)   
  test.sum <- aggregate(p ~ project, data = pwr.test, mean) %>% 
    left_join(pwr.test[1:2,], by = "project")
  # result visualization
  #plotting position
  label.position <- min(pwr.test$y.position)
  test.sum$y.position <- c(label.position, 
                           (label.position - label.position/10))
  test.sum$xmin <- 1
  test.sum$xmax <- 2
  df_p <- tb1 %>% filter(!grepl('crude', sample.size)) %>% 
    pivot_longer(first:last, names_to = "visit", values_to = "values")
  p <- ggline(
    df_p, x = "visit", y = "values",
    color = "sample.size", palette = "jco") +
    stat_pvalue_manual(test.sum, label = "{project}: p = {p.x}") +
    labs(x = "", y = str_split(var.to.test[1], fixed("_"))[[1]][1],
         subtitle = paste("ANOVA, p =", anova.sum[2,2]),
         title = str_c(unique(unlist(tier.is)), collapse = ","),
         caption = paste("p(project) = ", anova.sum[1,2], "; ",
                         "p(time) = ", anova.sum[3,2],
                         sep = "")) +
    theme(legend.title = element_text(size = 0)) + 
    coord_cartesian(ylim = c(0, label.position + label.position/10))  

  return(list(tb1, anova.sum, test.sum, p, out.trans[[5]]))
}