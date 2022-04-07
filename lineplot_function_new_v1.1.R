####################################
### Updated March 11 by Catherine ##
####################################

# Version 1.1 updates: 
# 1. fixed the p-values for groups not showing completely in plots
# 2. simplify the if else process, remove duplicate process
# 3. clarify the demo `tier[[1]]` part.
# 4. remove arguments tier.t0plus1 and rename argument tier.1 -> tier.is
# 5. return matched data frame instead of join data frame

# DEMO (not run)

# Specify tiers options in a list (for the use of iterations)
# tiers <- list(c("T0", "T1", "T2", "T3"),
#               c("T0", "T1),
#               c("T2"),
#               c("T3"))
# OR #
# tiers.v2 <- list(c("T0", "T1", "T2", "T3"),
#                  c("T0"),
#                  c("T1", "T2"),
#                  c("T3"))

# Specify hub names in a list (for the use of iterations)
# hubs <- str_split(paste(
#  c("BGLC","FYTW","PAKT","PAWT","CALM","CAKC","SJCW","CYSS"),
#  "ICY"), " ")
# OR # 
# hubs <- str_split(paste(
#  c("BGLC","FYTW","PAKT","PAWT","CALM","CAKC","SJCW","CYSS"),
#  "YES"), " ")
# OR #
# hubs <- c("HUB", "YES")

# Try it on PHQ-9 comparing QCT hub and YES in T2
# phq9.out <- stat_test(hub_first =  hub_first,
#                       hub_last = hub_last,
#                       control_first = yes_first,
#                       control_last = yes_last,
#                       var.to.test = c("phq9_first", "phq9_last"), 
#                       n.bin = 27,
#                       tier.is = c("T2"), 
#                       hub.name = c("HUB","YES"))
# 
# *extract output: phq9.out[[1]], phq9.out[[2]], etc.
# *for different tiers please change the number in the bracket, e.g., tiers[[2]]
# *for different hub.name do it the same way, e.g., hubs[[2]]
#
# Six lists in the output:
# 1) plot 2) matched sample means 3) crude sample mean 
# 4) mixed ANOVA 5) separate paired non-parametric test 
# 6) match data frame of first four arguments

### matching ###
bin_by_bin_matching <- function(df_join, n.bin, match.var) {
  # bin by bin matching
  require(dplyr)
  set.seed(1234)
  df_new_match <- data.frame()
  df_join_1 <- df_join %>% select(as.name(match.var), everything())
  for (i in 0:n.bin) {
    df_k6 <- df_join_1[df_join_1[1] == i,]
    nsmall <- min(c(sum(df_k6$project == 'Hub'),sum(df_k6$project == 'Control')))
    if (nsmall > 0) {
      df_new_hub <- sample_n(df_k6[df_k6$project == 'Hub',],nsmall,replace = FALSE)
      df_new_control <- sample_n(df_k6[df_k6$project == 'Control',],nsmall,replace = FALSE)
      df_new_match <- rbind(df_new_match, df_new_control, df_new_hub)
      #df_new_match <- rbind(df_new_match, df_new_hub)
    }    
  }
  return(df_new_match)
}

### statistical testing ###
stat_test <- function(hub_first, hub_last, control_first, control_last,
                      var.to.test, tier.is, hub.name, n.bin) {
  require(MatchIt);require(tidyverse);require(gtsummary);
  require(rstatix);require(ggpubr)
  # union first last data between hub and control
  df_join_pre <- 
    hub_last %>% 
    janitor::clean_names() %>% 
    select(subject_code, var.to.test[2]) %>% 
    left_join(hub_first %>% 
                janitor::clean_names() %>% 
                select(subject_code, var.to.test[1], k6_first),
              by = "subject_code") %>% 
    mutate(project = 1) %>% 
    union_all(
      control_last %>% 
        janitor::clean_names() %>% 
        select(subject_code, var.to.test[2]) %>% 
        left_join(control_first %>% 
                    janitor::clean_names() %>% 
                    select(subject_code, var.to.test[1], k6_first),
                  by = "subject_code") %>% 
        mutate(project = 0) 
    ) %>% 
    ungroup()
    # identify which variable are we testing
    df_join <- 
      df_join_pre %>%
      mutate(tier = case_when(k6_first %in% c(0:8) ~ "T0",
                              k6_first %in% c(9:10) ~ "T1",
                              k6_first %in% c(11:14) ~ "T2",
                              k6_first %in% c(15:24) ~ "T3"),
             hub = gsub("[^a-zA-Z]", "", subject_code),
             project = if_else(project == 1, "Hub","Control") %>% 
               factor(., levels = c("Hub","Control"))) %>% 
      filter(complete.cases(.) & (tier %in% tier.is)
             & (hub %in% hub.name)) %>% 
      arrange(project, desc(k6_first))
    
    if (length(unique(df_join$project)) < 2) {
      return(as.list(rep(c(NA),6)))
      }
    else {
      df_match <- bin_by_bin_matching(
        df_join, n.bin, match.var = var.to.test[1]) %>% 
        select(subject_code, var.to.test[1]:var.to.test[2], everything())
    }
    
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
  #plotting position
  pwc$y.position <- c(n.bin, (n.bin - 5))
  pwc$xmin <- 1
  pwc$xmax <- 2
  # result visualization
  p <- ggline(
    df_plot, x = "visit", y = "values",
    color = "project", palette = "jco", 
    add = "mean") +
    stat_pvalue_manual(pwc, label = "{project} (N = {n1}): p = {p}") +
    labs(x = "", y = str_split(var.to.test[1], fixed("_"))[[1]][1],
         subtitle = get_test_label(res.aov),
         title = str_c(unique(unlist(hub.name)), collapse = ",")
    ) +
    coord_cartesian(ylim = c(0, n.bin + 1))  
  return(list(p, tb1, tb2, res.aov, pwc, df_match))
}
