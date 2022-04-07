### DEMO for functions ###

# hub_f <- set_data(subset(df_serv, service_grouping == "1.8"), 
#                   df_2m_demo$subject_code, "first", "k6_score") %>% 
#   rename(k6_first = score_first)
# hub_l <- set_data(subset(df_serv, service_grouping == "1.8"), 
#                   df_2m_demo$subject_code, "last", "k6_score") %>% 
#   rename(k6_last = score_last)

# var.grid <- expand.grid(hubs, tiers)
# s1.out <- mapply(function(x, y){
#   stat_test(hub_first = hub_f, hub_last = hub_l,
#             control_first = df_yes_bl, control_last = df_yes_3m,
#             var.to.test = c("k6_first", "k6_last"),
#             n.bin = 24, 
#             tier.is = x,
#             hub.name = y)
# }, x = var.grid$Var2, y = var.grid$Var1)

# output_excel(s1.out, var.grid, "result1")

### select different first last visit ###
### For 2M data ONLY ##
# data = raw data, id_limit = df_2m_demo$subject_code, 
# select = type of visit to select, var.select = variable to select
set_data <- function(data, id_limit, select, var.select){
  df_set <- data %>% 
    janitor::clean_names() %>% 
    arrange(subject_code, submit_time) %>% 
    group_by(subject_code) %>% 
    filter(subject_code %in% id_limit & n() > 1) %>% 
    mutate(no_of_2m = 1:n(),
           days_of_2m = as.Date(submit_time) - first(as.Date(submit_time))) %>% 
    rename(score = all_of(var.select))
  # select max 
  if (select == "3max_1") {
    df_first_3max_1 <- df_set %>% 
      group_by(subject_code) %>% 
      slice(-n()) %>% 
      filter(no_of_2m <= 3) %>% 
      arrange(desc(score)) %>% 
      slice(1) %>% 
      rename(score_first = score)
    return(df_first_3max_1)
  }
  else if (select == "5max_1") {
    df_first_5max_1 <- df_set %>% 
      group_by(subject_code) %>% 
      slice(-n()) %>% 
      filter(no_of_2m <= 5) %>% 
      arrange(desc(score)) %>% 
      slice(1) %>% 
      rename(score_first = score)
    return(df_first_5max_1)
  }
  else if (select == "3max_2") {
    df_first_3max_2 <- df_set %>% 
      group_by(subject_code) %>% 
      slice(-(n():(n() - 1))) %>% 
      filter(no_of_2m <= 3) %>% 
      arrange(desc(score)) %>% 
      slice(1) %>% 
      rename(score_first = score)
    return(df_first_3max_2)
  }
  else if (select == "5max_2") {
    df_first_5max_2 <- df_set %>% 
      group_by(subject_code) %>% 
      slice(-(n():(n() - 1))) %>% 
      filter(no_of_2m <= 5) %>% 
      arrange(desc(score), submit_time) %>% 
      slice(1) %>% 
      rename(score_first = score)
    return(df_first_5max_2)
  }
  else if (select == "3max_3") {
    df_first_3max_3 <- df_set %>% 
      group_by(subject_code) %>% 
      slice(-(n():(n() - 1))) %>% 
      slice(-n()) %>% 
      filter(no_of_2m <= 3) %>% 
      arrange(desc(score)) %>% 
      slice(1) %>% 
      rename(score_first = score)
    return(df_first_3max_3)
  }
  else if (select == "avg_2w") {
    df_first_avg_2w <- df_set %>%
      filter(days_of_2m <= 14) %>% 
      group_by(subject_code) %>% 
      mutate(score_first = mean(score, na.rm = T)) %>% 
      slice(1) 
    return(df_first_avg_2w)
  }
  else if (select == "first") {
    df_first <- df_set %>% 
      group_by(subject_code) %>% 
      slice(1) %>% 
      rename(score_first = score)
    return(df_first)
  }
  # max within first 2 weeks
  else if (select == "max_2w") {
    df_max_2w <- df_set %>%
      filter(days_of_2m <= 14) %>% 
      group_by(subject_code) %>% 
      arrange(desc(score)) %>% 
      slice(1) %>% 
      rename(score_first = score)
    return(df_max_2w)
  }
  else if (select == "fu_avg_af1m") {
    df_fu_avg_af1m <- df_set %>%
      filter(days_of_2m >= 30) %>% 
      group_by(subject_code) %>% 
      mutate(score_last = mean(score, na.rm = T)) %>% 
      slice(n())
    return(df_fu_avg_af1m)
  }
  # after 2 weeks min of all fu
  else if (select == "fu_min_af2w") {
    df_fu_min_af2w <- df_set %>%
      filter(days_of_2m > 14) %>% 
      group_by(subject_code) %>% 
      mutate(score_last = min(score, na.rm = T)) %>% 
      slice(n())
    return(df_fu_min_af2w)
  }
  else if (select == "last_2w_avg") {
    df_last_2w_avg <- df_set %>%
      group_by(subject_code) %>%
      filter(days_of_2m >= 14 & days_of_2m >= (max(days_of_2m) - 14)) %>% 
      mutate(score_last = mean(score, na.rm = T)) %>% 
      select(-score) %>% 
      slice(n())
    return(df_last_2w_avg)
  }
  # last two week minimum
  else if (select == "last_2w_min") {
    df_last_2w_min <- df_set %>%
      group_by(subject_code) %>%
      filter(days_of_2m > 14 & days_of_2m >= (max(days_of_2m) - 14)) %>% 
      mutate(score_last = min(score, na.rm = T)) %>% 
      select(-score) %>% 
      slice(n()) 
    return(df_last_2w_min)
  }
  else if (select == "last") {
    df_last <- df_set %>% 
      group_by(subject_code) %>%
      slice(n()) %>% 
      rename(score_last = score)
    return(df_last)
  }
  else if (select == "last_avg2") {
    df_last_avg2 <- df_set %>% 
      group_by(subject_code) %>% 
      slice(n():(n() - 1)) %>% 
      filter(max(no_of_2m) > 2) %>% 
      mutate(score_last = mean(score, na.rm = T)) %>% 
      slice(n())
    return(df_last_avg2)
  }
  else{
    warning("T_T not yet been developed, fong gor me la pls")
  }
}

### output to excel ###
### ls_out = the mapply list include all outputs, 
### grid = the variable grid for mapply
### xlsx.name = specify a file name for excel
output_excel <- function(ls_out, grid, xlsx.name){
  mean_tb <- data.frame()
  for (i in 1:ncol(ls_out)) {
    mean_tb <- bind_rows(
      mean_tb, 
      bind_cols(
        grid[i,],
        unlist(ls_out[4,i][[1]])[15],
        ls_out[2,i][[1]] %>% 
          as_tibble() %>% 
          t() %>% 
          bind_cols(., match_n = rownames(.)),
        ls_out[3,i][[1]] %>% 
          as_tibble() %>% 
          t() %>% 
          bind_cols(., crude_n = rownames(.))))
  }
  names(mean_tb)[c(1:5, 7:8)] <- c("project", "tier",
                                   "interaction effect", "match1", "match2",
                                   "crude1", "crude2")
  mean_tb %>% 
    mutate(match_n = gsub("[^0-9.]", "",  match_n),
           crude_n = gsub("[^0-9.]", "",  crude_n),
           project = paste(project, sep = ""),
           tier = paste(tier, sep = "")) %>% 
    select(project, tier, crude_n, match_n, crude1, crude2, 
           match1, match2, `interaction effect`) %>% 
    writexl::write_xlsx(., paste(xlsx.name, ".xlsx", sep = ""))
}




