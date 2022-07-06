library(tidyverse);library(here);library(readxl);library(MatchIt);
library(ggpubr);library(rstatix);library(gtsummary);library(lme4)

source("lineplot_function_new_v2.4.R")

# HW data
df_hw <- here("data", "Jun 20",
              "2022 06 21 headwind plus lion_v2.xlsx") %>% 
    read_xlsx(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    rename(k6_first = first_k6, 
           k6_last = last_k6,
           submit_time = ax_date,
           prof_service = currentuse_pro,
           psymed = currentuse_psymed,
           sf6_z_first = sf,
           ageby_sub = age) %>% 
    mutate(k6_tier_sc = k6_first) %>% 
    filter(psymed != 1) %>% 
    group_by(subject_code) %>% 
    slice(1) %>% 
    ungroup()

# Hub data
psy_med <- here("data", "Jun 20", "18. Hub users medication use.xlsx") %>% 
    read_xlsx(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() 

df_2m_raw <- here("data", "Jun 20", 
                  "14Jun2022_2M (cleaned | self).xlsx") %>% 
    read_excel(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    group_by(subject_code) %>% 
    slice(1) %>% 
    select(subject_code, submit_time, sf6_z, k6_score, k10_k6_3, k10_k6_4,
           pcs_c, mcs_c) %>% 
    rename(sf6_z_first = sf6_z) %>% 
    left_join(.,
              here("data", "Jun 20", 
                   "05. hub users no of visits.xlsx") %>% 
                  read_xlsx(na = c("888","999","-999", "889")) %>% 
                  janitor::clean_names(),
              by = c("subject_code" = "sid"))

# acutual users only
df_2m_demo <- here("data", "Jun 20", 
                   "14Jun2022_Baseline (cleaned | self).xlsx") %>% 
    read_excel(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names()
# acutual + potential
df_2m_age <- here("data", "Jun 20", 
                  "00. hub users demographics.xlsx") %>% 
    read_excel(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() 
# hub first visit df
# remove psy med users; include only 15-24; include more than 2 visit
hub_first <- 
    here("data","Jun 20", "02. hub users first 2w mean k6.xlsx") %>% 
    read_xlsx(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    left_join(df_2m_raw, by = c("sid" = "subject_code")) %>% 
    left_join(df_2m_age, by = "sid") %>%  # actual + potential
    rename(k6_first = mean_1st2w_k6,
           subject_code = sid,
           k6_tier_sc = k6_score) %>% 
    union_all(., df_hw %>% select(-k6_last)) %>% # join headwind
    filter(!subject_code %in% psy_med$sid) %>%  # remove hub use med 
    filter(ageby_sub >= 14 & ageby_sub <= 25) %>% # include only 15-24
    filter(noofvisits > 1|is.na(noofvisits)==T) %>%  # visit > 2
    filter(!prof_service > 0|is.na(prof_service)==T) # exclude prof_service
    
hub_last <- 
    here("data","Jun 20", "04. hub users last 2w mean k6.xlsx") %>% 
    read_xlsx(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    rename(k6_last = mean_last2w_k6,
           subject_code = sid) %>% 
    union_all(., df_hw %>% select(subject_code, k6_last))
# YES data
# exclude use service, assessment before 2020-06, use psy med
df_yes_service <- 
    here("data", "Jun 20",
         "YES control group exclusion list (to Catherine).xlsx") %>% 
    read_excel(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    filter(exclusion == 1)

df_yes_doa <-
    here("data", "Jun 20",
         "20Jun2022_YES ax date.xlsx") %>% 
    read_excel(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    mutate(
        date =  paste(doa_year, doa_month, sep = "-") %>% zoo::as.yearmon(),
        time_exclude = ifelse(date < zoo::as.yearmon("2020-06"), 1, 0)) %>% 
    filter(time_exclude == 1)

df_psy_med <- 
    here("data", "Jun 20",
         "20Jun2022_YES med list.xlsx") %>% 
    read_excel(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    filter(x3 == "Exclude")

df_yes_sf <- here("data", "Jun 20",
                  "(DONOTTOUCH) HKYES Master Database_17 May 2022.xlsx") %>% 
    read_excel(na = c("888","999","-999", "889"), sheet = "Total") %>% 
    janitor::clean_names() %>% 
    slice(-1) %>% 
    select(yes_id, sf12_tot, k6_tot, sf12_mcs, sf12_pcs) %>% 
    rename(subject_code = yes_id, 
           sf6_z_first = sf12_tot,
           k6_tier_sc = k6_tot,
           mcs_c = sf12_mcs,
           pcs_c = sf12_pcs) %>% 
    group_by(subject_code) %>% 
    slice(1) %>% 
    ungroup()

df_yes_demo <- here("data", "Jun 20",
                "(DONOTTOUCH) HKYES Master Database_17 May 2022.xlsx") %>% 
    read_excel(na = c("888","999","-999", "889"), sheet = "Raw") %>% 
    janitor::clean_names() %>% 
    slice(-1) %>% 
    select(yes_id, age, sex, edu_yes_no, job, marriage, addss, k10_k6_3, k10_k6_4)

yes_first <- 
    here("data", "Jun 20", "17. HKYES BL M3 K6.xlsx") %>% 
    read_xlsx(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    left_join(., df_yes_sf, by = c("yes_id" = "subject_code")) %>% 
    left_join(., df_yes_demo, by = "yes_id") %>% 
    rename(subject_code = yes_id,
           k6_first = bl_k6) %>% 
    select(-m3_k6) %>% 
    filter(!subject_code %in% df_yes_service$parti_id) %>%  # remove use prof_service
    filter(!subject_code %in% df_yes_doa$yes_id) %>%  # remove ax before 2020-06
    filter(!subject_code %in% df_psy_med$yes_id) # remove med users
    
yes_last <- 
    here("data", "Jun 20", "17. HKYES BL M3 K6.xlsx") %>% 
    read_xlsx(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    select(-bl_k6) %>% 
    rename(subject_code = yes_id,
           k6_last = m3_k6)

#save(hub_first, hub_last, yes_first, yes_last, 
 #    file = "jun20_itt_subgroup_data.rdata")

tiers <- list(tier.is = c("T0", "T1", "T2", "T3"),
              c("T0"),
              c("T1", "T2"),
              #c("T2"),
              c("T3"))

#save.image("jun20_itt_cart_actual.rdata")

out.all <-   
    mapply(function(y){
    list <- 
        stat_test_perm(hub_first = hub_first %>% filter,
                       hub_last = hub_last,
                       control_first = yes_first,
                       control_last = yes_last,
                       var.to.test = c("k6_first", "k6_last"),
                       tier.is = c("T0", "T1", "T2", "T3"),
                       num_pmt = 10,
                       cart = T,
                       cluster.n = 4)
    return(list)
    }, y = tiers)

################################################################################
# LMM data prep
hub_first_lmm <-
    here("data", "Jun 20", "02. hub users first 2w mean k6.xlsx") %>%
    read_xlsx(na = c("888", "999", "-999", "889")) %>%
    janitor::clean_names() %>%
    left_join(df_2m_raw, by = c("sid" = "subject_code")) %>%
    left_join(df_2m_age, by = "sid") %>%
    left_join(df_2m_demo %>% select(subject_code, addss), 
              by = c("sid" = "subject_code")) %>% 
    rename(k6_first = mean_1st2w_k6,
           subject_code = sid,
           k6_tier_sc = k6_score) %>%
    union_all(df_hw) %>% # only remove med users
    group_by(subject_code) %>%
    mutate(
        submit_time = as.Date(submit_time),
        doa_bl = min(submit_time) %>%
            lubridate::floor_date(., "season") %>%
            zoo::as.yearmon(),
        prof_service = 0
    ) %>%
    arrange(subject_code, submit_time) %>%
    slice(1) %>%
    filter(!subject_code %in% psy_med$sid & # remove hub use med
               (noofvisits > 1 | is.na(noofvisits) == T)) # visit > 1

hub_last_lmm <- 
    here("data", "Jun 20", "04. hub users last 2w mean k6.xlsx") %>% 
    read_xlsx(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    rename(k6_last = mean_last2w_k6,
           subject_code = sid) %>% 
    union(df_hw %>% select(subject_code, k6_last))

yes_first_lmm <-
    here("data", "Jun 20", "17. HKYES BL M3 K6.xlsx") %>%
    read_xlsx(na = c("888", "999", "-999", "889")) %>%
    janitor::clean_names() %>%
    select(-m3_k6) %>%
    rename(subject_code = yes_id,
           k6_first = bl_k6) %>%
    left_join(., df_yes_sf, by = "subject_code") %>%
    left_join(., df_yes_demo, by = c("subject_code" = "yes_id")) %>%
    filter(!subject_code %in% df_psy_med$yes_id) %>% # only remove psy med
    left_join(
        .,
        here("data", "Jun 20",
             "20Jun2022_YES ax date.xlsx") %>%
            read_excel(na = c("888", "999", "-999", "889")) %>%
            janitor::clean_names() %>%
            mutate(doa_bl =  paste(doa_year, doa_month, sep = "-") %>%
                       zoo::as.yearmon()),
        by = c("subject_code" = "yes_id")
    ) %>%
    mutate(prof_service = ifelse(subject_code %in% df_yes_service$parti_id,
                                 1, 0)) %>%
    mutate_at(c("sex", "edu_yes_no", "job", "marriage", "k10_k6_3", "k10_k6_4"),
              as.numeric)

yes_last_lmm <-     
    here("data", "Jun 20", "17. HKYES BL M3 K6.xlsx") %>% 
    read_xlsx(na = c("888","999","-999", "889")) %>% 
    janitor::clean_names() %>% 
    select(-bl_k6) %>% 
    rename(subject_code = yes_id,
           k6_last = m3_k6)

#save.image("jun27_lmm_all_data.rdata")
#save(yes_last_lmm, yes_first_lmm, hub_first_lmm, hub_last_lmm, 
    # file = "jun27_lmm_cart.rdata")
#load("jun20_lmm_cart.rdata")

###############################################################################
# LMM by subgroup
df_join_pre <- 
    hub_last_lmm %>% 
    janitor::clean_names() %>% 
    select(subject_code, k6_last) %>% 
    left_join(hub_first_lmm %>% 
                  janitor::clean_names() %>% 
                  filter(k10_k6_3 > 0) %>% # subgroup
                  select(subject_code, k6_first, k6_tier_sc,
                         doa_bl, prof_service, k10_k6_3),
              by = "subject_code") %>%
    mutate(project = 1) %>% 
    union_all(
        yes_last_lmm %>% 
            janitor::clean_names() %>% 
            select(subject_code, k6_last) %>% 
            left_join(yes_first_lmm %>% 
                          #mutate(hub = gsub("[^a-zA-Z]", "", subject_code)) %>% 
                          janitor::clean_names() %>% 
                          filter(k10_k6_3 > 0) %>% 
                          select(subject_code, k6_first, k6_tier_sc,
                                 doa_bl, prof_service, k10_k6_3),
                      by = "subject_code") %>% 
            mutate(project = 0) 
    ) %>% 
    ungroup()

df_join <- 
    df_join_pre %>%
    mutate(#tier = case_when(k6_tier_sc >= 0 & k6_tier_sc < 5 ~ "T0",
                            #k6_tier_sc >= 8 & k6_tier_sc < 11 ~ "T1",
            #                k6_tier_sc >= 5 & k6_tier_sc < 13 ~ "T2",
             #               k6_tier_sc >= 13 & k6_tier_sc <= 24 ~ "T3"),
           hub = gsub("[^a-zA-Z]", "", subject_code),
           project = ifelse(project == 1, "Hub","Control") %>% 
               factor(., levels = c("Hub","Control"))) %>% 
    filter(complete.cases(.)) %>% 
    arrange(project, desc(k6_tier_sc))

df_join_flip <- df_join %>% 
    pivot_longer(k6_first:k6_last,
                 names_to = "visit", values_to = "values") %>% 
    mutate(visit = factor(visit, levels = c("k6_first", "k6_last")))

df_join_flip$project <- relevel(df_join_flip$project, ref = "Control")

lme1 <- lmer(
    values ~ project*visit + (1|doa_bl) + (1|prof_service) + (1|k6_tier_sc), 
    data = df_join_flip)

set.seed(1234)
boot <- 
    parameters::bootstrap_parameters(lme1, iterations = 100, bootstrap = T, test = "p")
null.model <- lmer(
    values ~ 1 + (1|doa_bl) + (1|prof_service) + (1|k6_tier_sc), 
    data = df_join_flip)
r2 <- 
    (summary(null.model)[["sigma"]]^2 - summary(lme1)[["sigma"]]^2)/
    summary(null.model)[["sigma"]]^2
f2 <- round(r2/(1-r2), 3)
# A practical guide to calculating Cohen’s f2, a measure of local effect size, from PROC MIXED
boot.df <- tibble(visit = rep(c("k6_first", "k6_last"), 2),
                  project = rep(c("Hub", "Control"), each = 2),
                  values = c(
                      boot$Coefficient[1] + boot$Coefficient[2],
                      boot$Coefficient[1] + boot$Coefficient[2] + 
                          boot$Coefficient[3] + boot$Coefficient[4],
                      boot$Coefficient[1],
                      boot$Coefficient[1] + boot$Coefficient[3]))

ggplot(boot.df,
       aes(x = visit, y = values, color = project, group = project)) +
    geom_point() +
    geom_line(size = 1) +
    scale_y_continuous(limits = c(0, 24)) +
    geom_text(aes(label = round(values, 3)), vjust = 0.1,
              position = position_dodge(width = 1), size = 4) +
    labs(subtitle = bquote(paste("Cohen's", " ", f[Fixed]^2, " = ", .(f2)))) + 
    theme(legend.position = "bottom") + 
    ggtitle("Predicted Value") +
    scale_color_manual(values = c("#00AFBB", "#FC4E07"))

boot
table(df_join$project)

library(ggpubr)

gghistogram(hub_first_lmm$doa_bl, bins = 30) +
    labs(x = "K6 BL in Hub")

gghistogram(yes_first$k6_first, bins = 24) +
    labs(x = "K6 BL in YES")

table(hub_first_lmm$sf6_z_first) %>% as.data.frame() %>% 
    ggbarplot(x = "Var1", y = "Freq", fill = "skyblue3") +
    labs(x = "BL K6 (YES)") + 
    scale_y_continuous(limits = c(0,290))#+
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
gghistogram(hub)

###############################################################################
# Categorical analysis
hub_long <- left_join(hub_first, hub_last, by = "subject_code") %>% 
    filter(!is.na(k6_first) & !is.na(k6_last)) %>% 
    pivot_longer(c(k6_first, k6_last), 
                 names_to = "visit", 
                 values_to = "k6_score") %>% 
    mutate(tier = case_when(k6_score >= 0 & k6_score < 10 ~ "T1",
                            #k6_score >= 9 & k6_score < 10 ~ "T1",
                            k6_score >= 10 & k6_score < 15 ~ "T2",
                            k6_score >= 15 & k6_score <= 24 ~ "T3"))

hub_count <-  left_join(hub_first, hub_last, by = "subject_code") %>% 
    mutate_at(vars(one_of("k6_first", "k6_last")), 
              funs(case_when(
                  . >= 0 & . < 10 ~ "T1",
                  #. >= 9 & . < 10 ~ "T1",
                  . >= 10 & . < 15 ~ "T2",
                  . >= 15 & . <= 24 ~ "T3")))

table(hub_count$k6_first, hub_count$k6_last) %>% 
    as.data.frame() %>% 
    group_by(Var1) %>% 
    mutate(sum = sum(Freq),
           prop = Freq/sum*100) %>% 
    arrange(Var1)

library(ggalluvial)
hub_long %>% 
    ggplot(aes(x=visit,
               stratum= tier,
               alluvium = subject_code,
               fill = tier,
               label = tier)) +
    geom_flow() +
    geom_stratum(alpha = .5) +
    geom_text(stat = "stratum", size = 4, min.y = 50)+
    xlab("Hub")

###############################################################################
# LMM plot template
library(scales)
lmm_plot <- function(pval, es, n_hub, n_control, hub_first, hub_last,
                     control_first, control_last,title){
    Matrix <- matrix(c("Hub","k6_first",hub_first,"Control","k6_first",control_first,
                       "Hub","k6_last",hub_last,"Control","k6_last",control_last), byrow = T,
                     nrow = 4,ncol = 3)
    Matrix <- Matrix %>% as.data.frame() %>% rename(.,project = V1, visit = V2, k6_score = V3) %>% 
        mutate(k6_score = as.numeric(k6_score))
    
    Matrix %>% ggplot(aes(x = visit,y=k6_score,color=project,group=project,label=k6_score)) + 
        geom_line(size=1.5) +
        geom_point(size=2) +
        scale_y_continuous(limits = c(0,25)) +
        geom_text(data = Matrix[Matrix$project=='Hub',], aes(vjust=1.7,hjust=0.5),
                  size=5) +
        geom_text(data = Matrix[Matrix$project=='Control',], aes(vjust=-1),
                  size=5) +
        geom_text(aes(x = 1.5, y = 24,
                      label = paste("p-value", pval)),
                  stat = "unique",
                  color = "black",
                  size=5)+
        geom_text(aes(x = 1.5, y = 23,
                      label = paste("Effect size =", es)),
                  stat = "unique",
                  color = "black",
                  size=5)+
        theme_bw(base_size = 13) +
        labs(title = title,
             subtitle = paste("N (hub) =", n_hub, "| N (Control) =", n_control)) +
        theme(plot.title = element_text(size=20)) + 
        scale_color_brewer(palette = "Set2")
}
quartz(width = 16,height = 6,pointsize = 10,dpi = 100)
ggarrange(plotlist = list(
    lmm_plot("<.001", 0.033, 2041, 1510, 11.02, 9.587, 11.05, 10.852, "All"),
    lmm_plot("=.08", 0.125, 468, 484, 4.463, 4.812, 4.943, 5.559, "S1"),
    lmm_plot("<.001", 0.237, 1293, 877, 12.286, 8.81, 12.93, 10.602, "S2"),
    lmm_plot("= 0.74", 0.364, 279, 149, 18.104, 13.377, 18.787, 14.276, "S3")),
    common.legend = T, 
    nrow = 1)

