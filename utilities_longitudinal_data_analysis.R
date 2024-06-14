
to_wide_format_one_variable<-function(all_combined_data_frames,numeric_colname){
  all_combined_data_frames11<-all_combined_data_frames %>% dplyr::select(subj_session_count,subject_id1,as.name(numeric_colname))
  all_combined_data_frames11<-all_combined_data_frames11 %>% pivot_wider(
    names_from = subj_session_count,
    names_sep = ".",
    values_from = as.name(numeric_colname),id_cols=subject_id1,names_prefix='SCAN_NUM'
  )
  return(all_combined_data_frames11)
  
}
add_project_type<-function(df){
  df<-df %>% mutate(project_type = case_when(!str_detect(subject_id1,"SAH") & !str_detect(subject_id1,"ICH")   ~ 'INFARCT',
                                             str_detect(subject_id1,"ICH")  ~ 'ICH',
                                             str_detect(subject_id1,"SAH")  ~ 'SAH'
                                             
  ))
  return(df)
}
# print(head(dental))
########
longitudinal_analysis<-function(all_combined_data_frames,numeric_colname){
dental<-c()
# numeric_colname='CSF.RATIO'
# all_combined_data_frames<-read_csv('all_combined_data_frames_11.csv')
all_combined_data_frames11<-to_wide_format_one_variable(all_combined_data_frames,numeric_colname)
all_combined_data_frames11<-add_project_type(all_combined_data_frames11)
dental$id<-all_combined_data_frames11$subject_id1
dental$sex<-all_combined_data_frames11$project_type
dental$scan1<-all_combined_data_frames11$SCAN_NUM1
dental$scan2<-all_combined_data_frames11$SCAN_NUM2
dental$scan3<-all_combined_data_frames11$SCAN_NUM3
dental$scan4<-all_combined_data_frames11$SCAN_NUM4
dental<-data.frame(dental)
write.csv(dental,'wide_data.csv',row.names = FALSE)
# all_combined_data_frames_subset<-all_combined_data_frames %>% dplyr::select(timeinterval_asfactor,timeinterval,project_type,subject_id1,subj_session_count,datetime_synth.diff,as.name(numeric_colname))
# all_combined_data_frames_subset<-all_combined_data_frames_subset %>% drop_na()
# dental_long <- pivot_longer(dental, cols = starts_with("y"),
#                             names_to = "measurement", values_to = "distance")  %>%
#   mutate(
#     age = parse_number(measurement),
#     measurement = fct_inorder(paste("Measure at scan", age))
#   ) %>%
# set_variable_labels(
#   age = "Age of the child at measurement",
#   measurement = "Label for time measurement",
#   distance = "Measurement"
# )%>%
#   mutate(
#     age = parse_number(as.character(measurement)),
#     measurement = fct_inorder(paste("Measure at scan", age))
#   )
column_to_measure<-numeric_colname #'CSF.RATIO'
# dental_long<-all_combined_data_frames_subset %>%dplyr::rename('id'='subject_id1','sex'='project_type','age'='timeinterval','distance'=numeric_colname,'measurement'='timeinterval_asfactor') %>%
#   set_variable_labels(
#     age = "Age of the child at measurement",
#     measurement = "Label for time measurement",
#     distance = "Measurement"
#   )%>%
#   mutate(
#     age = parse_number(as.character(measurement)),
#     measurement = fct_inorder(paste("Measure at scan", age))
#   )

# print(head(dental_long))
# dental_long$datetime_synth.diff<-NULL
# dental_long$subj_session_count<-NULL
# dental_long<-dental_long%>%dplyr::select(id, sex,   measurement ,      distance,   age)

dental_long <- pivot_longer(dental, cols = starts_with("scan"), 
                            names_to = "measurement", values_to = "distance") %>% 
  mutate(
    age = parse_number(measurement),
    measurement = fct_inorder(paste("Measure at scan", age))
  ) %>% 
  set_variable_labels(
    age = "Age of the child at measurement",
    measurement = "Label for time measurement",
    distance = "Measurement"
  )
dental_long<-dental_long %>% drop_na()
write.csv(dental_long,'long_data.csv',row.names = FALSE)
# head(dental_long)
# sdata<-distinct(dental_long)
# sdata$measurement<-NULL
# sdata_wide<-sdata %>%
#   group_by(grp = cumsum(str_detect(age, '^X'))) %>%
#   mutate(age = case_when(row_number() > 1 ~ str_c('hours',first(age), age, sep="_"),
#                          TRUE ~ as.character(age))) %>%
#   ungroup %>%
#   select(-grp) %>%
#   pivot_wider(names_from = age, values_from = distance)
##
print(ggpairs(dental, mapping = aes(colour = sex), columns = 3:6,
              lower = list(continuous = "smooth")))

##############
print(group_by(dental_long, age) %>%
        get_summary_stats(distance))

##
print(ggplot(dental_long, aes(measurement, distance, fill = measurement)) +
        geom_boxplot() +
        geom_jitter(width = 0.2) +
        guides(fill = "none") +
        labs(x = "", y = column_to_measure))

# print(head(xx))
#######
print(group_by(dental_long, sex, measurement) %>%
        get_summary_stats(distance, show = c("mean", "sd")))
###
print(ggplot(dental_long, aes(sex, distance, fill = measurement)) +
        geom_boxplot() +
        labs(x = "", y = column_to_measure, fill = ""))

######
print(group_by(dental_long, sex, measurement) %>%
        summarise(mean_distance = mean(distance), .groups = "drop") %>%
        ggplot(aes(sex, mean_distance, fill = measurement, label = round(mean_distance))) +
        geom_col(position = "dodge") +
        geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
        coord_flip() +
        labs(x = "", y = column_to_measure, fill = ""))
#################
# # co-variance matrix
# cov_obs <- dplyr::select(dental, starts_with("y")) %>%
#   cov()
# print(cov_obs)
#print(# correlation matrix
# cov2cor(cov_obs))
# ggpairs(select(dental, starts_with("y")), lower = list(continuous = "smooth"))
##########
print(group_by(dental_long, sex, age) %>%
        summarise(mean = list(mean_ci(distance)), .groups = "drop") %>%
        unnest_wider(mean) %>%
        mutate(agex = age - .05 + .05*(sex == "Boy")) %>%
        ggplot(aes(agex, y, col = sex, shape = sex)) +
        geom_point() +
        geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
        geom_line() +
        labs(x = "SCAN NUM", y = column_to_measure, shape = "", col = ""))

# #######################
# print(ggplot(dental_long, aes(age, distance, col = factor(id))) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~ id) +
#   labs(x = " SCAN NUM", y = "CSF.RATIO", col = "Child id") +
#   guides(col = guide_legend(nrow = 3)))
#########
# print(plot(ggplot(dental_long, aes(age, distance, col = factor(id))) +
#   geom_line() +
#   labs(x = " SCAN NUM", y = "CSF.RATIO", col = "Child id") +
#   guides(col = guide_legend(nrow = 3))))
# print(ggplot(dental_long, aes(age, distance)) +
#         geom_line(aes(group = factor(id))) +
#         geom_smooth() +
#         facet_grid(~ sex) )
lin_0 <- lmer(distance ~ 1 + (1 | id), data = dental_long)
print(summary(lin_0))
print(ci.lin(lin_0))
print(ranova(lin_0))
print(ggplot(dental_long, aes(id, distance)) +
        geom_point(aes(col = measurement, shape = measurement)) +
        geom_point(data = group_by(dental_long, id) %>%
                     summarise(distance = mean(distance), .groups = "drop"),
                   aes(col = "Mean", shape = "Mean"), size = 2.5) +
        geom_hline(yintercept = mean(dental_long$distance)) +
        # scale_shape_manual(values = c(4, 19, 19, 19, 19)) +
        labs(x = "id", y = column_to_measure, col = "Measurement", shape = "Measurement"))

lin_age <- lmer(distance ~ measurement + (1 | id), data = dental_long)
print(      summary(lin_age))
print(tidy(emmeans(lin_age, "measurement"), conf.int = TRUE))
print(Anova(lin_age))
dental_fit <- bind_cols(
  dental_long, pred_age = predict(lin_age, re.form = ~ 0)
)
print(ggplot(dental_fit, aes(age, distance)) +
        geom_line(aes(group = factor(id))) +
        geom_point(aes(y = pred_age), col = "blue", size = 2) +
        labs(x = " SCAN NUM", y = numeric_colname))

##########
lin_agesex <- lmer(distance ~ measurement + sex + (1 | id), data = dental_long)
print(summary(lin_agesex))
print(ci.lin(lin_agesex))
print(tidy(emmeans(lin_agesex, c("measurement", "sex")), conf.int = TRUE))

#########
dental_fit$pred_agesex <- predict(lin_agesex, re.form = ~ 0)
print(ggplot(dental_fit, aes(age, distance)) +
        geom_line(aes(group = factor(id))) +
        geom_point(aes(y = pred_agesex, col = sex), size = 2) +
        labs(x = " SCAN NUM", y = numeric_colname, col = ""))

###########
lin_agecsexinter <- lmer(distance ~ sex*age + (1 | id), data = dental_long)
print(summary(lin_agecsexinter))
#################
K <- rbind(
  c(0, 1, 0, 8),
  c(0, 1, 0, 14),
  c(0, 0, 1, 1),
  c(0, 0, 2, 2),
  c(1, 1, 8, 8),
  c(1, 1, 14, 14),
  c(0, 0, 14-8, 14-8)
)
# rownames(K) <- c("Difference between boys and girls at age 8 SCAN NUM",
#                  "Difference between boys and girls at age 14 SCAN NUM",
#                  "Linear trend for 1 year increase of age among boys",
#                  "Linear trend for 2 hours increase of age among boys",
#                  "Mean response at age 8 hours among boys",
#                  "Mean response at age 14 hours among boys",
#                  "Difference between age 14 and 8 among boys")
# tidy(glht(lin_agecsexinter, linfct =  K), conf.int = TRUE) %>%
#   gt() %>%
#   fmt_number(columns = -1,decimals = 2)
# #############################
# pred_agecsexinter <- expand.grid(
#   age = seq(8, 14, .5),
#   sex = levels(dental_long$sex)
# ) %>%
#   bind_cols(pred = predict(lin_agecsexinter, newdata = ., re.form = ~ 0))
# ggplot(pred_agecsexinter, aes(age, pred, col = sex)) +
#   geom_line() +
#   labs(x = " SCAN NUM", y = "CSF.RATIO", col = "")
#
# ##################
# pred_agecsexinter <- expand.grid(
#   age = seq(8, 14, .5),
#   sex = levels(dental_long$sex)
# ) %>%
#   bind_cols(pred = predict(lin_agecsexinter, newdata = ., re.form = ~ 0))
# ggplot(pred_agecsexinter, aes(age, pred, col = sex)) +
#   geom_line() +
#   labs(x = " SCAN NUM", y = "CSF.RATIO", col = "")
# ######################
# lin_agec <- lmer(distance ~ age + (1 | id), data = dental_long)
# print(lin_agec)
#
# ##############
# sid <- c("BWH_415", "BWH_447")
# expand.grid(
#   age = seq(8, 14, .5),
#   id = sid
# ) %>%
#   bind_cols(
#     indiv_pred = predict(lin_agec, newdata = .),
#     marg_pred = predict(lin_agec, newdata = ., re.form = ~ 0)
#   ) %>%
#   left_join(
#     filter(dental_long, id %in% sid), by = c("id", "age")
#   ) %>%
#   print(ggplot(aes(age, indiv_pred, group = id, col = factor(id))) +
#   geom_line() +
#   geom_point(aes(y = distance)) +
#   geom_line(aes(y = marg_pred, col = "Marginal"), lwd = 1.5) +
#   labs(x = " SCAN NUM", y = "CSF.RATIO", col = "Curve"))
#############################
# lin_agecr <- lmer(distance ~ age + (age | id), data = dental_long)
# print(summary(lin_agecr))
# ###############
# print(VarCorr(lin_agecr))
# print(as.data.frame(VarCorr(lin_agecr)))
#
# #################

# expand.grid(
#   age = seq(50, 60, 75),
#   id = unique(dental_long$id)
# ) %>%
#   bind_cols(
#     indiv_pred = predict(lin_agecr, newdata = .),
#     marg_pred = predict(lin_agecr, newdata = ., re.form = ~ 0)
#   ) %>%
#   print(ggplot(aes(age, indiv_pred, group = id)) +
#   geom_line(col = "grey") +
#   geom_line(aes(y = marg_pred), col = "blue", lwd = 1.5) +
#   labs(x = " SCAN NUM", y = "CSF.RATIO"))

#########################
# lin_agecsexinterr <- lmer(distance ~ age*sex + (age | id), data = dental_long)
# print(summary(lin_agecsexinterr))
}