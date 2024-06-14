# source('utilities_longitudinal_data_analysis.R')
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(xtable)
library(stringr)
source('utilities.R')
rm()
append_newline<-function(filename_output){
  file_conn <- file(filename_output, open = "a")
  
  # Append the content to the file using the connection
  writeLines("\n  ", file_conn)
  writeLines("\n  ", file_conn)
  writeLines("\n  ", file_conn)
  writeLines("\n  ", file_conn)
  writeLines("\n  ", file_conn)
  # Close the connection
  close(file_conn)
  
}

count_col_non_nas<-function(df,colname,output_df){
  total_in_each_pipeline<-df %>% group_by(project) %>% summarise(count = sum(!is.na(!!sym(colname))))
  # print(total_in_each_pipeline)
  total_in_each_pipeline$value.type<-colname
  output_df<-rbind(output_df,total_in_each_pipeline)
  print(output_df)
}
## load synthesized master file:
## descriptive analysis
all_combined_data_frames<-read.csv('../RFILES/all_combined_data_frames_1.csv')

all_combined_data_frames<-all_combined_data_frames %>% rename('SAH.VOLUME'='SAH_SEG_TOTAL')
all_combined_data_frames<-all_combined_data_frames %>% rename('NIFTI.FILES'='FileName_slice')
## DATA CLEANING
## REMOVE MGB:
all_combined_data_frames<-all_combined_data_frames%>%filter(project!="MGBBMC")
## COUNTS of different cohorts: ## subject count ## sessions count ## Axial brain count ## 
## number of cohorts:
#  all_combined_data_frames_withresults<-all_combined_data_frames%>% filter(str_detect(CSV_FILE_PATH, '.csv'))
# cohorts_name<-unique(all_combined_data_frames_withresults$project_type)
# cohorts_count<-all_combined_data_frames_withresults %>% count(project_type)
###### SUBJECT COUNT-> SESSION COUNT -> AXIAL COUNT with BODY PARTS -> 
############# TOTAL SUBJECT COUNT#########################
#SAH_COUNT
sah<-all_combined_data_frames %>% filter(project_type=='SAH')
sah <- sah %>%
  mutate(
    split_text = str_split(label, "_"),
    sah_sub_id = map_chr(split_text, ~paste(.x[1], .x[2], sep = "_"))
  )
sah_subject_count<-sah %>%
  summarise(UniqueCount = n_distinct(sah_sub_id))
print(sah_subject_count, include.rownames = FALSE, caption = "Total number of sessions in SNIPR", label = "tab:example", align = "lcc")
subject_count<-all_combined_data_frames %>%
  group_by(project_type) %>%
  summarise(subject = n_distinct(subject_id))
subject_count$subject[3]<-sah_subject_count$UniqueCount[1]
subject_count_latex <- xtable(subject_count)
print(subject_count_latex) #, include.rownames = FALSE, caption = "Total number of sessions in SNIPR", label = "tab:example", align = "lcc")
#########################################################

####### TOTAL SESSION COUNT ##########################
sessoin_count<-all_combined_data_frames %>%
  group_by(project_type) %>%
  summarise(session= n_distinct(ID))
session_count_latex <- xtable(sessoin_count)
#############################

############### AXIAL COUNT #################
axial_count<-all_combined_data_frames %>%
  group_by(project_type) %>%
  summarise(axial = sum(axial_number>0))
axial_count_latex <- xtable(axial_count)
############################################
### TOTAL NIFTI FILE COUNTS ####################
nifti_count<-all_combined_data_frames %>%
  group_by(project_type) %>% 
  summarise(nifti = n_distinct(NIFTIFILES_PREFIX))
nifti_count_latex <- xtable(nifti_count)

#########################################
### TOTAL CSV FILE COUNTS ####################
csvfile_count<-all_combined_data_frames %>%
  group_by(project_type) %>%
  summarise(csvfile = n_distinct(CSV_FILE_PATH))
csvfile_count_latex <- xtable(csvfile_count)

#########################################
######## COMBINED COUNTS################
filename_output<-'tex_tables.tex'
combined_df <-merge(subject_count,sessoin_count,  by = "project_type")
combined_df <-merge(combined_df, axial_count, by = "project_type")
combined_df <-merge(combined_df,nifti_count, by = "project_type")
combined_df <-merge(combined_df,csvfile_count, by = "project_type")
total_cohorts<-bind_rows(summarise(combined_df, "project_type" = "TOTAL", across(where(is.numeric), sum, na.rm = TRUE)))
combined_df<-rbind(combined_df, total_cohorts)
combined_df<-combined_df %>% dplyr::rename( Cohorts=project_type ) 
combined_df <- combined_df %>%
  mutate(Success.Rate = (.[[ncol(.)]] / .[[ncol(.)-1]]))
combined_df_latex <- print(xtable(combined_df),file = filename_output, include.rownames = FALSE)
#############################


## write empty lines:
# Open a connection in append mode
file_conn <- file(filename_output, open = "a")

# Append the content to the file using the connection
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
# Close the connection
close(file_conn)
### LOCATION COUNT
location_count <- all_combined_data_frames %>%
  group_by( project,project_type) %>%
  summarise(Count = n(), .groups = 'drop')
location_count <- pivot_wider(location_count, names_from = project, values_from = Count)
location_count<-location_count %>% rename(Cohorts=project_type )
location_count_latex <- print(xtable(location_count),file = filename_output,append=TRUE, include.rownames = FALSE)
# bar plot total counts:
## TOTAL number in each pipeline type:
tota_session_analyzed<-all_combined_data_frames%>% filter(str_detect(CSV_FILE_PATH, '.csv'))  %>% summarise(total_non_na = sum(!is.na(CSV_FILE_PATH)))
all_combined_data_frames_withresults<-all_combined_data_frames%>% filter(str_detect(CSV_FILE_PATH, '.csv'))
cohorts_name<-unique(all_combined_data_frames_withresults$project_type)
cohorts_count<-all_combined_data_frames_withresults %>% count(project_type)
print(cohorts_count, include.rownames = FALSE, caption = "Total number of sessions with results", label = "tab:example", align = "lcc")
################
file_conn <- file(filename_output, open = "a")

# Append the content to the file using the connection
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
# Close the connection
close(file_conn)

############ BIOMARKERS COUNT ####################################
csf_count <- all_combined_data_frames %>%
  filter(!is.na(CSF.RATIO)) %>%   group_by(project_type) %>% 
  summarise(Count = n())
nwu_count <- all_combined_data_frames %>%
  filter(!is.na(NWU)) %>%   group_by(project_type) %>% 
  summarise(Count = n())
nwu_count_latex <- xtable(nwu_count)
ich_vol_count <- all_combined_data_frames %>%
  filter(!is.na(ICH.VOLUME)) %>%   group_by(project_type) %>% 
  summarise(Count = n())
sah_vol_count <- all_combined_data_frames %>%
  filter(!is.na(SAH.VOLUME)) %>%   group_by(project_type) %>% 
  summarise(Count = n())
nwu_count_latex <- xtable(nwu_count)

biomarker_df <-rbind(csf_count,nwu_count) #,  by = "project_type")
biomarker_df <-rbind(biomarker_df, ich_vol_count) #, by = "project_type")
biomarker_df <-rbind(biomarker_df,sah_vol_count) #, by = "project_type")
# biomarker_df <-merge(biomarker_df,csvfile_count, by = "project_type")
total_biomarker<-bind_rows(summarise(biomarker_df, "project_type" = "TOTAL", across(where(is.numeric), sum, na.rm = TRUE)))
biomarker_df<-rbind(biomarker_df, total_biomarker)
biomarker_df<-biomarker_df %>% dplyr::rename( Cohorts=project_type ) 
biomarker_df_latex <- print(xtable(biomarker_df),file = filename_output,append=TRUE, include.rownames = FALSE)

#############################################################
################
file_conn <- file(filename_output, open = "a")

# Append the content to the file using the connection
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
writeLines("\n  ", file_conn)
# Close the connection
close(file_conn)
### BIOMARKERS COUNT IN EACH LOCATION:
csf_count <- all_combined_data_frames %>%
  filter(!is.na(CSF.RATIO)) %>%   group_by(project) %>% 
  summarise(CSF.RATIO = n())
nwu_count <- all_combined_data_frames %>%
  filter(!is.na(NWU)) %>%   group_by(project) %>% 
  summarise(NWU = n())
# nwu_count_latex <- xtable(nwu_count)
ich_vol_count <- all_combined_data_frames %>%
  filter(!is.na(ICH.VOLUME)) %>%   group_by(project) %>% 
  summarise(ICH.VOLUME = n())
sah_vol_count <- all_combined_data_frames %>%
  filter(!is.na(SAH.VOLUME)) %>%   group_by(project) %>% 
  summarise(SAH.VOLUME = n())
# nwu_count_latex <- xtable(sah_vol_count)

biomarker_df <-merge(csf_count, nwu_count, by = "project", all = TRUE)
biomarker_df <-merge(biomarker_df, ich_vol_count, by = "project", all = TRUE)
biomarker_df <-merge(biomarker_df, sah_vol_count, by = "project", all = TRUE)
# biomarker_df <-merge(biomarker_df,csvfile_count, by = "project_type")
# total_biomarker<-bind_rows(summarise(biomarker_df, "project" = "TOTAL", across(where(is.numeric), sum, na.rm = TRUE)))
# biomarker_df<-rbind(biomarker_df, total_biomarker)
biomarker_df<-biomarker_df %>% dplyr::rename( Location=project ) 
biomarker_df_latex <- print(xtable(biomarker_df),file = filename_output,append=TRUE, include.rownames = FALSE)


#####################################

output_df<-c()
colname<-'CSF.RATIO'
output_df<-count_col_non_nas(all_combined_data_frames,colname,output_df)
colname<-'NWU'
output_df<-count_col_non_nas(all_combined_data_frames,colname,output_df)
colname<-'ICH.VOLUME'
output_df<-count_col_non_nas(all_combined_data_frames,colname,output_df)
colname<-'SAH.VOLUME'
output_df<-count_col_non_nas(all_combined_data_frames,colname,output_df)
colname<-'NIFTI.FILES'
output_df<-count_col_non_nas(all_combined_data_frames,colname,output_df)
output_df<- output_df %>% mutate(count = replace(count, count==0, NA))
p<-ggplot(output_df, aes(fill=project, y=count, x=value.type)) +
  geom_bar(position='dodge', stat='identity',width = 1) +
  theme_ben()+
  guides(fill=guide_legend(title="Cohorts"))+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  ylim(0,5000)+
  # xlim()+
  xlab("") +
  ggtitle(paste0('Sessions Analyzed:',tota_session_analyzed$total_non_na)) + #,sep="\n")) + #paste(paste0(data_frame_name), paste0('N:',data_count),sep="\n",paste0('p-value:', sprintf("%.3f", correl$p.value)),paste0('corr:',sprintf("%.3f", correl$statistic))))  #+
  geom_text(aes(label=count), position=position_dodge(width=0.9), hjust=-0.1,angle = 90) #+
  # annotate(
  #   "label", x=length(unique(output_df$value.type))*0.9, y=max(na.omit(output_df$count))*0.95, label=paste('Session', 'Analyzed:',tota_session_analyzed$total_non_na,sep="\n"),
  #   color="red", size=5) # +
print(p)
ggsave('project_pipeline_bar_plot.png',dpi=300)
## At least one scan axial scan in one of them

## analysis completed in how many scans

## percentage of success
output_df_wide<-output_df %>% pivot_wider(
  names_from = value.type,
  values_from = count,id_cols=project
)

col_names<-colnames(output_df_wide)
output_df_wide<-output_df_wide %>%select(col_names[1],col_names[6],col_names[2],col_names[3],col_names[4],col_names[5])
output_df_wide<-output_df_wide %>%
  mutate(
    across(c(3:6),
           .fns = ~./NIFTI.FILES *100))
print(output_df_wide)
output_df_wide<-output_df_wide%>%select(-CSF.RATIO)
print(xtable(output_df_wide , type = "latex"),include.rownames=FALSE)
## time taken for each pipeline.
pipelines_time<-c()
pipelines_time$mean<-c(1745, 2259.5,1538.3)
pipelines_time$std<-c(217.08 ,318.20,280.04)
pipelines_time<-data.frame(pipelines_time)
colnames(pipelines_time)<-c('Mean','SD')
row.names(pipelines_time)<-c('ICH','NWU','SAH')
pipelines_time<- pipelines_time %>%  mutate( across(c(1:2), .fns = ~./60))
print(pipelines_time)
# print(xtable(pipelines_time , type = "latex"),include.rownames=TRUE)
append_newline(filename_output)
print(xtable(pipelines_time),file = filename_output,append=TRUE, include.rownames = FALSE)


## numbers of Biomarkers calculated : NWU, CSF.RATIO, INFARCT,ICH,PHE,CSF,BRAIN volumes
################ DATA CLEANING 2 #################################
# "REMOVE SAH VOLUME > 200"
all_combined_data_frames <- all_combined_data_frames %>% filter(!(project_type=='SAH' & SAH.VOLUME > 100))
all_combined_data_frames <- all_combined_data_frames %>% filter(!(project_type=='ICH' & ICH.VOLUME > 100))
all_combined_data_frames <- all_combined_data_frames %>% filter(!(project_type=='ICH' & ICH.EDEMA.VOLUME > 100))
all_combined_data_frames <- all_combined_data_frames %>% filter(!(project_type=='INFARCT' & INFARCT.VOLUME >400))
all_combined_data_frames <- all_combined_data_frames %>% filter(!( TOTAL.CSF.VOLUME >500))

# all_combined_data_frames <- all_combined_data_frames[!(all_combined_data_frames$project_type =='SAH'  & all_combined_data_frames$SAH.VOLUME >100 ),]
# all_combined_data_frames <- all_combined_data_frames[!(all_combined_data_frames$project_type =='ICH'  & all_combined_data_frames$ICH.VOLUME >100 ),]
# all_combined_data_frames <- all_combined_data_frames[!(all_combined_data_frames$project_type =='ICH'  & all_combined_data_frames$ICH.EDEMA.VOLUME >100 ),]
# all_combined_data_frames <- all_combined_data_frames[!(all_combined_data_frames$project_type =='NWU'& all_combined_data_frames$INFARCT.VOLUME >400 ),]
# all_combined_data_frames <- all_combined_data_frames[!(all_combined_data_frames$project_type =='ICH'& all_combined_data_frames$INFARCT.VOLUME >400 ),]
# all_combined_data_frames <- all_combined_data_frames[!(all_combined_data_frames$project_type =='SAH'& all_combined_data_frames$INFARCT.VOLUME >400 ),]
# all_combined_data_frames <- all_combined_data_frames[!( all_combined_data_frames$TOTAL.CSF.VOLUME >500 ),]
# all_combined_data_frames <- all_combined_data_frames[!( all_combined_data_frames$INFARCT.VOLUME >400 ),]
# 
# #
# # all_combined_data_frames <- all_combined_data_frames %>%
# #   group_by(project_type) %>%
# #   filter(!(project_type == "SAH" & SAH.VOLUME > 200)) %>%
# #   ungroup()  # It's a good practice to ungroup data after operations that use grouping.
# # all_combined_data_frames <- all_combined_data_frames %>%
# #   group_by(project_type) %>%
# #   filter(!(project_type == "INFARCT" & TOTAL.CSF.VOLUME > 200)) %>%
# #   ungroup()  # It's a good practice to ungroup data after operations that use grouping.
# 
## Descriptive analysis of each biomarker:
means_biomarker<-all_combined_data_frames  %>%  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)))
sd_biomarker<-all_combined_data_frames  %>%  summarise(across(where(is.numeric), ~ IQR(.x, na.rm = TRUE)))
mean_sd<-rbind(means_biomarker,sd_biomarker)
mean_sd<-mean_sd %>% select(CSF.RATIO,NWU,ICH.VOLUME,ICH.EDEMA.VOLUME,SAH.VOLUME,INFARCT.VOLUME,TOTAL.CSF.VOLUME,BET.VOLUME)

row.names(mean_sd)<-c('Median','IQR')
# print(xtable(t(mean_sd) , type = "latex"),include.rownames=TRUE)
print(mean_sd)

append_newline(filename_output)
# print(xtable(t(mean_sd)),file = filename_output,append=TRUE, include.rownames = FALSE)
print(xtable(t(mean_sd) , type = "latex"),file = filename_output,append=TRUE,include.rownames=TRUE)
# histograms:
numerical_values<-c('SAH_SEG_SULCAL','CSF.RATIO','ICH.EDEMA.VOLUME','TOTAL.CSF.VOLUME','SAH.VOLUME','ICH.EDEMA.VOLUME','ICH.VOLUME','INFARCT.VOLUME','NWU')

for (colname1 in numerical_values){
  # colname1<-'CSF.RATIO'
  print(numerical_histogram(all_combined_data_frames,colname1,paste0('histogram',str_replace_all(colname1, "[[:punct:]]", ""),'.png')))

}
# #
# # colname1<-'CSF.RATIO'
# # print(numerical_histogram(all_combined_data_frames,colname1,paste0('histogram',str_replace_all(colname1, "[[:punct:]]", ""),'.png')))
# # colname1<-'NWU'
# # print(numerical_histogram(all_combined_data_frames,colname1,paste0('histogram',str_replace_all(colname1, "[[:punct:]]", ""),'.png')))
# # colname1<-'ICH.VOLUME'
# # print(numerical_histogram(all_combined_data_frames,colname1,paste0('histogram',str_replace_all(colname1, "[[:punct:]]", ""),'.png')))
# # colname1<-'ICH.EDEMA.VOLUME'
# # print(numerical_histogram(all_combined_data_frames,colname1,paste0('histogram',str_replace_all(colname1, "[[:punct:]]", ""),'.png')))
# # colname1<-'SAH.VOLUME'
# # print(numerical_histogram(all_combined_data_frames,colname1,paste0('histogram',str_replace_all(colname1, "[[:punct:]]", ""),'.png')))
# # colname1<-'INFARCT.VOLUME'
# # print(numerical_histogram(all_combined_data_frames,colname1,paste0('histogram',str_replace_all(colname1, "[[:punct:]]", ""),'.png')))
# ## correlation
# # col1_name1<-'NWU'
# # col2_name2<-'INFARCT.VOLUME'
# # imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# # numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)
# # col1_name1<-'NWU'
# # col2_name2<-'CSF.RATIO'
# # imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# # numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)
# # col1_name1<-'CSF.RATIO'
# # col2_name2<-'INFARCT.VOLUME'
# # imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# # numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)
# #
# # col1_name1<-'CSF.RATIO'
# # col2_name2<-'ICH.VOLUME'
# # imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# # numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)
# # col1_name1<-'CSF.RATIO'
# # col2_name2<-'ICH.EDEMA.VOLUME'
# # imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# # numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)
# # col1_name1<-'ICH.EDEMA.VOLUME'
# # col2_name2<-'ICH.VOLUME'
# # imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# # numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)
# # col1_name1<-'CSF.RATIO'
# # col2_name2<-'SAH.VOLUME'
# # imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# # numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)
# # col1_name1<-'TOTAL.CSF.VOLUME'
# # col2_name2<-'SAH.VOLUME'
# # imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# # numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)
# # col1_name1<-'TOTAL.CSF.VOLUME'
# # col2_name2<-'ICH.EDEMA.VOLUME'
# # imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# # numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)
# # col1_name1<-'CSF.RATIO'
# # col2_name2<-'SAH_SEG_SULCAL'
# # imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# # numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)
# #
# #
# # ## temporal relation of each biomarker with subject as a function  of hours and as a function of
# # # hours interval: GENERATED FROM THE FILE snipr_only.R
# #
# #
# # ## Overall model fit to the temporal data for each pipeline
# # # and check if it corroborates with the previous studies.
# #
# # ## from the clinical data:
# # numerical_values_1<-c('SAH_SEG_SULCAL','CSF.RATIO','ICH.EDEMA.VOLUME','TOTAL.CSF.VOLUME')
# #
# # numerical_values_2<-c('SAH.VOLUME','ICH.EDEMA.VOLUME','ICH.VOLUME','INFARCT.VOLUME','NWU')
for (first_numerical_val in numerical_values){
  for (second_numerical_val in numerical_values) {
    col1_name1<-first_numerical_val #'CSF.RATIO'
    col2_name2<-second_numerical_val #'SAH_SEG_SULCAL'
    imagefilename<-paste0('correlation_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
    numerical_cols_corr(all_combined_data_frames,col1_name1,col2_name2,imagefilename)


  }

}


write.csv(all_combined_data_frames,'../RFILES/all_combined_data_frames_04_16_2024.csv')
