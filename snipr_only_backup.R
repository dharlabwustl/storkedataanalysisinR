rm()
library(stringr)
library(plyr)
library(dplyr)
library(grid)
# library(tidyverse)
library(plyr)
library(tidyr)
library(lubridate)
library(ggExtra)
warning()
source('/storage1/fs1/dharr/Active/ATUL/PROJECTS/DOCKERIZE/SNIPR_PIPELINE/ANALYSIS_OF_ANALYTICS/utilities.R')
########################## DATA PREPARATION ###########################################
directory_of_files='/storage1/fs1/dharr/Active/ATUL/PROJECTS/DOCKERIZE/SNIPR_PIPELINE/ANALYSIS_OF_ANALYTICS/SNIPRFILES/SESSIONS/'
directory_of_outputfiles='/storage1/fs1/dharr/Active/ATUL/PROJECTS/DOCKERIZE/SNIPR_PIPELINE/ANALYSIS_OF_ANALYTICS/SNIPRFILES/SESSIONS/OUTPUT'
#print(directory_of_files)
filenames <- list.files(directory_of_files, pattern="*.csv", full.names=TRUE)

## analysis list
list_with_lesion_type<-function(filenames,cohort_include) {
list_df=c()
for (cohort_name in cohort_include ){
for (x in 1:length(filenames)) {
  if (grepl( cohort_include, filenames[x], fixed = TRUE)) {
  if (x==1) {
    list_df<-read.csv(filenames[x])
  }
  else {
  a <- read.csv(filenames[x])
  list_df<-rbind(list_df,a) 
  }
  }
}
}
return(list_df)
}
# ich_list_df<-list_with_lesion_type(filenames,c("sessions_ICH"))
# sah_list_df<-list_with_lesion_type(filenames,c("sessions_SAH"))
infarct_cohort_names<-c("sessions_COLI","sessionsBM","MGBBMC_sessions","sessionsWashU")
# infarct_list_df<-list_with_lesion_type(filenames,infarct_cohort_names)
# 
# ## total number of sessions
# total_sessions=dim(infarct_list_df)[1]+dim(ich_list_df)[1]+dim(sah_list_df)[1]
# print('dim(infarct_list_df)[1]+dim(ich_list_df)[1]+dim(sah_list_df)[1]')
# print(paste0('total_sessions::',total_sessions))

total_session_count=0
total_axials=0
total_thins=0
total_nifti=0
total_infarct_sessions=0
total_ich_sessions=0
total_sah_sessions=0
total_nwu=0
total_csf_ratio=0
total_sah=0
total_ich=0
all_combined_data_frames<-c()

for (x in 1:length(filenames)) {

  this_df<-read.csv(filenames[x])
  if (grepl("sessions_SAH", filenames[x], fixed = TRUE)){
    this_df$project<-"SAH"
  }
  if (grepl("MGBBMC_sessions", filenames[x], fixed = TRUE)){
    this_df$project<-"MGBBMC"
  }
  total_session_count=total_session_count+dim(this_df)[1]
######################################################
  this_column<-count_a_column_withtry(this_df,'axial_number')
  if (length(this_column) == 0 ) {
  this_column<-count_a_column_withtry(this_df,'AXIAL_SCAN_NUM') }
  total_axials=total_axials+length(this_column)
#############################################
  this_column<-count_a_column_withtry(this_df,'axial_thin_number')
  if (length(this_column) == 0 ) {
    this_column<-count_a_column_withtry(this_df,'THIN_SCAN_NUM') }
  total_thins=total_thins+length(this_column)
#####################################################
  this_column<-count_a_column_withtry(this_df,'NUMBER_NIFTIFILES')
  total_nifti=total_nifti+length(this_column) 
  #####################################################
  for (cohort_include in infarct_cohort_names){
  if (grepl(cohort_include, filenames[x], fixed = TRUE)) {

    print(cohort_include)
    this_column<-count_a_column_withtry(this_df,'ID')
    total_infarct_sessions=total_infarct_sessions+length(this_column) 
    this_column<-count_a_value_withtry(this_df,"NWU") #,".csv")
    total_nwu=total_nwu+length(this_column) 
    }
  }
#############################################################
    if (grepl("sessions_ICH", filenames[x], fixed = TRUE)) {
      
      print('sessions_ICH')
      this_column<-count_a_column_withtry(this_df,'ID')
      total_ich_sessions=total_ich_sessions+length(this_column) 
      this_column<-count_a_column_withtry(this_df,'ICH.VOLUME')
      total_ich=total_ich+length(this_column) 
      numerical_cols_corr(this_df,'ICH.VOLUME','ICH.EDEMA.VOLUME')
      numerical_cols_corr(this_df,'ICH.VOLUME','CSF.RATIO')
      numerical_cols_corr(this_df,'ICH.EDEMA.VOLUME','CSF.RATIO')
    }
  ##################################################################
  if (grepl("sessions_SAH", filenames[x], fixed = TRUE)) {
    
    print('sessions_SAH')

    this_column<-count_a_column_withtry(this_df,'ID')
    total_sah_sessions=total_sah_sessions+length(this_column)  
    this_column<-count_a_column_withtry(this_df,'CSV_FILE_NUM')
    total_sah=total_sah+length(this_column)  
    
  }
  ########################################################
  this_column<-count_a_column_withtry(this_df,'CSF.RATIO')
  total_csf_ratio=total_csf_ratio+length(this_column)
  all_combined_data_frames<-rbind.fill(all_combined_data_frames,this_df)
}

# print(paste0('total_session_count::',total_session_count))
# print(paste0('total_axials::',total_axials))
# print(paste0('total_thins::',total_thins))
# print(paste0('total_nifti::',total_nifti))
# print(paste0('total_infarct_sessions::',total_infarct_sessions))
# print(paste0('total_ich_sessions::',total_ich_sessions))
# print(paste0('total_sah_sessions::',total_sah_sessions))
# print(paste0('total_nwu::',total_nwu) ) # total_csf_ratio
# print(paste0('total_csf_ratio::',total_csf_ratio) )
# print(paste0('total_ich::',total_ich) ) # total_csf_ratio
# print(paste0('total_sah::',total_sah) )
totals<-c(total_session_count,total_axials,total_nifti,total_infarct_sessions,total_ich_sessions,total_sah_sessions,total_nwu,total_csf_ratio,total_ich,total_sah)
totals_cols<-c("total_session_count","total_axials","total_nifti","total_infarct_sessions","total_ich_sessions","total_sah_sessions","total_nwu","total_csf_ratio","total_ich","total_sah")
total_data_frame<-t(data.frame(totals))
colnames(total_data_frame)<-totals_cols 
# print(total_data_frame)
allfilestogether<- file.path(directory_of_outputfiles, 'all_cohorts.csv')
write.csv(all_combined_data_frames,allfilestogether,row.names = FALSE)
all_combined_data_frames<-read.csv(allfilestogether)

sah_volume_file<-'/storage1/fs1/dharr/Active/ATUL/PROJECTS/DOCKERIZE/SNIPR_PIPELINE/ANALYSIS_OF_ANALYTICS/SNIPRFILES/COMBINED_SESSIONS_SAH_METRICS_20231031043741.csv'
sah_volume_file_df<-read.csv(sah_volume_file)
sah_volume_file_df$CSF_RATIO<-as.character(sah_volume_file_df$CSF_RATIO)
# sah_volume_file_df$FileName_slice<-as.character(sah_volume_file_df$FILENAME)
## merge SAH to the all_combined_data_frames
all_combined_data_frames1<-full_join(all_combined_data_frames,sah_volume_file_df,by=c('label'='SESSION_LABEL'))
all_combined_data_frames1$CSF.RATIO[all_combined_data_frames1$project=='SAH']<-all_combined_data_frames1$CSF_RATIO[all_combined_data_frames1$project=='SAH']
all_combined_data_frames1$FileName_slice[all_combined_data_frames1$project=='SAH']<-all_combined_data_frames1$FILENAME[all_combined_data_frames1$project=='SAH']

# all_combined_data_frames1$subject_id1<-sapply(strsplit(all_combined_data_frames1$label, '\\_'), function(x) paste(x[1:2], collapse = '_'))
all_combined_data_frames<-all_combined_data_frames1

cohort_names<-unique(all_combined_data_frames$project)
all_combined_data_frames$SAH.SEG<-all_combined_data_frames$SAH_SEG_TOTAL
row_names<-c()
colname="NWU"
row_names[1]<-colname
row1<-count_biomarkers(all_combined_data_frames,cohort_names,colname)
colname="ICH.VOLUME"
row_names[2]<-colname
row1<-rbind(row1,count_biomarkers(all_combined_data_frames,cohort_names,colname))
colname="CSF.RATIO"
row_names[3]<-colname
row1<-rbind(row1,count_biomarkers(all_combined_data_frames,cohort_names,colname))
colname="SAH.SEG"
row_names[4]<-colname
row1<-rbind(row1,count_biomarkers(all_combined_data_frames,cohort_names,colname))
row1_df<-data.frame(row1)
colnames(row1_df)<-cohort_names
rownames(row1_df)<-row_names
row1_df_t<-data.frame(t(row1_df))
row1_df_t$cohorts<-cohort_names
row1_df_t_1<-row1_df_t %>% pivot_longer(cols=row_names,
                           names_to='ANALYSIS.TYPE',
                           values_to='COUNT')
row1_df_t_1[row1_df_t_1 == 0] <- NA


colname<-'CSV_FILE_PATH'
subspatt<-'.csv'
total_csv_file<-dim(count_a_column_with_char_v1(all_combined_data_frames,this_column,colname,subspatt))[1]
print(total_csv_file)
p<-ggplot(row1_df_t_1, aes(fill=cohorts, y=COUNT, x=ANALYSIS.TYPE)) +
  geom_bar(position='dodge', stat='identity') +
  theme_classic()+
  geom_text(aes(label=COUNT), position=position_dodge(width=0.9), vjust=-0.25)+
  annotate(
    "label", x=length(row_names)*0.9, y=max(na.omit(row1_df_t_1$COUNT))*0.95, label=paste('Session', 'Analyzed:',total_csv_file,sep="\n"),
    color="red", size=5) # +

print(p)
numerical_cols_corr(all_combined_data_frames,'ICH.EDEMA.VOLUME','CSF.RATIO')
numerical_cols_corr(all_combined_data_frames,'INFARCT.VOLUME','CSF.RATIO')
numerical_cols_corr(all_combined_data_frames,'NWU','CSF.RATIO')
numerical_cols_corr(all_combined_data_frames,'INFARCT.VOLUME','NWU')
# numerical_cols_corr(all_combined_data_frames,'INFARCT.VOLUME','CSF.RATIO')
numerical_cols_corr(all_combined_data_frames,'INFARCT.VOLUME','TOTAL.CSF.VOLUME')
numerical_cols_corr(all_combined_data_frames,'ICH.VOLUME','CSF.RATIO')
numerical_cols_corr(all_combined_data_frames,'ICH.VOLUME','TOTAL.CSF.VOLUME')
numerical_cols_corr(all_combined_data_frames,'ICH.EDEMA.VOLUME','TOTAL.CSF.VOLUME')
numerical_cols_corr(all_combined_data_frames,'ICH.VOLUME','ICH.EDEMA.VOLUME')
# # numerical_histogram( all_combined_data_frames,'ICH.EDEMA.VOLUME')
# # numerical_histogram( all_combined_data_frames,'CSF.RATIO')
# ################################################ Temporal analysis ############################
all_combined_data_frames$filenamesplit<-sapply(strsplit(all_combined_data_frames$FileName_slice, '\\_'), function(x) paste(x[3:4], collapse = '_'))
all_combined_data_frames$datetime_synth<-as.POSIXct(all_combined_data_frames$filenamesplit, format="%m%d%Y_%H%M", tz="UTC")
all_combined_data_frames %>% arrange(all_combined_data_frames)
all_combined_data_frames <- all_combined_data_frames  %>% arrange(datetime_synth) %>% arrange(subject_id)
all_combined_data_frames<-all_combined_data_frames %>% group_by(subject_id) %>% mutate(subj_session_count = 1:n()) %>% mutate(datetime_synth.diff = datetime_synth - first(datetime_synth))
all_combined_data_frames_subset<-all_combined_data_frames %>% select(subject_id,subj_session_count,datetime_synth.diff,CSF.RATIO)
all_combined_data_frames_subset<-all_combined_data_frames_subset %>% drop_na()
p1<-ggplot(all_combined_data_frames_subset, aes(x=datetime_synth.diff, y=CSF.RATIO, color=factor(subject_id))) +
  geom_line() + geom_point() +
  theme_bw()
