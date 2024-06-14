rm()
library(plotrix)
library(neatStats)
library(rstatix)
library(stringr)
library(plyr)
library(dplyr)
library(grid)
library(plyr)
library(tidyr)
library(lubridate)
library(ggExtra)
library(lme4)
library(tidyverse)
library(xtable)
##############
# library(tidyverse)
# library(ggplot2)
# library(ggpubr)
# library(xtable)
# library(stringr)
warning()
RFILES_DIR<-'D:/RSTUDIOJan30/RFILES'
source(file.path(RFILES_DIR,'utilities.R'))
DATA_DIR<-'D:/RSTUDIOJan30'
########################## DATA PREPARATION ###########################################
directory_of_files=file.path(DATA_DIR,"SNIPRFILES","SESSIONS")
directory_of_outputfiles=file.path(DATA_DIR,'SNIPRFILES','SESSIONS','OUTPUT')
#print(directory_of_files)
filenames <- list.files(directory_of_files , pattern="*.csv", full.names=TRUE)

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

sah_volume_file<-file.path(DATA_DIR,'SNIPRFILES','COMBINED_SESSIONS_SAH_METRICS_20231031043741.csv')
sah_volume_file_df<-read.csv(sah_volume_file)
# sah_volume_file_df$CSF_RATIO<-as.character(sah_volume_file_df$CSF_RATIO)
# sah_volume_file_df$FileName_slice<-as.character(sah_volume_file_df$FILENAME)
## merge SAH to the all_combined_data_frames
all_combined_data_frames1<-full_join(all_combined_data_frames,sah_volume_file_df,by=c('label'='SESSION_LABEL'))
all_combined_data_frames1$CSF.RATIO[all_combined_data_frames1$project=='SAH']<-all_combined_data_frames1$CSF_RATIO[all_combined_data_frames1$project=='SAH']
all_combined_data_frames1$FileName_slice[all_combined_data_frames1$project=='SAH']<-all_combined_data_frames1$FILENAME[all_combined_data_frames1$project=='SAH']
all_combined_data_frames1$TOTAL.CSF.VOLUME[all_combined_data_frames1$project=='SAH']<-all_combined_data_frames1$CSF[all_combined_data_frames1$project=='SAH']

all_combined_data_frames1$subject_id1<-sapply(strsplit(all_combined_data_frames1$label, '\\_'), function(x) paste(x[1:2], collapse = '_'))
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
all_combined_data_frames_ICH<-all_combined_data_frames
all_combined_data_frames_ICH<-all_combined_data_frames_ICH %>% filter(ICH.VOLUME < 200)  %>% filter(ICH.EDEMA.VOLUME < 200)
numerical_cols_corr(all_combined_data_frames_ICH,'ICH.VOLUME','ICH.EDEMA.VOLUME')
# # numerical_histogram( all_combined_data_frames,'ICH.EDEMA.VOLUME')
# # numerical_histogram( all_combined_data_frames,'CSF.RATIO')
# ################################################ Temporal analysis ############################
all_combined_data_frames$filenamesplit<-sapply(strsplit(all_combined_data_frames$FileName_slice, '\\_'), function(x) paste(x[3:4], collapse = '_'))
all_combined_data_frames$datetime_synth<-as.POSIXct(all_combined_data_frames$filenamesplit, format="%m%d%Y_%H%M", tz="UTC")
all_combined_data_frames %>% arrange(all_combined_data_frames)
all_combined_data_frames <- all_combined_data_frames  %>% arrange(datetime_synth) %>% arrange(subject_id1)
all_combined_data_frames<-all_combined_data_frames %>% group_by(subject_id1) %>% mutate(subj_session_count = 1:n()) %>% mutate(datetime_synth.diff = datetime_synth - first(datetime_synth)) %>% mutate(delta_csf = TOTAL.CSF.VOLUME - first(TOTAL.CSF.VOLUME))
all_combined_data_frames<-all_combined_data_frames %>% mutate_at(c('datetime_synth.diff'), as.numeric)
all_combined_data_frames<-all_combined_data_frames %>% mutate_at(c('CSF.RATIO'), as.numeric)
all_combined_data_frames$datetime_synth.diff<-(all_combined_data_frames$datetime_synth.diff)/3600
all_combined_data_frames$project_type<-''
all_combined_data_frames$project_type[all_combined_data_frames$project=='COLI']<-'INFARCT'
all_combined_data_frames$project_type[all_combined_data_frames$project=='BM']<-'INFARCT'
all_combined_data_frames$project_type[all_combined_data_frames$project=='MGBBMC']<-'INFARCT'
all_combined_data_frames$project_type[all_combined_data_frames$project=='WashU']<-'INFARCT'
all_combined_data_frames$project_type[all_combined_data_frames$project=='ICH']<-'ICH'
all_combined_data_frames$project_type[all_combined_data_frames$project=='SAH']<-'SAH'
all_combined_data_frames$axial_number[all_combined_data_frames$project=='SAH']<-all_combined_data_frames$AXIAL_SCAN_NUM[all_combined_data_frames$project=='SAH']
all_combined_data_frames$axial_thin_number[all_combined_data_frames$project=='SAH']<-all_combined_data_frames$THIN_SCAN_NUM[all_combined_data_frames$project=='SAH']
all_combined_data_frames<-all_combined_data_frames %>% mutate_at(c( 'datetime_synth.diff'), as.numeric)
all_combined_data_frames$timeinterval<-NA
all_combined_data_frames$timeinterval[all_combined_data_frames$datetime_synth.diff==0]<-0
all_combined_data_frames$timeinterval[all_combined_data_frames$datetime_synth.diff>0 & all_combined_data_frames$datetime_synth.diff<=12]<-12
all_combined_data_frames$timeinterval[all_combined_data_frames$datetime_synth.diff>12 & all_combined_data_frames$datetime_synth.diff<=24]<-24
all_combined_data_frames$timeinterval[all_combined_data_frames$datetime_synth.diff>24 & all_combined_data_frames$datetime_synth.diff<=36]<-36
all_combined_data_frames$timeinterval[all_combined_data_frames$datetime_synth.diff>36 & all_combined_data_frames$datetime_synth.diff<=48]<-48
all_combined_data_frames$timeinterval[all_combined_data_frames$datetime_synth.diff>48 & all_combined_data_frames$datetime_synth.diff<=60]<-60
all_combined_data_frames$timeinterval[all_combined_data_frames$datetime_synth.diff>60 & all_combined_data_frames$datetime_synth.diff<=72]<-72
all_combined_data_frames$timeinterval[all_combined_data_frames$datetime_synth.diff>72 & all_combined_data_frames$datetime_synth.diff<=96]<-96
all_combined_data_frames$timeinterval[all_combined_data_frames$datetime_synth.diff>96]<-120
all_combined_data_frames$timeinterval_asfactor<-factor(all_combined_data_frames$timeinterval,levels = c(0,12,24,36,48,60,72,96,120))
# all_combined_data_frames<-all_combined_data_frames %>% filter(datetime_synth.diff < 97)
######################################
# all_combined_data_frames$lt12hrs<-all_combined_data_frames$timeinterval[all_combined_data_frames$datetime_synth.diff<12]<-'lt_12hrs'
# all_combined_data_frames_lt12hrs<-all_combined_data_frames %>% filter(lt12hrs=='lt_12hrs')
# numerical_cols_corr(all_combined_data_frames,'ICH.VOLUME','ICH.EDEMA.VOLUME')

write.csv(all_combined_data_frames,'all_combined_data_frames_1.csv',row.names = FALSE) ##'all_combined_data_frames_04_16_2024.csv',row.names = FALSE) #####
#################### DATA CLEANING##############################
all_combined_data_frames<-all_combined_data_frames%>%filter(project!="MGBBMC")
all_combined_data_frames<-all_combined_data_frames %>% rename('SAH.VOLUME'='SAH_SEG_TOTAL')
all_combined_data_frames <- all_combined_data_frames %>% filter(!(project_type=='SAH' & SAH.VOLUME > 100))
all_combined_data_frames <- all_combined_data_frames %>% filter(!(project_type=='ICH' & ICH.VOLUME > 100))
all_combined_data_frames <- all_combined_data_frames %>% filter(!(project_type=='ICH' & ICH.EDEMA.VOLUME > 100))
all_combined_data_frames <- all_combined_data_frames %>% filter(!(project_type=='INFARCT' & INFARCT.VOLUME >400))
all_combined_data_frames <- all_combined_data_frames %>% filter(!( TOTAL.CSF.VOLUME >500))
all_combined_data_frames <- all_combined_data_frames %>%
  mutate(
    ICH.VOLUME.RATIO = ICH.VOLUME/ICH.EDEMA.VOLUME 
  )
########################################################################

temporal_trends<-function(all_combined_data_frames,numeric_colname,filenameforthis=''){
all_combined_data_frames_subset<-all_combined_data_frames %>% select(timeinterval,project_type,subject_id1,subj_session_count,datetime_synth.diff,as.name(numeric_colname))
all_combined_data_frames_subset<-all_combined_data_frames_subset %>% drop_na()
print(dim(all_combined_data_frames_subset))

p <- ggplot(data = all_combined_data_frames_subset, aes_string(x ="timeinterval", y = numeric_colname, group = "subject_id1"))

p<-p +  geom_line(aes_string(color='subject_id1'))   + stat_smooth(aes(group = 1), method = "lm") +
  stat_summary(aes(group = 1), geom = "point", fun.y = mean,
               shape = 17, size = 3) +  theme(legend.position="none") +facet_grid(. ~ project_type)# + geom_point() +stat_summary(aes(group = 1), geom = "point", fun.y = mean,       shape = 17, size = 3)
print(p)
if (str_length(filenameforthis)>0){
ggsave(filenameforthis,dpi=300)
}
}
##########################################
###################################
temporal_trends_median_plot<-function(all_combined_data_frames,numeric_colname,filenameforthis='',legend_title='',xlabel='',ylabel=''){
  
  xlabel_1<-''
  ylabel_1<-''
  legend_title_1<-''
  
  if (str_length(xlabel)>1){xlabel_1<-xlabel}
  else{  xlabel_1<-'Time (hours)'}
  if (str_length(ylabel)>1){ylabel_1<-ylabel}
  else {  ylabel_1<-numeric_colname}
  if (str_length(ylabel)>1){legend_title_1<-legend_title}
  else {  legend_title_1<-'Cohorts'}
  all_combined_data_frames_subset<-all_combined_data_frames %>% select(timeinterval,project_type,subject_id1,subj_session_count,datetime_synth.diff,as.name(numeric_colname))
  all_combined_data_frames_subset<-all_combined_data_frames_subset %>% drop_na()
  expression<-paste0('all_combined_data_frames_subset$this_numeric<-all_combined_data_frames_subset$',numeric_colname)
  print(expression)
  eval(parse(text=expression))
  project_type_name<-unique(all_combined_data_frames$project_type)
  title_this_plot<-''
  for (each_project_id in 1:length(project_type_name)){
    this_expression=paste0('this_df_',each_project_id,'<-','all_combined_data_frames_subset %>% filter(project_type==project_type_name[',each_project_id,'])')
    eval(parse(text=this_expression))
    print(this_expression)   
    this_expression<-paste0('N<-nrow(','this_df_',each_project_id ,')')
    eval(parse(text=this_expression))
    if (N>6){
      this_expression<-paste0('correl<-kruskal.test(this_numeric~timeinterval,data=this_df_',each_project_id,')')
      eval(parse(text=this_expression))
      
      title_this_plot<-paste(title_this_plot,paste0(project_type_name[each_project_id],': N:',N,' corr:',signif(correl$statistic, digits = 2),' p-value:',signif(correl$p.value, digits = 2)),sep='\n')
      
      print(title_this_plot)
    }
  }
  
  
  data<-all_combined_data_frames_subset %>%
    group_by(project_type, timeinterval) %>%
    summarise(mean_value = median(this_numeric), # mean(this_numeric),
              Lower = quantile(this_numeric, 0.25),
              Upper = quantile(this_numeric, 0.75),
              sd_error = mad(this_numeric)) #std.error(this_numeric)) # %>%
  corr_coeff<-cor.test(data$timeinterval,data$mean_value)
  print(corr_coeff)
  title_this_plot<-paste( sep="\n",paste0('p-value:', sprintf("%.3f", corr_coeff$p.value),paste0(', corr:',sprintf("%.3f", corr_coeff$estimate)))) 
  latex_output <-capture.output(data)
  cat(latex_output, file = 'tables_tex.tex', append = TRUE, sep = "\n")
  plot<-ggplot(data=data,aes(x = timeinterval, y = mean_value, color = project_type, group = project_type)) +
    geom_point() +
    geom_line(show.legend = FALSE) +
    # geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
    # geom_errorbar(aes(ymin = mean_value-sd_error, ymax = mean_value+sd_error)) +
    # labs(title = ) + geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue")
    geom_smooth(method = "lm", se = FALSE,  aes(group = project_type)) +
    #######     ggtitle(paste(paste0(data_frame_name), sep="\n",paste0(paste0('N:',data_count),', p-value:', sprintf("%.3f", corr_coeff$p.value),paste0(', corr:',sprintf("%.3f", corr_coeff$estimate)))) )  + 
    # geom_point(color="red")
    
    labs (title=title_this_plot,x=xlabel_1,y=ylabel_1) + # scale_fill_discrete(name = "New Legend Title")+
    theme_classic() +facet_grid(. ~ project_type)+theme(legend.position = "none") # +
  # guides(fill=guide_legend(title="New Legend Title"))
  # annotate("text", x = Inf, y = Inf, label = sprintf("y = %.2fx + %.2f", slope, intercept),
  #          hjust = 1.1, vjust = 2, size = 5, color = "black")# + guides(fill=guide_legend(title="New Legend Title"))
  if (str_length(filenameforthis)>1){
    ggsave(filenameforthis,dpi=300)}
  print(plot)
  return(data)
  
}


temporal_trends_mean_plot<-function(all_combined_data_frames,numeric_colname,filenameforthis='',legend_title='',xlabel='',ylabel=''){

  xlabel_1<-''
  ylabel_1<-''
  legend_title_1<-''

  if (str_length(xlabel)>1){xlabel_1<-xlabel}
  else{  xlabel_1<-'Time (hours)'}
  if (str_length(ylabel)>1){ylabel_1<-ylabel}
  else {  ylabel_1<-numeric_colname}
  if (str_length(ylabel)>1){legend_title_1<-legend_title}
  else {  legend_title_1<-'Cohorts'}
  all_combined_data_frames_subset<-all_combined_data_frames %>% select(timeinterval,project_type,subject_id1,subj_session_count,datetime_synth.diff,as.name(numeric_colname))
  all_combined_data_frames_subset<-all_combined_data_frames_subset %>% drop_na()
  expression<-paste0('all_combined_data_frames_subset$this_numeric<-all_combined_data_frames_subset$',numeric_colname)
  print(expression)
  eval(parse(text=expression))
  project_type_name<-unique(all_combined_data_frames$project_type)
  title_this_plot<-''
  for (each_project_id in 1:length(project_type_name)){
    this_expression=paste0('this_df_',each_project_id,'<-','all_combined_data_frames_subset %>% filter(project_type==project_type_name[',each_project_id,'])')
    eval(parse(text=this_expression))
    print(this_expression)   
    this_expression<-paste0('N<-nrow(','this_df_',each_project_id ,')')
    eval(parse(text=this_expression))
    if (N>6){
    this_expression<-paste0('correl<-kruskal.test(this_numeric~timeinterval,data=this_df_',each_project_id,')')
    eval(parse(text=this_expression))

    title_this_plot<-paste(title_this_plot,paste0(project_type_name[each_project_id],': N:',N,' corr:',signif(correl$statistic, digits = 2),' p-value:',signif(correl$p.value, digits = 2)),sep='\n')

    print(title_this_plot)
    }
  }
  

  data<-all_combined_data_frames_subset %>%
    group_by(project_type, timeinterval) %>%
    summarise(mean_value = mean(this_numeric), 
              sd_error = std.error(this_numeric)) # %>%
    plot<-ggplot(data=data,aes(x = timeinterval, y = mean_value, color = project_type, group = project_type)) +
    geom_point() +
    geom_line(show.legend = FALSE) +
    geom_errorbar(aes(ymin = mean_value-sd_error, ymax = mean_value+sd_error)) +
    # labs(title = ) + geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue")
    geom_smooth(method = "lm", se = FALSE,  aes(group = project_type)) +
    labs (title=title_this_plot,x=xlabel_1,y=ylabel_1) + # scale_fill_discrete(name = "New Legend Title")+
    theme_classic() +facet_grid(. ~ project_type)+theme(legend.position = "none") # +
      # guides(fill=guide_legend(title="New Legend Title"))
      # annotate("text", x = Inf, y = Inf, label = sprintf("y = %.2fx + %.2f", slope, intercept),
      #          hjust = 1.1, vjust = 2, size = 5, color = "black")# + guides(fill=guide_legend(title="New Legend Title"))
    if (str_length(filenameforthis)>1){
    ggsave(filenameforthis,dpi=300)}
    print(plot)
    
  
}

temporal_trends_mean_plot_linear_regression<-function(all_combined_data_frames,numeric_colname,filenameforthis='',texfilename='test.tex',legend_title='',xlabel='',ylabel=''){
  print(texfilename)
  xlabel_1<-''
  ylabel_1<-''
  legend_title_1<-''
  
  if (str_length(xlabel)>1){xlabel_1<-xlabel}
  else{  xlabel_1<-'Time (hours)'}
  if (str_length(ylabel)>1){ylabel_1<-ylabel}
  else {  ylabel_1<-numeric_colname}
  if (str_length(ylabel)>1){legend_title_1<-legend_title}
  else {  legend_title_1<-'Cohorts'}
  all_combined_data_frames_subset<-all_combined_data_frames %>% select(timeinterval,project_type,subject_id1,subj_session_count,datetime_synth.diff,as.name(numeric_colname))
  all_combined_data_frames_subset<-all_combined_data_frames_subset %>% drop_na()
  expression<-paste0('all_combined_data_frames_subset$this_numeric<-all_combined_data_frames_subset$',numeric_colname)
  print(expression)
  eval(parse(text=expression))
  project_type_name<-unique(all_combined_data_frames$project_type)
  title_this_plot<-''
  for (each_project_id in 1:length(project_type_name)){
        this_expression=paste0('this_df_',each_project_id,'<-','all_combined_data_frames_subset %>% filter(project_type==project_type_name[',each_project_id,'])')
    eval(parse(text=this_expression))
    print(this_expression)   
    this_expression<-paste0('N<-nrow(','this_df_',each_project_id ,')')
    eval(parse(text=this_expression))
    if (N>6){
      this_expression<-paste0('model<-lm(this_numeric~timeinterval,data=this_df_',each_project_id,')')
      # this_expression<-paste0('correl<-kruskal.test(this_numeric~timeinterval,data=this_df_',each_project_id,')')
      eval(parse(text=this_expression))
      model_summary <- summary(model)
      #latex_table <- xtable(model_summary)
      # Save summary to a text file
      # Extract R-squared and other statistics
      r_squared <- summary(model)$r.squared
      adj_r_squared <- summary(model)$adj.r.squared
      f_statistic <- summary(model)$fstatistic[1]
      cat(paste0(numeric_colname,'::',project_type_name[each_project_id]), file = texfilename, append = TRUE, sep = "\n")
      
      # Write a custom summary to a text file
      latex_output <-capture.output(cat("Summary of Linear Model Fit:\n",
                         "R-squared: ", r_squared, "\n",
                         "Adjusted R-squared: ", adj_r_squared, "\n",
                         "F-statistic: ", f_statistic, "\n"))
      # numeric_colname

      latex_output <-capture.output(model_summary)
      cat(latex_output, file = texfilename, append = TRUE, sep = "\n")
      latex_output <- capture.output(print(xtable(model_summary), type = "latex", include.rownames = FALSE))
      
      # Append the captured output to an existing file
      cat(latex_output, file = texfilename, append = TRUE, sep = "\n")
      
      # Print the LaTeX code for the table
      # Append xtable output to a LaTeX file
      # print(xtable(model_summary), type = "latex", file = texfilename, append = TRUE, include.rownames = FALSE)
      
      # print(xtable(model_summary),file = texfilename,append=TRUE, include.rownames = FALSE)
      # 
      # title_this_plot<-paste(title_this_plot,paste0(project_type_name[each_project_id],': N:',N,' corr:',signif(correl$statistic, digits = 2),' p-value:',signif(correl$p.value, digits = 2)),sep='\n')
      # 
      # print(title_this_plot)
    }
  }
  
  
  # data<-all_combined_data_frames_subset %>%
  #   group_by(project_type, timeinterval) %>%
  #   summarise(mean_value = mean(this_numeric),
  #             sd_error = std.error(this_numeric)) # %>%
  # plot<-ggplot(data=data,aes(x = timeinterval, y = mean_value, color = project_type, group = project_type)) +
  #   geom_point() +
  #   geom_line(show.legend = FALSE) +
  #   geom_errorbar(aes(ymin = mean_value-sd_error, ymax = mean_value+sd_error)) +
  #   # labs(title = ) + geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue")
  #   geom_smooth(method = "lm", se = FALSE,  aes(group = project_type)) +
  #   labs (title=title_this_plot,x=xlabel_1,y=ylabel_1) + # scale_fill_discrete(name = "New Legend Title")+
  #   theme_classic() +facet_grid(. ~ project_type)+theme(legend.position = "none") # +
  # # guides(fill=guide_legend(title="New Legend Title"))
  # # annotate("text", x = Inf, y = Inf, label = sprintf("y = %.2fx + %.2f", slope, intercept),
  # #          hjust = 1.1, vjust = 2, size = 5, color = "black")# + guides(fill=guide_legend(title="New Legend Title"))
  # if (str_length(filenameforthis)>1){
  #   ggsave(filenameforthis,dpi=300)}
  # print(plot)
  
  
}

examine_if_temporal_trends_are_different<-function(all_combined_data_frames,texfilename=''){ #,ischemic_data,sah_data,ich_data) {
  ischemic_data<-all_combined_data_frames %>% filter(project_type=="INFARCT")
  sah_data<-all_combined_data_frames %>% filter(project_type=="SAH")
  ich_data<-all_combined_data_frames %>% filter(project_type=="ICH")
  # Assuming you have datasets data1, data2, data3
  model1 <- lm( CSF.RATIO ~ timeinterval, data = ischemic_data)
  model2 <- lm( CSF.RATIO ~ timeinterval, data = sah_data)
  model3 <- lm( CSF.RATIO ~ timeinterval, data = ich_data)
  
  # You can use ANOVA to test if there are statistically significant differences among models
  # library(car)  # car package might be needed for Type II or III ANOVA
  global_model <- lm(CSF.RATIO ~ timeinterval * project_type, data = all_combined_data_frames)  # combined data needs a dataset indicator
  Anova(global_model, type = "III")
  print(Anova(global_model, type = "III"))
  cat('CSF.RATIO', file = texfilename, append = TRUE, sep = "\n")
  latex_output <-capture.output(Anova(global_model, type = "III"))
  cat(latex_output, file = texfilename, append = TRUE, sep = "\n")
  ############################################
  model1 <- lm( TOTAL.CSF.VOLUME ~ timeinterval, data = ischemic_data)
  model2 <- lm( TOTAL.CSF.VOLUME ~ timeinterval, data = sah_data)
  model3 <- lm( TOTAL.CSF.VOLUME ~ timeinterval, data = ich_data)
  
  # You can use ANOVA to test if there are statistically significant differences among models
  # library(car)  # car package might be needed for Type II or III ANOVA
  global_model <- lm(TOTAL.CSF.VOLUME ~ timeinterval * project_type, data = all_combined_data_frames)  # combined data needs a dataset indicator
  Anova(global_model, type = "III")
  print(Anova(global_model, type = "III"))
  cat('TOTAL.CSF.VOLUME', file = texfilename, append = TRUE, sep = "\n")
  latex_output <-capture.output(Anova(global_model, type = "III"))
  cat(latex_output, file = texfilename, append = TRUE, sep = "\n")
  
}




correlation_among_trend_plots<-function(all_combined_data_frames,numeric_colname,filenameforthis='',legend_title='',xlabel='',ylabel=''){
  all_combined_data_frames_subset1<-all_combined_data_frames %>% select(timeinterval,project_type,'NWU','TOTAL.CSF.VOLUME','CSF.RATIO','INFARCT.VOLUME','SAH.VOLUME','ICH.VOLUME')
#' 
#'   #'NWU','TOTAL.CSF.VOLUME','CSF.RATIO','INFARCT.VOLUME','SAH.VOLUME','ICH.VOLUME'
#'   
#'   #subject_id1,subj_session_count,datetime_synth.diff,as.name(numeric_colname))
  # all_combined_data_frames_subset1<-all_combined_data_frames_subset1 %>% drop_na()
#'   
  data<-all_combined_data_frames_subset1 %>%
    group_by(timeinterval) %>%
    summarise(median_NWU = median(INFARCT.VOLUME), median_NWU = median(NWU),median_TOTAL.CSF.VOLUME = median(TOTAL.CSF.VOLUME),median_CSF.RATIO = median(CSF.RATIO)) #std.error(this_numeric)) # %>%

#'     xlabel_1<-''
#'     ylabel_1<-''
#'     legend_title_1<-''
#'     if (str_length(xlabel)>1){xlabel_1<-xlabel}
#'     else{  xlabel_1<-'Time (hours)'}
#'     if (str_length(ylabel)>1){ylabel_1<-ylabel}
#'     else {  ylabel_1<-numeric_colname}
#'     if (str_length(ylabel)>1){legend_title_1<-legend_title}
#'     else {  legend_title_1<-'Cohorts'}
#'     all_combined_data_frames_subset<-all_combined_data_frames %>% select(timeinterval,project_type,subject_id1,subj_session_count,datetime_synth.diff,as.name(numeric_colname))
#'     all_combined_data_frames_subset<-all_combined_data_frames_subset %>% drop_na()
#'     expression<-paste0('all_combined_data_frames_subset$this_numeric<-all_combined_data_frames_subset$',numeric_colname)
#'     print(expression)
#'     eval(parse(text=expression))
#'     project_type_name<-unique(all_combined_data_frames$project_type)
#'     title_this_plot<-''
#'     for (each_project_id in 1:length(project_type_name)){
#'       this_expression=paste0('this_df_',each_project_id,'<-','all_combined_data_frames_subset %>% filter(project_type==project_type_name[',each_project_id,'])')
#'       eval(parse(text=this_expression))
#'       print(this_expression)   
#'       this_expression<-paste0('N<-nrow(','this_df_',each_project_id ,')')
#'       eval(parse(text=this_expression))
#'       if (N>6){
#'         this_expression<-paste0('correl<-kruskal.test(this_numeric~timeinterval,data=this_df_',each_project_id,')')
#'         eval(parse(text=this_expression))
#'         
#'         title_this_plot<-paste(title_this_plot,paste0(project_type_name[each_project_id],': N:',N,' corr:',signif(correl$statistic, digits = 2),' p-value:',signif(correl$p.value, digits = 2)),sep='\n')
#'         
#'         print(title_this_plot)
#'       }
#'     }
#'     
#'     
#'     data<-all_combined_data_frames_subset %>%
#'       group_by(project_type, timeinterval) %>%
#'       summarise(mean_value = median(this_numeric), # mean(this_numeric),
#'                 Lower = quantile(this_numeric, 0.25),
#'                 Upper = quantile(this_numeric, 0.75),
#'                 sd_error = mad(this_numeric)) #std.error(this_numeric)) # %>%
#'     plot<-ggplot(data=data,aes(x = timeinterval, y = mean_value, color = project_type, group = project_type)) +
#'       geom_point() +
#'       geom_line(show.legend = FALSE) +
#'       geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
#'       # geom_errorbar(aes(ymin = mean_value-sd_error, ymax = mean_value+sd_error)) +
#'       # labs(title = ) + geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue")
#'       geom_smooth(method = "lm", se = FALSE,  aes(group = project_type)) +
#'       labs (title=title_this_plot,x=xlabel_1,y=ylabel_1) + # scale_fill_discrete(name = "New Legend Title")+
#'       theme_classic() +facet_grid(. ~ project_type)+theme(legend.position = "none") # +
#'     # guides(fill=guide_legend(title="New Legend Title"))
#'     # annotate("text", x = Inf, y = Inf, label = sprintf("y = %.2fx + %.2f", slope, intercept),
#'     #          hjust = 1.1, vjust = 2, size = 5, color = "black")# + guides(fill=guide_legend(title="New Legend Title"))
#'     if (str_length(filenameforthis)>1){
#'       ggsave(filenameforthis,dpi=300)}
#'     print(plot)
    
    
  }
  
## with respect to INFARCT VOLUME:
  

  
  
  
  
##########################################


#
############################## Descriptive statistics######################
descriptive_analysis<-function(all_combined_data_frames,numeric_colname,filenameforthis=''){
  all_combined_data_frames_subset<-all_combined_data_frames %>% select(timeinterval_asfactor,timeinterval,project_type,subject_id1,subj_session_count,datetime_synth.diff,as.name(numeric_colname))
  all_combined_data_frames_subset<-all_combined_data_frames_subset %>% drop_na()
  print(all_combined_data_frames_subset %>%group_by(timeinterval) %>%
          get_summary_stats(as.name(numeric_colname)))
  # expression<-paste0('all_combined_data_frames_subset %>% do(model = lm(',numeric_colname,' ~ ','timeinterval',', data = . ))')
  # model<-eval(parse(text=expression))
  p1<-ggplot(all_combined_data_frames_subset, aes_string('timeinterval_asfactor', numeric_colname, fill = 'timeinterval_asfactor')) +
    geom_boxplot( show.legend = FALSE) +
    # geom_abline(intercept = model$model[1], slope = model$model[2])+
    labs(x = "Time interval (hrs)", y = numeric_colname) + # guides(fill=guide_legend(title="Time intervals")) + 
    facet_grid(. ~ project_type) # facet_wrap(. ~ project_type, ncol = 1)
  print(p1)
  if (str_length(filenameforthis)>0){
    ggsave(filenameforthis,dpi=300)
  }
  # lin_agesexinter <- lmer(as.name(numeric_colname) ~ timeinterval, data = all_combined_data_frames_subset)
  # prin
  # p2<-group_by(all_combined_data_frames_subset, project_type, timeinterval_asfactor) %>%
  #   summarise(mean_distance = mean(as.name(numeric_colname)), .groups = "drop") %>%
  #   ggplot(aes_string("project_type", "mean_distance", fill = "timeinterval_asfactor", label = round(mean_distance))) +
  #   geom_col(position = "dodge") +
  #   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  #   coord_flip() +
  #   labs(x = "", y = paste0("Mean:",numeric_colname), fill = "")
  # print(p2)
}
#############
####################
# texfilename<-'tex_tables.tex'
# numeric_colname='CSF.RATIO'
# imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
# #temporal_trends_mean_plot_linear_regression(all_combined_data_frames,numeric_colname,imagefilename,texfilename)
#############
all_combined_data_frames$TOTAL.CSF.VOLUME[all_combined_data_frames$TOTAL.CSF.VOLUME==0]<-NA
numeric_colname='CSF.RATIO'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
# temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename)
temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename)
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png')
temporal_trends_median_CSF.RATIO<-temporal_trends_median_plot(all_combined_data_frames,numeric_colname,imagefilename)
#temporal_trends_mean_plot_linear_regression(all_combined_data_frames,numeric_colname,imagefilename,texfilename)
numeric_colname='SAH.VOLUME'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
# temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename)
temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename) 
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
temporal_trends_median_SAH.VOLUME<-temporal_trends_median_plot(all_combined_data_frames,numeric_colname,imagefilename)
#temporal_trends_mean_plot_linear_regression(all_combined_data_frames,numeric_colname,imagefilename,texfilename)

imagefilename<-paste0('time_series_box_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
descriptive_analysis(all_combined_data_frames,numeric_colname,imagefilename)
numeric_colname='NWU'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename) 
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
temporal_trends_median_NWU<-temporal_trends_median_plot(all_combined_data_frames,numeric_colname,imagefilename) 
#temporal_trends_mean_plot_linear_regression(all_combined_data_frames,numeric_colname,imagefilename,texfilename)
imagefilename<-paste0('time_series_box_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
descriptive_analysis(all_combined_data_frames,numeric_colname,imagefilename)
numeric_colname='TOTAL.CSF.VOLUME'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename) 
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
temporal_trends_median_TOTAL.CSF.VOLUME<-temporal_trends_median_plot(all_combined_data_frames,numeric_colname,imagefilename) 
#temporal_trends_mean_plot_linear_regression(all_combined_data_frames,numeric_colname,imagefilename,texfilename)
imagefilename<-paste0('time_series_box_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
descriptive_analysis(all_combined_data_frames,numeric_colname,imagefilename)
numeric_colname='ICH.VOLUME'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename) 
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
temporal_trends_median_ICH.VOLUME<-temporal_trends_median_plot(all_combined_data_frames,numeric_colname,imagefilename) 
#temporal_trends_mean_plot_linear_regression(all_combined_data_frames,numeric_colname,imagefilename,texfilename)
imagefilename<-paste0('time_series_box_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
descriptive_analysis(all_combined_data_frames,numeric_colname,imagefilename)
numeric_colname='ICH.EDEMA.VOLUME'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename) 
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
temporal_trends_median_ICH.EDEMA.VOLUME<-temporal_trends_median_plot(all_combined_data_frames,numeric_colname,imagefilename) 
#temporal_trends_mean_plot_linear_regression(all_combined_data_frames,numeric_colname,imagefilename,texfilename)
imagefilename<-paste0('time_series_box_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
descriptive_analysis(all_combined_data_frames,numeric_colname,imagefilename)
numeric_colname='INFARCT.VOLUME'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename) 
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
temporal_trends_median_INFARCT.VOLUME<-temporal_trends_median_plot(all_combined_data_frames,numeric_colname,imagefilename) 
#temporal_trends_mean_plot_linear_regression(all_combined_data_frames,numeric_colname,imagefilename,texfilename)
imagefilename<-paste0('time_series_box_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
descriptive_analysis(all_combined_data_frames,numeric_colname,imagefilename)
numeric_colname='delta_csf'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename) 
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
temporal_trends_median_deltacsf<-temporal_trends_median_plot(all_combined_data_frames,numeric_colname,imagefilename) 
#temporal_trends_mean_plot_linear_regression(all_combined_data_frames,numeric_colname,imagefilename,texfilename)
imagefilename<-paste0('time_series_box_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
descriptive_analysis(all_combined_data_frames,numeric_colname,imagefilename)
##################
mean_infarct <- temporal_trends_median_INFARCT.VOLUME$mean_value
mean_csfratio <- temporal_trends_median_CSF.RATIO$mean_value[temporal_trends_median_CSF.RATIO$project_type=='INFARCT'] # in feet
mean_nwu<- temporal_trends_median_NWU$mean_value
# Create data frame using these vectors
df <- data.frame(median_infarct = mean_infarct,median_nwu=mean_nwu, median_csfratio = mean_csfratio)
numerical_cols_corr(df,'median_infarct','median_nwu',imagefilename='mean_infarct_mean_nwu.png',data_frame_name='')
numerical_cols_corr(df,'median_infarct','median_csfratio',imagefilename='mean_infarct_mean_csfratio.png',data_frame_name='')
write.csv(df,'medians_of_different_biomarkers.csv',row.names = FALSE)
write.csv(temporal_trends_median_INFARCT.VOLUME,'temporal_trends_median_INFARCT_VOLUME.csv',row.names = FALSE)
write.csv(temporal_trends_median_CSF.RATIO,'temporal_trends_median_CSF_RATIO.csv',row.names = FALSE)
write.csv(temporal_trends_median_ICH.VOLUME,'temporal_trends_median_ICH.VOLUME.csv',row.names = FALSE)
write.csv(temporal_trends_median_TOTAL.CSF.VOLUME,'temporal_trends_median_TOTAL_CSF_VOLUME.csv',row.names = FALSE)
write.csv(temporal_trends_median_NWU,'temporal_trends_median_NWU.csv',row.names = FALSE)

write.csv(temporal_trends_median_SAH.VOLUME,'temporal_trends_median_SAH_VOLUME.csv',row.names = FALSE)
# #################
# numeric_colname='ICH.VOLUME.RATIO'
# imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
# # temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename)
# temporal_trends_mean_plot(all_combined_data_frames,numeric_colname,imagefilename)
# imagefilename<-paste0('time_series_box_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'.png')
# descriptive_analysis(all_combined_data_frames,numeric_colname,imagefilename)
# ###########