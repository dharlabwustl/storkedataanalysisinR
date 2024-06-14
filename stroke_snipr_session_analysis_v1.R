rm()
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(xtable)
source('utilities.R')
source('D:/RSTUDIOJan30/RFILES/utilities.R')
####################

for_each_sequence_of_ct<-function(this_time_df,data_frame_name=""){
  col1_name1<-'csf_ratio'
  col2_name2<-'gender'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'nwu'
  col2_name2<-'gender'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'nwu'
  col2_name2<-'ced_grade'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'nwu'
  col2_name2<-'nihss_bl'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'csf_ratio'
  col2_name2<-'nihss_bl'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'csf_ratio'
  col2_name2<-'ced_grade'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'nwu'
  col2_name2<-'aspects_bl'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'csf_ratio'
  col2_name2<-'aspects_bl'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  
  
  col1_name1<-'nwu'
  col2_name2<-'aspects_bl'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'csf_ratio'
  col2_name2<-'aspects_bl'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  
  # temp_df<-cat_numeric_corr_with_anova(this_time_df,'nwu','gender',data_frame_name=data_frame_name)
  # temp_df<-cat_numeric_corr_with_anova(this_time_df,'nwu','ced_grade',data_frame_name=data_frame_name)
  # temp_df<-cat_numeric_corr_with_anova(this_time_df,'nwu','nihss_bl',data_frame_name=data_frame_name)
  # temp_df<-cat_numeric_corr_with_anova(this_time_df,'csf_ratio','ced_grade',data_frame_name=data_frame_name)
  # temp_df<-cat_numeric_corr_with_anova(this_time_df,'csf_ratio','nihss_bl',data_frame_name=data_frame_name)
  # temp_df<-cat_numeric_corr_with_anova(this_time_df,'nwu','aspects_bl',data_frame_name=data_frame_name)
  # temp_df<-cat_numeric_corr_with_anova(this_time_df,'csf_ratio','aspects_bl',data_frame_name=data_frame_name)
  # ############################################
  col1_name1<-'age'
  col2_name2<-'nwu'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  # numerical_cols_corr(this_time_df,'age','nwu')
  col1_name1<-'age'
  col2_name2<-'csf_ratio'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  # numerical_cols_corr(this_time_df,'age','nwu')
  col1_name1<-'mls'
  col2_name2<-'nwu'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  col1_name1<-'mls'
  col2_name2<-'csf_ratio'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.pdf')
  numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
}
############################################
########################## DATA PREPARATION ###########################################
directory_of_files='D:/RSTUDIOJan30/STROKE_DATA'
directory_of_outputfiles='D:/RSTUDIOJan30/STROKE_DATA/OUTPUT'
#print(directory_of_files)
filenames <- list.files(directory_of_files, pattern="^StrokeImagingAnalysi", full.names=TRUE)
#df1<-c()
#df2<-c()
if ( grepl( 'Project',filenames[1])) {
  df1<-read.csv(filenames[1])
  df2<-read.csv(filenames[2])

  df2<-subset (df2, select = -c(redcap_repeat_instance,redcap_repeat_instrument))

} else {
  df2<-read.csv(filenames[1])
  df1<-read.csv(filenames[2])
  df2<-subset (df2, select = -c(redcap_repeat_instance,redcap_repeat_instrument))

}

merged_df<-merge(df1,df2,by.x='record_id',by.y='record_id')
repeat_instance_data_frames<-c()
## separate different repeat_instance into different files ############################
for (x in 1:max(merged_df['redcap_repeat_instance'],na.rm=TRUE)) {
  variable_name<-paste0('repeat_instance_',x)
  this_df<-subset(merged_df, redcap_repeat_instance == x)
  assign(variable_name,this_df)
  repeat_instance_data_frames<-this_df
  # if (x<6){
  # for_each_sequence_of_ct(this_df,data_frame_name=variable_name)
  # }
    # print(eval(parse(text = variable_name)))
  # write.csv(this_df, file.path(directory_of_outputfiles,paste0(variable_name,'.csv')), row.names=FALSE)
  
}
# different_time_data<-c()
################TIME INTERVAL########################
merged_df$mls<-as.numeric(merged_df$mls)
merged_df$timeinterval<-NA
merged_df$timeinterval[merged_df$onset_to_scan_hours==0]<-0
merged_df$timeinterval[merged_df$onset_to_scan_hours>0 & merged_df$onset_to_scan_hours<=12]<-12
merged_df$timeinterval[merged_df$onset_to_scan_hours>12 & merged_df$onset_to_scan_hours<=24]<-24
merged_df$timeinterval[merged_df$onset_to_scan_hours>24 & merged_df$onset_to_scan_hours<=36]<-36
merged_df$timeinterval[merged_df$onset_to_scan_hours>36 & merged_df$onset_to_scan_hours<=48]<-48
merged_df$timeinterval[merged_df$onset_to_scan_hours>48 & merged_df$onset_to_scan_hours<=60]<-60
merged_df$timeinterval[merged_df$onset_to_scan_hours>60 & merged_df$onset_to_scan_hours<=72]<-72
merged_df$timeinterval[merged_df$onset_to_scan_hours>72 & merged_df$onset_to_scan_hours<=96]<-96
merged_df$timeinterval[merged_df$onset_to_scan_hours>96]<-120
merged_df$timeinterval_asfactor<-factor(merged_df$timeinterval,levels = c(0,12,24,36,48,60,72,96,120))
# merged_df<-merged_df %>% filter(onset_to_scan_hours < 97)
###################################### correlation plots #####################################
merged_df_subset_12hrs<-merged_df%>%filter(timeinterval==12)
merged_df_subset_24hrs<-merged_df%>%filter(timeinterval==24)
merged_df_subset_36hrs<-merged_df%>%filter(timeinterval==36)
merged_df_subset_48hrs<-merged_df%>%filter(timeinterval==48)
merged_df_subset_60hrs<-merged_df%>%filter(timeinterval==60)
# for_each_sequence_of_ct(merged_df_subset_12hrs,data_frame_name='merged_df_subset_12hrs')
for_each_sequence_of_ct(merged_df_subset_12hrs,data_frame_name='hrs_12')
for_each_sequence_of_ct(merged_df_subset_24hrs,data_frame_name='hrs_24')
for_each_sequence_of_ct(merged_df_subset_36hrs,data_frame_name='hrs_36')
for_each_sequence_of_ct(merged_df_subset_48hrs,data_frame_name='hrs_48')
for_each_sequence_of_ct(merged_df_subset_60hrs,data_frame_name='hrs_60')

##########
colname1<-'age'
print(numerical_histogram(merged_df,colname1,paste0('clinical_histogram',str_replace_all(colname1, "[[:punct:]]", ""),'.pdf')))
# for_each_sequence_of_ct(merged_df,data_frame_name="merged_df_lt_24hrs")
# #### scan 1

# merged_df_lt_24hrs<-subset(merged_df,onset_to_scan_hours<=6)
# merged_df_gt_24hrs<-subset(merged_df,onset_to_scan_hours>6)
# for_each_sequence_of_ct(merged_df_lt_24hrs,data_frame_name="merged_df_lt_24hrs")
# for_each_sequence_of_ct(merged_df_gt_24hrs,data_frame_name="merged_df_gt_24hrs")
# 
# temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'csf_total','gender')
# temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'nwu','gender')
# temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'nwu','ced_grade')
# temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'nwu','nihss_bl')
# temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'csf_ratio','ced_grade')
# temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'csf_ratio','nihss_bl')
# temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'nwu','aspects_bl')
# temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'csf_ratio','aspects_bl')
# 
# library('ggplot2')
# 
# 
# numerical_cols_corr(repeat_instance_1,'age','nwu')
# #######
# numerical_cols_corr(repeat_instance_1,'age','csf_ratio')
# ##########
# numerical_cols_corr(repeat_instance_1,'age','nwu')
# ####
# numerical_cols_corr(repeat_instance_3,'mls','nwu')
# #######
# numerical_cols_corr(repeat_instance_3,'mls','csf_ratio')
# 
