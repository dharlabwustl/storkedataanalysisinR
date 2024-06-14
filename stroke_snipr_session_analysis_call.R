# rm()
# library(plotrix)
# library(MASS)
# library(tidyverse)
# library(ggplot2)
# library(ggpubr)
# library(xtable)
# ################
# # library(plotrix)
# # library(neatStats)
# # library(rstatix)
# # library(stringr)
# library(plyr)
# library(dplyr)
# # library(grid)
# # library(plyr)
# # library(tidyr)
# library(lubridate)
# # library(ggExtra)
# # library(lme4)
# library(tidyverse)
# ##########

source('utilities.R')

# 
# source('D:/RSTUDIOJan30/RFILES/utilities.R')
source('stroke_snipr_session_analysis_utilities.R')

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
merged_df$ced_grade[merged_df$ced_grade >5] <- NA
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
merged_df<-merged_df[merged_df$csf_total>0,]
merged_df<-merged_df[merged_df$csf_total<500,]
merged_df<-merged_df[merged_df$infarct_volume<400,]
# merged_df$timeinterval[merged_df$onset_to_scan_hours==0]<-0
merged_df$timeinterval[ merged_df$onset_to_scan_hours<=12]<-12
merged_df$timeinterval[merged_df$onset_to_scan_hours>12 & merged_df$onset_to_scan_hours<=24]<-24
merged_df$timeinterval[merged_df$onset_to_scan_hours>24 & merged_df$onset_to_scan_hours<=36]<-36
merged_df$timeinterval[merged_df$onset_to_scan_hours>36 & merged_df$onset_to_scan_hours<=48]<-48
merged_df$timeinterval[merged_df$onset_to_scan_hours>48 & merged_df$onset_to_scan_hours<=60]<-60
merged_df$timeinterval[merged_df$onset_to_scan_hours>60 & merged_df$onset_to_scan_hours<=72]<-72
merged_df$timeinterval[merged_df$onset_to_scan_hours>72 & merged_df$onset_to_scan_hours<=96]<-96
merged_df$timeinterval[merged_df$onset_to_scan_hours>96]<-120
merged_df$timeinterval_asfactor<-factor(merged_df$timeinterval,levels = c(12,24,36,48,60,72,96,120))
# merged_df<-merged_df %>% filter(onset_to_scan_hours < 97)
###################################### correlation plots #####################################
merged_df_subset_12hrs<-merged_df%>%filter(timeinterval==12)
merged_df_subset_24hrs<-merged_df%>%filter(timeinterval==24)
merged_df_subset_36hrs<-merged_df%>%filter(timeinterval==36)
merged_df_subset_48hrs<-merged_df%>%filter(timeinterval==48)
merged_df_subset_60hrs<-merged_df%>%filter(timeinterval==60)
# # # for_each_sequence_of_ct(merged_df_subset_12hrs,data_frame_name='merged_df_subset_12hrs')
# # for_each_sequence_of_ct(merged_df_subset_12hrs,data_frame_name='hrs_12')
# # for_each_sequence_of_ct(merged_df_subset_24hrs,data_frame_name='hrs_24')
# # for_each_sequence_of_ct(merged_df_subset_36hrs,data_frame_name='hrs_36')
# # for_each_sequence_of_ct(merged_df_subset_48hrs,data_frame_name='hrs_48')
# # for_each_sequence_of_ct(merged_df_subset_60hrs,data_frame_name='hrs_60')
# for_each_sequence_of_ct_v1(merged_df_subset_12hrs,data_frame_name='hrs_12')
# for_each_sequence_of_ct_v1(merged_df_subset_24hrs,data_frame_name='hrs_24')
# for_each_sequence_of_ct_v1(merged_df_subset_36hrs,data_frame_name='hrs_36')
# for_each_sequence_of_ct_v1(merged_df_subset_48hrs,data_frame_name='hrs_48')
# for_each_sequence_of_ct_v1(merged_df_subset_60hrs,data_frame_name='hrs_60')
# nihss_analysis(merged_df_subset_36hrs,data_frame_name='hrs_36')
merged_df_subset_12hrs$hoursnumber<-"hrs_12"
merged_df_subset_24hrs$hoursnumber<-"hrs_24"
merged_df_subset_36hrs$hoursnumber<-"hrs_36"
merged_df_subset_48hrs$hoursnumber<-"hrs_48"
merged_df_subset_60hrs$hoursnumber<-"hrs_60"
all_merged_df<-rbind(merged_df_subset_12hrs,merged_df_subset_24hrs)
all_merged_df<-rbind(all_merged_df,merged_df_subset_36hrs)
all_merged_df<-rbind(all_merged_df,merged_df_subset_48hrs)
all_merged_df<-rbind(all_merged_df,merged_df_subset_60hrs)
numeric_colname='csf_total'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
texfilename='test.tex'
csf_total_median<-merged_df_subset_36hrs_csf_total<-temporal_trends_median_plot_stroke_snipr(all_merged_df,numeric_colname,texfilename,imagefilename) 
write.csv(csf_total_median,'csf_total_median.csv',row.names = TRUE)

numeric_colname='csf_ratio'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
texfilename='test.tex'
csf_ratio_median<-merged_df_subset_36hrs_csf_total<-temporal_trends_median_plot_stroke_snipr(all_merged_df,numeric_colname,texfilename,imagefilename) 
write.csv(csf_ratio_median,'csf_ratio_median.csv',row.names = TRUE)

numeric_colname='nwu'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
texfilename='test.tex'
nwu_median<-merged_df_subset_36hrs_csf_total<-temporal_trends_median_plot_stroke_snipr(all_merged_df,numeric_colname,texfilename,imagefilename) 
write.csv(nwu_median,'nwu_median.csv',row.names = TRUE)

numeric_colname='infarct_volume'
imagefilename<-paste0('time_series_',str_replace_all(numeric_colname, "[[:punct:]]", ""),'median.png') 
texfilename='test.tex'
infarct_volume_median<-merged_df_subset_36hrs_csf_total<-temporal_trends_median_plot_stroke_snipr(all_merged_df,numeric_colname,texfilename,imagefilename) 
write.csv(infarct_volume_median,'infarct_volume_median.csv',row.names = TRUE)
# numerical_cols_corr(infarct_volume_median,'nihss_24h','median_value',imagefilename='test.png',data_frame_name='')


# # if ( grepl( 'csf_total',each_imaging_var)) {
# data_frame_name='hrs_12'
# col1_name1='age'
# col2_name2='csf_total'
# this_time_df_age<-merged_df_subset_12hrs %>% filter(csf_total<200)
# imagefilename<-paste0('correlation_clinical_filtered',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
# numerical_cols_corr(this_time_df_age,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
# #   
# # }
# # merged_df_subset_lt12hrs<-merged_df %>%filter(onset_to_scan_hours<=12)
# # for_each_sequence_of_ct(merged_df_subset_lt12hrs,data_frame_name='hrs_lt12')
# # merged_df_subset_lt24hrs<-merged_df %>%filter(onset_to_scan_hours<24)
# # for_each_sequence_of_ct(merged_df_subset_lt24hrs,data_frame_name='hrs_lt24')
# # merged_df_subset_gt24hrs<-merged_df %>%filter(onset_to_scan_hours>24)
# # for_each_sequence_of_ct(merged_df_subset_gt24hrs,data_frame_name='hrs_gt24')
# ##########
# # colname1<-'age'
# # print(numerical_histogram(merged_df,colname1,paste0('clinical_histogram',str_replace_all(colname1, "[[:punct:]]", ""),'.png')))
# # for_each_sequence_of_ct(merged_df,data_frame_name="merged_df_lt_24hrs")
# # #### scan 1
# 
# # merged_df_lt_24hrs<-subset(merged_df,onset_to_scan_hours<=6)
# # merged_df_gt_24hrs<-subset(merged_df,onset_to_scan_hours>6)
# # for_each_sequence_of_ct(merged_df_lt_24hrs,data_frame_name="merged_df_lt_24hrs")
# # for_each_sequence_of_ct(merged_df_gt_24hrs,data_frame_name="merged_df_gt_24hrs")
# #
# # temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'csf_total','gender')
# # temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'nwu','gender')
# # temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'nwu','ced_grade')
# # temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'nwu','nihss_bl')
# # temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'csf_ratio','ced_grade')
# # temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'csf_ratio','nihss_bl')
# # temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'nwu','aspects_bl')
# # temp_df<-cat_numeric_corr_with_anova(repeat_instance_1,'csf_ratio','aspects_bl')
# #
# # library('ggplot2')
# #
# #
# # numerical_cols_corr(repeat_instance_1,'age','nwu')
# # #######
# # numerical_cols_corr(repeat_instance_1,'age','csf_ratio')
# # ##########
# # numerical_cols_corr(repeat_instance_1,'age','nwu')
# # ####
# # numerical_cols_corr(repeat_instance_3,'mls','nwu')
# # #######
# # numerical_cols_corr(repeat_instance_3,'mls','csf_ratio')
# #
# 
# ## NISSH SCALE analysis
# 
