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

source('utilities.R')


source('D:/RSTUDIOJan30/RFILES/utilities.R')
####################
temporal_trends_median_plot_stroke_snipr<-function(all_combined_data_frames,numeric_colname,texfilename='test.tex',filenameforthis='',legend_title='',xlabel='',ylabel=''){
  print("I AM HERE")
  xlabel_1<-''
  ylabel_1<-''
  legend_title_1<-''

  if (str_length(xlabel)>1){xlabel_1<-xlabel}
  else{  xlabel_1<-'Time (hours)'}
  if (str_length(ylabel)>1){ylabel_1<-ylabel}
  else {  ylabel_1<-numeric_colname}
  if (str_length(ylabel)>1){legend_title_1<-legend_title}
  else {  legend_title_1<-'Cohorts'}
  all_combined_data_frames_subset<-all_combined_data_frames %>% select(hoursnumber,record_id,nihss_24h,redcap_repeat_instance,timeinterval,as.name(numeric_colname))
  all_combined_data_frames_subset<-all_combined_data_frames_subset %>% drop_na()
  all_combined_data_frames_subset <- all_combined_data_frames_subset %>%
    mutate(timeinterval = factor(timeinterval))
  expression<-paste0('all_combined_data_frames_subset$this_numeric<-all_combined_data_frames_subset$',numeric_colname)
  print(expression)
  eval(parse(text=expression))
  # project_type_name<-unique(all_combined_data_frames$project_type)
  title_this_plot<-''
  # for (each_project_id in 1:length(project_type_name)){
  # each_project_id=1
  # this_expression=paste0('this_df_',each_project_id,'<-','all_combined_data_frames_subset %>% filter(project_type==project_type_name[',each_project_id,'])')
  # eval(parse(text=this_expression))
  # print(this_expression)
  this_expression<-paste0('N<-nrow(','all_combined_data_frames_subset',')')
  eval(parse(text=this_expression))
  if (N>6){
    # this_expression<-paste0('model<-lm(this_numeric~timeinterval,data=all_combined_data_frames_subset',')')
    # # this_expression<-paste0('correl<-kruskal.test(this_numeric~timeinterval,data=this_df_',each_project_id,')')
    # eval(parse(text=this_expression))
    # model_summary <- summary(model)
    # #latex_table <- xtable(model_summary)
    # # Save summary to a text file
    # # Extract R-squared and other statistics
    # r_squared <- summary(model)$r.squared
    # adj_r_squared <- summary(model)$adj.r.squared
    # f_statistic <- summary(model)$fstatistic[1]
    # cat(paste0(numeric_colname,'::'), file = texfilename, append = TRUE, sep = "\n")
    # 
    # # Write a custom summary to a text file
    # latex_output <-capture.output(cat("Summary of Linear Model Fit:\n",
    #                                   "R-squared: ", r_squared, "\n",
    #                                   "Adjusted R-squared: ", adj_r_squared, "\n",
    #                                   "F-statistic: ", f_statistic, "\n"))
    # # numeric_colname
    # 
    # latex_output <-capture.output(model_summary)
    # cat(latex_output, file = texfilename, append = TRUE, sep = "\n")
    # latex_output <- capture.output(print(xtable(model_summary), type = "latex", include.rownames = FALSE))
    # 
    # # Append the captured output to an existing file
    # cat(latex_output, file = texfilename, append = TRUE, sep = "\n")
    # 

  data<-all_combined_data_frames_subset %>%
    group_by(nihss_24h) %>%
    summarise(median_value = median(this_numeric), #)  #, # mean(this_numeric),
              Lower = quantile(this_numeric, 0.25),
              Upper = quantile(this_numeric, 0.75),
              sd_error = mad(this_numeric)) #std.error(this_numeric)) # %>%
  # latex_output <-capture.output( data )
  # cat(latex_output, file = texfilename, append = TRUE, sep = "\n")
  # plot<-ggplot(data=data,aes(x = timeinterval, y = mean_value)) +
  #   # geom_point() +
  #   geom_line(show.legend = FALSE) +
  #   geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) # +
  #  # geom_smooth(method = "lm", se = TRUE,  aes(group = hoursnumber))
  # print(plot)
  return(data)
  }
}


nihss_analysis<-function(this_time_df,data_frame_name=""){
  non_imaging_continuous_vars<-c('nihss_bl','nihss_24h') #,'age','mls',
  # non_imaging_categorical_vars<-c('aspects_bl','ced_grade','race','gender','ethnicity',"lvo" ,                     "lvo_location___1"  ,       "lvo_location___2" ,       
  #                                 "lvo_location___3"  ,       "lvo_location___4"     ,    "lvo_location___5"    ,    
  #                                 "lvo_location___6"   ,      "lvo_location___7"      ,   "lvo_location___8"     ,   
  #                                 "lvo_location___9"   ,      "lvo_location___10")
  imaging_continous_vars<-c('csf_total','csf_ratio','nwu','infarct_volume','core_volume')
  for (each_imaging_var in imaging_continous_vars){
    for (each_non_imaging_cont_var in non_imaging_continuous_vars){
      print(paste0(each_non_imaging_cont_var,'::',each_imaging_var))
      col1_name1<-each_non_imaging_cont_var
      col2_name2<-each_imaging_var
      imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
      numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)

      # break
    }
    # for (each_non_imaging_cat_var in non_imaging_categorical_vars ){
    #   
    #   print(paste0(each_non_imaging_cont_var,'::',each_imaging_var))
    #   col1_name1<-each_imaging_var
    #   col2_name2<-each_non_imaging_cat_var
    #   imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
    #   # temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
    #   # if ( grepl( 'nihss',each_non_imaging_cat_var))
    #   # {
    #   #   imagefilename<-paste0('correlation_mean_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
    #   #   temp_df<-cat_numeric_corr_mean_plot(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
    #   # }
    #   # # break
    #   
    # }
    # # break
    
  }
}


for_each_sequence_of_ct_v1<-function(this_time_df,data_frame_name=""){
  non_imaging_continuous_vars<-c('age','mls') ##'nihss_bl','nihss_24h',
  non_imaging_categorical_vars<-c('aspects_bl','ced_grade','race','gender','ethnicity',"lvo" ,                     "lvo_location___1"  ,       "lvo_location___2" ,       
                                  "lvo_location___3"  ,       "lvo_location___4"     ,    "lvo_location___5"    ,    
                                  "lvo_location___6"   ,      "lvo_location___7"      ,   "lvo_location___8"     ,   
                                  "lvo_location___9"   ,      "lvo_location___10")
  imaging_continous_vars<-c('csf_total','csf_ratio','nwu','infarct_volume','core_volume')
  for (each_imaging_var in imaging_continous_vars){
    for (each_non_imaging_cont_var in non_imaging_continuous_vars){
      print(paste0(each_non_imaging_cont_var,'::',each_imaging_var))
      col1_name1<-each_non_imaging_cont_var
      col2_name2<-each_imaging_var
      imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
      numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)

      # break
      }
    for (each_non_imaging_cat_var in non_imaging_categorical_vars ){

      print(paste0(each_non_imaging_cont_var,'::',each_imaging_var))
      col1_name1<-each_imaging_var
      col2_name2<-each_non_imaging_cat_var
      imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
      temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
      if ( grepl( 'nihss',each_non_imaging_cat_var))
      {
      imagefilename<-paste0('correlation_mean_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
      temp_df<-cat_numeric_corr_mean_plot(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
      }
       # break

    }
    # break
    
  }
}


for_each_sequence_of_ct<-function(this_time_df,data_frame_name=""){
  col1_name1<-'mls'
  col2_name2<-'csf_ratio'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  ############################################
  
  col1_name1<-'age'
  col2_name2<-'csf_ratio'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  ##########################
  col1_name1<-'csf_ratio'
  col2_name2<-'gender'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  
  col1_name1<-'csf_ratio'
  col2_name2<-'aspects_bl'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  
  col1_name1<-'csf_ratio'
  col2_name2<-'ced_grade'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  
  
  col1_name1<-'nwu'
  col2_name2<-'gender'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'nwu'
  col2_name2<-'ced_grade'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  
  col1_name1<-'csf_total'
  col2_name2<-'gender'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'csf_total'
  col2_name2<-'ced_grade'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  
  
  col1_name1<-'infarct_volume'
  col2_name2<-'gender'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  col1_name1<-'infarct_volume'
  col2_name2<-'ced_grade'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  
  
  
  
  col1_name1<-'nwu'
  col2_name2<-'aspects_bl'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  
  col1_name1<-'csf_total'
  col2_name2<-'aspects_bl'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
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
  imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  # numerical_cols_corr(this_time_df,'age','nwu')
 
  # this_time_df_1<-this_time_df %>% filter(onset_to_scan_hours<12)
  col1_name1<-'age'
  col2_name2<-'csf_total'
  data_frame_name_age<-data_frame_name #%>% filter(csf_total<200)
  imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name_age)
  
  col1_name1<-'age'
  col2_name2<-'infarct_volume'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  
  ############################
  
  # numerical_cols_corr(this_time_df,'age','nwu')
  col1_name1<-'mls'
  col2_name2<-'nwu'
  imagefilename<-paste0('correlation_clinical_',data_frame_name,str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  
  
  # col1_name1<-'nwu'
  # col2_name2<-'nihss_bl'
  # imagefilename<-paste0('correlation_clinical_box',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  # numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  # #temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  # col1_name1<-'nwu'
  # col2_name2<-'nihss_24h'
  # imagefilename<-paste0('correlation_clinical_box',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  # numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  # #temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  # 
  # col1_name1<-'csf_ratio'
  # col2_name2<-'nihss_bl'
  # imagefilename<-paste0('correlation_clinical_box',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  # numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  # #temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  # col1_name1<-'csf_ratio'
  # col2_name2<-'nihss_24h'
  # imagefilename<-paste0('correlation_clinical_box',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  # numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  # #temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  # col1_name1<-'csf_total'
  # col2_name2<-'nihss_bl'
  # imagefilename<-paste0('correlation_clinical_box',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  # numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  # #temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  # col1_name1<-'csf_total'
  # col2_name2<-'nihss_24h'
  # imagefilename<-paste0('correlation_clinical_box',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  # numerical_cols_corr(this_time_df,col1_name1,col2_name2,imagefilename,data_frame_name = data_frame_name)
  # #temp_df<-cat_numeric_corr_with_anova(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  # imagefilename<-paste0('correlation_clinical_mean',data_frame_name,'_',str_replace_all(col1_name1, "[[:punct:]]", ""),str_replace_all(col2_name2, "[[:punct:]]", ""),'.png')
  # cat_numeric_corr_mean_plot(this_time_df,col1_name1,col2_name2,data_frame_name=data_frame_name,  imagefilename=imagefilename)
  
  ##################territory###########################
  
  
  
}
############################################
# ########################## DATA PREPARATION ###########################################
# directory_of_files='D:/RSTUDIOJan30/STROKE_DATA'
# directory_of_outputfiles='D:/RSTUDIOJan30/STROKE_DATA/OUTPUT'
# #print(directory_of_files)
# filenames <- list.files(directory_of_files, pattern="^StrokeImagingAnalysi", full.names=TRUE)
# #df1<-c()
# #df2<-c()
# if ( grepl( 'Project',filenames[1])) {
#   df1<-read.csv(filenames[1])
#   df2<-read.csv(filenames[2])
# 
#   df2<-subset (df2, select = -c(redcap_repeat_instance,redcap_repeat_instrument))
# 
# } else {
#   df2<-read.csv(filenames[1])
#   df1<-read.csv(filenames[2])
#   df2<-subset (df2, select = -c(redcap_repeat_instance,redcap_repeat_instrument))
# 
# }
# 
# 
# merged_df<-merge(df1,df2,by.x='record_id',by.y='record_id')
# merged_df$ced_grade[merged_df$ced_grade >5] <- NA
# repeat_instance_data_frames<-c()
# ## separate different repeat_instance into different files ############################
# for (x in 1:max(merged_df['redcap_repeat_instance'],na.rm=TRUE)) {
#   variable_name<-paste0('repeat_instance_',x)
#   this_df<-subset(merged_df, redcap_repeat_instance == x)
#   assign(variable_name,this_df)
#   repeat_instance_data_frames<-this_df
#   # if (x<6){
#   # for_each_sequence_of_ct(this_df,data_frame_name=variable_name)
#   # }
#     # print(eval(parse(text = variable_name)))
#   # write.csv(this_df, file.path(directory_of_outputfiles,paste0(variable_name,'.csv')), row.names=FALSE)
#   
# }
# # different_time_data<-c()
# ################TIME INTERVAL########################
# merged_df$mls<-as.numeric(merged_df$mls)
# merged_df$timeinterval<-NA
# # merged_df$timeinterval[merged_df$onset_to_scan_hours==0]<-0
# merged_df$timeinterval[ merged_df$onset_to_scan_hours<=12]<-12
# merged_df$timeinterval[merged_df$onset_to_scan_hours>12 & merged_df$onset_to_scan_hours<=24]<-24
# merged_df$timeinterval[merged_df$onset_to_scan_hours>24 & merged_df$onset_to_scan_hours<=36]<-36
# merged_df$timeinterval[merged_df$onset_to_scan_hours>36 & merged_df$onset_to_scan_hours<=48]<-48
# merged_df$timeinterval[merged_df$onset_to_scan_hours>48 & merged_df$onset_to_scan_hours<=60]<-60
# merged_df$timeinterval[merged_df$onset_to_scan_hours>60 & merged_df$onset_to_scan_hours<=72]<-72
# merged_df$timeinterval[merged_df$onset_to_scan_hours>72 & merged_df$onset_to_scan_hours<=96]<-96
# merged_df$timeinterval[merged_df$onset_to_scan_hours>96]<-120
# merged_df$timeinterval_asfactor<-factor(merged_df$timeinterval,levels = c(12,24,36,48,60,72,96,120))
# # merged_df<-merged_df %>% filter(onset_to_scan_hours < 97)
# ###################################### correlation plots #####################################
# merged_df_subset_12hrs<-merged_df%>%filter(timeinterval==12)
# merged_df_subset_24hrs<-merged_df%>%filter(timeinterval==24)
# merged_df_subset_36hrs<-merged_df%>%filter(timeinterval==36)
# merged_df_subset_48hrs<-merged_df%>%filter(timeinterval==48)
# merged_df_subset_60hrs<-merged_df%>%filter(timeinterval==60)
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
# 
# 
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
# nihss_analysis(merged_df_subset_36hrs,data_frame_name='hrs_36')
