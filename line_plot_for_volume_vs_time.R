library(tidyverse)
library(ggplot2)
library(ggpubr)
library(xtable)
source('utilities.R')
all_combined_data_frames<-read.csv('../RFILES/all_combined_data_frames_1.csv')
## for each subject:
## get the ICH.EDEMA.VOLUME and datetime_synth.diff
# plot correlation between them
ggp<-ggplot()

for (each_sub in all_combined_data_frames$subject_id1) {
  this_df<-all_combined_data_frames %>% filter(project=='ICH' & subject_id1==each_sub)
  this_df_selected<-this_df %>% select(ICH.EDEMA.VOLUME,datetime_synth.diff)
  this_df_selected<-na.omit(this_df_selected)
  # print(this_df)
  if (dim(this_df_selected)[1] > 1 ) {
    print(this_df_selected)
    ggp+geom_point(data=this_df_selected, mapping=aes_string(x=datetime_synth.diff,y=ICH.EDEMA.VOLUME),color="#1A334D")
    # corr_coeff<-cor.test(temp_df[,1],temp_df[,2])
    # print(corr_coeff)
    # data_count<-dim(temp_df)[1]
    # # corr_coeff<-sprintf("%.2f", corr_coeff)
    # # print(paste0('corr_coeff::',corr_coeff))
    # p <- ggplot(temp_df, aes_string(x=col1_name,y=col2_name)) +
    #   theme_ben()+
    #   ggtitle(paste0(paste0(data_frame_name), paste0(', N:',data_count),sep="\n",paste0(', p-value:', sprintf("%.3f", corr_coeff$p.value)),paste0('corr:',sprintf("%.3f", corr_coeff$estimate))) )  + 
    #   geom_point(color="red")
    # #   # annotation_custom(grid::textGrob(lb),  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    # # # print(length(temp_df$nwu)) 
    # p<- p+ geom_smooth(method = "lm", formula=y~x,level = 0.8) # +
    # # annotate(
    # #   "label", x=max(temp_df[,1])*0.8, y=max(temp_df[,2])*0.8, label=paste( paste0('N:',data_count),sep="\n",paste0('p-value:', sprintf("%.3f", corr_coeff$p.value)),paste0('corr:',sprintf("%.3f", corr_coeff$estimate))),
    # #   color="red", size=5)
    print(ggp)
    break
    
  }

  
}