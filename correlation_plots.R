# install.packages("ggplot2")
# install.packages("ggpubr")  # for stat_cor
library(ggplot2)
library(ggpubr)  # for displaying correlation coefficientC
# Create a sample data frame

corr_plot<-function(df,col1,col2,var1,var2,imagefiletosave='test.png') {
# Plot with correlation line and confidence band
plot <- ggplot(df, aes_string(x = col1, y = col2)) +
  theme_classic()+
  geom_point() +  # Add points for scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +  # Add regression line and confidence band
  stat_cor(label.x = 0.5*max(df[[col1]]), label.y = max(df[[col2]]) ) + # Show correlation coefficient

# ggtitle(paste0("Correlation between  ",var1, " and ", var2)) +  # Add plot title
  labs(
    # subtitle = "Linear regression with confidence band",  # Add subtitle
    x = var1,  # X-axis label
    y = var2  # Y-axis label
  )
print(plot)
ggsave(gsub("\\s+", "", imagefiletosave),dpi=300)
}

#############################################################################
df<-read.csv('csf_ratio_median.csv')
corr_plot(df,'nihss_24h','median_value','nihss_24','CSF Ratio','csfratiovsnihss24.png')
df<-read.csv('csf_total_median.csv')
corr_plot(df,'nihss_24h','median_value','nihss_24','Total CSF','csf_totalvsnihss24.png')
df<-read.csv('nwu_median.csv')
corr_plot(df,'nihss_24h','median_value','nihss_24','NWU','nwuvsnihss24.png')
df<-read.csv('infarct_volume_median.csv')
corr_plot(df,'nihss_24h','median_value','nihss_24','Ischemic Volume','infarctvsnihss24.png')
#############################################################################

# 
# 
# .csv
# .csv
df<-read.csv('temporal_trends_median_NWU.csv')
df<-df[df$project_type=='INFARCT',]
corr_plot(df,'timeinterval','mean_value','time (hrs)','NWU','timevsNWU.png')
df<-read.csv('temporal_trends_median_TOTAL_CSF_VOLUME.csv')
df<-df[df$project_type=='INFARCT',]
corr_plot(df,'timeinterval','mean_value', 'time (hrs)' ,'CSF TOTAL','timevsCSFTOTAL.png')
df<-read.csv('temporal_trends_median_ICH.VOLUME.csv')
# df<-df[df$project_type=='INFARCT',]
corr_plot(df,'timeinterval','mean_value', 'time (hrs)' ,'ICH','timevsICH.png')
df<-read.csv('temporal_trends_median_CSF_RATIO.csv')
df<-df[df$project_type=='INFARCT',]
corr_plot(df,'timeinterval','mean_value', 'time (hrs)' ,'CSF Ratio','timevsCSFRatio.png')
df<-read.csv('temporal_trends_median_INFARCT_VOLUME.csv')
corr_plot(df,'timeinterval','mean_value', 'time (hrs)' ,'Ischemic Volume','timevsIschemicVolume.png')
df<-read.csv('temporal_trends_median_SAH_VOLUME.csv')
corr_plot(df,'timeinterval','mean_value', 'time (hrs)' ,'SAH Volume','timevsSAHVolume.png')
# 
df<-read.csv('medians_of_different_biomarkers.csv')
corr_plot(df,'median_infarct','median_nwu','Ischemic volume','NWU','infarctvsnwu.png')
corr_plot(df,'median_infarct','median_csfratio','Ischemic volume','CSF Ratio','infarctvscsfratio.png')