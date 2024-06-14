library(ggplot2)
# library(ggExtra)
###########
theme_classic <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # L'ensemble de la figure
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # Zone où se situe le graphique
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      # La légende
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}
count_biomarkers<-function(all_combined_data_frames,cohort_names,colname,subspatt=""){
data_for_bar_plot<-c()
for (x in 1:length(cohort_names)) {
  # print(cohort_names[x])
  this_df<-subset(all_combined_data_frames, project==cohort_names[x])
  this_count<-0

  temp_df<-count_a_column(this_df,colname)
  # print(temp_df)
  this_count<-length(temp_df)
  if (this_count == 0) {
    count_a_column_with_char(this_df,colname,subspatt)

  }
  data_for_bar_plot[x]<-this_count
}
return(data_for_bar_plot)
}
####################

# # for_each_sequence_of_ct<-function(this_time_df,data_frame_name=""){
#   
#   temp_df<-cat_numeric_corr_with_anova(this_time_df,'csf_total','gender',data_frame_name=data_frame_name)
#   temp_df<-cat_numeric_corr_with_anova(this_time_df,'nwu','gender',data_frame_name=data_frame_name)
#   temp_df<-cat_numeric_corr_with_anova(this_time_df,'nwu','ced_grade',data_frame_name=data_frame_name)
#   temp_df<-cat_numeric_corr_with_anova(this_time_df,'nwu','nihss_bl',data_frame_name=data_frame_name)
#   temp_df<-cat_numeric_corr_with_anova(this_time_df,'csf_ratio','ced_grade',data_frame_name=data_frame_name)
#   temp_df<-cat_numeric_corr_with_anova(this_time_df,'csf_ratio','nihss_bl',data_frame_name=data_frame_name)
#   temp_df<-cat_numeric_corr_with_anova(this_time_df,'nwu','aspects_bl',data_frame_name=data_frame_name)
#   temp_df<-cat_numeric_corr_with_anova(this_time_df,'csf_ratio','aspects_bl',data_frame_name=data_frame_name)
#   ############################################
#   numerical_cols_corr(this_time_df,'age','nwu')
#   numerical_cols_corr(this_time_df,'age','csf_ratio')
#   numerical_cols_corr(this_time_df,'age','nwu')
#   numerical_cols_corr(this_time_df,'mls','nwu')
#   numerical_cols_corr(this_time_df,'mls','csf_ratio')
# }
############################################
fill_cohort_name_in_a_numeric_column<-function(all_df,colname,cohortname=""){
  tryCatch(
    expr = {
      # all_df$CSF.RATIO[all_df$CSF.RATIO >0 & all_df$project=='COLI']<-'COLI'
      temp_df<-subset(all_df,select=c(colname))
      temp_df<-replace(temp_df,temp_df=='',NA)
      expression<-paste0('all_df$',colname,'<-temp_df$',colname)
      # print(expression)
      eval(parse(text = expression))
      expression<-paste0('all_df$',colname,'[(all_df$',colname, '>0 | all_df$',colname, '<0)  & all_df$project==cohortname]<-cohortname')
      # print(expression)
            eval(parse(text = expression))

      # temp_df <- mutate_all(temp_df, function(x) as.numeric(as.character(x)))
      # # temp_df<-na.omit(temp_df) #subset(df,select=c(categ_colname,numeric_colname)))
      # # expression=paste0('temp_df$',colname,'[temp_df$',colname,'>0]<-', ATUL')
      # temp_df$CSF.RATIO[all_combined_data_frames$CSF.RATIO>0]<-'ATUL'
      # temp_df<-temp_df[temp_df[colname]>0, ]
      return(all_df) 
    }
    ,
    error = function(e){ 
      print("FAILED AT fill_cohort_name_in_a_numeric_column")
      # (Optional)
      # Do this if an error is caught...
    },
    warning = function(w){
      # (Optional)
      # Do this if a warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
    
  )
}

count_a_column<-function(df,colname){
  tryCatch(
    expr = {
  temp_df<-subset(df,select=c(colname))
  temp_df<-replace(temp_df,temp_df=='',NA)
  temp_df<-na.omit(temp_df) #subset(df,select=c(categ_colname,numeric_colname)))
  temp_df<-temp_df[temp_df[colname]>0, ]
  return(temp_df) 
    }
  ,
  error = function(e){ 
    # (Optional)
    # Do this if an error is caught...
  },
  warning = function(w){
    # (Optional)
    # Do this if a warning is caught...
  },
  finally = {
    # (Optional)
    # Do this at the end before quitting the tryCatch structure...
  }
  
  )
}

count_a_column_calculate_value<-function(df,colname){
  tryCatch(
    expr = {
  temp_df<-subset(df,select=c(colname))
  temp_df<-replace(temp_df,temp_df=='',NA)
  temp_df<-na.omit(temp_df) #subset(df,select=c(categ_colname,numeric_colname)))
  temp_df<-temp_df[temp_df[colname]>-99999999999999, ]
  return(temp_df) }
  ,
  error = function(e){ 
    # (Optional)
    # Do this if an error is caught...
  },
  warning = function(w){
    # (Optional)
    # Do this if a warning is caught...
  },
  finally = {
    # (Optional)
    # Do this at the end before quitting the tryCatch structure...
  }
  
  )
}
count_a_column_with_char_v1<-function(df,this_column,colname,subspatt){
  tryCatch(
    expr = {
      # print(" I AM AT ")
      expression<-paste0("temp_df<-all_combined_data_frames %>% filter(str_detect(",colname,",'",subspatt,"'))") 
      # print(expression)
      eval(parse(text = expression))
  # temp_df<-df[str_detect(this_column, subspatt), ]
  temp_df<-subset(df,select=c(colname))
  temp_df<-replace(temp_df,temp_df=='',NA)
  temp_df<-na.omit(temp_df) #subset(df,select=c(categ_colname,numeric_colname)))
  #temp_df<-temp_df[temp_df[colname]>0, ]
  return(temp_df)
    }
  ,
  error = function(e){ 
    # (Optional)
    # Do this if an error is caught...
  },
  warning = function(w){
    # (Optional)
    # Do this if a warning is caught...
  },
  finally = {
    # (Optional)
    # Do this at the end before quitting the tryCatch structure...
  }
  
  )
}
count_a_column_with_char<-function(df,colname,subspatt){
  tryCatch(
    expr = {
  temp_df<-df[str_detect(df[colname], subspatt), ]
  temp_df<-subset(df,select=c(colname))
  temp_df<-replace(temp_df,temp_df=='',NA)
  temp_df<-na.omit(temp_df) #subset(df,select=c(categ_colname,numeric_colname)))
  #temp_df<-temp_df[temp_df[colname]>0, ]
  return(temp_df)
    }
  ,
  error = function(e){
    # (Optional)
    # Do this if an error is caught...
  },
  warning = function(w){
    # (Optional)
    # Do this if a warning is caught...
  },
  finally = {
    # (Optional)
    # Do this at the end before quitting the tryCatch structure...
  }

  )
}
count_a_column_withtry<-function(df,colname){
  tryCatch(
    expr = {
      temp_df<-subset(df,select=c(colname))
      temp_df<-replace(temp_df,temp_df=='',NA)
      temp_df<-na.omit(temp_df) #subset(df,select=c(categ_colname,numeric_colname)))
      temp_df<-temp_df[temp_df[colname]>0, ]
      return(temp_df) }
    ,
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
    },
    warning = function(w){
      # (Optional)
      # Do this if a warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
    
  )
}
count_a_value_withtry<-function(df,colname){
  tryCatch(
    expr = {
      temp_df<-subset(df,select=c(colname))
      temp_df<-replace(temp_df,temp_df=='',NA)
      temp_df<-na.omit(temp_df) #subset(df,select=c(categ_colname,numeric_colname)))
      temp_df<-temp_df[temp_df[colname]>-9999999999999999, ]
      return(temp_df) }
    ,
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
    },
    warning = function(w){
      # (Optional)
      # Do this if a warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
    
  )
}

###################################### correlation plots #####################################
cat_numeric_corr_with_anova<-function(df,numeric_colname,categ_colname,data_frame_name="",imagefilename=""){
  tryCatch(
    expr = {

  expression<-paste0('df$this_numeric<-df$',numeric_colname)
  eval(parse(text=expression))
  expression<-paste0('df$this_categorical<-df$',categ_colname)
  eval(parse(text=expression))
  # print(paste0(categ_colname,'::',numeric_colname ))
  temp_df<-df %>% select(this_categorical,this_numeric) #subset(df,select=c(categ_colname,numeric_colname))
  print(paste0(categ_colname,'::',numeric_colname ))
  print('I AM AT cat_numeric_corr_with_anova')
  temp_df<-replace(temp_df,temp_df=='',NA)
  temp_df<-na.omit(temp_df)
  data_count<-dim(temp_df)[1]
  temp_df$this_categorical<-factor(temp_df$this_categorical)
  num_factors_in_cat=length(unique(temp_df$this_categorical)) #[,1]))
  
  print(paste0("num_factors_in_cat",'::',num_factors_in_cat,'::',categ_colname,'::',imagefilename))
  if (num_factors_in_cat==2) {

    expression=paste0('glm(factor(',categ_colname,')','~',numeric_colname,',data=temp_df, family=binomial)') 
    # print(expression)
    logit <-glm(factor(this_categorical)~ this_numeric,data=df,family=) # eval(parse(text = expression)) 
    wt <- wilcox.test(temp_df[,2][which(temp_df[,1] == "Male")], temp_df[,2][which(temp_df[,1]=="Female")])
    print(summary(logit))
    print(anova(logit))
    print(wt)
  }

  expression=paste0('kruskal.test(',numeric_colname,'~', categ_colname, ',data = temp_df)')
  print(expression)
  # Perform ANOVA
  anova_result <- aov(this_numeric ~ this_categorical, data = temp_df)
  
  # Use anova() function to get a detailed ANOVA table
  anova_table <- anova(anova_result)
  print(colnames(anova_table))
  
  # Calculate Eta Squared
  ss_total <- sum(anova_table$"Sum Sq") # Total sum of squares
  ss_between_groups <- anova_table$"Sum Sq"[1] # Sum of squares for the group effect
  
  eta_squared <- ss_between_groups / ss_total
  p_value<-anova_table$"Pr(>F)"
  # Print Eta Squared
  print(paste0('eta_squared::',eta_squared))
  # Calculate Eta Squared
  # eta_squared <- summary(anova_result)[["Terms"]][["Sum Sq"]][1] / sum(summary(anova_result)[["Terms"]][["Sum Sq"]])
  print(eta_squared)
  
  # correl<-kruskal.test(this_numeric~this_categorical,data=temp_df) #eval(parse(text = expression))
  # print(correl)
  # eta_squared_approx <- correl$statistic / (data_count * (num_factors_in_cat - 1))
  # p<-temp_df %>%
  #   group_by(this_categorical) %>%
  #   summarise(mean_value = mean(this_numeric),
  #             sd_error = std.error(this_numeric)) %>%
  #   ggplot(aes(x = this_categorical, y = mean_value)) +
  #   geom_point() +
  #   geom_line() +
  #   geom_errorbar(aes(ymin = mean_value-sd_error, ymax = mean_value+sd_error)) +
  #   # labs(title = ) +
  #   labs (x=categ_colname,y=numeric_colname) +
  #   theme_classic() +
  p<-ggplot(temp_df) + geom_boxplot(aes(x = factor(this_categorical), y = this_numeric)) +
    theme_classic()+
    # theme_classic()+
    xlab(categ_colname)+
    ylab(numeric_colname)+
    ggtitle(paste(paste0(data_frame_name), sep="\n",paste0(paste0('N:',data_count),', p-value:', sprintf("%.3f", p_value),paste0(', corr(\u03B7\u00B2):',sprintf("%.3f", eta_squared)))))  #+
    # annotate(
    #   "label", x=num_factors_in_cat*0.8, y=max(temp_df[,2])*0.9, label=paste( paste0('N:',data_count),sep="\n",paste0('p-value:', sprintf("%.3f", correl$p.value)),paste0('corr:',sprintf("%.3f", correl$statistic))),
    #   color="red", size=5)
  # annotation_custom(grid::textGrob(paste(paste0('corr:',sprintf("%.3f", correl$statistic)) ,paste0('p-value:', sprintf("%.3f", correl$p.value) ),sep = '\n')),  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  # print(eval(parse(text = expression)))
  
  print(p)
  if (str_length(imagefilename)>0){
    ggsave(imagefilename,dpi=300)
  }
  return(temp_df)
}
,
error = function(e){ 
  # (Optional)
  # Do this if an error is caught...
},
warning = function(w){
  # (Optional)
  # Do this if a warning is caught...
},
finally = {
  # (Optional)
  # Do this at the end before quitting the tryCatch structure...
}

)
}

ordnumeric_numeric_corr_with_anova<-function(data,numeric_colname,categ_colname,data_frame_name="",imagefilename=""){
  tryCatch(
    expr = {
      
      expression<-paste0('df$this_numeric<-df$',numeric_colname)
      eval(parse(text=expression))
      expression<-paste0('df$this_categorical<-df$',categ_colname)
      eval(parse(text=expression))
      # print(paste0(categ_colname,'::',numeric_colname ))
      temp_df<-df %>% select(this_categorical,this_numeric) #subset(df,select=c(categ_colname,numeric_colname))
      print(paste0(categ_colname,'::',numeric_colname ))
      print('I AM AT cat_numeric_corr_with_anova')
      temp_df<-replace(temp_df,temp_df=='',NA)
      temp_df<-na.omit(temp_df)
      data_count<-dim(temp_df)[1]
      num_factors_in_cat=length(unique(temp_df$this_categorical)) #[,1]))
      
      print(paste0("num_factors_in_cat",'::',num_factors_in_cat,'::',categ_colname,'::',imagefilename))
      if (num_factors_in_cat==2) {
        
        expression=paste0('glm(factor(',categ_colname,')','~',numeric_colname,',data=temp_df, family=binomial)') 
        # print(expression)
        logit <-glm(factor(this_categorical)~ this_numeric,data=df,family=) # eval(parse(text = expression)) 
        wt <- wilcox.test(temp_df[,2][which(temp_df[,1] == "Male")], temp_df[,2][which(temp_df[,1]=="Female")])
        print(summary(logit))
        print(anova(logit))
        print(wt)
      }
      
      expression=paste0('kruskal.test(',numeric_colname,'~', categ_colname, ',data = temp_df)')
      print(expression)
      correl<-kruskal.test(this_numeric~this_categorical,data=temp_df) #eval(parse(text = expression))
      print(correl)
      p<-temp_df %>%
        group_by(this_categorical) %>%
        summarise(mean_value = mean(this_numeric),
                  sd_error = std.error(this_numeric)) %>%
        ggplot(aes(x = this_categorical, y = mean_value)) +
        geom_point() +
        geom_line() +
        geom_errorbar(aes(ymin = mean_value-sd_error, ymax = mean_value+sd_error)) +
        # labs(title = ) +
        labs (x=categ_colname,y=numeric_colname) +
        theme_classic() + geom_smooth(method = "lm", formula=y~x,level = 0.8)
      # p<-ggplot(temp_df) + geom_boxplot(aes(x = factor(this_categorical), y = this_numeric)) + 
      #   # theme_classic()+
      #   theme_classic()+
      #   xlab(categ_colname)+
      #   ylab(numeric_colname)+
      #   ggtitle(paste0(paste0(data_frame_name), paste0(', N:',data_count),sep="\n",paste0(', p-value:', sprintf("%.3f", correl$p.value)),paste0('corr:',sprintf("%.3f", correl$statistic))))  #+ 
      #   # annotate(
      #   #   "label", x=num_factors_in_cat*0.8, y=max(temp_df[,2])*0.9, label=paste( paste0('N:',data_count),sep="\n",paste0('p-value:', sprintf("%.3f", correl$p.value)),paste0('corr:',sprintf("%.3f", correl$statistic))),
      #   #   color="red", size=5)
      # # annotation_custom(grid::textGrob(paste(paste0('corr:',sprintf("%.3f", correl$statistic)) ,paste0('p-value:', sprintf("%.3f", correl$p.value) ),sep = '\n')),  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
      # # print(eval(parse(text = expression)))
      
      print(p)
      if (str_length(imagefilename)>0){
        ggsave(imagefilename,dpi=300)
      }
      return(temp_df)
    }
    ,
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
    },
    warning = function(w){
      # (Optional)
      # Do this if a warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
    
  )
}
cat_numeric_corr_mean_plot<-function(df,numeric_colname,categ_colname,data_frame_name="",imagefilename=""){# Sample data creation
  # Assuming your data frame 'data' has 'time' and 'value' columns
  # data <- data.frame(time = sample(seq(as.Date('2020/01/01'), as.Date('2020/01/10'), by="day"), 100, replace = TRUE),
  #                    value = rnorm(100))
  
  # Calculate mean and SEM for each time point
  print("WO ZAI ZELI")
  expression<-paste0('df$value<-df$',numeric_colname)
  eval(parse(text=expression))
  expression<-paste0('df$time<-df$',categ_colname)
  eval(parse(text=expression))
  data<-df %>% select(time,value) #subset(df,select=c(categ_colname,numeric_colname))
  print(paste0(categ_colname,'::',numeric_colname ))
  print('I AM AT cat_numeric_corr_mean_plot')
  data<-replace(data,data=='',NA)
  data<-na.omit(data)
  data$time<-factor(data$time)
  correl<-kruskal.test(value~time,data=data) 
  data_summary <- data %>%
    group_by(time) %>%
    summarise(mean = mean(value),
              sem = sd(value) / sqrt(n()))
  
  # Plotting with error bars
  plot<-ggplot(data_summary, aes(x = time, y = mean)) +
    geom_line() + # Add line for mean trend
    geom_point() + # Add points for mean
    geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.2) + # Add error bars
    theme_minimal() +
    labs(title = paste(data_frame_name,paste0('N:',nrow(data),' ,Corr:',correl$statistic,', p-value:',correl$p.value), x = categ_colname, y = numeric_colname, title = "Time Series Analysis with Mean and Error Bars"),sep="\n")
  ggsave(imagefilename, plot, width = 10, height = 6, dpi = 300)
  print(plot)
  }

numerical_cols_corr<-function(df,col1_name,col2_name,imagefilename='',data_frame_name=''){
  tryCatch(
    expr = {
      print(" I AM AT numerical_cols_corr")
      print(paste0(col1_name,'::numerical_cols_corr::',col2_name,'::',imagefilename ))
      
  # temp_df<-df %>% select(as.name(col1_name),as.name(col2_name))
  # print(summary(temp_df))
  temp_df<-subset(df,select=c(col1_name,col2_name))
  # print(temp_df)
  temp_df <- mutate_all(temp_df, function(x) as.numeric(as.character(x)))
  temp_df<-replace(temp_df,temp_df=='',NA)
  temp_df<-na.omit(temp_df)

  # biomarker_summary<-data.frame(summary(temp_df))
  # first_qrt<-as.numeric(str_split(data.frame(summary(temp_df))$Freq[2],':')[[1]][2])
  # third_qrt<-as.numeric(str_split(data.frame(summary(temp_df))$Freq[2],':')[[1]][2])
  # temp_df<-temp_df %>% filter(as.name(col1_name) > first_qrt )
  # temp_df<-temp_df %>% filter(as.name(col1_name) > third_qrt )
  # corr_coeff<- temp_df%>% summarize (cor=cor (as.name(col1_name),as.name(col2_name)))
  
  corr_coeff<-cor.test(temp_df[,1],temp_df[,2])
  print(corr_coeff)
  data_count<-dim(temp_df)[1]
  # corr_coeff<-sprintf("%.2f", corr_coeff)
  # print(paste0('corr_coeff::',corr_coeff))
  p <- ggplot(temp_df, aes_string(x=col1_name,y=col2_name)) +
    theme_classic()+
    ggtitle(paste(paste0(data_frame_name), sep="\n",paste0(paste0('N:',data_count),', p-value:', sprintf("%.3f", corr_coeff$p.value),paste0(', corr:',sprintf("%.3f", corr_coeff$estimate)))) )  + 
    geom_point(color="red")
  #   # annotation_custom(grid::textGrob(lb),  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
  # # print(length(temp_df$nwu)) 
  p<- p+ geom_smooth(method = "lm", formula=y~x,level = 0.8) # +
    # annotate(
    #   "label", x=max(temp_df[,1])*0.8, y=max(temp_df[,2])*0.8, label=paste( paste0('N:',data_count),sep="\n",paste0('p-value:', sprintf("%.3f", corr_coeff$p.value)),paste0('corr:',sprintf("%.3f", corr_coeff$estimate))),
    #   color="red", size=5)
  print(p)

  if (str_length(imagefilename)>0){
    ggsave(imagefilename,dpi=300)
  }
  # return(list(corr_coeff,p))
    }
  ,
  error = function(e){ 
    print(" I Have error AT numerical_cols_corr")
    # (Optional)
    # Do this if an error is caught...
  },
  warning = function(w){
    # (Optional)
    # Do this if a warning is caught...
  },
  finally = {
    # (Optional)
    # Do this at the end before quitting the tryCatch structure...
  }
  
  )
}
numerical_histogram <- function(df,colname1,imagefilename="") {

  temp_df<-subset(df,select=c(colname1))
  temp_df <- mutate_all(temp_df, function(x) as.numeric(as.character(x)))
  temp_df<-replace(temp_df,temp_df=='',NA)
  temp_df<-na.omit(temp_df) 
  biomarker_summary<-data.frame(summary(temp_df))  %>%  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  sd_biomarker<-temp_df  %>%  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))
  this_median<-as.numeric(str_split(data.frame(summary(temp_df))$Freq[3],':')[[1]][2]) #biomarker_summary$Freqmeans_biomarker[[1]][1]
  # this_sd<-as.numeric(str_split(data.frame(summary(temp_df))$Freq[2],':')[[1]][2])
  this_sd_plus<-as.numeric(str_split(data.frame(summary(temp_df))$Freq[5],':')[[1]][2])
  this_sd_minus<-as.numeric(str_split(data.frame(summary(temp_df))$Freq[2],':')[[1]][2]) #this_mean-this_sd
  histgram<-ggplot(temp_df, aes_string(x = colname1)) +
    theme_classic()+
    geom_density(col='gray',fill='gray') +
    geom_vline(aes(xintercept=this_median),
                  color="red", linetype="dashed", size=1)+
    geom_vline(aes(xintercept=this_sd_plus),
             color="green", linetype="dashed", size=1)+
    geom_vline(aes(xintercept=this_sd_minus),
               color="blue", linetype="dashed", size=1)
  if (str_length(imagefilename)>0){
    ggsave(imagefilename,dpi=300)
  }
    # print(p)
    # geom_histogram(bins=10) +

  #+
    # geom_density(alpha = 0.3) 
  # print(histgram)
  # a=1
  return(histgram)
}

cat_cat_corr_with_cramer<-function(df,numeric_colname,categ_colname,data_frame_name="",imagefilename=""){
  tryCatch(
    expr = {
      
      expression<-paste0('df$this_numeric<-df$',numeric_colname)
      eval(parse(text=expression))
      expression<-paste0('df$this_categorical<-df$',categ_colname)
      eval(parse(text=expression))
      # print(paste0(categ_colname,'::',numeric_colname ))
      temp_df<-df %>% select(this_categorical,this_numeric) #subset(df,select=c(categ_colname,numeric_colname))
      print(paste0(categ_colname,'::',numeric_colname ))
      print('I AM AT cat_numeric_corr_with_anova')
      temp_df<-replace(temp_df,temp_df=='',NA)
      temp_df<-na.omit(temp_df)
      data_count<-dim(temp_df)[1]
      num_factors_in_cat=length(unique(temp_df$this_categorical)) #[,1]))
      
      print(paste0("num_factors_in_cat",'::',num_factors_in_cat,'::',categ_colname,'::',imagefilename))
      if (num_factors_in_cat==2) {
        
        expression=paste0('glm(factor(',categ_colname,')','~',numeric_colname,',data=temp_df, family=binomial)') 
        # print(expression)
        logit <-glm(factor(this_categorical)~ this_numeric,data=df,family=) # eval(parse(text = expression)) 
        wt <- wilcox.test(temp_df[,2][which(temp_df[,1] == "Male")], temp_df[,2][which(temp_df[,1]=="Female")])
        print(summary(logit))
        print(anova(logit))
        print(wt)
      }
      
      expression=paste0('kruskal.test(',numeric_colname,'~', categ_colname, ',data = temp_df)')
      print(expression)
      correl<-kruskal.test(this_numeric~this_categorical,data=temp_df) #eval(parse(text = expression))
      eta_squared_approx <- correl$statistic / (data_count * (num_factors_in_cat - 1))
      print(correl)
      # p<-temp_df %>%
      #   group_by(this_categorical) %>%
      #   summarise(mean_value = mean(this_numeric),
      #             sd_error = std.error(this_numeric)) %>%
      #   ggplot(aes(x = this_categorical, y = mean_value)) +
      #   geom_point() +
      #   geom_line() +
      #   geom_errorbar(aes(ymin = mean_value-sd_error, ymax = mean_value+sd_error)) +
      #   # labs(title = ) +
      #   labs (x=categ_colname,y=numeric_colname) +
      #   theme_classic() +
      p<-ggplot(temp_df) + geom_boxplot(aes(x = factor(this_categorical), y = this_numeric)) +
        # theme_classic()+
        theme_classic()+
        xlab(categ_colname)+
        ylab(numeric_colname)+
        ggtitle(paste(paste0(data_frame_name),sep="\n",paste0( paste0('N:',data_count),', p-value:', sprintf("%.3f", correl$p.value)),paste0('corr(eta_squared_approx):',sprintf("%.3f", eta_squared_approx))))  #+
      # annotate(
      #   "label", x=num_factors_in_cat*0.8, y=max(temp_df[,2])*0.9, label=paste( paste0('N:',data_count),sep="\n",paste0('p-value:', sprintf("%.3f", correl$p.value)),paste0('corr:',sprintf("%.3f", correl$statistic))),
      #   color="red", size=5)
      # annotation_custom(grid::textGrob(paste(paste0('corr:',sprintf("%.3f", correl$statistic)) ,paste0('p-value:', sprintf("%.3f", correl$p.value) ),sep = '\n')),  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
      # print(eval(parse(text = expression)))
      
      print(p)
      if (str_length(imagefilename)>0){
        ggsave(imagefilename,dpi=300)
      }
      return(temp_df)
    }
    ,
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
    },
    warning = function(w){
      # (Optional)
      # Do this if a warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
    
  )
}

cat_numeric_corr_with_kruskal<-function(df,numeric_colname,categ_colname,data_frame_name="",imagefilename=""){
  tryCatch(
    expr = {
      
      expression<-paste0('df$this_numeric<-df$',numeric_colname)
      eval(parse(text=expression))
      expression<-paste0('df$this_categorical<-df$',categ_colname)
      eval(parse(text=expression))
      # print(paste0(categ_colname,'::',numeric_colname ))
      temp_df<-df %>% select(this_categorical,this_numeric) #subset(df,select=c(categ_colname,numeric_colname))
      print(paste0(categ_colname,'::',numeric_colname ))
      print('I AM AT cat_numeric_corr_with_anova')
      temp_df<-replace(temp_df,temp_df=='',NA)
      temp_df<-na.omit(temp_df)
      data_count<-dim(temp_df)[1]
      num_factors_in_cat=length(unique(temp_df$this_categorical)) #[,1]))
      
      print(paste0("num_factors_in_cat",'::',num_factors_in_cat,'::',categ_colname,'::',imagefilename))
      if (num_factors_in_cat==2) {
        
        expression=paste0('glm(factor(',categ_colname,')','~',numeric_colname,',data=temp_df, family=binomial)') 
        # print(expression)
        logit <-glm(factor(this_categorical)~ this_numeric,data=df,family=) # eval(parse(text = expression)) 
        wt <- wilcox.test(temp_df[,2][which(temp_df[,1] == "Male")], temp_df[,2][which(temp_df[,1]=="Female")])
        print(summary(logit))
        print(anova(logit))
        print(wt)
      }
      
      expression=paste0('kruskal.test(',numeric_colname,'~', categ_colname, ',data = temp_df)')
      print(expression)
      correl<-kruskal.test(this_numeric~this_categorical,data=temp_df) #eval(parse(text = expression))
      print(correl)
      eta_squared_approx <- correl$statistic / (data_count * (num_factors_in_cat - 1))
      # p<-temp_df %>%
      #   group_by(this_categorical) %>%
      #   summarise(mean_value = mean(this_numeric),
      #             sd_error = std.error(this_numeric)) %>%
      #   ggplot(aes(x = this_categorical, y = mean_value)) +
      #   geom_point() +
      #   geom_line() +
      #   geom_errorbar(aes(ymin = mean_value-sd_error, ymax = mean_value+sd_error)) +
      #   # labs(title = ) +
      #   labs (x=categ_colname,y=numeric_colname) +
      #   theme_classic() +
      p<-ggplot(temp_df) + geom_boxplot(aes(x = factor(this_categorical), y = this_numeric)) +
        # theme_classic()+
        theme_classic()+
        xlab(categ_colname)+
        ylab(numeric_colname)+
        ggtitle(paste(paste0(data_frame_name), sep="\n",paste0(paste0('N:',data_count),', p-value:', sprintf("%.3f", correl$p.value),paste0(', corr(\u03B7\u00B2):',sprintf("%.3f", eta_squared_approx)))))  #+
      # annotate(
      #   "label", x=num_factors_in_cat*0.8, y=max(temp_df[,2])*0.9, label=paste( paste0('N:',data_count),sep="\n",paste0('p-value:', sprintf("%.3f", correl$p.value)),paste0('corr:',sprintf("%.3f", correl$statistic))),
      #   color="red", size=5)
      # annotation_custom(grid::textGrob(paste(paste0('corr:',sprintf("%.3f", correl$statistic)) ,paste0('p-value:', sprintf("%.3f", correl$p.value) ),sep = '\n')),  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
      # print(eval(parse(text = expression)))
      
      print(p)
      if (str_length(imagefilename)>0){
        ggsave(imagefilename,dpi=300)
      }
      return(temp_df)
    }
    ,
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
    },
    warning = function(w){
      # (Optional)
      # Do this if a warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
    
  )
}

numeric_cat_corr_for_boxplot_median<-function(column_numeric,column_cat,imagefilename=""){
  df<-df %>% select(!!sym(column_numeric),!!sym(column_cat))
  # df<-df %>% select(wfns,SAH_SEG_TOTAL)
  df<-replace(df,df=='',NA)
  df<-na.omit(df)
  df <- df %>%
    mutate(!!column_cat := as.factor(as.numeric(!!sym(column_cat))))
  median_data <- df %>%
    group_by(!!sym(column_cat)) %>%
    summarise(MedianValue = median(!!sym(column_numeric)))
  median_data <- median_data %>%
    mutate(GroupNumeric = as.numeric(!!sym(column_cat)))
  
  # median_data$GroupNumeric = as.numeric(median_data$wfns)  # Convert to numeric if necessary
  
  model <- lm(MedianValue ~ GroupNumeric, data = median_data)
  # Extract R-squared value
  summary_model <- summary(model)
  r_squared <- summary_model$r.squared
  
  # Extract p-value
  p_value <- summary_model$coefficients[2, 4]
  annotation_text <- sprintf("R² = %.2f\np-value = %.3f", r_squared, p_value)
  #
  #
  p<-ggplot(data = df, aes(x = factor(!!sym(column_cat)), y = !!sym(column_numeric))) +
    geom_boxplot() +
    # annotate("text", x = Inf, y = Inf, label = annotation_text, hjust = 1.1, vjust = 1.1, size = 5, color = "black") +
    
    geom_point(data = median_data, aes(x = !!sym(column_cat), y = MedianValue), color = "red", size = 3) +
    geom_line(data = median_data, aes(x = !!sym(column_cat), y = fitted(model)), size=2,group = 1, color = "blue") +
    geom_smooth(data = median_data, aes(x = GroupNumeric, y = MedianValue), method = "lm", se = TRUE, color = "blue", fill = "lightblue", size = 2) +  # Increased line thickness
    # title = paste0(column_cat,"VS",column_numeric),
    labs(
      x = column_cat,
      y = column_numeric) +
    theme_minimal()
  plot(p)
  if (str_length(imagefilename)>0){
    ggsave(imagefilename,dpi=300, bg = "white")
  }
}
