## Author: Cody Martin
## PI: Ry Young
## Institution: Texas A&M University
## Department of Biochemistry & Biophysics, Center for Phage Technology
## Date: 04/21/2021

# R v4.0.4
# ggplot2 v3.3.3
# ggprism v1.0.2
# dplyr v1.0.5
setwd("~/Documents/CPT/N4/CPT_code/Disruptin_Fig5/")
library(ggplot2)
library(ggprism)
library(dplyr)

data = read.csv("181120_ThT_Sytox_full_dataset.csv", header = T)

# Reorder Signal_Type to be in desired order
data$Sample = factor(data$Sample, levels = c("pRE", "gp28", "gp28 L46P", "gp28 K16C", 
                                             "gp28-his", "gp28-his L46P", "gp28-his K16C"))
data$Signal_Type = factor(data$Signal_Type, levels = c("Sytox", "ThT_and_Sytox", "ThT", "None"))

# create sub samples
samples1 = c("pRE", "gp28", "gp28 L46P", "gp28-his")
subdata1 = data[data$Sample %in% samples1,]
# reorder samples to be in desired order
subdata1$Sample = factor(subdata1$Sample, levels = samples1)

# plotting function
make_barplot = function(input_dataframe) {
  totals = input_dataframe %>% 
    group_by(Sample, Time) %>% 
    summarise(Total = sum(Count), .groups="drop")
  
  n_min = min(totals$Total)
  n_max = max(totals$Total)
  n_total = sum(totals$Total)
  
  sytox_count = sum(grepl("Sytox", input_dataframe$Signal_Type))
  if (sytox_count == 0) {
    label = c("ThT", "None")
  } else {
    label = c("Sytox", "ThT & Sytox", "ThT", "None")
  }
  
  return(ggplot(input_dataframe, aes(x=as.factor(Time), y=Count, 
                                     fill=Signal_Type, color=Signal_Type)) +
           geom_bar(position="fill", stat="identity", color="black", size=1.1, width=0.7) +
           geom_text(aes(label=ifelse(Count > 16, Count, ""), 
                         color=ifelse(Signal_Type=="None", "white","black")),
                     size=4.5, position=position_fill(vjust=0.5)) +
           scale_color_identity() +
           scale_y_continuous(expand = c(0,0)) +
           scale_fill_manual(values = c("None" = "#000000",
                                        "ThT" = "#1DBDFF",
                                        "ThT_and_Sytox" = "#FFFC4F",
                                        "Sytox" = "#FF860C"),
                             labels = label) +
           labs(x = "Time (min)", 
                y = "Relative Frequency", 
                fill="Signal", 
                title="Sample",
                caption=bquote(paste(.(n_min), " \u2264 ", "n"["ind"],  " \u2264 ", .(n_max), 
                                      " ", "n"["total"], " = ", .(n_total)
                                      )
                               )
                ) +
           theme_prism(border=T) +
           coord_cartesian(clip = "off") +
           facet_wrap(~Sample, nrow=1) +
           theme(aspect.ratio = (length(unique(input_dataframe$Sample))/2)/1, 
                 legend.title = element_text(), 
                 panel.spacing = unit(0, "lines"),
                 legend.key.width = unit(0.5, "cm"),
                 legend.key.height = unit(0.5, "cm")
                 )
        )
}

expt_name = "181120_ThT_Sytox"

plt_full=make_barplot(data)
svg(paste0("plots/", expt_name, "_fulldata.svg"), width = 15, height = 5)
plt_full
dev.off()

plt_1=make_barplot(subdata1)
sample_names = paste(gsub(" ", "-", samples1), collapse="_")
svg(paste0("plots/", expt_name, "_", sample_names, ".svg"), width = 10, height = 5)
plt_1
dev.off()

lambda_data = read.csv("181011_ThT_Sytox_Lambda_control.csv", header = T)
lambda_data$Sample = factor(lambda_data$Sample, levels = c("SamRRzRz1", "SRamRzRz1", "SRRzamRz1am"))
lambda_data$Signal_Type = factor(lambda_data$Signal_Type, levels = c("ThT", "None"))

expt_name = "181011_ThT_Sytox"
ctrl_plt=make_barplot(lambda_data)
svg(paste0("plots/", expt_name, "_Lambda_controls.svg"), width = 7.5, height = 5)
ctrl_plt
dev.off()
