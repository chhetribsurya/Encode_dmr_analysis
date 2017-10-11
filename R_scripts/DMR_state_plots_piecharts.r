
args <- commandArgs(TRUE)
setwd("/home/surya/Desktop/scripts/data")
getwd()

library(plyr)
library(data.table)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

#write.table(df, file="foo.bed", quote=F, sep="\t", row.names=F, col.names=F)


###############################################################################################
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




bed_g1_g2 <- fread("metilene_dmr_data/metilene_output_g1_g2_qval.0.05.out", sep="\t")
names(bed_g1_g2) <- c('chr','start','end','q_val','diff','CpG','group1','group2')
bed_g1_g2$H9_ESC_DMR_STATE <- ""
bed_g1_g2$SM_CELL_DMR_STATE <- ""
bed_g1_g2

#For ESC_DMR
#Just finding the indices for marking the state of DMRs:
hypo_indices  <- which(bed_g1_g2$group1 < 0.25)
bed_g1_g2[hypo_indices,]
intermed_indices  <- which(bed_g1_g2$group1 >= 0.25 & bed_g1_g2$group1 <= 0.75)
bed_g1_g2[intermed_indices,]
hyper_indices  <- which(bed_g1_g2$group1 > 0.75)
bed_g1_g2[hyper_indices,]


#Naming the DMRs:
hypo_indices  <- which(bed_g1_g2$group1 < 0.25)
bed_g1_g2[hypo_indices,]$H9_ESC_DMR_STATE <- "Hypo DMRs"
intermed_indices  <- which(bed_g1_g2$group1 >= 0.25 & bed_g1_g2$group1 <= 0.75)
bed_g1_g2[intermed_indices,]$H9_ESC_DMR_STATE <- "Inter DMRs"
hyper_indices  <- which(bed_g1_g2$group1 > 0.75)
bed_g1_g2[hyper_indices,]$H9_ESC_DMR_STATE <- "Hyper DMRs"
bed_g1_g2


#For SM_cell_state_DMRs:
#Just finding the indices for marking the state of DMRs:
hypo_indices  <- which(bed_g1_g2$group2 < 0.25)
bed_g1_g2[hypo_indices,]
intermed_indices  <- which(bed_g1_g2$group2 >= 0.25 & bed_g1_g2$group2 <= 0.75)
bed_g1_g2[intermed_indices,]
hyper_indices  <- which(bed_g1_g2$group2 > 0.75)
bed_g1_g2[hyper_indices,]


#Naming the DMRs:
hypo_indices  <- which(bed_g1_g2$group2 < 0.25)
bed_g1_g2[hypo_indices,]$SM_CELL_DMR_STATE <- "Hypo DMRs"
intermed_indices  <- which(bed_g1_g2$group2 >= 0.25 & bed_g1_g2$group2 <= 0.75)
bed_g1_g2[intermed_indices,]$SM_CELL_DMR_STATE <- "Inter DMRs"
hyper_indices  <- which(bed_g1_g2$group2 > 0.75)
bed_g1_g2[hyper_indices,]$SM_CELL_DMR_STATE <- "Hyper DMRs"
bed_g1_g2[which(bed_g1_g2$SM_CELL_DMR_STATE == ""),]
nrow(bed_g1_g2)


theme_cfg <- theme(
  #axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=14, face="bold")
  )


pdf(file="~/Desktop/R_dir/Piechart_ESC_vs_SM_cell.pdf")
p1 <- ggplot(bed_g1_g2, aes(x=factor(1), fill=H9_ESC_DMR_STATE))+
  geom_bar(width = 1)+ coord_polar("y") + theme_cfg
p1

p2 <- ggplot(bed_g1_g2, aes(x=factor(1), fill=SM_CELL_DMR_STATE))+
  geom_bar(width = 1) + coord_polar("y") + theme_cfg
p2
dev.off()

pdf(file="~/Desktop/R_dir/CombinedESC_vs_SM_cell.pdf")
grid.arrange(p3, arrangeGrob(p1,p2),ncol=2)
dev.off()







bed_g1_g3 <- fread("metilene_dmr_data/metilene_output_g1_g3_qval.0.05.out", sep="\t")
names(bed_g1_g3) <- c('chr','start','end','q_val','diff','CpG','group1','group2')
bed_g1_g3$H9_ESC_DMR_STATE <- ""
bed_g1_g3$Hepatocytes_DMR_STATE <- ""
bed_g1_g3

#For ESC_DMR
#Just finding the indices for marking the state of DMRs:
hypo_indices  <- which(bed_g1_g3$group1 < 0.25)
bed_g1_g3[hypo_indices,]
intermed_indices  <- which(bed_g1_g3$group1 >= 0.25 & bed_g1_g3$group1 <= 0.75)
bed_g1_g3[intermed_indices,]
hyper_indices  <- which(bed_g1_g3$group1 > 0.75)
bed_g1_g3[hyper_indices,]


#Naming the DMRs:
hypo_indices  <- which(bed_g1_g3$group1 < 0.25)
bed_g1_g3[hypo_indices,]$H9_ESC_DMR_STATE <- "Hypo DMRs"
intermed_indices  <- which(bed_g1_g3$group1 >= 0.25 & bed_g1_g3$group1 <= 0.75)
bed_g1_g3[intermed_indices,]$H9_ESC_DMR_STATE <- "Inter DMRs"
hyper_indices  <- which(bed_g1_g3$group1 > 0.75)
bed_g1_g3[hyper_indices,]$H9_ESC_DMR_STATE <- "Hyper DMRs"
bed_g1_g3


#For SM_cell_state_DMRs:
#Just finding the indices for marking the state of DMRs:
hypo_indices  <- which(bed_g1_g3$group2 < 0.25)
bed_g1_g3[hypo_indices,]
intermed_indices  <- which(bed_g1_g3$group2 >= 0.25 & bed_g1_g3$group2 <= 0.75)
bed_g1_g3[intermed_indices,]
hyper_indices  <- which(bed_g1_g3$group2 > 0.75)
bed_g1_g3[hyper_indices,]


#Naming the DMRs:
hypo_indices  <- which(bed_g1_g3$group2 < 0.25)
bed_g1_g3[hypo_indices,]$Hepatocytes_DMR_STATE <- "Hypo DMRs"
intermed_indices  <- which(bed_g1_g3$group2 >= 0.25 & bed_g1_g3$group2 <= 0.75)
bed_g1_g3[intermed_indices,]$Hepatocytes_DMR_STATE <- "Inter DMRs"
hyper_indices  <- which(bed_g1_g3$group2 > 0.75)
bed_g1_g3[hyper_indices,]$Hepatocytes_DMR_STATE <- "Hyper DMRs"
bed_g1_g3[which(bed_g1_g3$Hepatocytes_DMR_STATE == ""),]
nrow(bed_g1_g3)


theme_cfg <- theme(
  #axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=14, face="bold")
  )


pdf(file="~/Desktop/R_dir/Piechart_ESC_vs_Hepatocytes_cell.pdf")
p1 <- ggplot(bed_g1_g3, aes(x=factor(1), fill=H9_ESC_DMR_STATE))+
  geom_bar(width = 1)+ coord_polar("y") + theme_cfg
p1

p2 <- ggplot(bed_g1_g3, aes(x=factor(1), fill=Hepatocytes_DMR_STATE))+
  geom_bar(width = 1) + coord_polar("y") + theme_cfg
p2

dev.off()

pdf(file="~/Desktop/R_dir/CombinedESC_vs_Hepatocytes_cell.pdf")
grid.arrange(p3, arrangeGrob(p1,p2),ncol=2)
dev.off()












bed_g2_g3 <- fread("metilene_dmr_data/metilene_output_g2_g3_qval.0.05.out", sep="\t")
names(bed_g2_g3) <- c('chr','start','end','q_val','diff','CpG','group1','group2')
bed_g2_g3$SM_CELL_DMR_STATE <- ""
bed_g2_g3$Hepatocytes_DMR_STATE <- ""
bed_g2_g3

#For ESC_DMR
#Just finding the indices for marking the state of DMRs:
hypo_indices  <- which(bed_g2_g3$group1 < 0.25)
bed_g2_g3[hypo_indices,]
intermed_indices  <- which(bed_g2_g3$group1 >= 0.25 & bed_g2_g3$group1 <= 0.75)
bed_g2_g3[intermed_indices,]
hyper_indices  <- which(bed_g2_g3$group1 > 0.75)
bed_g2_g3[hyper_indices,]


#Naming the DMRs:
hypo_indices  <- which(bed_g2_g3$group1 < 0.25)
bed_g2_g3[hypo_indices,]$SM_CELL_DMR_STATE <- "Hypo DMRs"
bed_g2_g3
intermed_indices  <- which(bed_g2_g3$group1 >= 0.25 & bed_g2_g3$group1 <= 0.75)
bed_g2_g3[intermed_indices,]$SM_CELL_DMR_STATE <- "Inter DMRs"
hyper_indices  <- which(bed_g2_g3$group1 > 0.75)
bed_g2_g3[hyper_indices,]$SM_CELL_DMR_STATE <- "Hyper DMRs"
bed_g2_g3


#For SM_cell_state_DMRs:
#Just finding the indices for marking the state of DMRs:
hypo_indices  <- which(bed_g2_g3$group2 < 0.25)
bed_g2_g3[hypo_indices,]
intermed_indices  <- which(bed_g2_g3$group2 >= 0.25 & bed_g2_g3$group2 <= 0.75)
bed_g2_g3[intermed_indices,]
hyper_indices  <- which(bed_g2_g3$group2 > 0.75)
bed_g2_g3[hyper_indices,]


#Naming the DMRs:
hypo_indices  <- which(bed_g2_g3$group2 < 0.25)
bed_g2_g3[hypo_indices,]$Hepatocytes_DMR_STATE <- "Hypo DMRs"
intermed_indices  <- which(bed_g2_g3$group2 >= 0.25 & bed_g2_g3$group2 <= 0.75)
bed_g2_g3[intermed_indices,]$Hepatocytes_DMR_STATE <- "Inter DMRs"
hyper_indices  <- which(bed_g2_g3$group2 > 0.75)
bed_g2_g3[hyper_indices,]$Hepatocytes_DMR_STATE <- "Hyper DMRs"
bed_g2_g3[which(bed_g2_g3$Hepatocytes_DMR_STATE == ""),]
nrow(bed_g2_g3)


theme_cfg <- theme(
  #axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=14, face="bold")
  )


pdf(file="~/Desktop/R_dir/Piechart_SM_CELL_vs_Hepatocytes_cell.pdf")
p1 <- ggplot(bed_g2_g3, aes(x=factor(1), fill=SM_CELL_DMR_STATE))+
  geom_bar(width = 1)+ coord_polar("y") + theme_cfg
p1

p2 <- ggplot(bed_g2_g3, aes(x=factor(1), fill=Hepatocytes_DMR_STATE))+
  geom_bar(width = 1) + coord_polar("y") + theme_cfg
p2

dev.off()

pdf(file="~/Desktop/R_dir/Combined_SM_cell_vs_Hepatocytes_cell.pdf")
grid.arrange(p3, arrangeGrob(p1,p2),ncol=2)
dev.off()




###############################################################################################
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




## make nice plots
theme_cfg <-
theme(panel.background = element_rect(colour = "black"),
			axis.text=element_text(color="black",size=10),
	        axis.title=element_text(color="black",size=12,face="bold"),
	        plot.title = element_text(size = rel(1.3), colour = "darkblue", face="bold"))


bed_g1_g2 <- fread("metilene_dmr_data/metilene_output_g1_g2_qval.0.05.out", sep="\t")
bed_g1_g2
names(bed_g1_g2) <- c('chr','start','end','q_val','diff','CpG','group1','group2')
## Plot statistics
dir.create("~/Desktop/R_dir")
pdf(file="~/Desktop/R_dir/final_ESC_vs_SM_cell.pdf")
#difference histogram
p1 <- ggplot(bed_g1_g2, aes(x=diff)) + geom_histogram(binwidth=0.04, fill='darkgrey', color='red') + xlab("Mean Methylation Difference ( ESC - SM cell )") + ylab("DMR count") + scale_x_continuous(limits=c(-1,1)) + theme_cfg
p1
#q_val vs difference
p2 <- ggplot(bed_g1_g2, aes(x=diff, y=q_val)) + geom_point(alpha=.5,color="blue") + scale_y_log10() + xlab("Mean Methylation Difference ( ESC - SM cell )") + ylab("q-value") + theme_cfg
p2

melt_bed_g1_g2 <- bed_g1_g2
names(melt_bed_g1_g2) <- c('chr','start','end','q_val','diff','CpG','H9_ESC','SM_Cell')
melt_bed_g1_g2

merged_df <- melt(bed_g1_g2, id.vars = c('chr','start','end','q_val','diff','CpG'))
names(merged_df) <- c('chr','start','end','q_val','diff','CpG', 'Cell_Type', 'value')
merged_df

cdat <- ddply(merged_df, "Cell_Type", summarise, value.mean=mean(value))
p3 <-ggplot2::ggplot(merged_df, aes(x=value, y=..count../sum(..count..), group=Cell_Type, colour=Cell_Type, fill=Cell_Type)) +
    ggplot2::geom_density(alpha= 0.3) + geom_vline(data=cdat, aes(xintercept=value.mean,  colour=Cell_Type),
               linetype="dashed", size=1) + xlab("Mean methylation distribution for DMRs") + ylab("Density") + theme_cfg
p3



#dev.off()

#pdf(file="~/Desktop/R_dir/g1_g2_combined.pdf")
#grid.arrange(p1, p2, p3, nrow =2)

dev.off()







bed_g1_g3 <- fread("metilene_dmr_data/metilene_output_g1_g3_qval.0.05.out", sep="\t")
bed_g1_g3
names(bed_g1_g3) <- c('chr','start','end','q_val','diff','CpG','group1','group2')
## Plot statistics
dir.create("~/Desktop/R_dir")
pdf(file="~/Desktop/R_dir/final_ESC_vs_hepatocytes.pdf")
#difference histogram
p1 <- ggplot(bed_g1_g3, aes(x=diff)) + geom_histogram(binwidth=0.04, fill='darkgrey', color='red') + xlab("Mean Methylation Difference ( ESC - Hepatocytes )") + ylab("DMR count") + scale_x_continuous(limits=c(-1,1)) + theme_cfg
p1
#q_val vs difference
p2 <- ggplot(bed_g1_g3, aes(x=diff, y=q_val)) + geom_point(alpha=.5,color="blue") + scale_y_log10() + xlab("Mean Methylation Difference ( ESC - Hepatocytes)") + ylab("q-value") + theme_cfg
p2

melt_bed_g1_g2 <- bed_g1_g3
names(melt_bed_g1_g2) <- c('chr','start','end','q_val','diff','CpG','H9_ESC','Hepatocytes')
melt_bed_g1_g2

merged_df <- melt(bed_g1_g3, id.vars = c('chr','start','end','q_val','diff','CpG'))
names(merged_df) <- c('chr','start','end','q_val','diff','CpG', 'Cell_Type', 'value')
merged_df

cdat <- ddply(merged_df, "Cell_Type", summarise, value.mean=mean(value))
p3 <-ggplot2::ggplot(merged_df, aes(x=value, y=..count../sum(..count..), group=Cell_Type, colour=Cell_Type, fill=Cell_Type)) +
    ggplot2::geom_density(alpha= 0.3) + geom_vline(data=cdat, aes(xintercept=value.mean,  colour=Cell_Type),
               linetype="dashed", size=1) + xlab("Mean methylation distribution for DMRs") + ylab("Density") + theme_cfg
p3



#dev.off()

#pdf(file="~/Desktop/R_dir/g1_g2_combined.pdf")
#grid.arrange(p1, p2, p3, nrow =2)

dev.off()







bed <- fread("metilene_dmr_data/metilene_output_g2_g3_qval.0.05.out", sep="\t")
bed
names(bed) <- c('chr','start','end','q_val','diff','CpG','group1','group2')
## Plot statistics
dir.create("~/Desktop/R_dir")
pdf(file="~/Desktop/R_dir/final_smooth_vs_hepatocytes.pdf")
#difference histogram
p1 <- ggplot(bed, aes(x=diff)) + geom_histogram(binwidth=0.04, fill='darkgrey', color='red') + xlab("Mean Methylation Difference ( SM_Cell - Hepatocytes )") + ylab("DMR count") + scale_x_continuous(limits=c(-1,1)) + theme_cfg
p1
#q_val vs difference
p2 <- ggplot(bed, aes(x=diff, y=q_val)) + geom_point(alpha=.5,color="blue") + scale_y_log10() + xlab("Mean Methylation Difference ( SM cell - Hepatocytes )") + ylab("q-value") + theme_cfg
p2

melt_bed_g1_g2 <- bed
names(melt_bed_g1_g2) <- c('chr','start','end','q_val','diff','CpG','SM_Cell','Hepatocytes')
melt_bed_g1_g2

merged_df <- melt(bed, id.vars = c('chr','start','end','q_val','diff','CpG'))
names(merged_df) <- c('chr','start','end','q_val','diff','CpG', 'Cell_Type', 'value')
merged_df

cdat <- ddply(merged_df, "Cell_Type", summarise, value.mean=mean(value))
p3 <-ggplot2::ggplot(merged_df, aes(x=value, y=..count../sum(..count..), group=Cell_Type, colour=Cell_Type, fill=Cell_Type)) +
    ggplot2::geom_density(alpha= 0.3) + geom_vline(data=cdat, aes(xintercept=value.mean,  colour=Cell_Type),
               linetype="dashed", size=1) + xlab("Mean methylation distribution for DMRs") + ylab("Density") + theme_cfg
p3



#dev.off()

#pdf(file="~/Desktop/R_dir/g1_g2_combined.pdf")
#grid.arrange(p1, p2, p3, nrow =2)

dev.off()










###########################################################################################
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




















bed_g1_g3 <- fread("metilene_dmr_data/metilene_output_g1_g3_qval.0.05.out", sep="\t")
names(bed_g1_g3) <- c('chr','start','end','q_val','diff','CpG','group1','group2')

## Plot statistics
pdf(file="~/Desktop/R_dir/g1_g3.pdf")
#difference histogram
ggplot(bed_g1_g3, aes(x=diff)) + geom_histogram(binwidth=0.04, fill='darkgrey', color='red') + xlab("Mean Methylation Difference ( ESC - Hepatocytes )") + ylab("DMR count") + scale_x_continuous(limits=c(-1,1)) +theme_cfg
#length distribution CpGs
ggplot(bed_g1_g3, aes(x=length)) + geom_line(stat="density", size=1) + xlab("DMR length [nt]") + theme_cfg
#q_val vs difference
ggplot(bed_g1_g3, aes(x=diff, y=q_val)) + geom_point(alpha=.5,color="blue") + scale_y_log10() + xlab("Mean Methylation Difference ( ESC - Hepatocytes )") + ylab("q-value") + theme_cfg

dev.off()






bed_g2_g3 <- fread("metilene_dmr_data/metilene_output_g2_g3_qval.0.05.out", sep="\t")
names(bed_g1_g3) <- c('chr','start','end','q_val','diff','CpG','group1','group2')

pdf(file="~/Desktop/R_dir/g2_g3.pdf")
#difference histogram
ggplot(bed_g2_g3, aes(x=diff)) + geom_histogram(binwidth=0.02, fill='darkgrey', color='black') + xlab("mean methylation difference") + theme_cfg + scale_x_continuous(limits=c(-1,1))
#length distribution CpGs
ggplot(bed_g2_g3, aes(x=length)) + geom_line(stat="density", size=1) + xlab("DMR length [nt]") + theme_cfg
#q_val vs difference
ggplot(bed_g2_g3, aes(x=diff, y=q_val)) + geom_point(alpha=.5) + scale_y_log10() + xlab("mean methylation difference") + ylab("q-value") + theme_cfg
dev.off()

