library(ggplot2)
library(gg.gap)
library("RColorBrewer")

args = commandArgs(trailingOnly=TRUE)


DRAGEN_summary_data = read.csv2(args[1],sep=",",header=T,dec= '.')
DRAGEN_X_summary_data = read.csv2(args[2],sep=",",header=T,dec= '.')
outdir=args[3]

giab_snp_all<-data.frame(categary=c('DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'DRAGEN_X_TRUTH_TP',	'DRAGEN_X_TRUTH_FN','DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP',	'DRAGEN_X_TRUTH_TP',	'DRAGEN_X_QUERY_FP'),number=c(DRAGEN_summary_data$TRUTH.TP[3],	DRAGEN_summary_data$TRUTH.FN[3],	DRAGEN_X_summary_data$TRUTH.TP[3],	DRAGEN_X_summary_data$TRUTH.FN[3],DRAGEN_summary_data$TRUTH.TP[3],	DRAGEN_summary_data$QUERY.FP[3],	DRAGEN_X_summary_data$TRUTH.TP[3],	DRAGEN_X_summary_data$QUERY.FP[3]), comparison= c('TRUTH_DRAGEN', 'TRUTH_DRAGEN','TRUTH_DRAGEN_X', 'TRUTH_DRAGEN_X','QUERY_DRAGEN', 'QUERY_DRAGEN','QUERY_DRAGEN_X', 'QUERY_DRAGEN_X'), label_ypos=c(DRAGEN_summary_data$TRUTH.TP[3],	DRAGEN_summary_data$TRUTH.TP[3]+DRAGEN_summary_data$TRUTH.FN[3]+10000,	DRAGEN_X_summary_data$TRUTH.TP[3],	DRAGEN_X_summary_data$TRUTH.TP[3]+DRAGEN_X_summary_data$TRUTH.FN[3]+10000,DRAGEN_summary_data$TRUTH.TP[3],	DRAGEN_summary_data$TRUTH.TP[3]+DRAGEN_summary_data$QUERY.FP[3]+10000,	DRAGEN_X_summary_data$TRUTH.TP[3],	DRAGEN_X_summary_data$TRUTH.TP[3]+DRAGEN_X_summary_data$QUERY.FP[3]+10000))
giab_snp_all$comparison <- factor(giab_snp_all$comparison,levels = c('TRUTH_DRAGEN', 'TRUTH_DRAGEN_X','QUERY_DRAGEN','QUERY_DRAGEN_X'))
pdf(paste0(outdir,'/','giab_DRAGEN_X_snp_all.pdf'),width=7.5,height=4.5)
ggplot(data=giab_snp_all, aes(x=comparison, y=number, fill=factor(categary, levels= c('DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP','DRAGEN_X_TRUTH_FN','DRAGEN_X_QUERY_FP','DRAGEN_X_TRUTH_TP'))))+
geom_bar(stat="identity")+
scale_fill_brewer(palette = "Set1",name='categary')+
geom_text(aes(y=label_ypos, label=number), vjust=1.6, color="black", size=3)+
theme( axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="black", size=8, face="bold.italic",hjust = 0.5))+
#ylim(0, 100000)+
ggtitle("GIAB_SNP_ALL")       
dev.off()

giab_snp_all<-data.frame(categary=c('DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'DRAGEN_X_TRUTH_TP',	'DRAGEN_X_TRUTH_FN','DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP',	'DRAGEN_X_TRUTH_TP',	'DRAGEN_X_QUERY_FP'),number=c(DRAGEN_summary_data$TRUTH.TP[3],	DRAGEN_summary_data$TRUTH.FN[3],	DRAGEN_X_summary_data$TRUTH.TP[3],	DRAGEN_X_summary_data$TRUTH.FN[3],DRAGEN_summary_data$TRUTH.TP[3],	DRAGEN_summary_data$QUERY.FP[3],	DRAGEN_X_summary_data$TRUTH.TP[3],	DRAGEN_X_summary_data$QUERY.FP[3]), comparison= c('TRUTH_DRAGEN', 'TRUTH_DRAGEN','TRUTH_DRAGEN_X', 'TRUTH_DRAGEN_X','QUERY_DRAGEN', 'QUERY_DRAGEN','QUERY_DRAGEN_X', 'QUERY_DRAGEN_X'), label_ypos=c(DRAGEN_summary_data$TRUTH.TP[3],	DRAGEN_summary_data$TRUTH.TP[3]+DRAGEN_summary_data$TRUTH.FN[3],	DRAGEN_X_summary_data$TRUTH.TP[3],	DRAGEN_X_summary_data$TRUTH.TP[3]+DRAGEN_X_summary_data$TRUTH.FN[3],DRAGEN_summary_data$TRUTH.TP[3],	DRAGEN_summary_data$TRUTH.TP[3]+DRAGEN_summary_data$QUERY.FP[3],	DRAGEN_X_summary_data$TRUTH.TP[3],	DRAGEN_X_summary_data$TRUTH.TP[3]+DRAGEN_X_summary_data$QUERY.FP[3]))
giab_snp_all$comparison <- factor(giab_snp_all$comparison,levels = c('TRUTH_DRAGEN', 'TRUTH_DRAGEN_X','QUERY_DRAGEN','QUERY_DRAGEN_X'))
pdf(paste0(outdir,'/','giab_DRAGEN_X_snp_all2.pdf'),width=4.5,height=7.5)
p<-ggplot(data=giab_snp_all, aes(x=comparison, y=number, fill=factor(categary, levels= c('DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP','DRAGEN_X_TRUTH_FN','DRAGEN_X_QUERY_FP','DRAGEN_X_TRUTH_TP'))))+
geom_bar(stat="identity")+
scale_fill_brewer(palette = "Set1",name='categary')+
geom_text(aes(y=label_ypos, label=number), vjust=1.6, color="black", size=2.5)+
theme( axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="black", size=8, face="bold.italic",hjust = 0.5))+
#ylim(0, 100000)+
ggtitle("GIAB_SNP_ALL")
gg.gap(plot=p,
       segments=c(10000,57000),
       tick_width = c(10000,1000),
       rel_heights=c(0.4,0,1),
       ylim=c(0,68000))       
dev.off()


giab_indel_all<-data.frame(categary=c('DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'DRAGEN_X_TRUTH_TP',	'DRAGEN_X_TRUTH_FN','DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP',	'DRAGEN_X_TRUTH_TP',	'DRAGEN_X_QUERY_FP'),number=c(DRAGEN_summary_data$TRUTH.TP[1],	DRAGEN_summary_data$TRUTH.FN[1],	DRAGEN_X_summary_data$TRUTH.TP[1],	DRAGEN_X_summary_data$TRUTH.FN[1],DRAGEN_summary_data$TRUTH.TP[1],	DRAGEN_summary_data$QUERY.FP[1],	DRAGEN_X_summary_data$TRUTH.TP[1],	DRAGEN_X_summary_data$QUERY.FP[1]), comparison= c('TRUTH_DRAGEN', 'TRUTH_DRAGEN','TRUTH_DRAGEN_X', 'TRUTH_DRAGEN_X','QUERY_DRAGEN', 'QUERY_DRAGEN','QUERY_DRAGEN_X', 'QUERY_DRAGEN_X'), label_ypos=c(DRAGEN_summary_data$TRUTH.TP[1],	DRAGEN_summary_data$TRUTH.TP[1]+DRAGEN_summary_data$TRUTH.FN[1],	DRAGEN_X_summary_data$TRUTH.TP[1],	DRAGEN_X_summary_data$TRUTH.TP[1]+DRAGEN_X_summary_data$TRUTH.FN[1],DRAGEN_summary_data$TRUTH.TP[1],	DRAGEN_summary_data$TRUTH.TP[1]+DRAGEN_summary_data$QUERY.FP[1],	DRAGEN_X_summary_data$TRUTH.TP[1],	DRAGEN_X_summary_data$TRUTH.TP[1]+DRAGEN_X_summary_data$QUERY.FP[1]))
giab_indel_all$comparison <- factor(giab_snp_all$comparison,levels = c('TRUTH_DRAGEN', 'TRUTH_DRAGEN_X','QUERY_DRAGEN','QUERY_DRAGEN_X'))
pdf(paste0(outdir,'/','giab_DRAGEN_X_indel_all.pdf'),width=7.5,height=4.5)
ggplot(data=giab_indel_all, aes(x=comparison, y=number, fill=factor(categary, levels= c('DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP','DRAGEN_X_TRUTH_FN','DRAGEN_X_QUERY_FP','DRAGEN_X_TRUTH_TP'))))+
geom_bar(stat="identity")+
scale_fill_brewer(palette = "Set1",name='categary')+
geom_text(aes(y=label_ypos, label=number), vjust=1.6, color="black", size=3)+
theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="black", size=8, face="bold.italic",hjust = 0.5))+
ylim(0, 15000)+
ggtitle("GIAB_INDEL_ALL")
dev.off()

giab_indel_all<-data.frame(categary=c('DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'DRAGEN_X_TRUTH_TP',	'DRAGEN_X_TRUTH_FN','DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP',	'DRAGEN_X_TRUTH_TP',	'DRAGEN_X_QUERY_FP'),number=c(DRAGEN_summary_data$TRUTH.TP[3],	DRAGEN_summary_data$TRUTH.FN[3],	DRAGEN_X_summary_data$TRUTH.TP[3],	DRAGEN_X_summary_data$TRUTH.FN[3],DRAGEN_summary_data$TRUTH.TP[3],	DRAGEN_summary_data$QUERY.FP[3],	DRAGEN_X_summary_data$TRUTH.TP[3],	DRAGEN_X_summary_data$QUERY.FP[3]), comparison= c('TRUTH_DRAGEN', 'TRUTH_DRAGEN','TRUTH_DRAGEN_X', 'TRUTH_DRAGEN_X','QUERY_DRAGEN', 'QUERY_DRAGEN','QUERY_DRAGEN_X', 'QUERY_DRAGEN_X'), label_ypos=c(DRAGEN_summary_data$TRUTH.TP[1],	DRAGEN_summary_data$TRUTH.TP[1]+DRAGEN_summary_data$TRUTH.FN[1],	DRAGEN_X_summary_data$TRUTH.TP[1],	DRAGEN_X_summary_data$TRUTH.TP[1]+DRAGEN_X_summary_data$TRUTH.FN[1],DRAGEN_summary_data$TRUTH.TP[1],	DRAGEN_summary_data$TRUTH.TP[1]+DRAGEN_summary_data$QUERY.FP[1],	DRAGEN_X_summary_data$TRUTH.TP[1],	DRAGEN_X_summary_data$TRUTH.TP[1]+DRAGEN_X_summary_data$QUERY.FP[1]))
giab_indel_all$comparison <- factor(giab_snp_all$comparison,levels = c('TRUTH_DRAGEN', 'TRUTH_DRAGEN_X','QUERY_DRAGEN','QUERY_DRAGEN_X'))
pdf(paste0(outdir,'/','giab_DRAGEN_X_indel_all2.pdf'),width=4.5,height=7.5)
p<-ggplot(data=giab_indel_all, aes(x=comparison, y=number, fill=factor(categary, levels= c('DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP','DRAGEN_X_TRUTH_FN','DRAGEN_X_QUERY_FP','DRAGEN_X_TRUTH_TP'))))+
geom_bar(stat="identity")+
scale_fill_brewer(palette = "Set1",name='categary')+
geom_text(aes(y=label_ypos, label=number), vjust=1.6, color="black", size=3)+
theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="black", size=8, face="bold.italic",hjust = 0.5))+
#ylim(0, 15000)+
ggtitle("GIAB_INDEL_ALL")
gg.gap(plot=p,
       segments=c(1000,5000),
       tick_width = c(1000,500),
       rel_heights=c(0.4,0,1),
       ylim=c(0,10000)) 
dev.off()

#===========================================================
#giab_snp_pass<-data.frame(categary=c('custom_TRUTH_TP',	'custom_TRUTH_FN',	'DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'custom_TRUTH_TP',	'custom_QUERY_FP',	'DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP'),number=c(55704,	3426,	58244,	886,	55704,	3672,	58244,	4394), comparison= c('TRUTH_custom', 'TRUTH_custom','TRUTH_DRAGEN', 'TRUTH_DRAGEN','QUERY_custom', 'QUERY_custom','QUERY_DRAGEN', 'QUERY_DRAGEN'), label_ypos=c(55704,	55704+13426,	58244,	58244+10886,	55704,	55704+13672,	58244,	58244+14394))
#giab_snp_pass$comparison <- factor(giab_snp_pass$comparison,levels = c('TRUTH_custom', 'TRUTH_DRAGEN','QUERY_custom','QUERY_DRAGEN'))
#pdf('giab_custom_DRAGEN_snp_pass.pdf',width=5,height=3)
#ggplot(data=giab_snp_pass, aes(x=comparison, y=number, fill=factor(categary, levels= c('custom_TRUTH_FN','custom_QUERY_FP','custom_TRUTH_TP','DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP'))))+
#geom_bar(stat="identity")+
#scale_fill_brewer(palette = "Dark2",name='categary')+
#geom_text(aes(y=label_ypos, label=number), vjust=1.6, color="black", size=3)+
#theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="black", size=8, face="bold.italic",hjust = 0.5))+
#ylim(0, 100000)+
#ggtitle("GIAB_SNP_PASS")
#dev.off()
#
#
#giab_indel_pass<-data.frame(categary=c('custom_TRUTH_TP',	'custom_TRUTH_FN',	'DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'custom_TRUTH_TP',	'custom_QUERY_FP',	'DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP'),number=c(5762,	1124,	6266,	620,	5762,	2463,	6266,	2263), comparison= c('TRUTH_custom', 'TRUTH_custom','TRUTH_DRAGEN', 'TRUTH_DRAGEN','QUERY_custom', 'QUERY_custom','QUERY_DRAGEN', 'QUERY_DRAGEN'), label_ypos=c(5762,	5762+2124,	6266,	6266+1620,	5762,	5762+2463,	6266,	6266+2263))
#giab_indel_pass$comparison <- factor(giab_snp_pass$comparison,levels = c('TRUTH_custom', 'TRUTH_DRAGEN','QUERY_custom','QUERY_DRAGEN'))
#pdf('giab_custom_DRAGEN_indel_pass.pdf',width=5,height=3)
#ggplot(data=giab_indel_pass, aes(x=comparison, y=number, fill=factor(categary, levels= c('custom_TRUTH_FN','custom_QUERY_FP','custom_TRUTH_TP','DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP'))))+
#geom_bar(stat="identity")+
#scale_fill_brewer(palette = "Dark2",name='categary')+
#geom_text(aes(y=label_ypos, label=number), vjust=1.6, color="black", size=3)+
#theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="black", size=8, face="bold.italic",hjust = 0.5))+
#ylim(0, 15000)+
#ggtitle("GIAB_INDEL_PASS")
#dev.off()






