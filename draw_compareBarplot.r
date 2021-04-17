library(ggplot2)
library(gg.gap)
library("RColorBrewer")

giab_snp_all<-data.frame(categary=c('custom_TRUTH_TP',	'custom_TRUTH_FN',	'DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'DRAGEN_callable_TRUTH_TP',	'DRAGEN_callable_TRUTH_FN', 'custom_TRUTH_TP',	'custom_QUERY_FP',	'DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP',	'DRAGEN_callable_TRUTH_TP',	'DRAGEN_callable_QUERY_FP'),number=c(58277,	853,	58371,	759,	57658,	1472, 58277,	9420,	58371,	4937, 57658, 4495), comparison= c('TRUTH_custom', 'TRUTH_custom','TRUTH_DRAGEN', 'TRUTH_DRAGEN','TRUTH_DRAGEN_callable', 'TRUTH_DRAGEN_callable','QUERY_custom', 'QUERY_custom','QUERY_DRAGEN', 'QUERY_DRAGEN','QUERY_DRAGEN_callable', 'QUERY_DRAGEN_callable'), label_ypos=c(58277, 58277+10853, 58371, 58371+10759,57658,	57658 + 11472, 58277,58277+19420, 58371,58371+14937, 57658, 57658 + 14495))
giab_snp_all$comparison <- factor(giab_snp_all$comparison,levels = c('TRUTH_custom', 'TRUTH_DRAGEN', 'TRUTH_DRAGEN_callable','QUERY_custom','QUERY_DRAGEN','QUERY_DRAGEN_callable'))
pdf('giab_custom_DRAGEN_callable_snp_all.pdf',width=7.5,height=4.5)
ggplot(data=giab_snp_all, aes(x=comparison, y=number, fill=factor(categary, levels= c('custom_TRUTH_FN','custom_QUERY_FP','custom_TRUTH_TP','DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP','DRAGEN_callable_TRUTH_FN','DRAGEN_callable_QUERY_FP','DRAGEN_callable_TRUTH_TP'))))+
geom_bar(stat="identity")+
scale_fill_brewer(palette = "Set1",name='categary')+
geom_text(aes(y=label_ypos, label=number), vjust=1.6, color="black", size=3)+
theme( axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="black", size=8, face="bold.italic",hjust = 0.5))+
#ylim(0, 100000)+
ggtitle("GIAB_SNP_ALL")       
dev.off()

giab_snp_all<-data.frame(categary=c('custom_TRUTH_TP',	'custom_TRUTH_FN',	'DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'DRAGEN_callable_TRUTH_TP',	'DRAGEN_callable_TRUTH_FN', 'custom_TRUTH_TP',	'custom_QUERY_FP',	'DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP',	'DRAGEN_callable_TRUTH_TP',	'DRAGEN_callable_QUERY_FP'),number=c(58277,	853,	58371,	759,	57658,	1472, 58277,	9420,	58371,	4937, 57658, 4495), comparison= c('TRUTH_custom', 'TRUTH_custom','TRUTH_DRAGEN', 'TRUTH_DRAGEN','TRUTH_DRAGEN_callable', 'TRUTH_DRAGEN_callable','QUERY_custom', 'QUERY_custom','QUERY_DRAGEN', 'QUERY_DRAGEN','QUERY_DRAGEN_callable', 'QUERY_DRAGEN_callable'), label_ypos=c(58277, 58277+853, 58371, 58371+759,57658,	57658 + 1472, 58277,58277+9420, 58371,58371+4937, 57658, 57658 + 4495))
giab_snp_all$comparison <- factor(giab_snp_all$comparison,levels = c('TRUTH_custom', 'TRUTH_DRAGEN', 'TRUTH_DRAGEN_callable','QUERY_custom','QUERY_DRAGEN','QUERY_DRAGEN_callable'))
pdf('giab_custom_DRAGEN_callable_snp_all2.pdf',width=4.5,height=7.5)
p<-ggplot(data=giab_snp_all, aes(x=comparison, y=number, fill=factor(categary, levels= c('custom_TRUTH_FN','custom_QUERY_FP','custom_TRUTH_TP','DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP','DRAGEN_callable_TRUTH_FN','DRAGEN_callable_QUERY_FP','DRAGEN_callable_TRUTH_TP'))))+
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


giab_indel_all<-data.frame(categary=c('custom_TRUTH_TP',	'custom_TRUTH_FN',	'DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'DRAGEN_callable_TRUTH_TP',	'DRAGEN_callable_TRUTH_FN', 'custom_TRUTH_TP',	'custom_QUERY_FP',	'DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP',	'DRAGEN_callable_TRUTH_TP',	'DRAGEN_callable_QUERY_FP'),number=c(6040,	846,	6305,	581,	6264,	622, 6040,	3397,	6305,	2518, 6264,	3199), comparison= c('TRUTH_custom', 'TRUTH_custom','TRUTH_DRAGEN', 'TRUTH_DRAGEN','TRUTH_DRAGEN_callable', 'TRUTH_DRAGEN_callable','QUERY_custom', 'QUERY_custom','QUERY_DRAGEN', 'QUERY_DRAGEN','QUERY_DRAGEN_callable', 'QUERY_DRAGEN_callable'), label_ypos=c(6040,	6040+1846,	6305,	6305+1581,6264,6264+1622,	6040,	6040+3397,	6305,	6305+2518, 6264,	6264+3199))
giab_indel_all$comparison <- factor(giab_snp_all$comparison,levels = c('TRUTH_custom', 'TRUTH_DRAGEN', 'TRUTH_DRAGEN_callable','QUERY_custom','QUERY_DRAGEN','QUERY_DRAGEN_callable'))
pdf('giab_custom_DRAGEN_indel_all.pdf',width=7.5,height=4.5)
ggplot(data=giab_indel_all, aes(x=comparison, y=number, fill=factor(categary, levels= c('custom_TRUTH_FN','custom_QUERY_FP','custom_TRUTH_TP','DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP','DRAGEN_callable_TRUTH_FN','DRAGEN_callable_QUERY_FP','DRAGEN_callable_TRUTH_TP'))))+
geom_bar(stat="identity")+
scale_fill_brewer(palette = "Set1",name='categary')+
geom_text(aes(y=label_ypos, label=number), vjust=1.6, color="black", size=3)+
theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="black", size=8, face="bold.italic",hjust = 0.5))+
ylim(0, 15000)+
ggtitle("GIAB_INDEL_ALL")
dev.off()

giab_indel_all<-data.frame(categary=c('custom_TRUTH_TP',	'custom_TRUTH_FN',	'DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'DRAGEN_callable_TRUTH_TP',	'DRAGEN_callable_TRUTH_FN', 'custom_TRUTH_TP',	'custom_QUERY_FP',	'DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP',	'DRAGEN_callable_TRUTH_TP',	'DRAGEN_callable_QUERY_FP'),number=c(6040,	846,	6305,	581,	6264,	622, 6040,	3397,	6305,	2518, 6264,	3199), comparison= c('TRUTH_custom', 'TRUTH_custom','TRUTH_DRAGEN', 'TRUTH_DRAGEN','TRUTH_DRAGEN_callable', 'TRUTH_DRAGEN_callable','QUERY_custom', 'QUERY_custom','QUERY_DRAGEN', 'QUERY_DRAGEN','QUERY_DRAGEN_callable', 'QUERY_DRAGEN_callable'), label_ypos=c(6040,	6040+846,	6305,	6305+581,6264,6264+622,	6040,	6040+3397,	6305,	6305+2518, 6264,	6264+3199))
giab_indel_all$comparison <- factor(giab_snp_all$comparison,levels = c('TRUTH_custom', 'TRUTH_DRAGEN', 'TRUTH_DRAGEN_callable','QUERY_custom','QUERY_DRAGEN','QUERY_DRAGEN_callable'))
pdf('giab_custom_DRAGEN_indel_all2.pdf',width=4.5,height=7.5)
p<-ggplot(data=giab_indel_all, aes(x=comparison, y=number, fill=factor(categary, levels= c('custom_TRUTH_FN','custom_QUERY_FP','custom_TRUTH_TP','DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP','DRAGEN_callable_TRUTH_FN','DRAGEN_callable_QUERY_FP','DRAGEN_callable_TRUTH_TP'))))+
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

===========================================================
giab_snp_pass<-data.frame(categary=c('custom_TRUTH_TP',	'custom_TRUTH_FN',	'DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'custom_TRUTH_TP',	'custom_QUERY_FP',	'DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP'),number=c(55704,	3426,	58244,	886,	55704,	3672,	58244,	4394), comparison= c('TRUTH_custom', 'TRUTH_custom','TRUTH_DRAGEN', 'TRUTH_DRAGEN','QUERY_custom', 'QUERY_custom','QUERY_DRAGEN', 'QUERY_DRAGEN'), label_ypos=c(55704,	55704+13426,	58244,	58244+10886,	55704,	55704+13672,	58244,	58244+14394))
giab_snp_pass$comparison <- factor(giab_snp_pass$comparison,levels = c('TRUTH_custom', 'TRUTH_DRAGEN','QUERY_custom','QUERY_DRAGEN'))
pdf('giab_custom_DRAGEN_snp_pass.pdf',width=5,height=3)
ggplot(data=giab_snp_pass, aes(x=comparison, y=number, fill=factor(categary, levels= c('custom_TRUTH_FN','custom_QUERY_FP','custom_TRUTH_TP','DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP'))))+
geom_bar(stat="identity")+
scale_fill_brewer(palette = "Dark2",name='categary')+
geom_text(aes(y=label_ypos, label=number), vjust=1.6, color="black", size=3)+
theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="black", size=8, face="bold.italic",hjust = 0.5))+
ylim(0, 100000)+
ggtitle("GIAB_SNP_PASS")
dev.off()


giab_indel_pass<-data.frame(categary=c('custom_TRUTH_TP',	'custom_TRUTH_FN',	'DRAGEN_TRUTH_TP',	'DRAGEN_TRUTH_FN',	'custom_TRUTH_TP',	'custom_QUERY_FP',	'DRAGEN_TRUTH_TP',	'DRAGEN_QUERY_FP'),number=c(5762,	1124,	6266,	620,	5762,	2463,	6266,	2263), comparison= c('TRUTH_custom', 'TRUTH_custom','TRUTH_DRAGEN', 'TRUTH_DRAGEN','QUERY_custom', 'QUERY_custom','QUERY_DRAGEN', 'QUERY_DRAGEN'), label_ypos=c(5762,	5762+2124,	6266,	6266+1620,	5762,	5762+2463,	6266,	6266+2263))
giab_indel_pass$comparison <- factor(giab_snp_pass$comparison,levels = c('TRUTH_custom', 'TRUTH_DRAGEN','QUERY_custom','QUERY_DRAGEN'))
pdf('giab_custom_DRAGEN_indel_pass.pdf',width=5,height=3)
ggplot(data=giab_indel_pass, aes(x=comparison, y=number, fill=factor(categary, levels= c('custom_TRUTH_FN','custom_QUERY_FP','custom_TRUTH_TP','DRAGEN_TRUTH_FN','DRAGEN_QUERY_FP','DRAGEN_TRUTH_TP'))))+
geom_bar(stat="identity")+
scale_fill_brewer(palette = "Dark2",name='categary')+
geom_text(aes(y=label_ypos, label=number), vjust=1.6, color="black", size=3)+
theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(color="black", size=8, face="bold.italic",hjust = 0.5))+
ylim(0, 15000)+
ggtitle("GIAB_INDEL_PASS")
dev.off()






