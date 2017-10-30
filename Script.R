

df_hdi = read.csv("HDI.csv")
df_div = read.csv("diversity.csv")
df_hdi = read.csv("HDI.csv")
df_merged<- merge(df_div,df_hdi,by=c("Country"))
custom2 = (df_merged$Ethnic_Diversity_Rank>100 & df_merged$HDI.rank>125) 
custom1 = (df_merged$Ethnic_Diversity_Rank<90 & df_merged$HDI.rank<50) 
ggplot(df_merged, aes(Ethnic_Diversity_Rank,HDI.rank)) + geom_point(alpha = 0.5,color=ifelse(custom1,'blue',ifelse(custom2,'red','green'))) + geom_text(aes(label=Country),position = position_dodge(width=0.9),size = 2, hjust=1.1)+scale_colour_manual(values = c("20" = "red","20" = "blue", "1" = "green"))
df_epi = read.csv("EPI.csv")
df_total<- merge(df_merged,df_epi,by=c("Country"))

myfunction <- function(x) {
  if (x>=1 & x<=50) {
    result = "High"
  }
  else if (x>=51 & x<=100) {
    result = "Medium"
  }
  else{
    result = "Low"
  }
  return(result)
}
df_total$Environment_Performance <- as.character(lapply(df_total$EPI_Rank,myfunction))
myColors <- c("blue", "red", "green")
ggplot(df_total, aes(Ethnic_Diversity_Rank,HDI.rank,col=as.factor(Environment_Performance)))+geom_point()+ geom_text(aes(label=Country),color = "black",position = position_dodge(width=0.9),size = 2, hjust=1.1)+scale_color_manual(values=myColors)

