area = read.csv("~/Desktop/COVID-19/us data/Demographic/area.csv")
area = area[,c(1,2,3,4,15)]
area = area[which(area$Category=="Land (sq. mi.)"),]
area = area[-which(area$County=="All"),]
area = aggregate(area$X2019,by=list(area$County,area$State),FUN=sum)
area$Group.1 =as.character(area$Group.1)
area_nalas =area[grep("County",area$Group.1),]
area_nalas$Group.1 = str_sub(area_nalas$Group.1,end=-7)
area = rbind(area_nalas,area[-grep("County",area$Group.1),])
colnames(area)=c("Countynames","State","area")

demographic_clean = merge(demo,area,by=c("State","Countynames"))
demographic_clean =demographic_clean[,-3]
demographic_clean$density = demographic_clean$Population/demographic_clean$area

write.csv(demographic_clean,"/Users/chloehe/Desktop/COVID-19/clean_demo/demographic_clean.csv")
