############################## CA Vaccination Rates and PBE ####################################
#get CA vaccination data
# manually download from http://www.cdph.ca.gov/programs/immunize/Documents/2014-2015%20CA%20Child%20Care%20Data.xlsx
## convert to csv and save in working directory
### manually clear first several rows and rename variables of interest (PBE,PME,ENROLLMENT,MMR1)
year <- seq(from = 2008, to=2014, by=1)
year <- as.character(year)
vac <- list(7)
file <- "2008 CAKindergartenData.csv"
for (i in 1:length(year)){
  vac[[i]] <- read.csv(gsub("2008", year[i], file), 1)
}

#clean NA's
bad <- list(7)
for (i in 1:7){
  bad[[i]] <- is.na(vac[[i]]$ENROLLMENT)
  vac[[i]] <- vac[[i]][!bad[[i]], ]
}

#compute PBE+PME/Enrollment for each year, PME/Enrollment for each year and PBE enrollment for each year
##compute (Unvaccinated -(PBE + PME))/ Enrollment for each yaer
pbePme <- numeric(7)
pbe <- numeric(7)
pme <- numeric(7)
unexplainedUnvac <- numeric(7)
unvaccinated <- numeric(7)

for (i in 1:7){
  pbePme[i]<- (sum(vac[[i]]$PME)+sum(vac[[i]]$PBE))/sum(vac[[i]]$ENROLLMENT)
  pme[i] <- sum(vac[[i]]$PME)/sum(vac[[i]]$ENROLLMENT)
  pbe[i] <- sum(vac[[i]]$PBE)/sum(vac[[i]]$ENROLLMENT)
  unexplainedUnvac[i] <- (sum(vac[[i]]$ENROLLMENT)-sum(vac[[i]]$MMR1)-sum(vac[[i]]$PME)-sum(vac[[i]]$PBE))/sum(vac[[i]]$ENROLLMENT)
  unvaccinated[i] <- (sum(vac[[i]]$ENROLLMENT)-sum(vac[[i]]$MMR1))/sum(vac[[i]]$ENROLLMENT)
}

#create data frame of data for ggplot
data<- cbind(year,pbePme,pme,pbe,unexplainedUnvac,unvaccinated)
data <- data.frame(data)
year<-as.numeric(year)
colnames(data) <- c("Year", "PBEandPME", "PME", "PBE", "UnexplainedUnvac", "Unvaccination")

library(reshape2)
data <- melt(data, id="Year")
colnames(data)<- c("Year", "Series", "ProportionOfEnrollment")
data$Series <- factor(data$Series)
data$Year <- as.numeric(as.character(data$Year))
data$ProportionOfEnrollment <- as.numeric(data$ProportionOfEnrollment)

#plot data
png(file="vac.png", width=700, height=480, units ="px")
vacPlot <- ggplot(data, aes(x=Year, y=ProportionOfEnrollment, group=Series, color=Series))+
  geom_line()+ggtitle("CA MMR: Proportion of Kindergarten Enrollment")+xlab("Year")+ylab("Proportion")
print(vacPlot)
dev.off()
