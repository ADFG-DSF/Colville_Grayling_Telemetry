##Colville Arctic Grayling Telemetry 2019-2022##===========================
#April Behr
#December 26, 2023

#Setwd()
setwd("C:/Users/aebehr/Documents/REPORTS/RESIDENT SPECIES/Colville GR 2019-2022/Colville_analysis_ab")
wd <- getwd()


#Define location of data--------------------------------------------------------
dir.data <- file.path(wd, "data")
dir.data


#Define location of analysis output --------------------------------------------
dir.analysis <- file.path(wd, "analysis output")
dir.analysis


#Required packages--------------------------------------------------------------
library(riverdist)   # for spatial river network analysis
library(tidyverse)   # for streamlined data manipulation
library(sp)
library(xlsx)


#Import flight data-------------------------------------------------------------
flight_final <- read.csv(file.path(dir.data,"Colville_data.csv"))
head(flight_final)
summary(flight_final)
flight_final$Freq_Code <- as.factor(as.character(flight_final$Freq_Code))
str(flight_final)
head(flight_final)

#Define date
flight_final$Date <- as.Date(flight_final$Date, format = '%m/%d/%Y')
str(flight_final)
head(flight_final)

#Eliminate inactive tags
flight_data <- flight_final %>%
  filter(Use == "1") %>% #select where use = 1
  filter(Survey!=1) %>% #remove survey 1 because not all tags were deployed
  filter(Survey!=5) #remove survey 5 because it does not correspond to a season of interest
head(flight_data)
str(flight_data)
unique(flight_data$Survey)
unique(flight_data$Use)


#Import river shapefile---------------------------------------------------------
Colville1 <- line2network(path = dir.data, layer="Colville",reproject= "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154
     +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0", tolerance=50)
plot(Colville1)  

#Cleanup river shapefile and save it
Colville2 <- cleanup(Colville1)
#Dissolve Y
#Split segments Y
#Insert vertices... Y
#Minimum distance 50
#Segment of mouth xx
#Vertex of mouth xx
#Accept mouth Y
#Remove additional segments N
#Build segment routes y

#Check for connectedness
topologydots(Colville2)
#zoomtoseg(seg = c(25,4,3), rivers = Colville2)

#Save river shape
save(Colville2, file = "Colville2.rda")

#Load saved river shape
load(file="Colville2.rda")  


#Project flight data and plot it------------------------------------------------
AKalbers <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154
    +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

telem_albers <- flight_data %>% 
  select(c("Longitude", "Latitude")) %>%    # extracting coord matrix
  sf::sf_project(pts=., to=AKalbers) %>%    # re-projecting to Alaska Albers
  as.data.frame                             # make it a data.frame
colnames(telem_albers) <- c("AlbersX","AlbersY")
head(telem_albers)

plot(Colville2)
points(telem_albers)


#Convert tagging and aerial survey xy data to river locations-------------------
data_segvert <- xy2segvert(x=telem_albers[,1],y=telem_albers[,2],rivers=Colville2)
plot(Colville2)
riverpoints(data_segvert$seg,data_segvert$vert,Colville2)

#Trim river network to data-----------------------------------------------------
head(data_segvert)
head(Colville2)

#identify which river segments have fish data
segstokeep <- list()
uniquesegs <- unique(data_segvert$seg)
for(i in 1:length(uniquesegs)) {
  segstokeep[[i]] <- detectroute(uniquesegs[i],Colville2$mouth$mouth.seg,Colville2)
}
segstokeep <- unique(unlist(segstokeep))

#trimming river to only segments with fish data
Colville3 <- trimriver(trimto=segstokeep,rivers=Colville2)
plot(Colville3)
Colville4 <- dissolve(Colville3)
plot(Colville4)


#Assigning river locations based on new river shape-----------------------------
data_segvert <- xy2segvert(x=telem_albers[,1],y=telem_albers[,2],rivers=Colville4)
flight_data$seg <- data_segvert$seg
flight_data$vert <- data_segvert$vert

plot(Colville4)
riverpoints(flight_data$seg,flight_data$vert,Colville4)
length(unique(flight_data$Survey))

#Locations by survey------------------------------------------------------------
dates <- sort(unique(flight_data$Date))
dates
surveys <- unique(flight_data$Survey)
surveys
survey_period <- unique(flight_data$Survey_Period)
survey_period
# Note: dates and surveys need to be the same length and in the same order

jpeg(file.path(dir.analysis,"locations by survey.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mfrow=c(2,5), mar=c(3,2,3,1))
j <- 1 #index for dates so it pulls from position 1 to start
for(i in surveys) {
    plot(Colville4,empty=T,main=survey_period[j],linecol="grey")
    riverpoints(flight_data$seg[flight_data$Survey==i],flight_data$vert[flight_data$Survey==i],Colville4,jitter=500)
    j <- j + 1
}
dev.off()


#Simple density by survey (regions with >=n points in x distance)---------------
dens1 <- makeriverdensity(flight_data$seg,flight_data$vert,Colville4,survey=flight_data$Survey,kernel="rect",bw=2000)
names(dens1)
str(dens1$densities)
length(dens1$densities)
str(dens1$endptverts)

dev.off()
plotsimple <- function(dens1,n,lwdplot=5) {
  surv <- sort(unique(dens1$survey))
  for(i in 1:length(dens1$densities)) {
    plot(dens1$rivers, empty=T, segmentnum=F, main=paste0(survey_period[i]," (n=",sum(dens1$survey==surv[i]),")"), linecol="grey")   
    for(segi in 1:length(dens1$densities[[i]])) {
      for(verti in 1:length(dens1$densities[[i]][[segi]])) {
        vert1 <- dens1$endptverts[[segi]][verti]
        vert2 <- dens1$endptverts[[segi]][verti+1]
        if(dens1$densities[[i]][[segi]][[verti]]>=n) lines(dens1$rivers$lines[[segi]][vert1:vert2,],lwd=lwdplot,lend=1, col=1)
      }
    }
    riverpoints(dens1$pointsegs[dens1$survey==surv[i]], dens1$pointverts[dens1$survey==surv[i]], dens1$rivers, pch=21, bg=0)
  }
}
jpeg(file.path(dir.analysis,"density by survey.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mfrow=c(2,5),mar=c(3,2,3,1))
plotsimple(dens1,n=5,lwdplot=10)
dev.off()


#Mouth distance plots-----------------------------------------------------------
#by survey
mdist <- 0.001*mouthdistbysurvey(unique=flight_data$Freq_Code, survey=flight_data$Survey, seg=flight_data$seg, vert=flight_data$vert, rivers=Colville4)
par(mfrow=c(1,1))
plotseq(mdist)
plotseq(mdist,type="dotline")

#by date
mdist2 <- 0.001*mouthdistbysurvey(unique=flight_data$Freq_Code, survey=flight_data$Date, seg=flight_data$seg, vert=flight_data$vert, rivers=Colville4)
par(mfrow=c(1,1))
plotseq(mdist2,surveysareDates = T, type = "dotline")

#function magic from Tyers's Gulkana code
#Creates a plot function that spaces the x axis proportional to time elapsed between dates.
plotseq1 <- function (seqbysurvey, type = "boxplot", xlab = "", ylab = "", 
                      main = "", cex.axisX = 0.8, lowerbound = NULL, upperbound = NULL, 
                      boundtype = "negative", surveysareDates = F, xax = NULL, x45=F, x45y=0, x45x,...) 
{
  if (surveysareDates) 
    xplot <- as.Date(names(seqbysurvey))
  if (!surveysareDates) 
    xplot <- 1:(dim(seqbysurvey)[2])
  if (is.numeric(seqbysurvey[1, 1])) {
    plot(NA, xlim = c(xplot[1], xplot[length(xplot)]), ylim = c(min(seqbysurvey, 
                                                                    na.rm = T), max(seqbysurvey, na.rm = T)), xaxt = "n", 
         xlab = xlab, ylab = ylab, main = main, ... = ...)
    if (!is.null(lowerbound) & !is.null(upperbound)) {
      if (boundtype == "negative") {
        polygon(x = c(xplot[1], xplot, xplot[length(xplot)]), 
                y = c(par("usr")[3], lowerbound, par("usr")[3]), 
                col = "grey90", border = NA)
        polygon(x = c(xplot[1], xplot, xplot[length(xplot)]), 
                y = c(par("usr")[4], upperbound, par("usr")[4]), 
                col = "grey90", border = NA)
        lines(par("usr")[1:2], par("usr")[c(4, 4)])
      }
      if (boundtype == "positive") {
        polygon(x = c(xplot, xplot[(length(xplot)):1]), 
                y = c(lowerbound, (upperbound[(length(upperbound)):1])), 
                col = "grey90", border = NA)
      }
      if (boundtype == "lines") {
        del <- 0.4 * min(xplot[2:length(xplot)] - xplot[1:(length(xplot) - 
                                                             1)])
        for (i in 1:length(lowerbound)) {
          lines(xplot[i] + c(-1, 1) * del, rep(lowerbound[i], 
                                               2), lwd = 2)
          lines(xplot[i] + c(-1, 1) * del, rep(upperbound[i], 
                                               2), lwd = 2)
        }
      }
    }
    if (type == "dotline" | type == "boxline") {
      for (i in 1:(dim(seqbysurvey)[1])) {
        lines(xplot[!is.na(seqbysurvey[i, ])], seqbysurvey[i, 
        ][!is.na(seqbysurvey[i, ])], col = "grey60", 
        lty = 3)
        lines(xplot, seqbysurvey[i, ], col = "grey30")
      }
    }
    for (i in 1:(dim(seqbysurvey)[2])) {
      if ((type == "boxplot" | type == "boxline") & !all(is.na(seqbysurvey[, 
                                                                           i]))) 
        boxplot(seqbysurvey[, i], at = xplot[i], add = T, 
                yaxt = "n", col = NA)
      if (type == "dotplot") 
        points(jitter(rep(xplot[i], (dim(seqbysurvey)[1])), 
                      amount = 0.1), seqbysurvey[, i])
      if (type == "dotline") 
        points(rep(xplot[i], (dim(seqbysurvey)[1])), 
               seqbysurvey[, i])
    }
    if(is.null(xax)) xax <- names(seqbysurvey)
    if(!x45) axis(side = 1, at = xplot, labels = xax, 
                  cex.axis = cex.axisX, las = 2)
    if(x45) {
      axis(side = 1, at = xplot, labels=NA)
      text(xplot+x45x, par("usr")[3]+x45y, labels = xax, srt = 45, pos = 2, xpd = TRUE, cex=cex.axisX)
    }
  }
  if (is.character(seqbysurvey[1, 1]) | is.factor(seqbysurvey[1, 
                                                              1])) {
    stop("Plotting methods do not yet exist for matrices returned from riverdirectionseq().")
  }
}

#print plots
#by survey
jpeg(file.path(dir.analysis,"seqplot1.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mar=c(4,4,1,1))
plotseq(mdist,type="dotline",surveysareDates = F, ylab="River km", xlab="Survey")
dev.off()

#by date
jpeg(file.path(dir.analysis,"seqplot2.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mfrow=c(1,1))
plotseq(mdist2,type="dotline", surveysareDates = T, ylab="River km")
dev.off()

#dates in mmddyy format for the graph
dates_mmddyy <- format(dates, "%m/%d/%Y")
unique(dates_mmddyy)

jpeg(file.path(dir.analysis,"seqplot_mmddyy.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mar=c(6,3.8,1,1))
plotseq1(mdist2,type="dotline",surveysareDates = T, xax=dates_mmddyy, cex.axisX=.7)#, x45=T, x45y=-5, x45x=5)
mtext("Survey", side=1, line=4, outer = FALSE)
mtext("River km", side=2, line=2.5, outer = FALSE)
dev.off()

#survey periods for the graph
survey_period <-unique(flight_data$Survey_Period)

jpeg(file.path(dir.analysis,"seqplot_survey_period.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mar=c(6,3.8,1,1))
plotseq1(mdist2,type="dotline",surveysareDates = T, xax=survey_period, cex.axisX=.7)#, x45=T, x45y=-5, x45x=5)
mtext("Survey", side=1, line=4, outer = FALSE)
mtext("River km", side=2, line=2.5, outer = FALSE)
dev.off()

#Summary stats by survey
get_n <- function(x) sum(!is.na(x))
pos_summary <- rbind(apply(mdist, 2, mean, na.rm=T),
                     apply(mdist, 2, median, na.rm=T),
                     apply(mdist, 2, sd, na.rm=T),
                     apply(mdist, 2, min, na.rm=T),
                     apply(mdist, 2, max, na.rm=T),
                     apply(mdist, 2, get_n))
row.names(pos_summary) <- c("mean","median","sd","min","max","n")


#River positions and stats to .xlsx---------------------------------------------
write.xlsx(mdist, file.path(dir.analysis,"Colville_grayling_tabs.xlsx"), sheetName="position_km")
write.xlsx(pos_summary, file.path(dir.analysis,"Colville_grayling_tabs.xlsx"), sheetName="position_km_summary", append=T)
--------------------------------------------------------------------------------
  
  
#Upstream tables----------------------------------------------------------------
useq <- 0.001*upstreamseq(unique=flight_data$Freq_Code, survey=flight_data$Survey, seg=flight_data$seg, vert=flight_data$vert, rivers=Colville4)
useq1 <- 0.001*upstreamseq(unique=flight_data$Freq_Code, survey=flight_data$Date, seg=flight_data$seg, vert=flight_data$vert, rivers=Colville4)

#General plot
par(family="serif", mfrow=c(2,1))
plotseq(useq, xlab="Survey", ylab="Distance traveled (km)")
abline(h=0, lty=2)
plotseq(useq,type="dotline", xlab="Survey", ylab="Distance traveled (km)")

#Box plot
#by date
jpeg(file.path(dir.analysis,"sequential_surveys1_mmddyy.jpg"),width=15,height=7,units="in",res=300)
par(family="serif")
plotseq(useq, xlab="Survey", ylab="Distance traveled (km)")
par(las=1, mar=c(4,4,1,1))
boxplot(useq1, xlab="", ylab="Distance traveled (km)",xaxt='n')
axis(1,at=1:ncol(useq), labels=NA)
text(x=1:ncol(useq), y=par("usr")[3]-4, labels=paste0(dates_mmddyy[-(ncol(useq)+1)],"\n to \n",dates_mmddyy[-1]), xpd=T, pos=1)
abline(h=0, lty=2)
dev.off()

#by survey
jpeg(file.path(dir.analysis,"sequential_surveys2.jpg"),width=15,height=7,units="in",res=300)
par(family="serif")
plotseq(useq, xlab="Survey", ylab="Distance traveled (km)")
par(las=2, mar=c(6,4,1,1))
boxplot(useq, ylab="Distance traveled (km)", xlab="Survey")
abline(h=0, lty=2)
dev.off()


#by survey period
jpeg(file.path(dir.analysis,"sequential_surveys1_survey_period.jpg"),width=15,height=7,units="in",res=300)
par(family="serif",cex = 1.2)
plotseq(useq, xlab="Survey", ylab="Distance traveled (RKM)")
par(las=1, mar=c(4,4,1,1))
boxplot(useq1, xlab="", ylab="Distance traveled (RKM)",xaxt='n',cex = 1.3)
axis(1,at=1:ncol(useq), labels=NA)
text(x=1:ncol(useq), y=par("usr")[3]-4, labels=paste0(survey_period[-(ncol(useq)+1)],"\n to \n",survey_period[-1]), xpd=T, pos=1)
abline(h=0, lty=2)
dev.off()

#Export distance between surveys to Excel---------------------------------------
write.xlsx(useq, file.path(dir.analysis,"Colville_grayling_tabs.xlsx"), sheetName="sequential_surveys", append=T)
--------------------------------------------------------------------------------
  
    
#Summary stats for upstream and downstream movement-----------------------------
dev.off()
apply(useq,2,mean,na.rm=T)
apply(useq,2,sd,na.rm=T)
plot(apply(useq,2,mean,na.rm=T))
abline(h=0, lty=2)

#all dates
d0 <- sort(unique(flight_data$Date))
#minus the last survey
d1 <- d0[-length(d0)]
#minus tagging
d2 <- d0[-1]

#all surveys
s0 <- sort(unique(flight_data$Survey))
#minus the last survey
s1 <- s0[-length(s0)]
#minus tagging
s2 <- s0[-1]

t1 <- paste(s1,s2)
t2 <- as.numeric(d2-d1)
t3 <- apply(useq,2,function(x) sum(!is.na(x)))
t4 <- round(apply(abs(useq),2,mean,na.rm=T),1)
t5 <- round(apply(abs(useq),2,sd,na.rm=T),1)
t6 <- round(apply(useq,2,mean,na.rm=T),1)
t7 <- round(apply(useq,2,sd,na.rm=T),1)
t8 <- round(apply(useq,2,function(x) max(x[x>0],na.rm=T)),1)
t9 <- round(apply(useq,2,function(x) min(x[x>0],na.rm=T)),1)
t10 <- round(apply(useq,2,function(x) -min(x[x<0],na.rm=T)),1)
t11 <- round(apply(useq,2,function(x) -max(x[x<0],na.rm=T)),1)

movement_summary1 <- data.frame(Days_betw_surv=t2, N=t3, Abs_mn=t4, Abs_sd=t5, Dir_mn=t6, Dir_sd=t7, Upst_max=t8, Upst_min=t9, Downst_max=t10, Downst_min=t11, row.names=t1)
movement_summary1


#Write summary stats to .xlsx---------------------------------------------------
write.xlsx(movement_summary1, file.path(dir.analysis,"Colville_grayling_tabs.xlsx"), sheetName="movement_summary1", append=T)
#-------------------------------------------------------------------------------


#Movement-----------------------------------------------------------------------
t12 <- colSums(useq>0,na.rm=T)
t13 <- round(t12/t3,2)
t14 <- colSums(useq<=0,na.rm=T)
t15 <- round(t14/t3,2)

movement_summary2 <- data.frame(Days_betw_surv=t2, Number_upstream=t12, Prop_upstream=t13, Number_downstream=t14, Prop_downstream=t15, row.names=t1)
movement_summary2


#Movement summary to .xlsx------------------------------------------------------
write.xlsx(movement_summary2, file.path(dir.analysis,"Colville_grayling_tabs.xlsx"), sheetName="movement_summary2", append=T)
#-------------------------------------------------------------------------------


#Home ranges--------------------------------------------------------------------
hr <- homerange(unique=flight_data$Freq_Code, survey=flight_data$Survey, seg=flight_data$seg, vert=flight_data$vert, rivers=Colville4)


#Are home ranges different for the different tagging areas?---------------------
tagging_loc <- flight_data %>% filter(Use=="1", Survey=="0") %>%
  select(Freq_Code,Tagging_Location)  #selecting where each tag was deployed 
hr2 <- data.frame(hr$ranges) %>% 
  left_join(tagging_loc, by = c("ID"="Freq_Code"))  #adding tagging location to hr
hr2$Tagging_Location <- as.factor(hr2$Tagging_Location) #make tagging location a factor so I can graph it
hr2$Tagging_Location_Cond <- as.factor(ifelse(hr2$Tagging_Location == "Nuiqsut", "Nuiqsut", "Not_Nuiqsut"))
str(hr2)

hr2_Nuiqsut <- hr2[hr2$Tagging_Location_Cond == "Nuiqsut",]
hr2_Not_Nuiqsut <- hr2[hr2$Tagging_Location_Cond != "Nuiqsut",]

dev.off()
plot(hr2$range ~ hr2$Tagging_Location_Cond, data = hr2)
hist(hr2_Nuiqsut$range)
hist(hr2_Not_Nuiqsut$range) #doesn't look normally distributed

shapiro.test(hr2_Nuiqsut$range)
shapiro.test(hr2_Not_Nuiqsut$range) #p-value<0.05, reject, data are not normally distributed

test <- wilcox.test(hr2$range ~ hr2$Tagging_Location_Cond)
test #FTR, not statistically different


#Home range summary by tagging area and for all fish----------------------------
All_hr <- c(sum(!is.na(hr2$range)),
            round(mean(hr2$range, na.rm = T)/1000, digits = 2),
            round(sd(hr2$range, na.rm = T)/1000, digits = 2),
            round((sd(hr2$range, na.rm = T)/1000)/sqrt(sum(!is.na(hr2$range))), digits = 2),
            round(min(hr2$range, na.rm = T)/1000, digits = 2),
            round(max(hr2$range, na.rm = T)/1000, digits = 2))

Nuiqsut_hr <- c(sum(!is.na(hr2_Nuiqsut$range)),
              round(mean(hr2_Nuiqsut$range, na.rm = T)/1000, digits = 2),
              round(sd(hr2_Nuiqsut$range, na.rm = T)/1000, digits = 2),
              round((sd(hr2_Nuiqsut$range, na.rm = T)/1000)/sqrt(sum(!is.na(hr2_Nuiqsut$range))), digits = 2),
              round(min(hr2_Nuiqsut$range, na.rm = T)/1000, digits = 2),
              round(max(hr2_Nuiqsut$range, na.rm = T)/1000, digits = 2))

Not_Nuiqsut_hr <- c(sum(!is.na(hr2_Not_Nuiqsut$range)),
                  round(mean(hr2_Not_Nuiqsut$range, na.rm = T)/1000, digits = 2),
                  round(sd(hr2_Not_Nuiqsut$range, na.rm = T)/1000, digits = 2),
                  round((sd(hr2_Not_Nuiqsut$range, na.rm = T)/1000)/sqrt(sum(!is.na(hr2_Not_Nuiqsut$range))), digits = 2),
                  round(min(hr2_Not_Nuiqsut$range, na.rm = T)/1000, digits = 2),
                  round(max(hr2_Not_Nuiqsut$range, na.rm = T)/1000, digits = 2))

homeranges_by_tagging_loc <- data.frame(rbind(Nuiqsut_hr, Not_Nuiqsut_hr, All_hr))
colnames(Table_5) <- c("n", "mean", "sd", "se", "min", "max")
homeranges_by_tagging_loc

#Home ranges to .xlsx-----------------------------------------------------------
write.xlsx(hr$ranges, file.path(dir.analysis, "Colville_grayling_tabs.xlsx"), sheetName="all_homeranges", append=T)
write.xlsx(homeranges_by_tagging_loc, file.path(dir.analysis, "Colville_grayling_tabs.xlsx"), sheetName="homeranges_by_tagging_loc", append=T)
#-------------------------------------------------------------------------------


#Home ranges, detected in each unique pairing of surveys and in the ranges described
hr_pairing <- function(x,a,b) {
  theseones <- intersect(x$Freq_Code[x$Survey==a], x$Freq_Code[x$Survey==b])
  hr1 <- with(subset(x, Freq_Code %in% theseones & Survey %in% a:b), homerange(unique=Freq_Code, survey=Survey, seg=seg, vert=vert, rivers=Colville4))$ranges
  toreturn <- data.frame(n=nrow(hr1),
                         mean=0.001*mean(hr1$range, na.rm=T),
                         sd=0.001*sd(hr1$range, na.rm=T),
                         max=0.001*max(hr1$range, na.rm=T),
                         min=0.001*min(hr1$range, na.rm=T))
  return(toreturn)
}

#Change a and b below to reflect the surveys to summarize
sdates <- sort(unique(flight_data$Date))
surveys
hr_table <- rbind(hr_pairing(x=flight_data, a=2, b=6),
                  hr_pairing(x=flight_data, a=6, b=9),
                  hr_pairing(x=flight_data, a=2, b=4),
                  hr_pairing(x=flight_data, a=6, b=8))
row.names(hr_table) <- c("Winter 2020 - Winter 2021",
                         "Winter 2021 - Winter 2022",
                         "Winter 2020 - Summer 2020",
                         "Winter 2021 - Summer 2021")

#Save home range summary to .xlsx------------------------------------------------ 
write.xlsx(hr_table, file.path(dir.analysis, "Colville_grayling_tabs.xlsx"), sheetName="hr_table", append=T)
#-------------------------------------------------------------------------------







