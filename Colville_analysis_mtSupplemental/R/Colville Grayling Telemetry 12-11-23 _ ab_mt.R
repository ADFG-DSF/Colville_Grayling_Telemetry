##Colville Arctic Grayling Telemetry 2019-2022##===========================
#April Behr
#December 26, 2023

#Setwd()
# setwd("C:/Users/aebehr/Documents/REPORTS/RESIDENT SPECIES/Colville GR 2019-2022/Colville_analysis_ab")
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


# #Import river shapefile---------------------------------------------------------
# Colville1 <- line2network(path = dir.data, layer="Colville",reproject= "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154
#      +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0", tolerance=50)
# plot(Colville1)
#
# #Cleanup river shapefile and save it
# Colville2 <- cleanup(Colville1)
# #Dissolve Y
# #Split segments Y
# #Insert vertices... Y
# #Minimum distance 50
# #Segment of mouth xx
# #Vertex of mouth xx
# #Accept mouth Y
# #Remove additional segments N
# #Build segment routes y
#
# #Check for connectedness
# topologydots(Colville2)
# #zoomtoseg(seg = c(25,4,3), rivers = Colville2)
#
# #Save river shape
# save(Colville2, file = "Colville2.rda")

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

# jpeg(file.path(dir.analysis,"locations by survey.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mfrow=c(2,5), mar=c(3,2,3,1))
j <- 1 #index for dates so it pulls from position 1 to start
for(i in surveys) {
    plot(Colville4,empty=T,main=survey_period[j],linecol="grey")
    riverpoints(flight_data$seg[flight_data$Survey==i],flight_data$vert[flight_data$Survey==i],Colville4,jitter=500)
    j <- j + 1
}
# dev.off()


# #Simple density by survey (regions with >=n points in x distance)---------------
# dens1 <- makeriverdensity(flight_data$seg,flight_data$vert,Colville4,survey=flight_data$Survey,kernel="rect",bw=2000)
# names(dens1)
# str(dens1$densities)
# length(dens1$densities)
# str(dens1$endptverts)
#
# dev.off()
# plotsimple <- function(dens1,n,lwdplot=5) {
#   surv <- sort(unique(dens1$survey))
#   for(i in 1:length(dens1$densities)) {
#     plot(dens1$rivers, empty=T, segmentnum=F, main=paste0(survey_period[i]," (n=",sum(dens1$survey==surv[i]),")"), linecol="grey")
#     for(segi in 1:length(dens1$densities[[i]])) {
#       for(verti in 1:length(dens1$densities[[i]][[segi]])) {
#         vert1 <- dens1$endptverts[[segi]][verti]
#         vert2 <- dens1$endptverts[[segi]][verti+1]
#         if(dens1$densities[[i]][[segi]][[verti]]>=n) lines(dens1$rivers$lines[[segi]][vert1:vert2,],lwd=lwdplot,lend=1, col=1)
#       }
#     }
#     riverpoints(dens1$pointsegs[dens1$survey==surv[i]], dens1$pointverts[dens1$survey==surv[i]], dens1$rivers, pch=21, bg=0)
#   }
# }
# jpeg(file.path(dir.analysis,"density by survey.jpg"), height=6, width=8, units="in", res=300)
# par(family="serif", mfrow=c(2,5),mar=c(3,2,3,1))
# plotsimple(dens1,n=5,lwdplot=10)
# dev.off()


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
# jpeg(file.path(dir.analysis,"seqplot1.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mar=c(4,4,1,1))
plotseq(mdist,type="dotline",surveysareDates = F, ylab="River km", xlab="Survey")
# dev.off()

#by date
# jpeg(file.path(dir.analysis,"seqplot2.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mfrow=c(1,1))
plotseq(mdist2,type="dotline", surveysareDates = T, ylab="River km")
# dev.off()

#dates in mmddyy format for the graph
dates_mmddyy <- format(dates, "%m/%d/%Y")
unique(dates_mmddyy)

# jpeg(file.path(dir.analysis,"seqplot_mmddyy.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mar=c(6,3.8,1,1))
plotseq1(mdist2,type="dotline",surveysareDates = T, xax=dates_mmddyy, cex.axisX=.7)#, x45=T, x45y=-5, x45x=5)
mtext("Survey", side=1, line=4, outer = FALSE)
mtext("River km", side=2, line=2.5, outer = FALSE)
# dev.off()

#survey periods for the graph
survey_period <-unique(flight_data$Survey_Period)

# jpeg(file.path(dir.analysis,"seqplot_survey_period.jpg"), height=6, width=8, units="in", res=300)
par(family="serif", mar=c(6,3.8,1,1))
plotseq1(mdist2,type="dotline",surveysareDates = T, xax=survey_period, cex.axisX=.7)#, x45=T, x45y=-5, x45x=5)
mtext("Survey", side=1, line=4, outer = FALSE)
mtext("River km", side=2, line=2.5, outer = FALSE)
# dev.off()

#Summary stats by survey
get_n <- function(x) sum(!is.na(x))
pos_summary <- rbind(apply(mdist, 2, mean, na.rm=T),
                     apply(mdist, 2, median, na.rm=T),
                     apply(mdist, 2, sd, na.rm=T),
                     apply(mdist, 2, min, na.rm=T),
                     apply(mdist, 2, max, na.rm=T),
                     apply(mdist, 2, get_n))
row.names(pos_summary) <- c("mean","median","sd","min","max","n")


# #River positions and stats to .xlsx---------------------------------------------
# write.xlsx(mdist, file.path(dir.analysis,"Colville_grayling_tabs.xlsx"), sheetName="position_km")
# write.xlsx(pos_summary, file.path(dir.analysis,"Colville_grayling_tabs.xlsx"), sheetName="position_km_summary", append=T)
# --------------------------------------------------------------------------------


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
# jpeg(file.path(dir.analysis,"sequential_surveys1_mmddyy.jpg"),width=15,height=7,units="in",res=300)
par(family="serif")
plotseq(useq, xlab="Survey", ylab="Distance traveled (km)")
par(las=1, mar=c(4,4,1,1))
boxplot(useq1, xlab="", ylab="Distance traveled (km)",xaxt='n')
axis(1,at=1:ncol(useq), labels=NA)
text(x=1:ncol(useq), y=par("usr")[3]-4, labels=paste0(dates_mmddyy[-(ncol(useq)+1)],"\n to \n",dates_mmddyy[-1]), xpd=T, pos=1)
abline(h=0, lty=2)
# dev.off()

#by survey
# jpeg(file.path(dir.analysis,"sequential_surveys2.jpg"),width=15,height=7,units="in",res=300)
par(family="serif")
plotseq(useq, xlab="Survey", ylab="Distance traveled (km)")
par(las=2, mar=c(6,4,1,1))
boxplot(useq, ylab="Distance traveled (km)", xlab="Survey")
abline(h=0, lty=2)
# dev.off()


#by survey period
# jpeg(file.path(dir.analysis,"sequential_surveys1_survey_period.jpg"),width=15,height=7,units="in",res=300)
par(family="serif",cex = 1.2)
plotseq(useq, xlab="Survey", ylab="Distance traveled (RKM)")
par(las=1, mar=c(4,4,1,1))
boxplot(useq1, xlab="", ylab="Distance traveled (RKM)",xaxt='n',cex = 1.3)
axis(1,at=1:ncol(useq), labels=NA)
text(x=1:ncol(useq), y=par("usr")[3]-4, labels=paste0(survey_period[-(ncol(useq)+1)],"\n to \n",survey_period[-1]), xpd=T, pos=1)
abline(h=0, lty=2)
# dev.off()

# #Export distance between surveys to Excel---------------------------------------
# write.xlsx(useq, file.path(dir.analysis,"Colville_grayling_tabs.xlsx"), sheetName="sequential_surveys", append=T)
--------------------------------------------------------------------------------


#Summary stats for upstream and downstream movement-----------------------------
# dev.off()
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


# #Write summary stats to .xlsx---------------------------------------------------
# write.xlsx(movement_summary1, file.path(dir.analysis,"Colville_grayling_tabs.xlsx"), sheetName="movement_summary1", append=T)
# #-------------------------------------------------------------------------------


#Movement-----------------------------------------------------------------------
t12 <- colSums(useq>0,na.rm=T)
t13 <- round(t12/t3,2)
t14 <- colSums(useq<=0,na.rm=T)
t15 <- round(t14/t3,2)

movement_summary2 <- data.frame(Days_betw_surv=t2, Number_upstream=t12, Prop_upstream=t13, Number_downstream=t14, Prop_downstream=t15, row.names=t1)
movement_summary2


# #Movement summary to .xlsx------------------------------------------------------
# write.xlsx(movement_summary2, file.path(dir.analysis,"Colville_grayling_tabs.xlsx"), sheetName="movement_summary2", append=T)
# #-------------------------------------------------------------------------------


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

# dev.off()
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

# #Home ranges to .xlsx-----------------------------------------------------------
# write.xlsx(hr$ranges, file.path(dir.analysis, "Colville_grayling_tabs.xlsx"), sheetName="all_homeranges", append=T)
# write.xlsx(homeranges_by_tagging_loc, file.path(dir.analysis, "Colville_grayling_tabs.xlsx"), sheetName="homeranges_by_tagging_loc", append=T)
# #-------------------------------------------------------------------------------


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

# #Save home range summary to .xlsx------------------------------------------------
# write.xlsx(hr_table, file.path(dir.analysis, "Colville_grayling_tabs.xlsx"), sheetName="hr_table", append=T)
# #-------------------------------------------------------------------------------




#### MT addition 1/8/2024
save(flight_data, Colville4, file="flight_data.Rdata")

## lineplot, linked by paired observations
# flight_data has all the things
# telem_albers has raw xy telemetry data

# defining function to extract xy coords from seg-vert(river)
# might include this in riverdist
segvert2xy <- function(seg, vert, rivers) {
  if (!inherits(rivers, "rivernetwork"))
    stop("Argument 'rivers' must be of class 'rivernetwork'.  See help(line2network) for more information.")
  lines <- rivers$lines
  if (max(seg, na.rm = T) > length(lines) | min(seg, na.rm = T) <
      1)
    stop("Invalid segment numbers specified.")
  x <- y <- rep(NA, length(seg))
  for (i in 1:length(seg)) {
    x[i] <- lines[[seg[i]]][vert[i], 1]
    y[i] <- lines[[seg[i]]][vert[i], 2]
  }
  return(data.frame(x=x,y=y))
}
the_xy <- segvert2xy(seg=flight_data$seg, vert=flight_data$vert, rivers=Colville4)
flight_data$x <- the_xy$x
flight_data$y <- the_xy$y
flight_data$md <- mouthdist(seg=flight_data$seg, vert=flight_data$vert, rivers=Colville4)

x_wider <- pivot_wider(flight_data, names_from = Survey, values_from=x, id_cols=`Unique..`) %>% as.data.frame
y_wider <- pivot_wider(flight_data, names_from = Survey, values_from=y, id_cols=`Unique..`) %>% as.data.frame
md_wider <- pivot_wider(flight_data, names_from = Survey, values_from=md, id_cols=`Unique..`) %>% as.data.frame

# actually doing the paired thing
allsurveys <- sort(unique(flight_data$Survey))
par(mfrow=c(2,4))
for(i_survey in 2:length(allsurveys)) {
  # x0 <- flight_data$x[flight_data$Survey==allsurveys[i_survey-1]]
  # y0 <- flight_data$y[flight_data$Survey==allsurveys[i_survey-1]]
  # x1 <- flight_data$x[flight_data$Survey==allsurveys[i_survey]]
  # y1 <- flight_data$y[flight_data$Survey==allsurveys[i_survey]]

  # x0 <- ifelse(flight_data$Survey==allsurveys[i_survey-1], flight_data$x, NA)
  # y0 <- ifelse(flight_data$Survey==allsurveys[i_survey-1], flight_data$y, NA)
  # x1 <- ifelse(flight_data$Survey==allsurveys[i_survey], flight_data$x, NA)
  # y1 <- ifelse(flight_data$Survey==allsurveys[i_survey], flight_data$y, NA)

  x0 <- x_wider[,i_survey]
  x1 <- x_wider[,i_survey+1]
  y0 <- y_wider[,i_survey]
  y1 <- y_wider[,i_survey+1]

  thecol <- adjustcolor(ifelse(md_wider[,i_survey] - md_wider[,i_survey+1] < 0, 4, 2), alpha=.6)
  thelegend <- paste0(c("Net Upstream (n = ", "Net Downstream (n = "),
                      c(sum(md_wider[,i_survey] - md_wider[,i_survey+1] < 0, na.rm=T),
                        sum(md_wider[,i_survey] - md_wider[,i_survey+1] > 0, na.rm=T)),
                      rep(")", 2))

  plot(Colville4, empty=TRUE, linecol="grey", main=paste(allsurveys[i_survey-1],"to",allsurveys[i_survey]))
  points(x0, y0)
  points(x1, y1, pch=16)

  # themouthdist <- mouth
  #
  arrows(x0=x0, x1=x1, y0=y0, y1=y1, col=thecol, length=.05, lwd=2)
  legend("topleft",lwd=2,col=c(4,2),legend=thelegend)
}



## survival proportions(ish) by location somehow??
## survival proportions(ish) by size(ish)??

# Making a wide-format table for all these variables:
# Length Tagging_Location Located_1 Located_2 Located_3
length_wider <- pivot_wider(flight_data, names_from = Survey, values_from=Length, id_cols=`Unique..`) %>% as.data.frame
tag_wider <- pivot_wider(flight_data, names_from = Survey, values_from=Tagging_Location, id_cols=`Unique..`) %>% as.data.frame
loc1_wider <- pivot_wider(flight_data, names_from = Survey, values_from=Located_1, id_cols=`Unique..`) %>% as.data.frame
loc2_wider <- pivot_wider(flight_data, names_from = Survey, values_from=Located_2, id_cols=`Unique..`) %>% as.data.frame
loc3_wider <- pivot_wider(flight_data, names_from = Survey, values_from=Located_3, id_cols=`Unique..`) %>% as.data.frame

# defining a function to extract the mode of a vector
themode <- function(x) unname(names(sort(table(x), decreasing=T))[1])

# grabbing the mode of all the _wider rows
# this will make a vector corresponding to all tagged individuals
length_id <- length_wider[,2]
tag_id <- apply(tag_wider, 1, function(x) themode(as.character(x)[-1]))
loc1_id <- apply(loc1_wider, 1, function(x) themode(as.character(x)[-1]))
loc2_id <- apply(loc2_wider, 1, function(x) themode(as.character(x)[-1]))
loc3_id <- apply(loc3_wider, 1, function(x) themode(as.character(x)[-1]))

# will need by-indiv vectors of migratory behavior:
# might even restrict this to the first 4(ish) events??
# - homerange
# - cumulative distance (per survey)
## describe seasonal distributions and migrations (this will be the big one)
## INVESTIGATING CALCULATING A SUMMARY METRIC OF TRAVEL FOR EACH INDIVIDUAL
## (such that there is one value for each individual)

# - calculating minimum homerange for each individual
hr_mt <- homerange(unique=flight_data$Unique..,
                survey=flight_data$Survey,
                seg=flight_data$seg,
                vert=flight_data$vert,
                rivers=Colville4)
hr_table <- hr_mt$ranges  # extracting table of homeranges
hr_table$range <- hr_table$range/1000  # converting to km
hist(hr_table$range, main="", xlab="Minimum homerange (km)")

# looking at the locations of the top mover (seems reasonable)
hr_table[which.max(hr_table$range),]
par(mfrow=c(1,1))
plot(Colville4)
riverpoints(seg=flight_data$seg[flight_data$Unique..==65],
            vert=flight_data$vert[flight_data$Unique..==65],
            rivers=Colville4, pch=16)




## what if we did total observed distance INSTEAD of homerange?
## this is defined as the absolute distance between each possible pairing of
## sequential observations (or non-sequential if individual was not observed)

# to calculate, for each individual:
# - subset
# - sort by date
# - calculate non-missing distances sequentially
indiv <- sort(unique(flight_data$Unique..))
nobs <- cumuldist <- ndays <- rep(NA, length(indiv))
for(i in 1:length(indiv)) {
  # print(i)
  d1 <- flight_data[flight_data$Unique..==indiv[i],]
  d2 <- d1[order(d1$Survey),]
  nobs[i] <- nrow(d2)  # number of observations per individual
  ndays[i] <- d2$Date %>% range %>% diff %>% as.numeric  # total number of elapsed days
  cumuldist[i] <- 0
  if(nrow(d2)>1) {
    for(irow in 2:nrow(d2)) {
      cumuldist[i] <- cumuldist[i] + riverdistance(startseg=d2$seg[irow-1],
                                                   startvert=d2$vert[irow-1],
                                                   endseg=d2$seg[irow],
                                                   endvert=d2$vert[irow],
                                                   rivers=Colville4)/1000 # make it km
    }
  }
}
dtab <- data.frame(nobs, ndays, cumuldist)  # bundle summary metrics
rownames(dtab) <- indiv

## exploratory plots to look at behavior of possible summary metrics
# cumulative distance
hist(dtab$cumuldist)
plot(nobs, cumuldist)
boxplot(cumuldist~nobs)

# cumulative distance per possible pair of observations
hist(cumuldist/(nobs-1))
plot(nobs, cumuldist/(nobs-1))
boxplot(cumuldist/(nobs-1) ~ nobs)  # seems consistent enough to use as metric

# cumulative distance per day
boxplot(cumuldist/ndays ~ nobs)  # less consistent, one big outlier
hist(cumuldist/ndays, breaks=10)

## ok, bundle all summary metrics
dtab$dist_per_obs <- dtab$cumuldist/(dtab$nobs-1)   # distance per pair of observations
dtab$dist_per_day <- dtab$cumuldist/ndays
# this is a little awkward, have to include individuals with zero homerange
dtab$homerange <- 0
for(i in 1:nrow(hr_table)) dtab$homerange[rownames(dtab)==hr_table$ID[i]] <- hr_table$range[i]


# start plotting stuff!
par(mfrow=c(2,2))
plot(dtab$homerange ~ length_id)
plot(dtab$cumuldist ~ length_id)
plot(dtab$dist_per_obs ~ length_id)
plot(dtab$dist_per_day ~ length_id)

par(mfrow=c(2,2))
par(mar=c(6,4,3,2))
boxplot(dtab$homerange ~ tag_id, las=2, xlab="")
boxplot(dtab$cumuldist ~ tag_id, las=2, xlab="")
boxplot(dtab$dist_per_obs ~ tag_id, las=2, xlab="")
boxplot(dtab$dist_per_day ~ tag_id, las=2, xlab="")

par(mfrow=c(2,2))
par(mar=c(7,4,3,2))
boxplot(dtab$homerange ~ loc1_id, las=2, xlab="")
boxplot(dtab$cumuldist ~ loc1_id, las=2, xlab="")
boxplot(dtab$dist_per_obs ~ loc1_id, las=2, xlab="")
boxplot(dtab$dist_per_day ~ loc1_id, las=2, xlab="")

par(mfrow=c(2,2))
par(mar=c(7,4,3,2))
boxplot(dtab$homerange ~ loc2_id, las=2, xlab="")
boxplot(dtab$cumuldist ~ loc2_id, las=2, xlab="")
boxplot(dtab$dist_per_obs ~ loc2_id, las=2, xlab="")
boxplot(dtab$dist_per_day ~ loc2_id, las=2, xlab="")

par(mfrow=c(2,2))
par(mar=c(7,4,3,2))
boxplot(dtab$homerange ~ loc3_id, las=2, xlab="")
boxplot(dtab$cumuldist ~ loc3_id, las=2, xlab="")
boxplot(dtab$dist_per_obs ~ loc3_id, las=2, xlab="")
boxplot(dtab$dist_per_day ~ loc3_id, las=2, xlab="")

# will also need survival matrix
# AND fixed survival matrix
status_wider <- pivot_wider(flight_data, names_from = Survey, values_from=Status, id_cols=`Unique..`) %>% as.data.frame
surv <- status_wider[,-(1:2)]
for(j in 1:ncol(surv)) {
  surv[,j] <- as.numeric(surv[,j]=="A")
}
surv <- cbind(1,surv)

# now imputing ones or zeroes when it can be logically determined
for(i in 1:nrow(surv)) {
  which1 <- which(surv[i,]==1)
  which0 <- which(surv[i,]==0)
  if(length(which1) > 0) surv[i, 1:max(which1)] <- 1
  if(length(which0) > 0) surv[i, min(which0):ncol(surv)] <- 0
}

make_tottable <- function(x) {
  # t(apply(x,2,table))#, useNA="ifany"))
  mat <- matrix(ncol=2, nrow=ncol(x))
  for(i in 1:ncol(x)) {
    mat[i,1] <- sum(x[,i]==0, na.rm=T)
    mat[i,2] <- sum(x[,i]==1, na.rm=T)
  }
  rownames(mat) <- colnames(x)
  colnames(mat) <- c("0","1")
  return(mat)
}
par(mfrow=c(1,1))
mosaicplot(make_tottable(surv), main="all fish")

makeplotswith <- function(x) {
  surv <- surv[!is.na(x),]
  x <- x[!is.na(x)]

  for(i in 1:length(unique(x))) {
    thisone <- sort(unique(x))[i]
    mosaicplot(make_tottable(surv[x==thisone,]),
               main=paste(thisone, "n =",sum(x==thisone)))
  }

  tbls <- list()
  for(i in 1:length(unique(x))) {
    thisone <- sort(unique(x))[i]
    tbls[[i]] <- make_tottable(surv[x==thisone,])
  }
  plot(NA, xlim=c(1,10),ylim=c(0,1))
  for(i in 1:length(unique(x))) lines(tbls[[i]][,2]/rowSums(tbls[[i]]), col=i, lwd=2)
  legend("topright", lwd=2, col=1:length(unique(x)), legend=sort(unique(x)))
}

par(mfrow=c(2,3))
makeplotswith(x=loc3_id)
par(mfrow=c(2,3))
makeplotswith(x=loc2_id)
par(mfrow=c(2,3))
makeplotswith(x=cut(length_id, breaks=c(310,330,350,370,430)))
par(mfrow=c(2,3))
makeplotswith(x=cut(dtab$dist_per_day, breaks=c(0,.2,.4,.6,1)))
par(mfrow=c(2,3))
makeplotswith(x=cut(dtab$dist_per_obs, breaks=c(0,25,50,100,200)))
# par(mfrow=c(2,3))
# makeplotswith(x=cut(dtab$homerange/(dtab$nobs-1),
#                     breaks=c(0,20,50,100,200)))


### yes we could look at relationships between size/loc and movingness

par(mfrow=c(1,1))
mosaicplot(table(loc3_id, cut(dtab$dist_per_day, breaks=c(0,.2,.4,.6,1))),
           color=rev(grey.colors(4)), ylab="km per day")
mosaicplot(table(loc3_id, cut(dtab$dist_per_obs, breaks=c(0,25,50,100,200))),
           color=rev(grey.colors(4)), ylab="km per obs")
mosaicplot(table(cut(length_id, breaks=c(310,330,350,370,430)),
                 cut(dtab$dist_per_day, breaks=c(0,.2,.4,.6,1))),
           color=rev(grey.colors(4)), ylab="km per day")
mosaicplot(table(loc3_id, cut(dtab$dist_per_day, breaks=c(0,.2,.4,.6,1))),
           color=rev(grey.colors(4)))

# mosaicplot(table(loc2_id, cut(dtab$dist_per_day, breaks=c(0,.2,.4,.6,1))))
# mosaicplot(table(tag_id, cut(dtab$dist_per_day, breaks=c(0,.2,.4,.6,1))))


## survival model, motivated by above
library(jagsUI)
library(jagshelper)

# still need firstdead
firstdead <- rep(NA, nrow(surv))
for(i in 1:nrow(surv)) {
  firstdead[i] <- ifelse(!any(surv[i,]==0, na.rm=T),
                         ncol(surv),
                         which.max(surv[i,]==0))
}

# make appropriate cut variables: lengthcut, dist_obs, dist_day, homerange
lengthcut <- cut(length_id, breaks=c(310,330,350,370,430))
dist_obs <- cut(dtab$dist_per_obs, breaks=c(0,25,50,100,200))
dist_day <- cut(dtab$dist_per_day, breaks=c(0,.2,.4,.6,1))
sectionmode <- loc3_id

### TRYING A MORE STRUCTURED APPROACH TO MODEL SELECTION
# bundle data to pass into JAGS
surv_vbls_data <- list(survtable=surv,
                       firstdead=firstdead,
                       # firstpresent=firstpresent,
                       n=nrow(surv),
                       np=ncol(surv)-1,
                       sectionmode=as.numeric(as.factor(sectionmode)),
                       n_section=length(unique(sectionmode)),
                       lengthcut=as.numeric(as.factor(lengthcut)),
                       n_length=length(levels(lengthcut)),
                       length=length_id - mean(length_id, na.rm=T),
                       dist_obs=as.numeric(as.factor(dist_obs)),
                       n_dist_obs=length(levels(dist_obs)),
                       dist_day=as.numeric(as.factor(dist_day)),
                       n_dist_day=length(levels(dist_day)))

## taking out blank distance things
# surv_vbls_data$survtable <- surv_vbls_data$survtable[!is.na(dist_obs),]
# surv_vbls_data$firstdead <- surv_vbls_data$firstdead[!is.na(dist_obs)]
# # surv_vbls_data$firstpresent <- surv_vbls_data$firstpresent[!is.na(lengthcut)]
# surv_vbls_data$sectionmode <- surv_vbls_data$sectionmode[!is.na(dist_obs)]
# surv_vbls_data$lengthcut <- surv_vbls_data$lengthcut[!is.na(dist_obs)]
# surv_vbls_data$length <- surv_vbls_data$length[!is.na(dist_obs)]
# surv_vbls_data$dist_day <- surv_vbls_data$dist_day[!is.na(dist_obs)]
# surv_vbls_data$dist_obs <- surv_vbls_data$dist_obs[!is.na(dist_obs)]
# surv_vbls_data$n <- sum(!is.na(dist_obs))

## taking out blank distance things AND section=Other
surv_vbls_data$survtable <- surv_vbls_data$survtable[!is.na(dist_obs) & as.numeric(as.factor(sectionmode)) < 5,]
surv_vbls_data$firstdead <- surv_vbls_data$firstdead[!is.na(dist_obs) & as.numeric(as.factor(sectionmode)) < 5]
# surv_vbls_data$firstpresent <- surv_vbls_data$firstpresent[!is.na(lengthcut)]
surv_vbls_data$sectionmode <- surv_vbls_data$sectionmode[!is.na(dist_obs) & as.numeric(as.factor(sectionmode)) < 5]
surv_vbls_data$lengthcut <- surv_vbls_data$lengthcut[!is.na(dist_obs) & as.numeric(as.factor(sectionmode)) < 5]
surv_vbls_data$length <- surv_vbls_data$length[!is.na(dist_obs) & as.numeric(as.factor(sectionmode)) < 5]
surv_vbls_data$dist_day <- surv_vbls_data$dist_day[!is.na(dist_obs) & as.numeric(as.factor(sectionmode)) < 5]
surv_vbls_data$dist_obs <- surv_vbls_data$dist_obs[!is.na(dist_obs) & as.numeric(as.factor(sectionmode)) < 5]
surv_vbls_data$n <- sum(!is.na(dist_obs) & as.numeric(as.factor(sectionmode)) < 5)
surv_vbls_data$n_section=length(unique(surv_vbls_data$sectionmode))

# JAGS controls
niter <- 100*1000
ncores <- min(10, parallel::detectCores()-1)  # number of cores to use

jagsouts <- list()

## Model 1: baseline
surv_vbls_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    for(j in 2:firstdead[i]) {          # for each survey
      # survtable[i,j] ~ dbin(p[j-1], survtable[i,j-1])   # for each event present
      survtable[i,j] ~ dbin(p[i,j], survtable[i,j-1])
      logit(p[i,j]) <- b0[j-1]
      # + b_section[sectionmode[i]]
      # + b_lengthcut[lengthcut[i]]
      # + b_length*length[i]
      # + b_distday[dist_day[i]]
      # + b_distobs[dist_obs[i]]

      # survtable_pp[i,j] ~ dbin(p[i,j], survtable[i,j-1])   ### this is included for pp check

    }
  }

  for(j in 1:np) {
    b0[j] ~ dnorm(0, 0.1)
  }

  # for(i_section in 1:(n_section-1)) {
  #   b_section[i_section] ~ dnorm(0, 0.1)
  # }
  # b_section[n_section] <- -sum(b_section[1:(n_section-1)])
  #
  # for(i_length in 1:(n_length-1)) {
  #   b_lengthcut[i_length] ~ dnorm(0, 0.1)
  # }
  # b_lengthcut[n_length] <- -sum(b_lengthcut[2:(n_length-1)])
  #
  # b_length ~ dnorm(0, 0.1)
  #
  # for(i_distday in 1:(n_dist_day-1)) {
  #   b_distday[i_distday] ~ dnorm(0, 0.1)
  # }
  # b_distday[n_dist_day] <- -sum(b_distday[1:(n_dist_day-1)])
  #
  # for(i_distobs in 1:(n_dist_obs-1)) {
  #   b_distobs[i_distobs] ~ dnorm(0, 0.1)
  # }
  # b_distobs[n_dist_obs] <- -sum(b_distobs[1:(n_dist_obs-1)])

}', file=surv_vbls_jags)

{
  tstart <- Sys.time()
  print(tstart)
  surv_vbls_jags_out <- jagsUI::jags(model.file=surv_vbls_jags, data=surv_vbls_data,
                                     parameters.to.save=c("p","b0","b_section","b_length","b_lengthcut",
                                                          "b_distday","b_distobs",
                                                          "survtable_pp"), #"survtable",
                                     n.chains=ncores, parallel=T, n.iter=niter,
                                     n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)

  par(mfrow=c(3,3))
  plotRhats(surv_vbls_jags_out)
  traceworstRhat(surv_vbls_jags_out)
}
jagsouts[[1]] <- surv_vbls_jags_out



## Model 2: just section
surv_vbls_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    for(j in 2:firstdead[i]) {          # for each survey
      # survtable[i,j] ~ dbin(p[j-1], survtable[i,j-1])   # for each event present
      survtable[i,j] ~ dbin(p[i,j], survtable[i,j-1])
      logit(p[i,j]) <- b0[j-1]
      + b_section[sectionmode[i]]
      # + b_lengthcut[lengthcut[i]]
      # + b_length*length[i]
      # + b_distday[dist_day[i]]
      # + b_distobs[dist_obs[i]]

      # survtable_pp[i,j] ~ dbin(p[i,j], survtable[i,j-1])   ### this is included for pp check

    }
  }

  for(j in 1:np) {
    b0[j] ~ dnorm(0, 0.1)
  }

  for(i_section in 1:(n_section-1)) {
    b_section[i_section] ~ dnorm(0, 0.1)
  }
  b_section[n_section] <- -sum(b_section[1:(n_section-1)])

  # for(i_length in 1:(n_length-1)) {
  #   b_lengthcut[i_length] ~ dnorm(0, 0.1)
  # }
  # b_lengthcut[n_length] <- -sum(b_lengthcut[2:(n_length-1)])
  #
  # b_length ~ dnorm(0, 0.1)
  #
  # for(i_distday in 1:(n_dist_day-1)) {
  #   b_distday[i_distday] ~ dnorm(0, 0.1)
  # }
  # b_distday[n_dist_day] <- -sum(b_distday[1:(n_dist_day-1)])
  #
  # for(i_distobs in 1:(n_dist_obs-1)) {
  #   b_distobs[i_distobs] ~ dnorm(0, 0.1)
  # }
  # b_distobs[n_dist_obs] <- -sum(b_distobs[1:(n_dist_obs-1)])

}', file=surv_vbls_jags)

{
  tstart <- Sys.time()
  print(tstart)
  surv_vbls_jags_out <- jagsUI::jags(model.file=surv_vbls_jags, data=surv_vbls_data,
                                     parameters.to.save=c("p","b0","b_section","b_length","b_lengthcut",
                                                          "b_distday","b_distobs",
                                                          "survtable_pp"), #"survtable",
                                     n.chains=ncores, parallel=T, n.iter=niter,
                                     n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)

  par(mfrow=c(3,3))
  plotRhats(surv_vbls_jags_out)
  traceworstRhat(surv_vbls_jags_out)
}
jagsouts[[2]] <- surv_vbls_jags_out



## Model 3: just length (cut)
surv_vbls_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    for(j in 2:firstdead[i]) {          # for each survey
      # survtable[i,j] ~ dbin(p[j-1], survtable[i,j-1])   # for each event present
      survtable[i,j] ~ dbin(p[i,j], survtable[i,j-1])
      logit(p[i,j]) <- b0[j-1]
      # + b_section[sectionmode[i]]
      + b_lengthcut[lengthcut[i]]
      # + b_length*length[i]
      # + b_distday[dist_day[i]]
      # + b_distobs[dist_obs[i]]

      # survtable_pp[i,j] ~ dbin(p[i,j], survtable[i,j-1])   ### this is included for pp check

    }
  }

  for(j in 1:np) {
    b0[j] ~ dnorm(0, 0.1)
  }

  # for(i_section in 1:(n_section-1)) {
  #   b_section[i_section] ~ dnorm(0, 0.1)
  # }
  # b_section[n_section] <- -sum(b_section[1:(n_section-1)])

  for(i_length in 1:(n_length-1)) {
    b_lengthcut[i_length] ~ dnorm(0, 0.1)
  }
  b_lengthcut[n_length] <- -sum(b_lengthcut[2:(n_length-1)])

  # b_length ~ dnorm(0, 0.1)
  #
  # for(i_distday in 1:(n_dist_day-1)) {
  #   b_distday[i_distday] ~ dnorm(0, 0.1)
  # }
  # b_distday[n_dist_day] <- -sum(b_distday[1:(n_dist_day-1)])
  #
  # for(i_distobs in 1:(n_dist_obs-1)) {
  #   b_distobs[i_distobs] ~ dnorm(0, 0.1)
  # }
  # b_distobs[n_dist_obs] <- -sum(b_distobs[1:(n_dist_obs-1)])

}', file=surv_vbls_jags)

{
  tstart <- Sys.time()
  print(tstart)
  surv_vbls_jags_out <- jagsUI::jags(model.file=surv_vbls_jags, data=surv_vbls_data,
                                     parameters.to.save=c("p","b0","b_section","b_length","b_lengthcut",
                                                          "b_distday","b_distobs",
                                                          "survtable_pp"), #"survtable",
                                     n.chains=ncores, parallel=T, n.iter=niter,
                                     n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)

  par(mfrow=c(3,3))
  plotRhats(surv_vbls_jags_out)
  traceworstRhat(surv_vbls_jags_out)
}
jagsouts[[3]] <- surv_vbls_jags_out



## Model 4: just length (numeric)
surv_vbls_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    for(j in 2:firstdead[i]) {          # for each survey
      # survtable[i,j] ~ dbin(p[j-1], survtable[i,j-1])   # for each event present
      survtable[i,j] ~ dbin(p[i,j], survtable[i,j-1])
      logit(p[i,j]) <- b0[j-1]
      # + b_section[sectionmode[i]]
      # + b_lengthcut[lengthcut[i]]
      + b_length*length[i]
      # + b_distday[dist_day[i]]
      # + b_distobs[dist_obs[i]]

      # survtable_pp[i,j] ~ dbin(p[i,j], survtable[i,j-1])   ### this is included for pp check

    }
  }

  for(j in 1:np) {
    b0[j] ~ dnorm(0, 0.1)
  }

  # for(i_section in 1:(n_section-1)) {
  #   b_section[i_section] ~ dnorm(0, 0.1)
  # }
  # b_section[n_section] <- -sum(b_section[1:(n_section-1)])
  #
  # for(i_length in 1:(n_length-1)) {
  #   b_lengthcut[i_length] ~ dnorm(0, 0.1)
  # }
  # b_lengthcut[n_length] <- -sum(b_lengthcut[2:(n_length-1)])

  b_length ~ dnorm(0, 0.1)

  # for(i_distday in 1:(n_dist_day-1)) {
  #   b_distday[i_distday] ~ dnorm(0, 0.1)
  # }
  # b_distday[n_dist_day] <- -sum(b_distday[1:(n_dist_day-1)])
  #
  # for(i_distobs in 1:(n_dist_obs-1)) {
  #   b_distobs[i_distobs] ~ dnorm(0, 0.1)
  # }
  # b_distobs[n_dist_obs] <- -sum(b_distobs[1:(n_dist_obs-1)])

}', file=surv_vbls_jags)

{
  tstart <- Sys.time()
  print(tstart)
  surv_vbls_jags_out <- jagsUI::jags(model.file=surv_vbls_jags, data=surv_vbls_data,
                                     parameters.to.save=c("p","b0","b_section","b_length","b_lengthcut",
                                                          "b_distday","b_distobs",
                                                          "survtable_pp"), #"survtable",
                                     n.chains=ncores, parallel=T, n.iter=niter,
                                     n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)

  par(mfrow=c(3,3))
  plotRhats(surv_vbls_jags_out)
  traceworstRhat(surv_vbls_jags_out)
}
jagsouts[[4]] <- surv_vbls_jags_out



## Model 5: just distance per day
surv_vbls_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    for(j in 2:firstdead[i]) {          # for each survey
      # survtable[i,j] ~ dbin(p[j-1], survtable[i,j-1])   # for each event present
      survtable[i,j] ~ dbin(p[i,j], survtable[i,j-1])
      logit(p[i,j]) <- b0[j-1]
      # + b_section[sectionmode[i]]
      # + b_lengthcut[lengthcut[i]]
      # + b_length*length[i]
      + b_distday[dist_day[i]]
      # + b_distobs[dist_obs[i]]

      # survtable_pp[i,j] ~ dbin(p[i,j], survtable[i,j-1])   ### this is included for pp check

    }
  }

  for(j in 1:np) {
    b0[j] ~ dnorm(0, 0.1)
  }

  # for(i_section in 1:(n_section-1)) {
  #   b_section[i_section] ~ dnorm(0, 0.1)
  # }
  # b_section[n_section] <- -sum(b_section[1:(n_section-1)])
  #
  # for(i_length in 1:(n_length-1)) {
  #   b_lengthcut[i_length] ~ dnorm(0, 0.1)
  # }
  # b_lengthcut[n_length] <- -sum(b_lengthcut[2:(n_length-1)])
  #
  # b_length ~ dnorm(0, 0.1)

  for(i_distday in 1:(n_dist_day-1)) {
    b_distday[i_distday] ~ dnorm(0, 0.1)
  }
  b_distday[n_dist_day] <- -sum(b_distday[1:(n_dist_day-1)])

  # for(i_distobs in 1:(n_dist_obs-1)) {
  #   b_distobs[i_distobs] ~ dnorm(0, 0.1)
  # }
  # b_distobs[n_dist_obs] <- -sum(b_distobs[1:(n_dist_obs-1)])

}', file=surv_vbls_jags)

{
  tstart <- Sys.time()
  print(tstart)
  surv_vbls_jags_out <- jagsUI::jags(model.file=surv_vbls_jags, data=surv_vbls_data,
                                     parameters.to.save=c("p","b0","b_section","b_length","b_lengthcut",
                                                          "b_distday","b_distobs",
                                                          "survtable_pp"), #"survtable",
                                     n.chains=ncores, parallel=T, n.iter=niter,
                                     n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)

  par(mfrow=c(3,3))
  plotRhats(surv_vbls_jags_out)
  traceworstRhat(surv_vbls_jags_out)
}
jagsouts[[5]] <- surv_vbls_jags_out



## Model 6: just distance per obs
surv_vbls_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    for(j in 2:firstdead[i]) {          # for each survey
      # survtable[i,j] ~ dbin(p[j-1], survtable[i,j-1])   # for each event present
      survtable[i,j] ~ dbin(p[i,j], survtable[i,j-1])
      logit(p[i,j]) <- b0[j-1]
      # + b_section[sectionmode[i]]
      # + b_lengthcut[lengthcut[i]]
      # + b_length*length[i]
      # + b_distday[dist_day[i]]
      + b_distobs[dist_obs[i]]

      # survtable_pp[i,j] ~ dbin(p[i,j], survtable[i,j-1])   ### this is included for pp check

    }
  }

  for(j in 1:np) {
    b0[j] ~ dnorm(0, 0.1)
  }

  # for(i_section in 1:(n_section-1)) {
  #   b_section[i_section] ~ dnorm(0, 0.1)
  # }
  # b_section[n_section] <- -sum(b_section[1:(n_section-1)])
  #
  # for(i_length in 1:(n_length-1)) {
  #   b_lengthcut[i_length] ~ dnorm(0, 0.1)
  # }
  # b_lengthcut[n_length] <- -sum(b_lengthcut[2:(n_length-1)])
  #
  # b_length ~ dnorm(0, 0.1)
  #
  # for(i_distday in 1:(n_dist_day-1)) {
  #   b_distday[i_distday] ~ dnorm(0, 0.1)
  # }
  # b_distday[n_dist_day] <- -sum(b_distday[1:(n_dist_day-1)])

  for(i_distobs in 1:(n_dist_obs-1)) {
    b_distobs[i_distobs] ~ dnorm(0, 0.1)
  }
  b_distobs[n_dist_obs] <- -sum(b_distobs[1:(n_dist_obs-1)])

}', file=surv_vbls_jags)

{
  tstart <- Sys.time()
  print(tstart)
  surv_vbls_jags_out <- jagsUI::jags(model.file=surv_vbls_jags, data=surv_vbls_data,
                                     parameters.to.save=c("p","b0","b_section","b_length","b_lengthcut",
                                                          "b_distday","b_distobs",
                                                          "survtable_pp"), #"survtable",
                                     n.chains=ncores, parallel=T, n.iter=niter,
                                     n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)

  par(mfrow=c(3,3))
  plotRhats(surv_vbls_jags_out)
  traceworstRhat(surv_vbls_jags_out)
}
jagsouts[[6]] <- surv_vbls_jags_out



## Model 7: section, length (num), and dist per obs!
surv_vbls_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    for(j in 2:firstdead[i]) {          # for each survey
      # survtable[i,j] ~ dbin(p[j-1], survtable[i,j-1])   # for each event present
      survtable[i,j] ~ dbin(p[i,j], survtable[i,j-1])
      logit(p[i,j]) <- b0[j-1]
      + b_section[sectionmode[i]]
      # + b_lengthcut[lengthcut[i]]
      + b_length*length[i]
      # + b_distday[dist_day[i]]
      + b_distobs[dist_obs[i]]

      # survtable_pp[i,j] ~ dbin(p[i,j], survtable[i,j-1])   ### this is included for pp check

    }
  }

  for(j in 1:np) {
    b0[j] ~ dnorm(0, 0.1)
  }

  for(i_section in 1:(n_section-1)) {
    b_section[i_section] ~ dnorm(0, 0.1)
  }
  b_section[n_section] <- -sum(b_section[1:(n_section-1)])

  # for(i_length in 1:(n_length-1)) {
  #   b_lengthcut[i_length] ~ dnorm(0, 0.1)
  # }
  # b_lengthcut[n_length] <- -sum(b_lengthcut[2:(n_length-1)])

  b_length ~ dnorm(0, 0.1)

  # for(i_distday in 1:(n_dist_day-1)) {
  #   b_distday[i_distday] ~ dnorm(0, 0.1)
  # }
  # b_distday[n_dist_day] <- -sum(b_distday[1:(n_dist_day-1)])

  for(i_distobs in 1:(n_dist_obs-1)) {
    b_distobs[i_distobs] ~ dnorm(0, 0.1)
  }
  b_distobs[n_dist_obs] <- -sum(b_distobs[1:(n_dist_obs-1)])

}', file=surv_vbls_jags)

{
  tstart <- Sys.time()
  print(tstart)
  surv_vbls_jags_out <- jagsUI::jags(model.file=surv_vbls_jags, data=surv_vbls_data,
                                     parameters.to.save=c("p","b0","b_section","b_length","b_lengthcut",
                                                          "b_distday","b_distobs",
                                                          "survtable_pp"), #"survtable",
                                     n.chains=ncores, parallel=T, n.iter=niter,
                                     n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)

  par(mfrow=c(3,3))
  plotRhats(surv_vbls_jags_out)
  traceworstRhat(surv_vbls_jags_out)
}
jagsouts[[7]] <- surv_vbls_jags_out



## Model 8: length (num) and dist per obs
surv_vbls_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    for(j in 2:firstdead[i]) {          # for each survey
      # survtable[i,j] ~ dbin(p[j-1], survtable[i,j-1])   # for each event present
      survtable[i,j] ~ dbin(p[i,j], survtable[i,j-1])
      logit(p[i,j]) <- b0[j-1]
      # + b_section[sectionmode[i]]
      # + b_lengthcut[lengthcut[i]]
      + b_length*length[i]
      # + b_distday[dist_day[i]]
      + b_distobs[dist_obs[i]]

      # survtable_pp[i,j] ~ dbin(p[i,j], survtable[i,j-1])   ### this is included for pp check

    }
  }

  for(j in 1:np) {
    b0[j] ~ dnorm(0, 0.1)
  }

  # for(i_section in 1:(n_section-1)) {
  #   b_section[i_section] ~ dnorm(0, 0.1)
  # }
  # b_section[n_section] <- -sum(b_section[1:(n_section-1)])

  # for(i_length in 1:(n_length-1)) {
  #   b_lengthcut[i_length] ~ dnorm(0, 0.1)
  # }
  # b_lengthcut[n_length] <- -sum(b_lengthcut[2:(n_length-1)])

  b_length ~ dnorm(0, 0.1)

  # for(i_distday in 1:(n_dist_day-1)) {
  #   b_distday[i_distday] ~ dnorm(0, 0.1)
  # }
  # b_distday[n_dist_day] <- -sum(b_distday[1:(n_dist_day-1)])

  for(i_distobs in 1:(n_dist_obs-1)) {
    b_distobs[i_distobs] ~ dnorm(0, 0.1)
  }
  b_distobs[n_dist_obs] <- -sum(b_distobs[1:(n_dist_obs-1)])

}', file=surv_vbls_jags)

{
  tstart <- Sys.time()
  print(tstart)
  surv_vbls_jags_out <- jagsUI::jags(model.file=surv_vbls_jags, data=surv_vbls_data,
                                     parameters.to.save=c("p","b0","b_section","b_length","b_lengthcut",
                                                          "b_distday","b_distobs",
                                                          "survtable_pp"), #"survtable",
                                     n.chains=ncores, parallel=T, n.iter=niter,
                                     n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)

  par(mfrow=c(3,3))
  plotRhats(surv_vbls_jags_out)
  traceworstRhat(surv_vbls_jags_out)
}
jagsouts[[8]] <- surv_vbls_jags_out


sapply(jagsouts, function(x) x$DIC)
# [1] 304.3466 309.0419 304.1840 302.1355 308.6036 300.7047   # keeping ALL priors
# [1] 305.0627 309.2201 304.2258 302.1256 308.6323 300.3952 300.1507 296.9016   # excluding priors

par(mfrow=c(2,2))
caterpillar(jagsouts[[1]], p="b0", main="baseline log-odds")
caterpillar(expit(jagsouts[[1]]$sims.list$b0), main="baseline probabilities", ylim=0:1)

par(mfrow=c(2,2))
caterpillar(jagsouts[[2]], p="b0", main="baseline log-odds")
caterpillar(expit(jagsouts[[2]]$sims.list$b0), main="baseline probabilities", ylim=0:1)
caterpillar(jagsouts[[2]], p="b_section", main="section log-odds adjustment")
abline(h=0, lty=2)
caterpillar(exp(jagsouts[[2]]$sims.list$b_section), main="section odds adjustment")
abline(h=1, lty=2)

par(mfrow=c(2,2))
caterpillar(jagsouts[[3]], p="b0", main="baseline log-odds")
caterpillar(expit(jagsouts[[3]]$sims.list$b0), main="baseline probabilities", ylim=0:1)
caterpillar(jagsouts[[3]], p="b_lengthcut", main="length log-odds adjustment")
abline(h=0, lty=2)
caterpillar(exp(jagsouts[[3]]$sims.list$b_lengthcut), main="length odds adjustment")
abline(h=1, lty=2)

par(mfrow=c(2,2))
mod <- 4
caterpillar(jagsouts[[mod]], p="b0", main="baseline log-odds")
caterpillar(expit(jagsouts[[mod]]$sims.list$b0), main="baseline probabilities", ylim=0:1)
# caterpillar(jagsouts[[mod]], p="b_length", main="length log-odds adjustment")
# abline(h=0, lty=2)
# caterpillar(exp(jagsouts[[mod]]$sims.list$b_length), main="length odds adjustment")
# abline(h=1, lty=2)
plotdens(jagsouts[[mod]], p="b_length", exact=T)
abline(v=0, lty=2)

par(mfrow=c(2,2))
mod <- 5
caterpillar(jagsouts[[mod]], p="b0", main="baseline log-odds")
caterpillar(expit(jagsouts[[mod]]$sims.list$b0), main="baseline probabilities", ylim=0:1)
caterpillar(jagsouts[[mod]], p="b_distday", main="dist per day log-odds adjustment")
abline(h=0, lty=2)
caterpillar(exp(jagsouts[[mod]]$sims.list$b_distday), main="dist per day odds adjustment")
abline(h=1, lty=2)

par(mfrow=c(2,2))
mod <- 6
caterpillar(jagsouts[[mod]], p="b0", main="baseline log-odds")
caterpillar(expit(jagsouts[[mod]]$sims.list$b0), main="baseline probabilities", ylim=0:1)
caterpillar(jagsouts[[mod]], p="b_distobs", main="dist per obs log-odds adjustment")
abline(h=0, lty=2)
caterpillar(exp(jagsouts[[mod]]$sims.list$b_distobs), main="dist per obs odds adjustment")
abline(h=1, lty=2)

par(mfrow=c(4,2))
mod <- 7
caterpillar(jagsouts[[mod]], p="b0", main="baseline log-odds")
caterpillar(expit(jagsouts[[mod]]$sims.list$b0), main="baseline probabilities", ylim=0:1)
caterpillar(jagsouts[[mod]], p="b_section", main="section log-odds adjustment")
abline(h=0, lty=2)
caterpillar(exp(jagsouts[[mod]]$sims.list$b_section), main="section odds adjustment")
abline(h=1, lty=2)
caterpillar(jagsouts[[mod]], p="b_distobs", main="dist per obs log-odds adjustment")
abline(h=0, lty=2)
caterpillar(exp(jagsouts[[mod]]$sims.list$b_distobs), main="dist per obs odds adjustment")
abline(h=1, lty=2)
plotdens(jagsouts[[mod]], p="b_length", exact=T)
abline(v=0, lty=2)

par(mfrow=c(3,2))
mod <- 8
caterpillar(jagsouts[[mod]], p="b0", main="baseline log-odds")
caterpillar(expit(jagsouts[[mod]]$sims.list$b0), main="baseline probabilities", ylim=0:1)
caterpillar(jagsouts[[mod]], p="b_distobs", main="dist per obs log-odds adjustment")
abline(h=0, lty=2)
caterpillar(exp(jagsouts[[mod]]$sims.list$b_distobs), main="dist per obs odds adjustment")
abline(h=1, lty=2)
plotdens(jagsouts[[mod]], p="b_length", exact=T)
abline(v=0, lty=2)


# logit(p[i,j]) <- b0[j-1]
# + b_section[sectionmode[i]]
# + b_lengthcut[lengthcut[i]]
# + b_length*length[i]
# + b_distday[dist_day[i]]
# + b_distobs[dist_obs[i]]



## OK NOW WE'RE GOING TO TRY EVERY POSSIBLE COMBO!!
surv_vbls_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    for(j in 2:firstdead[i]) {          # for each survey
      survtable[i,j] ~ dbin(p[i,j], survtable[i,j-1])
      logit(p[i,j]) <- b0[j-1]
      + onoff[1]*b_section[sectionmode[i]]
      + onoff[2]*b_lengthcut[lengthcut[i]]
      + onoff[3]*b_length*length[i]
      + onoff[4]*b_distday[dist_day[i]]
      + onoff[5]*b_distobs[dist_obs[i]]

      # survtable_pp[i,j] ~ dbin(p[i,j], survtable[i,j-1])   ### this is included for pp check

    }
  }

  for(j in 1:np) {
    b0[j] ~ dnorm(0, 0.1)
  }

  for(i_section in 1:(n_section-1)) {
    b_section[i_section] ~ dnorm(0, 0.1)
  }
  b_section[n_section] <- -sum(b_section[1:(n_section-1)])

  for(i_length in 1:(n_length-1)) {
    b_lengthcut[i_length] ~ dnorm(0, 0.1)
  }
  b_lengthcut[n_length] <- -sum(b_lengthcut[2:(n_length-1)])

  b_length ~ dnorm(0, 0.1)

  for(i_distday in 1:(n_dist_day-1)) {
    b_distday[i_distday] ~ dnorm(0, 0.1)
  }
  b_distday[n_dist_day] <- -sum(b_distday[1:(n_dist_day-1)])

  for(i_distobs in 1:(n_dist_obs-1)) {
    b_distobs[i_distobs] ~ dnorm(0, 0.1)
  }
  b_distobs[n_dist_obs] <- -sum(b_distobs[1:(n_dist_obs-1)])

}', file=surv_vbls_jags)



# + onoff[1]*b_section[sectionmode[i]]
# + onoff[2]*b_lengthcut[lengthcut[i]]
# + onoff[3]*b_length*length[i]
# + onoff[4]*b_distday[dist_day[i]]
# + onoff[5]*b_distobs[dist_obs[i]]
newjagsouts <- list()
# onoffs <- list(rep(0,5),      # baseline
#                c(1,0,0,0,0),  # only one predictor
#                c(0,1,0,0,0),
#                c(0,0,1,0,0),
#                c(0,0,0,1,0),
#                c(1,0,0,0,1),  # keep section, only one other
#                c(1,1,0,0,0),
#                c(1,0,1,0,0),
#                c(1,0,0,1,0),
#                c(1,0,0,0,1),
# )
onoffs <- expand.grid(0:1,0:1,0:1,0:1,0:1)
allowthese <- !(onoffs$Var4==1 & onoffs$Var5==1) & !(onoffs$Var2==1 & onoffs$Var3==1)
onoffs <- onoffs[allowthese,]
for(ilist in 1:nrow(onoffs)) {
  surv_vbls_data$onoff <- as.numeric(onoffs[ilist,])
  print(ilist)
  print(surv_vbls_data$onoff)
{
  tstart <- Sys.time()
  print(tstart)
  surv_vbls_jags_out <- jagsUI::jags(model.file=surv_vbls_jags, data=surv_vbls_data,
                                     parameters.to.save=c("p","b0","b_section","b_length","b_lengthcut",
                                                          "b_distday","b_distobs",
                                                          "survtable_pp"), #"survtable",
                                     n.chains=ncores, parallel=T, n.iter=niter,
                                     n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)

  par(mfrow=c(3,3))
  plotRhats(surv_vbls_jags_out)
  traceworstRhat(surv_vbls_jags_out)
}
  newjagsouts[[ilist]] <- surv_vbls_jags_out
}
sapply(newjagsouts, function(x) x$DIC)
which.min(sapply(newjagsouts, function(x) x$DIC))
onoffs[which.min(sapply(newjagsouts, function(x) x$DIC)),]

# DICs <- sapply(newjagsouts, function(x) x$DIC)
# newonoffs <- onoffs
# allowthese <- !(onoffs$Var4==1 & onoffs$Var5==1) & !(onoffs$Var2==1 & onoffs$Var3==1)
# newonoffs <- onoffs[allowthese,]
# newDICs <- DICs[allowthese]
# newonoffs[which.min(newDICs),]



#### verifying proportions in report

sum(tag_id=="Nuiqsut")  # 51
sum(tag_id=="Nuiqsut") - sum(tag_id=="Nuiqsut" & status_wider$`3`=="A", na.rm=T) # 23
sum(tag_id=="Nuiqsut") - sum(surv[tag_id=="Nuiqsut",]$`3`==1, na.rm=T)  # 22

sum(tag_id!="Nuiqsut")  # 116
sum(tag_id!="Nuiqsut") - sum(tag_id!="Nuiqsut" & status_wider$`3`=="A", na.rm=T) # 60
sum(tag_id!="Nuiqsut") - sum(surv[tag_id!="Nuiqsut",]$`3`==1, na.rm=T)  # 58

table(flight_data$Date , flight_data$Survey)
(167 - colSums(surv==1, na.rm=T))/167
(colSums(surv==0, na.rm=T))/167

(sum(tag_id=="Nuiqsut") - colSums(surv[tag_id=="Nuiqsut",]==1, na.rm=T))/sum(tag_id=="Nuiqsut")
(colSums(surv[tag_id=="Nuiqsut",]==0, na.rm=T))/sum(tag_id=="Nuiqsut")

(sum(tag_id!="Nuiqsut") - colSums(surv[tag_id!="Nuiqsut",]==1, na.rm=T))/sum(tag_id!="Nuiqsut")
(colSums(surv[tag_id!="Nuiqsut",]==0, na.rm=T))/sum(tag_id!="Nuiqsut")

table(loc1_wider$`2`[tag_id!="Nuiqsut"])
sum(table(loc3_wider$`2`[tag_id!="Nuiqsut"]))

table(loc1_wider$`2`[tag_id!="Nuiqsut" & tag_id!="Kikiakrorak"])
sum(table(loc3_wider$`2`[tag_id!="Nuiqsut" & tag_id!="Kikiakrorak"]))

table(loc2_wider$`3`[tag_id=="Nuiqsut"])
sum(table(loc2_wider$`3`[tag_id=="Nuiqsut"]))
table(loc2_wider$`4`[tag_id=="Nuiqsut"])
sum(table(loc2_wider$`4`[tag_id=="Nuiqsut"]))


table(status_wider$`2`, useNA = "ifany")
table(surv$`2`, useNA = "ifany")
diff(colSums(surv, na.rm=T))
