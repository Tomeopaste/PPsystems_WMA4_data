################################################################################
##### Script for turning the text output of PuTTy, reading data from ###########
##### a PP-system WMA-4 CO2-analyzer, into something useable         ###########
################################################################################

## The data is transfered to a PC via RS232-null-modem cable. The terminal app PuTTy
## recieves the data and dumps it to a text file. The data in the file consists of one 
## column lacking delimeters between the variables and staggered regularly throughout are 
## rows containing data on the zeroing of the IRGA. The script below seperates out the 
## rows containing actual data (non-zeroing information), then pulls out the
## the variables of interest (month, day, hour, minute of observation and the [CO2] in 
## ppm). Finally a new data file is written with just this information as one-minute
##	averages. 


################################################################################
# If not operating in a project framework, set the working directory:

setwd("C:/WMA4_Data")

# Change file path to select the file that needs processing:
################################################################################
putty1 = read.table(
					file="PuTTyOutput/Chamber3_22Feb_2016.log",
                    skip=1, sep="", blank.lines.skip=TRUE, colClasses="character")
                    
# Identifying information. For our data we are measuring multiple growth chambers
#	set to different treatments, hence we want the number of the chamber and the treatment
#	in the final data file for later use. Other ID info could be used.                     
################################################################################

# What is the chamber number for this data? 

chamber = 3 

# What was the treatment that this chamber was subject to for this time period?
# NB: make sure thisis surrounded by quotes so R knows it is a character string.

treat = "ACET"
################################################################################
################################################################################


################################################################################
################################################################################
################################################################################
## All of the code below should be run by highlighting and clicking the "Run" ##
##     button above. No need to even look at it.                              ##
################################################################################
################################################################################
################################################################################
################################################################################

# load dependent packages 
library("dplyr")
library("lubridate")


#create logical column indicating rows to keep (TRUE's)
putty1[,2] = grepl("^[^Z]",putty1[,1])
#Selects out the rows we want (those without IRGA Zeroing data)
putty2 = subset(putty1[,1], putty1[,2]==TRUE) 
                                              
co2 = as.double(substring(putty2, 16, 20)) #extracts CO2 reading from each row
day = as.double(substring(putty2,8,9)) #extracts the day of month 
month = as.double(substring(putty2,10,11)) #extracts month
hour = as.double(substring(putty2,12,13)) #extracts hour (24h time)
minute = as.double(substring(putty2,14,15)) #extracts minute
rh.mv = as.double(substring(putty2,40,43)) #extracts millivolt signal for RH
temp.mv = as.double(substring(putty2,44,47)) #extracts millivolt signal for temperature
## NB: the next two lines are specific to Viasala HMP60 dual temp and RH probes.
RH = rh.mv/10	#calculates relative humidity (as %)
Temp = -40+temp.mv/10	#calculates temperature (ºC)
#create new dataframe with date-time, [co2], temp, and RH
co2measure = data.frame(day,month,hour,minute,co2,Temp,RH) 


################################################################################
# make a date formatted column from the month and day columns (currently formatted as
#	numeric)
co2measure$date = as.character(paste(co2measure$month, co2measure$day, sep="/"))
                          #,format = "%m/%d")
co2measure$time = paste(co2measure$hour,co2measure$minute,
                        rep("00",length(co2measure$minute)), sep=":")
co2measure$dt = strptime(paste(co2measure$date,co2measure$time, sep=" "),
                              format="%m/%d %H:%M:%S")
full = data.frame(DateTime = co2measure$dt, CO2 = co2measure$co2,
                        RH = co2measure$RH, Temp = co2measure$Temp, 
                  Month = co2measure$month, Day=co2measure$day, 
                  Date=co2measure$date, Time = co2measure$time)

# Clean up some of the excess objects in the environment
deletes = c("putty1","putty2", "co2", "day", "month", "hour", "minute", 
            "rh.mv","temp.mv", "RH", "Temp", "co2measure")
rm(list=deletes)

################################################################################
################################################################################
### summarise into 1-minute chunks taking the mean minute:
# 1-[CO2], 2-Temp, 3-RH

outputs = full %>%
      group_by(DateTime = cut(DateTime, breaks="1 min")) %>%
      summarize(CO2 = signif(mean(CO2),digits=4),
                RH = signif(mean(RH),digits=4),
                Temp = signif(mean(Temp),digits=4))

# Add identifying info columns
outputs$Chamber = rep(chamber, length(outputs$Temp))
outputs$Treatment = rep(treat, length(outputs$Temp))

# We wanted all of the date & time info seperated into their own columns for some reason
#	I cannot recall at the moment. So, these lines do that...
outputs$Month = month(outputs$DateTime)
outputs$Day = day(outputs$DateTime)
outputs$Time = substring(outputs$DateTime, 12, 16)
outputs$Year = year(outputs$DateTime)


################################################################################
########	Write the file												########
################################################################################
write.csv(outputs, file=paste0("FinalDataFiles/",treat,"_Begins_",
                              outputs$Year[1],"_",outputs$Month[1],
                              "_",outputs$Day[1],"_Chamber",chamber,".csv"),
          row.names = FALSE)
