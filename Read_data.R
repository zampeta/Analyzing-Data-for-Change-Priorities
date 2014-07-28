#---------------------------------------------------------------
# Changing Priorities Experiment
# Introduction to R 
#
# by Betty Kalogeropoulou
# 09.07.2014
#---------------------------------------------------------------

#---------------------------------------------------------------
# Remove everything from the workspace
rm(list=ls())

#Get current path
#curpath <- getwd()

#Read data
setwd("C:/Dokumente und Einstellungen/iView_X/Eigene Dateien/Polina/Change Priorities/Data2")
#---------------------------------------------------------------
#---------------------------------------------------------------
# Load interesting packages
library(ggplot2)
library(psyphy)
library(reshape)
library(CircStats)
library(circular)
#---------------------------------------------------------------
#---------------------------------------------------------------

# Define some colours
def.orange    <- rgb(252, 141, 89  ,max=255)
def.green     <- rgb(153, 213, 148 ,max=255)
def.bisque    <- rgb(255, 228, 196,max=255 )
def.azure     <- rgb(240, 255, 255,max=255 )
def.aliceblue <- rgb(240, 248, 255,max=255 )
def.palegreen <- rgb(151, 251, 152,max=255)
#---------------------------------------------------------------



#---------------------------------------------------------------
# Load data file and name columns
cat("\n","How many files would u like to read","\n") # prompt
    # numbfiles <- scan(n=1)
	numbfiles <- 1

#Put every graph into a matrix/list in this case
#R saves plots into lists
#fig_four <- list() 

for (numb in  1:numbfiles) {
	
	loadstr <- select.list(dir())
	data    <- read.table(loadstr)
	
	#The data file created will have these columns	
	colnames(data) <- c("BlockN","TrialN","RealTrialN","OriTarg","OriDistr","PrecueType","RetrocueType","PrecueColor","RetrocueColor","Stimulus2Ori","tFix","tPrecue",
	"tISI1","tStim1","tISI2","tRecue","tISI3","tStim2On","tStim2Off","tClr",
	"KeyRT1000","ChosenRes", "RespCorrect")

	#---------------------------------------------------------------
	if(numb == 1) {alldata <- data}
	else{alldata <- rbind(alldata,data)}
	}

#Compute deeprime
#import Functions
setwd("C:/Dokumente und Einstellungen/iView_X/Eigene Dateien/Polina/Analyzing data for Change Priorities")
source("deeprime.R")
alldata <- deeprime(alldata)
head(alldata)