deeprime <- function(alldata) {

#From Herrman et al. 2012

# 2.6.1. Psychophysics
# Performance, dâ€™ = z(hit rate)  z(false alarm rate), was computed
# for each observer across experimental sessions, separately
# for each contrast and each pre-cue (valid and invalid) (see also
# Herrmann et al., 2010). A hit was (arbitrarily) defined as counterclockwise
# response to counter-clockwise stimulus tilt and a false
# alarm as counter-clockwise response to clockwise stimulus. The
# psychometric alldata were fit with the Nakaâ€“Rushton equation
# (e.g., Ross and Speed, 1991) to the mean performance (across
# observers), using a nonlinear least-square procedure:



#---------------------------------------------------------------


#---------------------------------------------------------------
#Let's define conditions. We have 3 types of pre-cues (valid, invalid and neutral) and two types of retro-cues (valid and neutral).
#This is stored in two variables: PrecueType and RetrocueType. So there're 6 combinations of conditions overall:
# 		Pre		Retro
# 1). 	valid		valid
# 2). 	invalid	valid
# 3).	neutral	valid
# 4).	valid	neutral
# 5).	invalid	neutral
# 6).	neutral	neutral

# 		Pre		Retro
# 1). 	1		1
# 2). 	2		1
# 3).	0		1
# 4).	1		0
# 5).	2		0
# 6).	0		0

alldata["condition"] <- NA					#creates column 'condition'
for (i in 1:length(alldata$BlockN)) {	#applies condition name for a trial according to the precue type and the retrocue type, condition_num - numerical indexes of conditions
		
			if(alldata$PrecueType[i]==1 & alldata$RetrocueType[i]==1) {alldata$condition[i]	<-  ("ValidValid")}
			if(alldata$PrecueType[i]==2 & alldata$RetrocueType[i]==1) {alldata$condition[i]	<-  ("InvalidValid")}
			if(alldata$PrecueType[i]==0 & alldata$RetrocueType[i]==1) {alldata$condition[i]	<-  ("NeutralValid")}
			if(alldata$PrecueType[i]==1 & alldata$RetrocueType[i]==0) {alldata$condition[i]	<-  ("ValidNeutral")}
			if(alldata$PrecueType[i]==2 & alldata$RetrocueType[i]==0) {alldata$condition[i]	<-  ("InvalidNeutral")}
			if(alldata$PrecueType[i]==0 & alldata$RetrocueType[i]==0) {alldata$condition[i]	<-  ("NeutralNeutral")}
								
			if(alldata$PrecueType[i]==1 & alldata$RetrocueType[i]==1) {alldata$condition_num[i]	<-  (1)}
			if(alldata$PrecueType[i]==2 & alldata$RetrocueType[i]==1) {alldata$condition_num[i]	<-  (2)}
			if(alldata$PrecueType[i]==0 & alldata$RetrocueType[i]==1) {alldata$condition_num[i]	<-  (3)}
			if(alldata$PrecueType[i]==1 & alldata$RetrocueType[i]==0) {alldata$condition_num[i]	<-  (4)}
			if(alldata$PrecueType[i]==2 & alldata$RetrocueType[i]==0) {alldata$condition_num[i]	<-  (5)}
			if(alldata$PrecueType[i]==0 & alldata$RetrocueType[i]==0) {alldata$condition_num[i]	<-  (6)}

}
			alldata$condition		<- as.factor(alldata$condition)		#sets condition as a factor
#---------------------------------------------------------------	
#Add a column with the name, the session number and the orientation difference between stimulus 1 and stimulus 2 (response array)

		alldata$session        	<- substr(loadstr, 7, 8)
		alldata$name           	<- substr(loadstr, 5, 6)
		alldata$OriDifference		<- alldata$Stimulus2Ori-alldata$OriTarg 	#positive difference would indicate the counterclockwise (ccw) displacement, negative - clockwise (cw)
#---------------------------------------------------------------

# #---------------------------------------------------------------
# Signal detection variables ... d' = Z(HIT-Rate) - Z(FA-Rate)

		alldata$HIT  <- ifelse((alldata$OriDifference > 0) & alldata$RespCorrect == 1,1,0) 	# HIT is arbitrary defined as CCW response to CCW stimulus (as in Herrmann et al 2012)
		alldata$FA   <- ifelse((alldata$OriDifference < 0) & alldata$RespCorrect == 0,1,0)	# FA is a CCW response to a CW stimulus
		alldata$MISS <- ifelse((alldata$OriDifference > 0) & alldata$RespCorrect == 0,1,0)	# MISS is a CW response to a CCW stimulus
	  	alldata$CR   <- ifelse((alldata$OriDifference < 0) & alldata$RespCorrect == 1,1,0)	# CR is a CW response to a CW stimulus

#---------------------------------------------------------------

#---------------------------------------------------------------
# Get an idea whether there are more misses or false alarms
		alldata$four <- alldata$HIT + 2*alldata$FA + 3*alldata$MISS + 4*alldata$CR
		alldata$four <- as.factor(alldata$four)
		levels(alldata$four) <- c("HIT","FA","MISS","CR")	# makes a column which says whether a trial is HIT, FA, MISS or CR.


#---------------------------------------------------------------
# This dp is the file that holds the hits/false alarms/correct rejections/misses
		dp <- data.frame(condition_num=rep(0,6),hits=rep(0,6),fas=rep(0,6),dprime=rep(0,6),sum_ccw=rep(0,6),sum_cw=rep(0,6),name=rep (0,6),session=rep(0,6))

#----------------------------------------------------------------
count <- 0
#Conditions
	for (count in 1:6){
		for (i in 1:length(alldata$BlockN)) {
			if (alldata$condition_num[i] == count) {
				 dp$sum_ccw[count] 			<- dp$sum_ccw[count] + alldata$HIT[i] + alldata$MISS[i]
				 dp$sum_cw[count] 			<- dp$sum_cw[count] + alldata$FA[i] + alldata$CR[i]
				 dp$hits[count] 			<- dp$hits[count] + alldata$HIT[i] 
				 dp$fas[count] 				<- dp$fas[count] + alldata$FA[i]
				 dp$condition_num[count]	<- alldata$condition_num[i]
				 dp$name[count]				<- alldata$name[1]
				 dp$session[count]			<- alldata$session[1]

			}
		}
		 dp$dprime[count]	<- qnorm(dp$hits[count]/dp$sum_ccw[count], mean=0, sd=1) - qnorm(dp$fas[count]/dp$sum_cw[count], mean=0, sd=1)			
	}
	
	print(dp)
return(alldata)
	}
	
