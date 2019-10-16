####################################################
#### Script written by Milou Straathof          ####
#### Biomedical MR Imaging & Spectroscopy group ####
#### University Medical Center Utrecht          ####
#### M.Straathof-2@umcutrecht.nl                ####
####################################################

# load required packages (install with install.packages( "<name>" )

library( 'reshape2' ) 	# to convert wide -> long format with 'melt' function
library( 'plyr' ) 		# to summarize data.frames
library( 'nlme' )

####################################################################################
#####Only include interhemispheric connections & ipsilateral connections of VTA ####
####################################################################################

#File names pre-data
indir <- 'pre'

adlib.file.inter.pre <-  paste( indir, '/3_fc_zvalues_pre_adlib_animals_inter_inclS2.csv', sep = '')  
adlib.file.intra.pre <-  paste( indir, '/3_fc_zvalues_pre_adlib_animals_intra.csv', sep = '')  

FR.file.inter.pre <-  paste( indir, '/3_fc_zvalues_pre_FR_animals_inter_inclS2.csv', sep = '') 
FR.file.intra.pre <-  paste( indir, '/3_fc_zvalues_pre_FR_animals_intra.csv', sep = '')   

#Read in data files pre-data
d.adlib.inter.pre <- data.frame(read.table( adlib.file.inter.pre, sep=',', header=TRUE, dec=','))
d.adlib.intra.pre <- data.frame(read.table( adlib.file.intra.pre, sep=',', header=TRUE, dec=','))

d.FR.inter.pre <- data.frame(read.table(FR.file.inter.pre, sep=',', header=TRUE, dec=','))
d.FR.intra.pre <- data.frame(read.table(FR.file.intra.pre, sep=',', header=TRUE, dec=','))

#File names post-data
indir <- 'post'

adlib.file.inter.post <-  paste( indir, '/3_fc_zvalues_inbetween_adlib_animals_inter_inclS2.csv', sep = '')  
adlib.file.intra.post <-  paste( indir, '/3_fc_zvalues_inbetween_adlib_animals_intra.csv', sep = '')  

FR.file.inter.post <-  paste( indir, '/3_fc_zvalues_inbetween_FR_animals_inter_inclS2.csv', sep = '') 
FR.file.intra.post <-  paste( indir, '/3_fc_zvalues_inbetween_FR_animals_intra.csv', sep = '')   

#Read in data files post-data
d.adlib.inter.post <- data.frame(read.table( adlib.file.inter.post, sep=',', header=TRUE, dec="."))
d.adlib.intra.post <- data.frame(read.table( adlib.file.intra.post, sep=',', header=TRUE, dec='.'))

d.FR.inter.post <- data.frame(read.table(FR.file.inter.post, sep=',', header=TRUE, dec='.'))
d.FR.intra.post <- data.frame(read.table(FR.file.intra.post, sep=',', header=TRUE, dec='.'))


#Combine left and right ipsilateral connections from VTA to other ROIs into one column per connection
VTA.OFC.adlib.pre <- data.frame(VTA.OFC = c(d.adlib.intra.pre[,"VTAR.OFCR"], d.adlib.intra.pre[,"VTAL.OFCL"]))
VTA.NAcc.adlib.pre <- data.frame(VTA.NAcc = c(d.adlib.intra.pre[,"VTAR.NAccR"], d.adlib.intra.pre[,"VTAL.NAccL"]))
VTA.mPFC.adlib.pre <- data.frame(VTA.mPFC = c(d.adlib.intra.pre[,"VTAR.mPFCR"], d.adlib.intra.pre[,"VTAL.mPFCL"]))
VTA.NTS.adlib.pre <- data.frame(VTA.NTS = c(d.adlib.intra.pre[,"VTAR.NTSR"], d.adlib.intra.pre[,"VTAL.NTSL"]))
VTA.Ins.adlib.pre <- data.frame(VTA.Ins = c(d.adlib.intra.pre[,"VTAR.insulaR"], d.adlib.intra.pre[,"VTAL.insulaL"]))
VTA.MH.adlib.pre <- data.frame(VTA.MH = c(d.adlib.intra.pre[,"VTAR.MHR"], d.adlib.intra.pre[,"VTAL.MHL"]))
VTA.LH.adlib.pre <- data.frame(VTA.LH = c(d.adlib.intra.pre[,"VTAR.LHR"], d.adlib.intra.pre[,"VTAL.LHL"]))
VTA.Cpu.adlib.pre <- data.frame(VTA.CPu = c(d.adlib.intra.pre[,"VTAR.CPuR"], d.adlib.intra.pre[,"VTAL.CPuL"]))

VTA.OFC.FR.pre <- data.frame(VTA.OFC = c(d.FR.intra.pre[,"VTAR.OFCR"], d.FR.intra.pre[,"VTAL.OFCL"]))
VTA.NAcc.FR.pre <- data.frame(VTA.NAcc = c(d.FR.intra.pre[,"VTAR.NAccR"], d.FR.intra.pre[,"VTAL.NAccL"]))
VTA.mPFC.FR.pre <- data.frame(VTA.mPFC = c(d.FR.intra.pre[,"VTAR.mPFCR"], d.FR.intra.pre[,"VTAL.mPFCL"]))
VTA.NTS.FR.pre <- data.frame(VTA.NTS = c(d.FR.intra.pre[,"VTAR.NTSR"], d.FR.intra.pre[,"VTAL.NTSL"]))
VTA.Ins.FR.pre <- data.frame(VTA.Ins = c(d.FR.intra.pre[,"VTAR.insulaR"], d.FR.intra.pre[,"VTAL.insulaL"]))
VTA.MH.FR.pre <- data.frame(VTA.MH = c(d.FR.intra.pre[,"VTAR.MHR"], d.FR.intra.pre[,"VTAL.MHL"]))
VTA.LH.FR.pre <- data.frame(VTA.LH = c(d.FR.intra.pre[,"VTAR.LHR"], d.FR.intra.pre[,"VTAL.LHL"]))
VTA.Cpu.FR.pre <- data.frame(VTA.CPu = c(d.FR.intra.pre[,"VTAR.CPuR"], d.FR.intra.pre[,"VTAL.CPuL"]))

VTA.OFC.adlib.post <- data.frame(VTA.OFC = c(d.adlib.intra.post[,"VTAR.OFCR"], d.adlib.intra.post[,"VTAL.OFCL"]))
VTA.NAcc.adlib.post <- data.frame(VTA.NAcc = c(d.adlib.intra.post[,"VTAR.NAccR"], d.adlib.intra.post[,"VTAL.NAccL"]))
VTA.mPFC.adlib.post <- data.frame(VTA.mPFC = c(d.adlib.intra.post[,"VTAR.mPFCR"], d.adlib.intra.post[,"VTAL.mPFCL"]))
VTA.NTS.adlib.post <- data.frame(VTA.NTS = c(d.adlib.intra.post[,"VTAR.NTSR"], d.adlib.intra.post[,"VTAL.NTSL"]))
VTA.Ins.adlib.post <- data.frame(VTA.Ins = c(d.adlib.intra.post[,"VTAR.insulaR"], d.adlib.intra.post[,"VTAL.insulaL"]))
VTA.MH.adlib.post <- data.frame(VTA.MH = c(d.adlib.intra.post[,"VTAR.MHR"], d.adlib.intra.post[,"VTAL.MHL"]))
VTA.LH.adlib.post <- data.frame(VTA.LH = c(d.adlib.intra.post[,"VTAR.LHR"], d.adlib.intra.post[,"VTAL.LHL"]))
VTA.Cpu.adlib.post <- data.frame(VTA.CPu = c(d.adlib.intra.post[,"VTAR.CPuR"], d.adlib.intra.post[,"VTAL.CPuL"]))

VTA.OFC.FR.post <- data.frame(VTA.OFC = c(d.FR.intra.post[,"VTAR.OFCR"], d.FR.intra.post[,"VTAL.OFCL"]))
VTA.NAcc.FR.post <- data.frame(VTA.NAcc = c(d.FR.intra.post[,"VTAR.NAccR"], d.FR.intra.post[,"VTAL.NAccL"]))
VTA.mPFC.FR.post <- data.frame(VTA.mPFC = c(d.FR.intra.post[,"VTAR.mPFCR"], d.FR.intra.post[,"VTAL.mPFCL"]))
VTA.NTS.FR.post <- data.frame(VTA.NTS = c(d.FR.intra.post[,"VTAR.NTSR"], d.FR.intra.post[,"VTAL.NTSL"]))
VTA.Ins.FR.post <- data.frame(VTA.Ins = c(d.FR.intra.post[,"VTAR.insulaR"], d.FR.intra.post[,"VTAL.insulaL"]))
VTA.MH.FR.post <- data.frame(VTA.MH = c(d.FR.intra.post[,"VTAR.MHR"], d.FR.intra.post[,"VTAL.MHL"]))
VTA.LH.FR.post <- data.frame(VTA.LH = c(d.FR.intra.post[,"VTAR.LHR"], d.FR.intra.post[,"VTAL.LHL"]))
VTA.Cpu.FR.post <- data.frame(VTA.CPu = c(d.FR.intra.post[,"VTAR.CPuR"], d.FR.intra.post[,"VTAL.CPuL"]))

#Combine all ipsilateral connections into one dataframe
d.adlib.intra.select.pre <- cbind(VTA.OFC.adlib.pre, VTA.NAcc.adlib.pre, VTA.mPFC.adlib.pre, VTA.NTS.adlib.pre, VTA.Ins.adlib.pre, VTA.MH.adlib.pre, VTA.LH.adlib.pre, VTA.Cpu.adlib.pre)

d.FR.intra.select.pre <- cbind(VTA.OFC.FR.pre, VTA.NAcc.FR.pre, VTA.mPFC.FR.pre, VTA.NTS.FR.pre, VTA.Ins.FR.pre, VTA.MH.FR.pre, VTA.LH.FR.pre, VTA.Cpu.FR.pre)

d.adlib.intra.select.post <- cbind(VTA.OFC.adlib.post, VTA.NAcc.adlib.post, VTA.mPFC.adlib.post, VTA.NTS.adlib.post, VTA.Ins.adlib.post, VTA.MH.adlib.post, VTA.LH.adlib.post, VTA.Cpu.adlib.post)

d.FR.intra.select.post <- cbind(VTA.OFC.FR.post, VTA.NAcc.FR.post, VTA.mPFC.FR.post, VTA.NTS.FR.post, VTA.Ins.FR.post, VTA.MH.FR.post, VTA.LH.FR.post, VTA.Cpu.FR.post)

#Colnames interhemispheric connections
colnames(d.adlib.inter.pre) <- colnames(d.adlib.inter.post) <- c("VTA", "NAcc" , "Insula" , "MH", "LH", "mPFC", "OFC", "CPu", "NTS", "S2")
colnames(d.FR.inter.pre) <- colnames(d.FR.inter.post) <- c("VTA", "NAcc" , "Insula" , "MH", "LH", "mPFC", "OFC", "CPu", "NTS", "S2")

#Add variables rat, time, group
d.adlib.intra.select.pre$group  <- d.adlib.intra.select.post$group  <-"adlib"
d.adlib.intra.select.pre$rat <- d.adlib.intra.select.post$rat <- c(18,22,24,18,22,24)
d.adlib.intra.select.pre$time <- "pre"
d.adlib.intra.select.post$time <- "post"

d.FR.intra.select.pre$group <- d.FR.intra.select.post$group <- "FR"
d.FR.intra.select.pre$rat <- d.FR.intra.select.post$rat <- c(17,21,23,17,21,23)
d.FR.intra.select.pre$time <- "pre"
d.FR.intra.select.post$time <- "post"

d.adlib.inter.pre$group <- d.adlib.inter.post$group <- "adlib"
d.adlib.inter.pre$rat <- d.adlib.inter.post$rat <- c(18,22,24)
d.adlib.inter.pre$time <- "pre"
d.adlib.inter.post$time <- "post"

d.FR.inter.pre$group <- d.FR.inter.post$group <- "FR"
d.FR.inter.pre$rat <- d.FR.inter.post$rat <- c(17,21,23)
d.FR.inter.pre$time <- "pre"
d.FR.inter.post$time <- "post"

#Combine FR and adlib into one dataset
data.FR.adlib.intra <- rbind(d.FR.intra.select.pre, d.adlib.intra.select.pre, d.FR.intra.select.post, d.adlib.intra.select.post)
data.FR.adlib.inter <- rbind(d.FR.inter.pre, d.adlib.inter.pre, d.FR.inter.post, d.adlib.inter.post)

#Make group & time factors
data.FR.adlib.intra$group <- factor(data.FR.adlib.intra$group)
data.FR.adlib.intra$time <- factor(data.FR.adlib.intra$time)

data.FR.adlib.inter$group <- factor(data.FR.adlib.inter$group)
data.FR.adlib.inter$time <- factor(data.FR.adlib.inter$time)

#melt dataset to long format
melt.data.intra <- melt(data.FR.adlib.intra, id=c("group", "rat", "time"))
melt.data.inter <- melt(data.FR.adlib.inter, id=c("group", "rat", "time"))

#Combine inter and intrahemispheric connections
melt.data <- rbind(melt.data.intra, melt.data.inter)

#Calculate mean
mean.FC <- ddply( melt.data, .(time, group), summarise, mean.FC = mean( value ), sd.FC = sd( value ) )

####################################################
## Determine results for all connections together ##
####################################################

#Perform linear model (Over all connections!)
#correct for the fact that you include several connections measured in 1 animal.
group.effect.6ROIs <- lme(value ~ group*time, random = ~1 | rat/variable, data=melt.data)

#Check output table for fixed effects!
summary(group.effect.6ROIs)

#Determine 95% Confidence Intervals
data.group.6ROIs <- intervals(group.effect.6ROIs, which="fixed")

##############################################################################
## Determine results for inter- and intra-hemispheric connections seperately##
##############################################################################

#Calculate means
mean.inter <- ddply( melt.data.inter, .(time, group ), summarise, mean.FC = mean( value ), sd.FC = sd( value ) )
mean.intra <- ddply( melt.data.intra, .(time, group ), summarise, mean.FC = mean( value ), sd.FC = sd( value ) )

#Perform linear model for inter- and intra-hemispheric connections separately
#correct for the fact that you include several connections measured in 1 animal.
group.effect.6ROIs.inter <- lme(value ~ group*time, random = ~1 | rat/variable, data=melt.data.inter)
group.effect.6ROIs.intra <- lme(value ~ group*time, random = ~1 | rat/variable, data=melt.data.intra)

#Check output table for fixed effects!
summary(group.effect.6ROIs.inter)
summary(group.effect.6ROIs.intra)

#Determine CI interval
data.group.inter <- intervals(group.effect.6ROIs.inter, which="fixed")
data.group.intra <- intervals(group.effect.6ROIs.intra, which="fixed")


