# ###################################################################
# #
# # Khan
# # 12 Feb 2013
# #
# # Introduction:
# # We got preliminary data from the khan academy.  Here's the
# # relevant email: 
# # https://mail.google.com/mail/u/0/#inbox/13cccec0c854c7f9
# #
# # I want to load it, clean it, and play around just a bit.

	# # Libraries
	# library(plyr)
	# library(ggplot2)
	# library(gbm)

	# # Constants
	# headers = c(
		# 'study', 
		# 'group', 
		# 'id', 
		# 'time', 
		# 'exercise', 
		# 'type', 
		# 'seed', 
		# 'attempts', 
		# 'mode', 
		# 'correct', 
		# 'proficiency', 
		# 'hints', 
		# 'elapsed'
	# )
	
	# # Data
	# small <- read.csv('~/Downloads/gm.small.csv', header=F)
	# names(small) <- headers
	
	
	# # Histograms
	# .d <- data.frame(llply(small, as.numeric))
	# .d$id <- 1:nrow(small)
	# m <- melt(.d, id.vars='id')
	# ggplot(m, aes(value)) + 
		# geom_density() + 
		# facet_wrap(~variable, scales='free')
		
	# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-12%20at%204.10.31%20PM.png
	
	# # Two way scatter
	# .d <- head(small, 1000)
	# .d <- data.frame(llply(.d, as.numeric))
	# .d <- data.frame(llply(.d, function(x) log(x + 1)))
	# .d <- data.frame(scale(.d))
	# .d$id <- 1:nrow(small)
	# m <- melt(.d, id.vars='id')
	# cross <- ddply(m, .(variable), function(df){data.frame(
		# x=df$variable,
		# y=m$variable,
		# x.value=df$value,
		# y.value=m$value
	# )})
	# ggplot(cross, aes(x.value, y.value)) + 
		# geom_point(alpha=0.01, size=1) + 
		# facet_grid(y ~ x) +
		# ylim(-3, 3) + 
		# xlim(-3, 3)
		
	# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-12%20at%204.31.05%20PM.png
	
# # At a gross level the data is rather orthoganal to one another
# # more careful inspection will be necessary to discern the primary
# # patterns with better resolution.



# ###################################################################
# #
# # GBM
# # 12 Feb 2013
# #
# # Introduction:
# # Just a quick GBM to get a sense of which varaibles are relevant
# # to our outcomes, correct, and proficiencies.

# # Libraries
	# library(plyr)
	# library(ggplot2)
	# library(gbm)

	# # Constants
	# headers = c(
		# 'study', 
		# 'group', 
		# 'id', 
		# 'time', 
		# 'exercise', 
		# 'type', 
		# 'seed', 
		# 'attempts', 
		# 'mode', 
		# 'correct', 
		# 'proficiency', 
		# 'hints', 
		# 'elapsed'
	# )
	
	# # Data
	# medium <- read.csv('~/Downloads/gm.medium.csv', header=F)
	# names(medium) <- headers
	
	# .d <- medium[
		# !is.na(medium$correct) & 
		# !is.na(medium$proficiency),
	# ]
	# .d <- .d[sample(nrow(.d)), ]
	# .d <- .d[,!names(.d) %in% c('id')]
	# m <- gbm(
		# correct ~ .,
		# data=.d,
		# n.trees=2000,
		# cv.folds=2,
		# interaction.depth=2,
		# shrinkage=0.2
	# )
	# trees <- gbm.perf(m, method="cv")
	# print(trees)
	# m$cv.error[trees]
	# summary(m, n.trees=trees)
	
	# # perform	trees	shrinkage	interaction
	# # 0.595		1510	0.2			2
	
	# # We seem to be approaching a real performance limit
	# # group matters with relative influence of 0.8%
	# # Hints are by far the most important
	# #
	# # Notably we do substantially better when we do not
	# # consider id, than when we do.  Surely this is a 
	# # failure in the model, it seems likely to overfit
	# # on id.
	

# ###################################################################
# #
# # Individuals
# # 13 Feb 2012
# #
# # Introduction:
# # We really want to perform our anlaysis at the level of individuals
# # because this is the level of group assignment.  Here we will 
# # explore what several specific individuals look like and think
# # of ways to see what they are up to in aggregate.

	# # Libraries
	# library(plyr)
	# library(ggplot2)
	# library(gbm)

	# # Constants
	# headers = c(
		# 'study', 
		# 'group', 
		# 'id', 
		# 'time', 
		# 'exercise', 
		# 'type', 
		# 'seed', 
		# 'attempts', 
		# 'mode', 
		# 'correct', 
		# 'proficiency', 
		# 'hints', 
		# 'elapsed'
	# )
	
	# # Data
	# medium <- read.csv('~/Downloads/gm.medium.csv', header=F)
	# names(medium) <- headers
	
	# # Simplify ids
	# length(unique(medium$id))		# 1000
	# medium$id <- as.factor(sub('^.{52}', '', medium$id))
	# length(unique(medium$id))		# still 1000
	
	# # Examples
	# set.seed(1)
	# examples <- sample(unique(medium$id))
	
	# ex1 <- medium[medium$id == examples[1],]
	
	# # Surprisingly this person appears in two studies
	# # growth-mindset and growth-mindset-subtest
	# unique(ex1$study)

	# # In fact the time stamps reveal that their data
	# # appears twice!
	# plot(ex1$time)
	
	# # Ok, so this makes it clear that the subtest
	# # sometimes, but not always repeats data
	# # I'm not quite sure why, but its ressuring to 
	# # know the scope of the problem.
	# table(paste(medium$study, medium$group), medium$id)
	
	# # From attempts and correct we can see a clear story
	# # ex1 missed their first effort and nailed all the 
	# # rest.  Its surprising ex1 didn't bother to achieve
	# # profeciency when it is rather evident to me that 
	# # they understood the drill.
	# ex1
	
	# # From this first analysis I am quite convinced that
	# # by adding previous attempt correctnesses to the 
	# # gbm, I will get a much better prediction of whether
	# # the next one will be correct.  Lets try that before
	# # returning to this analysis.
	
	
# ###################################################################
# #
# # GBM with history
# # 12 Feb 2013
# #
# # Introduction:
# # Add previous attempt to our gbm and watch the good times roll.

# # Libraries
	# library(plyr)
	# library(ggplot2)
	# library(gbm)

	# # Constants
	# headers = c(
		# 'study', 
		# 'group', 
		# 'id', 
		# 'time', 
		# 'exercise', 
		# 'type', 
		# 'seed', 
		# 'attempts', 
		# 'mode', 
		# 'correct', 
		# 'proficiency', 
		# 'hints', 
		# 'elapsed'
	# )
	
	# # Data
	# medium <- read.csv('~/Downloads/gm.medium.csv', header=F)
	# names(medium) <- headers
	
	# # Add previous
	# medium <- ddply(medium, .(id, exercise), function(df){
		# df$previous <- NA
		# df$previous2 <- NA
		# if(nrow(df) > 1){
			# df$previous <- c(NA, df$correct[1:(nrow(df)-1)])		
		# }
	# })
	
	# .d <- medium[
		# !is.na(medium$correct) & 
		# !is.na(medium$proficiency),
	# ]
	# .d <- .d[sample(nrow(.d)), ]
	# .d <- .d[,!names(.d) %in% c('id')]
	# m <- gbm(
		# correct ~ .,
		# data=.d,
		# n.trees=2000,
		# cv.folds=2,
		# interaction.depth=2,
		# shrinkage=0.2
	# )
	# trees <- gbm.perf(m, method="cv")
	# print(trees)
	# m$cv.error[trees]
	# summary(m, n.trees=trees)
	
	# # perform	trees	shrinkage	interaction		notes
	# # 0.595		1510	0.2			2
	# # 0.587		1501	0.2			2				+ previous
	# # 0.588		1918	0.2			2				+ previous2
	
# # Results
# # Previous accounts for some, but not as much as I might have 
# # thought.  Adding a second previous value taught me nothing.



# # ###################################################################
# #
# # More Individuals
# # 13 Feb 2012
# #
# # Introduction:
# # Here I will continue my work exploring individual results.
# # I will add a section for the sub-study so that the data does
# # not repeat and so that it appears as a column instead.

	# # Libraries
	# library(plyr)
	# library(ggplot2)
	# library(gbm)
	# library(stringr)

	# # Constants
	# headers = c(
		# 'study', 
		# 'group', 
		# 'id', 
		# 'time', 
		# 'exercise', 
		# 'type', 
		# 'seed', 
		# 'attempts', 
		# 'mode', 
		# 'correct', 
		# 'proficiency', 
		# 'hints', 
		# 'elapsed'
	# )
	
	# # Data
	# data <- read.csv('~/Downloads/gm.medium.csv', header=F)
	# names(data) <- headers
	
	# # Simplify ids
	# length(unique(data$id))		# 101
	# data$id <- as.factor(str_extract(data$id, '.{6}$'))
	# length(unique(data$id))		# still 101
	
	# # Make subtest a column
	# table(data$group, data$study)
	# data <- ddply(data, .(id), function(df){
		# df$subgroup <- NA
		# subtest <- df$study == 'growth-mindset-subtest'
		# if(nrow(df[subtest,]) > 0){
			# df$subgroup <- as.character(df[subtest,]$group[1])
		# }
		# df[!subtest,]
	# })
	# table(data$group, data$study)
	# table(data$group, data$subgroup, useNA='ifany')

	
	
	# # Plot experience over time
	# g <- data[data$id %in% examples[1:1],]
	# g <- ddply(g, .(id), function(df){
		# df <- df[order(sort(df$time)),]
		# df$n <- 1:nrow(df)
		# df
	# })
	
	# ggplot(g, aes(
		# x=time/(60*60), 
		# y=n,
		# color=factor(paste(proficiency, correct)),
		# alpha=proficiency + (1-correct) + 0.001,
	# )) + 
		# geom_point() + 
		# facet_wrap(~group + id) +
		# xlim(377413, 377417)
	
	# # Experience Over Time
	# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-13%20at%203.41.59%20PM.png
	# # This seems to represent a pretty typical user 
	# # experience, they work for some amount of hours at
	# # a stretch achieving periodic proficiencies.
	

	# # Make time relative to the intervention
	# g <- data[,]
	# g$treatment <- grepl('fraction', g$exercise)
	# g <- g[which(g$proficiency == 1),]
	# g <- ddply(g, .(id), function(df){
		# df <- df[order(sort(df$time)),]
		# df$n <- 1:nrow(df)
		# if(sum(df$treatment) == 0){
			# df$time <- NA
		# } else {
			# first <- min(which(df$treatment))
			# df$time <- df$time - df$time[first]
			# df$n <- df$n - df$n[first]
		# }
		# df
	# })
	# g <- g[!is.na(g$time),]
	
	# ggplot(g, aes(
		# x=time/(60), 
		# y=n,
		# color=group
	# )) + 
		# geom_point(alpha=0.5, size=1) + 
		# geom_smooth(method='lm') +
		# ylim(-20, 20)
		
		

# ###################################################################
# #
# # Proficiencies
# # 13 Feb 2012
# #
# # Introduction:
# # The previous examples taught me that it is useful to think of
# # events as before or after the intervention.  With this insight
# # I have composed a dataset where we aggregate events by summing
# # depending on whether they occured before or after the first
# # intervention.
# #
# # I will use this set to try to predict major outcomes.

	# # Libraries
	# library(plyr)
	# library(ggplot2)
	# library(gbm)
	# library(stringr)
	# library(reshape2)
	
	# # Data
	# data <- read.csv('~/Downloads/gm.aggregate.csv')
	
	# # Remove No intervention
	# nrow(data)			# 118580
	# data <- data[data$intervention_time != 0,]
	# nrow(data)			# 104692
	
	# # Show outcomes
	# m <- data
	# m$treatment <- paste(m$group)
	# m <- m[,names(m) %in% c(
		# 'treatment', 
		# 'start',
		# 'end',
		# 'intervention_time',
		# 'interventions',
		# 'p_exercises',
		# 'a_exercises',
		# 'p_attempts',
		# 'a_attempts',
		# 'p_correct',
		# 'a_correct',
		# 'p_proficiency',
		# 'a_proficiency',
		# 'p_hints',
		# 'a_hints'
	# )]
	# m$intervention_time <- m$intervention_time / 3600
	# m$start <- m$start / 3600
	# m$end <- m$end / 3600
	# m <- melt(m, id.vars=c('treatment'))
	# ggplot(
		# m, 
		# aes(sign(value)*log(abs(value)+1, base=10), color=treatment)
	# ) +
		# geom_density() + 
		# facet_wrap(~variable, scales='free')
		
	# # All histograms
	# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-14%20at%205.06.48%20PM.png
	
	# s <- m[m$variable=='a_correct',]
	# ggplot(s, aes(x=treatment, y=log(value+1, 10))) +
		# geom_boxplot(alpha=0.5)
		
	# # Predicting
	# m <- gbm(
		# a_proficiency ~ 
			# group + 
			# subtest + 
			# start + 
			# intervention_time + 
			# p_expercises +
			# p_attempts +
			# p_correct +
			# p_proficiency +
			# p_hints,
		# distribution='gaussian',
		# data=data,
		# n.trees=1000,
		# interaction.depth=3,
		# shrinkage=0.01,
		# cv.folds=2
	# )
	# trees <- gbm.perf(m, method="cv")
	# print(trees)
	# m$cv.error[trees]
	# summary(m, n.trees=trees)
	# data$p <- predict(m, n.trees=trees, newdata=data)

	# ggplot(data, aes(a_proficiency, p)) + 
		# geom_point(size=1)
	
	# # performance	trees	interactions	shrinkage
	# # 109.3			10		2				0.5	
	# # 108.3			18		2				0.5	
	# # 105.8			85		2				0.1
	# # 105.7			69		3				0.1
	# # 104.5			792		3				0.01
	
	# # predictions vs actual
	# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-15%20at%2010.33.14%20AM.png

	# # Its pretty obvious that the result varies as performance
	# # grows higher.  Perhaps we should try to predict a log
	# # transform instead.	
	
	# data$o <- log(data$a_proficiency+1)
	# m <- gbm(
		# o ~ 
			# group + 
			# subtest + 
			# start + 
			# intervention_time + 
			# p_expercises +
			# p_attempts +
			# p_correct +
			# p_proficiency +
			# p_hints,
		# distribution='gaussian',
		# data=data,
		# n.trees=500,
		# interaction.depth=3,
		# shrinkage=0.1,
		# cv.folds=2
	# )
	# trees <- gbm.perf(m, method="cv")
	# print(trees)
	# m$cv.error[trees]
	# summary(m, n.trees=trees)
	# data$p <- predict(m, n.trees=trees, newdata=data)

	# # performance	trees	interactions	shrinkage
	# # 0.720			99		3				0.1
	# # 0.717			474		3				0.1
	# # 0.717			404		4				0.1
	# # 0.718			463		2				0.1

	# # 1 intervention_time 52.97052456
	# # 2     p_proficiency 35.16325592
	# # 3             start  5.72089233
	# # 4      p_expercises  3.65594237
	# # 5         p_correct  1.57109644
	# # 6        p_attempts  0.44799661
	# # 7           p_hints  0.40300261
	# # 8             group  0.05245956
	# # 9           subtest  0.01482960

	# ggplot(data, aes(o, p)) + 
		# geom_point(
			# size=1, 
			# alpha=0.1
		# ) +
		# geom_smooth()
		
	# # predictions vs actual
	# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-15%20at%2010.50.37%20AM.png
	# # looks more reasonable!  That is varaince does not increase
	# # a lot with the size of the outcome.


	# ggplot(data, aes(intervention_time/(60*60*24), o)) + 
		# geom_point(
			# size=1, 
			# alpha=0.1
		# ) +
		# geom_smooth()	
		
	# # proficiencies over time
	# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-15%20at%2011.11.35%20AM.png
	# # Here we see two important effects.  1 there are some dates
	# # that are probably erroneous.  2. the numer of proficiencies
	# # declines dramatically with time.  This emphasizes what dave
	# # suggested, looking forward a fixed amount of time from
	# # the intervention, like one week or so.  The only caveat is
	# # that we need to be sure that at least one week has passed
	# # for this purpose it will be important to find the latest
	# # date in the dataset and check that the intervention happend
	# # at least one week before that.  But even latest date might
	# # be inappropriate because some of these values seem erroneous
	# # so we might want to use the date within 5% of the latest.
	
	
##################################################################
#
# Khan's Revenge
# 22 Feb 2013
#
# Introduction:
# We got new data.  See 
# https://mail.google.com/mail/u/0/#inbox/13cdf1651581896c
# Time to go spelunking.

	# Libraries
	library(plyr)
	library(ggplot2)
	library(gbm)
	library(stringr)
	library(reshape2)
	
	# Load
	data <- read.csv('~/Downloads/gm.aggregate.csv')

	# Elaborate
	data$study <- as.factor(paste(data$group, data$subtest))


	# Shuffle
	data <- data[sample(nrow(data)),]
	
	
	# What predicts Study?
	for(s in unique(data$study)){
		m <- gbm(
			study == s ~ 
				study + 
				start + 
				end + 
				intervention_time + 
				interventions + 
				p_exercises + 
				a_exercises + 
				p_attempts + 
				a_attempts + 
				p_correct + 
				a_correct + 
				p_proficiency + 
				a_proficiency + 
				p_hints + 
				a_hints,
			data=data,
			n.trees=100,
			interaction.depth=3,
			shrinkage=0.01,
			cv.folds=2,
			verbose=F
		)
		print(gbm.perf(m, method="cv"))
		trees <- gbm.perf(m, method="cv")
		print(s)
		print(trees)  
		print(m$cv.error[1])
		print(m$cv.error[trees])
		print(summary(m))
	}
	

##################################################################
#
# Why broken
# 24 Feb 2013
#
# Introduction:
# My previous analysis showed that we have no ability to predict
# study group based on the other factors in the aggregated data.
# 
# My top suspicion is that this is because that data is wrong
# somehow.  To test that idea I will see how well my data matches
# with the results on the testing dashboard.


	# Libraries
	library(plyr)
	library(ggplot2)
	library(gbm)
	library(stringr)
	library(reshape2)
	
	# Load
	data <- read.csv('~/Downloads/gm.aggregate.csv')


	# Can reproduce the dashboard data
	result <- ddply(data, .(group), function(df){c(
		n=nrow(df),
		new_proficiency=sum(df$a_proficiency > 0, na.rm=T),
		any_proficiency=sum(
			df$a_proficiency > 0 | 
			df$p_proficiency > 0, 
		na.rm=T),
		new_proficiency_count=sum(df$a_proficiency, na.rm=T),
		any_proficiency_count=sum(
			df$a_proficiency, 
			df$p_proficiency, 
			na.rm=T
		)		
	)})
	
	percents <- result / result$n
	percents$group <- result$group	
	percents$n <- result$n	

	# New Proficiencies
	# I could not reproduce the data on new proficiencies.
	# 
	# Firstly, the dashboard count has 1.56-1.54 times as many
	# participants per group.  For example there were 163427
	# participants listed in the 'no header' group on the dashboard
	# and only 105731 in my data.
	#
	# Secondly my data says that new proficiencies are obtained
	# at a substantially higher rate, ~ 60% vs ~35% on the dashboard
	# 
	# Thirdly my counts of new proficiencies are higher than the
	# counts from the dashboard by about 11%
	
	
	data$condition <- as.character(data$group)
	data$condition[data$group %in% c(
		'growth mindset + link',
		'growth mindset'
	)] <- 'mindset'
	data$condition <- as.factor(as.character(data$condition))
	ggplot(data, aes(p_correct, color=condition)) + 
		geom_density() +
		xlim(0, 1000)
		

##################################################################
#
# Reproduction
# 25 Feb 2013
#
# Trying to reproduce some of the conclusions from Dave and Joseph
#

	# Load
	data <- read.csv('~/Downloads/gm.aggregate.csv')

	# Add columns
	data$condition <- as.character(data$group)
	data$condition[data$group %in% c(
		'growth mindset + link',
		'growth mindset'
	)] <- 'mindset'
	data$condition <- as.factor(as.character(data$condition))


	# Joseph's badly aligned graph
	r <- ddply(data, .(a_proficiency, condition), function(df){
		condition <- as.character(df$condition[1])
		c(
			condition=condition, 
			n=nrow(df), 
			p=nrow(df)/sum(data$condition == condition)
		)})
	ggplot(
		r[r$a_proficiency < 6,], 
		aes(x=condition, y=as.numeric(p), fill=condition)
	) + geom_bar() + facet_wrap(~a_proficiency)



##################################################################
#
# Clustering
# 25 Feb 2013
#
# See what data looks too weird
#

	# Load
	data <- read.csv('~/Downloads/gm.aggregate.csv')

	# Add columns
	data$condition <- as.character(data$group)
	data$condition[data$group %in% c(
		'growth mindset + link',
		'growth mindset'
	)] <- 'mindset'
	data$condition <- as.factor(as.character(data$condition))


	numeric <- c(
		"start", 
		"end", 
		"intervention_time", 
		"interventions", 
		"p_exercises", 
		"a_exercises", 
		"p_attempts", 
		"a_attempts", 
		"p_correct", 
		"a_correct", 
		"p_proficiency", 
		"a_proficiency", 
		"p_hints", 
		"a_hints" 
	)
	
	
	# Clustering
	means <- sapply(data[,numeric], mean)
	sds <- sapply(data[,numeric], sd)
	scaled <- data[,numeric]
	for(i in 1:ncol(scaled)) scaled[,i] <- (scaled[,i] - means[i])/sds[i]
	clusters <- kmeans(scaled, 10, nstart=10)
	centers <- clusters$center
	for(i in 1:ncol(centers)) centers[,i] <- centers[,i]*sds[i] + means[i]
	centers
	
	# Non negative start time (3, 5, 8)
	# More than 1000 interventions (2)
	# Newbies p_proficiency < 1 (6)
	
	sum(data$start > 0)  				# 35181
	sum(data$interventions > 1000)		# 26
	sum(data$p_proficiency < 1)			# 118699



##################################################################
#
# Graphing
# 25 Feb 2013
#
# After removing the weird data, what do things look like?
#
	# Libraries
	library(plyr)
	library(ggplot2)
	library(gbm)
	library(stringr)
	library(reshape2)
	
	# Load
	data <- read.csv('~/Downloads/gm.aggregate.csv')

	# Add columns
	data$condition <- as.character(data$group)
	data$condition[data$group %in% c(
		'growth mindset + link',
		'growth mindset'
	)] <- 'mindset'
	data$condition <- as.factor(as.character(data$condition))

	numeric <- c(
		"start", 
		"end", 
		"intervention_time", 
		"interventions", 
		"p_exercises", 
		"a_exercises", 
		"p_attempts", 
		"a_attempts", 
		"p_correct", 
		"a_correct", 
		"p_proficiency", 
		"a_proficiency", 
		"p_hints", 
		"a_hints" 
	)
	
	# Use data that seems normal
	subset <- data[
		(data$start < 0) &
		(data$interventions < 1000) &
		(data$p_proficiency >= 1),
	]
	
	# Make time in days
	subset$start <- subset$start / (60*60*24)
	subset$end <- subset$end / (60*60*24)
	subset$intervention_time <- subset$intervention_time / (60*60*24)

	# Histograms
	m <- melt(subset[,numeric])
	ggplot(m, aes(value)) + 
		geom_density(adjust=2) + 
		facet_wrap(~variable, scale='free') + 
		scale_x_log10()
		
	# Regular scales
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-25%20at%201.34.49%20PM.png
	
	# Log scales
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-25%20at%201.37.10%20PM.png
	# Note: Logging makes a weird waviness for low values
	#       this does not represent the data, but just an 
	#       artifact.  Ignore it.
	
	
	# Two way scatters
	reduced_numeric <- c(
		'start', 
		'end', 
		'interventions',
		'p_attempts', 
		'a_attempts', 
		'p_correct', 
		'a_correct'
	)
	s <- subset[sample(nrow(subset), 10000),reduced_numeric]
	m <- melt(s)
	two_way <- ddply(m, .(variable), function(df) data.frame(df, m))	
	
	ggplot(two_way, aes(x=log(abs(value), base=10), y=log(abs(value.1), base=10))) + 
		geom_point(size=0.5, alpha=0.5) + 
		facet_grid(variable.1~variable, scale='free') + 
		geom_density2d(alpha=0.5)

		
	
	# Regular scales
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-26%20at%2011.08.19%20AM.png

	# Log scale
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-26%20at%2011.12.53%20AM.png
	
	g <- data[data$start <= 0,]
	ggplot(g, aes(start/(60*60*24), end/(60*60*24))) + 
		geom_point(size=0.5, alpha=0.5) + 
		geom_density2d(alpha=0.5)

	g <- subset
	ggplot(g, aes(start/(60*60*24), end/(60*60*24))) + 
		geom_point(size=0.5, alpha=0.5) + 
		geom_density2d()
					
	# Weird relationship between start and end times
	# All data
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-26%20at%2011.14.47%20AM.png
	#
	# Subset
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-26%20at%2011.16.39%20AM.png
	
	



##################################################################
#
# Reproducing Joseph
# 26 Feb 2013
#
# I want to prove that I can reproduce Joseph's Mann Witney-U
# test result on the number of proficiencies achieved, without
# peaking.
#
	
	# Load
	data <- read.csv('~/Downloads/gm.aggregate.csv')

	# Add columns
	data$condition <- as.character(data$group)
	data$condition[data$group %in% c(
		'growth mindset + link',
		'growth mindset'
	)] <- 'mindset'
	data$condition <- as.factor(as.character(data$condition))

	# Test
	r <- wilcox.test(
		x=data$a_proficiency[data$condition == 'mindset'],
		y=data$a_proficiency[!data$condition == 'mindset']
	)
	r$p.value  # 0.023

	# With start time
	r <- wilcox.test(
		x=data$a_proficiency[
			data$condition == 'mindset' &
			data$intervention_time != 0
		],
		y=data$a_proficiency[
			!data$condition == 'mindset' &
			data$intervention_time != 0
		]
	)
	r$p.value  # 0.061
	
	
	# Total Proficiencies
	p <- data$a_proficiency + data$p_proficiency
	r <- wilcox.test(
		x=p[data$condition == 'mindset'],
		y=p[!data$condition == 'mindset']
	)
	r$p.value  # 0.048	
	
	
	# Permutation
	cond <- data$condition == 'mindset'
	ps <- ldply(1:1000, function(i){
		shuffled <- sample(data$a_proficiency)
		r <- wilcox.test(
			x=shuffled[cond],
			y=shuffled[!cond]
		)
		if(i %% 20 == 0){ print(i) }
		r$p.value
	})
	sum(ps < 0.05) / nrow(ps)		# 0.052, as expected
	
	# * I tried it on the subsets, it seems that the
	# effect is distributed through both.


	# False Negative permutation
	cond <- data$condition == 'mindset'
	ps <- ldply(1:1000, function(i){
		shuffled <- sample(nrow(data), nrow(data), replace=T)
		d <- data[shuffled,]
		r <- wilcox.test(
			x=d$a_proficiency[d$condition == 'mindset'],
			y=d$a_proficiency[!d$condition == 'mindset']
		)

		if(i %% 20 == 0){ print(i) }
		r$p.value
	})
	sum(ps < 0.05) / nrow(ps)		# 0.609, our apparent power
		
	
	# All proficiencies
	prof <- data$a_proficiency + data$p_proficiency
	r <- wilcox.test(
		x=prof[data$condition == 'mindset'],
		y=prof[!data$condition == 'mindset']
	)
	r$p.value  # 0.04!


	# Only before
	prof <- data$p_proficiency
	r <- wilcox.test(
		x=prof[data$condition == 'mindset'],
		y=prof[!data$condition == 'mindset']
	)
	r$p.value  # 0.606

# Results
#
# The p value was reproduced pretty well!
#
# Notably I also found that even the sum of previous proficiencies
# and after proficiencies was also signficantly increasead which
# implies that the time break point probably does not matter.



##################################################################
#
# My Effort
# 26 Feb 2013
#
# Based on the result that our data is exp(normal) I wanted to
# run some basic tests on the log of the number of exercises
#

	# Load
	data <- read.csv('~/Downloads/gm.aggregate.csv')

	# Add columns
	data$condition <- as.character(data$group)
	data$condition[data$group %in% c(
		'growth mindset + link',
		'growth mindset'
	)] <- 'mindset'
	data$condition <- as.factor(as.character(data$condition))

	# Experienced users
	experienced <- data[
		(data$start < 0) &
		(data$interventions < 1000) &
		(data$p_proficiency >= 1),
	]

	## Proficiencies ##
	# Control (without the log)
	t.test(
		x=data$a_proficiency[data$condition == 'mindset'], 				y=data$a_proficiency[data$condition != 'mindset']
	)
	# 95% = 0.016 to 0.231
		
	# Experimental (with log)
	t.test(
		x=log(data$a_proficiency[data$condition == 'mindset'] + 1), 
		y=log(data$a_proficiency[data$condition != 'mindset'] + 1)
	)
	# 95% = 0.0030 to 0.025
	# e^(95%) = 1.0030 - 1.025

	
	# Linear
	m <- glm(
		formula = log(a_proficiency + 1) ~
			(condition == 'mindset') +
			start +
			intervention_time +
			log(p_exercises + 1) +
			log(p_attempts + 1) +
			log(p_correct + 1) +
			log(p_proficiency + 1) +
			log(p_hints + 1)
		, 
		data=data[-50*60*60*24 < data$start,]
	)
	# 95% = 0.0021 to 0.0214
	# e^(95%) = 1.0021 to 1.021
	
	
# Results
# Notably we get significance in the number of attempts when
# we convert to a log scale.  Controlling for cofactors results
# in a negligably tighter fit.
# 
# In general we would expect that students in the growth mindset
# conditions will complete 0.2% - 2% more proficiencies.
#
# Also, a theory on why student outcomes display a exp(normal)
# kind of curve.  Say in any given moment a student has a certain
# likelihood of quitting, like 1% and that these likelihoods are
# distributed normally.  Then the number of exercises completed
# will show patterns like expotential decay resulting in an 
# exponetial pattern.
#
# Also there is a large tail of students who quit immediately
# these are students that had such a high chance of quitting that
# we measured their 


##################################################################
#
# My Effort - Correct
# 26 Feb 2013
#
# While proficiencies are not normally distributed, problems
# correct actually are provided that we look at cases where
# more than one correct answer has been observed.  Therefore
# I will produce two models.  The first will be the odds that
# you will complete any more.  The second will be the number
# we expect you to complete correctly if you completed at least
# one.
#

	# Load
	data <- read.csv('~/Downloads/gm.aggregate.csv')

	# Add columns
	data$condition <- as.character(data$group)
	data$condition[data$group %in% c(
		'growth mindset + link',
		'growth mindset'
	)] <- 'mindset'
	data$condition <- as.factor(as.character(data$condition))

	# Histograms
	histogram(log(data$a_correct + 1)) 					# with ones
	histogram(log(data$a_correct[data$a_correct > 0])) 	# without

	# Logrithmic
	m <- glm(
		formula = (a_correct > 0) ~
			(condition == 'mindset') +
			start +
			intervention_time +
			log(p_exercises + 1) +
			log(p_attempts + 1) +
			log(p_correct + 1) +
			log(p_proficiency + 1) +
			log(p_hints + 1) +
			(p_exercises > 0) +
			(p_attempts > 0) +
			(p_correct > 0) +
			(p_proficiency > 0) +
			(p_hints > 0)
		, 
		family='binomial',
		data=data
	)
	# 95% 				= 0.0087 to 0.083
	# 1/(1+exp(-95%)) 	= .5022 to .521  (odds ratio)
	
	# Linear
	m <- glm(
		formula = log(a_correct) ~
			(condition == 'mindset') +
			start +
			intervention_time +
			log(p_exercises + 1) +
			log(p_attempts + 1) +
			log(p_correct + 1) +
			log(p_proficiency + 1) +
			log(p_hints + 1) +
			(p_exercises > 0) +
			(p_attempts > 0) +
			(p_correct > 0) +
			(p_proficiency > 0) +
			(p_hints > 0)
		, 
		family='gaussian',
		data=data[data$a_correct > 0,]
	)
	# 95% 				= -0.00604 to 0.0269
	# exp(95%)			= 99.3% to 102.7%
	
	table(data$a_correct > 0)		# 169942 True
	
# Results
# Here we see that we can measurably show that students are
# significantly more likely to answer at least one new question
# correctly in the mindset condition, somewhere around 0.2% - 2%.
# The number that they will complete is arbitrary ranging somewhere
# from 99.3% to 102.7%.  A little time should flesh this number 
# out.



##################################################################
#
# e^normal
# 27 Feb 2013
#
# Introduction:
# I want to prove that the distribution of user activity for
# any outcome is e^normal with the stipulation that we lack
# the resolution to see this distribution when too many students
# have a score of zero.

	# Common outcomes, like previous attempts, look like a 
	# normal curve with a huge number of entries at zero
	ggplot(
		data, 
		aes(log(p_attempts - runif(nrow(data))))
	) + geom_histogram()
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-27%20at%2010.44.53%20AM.png

	# Rare outcomes, like after proficiencies, far less so
	ggplot(
		data, 
		aes(log(a_proficiency + 1))
	) + geom_histogram()
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-27%20at%209.55.16%20AM.png
	
	# I propose that this is because we lose resolution at
	# lower numbers.  We are unable to distinguish between
	# 0 proficiencies and 0.1 proficiencies even though surely
	# some students get closer to completing a proficiency than
	# others.
	
	# We can show this effect if we make one of the common outcomes
	# rarer.  We can do this by dividing the outcome by some
	# number and rounding.
	#
	# For instance here we make previous attempts look like 
	# after proficiencies.
	ggplot(
		data, 
		aes(log(round((p_attempts)/20) + 1))
	) + geom_histogram()
	# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-02-27%20at%2010.45.59%20AM.png

	# At first I believed that the tail we saw at zero was
	# just an unresolved part of the distribution, that if we
	# had finer resolution we would see it stretch out and fill
	# in the rest of the normal distribution.  But if we return
	# to the after proficiencies graph it is clear that the size
	# of the bar at zero is far too large to peacefully fill in
	# the rest of the data.
	# 
	# Instead it seems that we have two distinct operations
	# one a binary choice if students will do something or 
	# nothing and another exp^normal choice of how much a student
	# will do.
	
		
# ###################################################################
# # 
# # Mo Data, Mo Problems
# # 13 March 2013
# #
# # Just got more data from Jascha 
# # https://mail.google.com/mail/u/0/?ui=2&shva=1#sent/13d5f29363897323
# # Now I want to make a concordance of this new data and graph its
# # distribtions

	# # Libraries
	# library(ggplot2)
	# library(reshape2)
	# library(plyr)
	# library(gbm)

	# # Data
	# data <- read.csv('~/Downloads/gm_process_output_20130312.csv')
	
	# # Modify
	# gms_conditions <- c(
		# 'growth mindset', 
		# 'growth mindset + link'
	# )
	# data$is_gms <- data$alternative %in% gms_conditions
	
	# # Factor Summaries
	# n = nrow(data)
	# n								# ~ 300 K students
	# sum(data$is_gms)/n				# ~ 18% in growth mindset exp


	# # Numeric Summaries
	# numeric <- laply(data, is.numeric)
	# s <- data[sample(n, 10000),numeric]
	# m <- melt(s)
	
	# ggplot(m, aes(value)) + 
		# geom_histogram() +
		# facet_wrap(~variable, scales='free')

	# ggplot(m[m$value > 0,], aes(log(value))) + 
		# geom_histogram() +
		# facet_wrap(~variable, scales='free')
	
	# # Two ways
	# of_interest <- c(
		# 'num_pre', 
		# 'proficiencies_pre', 
		# 'num_post',
		# 'proficiencies_post'
	# )
	# numeric <- laply(data, is.numeric)
	# s <- data[sample(n, 10000),numeric]
	# m <- melt(s)
	# small_m <- m[m$variable %in% of_interest,]
	# two_way <- ddply(small_m, .(variable), function(df){
		# data.frame(df, small_m)	
	# })
	
	# ggplot(two_way, aes(value, value.1)) + 
		# geom_point(size=0.5, alpha=0.5) + 
		# geom_density2d() + 
		# facet_grid(variable.1 ~ variable, scales="free")

	# ggplot(two_way, aes(log(value), log(value.1))) + 
		# geom_point(size=0.5, alpha=0.5) + 
		# geom_density2d() + 
		# facet_grid(variable.1 ~ variable, scales="free")




	# # Custom graphs
	# ggplot(s, aes(num_post, num_post*proficiencies_post)) + 
		# geom_point() +
		# xlim(0, 100) +
		# ylim(0, 100) +
		# geom_line(aes(1:nrow(s), (1:nrow(s))/10))

		
# # Results
# #
# # 300 K students 18% in growth mindset
# #
# # Histograms
# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-03-13%20at%204.14.24%20PM.png
# #
# # Histograms (log - zeros excluded)
# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-03-13%20at%204.17.12%20PM.png
# #
# # Proficiencies vs Number done
# # http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-03-14%20at%2010.53.00%20AM.png
# # *note: line represents 1/10

# ###################################################################
# # 
# # Wilcox or Die
# # 13 March 2013
# #
# # Lets be sure this new data still passes the wilcox test from
# # before.

	# # Libraries
	# library(ggplot2)
	# library(reshape2)
	# library(plyr)
	# library(gbm)

	# # Data
	# data <- read.csv('~/Downloads/gm_process_output_20130312.csv')
	# data <- data[data$experiment == 'growth-mindset',]

	
	# # Modify
	# gms_conditions <- c(
		# 'growth mindset', 
		# 'growth mindset + link'
	# )
	# data$is_gms <- data$alternative %in% gms_conditions

	# # Helper
	# run_wilcox <- function(x){
		# wilcox.test(
			# x=x[data$is_gms],
			# y=x[!data$is_gms]
		# )		
	# }

	# # Are they doing more?
	# r <- run_wilcox(data$num_post)
	# r$p.value  # 0.095
	
	# # Are they getting more proficiencies?
	# proficiencies <- data$proficiencies_post * data$num_post
	# r <- run_wilcox(proficiencies)
	# r$p.value  # 0.130
	
	# # Is their proficiency rate increasing?
	# r <- run_wilcox(data$proficiencies_post)
	# r$p.value  # 0.313
	
	# # Are they doing more fractions?
	# r <- run_wilcox(data$num_post_frac)
	# r$p.value  # 0.14
	
	# # Are they doing more other?
	# r <- run_wilcox(data$num_post_other)
	# r$p.value  # 0.17
	
	
	
	
# # Results
# #
# # Disturbiningly the wilcox test is not improving.  What shall we
# # do?  Double check the data I suppose.




# ###################################################################
# # 
# # Somethings Rotten in the State of Denmark
# # 13 March 2013
# #
# # The wilcox tests are comming out poor.  I wonder if some of the
# # data is strange.

	# # Libraries
	# library(plyr)
	# library(gbm)

	# # Data
	# data <- read.csv('~/Downloads/gm_process_output_20130312.csv')
	
	# # Modify
	# gms_conditions <- c(
		# 'growth mindset', 
		# 'growth mindset + link'
	# )
	# data$is_gms <- data$alternative %in% gms_conditions


	# # Sample
	# s <- data[
		# sample(nrow(data), 10000),
		# !names(data) %in% c('identity', 'alternative', 'experiment')
	# ]
	
	# # Discriminate
	# m <- gbm(
		# is_gms + 0 ~ .,
		# n.trees = 1000,
		# interaction.depth = 1,
		# shrinkage=0.0005,
		# data = s,
		# cv.folds=2
	# )
	# best.iter <- gbm.perf(m, method='cv')
	# summary(m, n.trees=best.iter)
	# p <- predict(m, n.trees=best.iter, newdata=s)
	# plot(p, s$noisy)

# # Result
# # Found nothing that predicted condition.  That's good.
	
	
# ###################################################################
# # 
# # What's a matta you?
# # 13 March 2013
# #
# # If we rotate a column and check for noise, can we see any
# # original data that looks like noise?

	# # Libraries
	# library(plyr)
	# library(gbm)

	# # Data
	# clean <- read.csv('~/Downloads/gm_process_output_20130312.csv')
	
	# # Add noise
	# to_shuffle <- c('num_pre')
	# noisy <- clean
	# noisy[,to_shuffle] <- noisy[sample(nrow(noisy)),to_shuffle]
	
	# # Build data
	# noisy$noisy <- 1
	# clean$noisy <- 0
	# data <- rbind(clean, noisy)

	# # Sample
	# s <- data[
		# sample(nrow(data), 100000),
		# !names(data) %in% c('identity')
	# ]
	
	# # Discriminate
	# m <- gbm(
		# noisy ~ .,
		# n.trees = 100,
		# interaction.depth = 3,
		# shrinkage=0.5,
		# data = s,
		# cv.folds=2
	# )
	# best.iter <- gbm.perf(m, method='cv')
	# summary(m, n.trees=best.iter)
	# p <- predict(m, n.trees=best.iter, newdata=s)
	# plot(p, s$noisy)


	# s[s$noisy == 0,][which.max(p[s$noisy == 0]),]



# ###################################################################
# # 
# # Testing the Aggregator
# # 14 March 2013
# #
# # Does the aggregator work as I expect that it should?


# # ~/Downloads/gm_ab_perproblems_example_rows.csv | python2 gm_process.py > ~/temp.csv

	# # Data
	# data <- read.csv('~/Downloads/gm_process_output_20130312.csv')
	
	# id <- "..5TuYrPFIATiv5F_2uJ1SbGP1R91SM0Py6rOnZ9"
	# matches <- grepl(id, data$identity)
	# data[matches,]
	
# # In comparing this student to his raw data....
# # First thing to note is that this damn student is repeated twice because he is in the subtest.
# # Second, within the subtest, in the alternative column he is listed as 'positive statment' in the raw data, but as 'science statement' in my data.  WTF!
# # The calculated intervention time is not in the data, it should be
# ## Other factors I have checked look fine


# ###################################################################
# # 
# # Experiment Assignment?
# # 14 March 2013
# #
# # The subset was wrong in one case before.  Is study assignment 
# # ever affected?

	# # Data
	# new <- read.csv('~/Downloads/gm_process_output_20130312.csv')
	# old <- read.csv('~/Downloads/gm.aggregate.csv')
	
	# # Shrink em
	# new <- new[
		# new$experiment == "growth-mindset",
		# c('alternative', 'identity')
	# ]
	# names(new) <- c('study', 'id')
	# old <- old[
		# ,
		# c('group', 'id')
	# ]
	# names(old) <- c('study', 'id')
	
	# # Merge em
	# data <- merge(new, old, by='id')
	
	
	# # Show problems
	# sum(
		# data$study.x == 'growth mindset' &
		# data$study.y == 'growth mindset + link'
	# )
	# # 9383
	
	# sum(
		# data$study.x == 'growth mindset + link' &
		# data$study.y == 'growth mindset'
	# )
	# # 9413
	
		
# # Results
# #
# # On the study level it seems as if growth mindset and growth
# # mindset with link have been scrambled, though the rest seem
# # pretty regular.



# ###################################################################
# # 
# # Subtest Assignment?
# # 14 March 2013
# #
# # The subtest was wrong in one case before.  Is subtest assignment 
# # ever affected?

	# # Data
	# new <- read.csv('~/Downloads/gm_process_output_20130312.csv')
	# old <- read.csv('~/Downloads/gm.aggregate.csv')
	
	# # Shrink em
	# new <- new[
		# new$experiment == "growth-mindset-subtest",
		# c('alternative', 'identity')
	# ]
	# names(new) <- c('subtest', 'id')
	# old <- old[
		# ,
		# c('subtest', 'id')
	# ]
	# names(old) <- c('subtest', 'id')
	
	# # Merge em
	# data <- merge(new, old, by='id')
	
	
	# # Show problems
	# sum(
		# data$subtest.x == 'positive statement' &
		# data$subtest.y == 'science statement'
	# )
	# # 9345
	
	# sum(
		# data$subtest.x == 'science statement' &
		# data$subtest.y == 'positive statement'
	# )
	# # 9393
	
		
# # Results
# #
# # Roughly the same number of students were switched as above
# # very curious.  Perhaps Jascha has insight by now?




# ###################################################################
# # 
# # Do I believe the dashboard?
# # 14 March 2013
# #
# # Jascha sent the dashboard link which shows that the students in
# # the growth mindset condition are achieving more proficiencies.
# #
# # Do I see that in this data?

	# # Libraries
	# library(plyr)

	# # Data
	# data <- read.csv('~/Downloads/gm_process_output_20130312.csv')
	
	# # Remove subtest
	# data <- data[data$experiment == "growth-mindset",]
	
	# # Check percentages
	# ddply(data, .(alternative), function(df){data.frame(
		# post=sum(df$proficiencies_post*df$num_post)/nrow(df)
	# )})	
		
# # Results
# #
# # Very weird, these values are near 12, yet the ones in the dashboard
# # are near 2.5.  Something is rotten in the state of Denmark.



###################################################################
# 
# Intervention Times!
# 15 March 2013
#
# It dawned on me last night that our intervention times are 
# probably wrong.  Jascha's script does not account for the fact
# that some students may have performed fractions exercises before
# the intervention began.
#
# I want to take the following steps
#
# 	. (x) Add intervention time to Jascha's report.
#	. (X) Check that its the time I expect on the sample.
#	. (x) Change the sample time to prove you can get an impossible one.
#	. (x) Write to the group.
#	. (x) Fix it.
#	. (x) Check that its fixed using the example data.
#	. (x) Run on the new data.
#	. (x) Post new data, script, and write to group.
#	. (x) Comment out previous silly entries.
#	. (x) Rerun these ananlysis with the new data.

	# Libraries
	library(plyr)

	# Data
	data <- read.csv('~/Downloads/gm_process_output_bmh_fix_20130315.csv')
	
	# Remove Subtest
	data <- data[data$experiment != 'growth-mindset-subtest',]
	
	# Remove those with no intervention time
	data <- data[data$intervention_time != -1,]
	
	# Check Number of students
	dim(data)
	table(data$alternative)
	
	# Check proviciencies
	proficiencies <- data$num_post * data$proficiencies_post
	mean(proficiencies, na.rm=T)
	
	# Proficiencies by mindset
	control <- data$alternative %in% c(
		'control statement', 'no header'
	)
	mean(proficiencies[control], na.rm=T)
	mean(proficiencies[!control], na.rm=T)
	
	# Wilcox test
	wilcox.test(
		x=proficiencies[control],
		y=proficiencies[!control]
	)
	
	# Density plots
	ggplot(NULL, aes(log(proficiencies, base=10), color=data$alternative)) + 
		geom_density(adjust=5)
		
	# Histograms
	ggplot(NULL, aes(proficiencies, color=data$alternative)) + 
		geom_bar(position='fill') +
		xlim(0,100)
		
# Results
#
# We seem to be back in buisness.


	
###################################################################
# 
# Distributional Concerns
# 15 March 2013
#
# Just fixed the data we got from Jascha, time to look at it more
# closely.

	# Libraries
	library(ggplot2)
	library(reshape2)
	library(plyr)
	library(gbm)

	# Data
	url = '~/Downloads/gm_process_output_bmh_fix_20130315.csv'
	data <- read.csv()
	
	# Remove Subtest
	data <- data[data$experiment != 'growth-mindset-subtest',]
	
	# Remove those with no intervention time
	data <- data[data$intervention_time != -1,]
	
	# Modify
	gms_conditions <- c(
		'growth mindset', 
		'growth mindset + link'
	)
	data$is_gms <- data$alternative %in% gms_conditions
	
	# Factor Summaries
	n = nrow(data)
	n								# ~ 214 K students
	sum(data$is_gms)/n				# ~ 22% in growth mindset exp

	# Numeric Summaries
	numeric <- laply(data, is.numeric)
	s <- data[sample(n, 10000),numeric]
	m <- melt(s)
	
	ggplot(m, aes(value)) + 
		geom_histogram() +
		facet_wrap(~variable, scales='free')
	

	ggplot(m[m$value > 0,], aes(log(value))) + 
		geom_histogram() +
		facet_wrap(~variable, scales='free')

		
# Results
#
# 214 K students 22% in growth mindset
#
# Histograms
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-03-15%20at%201.31.50%20PM.png
#
# Histograms (log - zeros excluded)
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-03-15%20at%201.32.44%20PM.png



###################################################################
# 
# How low can you go?
# 15 March 2013
#
# Lets try predicting proficiencies using data that we had before
# the intervention.

	# Libraries
	library(ggplot2)
	library(reshape2)
	library(plyr)
	library(gbm)

	# Data
	url = '~/Downloads/gm_process_output_bmh_fix_20130315.csv'
	data <- read.csv()
	
	# Remove Subtest
	data <- data[data$experiment != 'growth-mindset-subtest',]
	
	# Remove those with no intervention time
	data <- data[data$intervention_time != -1,]
	
	# Modify
	gms_conditions <- c(
		'growth mindset', 
		'growth mindset + link'
	)
	data$is_gms <- data$alternative %in% gms_conditions
	
	# gbm
	s <- data[sample(nrow(data), nrow(data)),]
	s <- s[s$proficiencies_post > 0,]
	m <- gbm(
		log(s$proficiencies_post) ~
			#alternative +
			intervention_time +
			num_coaches +
			max_coach_students +
			num_pre +
			correct_pre +
			proficiencies_pre +
			hints_pre +
			median_time_pre +
			review_pre +
			irt_easiness_pre,
		data=s,
		distribution='gaussian',
		n.trees=200,
		interaction.depth=2,
		shrinkage=0.1,
		cv.folds=2
	)
	best.iter <- gbm.perf(m, method='cv')
	m$cv.error[best.iter]		# 0.570
	summary(m, n.trees=best.iter)
	s$predicted_proficiencies <- predict(m, n.trees=best.iter, newdata=s)
	ggplot(s, aes(log(proficiencies_post), predicted_proficiencies)) + 
		geom_point(alpha=0.5) + 
		geom_density2d()
	
	
	# Numeric Summaries
	s$weird <- (
		s$predicted_proficiencies > -2.5 & 
		log(s$proficiencies_post) < -2.5
	) + 0
	numeric <- laply(s, is.numeric)
	m <- melt(s[1:1000,numeric], id.vars=c('weird'))
	
	ggplot(m, aes(log(value, base=10), color=factor(weird))) + 
		geom_density() +
		facet_wrap(~variable, scales='free')

	# Uncertainty
	m <- gbm(
		abs(log(proficiencies_post) - predicted_proficiencies) ~
			#alternative +
			intervention_time +
			num_coaches +
			max_coach_students +
			num_pre +
			correct_pre +
			proficiencies_pre +
			hints_pre +
			median_time_pre +
			review_pre +
			irt_easiness_pre +
			predicted_proficiencies,
		data=s,
		distribution='gaussian',
		n.trees=200,
		interaction.depth=2,
		shrinkage=0.1,
		cv.folds=2
	)
	best.iter <- gbm.perf(m, method='cv')
	m$cv.error[best.iter]		# 0.226
	summary(m, n.trees=best.iter)
	s$uncertainty <- predict(m, n.trees=best.iter, newdata=s)
	ggplot(s, aes(
		log(proficiencies_post), 
		predicted_proficiencies, 
		color=uncertainty > median(uncertainty)
	)) + 
		geom_point(alpha=0.1) + 
		geom_density2d()
	
	
	m <- glm(
		log(proficiencies_post) ~ is_gms + predicted_proficiencies,
		data=s[s$uncertainty > median(s$uncertainty),]
	)
	summary(m)

	m <- glm(
		log(proficiencies_post) ~ is_gms + predicted_proficiencies,
		data=s[s$uncertainty < median(s$uncertainty),]
	)
	summary(m)
	
# Results
#		
# Log proficiency predictions
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-03-15%20at%203.08.20%20PM.png
# *We are decent at predicting proficiency rate
# but get really bad when the predicted outcome is near -2.5
# for reasons I ought to investigate
#
#
# Bad Predictions
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-03-15%20at%202.39.29%20PM.png
# From these density plots we can see that the group with highly
# variable outcomes are those who had very few exercises completed
# prior to the intervention.  They regularly had only done 10
# problems and it is no suprise why they are hard to predict.
#
#
# Predicting Uncertainty
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-03-15%20at%203.21.57%20PM.png
# We can employ a second gbm and predcit which data points we
# are uncertain about.  The result a very clean dataset appropriate
# for real analysis.
#
# 
# Significance... we get none.  But At least I have some insight
# into the data now.





###################################################################
# 
# Number Post
# 15 March 2013
#
# Lets try this same prediction, but use number post instead.

	# Libraries
	library(ggplot2)
	library(reshape2)
	library(plyr)
	library(gbm)

	# Data
	url = '~/Downloads/gm_process_output_bmh_fix_20130315.csv'
	data <- read.csv(url)
	
	# Remove Subtest
	data <- data[data$experiment != 'growth-mindset-subtest',]
	
	# Remove those with no intervention time
	data <- data[data$intervention_time != -1,]
	
	# Modify
	gms_conditions <- c(
		'growth mindset', 
		'growth mindset + link'
	)
	data$is_gms <- data$alternative %in% gms_conditions
	
	# gbm
	data$outcome <- log(data$proficiencies_post)
	histogram(data$outcome)
	s <- data[sample(nrow(data), nrow(data)),]
	s <- s[!(
		is.na(s$outcome) | 
		s$outcome == -Inf
	),]
	model_n <- nrow(s)
	graph_n <- 10000
	m <- gbm(
		outcome ~
			#alternative +
			intervention_time +
			num_coaches +
			max_coach_students +
			num_pre +
			correct_pre +
			proficiencies_pre +
			hints_pre +
			median_time_pre +
			review_pre +
			irt_easiness_pre,
		data=head(s, model_n),
		distribution='gaussian',
		n.trees=200,
		interaction.depth=2,
		shrinkage=0.1,
		cv.folds=2
	)
	best.iter <- gbm.perf(m, method='cv')
	m$cv.error[best.iter]		# 1.92
	summary(m, n.trees=best.iter)
	s$prediction <- predict(m, n.trees=best.iter, newdata=s)
	ggplot(head(s, graph_n), aes(outcome, prediction)) + 
		geom_point(alpha=0.5) + 
		geom_density2d()
	
	
	# Uncertainty
	m <- gbm(
		abs(outcome - prediction) ~
			intervention_time +
			num_coaches +
			max_coach_students +
			num_pre +
			correct_pre +
			proficiencies_pre +
			hints_pre +
			median_time_pre +
			review_pre +
			irt_easiness_pre +
			prediction
			,
		data=head(s, model_n),
		distribution='gaussian',
		n.trees=200,
		interaction.depth=2,
		shrinkage=0.1,
		cv.folds=2
	)
	best.iter <- gbm.perf(m, method='cv')
	m$cv.error[best.iter]		# 0.230
	summary(m, n.trees=best.iter)
	s$uncertainty <- predict(m, n.trees=best.iter, newdata=s)
	ggplot(head(s, graph_n), aes(
		outcome, 
		prediction, 
		color=uncertainty > median(uncertainty)
	)) + 
		geom_point(alpha=0.1) + 
		geom_density2d()

	
	# Naive model
	m <- glm(
		outcome ~ is_gms,
		data=s
	)
	summary(m)

	# Add prediction
	m <- glm(
		outcome ~ is_gms + prediction,
		data=s
	)
	summary(m)
	
	# Add uncertainty
	m <- glm(
		outcome ~ is_gms + prediction,
		data=s[s$uncertainty > median(s$uncertainty),]
	)
	summary(m)
	


	
# Results
#		
# I see little evidence that students are doing more necessarily.



###################################################################
# 
# Intervals
# 16 March 2013
#
# I'm curious what the distributions of intervals are by user.  Is
# Median really a good way to capture this number?

	# Libraries
	library(ggplot2)
	library(plyr)

	# Data
	url = '~/Downloads/gm_ab_perproblems_example_rows.csv'
	data <- read.csv(url)
	
	# Add some names
	names(data) <- paste0('X.', 1:17)
	
	# Add counts per person
	data <- ddply(data, .(X.3), function(df){
		df$n <- nrow(df)
		df
	})

	# Everyone
	g <- data[, c('X.17', 'X.3')]
	ggplot(g, aes(X.17)) + 
		geom_bar() + 
		theme(legend.position="none") +
		scale_x_log10() +
		geom_vline(xintercept = 1, color='blue') +
		geom_vline(xintercept = 10, color='red') +
		geom_vline(xintercept = 60, color='blue') +
		geom_vline(xintercept = 5*60, color='red') +
		geom_vline(xintercept = 60*60, color='blue') +
		geom_vline(xintercept = 60*60*24, color='blue')
	
# Results
#
# dTime Distribution
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-03-16%20at%2012.17.18%20PM.png
# * vertical lines are 1 sec, 10 sec, 1 min, 5 min, 1 hour, 1 day
# 
# The distrubution is not too unusual, but thinking carefully about
# it, one realizes that the long tail is very important.  dTime
# greater than an hour or so represents a significant break from
# the khan academy.  When one imagines a user, they will work in
# spurts with long breaks.  The duration of spurts and frequency
# of long breaks are both important indicators.



###################################################################
# 
# Intervals
# 16 March 2013
#
# I'm curious what the distributions of intervals are by user.  Is
# Median really a good way to capture this number?

	# Libraries
	library(ggplot2)
	library(plyr)

	# Data
	url = '~/Downloads/gm_ab_perproblems_example_rows.csv'
	data <- read.csv(url)
	
	# Add some names
	names(data) <- paste0('X.', 1:17)
	
	# Add counts per person
	data <- ddply(data, .(X.3), function(df){
		df$n <- nrow(df)
		df
	})

	# Everyone
	g <- data[, c('X.17', 'X.3')]
	ggplot(g, aes(X.17)) + 
		geom_bar() + 
		theme(legend.position="none") +
		scale_x_log10() +
		geom_vline(xintercept = 1, color='blue') +
		geom_vline(xintercept = 10, color='red') +
		geom_vline(xintercept = 60, color='blue') +
		geom_vline(xintercept = 5*60, color='red') +
		geom_vline(xintercept = 60*60, color='blue') +
		geom_vline(xintercept = 60*60*24, color='blue')
	
# Results
#
# dTime Distribution
# http://dl.dropbox.com/u/1131693/bloodrop/Screen%20Shot%202013-03-16%20at%2012.17.18%20PM.png
# * vertical lines are 1 sec, 10 sec, 1 min, 5 min, 1 hour, 1 day
# 
# The distrubution is not too unusual, but thinking carefully about
# it, one realizes that the long tail is very important.  dTime
# greater than an hour or so represents a significant break from
# the khan academy.  When one imagines a user, they will work in
# spurts with long breaks.  The duration of spurts and frequency
# of long breaks are both important indicators.



###################################################################
# 
# Adding the Interval
# 18 March 2013
#
# Dave suggested that we should add the earliest time and last time
# the student did anything to the aggregation script.  This is an
# excellent suggestion and should be relatively easy, except that
# the file will take a while to process so it will be good to work
# with a test case first.
#
# The steps
#	(x) Add columns for start and stop time in the aggreagtor
#	(x) Run on example rows
#	(x) Check that they match expectations
#	(x) Run on all rows
#	(x) Upload to dropbox, update script, write to crew

	# Data
	url = '~/Dropbox/growth_mindset (1)/gm_process_output_with_times_20130318.csv'
	data <- read.csv(url)
	
	# Remove Subtest
	data <- data[data$experiment != 'growth-mindset-subtest',]
	
	# Remove those with no intervention time
	data <- data[data$intervention_time != -1,]
			
# Results
#
# Feeling good, feeling great.



###################################################################
# 
# Exploring times
# 19 March 2013
#
# I want to see how time on problem relates to other factors.


	# Libraries
	library(ggplot2)
	library(plyr)

	# Data
	url = '~/Downloads/gm_ab_perproblems_example_rows.csv'
	data <- read.csv(url)
	
	# Add some names
	names(data) <- c('experiment', 'alternative', 'identity', 'num_coaches', 'max_coach_students', 'time_done', 'dt', 'exercise', 'problem_type', 'seed', 'problem_number', 'topic_mode', 'review_mode', 'correct', 'proficiency', 'hints', 'time_taken')
	
	# Add some features
	data$problem <- factor(paste(data$exercise, data$problem_type))
	data <- ddply(data, .(identity, problem), function(df){
		cbind(
			df, 
			as.list(quantile(df$time_taken)),
			n=nrow(df)
		)
	})
	data <- ddply(data, .(problem), function(df){
		cbind(
			df, 
			p.n=nrow(df),
			p.median_time=median(df$time_taken, na.rm=T)
		)
	})
	data <- ddply(data, .(identity), function(df){
		cbind(
			df, 
			i.n=nrow(df),
			i.median_time=median(df$time_taken, na.rm=T)
		)
	})


	# Model
	s <- data[sample(nrow(data)),]
	trees <- 100
	m <- gbm(
		log(time_taken) ~ . -
			experiment -
			alternative - 
			dt -
			identity -
			num_coaches -
			max_coach_students -
			time_done - 
			exercise - 
			problem_type -
			seed - 
			topic_mode -
			review_mode - 
			proficiency -
			problem, 
		distribution='gaussian',
		data=s[s$time_taken > 0,],
		n.trees=trees,
		interaction.depth=3,
		shrinkage=0.5,
		cv.folds=2
	)
	best.iter <- gbm.perf(m, method='cv')
	m$cv.error[best.iter]		# 0.45
	summary(m, n.trees=best.iter)
	s$p <- predict(m, n.trees=best.iter, newdata=s)
	
	# Prediction vs Actual
	ggplot(s, aes(p, log(time_taken))) + 
		geom_point() +
		geom_density2d() 
		
	# Prediction by correct
	ggplot(s, aes(factor(correct), p)) + 
		geom_boxplot()
		
	# Time by identity
	g <- s[
		grepl('.*[A-G]$', s$identity)
	,]	
	ggplot(g, aes(log(time_taken), color=identity)) +
		geom_histogram() +
		facet_wrap(~identity)

	# Time by exercise
	g <- s[
		grepl('^e.*', s$exercise)
	,]	
	table(table(g$exercise))
	ggplot(g, aes(log(time_taken), color=exercise)) +
		geom_histogram() + 
		facet_wrap(~exercise)
		
	# Prediction by 50%
	s$median <- s[,'50%']
	ggplot(s, aes(log(median), log(time_taken))) + 
		geom_point() +
		geom_density2d(aes(color=problem_number == 1)) +
		facet_grid(~correct)

	# Prediction by 75%
	ggplot(s, aes(log(s[,'75%']), p)) + 
		geom_point() +
		geom_density2d() 
		
# Results:
# Time on problem can be accurately modeled by the median time on
# an given problem per student, whether they got it correct or not
# the problem number, and a few other factors.  Median time handles
# most of the prediction on its own.  I imagine I can exploit this
# fact when trying to see if intervention affects problem done.




###################################################################
# 
# Like Reading?
# 19 March 2013
#
# If students are reading the prompt, then presumbably this will be
# reflected in the amount of time that they spend on an assignemnt
# here we want to prove that.

	# Libraries
	require(data.table)
	require(gbm)
	require(ggplot2)
	require(plyr)
	require(reshape2)

	# Data
	data <- read.csv('~/temp.csv')
	
	# Stats
	dim(data)
	length(unique(data$identity))
	head(data)
	table(data$alternative)
		
	# Add factors
	data$problem <- factor(paste(data$excercise, data$problem_type))
	data$user_problem <- factor(paste(data$identity, data$problem))
	data <- data.table(data)
	aggregate <- data[,
		as.double(median(time_taken, na.rm=T)),
		by=user_problem
	]
	data <- merge(data, aggregate, by='user_problem', all.x=T)
	experimental_conditions <- c(
		'growth mindset', 
		'growth mindset + link'
	)
	data$is_gms <- data$alternative %in% experimental_conditions

	# Clean
	data <- data[data$experiment == 'growth-mindset',]
	
	# Model
	users <- sample(unique(data$identity), 10000) 
	s <- data[data$identity %in% users,]
	s <- s[sample(nrow(s)),]
	trees <- 100
	m <- gbm(
		log(time_taken) ~ . - 
			identity - 
			user_problem -
			problem - 
			experiment -
			dt -
			is_gms, 
		distribution='gaussian',
		data=s[s$time_taken > 0,],
		n.trees=trees,
		interaction.depth=3,
		shrinkage=0.5,
		cv.folds=2
	)
	best.iter <- gbm.perf(m, method='cv')
	m$cv.error[best.iter]		# 0.45
	summary(m, n.trees=best.iter)

	# Make predictions
	temp <- function(data, new_alternative){
		data$alternative <- new_alternative
		data
	}
	s$p <- predict(m, n.trees=best.iter, newdata=s)
	s$p_no_header <- predict(m, n.trees=best.iter, newdata=temp(s, 'no header'))
	s$r_no_header <- s$p - s$p_no_header
	
	# Plot the strange ones
	numeric <- laply(s, is.numeric)
	rando <- sample(nrow(s), 1000)
	s2 <- data.frame(s)
	s2 <- s2[rando, numeric]
	m <- melt(s2, id.vars='r_no_header')
	ggplot(m, aes(value, color=r_no_header == 0)) + 
		geom_density(adjust=2) + 
		facet_wrap(~variable, scale='free') + 
		scale_x_log10()
	
	
	
	
	# Predict Altenative
	users <- sample(unique(data$identity), 10000) 
	s <- data[data$identity %in% users,]
	s <- s[sample(nrow(s)),]
	trees <- 100
	m <- gbm(
		is_gms ~ . - 
			identity - 
			user_problem -
			problem - 
			experiment -
			dt -
			alternative, 
		distribution='bernoulli',
		data=s,
		n.trees=trees,
		interaction.depth=3,
		shrinkage=0.5,
		cv.folds=2
	)
	best.iter <- gbm.perf(m, method='cv')
	m$cv.error[best.iter]		# 0.89
	summary(m, n.trees=best.iter)
	s$p <- predict(m, n.trees=best.iter, newdata=s)
	
	s$accuracy <- 'unknown'
	s$accuracy[ s$is_gm & s$p > 0] <- 'true positive'
	s$accuracy[ s$is_gm & s$p < 0] <- 'false negative'
	s$accuracy[!s$is_gm & s$p > 0] <- 'false positive'
	s$accuracy[!s$is_gm & s$p < 0] <- 'true negative'
	
	# Plot the strange ones
	numeric <- laply(s, is.numeric)
	rando <- sample(nrow(s), 10000)
	accuracy <- s$accuracy
	s2 <- data.frame(s)
	s2 <- s2[rando, numeric]
	s2$accuracy <- accuracy[rando]
	m <- melt(s2, id.vars='accuracy')
	ggplot(m, aes(value, color=accuracy)) + 
		geom_density(adjust=2) + 
		facet_wrap(~variable, scale='free') + 
		scale_x_log10()


	# Sans interpretations
	numeric <- laply(data, is.numeric)
	control <- data$alternative == 'control statement'
	growth <- data$alternative == 'growth mindset'
	link <- data$alternative == 'growth mindset + link'
	none <- data$alternative == 'no header'
	n <- 10000
	rando <- 
		data$identity %in% sample(unique(data$identity[control]), n) |
		data$identity %in% sample(unique(data$identity[growth]), n) |
		data$identity %in% sample(unique(data$identity[link]), n) |
		data$identity %in% sample(unique(data$identity[none]), n)
		
	alternative <- data$alternative
	s2 <- data.frame(data)
	s2 <- s2[rando, numeric]
	s2$alternative <- alternative[rando]
	m <- melt(s2, id.vars='alternative')
	ggplot(m, aes(value, color=alternative)) + 
		geom_density(adjust=2) + 
		facet_wrap(~variable, scale='free') + 
		scale_x_log10()

		


###################################################################
# 
# Adding 
# 18 March 2013
#
# The Khan data could use number of unique exercises attempted
# Also, I have found in verifying my assumptions that some of the
# data rows are repeated.  I now remove repeated rows.
#
# The steps
#   (x) Remove Duplicate entries
#   (x) Check on sample data
#	(x) Add n unique exercises
#	(x) Check on sample data
#   (x) Run on full data
#	(x) Upload to dropbox, update script, write to crew

	# Data
	cols <- c(
			'experiment', 
			'alternative', 
			'identity', 
			'num_coaches', 
			'max_coach_students', 
			'time_done', 
			'dt', 
			'exercise', 
			'problem_type', 
			'seed', 
			'problem_number', 
			'topic_mode', 
			'review_mode', 
			'correct', 
			'proficiency', 
			'hints', 
			'time_taken'
	)
	examples <- read.csv(
		'~/Downloads/gm_ab_perproblems_example_rows.csv', 
		header=FALSE,
		col.names=cols
	)
	data <- read.csv(
		'~/Dropbox/growth_mindset (1)/gm_process_output_with_times_20130318.csv'
	)
	new <- read.csv('~/temp.csv')
	
	# Validate
	# Prove there is only one proficiency per exercise per user
	t <- ddply(examples, .(identity, exercise), function(df){
		sum(df$proficiency)	
	}) 
	nrow(t[t$V1 > 1,])    # 22
	head(t[t$V1 > 1,])
	# Do'H sometimes there are two!  WTF?
	
	# Duplicate Rows?
	# Looking at the data, its clear that some rows are duplicated
	# how many?
	mash <- do.call("paste", c(examples, sep = "..."))
	nrow(examples) - length(unique(mash))
	# 605 of 100,000 are duplicated rows, ~0.6%
	
	# Confirm they are removed
	1e5 - sum(c(new$num_pre,new$num_post), na.rm=T)
	# 605 gone, good!
		
	# Validate Unique Exercises
	t <- ddply(examples, .(identity), function(df){
		length(unique(df$exercise))	
	})
	t2 <- ddply(new, .(identity), function(df){
		df$num_pre * df$unique_exercises_pre + 
		df$num_post * df$unique_exercises_post
	})
	# Looking nicely identical!
				
# Results
#
# Went alright, surprised to find that duplicate rows exist!





