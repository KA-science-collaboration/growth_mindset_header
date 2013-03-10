###
###		Khan Academy Growth Mindset Experiment 1
###		Growth Mindset Headers
###		dp 2013

#	LIBRARIES
library(ggplot2)
library(MASS) 		# for fitting negative binomial
library(scales)

#	HELPER FUNCTIONS

#	quantile split; 
#	e.g, split_quantiles =.5 is median split, c=(.25,.5,.75) quartile split 
qs <- function( vec, split_quantiles=c(.5), labels=c() ){
	split_values <- quantile( vec, split_quantiles, na.rm=TRUE )
	quantile_vec <- NA
	
	#	if names for quantiles were not passed in, auto-generate
	if( length( labels ) == 0 ){
		labels[ 1 ] <- "above 0"
		for( i in 1:length(split_quantiles) ){
			labels[ i+1 ] <- paste("above", split_quantiles[i])
		}		
	}else{
		if( length( labels ) != length( split_quantiles ) + 1 ){
			stop("There should be 1 fewer split quantiles than labels")
		}
	}
	
	quantile_vec[ !is.na( vec ) ] <- labels[1]
	for( i in 1:length(split_quantiles) ){
		quantile_vec[ vec > split_values[i] ] <- labels[ i + 1]
	}
	#	return as factor with levels explicitly set in correct order
	quantile_vec <- factor( quantile_vec, levels = labels )
	return( quantile_vec )
}

#	return a named vector with the % of the distribution occupied by each unique value
pct <- function( x ){
	pcts <- c()
	x <- x[ ! is.na( x ) ]
	for( group in sort(unique(x)) ){
		group <- as.character( group ) 
		pcts[group] <- round(sum( x==group ) / length( ! is.na(x) ) * 100)
	}
	return(pcts)
}

#	ggplot2 Helpers
#	You can use Hmisc to auto-generate error bars using bootstrapped intervals
library( Hmisc )
stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data=fun, colour="black", geom=geom, width=.5,  ...)
}

#	definte the dodge object for equivalent	error bar and geom_bar
dodge <- position_dodge( width=0.9 )

library( grid )     # for units
text_size <- 8
horzi_theme <- theme(    #   remove the gray background
    panel.background    = element_blank() ,
    #   make the major gridlines light gray and thin
    panel.grid.major.y  = element_line( size=.1, colour="#666666" ) ,
    #   suppress the vertical grid lines
    panel.grid.major.x  = element_blank() ,
    #   suppress the minor grid lines
    panel.grid.minor    = element_blank() ,
    #   adjust the axis ticks
    axis.ticks          = element_line( size=.1 , colour="#666666" ),
    #   move the y-axis over to the left 
	axis.title.x 		= element_text( color="#333333", angle=0, size=text_size ) ,
	axis.text 			= element_text( vjust=0 , color="#333333", angle=0, size=text_size ) ,
	axis.title.y 		= element_text( color="#333333", angle=90, size=text_size ) ,
	plot.title			= element_text( color="#333333", angle=0, size=text_size ) 
)

bars <- geom_bar( stat="summary", fun.y="mean", position=dodge )




#	READ IN DATA
setwd("~/big_data/KA 02-2013")
d	<- read.csv( "gm.aggregate.csv", stringsAsFactors=FALSE )
nrow(d)


#	DATA PREP
#	identify people who never completed a single intervention problem
aggregate(d$intervention_time == 0 ~ d$group , data = d, mean)
	# 1             no header                0.1848748
	# 2    positive statement                0.1897296
	# 3     science statement                0.1853385
	# 4        growth mindset                0.1880684
	# 5 growth mindset + link                0.1836177
	
d$no_intervention <- d$intervention_time == 0
d$no_fraction_problems <- "fraction problems submitted"
d$no_fraction_problems[d$no_intervention] <- "fraction problems not submitted"


#	turn UNIX dates into human-readable dates & drop people without intervention
d$intervention_date <- as.POSIXct(d$intervention_time, origin="1970-01-01")                # local
d$intervention_date[ d$no_intervention ] <- NA
d$idate_str <- as.character( d$intervention_date )

#	add subtest condition to the main group variable
d$group[d$subtest != ""]  <- d$subtest[d$subtest != ""] 
d$group <- factor( d$group, 
	c("no header","positive statement","science statement","growth mindset","growth mindset + link" ) )

#	create a mindset contrast (vs others)
d$gms <- FALSE
d$gms[ grep("growth",d$group) ] <- TRUE

#	collapse mindset conditions
d$group_col <- as.character(d$group)
d$group_col[ grep("growth",d$group) ] <- "mindset"
d$group_col <- factor( d$group_col , 
	c("no header","positive statement","science statement","mindset" ) )

d$gms_vs_nohead <- NA
d$gms_vs_nohead[d$gms] <- TRUE
d$gms_vs_nohead[d$group=="no header"] <- FALSE

d$three_group <- "no header"
d$three_group[ grep("statement", d$group) ] <- "control statement"
d$three_group[ grep("growth", d$group) ] <- "mindset"

#	How can users get grouped based on prior exposure?
quantile( d$p_proficiency, c(.5,.75,.9,.95,.99) )

#	Make some (arbitrary) but potentially useful subdivisions 
#	for user-types based on prior proficiencies
d$p_prof_group <- "None"
d$p_prof_group[d$p_proficiency > 0] <- "1-3"
d$p_prof_group[d$p_proficiency > 3] <- "4-19"
d$p_prof_group[d$p_proficiency > 19] <- "20+"
d$p_prof_group <- factor( d$p_prof_group , c("None","1-3","4-19","20+") )

d$newbie <- "veteran"
d$newbie[d$p_proficiency == 0] <- "newbie"



#	Change in proficicies from pre-to-post
d$delta_prof <- d$a_proficiency - d$p_proficiency

##	Change in # Correct
d$delta_correct <- d$a_correct - d$p_correct
d$delta_correct_sqrt <- (d$a_correct)**.5 - log(d$p_correct)**.5
hist( d$delta_correct_sqrt )

#	% Change in Proficiencies
#	57000 people NEVER earned proficiencies
#	The fractional approaches below remove 
table(d$p_proficiency == 0 & d$a_proficiency == 0)

#	Binomial Splits for Logistic Regressions
d$any_profs <- ifelse( d$a_proficiency > 0, 1, 0 )
d$two_plus_profs <- ifelse( d$a_proficiency > 1, 1, 0 )
d$five_plus_profs <- ifelse( d$a_proficiency > 4, 1, 0 )
d$ten_plus_profs <- ifelse( d$a_proficiency > 9, 1, 0 )
d$fifteen_plus_profs <- ifelse( d$a_proficiency > 14, 1, 0 )
d$twenty_plus_profs <- ifelse( d$a_proficiency > 19, 1, 0 )



################################################################
####
####	Ordinal Logistic Regression
####	
####	Reference page http://www.ats.ucla.edu/stat/r/dae/ologit.htm
####
####	Results: Mindset shifts the proficiency and correct deciles.
####
################################################################

library( MASS )

#	or for cumulative ordinal... 
#	http://cran.r-project.org/web/packages/ordinal/ordinal.pdf
#	library( ordinal )

#	convert proficiencies to an ordered factor by decile
d$a_prof_dec <- as.factor(round( 10 * rank(d$a_proficiency)/length( d$a_proficiency ) ))
d$a_correct_dec <- as.factor(round( 10 * rank(d$a_correct)/length( d$a_correct ) ))


#	Mindset conditions lead to higher proficiency decile
#	The odds ratio is 1.02, i.e., exp(.024463)
a <- polr( a_prof_dec ~ group_col , data= d)
summary( a )
	# Coefficients:
	                               # Value Std. Error t value
	# group_colpositive statement 0.007282    0.01355  0.5375
	# group_colscience statement  0.007877    0.01349  0.5838
	# group_colmindset            0.024463    0.01034  2.3663
	
	# Intercepts:
	     # Value    Std. Error t value 
	# 2|5   -0.4902   0.0060   -81.8875
	# 5|6    0.2275   0.0059    38.5580
	# 6|7    0.6245   0.0061   103.1480
	# 7|8    1.1980   0.0066   182.6518
	# 8|9    1.8265   0.0076   240.9285
	# 9|10   2.9975   0.0114   263.5705
	
	# Residual Deviance: 657408.92 
	# AIC: 657426.92 

#	Mindset conditions lead to higher correct answer decile
#	The odds ratio is 1.02, i.e., exp(0.0230106)
a <- polr( a_correct_dec ~ group_col , data= d)
summary( a )

	# Coefficients:
	                                 # Value Std. Error  t value
	# group_colpositive statement -0.0002293    0.01322 -0.01734
	# group_colscience statement   0.0095140    0.01315  0.72372
	# group_colmindset             0.0230106    0.01007  2.28525
	
	# Intercepts:
	     # Value     Std. Error t value  
	# 1|2    -1.5997    0.0071  -225.0602
	# 2|3    -1.0560    0.0063  -166.3715
	# 3|4    -0.5632    0.0060   -94.5031
	# 4|5    -0.1844    0.0058   -31.6455
	# 5|6     0.2031    0.0058    34.8440
	# 6|7     0.6172    0.0060   103.1057
	# 7|8     1.1102    0.0064   173.4851
	# 8|9     1.7392    0.0074   236.3959
	# 9|10    2.9482    0.0111   265.0805
	
	# Residual Deviance: 859236.57 
	# AIC: 859260.57 




################################################################
####
####	Plots Comparing the Groups 
####	
####
################################################################


ggplot( d[,], aes( a_proficiency , color=three_group ) ) + 
	geom_density( alpha=.2, adjust=2 ) +
	scale_colour_manual( 
		breaks=c("control statement","mindset","no header"), 
		values=c("red","blue","orange") 
		 ) + 
	scale_x_log10()

#	problems answered correctly post-study
ggplot( d[,], aes( a_correct + .1 , color=three_group ) ) + 
	geom_density( alpha=.2, adjust=2 ) +
	scale_colour_manual( "Condition",
		breaks=c("control statement","mindset","no header"), 
		values=c("red","blue","orange") 
		 ) + 
	scale_x_sqrt( ) +
	scale_y_continuous( labels=percent ) +
	coord_cartesian( xlim=c(0,1000) ) +
	ylab("") +
	xlab("Problems answered correctly post-study")
	ggsave("Distribution of correct problems by group.png", dpi=300, width=10, height=5 )


ggplot( d[,], aes( a_proficiency , color=three_group ) ) + 
	geom_density( alpha=.2, adjust=2 ) +
	scale_colour_manual( 
		breaks=c("control statement","mindset","no header"), 
		values=c("red","blue","orange") 
		 ) + 
	scale_x_log10()
	
ggplot( d[d$three_group !="no header",], aes( a_proficiency , fill=three_group, color=three_group ) ) + 
	geom_density( alpha=.3, adjust=1.8 ) +
	scale_x_log10()

ggplot( d[d$three_group !="control statement",], aes( a_proficiency , fill=three_group, color=three_group ) ) + 
	geom_density( alpha=.3, adjust=1.8 ) +
	scale_x_log10()


################################################################
####
####	Visualize Sub-Populations
####	
####	
####
################################################################

ggplot( d[,], aes( group, a_correct , fill=three_group ) ) + 
	bars +
	scale_fill_manual( "Condition",
		breaks=c("control statement","mindset","no header"), 
		values=c("red","blue","orange") 
		 ) + 
	ylab("Problems answered correctly post-study") +
	xlab("") + 
	facet_wrap( no_fraction_problems ~ newbie )

ggplot( d[ d$newbie == "veteran",], aes( intervention_date, a_proficiency , color=three_group ) ) + 
	geom_smooth( se=FALSE ) +
	scale_colour_manual( "Condition",
		breaks=c("control statement","mindset","no header"), 
		values=c("red","blue","orange") 
		 ) + 
	ylab("Problems answered correctly post-study") +
	xlab("") 


################################################################
####
####	Mann-Whitey U on Proficiencies
####	
####	Use the standard non-parametric test to see if the group
####	ranks differ.
####
####	Results: They do.
####
################################################################

d$a_prof_rank <- rank( d$a_proficiency )
aggregate( a_prof_rank ~ group, data = d, mean )

d$delta_prof_rank <- rank( d$a_proficiency - d$p_proficiency )
aggregate( delta_prof_rank ~ group, data = d, mean )

ggplot( d, aes( a_proficiency , fill=three_group ) ) + 
	geom_density( alpha=.3 ) +
	scale_x_log10()


wilcox.test( a_proficiency ~ gms , data=d )
wilcox.test( delta_prof ~ gms , data=d )

#	just no headers vs mindset
wilcox.test( a_proficiency ~ group_col , 
	data=d[d$group %in% c("no header","growth mindset","growth mindset + link"),] )


#	just control statements vs mindset
#	is p=.16 for post, p=.047 for delta
wilcox.test( a_proficiency ~ three_group , 
	data=d[d$three_group %in% c("control statement","mindset"),] )

wilcox.test( delta_prof ~ three_group , 
	data=d[d$three_group %in% c("control statement","mindset"),] )

	# Wilcoxon rank sum test with continuity correction
	# data:  delta_prof by three_group 
	# W = 869830734, p-value = 0.04708
	# alternative hypothesis: true location shift is not equal to 0 


################################################################
####
####	Proficiecies (Permuatation Test)
####	
####	Given that the distribution of proficiencies is pretty
####	crazy (see below), I wanted to use a robust test to determine
####	whether change in proficienices is higher in the mindset
####	groups.
####
####
####	Result Summary
####	The mindset treatments, compared against all other treatments, 
####	lead to a gain in proficiencies. The one-tailed p=.031.
####	
####
################################################################

#	Contrast of GMS vs all controls
#	Contrast of GMS vs all controls
#	p=.058

real_dif <- mean( d$delta_prof[d$gms] ) - mean( d$delta_prof[ ! d$gms] )
shuf_dif <- c()
lapply( 1:1000, function(x){
	tgms <- sample( d$gms, length(d$gms) )
	shuf_dif <<- c( shuf_dif , mean( d$delta_prof[tgms] ) - mean( d$delta_prof[ ! tgms] ) )
})

1 - mean( real_dif > shuf_dif )
#	0.031



#	Contrast of GMS vs all controls without "0 intervention" people
#	Contrast of GMS vs all controls without "0 intervention" people
#	p=.068

tmp <- d[ ! d$no_intervention, ]
real_dif <- mean( tmp$delta_prof[ tmp$gms  ] ) - mean( tmp$delta_prof[ ! tmp$gms  ] )
shuf_dif <- c()
lapply( 1:1000, function(x){
	tgms <- sample( tmp$gms, length(tmp$gms) )
	shuf_dif <<- c( shuf_dif , mean( tmp$delta_prof[tgms] ) - mean( tmp$delta_prof[ ! tgms] ) )
})

1 - mean( real_dif > shuf_dif )
#	0.068


#	Contrast of GMS vs no header
#	Contrast of GMS vs no header
#	p=.029

td <- d[ ! is.na( d$a_attemptsgms_vs_nohead ) , ]

real_dif <- mean( td$delta_prof[td$gms_vs_nohead] ) - mean( td$delta_prof[ !td$gms_vs_nohead] )
shuf_dif <- c()
lapply( 1:1000, function(x){
	tgms <- sample( td$gms_vs_nohead, length(td$gms_vs_nohead) )
	shuf_dif <<- c( shuf_dif , 
		mean( td$delta_prof[tgms %in% TRUE] ) - mean( td$delta_prof[ tgms %in% FALSE] ) )
	return(c())
})

1 - mean( real_dif > shuf_dif )
#	0.029



################################################################
####
####	Social Lab Analyses and Graphs 03-04-2012
####	
####	Ref Id: 83928209
####	
####
################################################################

#	proficiency histogram
text_size <- 12
ggplot(d, aes(a_proficiency)) + 
	geom_histogram(binwidth=1) + 
	xlim(0,30) +
	horzi_theme +
	theme(
	    axis.title.y = element_text( angle=90, size=12 ),
	    axis.title.x = element_text( size=12 ),
	    axis.text = element_text( size=12 )
	) +
	xlab("Proficiencies earned post-study") 
	ggsave("post_prof.png",dpi=300,width=10,height=5)

#	% achieving different proficiencies
summary( glm( any_profs ~ three_group + p_proficiency, data=d, family=binomial ) )
summary( glm( five_plus_profs ~ three_group + p_proficiency, data=d, family=binomial ) )
summary( glm( ten_plus_profs ~ three_group + p_proficiency, data=d, family=binomial ) )
aggregate( any_profs ~ three_group, data = d , mean ) 
aggregate( five_plus_profs ~ three_group, data = d , mean ) 
aggregate( ten_plus_profs ~ three_group, data = d , mean ) 


#	proficiency and negative binomial
text_size <- 12
ggplot( ) + 
	geom_density( aes(d$a_proficiency) ) +
	geom_density( aes( rnbinom( 10000, 1, .3 ) ), color="blue" ) +
#	geom_density( aes( rnbinom( 10000, 1.5, .4 ) ), color="red" ) +
	xlim(0,30) +
	horzi_theme +
	theme(
	    axis.title.y = element_text( angle=90, size=12 ),
	    axis.title.x = element_text( size=12 ),
	    axis.text = element_text( size=12 )
	) +
	xlab("Proficiencies earned post-study") 
	ggsave("post_prof_negative_binomial.png",dpi=300,width=10,height=5)

text_size <- 12
ggplot( ) + 
	geom_density( aes(d$a_proficiency) ) +
#	geom_density( aes( rnbinom( 10000, 1, .3 ) ), color="blue" ) +
#	geom_density( aes( rnbinom( 10000, 1.5, .4 ) ), color="red" ) +
	xlim(0,30) +
	horzi_theme +
	theme(
	    axis.title.y = element_text( angle=90, size=12 ),
	    axis.title.x = element_text( size=12 ),
	    axis.text = element_text( size=12 )
	) +
	xlab("Proficiencies earned post-study") 
	ggsave("post_prof_sans_negative_binomial.png",dpi=300,width=10,height=5)


library(pscl)
zi_nb = zeroinfl( a_proficiency ~ three_group + p_proficiency | 1 + p_proficiency, data = d, dist="negbin" )
summary(zi_nb)

	# Count model coefficients (negbin with log link):
	                       # Estimate Std. Error  z value Pr(>|z|)    
	# (Intercept)           1.2464032  0.0080476  154.879   <2e-16 ***
	# three_groupmindset    0.0271983  0.0107573    2.528   0.0115 *  
	# three_groupno header -0.0001620  0.0089986   -0.018   0.9856    
	# p_proficiency         0.0426283  0.0004383   97.254   <2e-16 ***
	# Log(theta)           -0.5502423  0.0054750 -100.501   <2e-16 ***
	
	# Zero-inflation model coefficients (binomial with logit link):
	              # Estimate Std. Error z value Pr(>|z|)    
	# (Intercept)   -1.19366    0.01536 -77.720  < 2e-16 ***
	# p_proficiency -6.34279    2.23878  -2.833  0.00461 ** 
	# ---
	# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
	
	# Theta = 0.5768 
	# Number of iterations in BFGS optimization: 31 
	# Log-likelihood: -4.255e+05 on 7 Df

exp( .0271983 )
#	1.027572

#	graph the model predictions
three_group <- c("control statement","mindset","no header")
p_proficiency <- c(0:50)
nd <- expand.grid( three_group=three_group, p_proficiency=p_proficiency )
nd$prediction <- predict( zi_nb, nd )

text_size <- 12
ggplot( nd, aes( p_proficiency , prediction, color=three_group) ) + 
	geom_smooth( se=FALSE ) +
	horzi_theme +
	theme(
	    axis.title.y = element_text( angle=90, size=12 ),
	    axis.title.x = element_text( size=12 ),
	    axis.text = element_text( size=12 )
	) +
	xlab("Pre-intervention proficiencies") +
	ylab("Predicted proficiencies post-intervention") +
	scale_colour_manual( 	"Condition", 
							breaks=c("control statement","mindset","no header") ,
							values=c("gray","blue","red") )
	
	ggsave("predicted proficiencies.png",dpi=300,width=10,height=5)




################################################################
####
####	Predicting Correct Problems with Negative Binomial Models
####	
####	Both the zero inflated and regular negbin models
####	show mindset outperfoming other groups on # correct.
####
################################################################

text_size <- 12
ggplot( ) + 
	geom_density( aes(d$a_correct) ) +
#	geom_density( aes( rnbinom( 10000, 2, .3 )  ), color="blue" ) +
	geom_density( aes( rnbinom( 10000, 2, .3 ) ), color="red" ) +
	xlim(0,30) +
	horzi_theme +
	theme(
	    axis.title.y = element_text( angle=90, size=12 ),
	    axis.title.x = element_text( size=12 ),
	    axis.text = element_text( size=12 )
	) +
	xlab("Problems correct post-study") 
	#ggsave("post_prof_negative_binomial.png",dpi=300,width=10,height=5)
	
#	a regular negative binomial models shows mindset group 
#	participants have a higher rate of answering problems correctly
#	exp(.0294) = 1.029836; i.e., a 3% higher rate

nb = glm.nb( a_correct ~ three_group + p_correct, data = d )
summary(nb)
	# Deviance Residuals: 
	    # Min       1Q   Median       3Q      Max  
	# -7.7556  -1.1638  -0.6180   0.0504   9.4444  
	
	# Coefficients:
	                       # Estimate Std. Error z value Pr(>|z|)    
	# (Intercept)           3.912e+00  7.626e-03 512.992  < 2e-16 ***
	# three_groupmindset    2.940e-02  1.066e-02   2.757  0.00583 ** 
	# three_groupno header -3.218e-03  8.909e-03  -0.361  0.71795    
	# p_correct             4.037e-03  1.928e-05 209.349  < 2e-16 ***
	# ---
	# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
	
	# (Dispersion parameter for Negative Binomial(0.4231) family taken to be 1)
	
	    # Null deviance: 266504  on 189474  degrees of freedom
	# Residual deviance: 232245  on 189471  degrees of freedom
	# AIC: 1852592
	
	# Number of Fisher Scoring iterations: 1
	
	
	              # Theta:  0.42308 
	          # Std. Err.:  0.00124 
	
	 # 2 x log-likelihood:  -1852581.56700 


#	The zero-inflated model shows mindset marginally, p=0.1, decreasing zeros
#	while simultaneously raising # correct by 2.6%, exp(.02561)

library(pscl)
zi_nb = zeroinfl( a_correct ~ three_group + p_correct | 1 + p_correct + three_group, data = d, dist="negbin" )
summary(zi_nb)
zeroinfl(formula = a_correct ~ three_group + p_correct | 1 + p_correct + three_group, 
    data = d, dist = "negbin")
	# Pearson residuals:
	     # Min       1Q   Median       3Q      Max 
	# -0.67891 -0.60783 -0.44634  0.04705 72.19582 
	
	# Count model coefficients (negbin with log link):
	                       # Estimate Std. Error  z value Pr(>|z|)    
	# (Intercept)           3.955e+00  7.728e-03  511.757   <2e-16 ***
	# three_groupmindset    2.561e-02  1.053e-02    2.432    0.015 *  
	# three_groupno header -2.883e-03  8.806e-03   -0.327    0.743    
	# p_correct             3.871e-03  3.061e-05  126.451   <2e-16 ***
	# Log(theta)           -7.745e-01  3.567e-03 -217.121   <2e-16 ***
	
	# Zero-inflation model coefficients (binomial with logit link):
	                     # Estimate Std. Error z value Pr(>|z|)    
	# (Intercept)          -2.70622    0.05138 -52.670  < 2e-16 ***
	# p_correct            -1.28352    0.28156  -4.559 5.15e-06 ***
	# three_groupmindset   -0.12196    0.07374  -1.654   0.0981 .  
	# three_groupno header  0.01566    0.05856   0.267   0.7891    
	# ---
	# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
	
	# Theta = 0.4609 
	# Number of iterations in BFGS optimization: 32 
	# Log-likelihood: -9.255e+05 on 9 Df


#	graph the model predictions continuously
three_group <- c("control statement","mindset","no header")
p_correct <- c(0:300)
nd <- expand.grid( three_group=three_group, p_correct=p_correct )
nd$prediction <- predict( zi_nb, nd )

text_size <- 12
ggplot( nd, aes( p_correct , prediction, color=three_group) ) + 
	geom_smooth( se=FALSE ) +
	horzi_theme +
	theme(
	    axis.title.y = element_text( angle=90, size=12 ),
	    axis.title.x = element_text( size=12 ),
	    axis.text = element_text( size=12 )
	) +
	xlab("Pre-intervention # correct") +
	ylab("Predicted # correct post-intervention") +
	scale_colour_manual( 	"Condition", 
							breaks=c("control statement","mindset","no header") ,
							values=c("gray","blue","red") )
	#	https://dl.dropbox.com/u/1454695/ka%20graphs/predicted%20correct.png
	ggsave("predicted correct.png",dpi=300,width=10,height=5)


#	graph the model predictions at the mean # prior correct
mean_p_correct <- mean(d$p_correct, na.rm=TRUE)
three_group <- c("control statement","mindset","no header")
p_correct <- mean_p_correct
nd <- expand.grid( three_group=three_group, p_correct=p_correct )
nd$prediction <- predict( zi_nb, nd )

text_size <- 12
ggplot( nd, aes( three_group , prediction, fill=three_group) ) + 
	geom_bar( stat="identity", alpha=.6  )  +
	horzi_theme +
	theme(
	    axis.title.y = element_text( angle=90, size=12 ),
	    axis.title.x = element_text( size=12 ),
	    axis.text = element_text( size=12 )
	) +
	xlab("") +
	ylab("Predicted # correct post-intervention") +
	scale_fill_manual( 	guide="none",
							breaks=c("control statement","mindset","no header") ,
							values=c("gray","blue","red") )
	#	https://dl.dropbox.com/u/1454695/ka%20graphs/predicted%20correct%20at%20mean.png
	ggsave("predicted correct at mean.png",dpi=300,width=5,height=5)

################################################################
####
####	Proficiecies (GBM)
####	
####	Result Summary
####	GBM cv.error does not seem to improve with inclusion of group
####	
####
################################################################

library(gbm)

CV_FOLDS <- 3
GBM_NTREES <- 1000

#	GBM OPTIMIZATION
model <- gbm(	a_proficiency ~ p_proficiency + intervention_time + group
				, data = d
			 	, distribution = "poisson"
			 	#,distribution = list(name="quantile",alpha=0.5)
			 	, n.trees = GBM_NTREES
			    , shrinkage = 0.01
			 	, interaction.depth = 4
			 	, n.minobsinnode = 100
			 	, verbose = FALSE
			 	, cv.folds = CV_FOLDS
			 	)


#	shows error in training and cv sets
best.iter <- gbm.perf(model, method="cv")
print(best.iter)
model$cv.error[best.iter]

#	graph the relative influence
summary(model,n.trees=best.iter) # based on the estimated best number of trees
#	with group: 	-5.627666
#	without group 	-5.627687; -5.625211

d$gbm_yhat <- predict( model , d, best.iter )

#	Sadly, the model seems to do a pretty horrible job of predicting
#	the values
ggplot( d, aes( a_proficiency, gbm_yhat ) ) +
	geom_smooth( se=F ) + 
	#geom_jitter( alpha=.03 ) +
	xlim(0,20)

#	though the correlation is pretty high
cor( d$gbm_yhat , d$a_proficiency )


################################################################
####
####	Deciding on a Distribution
####	
####	What distribution should we use in proficiency modeling?
####	
####	Conclusion: Probably the zero-inflated negative binomial.
####
################################################################



#	this is what the data look like
ggplot(d, aes(a_proficiency)) + geom_histogram() + xlim(0,30)
ggsave("post_prof.png",dpi=300,width=10,height=5)
#	https://dl.dropbox.com/u/1454695/post_prof.png


#	obviously isn't normal given it's a count, I thought: Poission! 
#	But see here: http://www.ats.ucla.edu/stat/r/dae/poissonreg.htm
#	Specifically: Poisson assumes equal variance and mean. Not true in our data:
	mean(d$a_proficiency)
	sd(d$a_proficiency)^2
#	So our data are "overdispersed." The UCLA page recommends negative binomial
#	as a generalized, more tolerant form of Poisson
#	http://www.ats.ucla.edu/stat/r/dae/nbreg.htm

#	let's compare our own data to poission and negative binomial distributions
#	The fit isn't so great... specifically, there are too many zeros!
ggplot( ) + 
	xlim(0,30) +
	geom_density( aes(d$a_proficiency) ) +
	#geom_density( aes( rpois( 10000, .5 ) ), color="red" ) +
	geom_density( aes( rnbinom( 10000, 1, .3 ) ), color="blue" ) +
	geom_density( aes( rnbinom( 10000, 1.5, .4 ) ), color="red" ) 


#	So let's try a zero-inflated Poisson and zero-inflated Neg-Binomial
#	see here:
#	http://datavoreconsulting.com/programming-tips/count-data-glms-choosing-poisson-negative-binomial-zero-inflated-poisson/
library(pscl)
zi_pois = zeroinfl( a_proficiency ~ group + p_proficiency  | 1 , data = d[,])

zi_pois = zeroinfl( a_proficiency ~ group + p_proficiency  | 1 + group + p_prof_group , data = d[,])
summary(zi_pois)
cor( d$a_proficiency, zi_pois$fitted.values )
#	0.1051805
#	This fit seems semi-reasonsable, r=.11. 
#	But see it plotted:
ggplot( ) + 
	geom_density( aes( d$a_proficiency ) ) +
	geom_density( aes( zi_pois$fitted.values ) , color="blue" ) +
	xlim(0,20)
	
count <- predict( zi_pois, type="count" )
response <- predict( zi_pois, type="response" )
d$yhat_zero <- predict( zi_pois, type="zero" )


ggplot( ) + 
	geom_density( aes( d$a_proficiency ) ) +
	geom_density( aes( response ) , color="blue" ) +
	geom_density( aes( count ) , color="green" ) +
	geom_density( aes( zero ) , color="red" ) +
	xlim(0,20)


predict( zi_pois, data.frame( group="no header", p_proficiency=0, p_prof_group="None" ) , type="zero" )

d$real_zero <- ifelse(d$a_proficiency == 0 ,1,0)
ggplot( d, aes( yhat_zero, real_zero ) ) + geom_jitter(alpha=.3) + geom_smooth()
	




################################################################
####
####	Effects on Proficiecies (with gam-poisson)
####	
####	Let's try a GAM
####	See: http://cran.r-project.org/doc/contrib/Fox-Companion/appendix-nonparametric-regression.pdf
####
####	This is probably not reliable; see: gam.check( a2 )
####	They note in ?gam that it does not work well with many 0s
####
####	
####	
################################################################

library( mgcv )
a2 <- gam( a_proficiency ~ group + s(p_proficiency) , data = d , family=poisson )
a1 <- gam( a_proficiency ~ s(p_proficiency) , data = d , family=poisson )

a1 <- gam( a_proficiency ~ s(p_proficiency) + s(intervention_time), data = d , family=poisson )
summary(a1)
	# formula:
	# a_proficiency ~ s(p_proficiency) + s(intervention_time)
	
	# Parametric coefficients:
	            # Estimate Std. Error z value Pr(>|z|)    
	# (Intercept) 1.165862   0.001341   869.4   <2e-16 ***
	# ---
	# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
	
	# Approximate significance of smooth terms:
	                       # edf Ref.df Chi.sq p-value    
	# s(p_proficiency)     8.982  9.000 545187  <2e-16 ***
	# s(intervention_time) 8.959  8.999  63493  <2e-16 ***
	# ---
	# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
	
	# R-sq.(adj) =  0.208   Deviance explained =   26%
	# UBRE score = 6.0443  Scale est. = 1         n = 189475


d$gam_yhat_no_group <- predict( a1,  d )
cor( d$gam_yhat_no_group, d$a_proficiency )


a2 <- gam( a_proficiency ~ group + s(p_proficiency) + s(intervention_time), data = d , family=poisson )
summary(a2)
# Formula:
# a_proficiency ~ group + s(p_proficiency) + s(intervention_time)

# Parametric coefficients:
                           # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                1.159538   0.001692 685.278  < 2e-16 ***
# grouppositive statement    0.005533   0.003796   1.458    0.145    
# groupscience statement     0.000807   0.003821   0.211    0.833    
# groupgrowth mindset        0.028052   0.003753   7.476 7.68e-14 ***
# groupgrowth mindset + link 0.022513   0.003766   5.978 2.26e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# Approximate significance of smooth terms:
                       # edf Ref.df Chi.sq p-value    
# s(p_proficiency)     8.982  9.000 545009  <2e-16 ***
# s(intervention_time) 8.958  8.999  63477  <2e-16 ***

# R-sq.(adj) =  0.208   Deviance explained =   26%
# UBRE score = 6.0439  Scale est. = 1         n = 189475


#	the model fit improves significantly with the addition of group
anova(a1,a2, test='Chisq')
	# Model 1: a_proficiency ~ s(p_proficiency) + s(intervention_time)
	# Model 2: a_proficiency ~ group + s(p_proficiency) + s(intervention_time)
	  # Resid. Df Resid. Dev     Df Deviance  Pr(>Chi)    
	# 1    189456    1334685                              
	# 2    189452    1334604 3.9997   81.499 < 2.2e-16 ***

#	exponentiate the coefficients so that they can be
#	interpreted as multiples. i.e., mindset increases prof by 2-3%
exp(coef(a2))

d$gam_yhat <- predict( a2,  d )
cor( d$gam_yhat, d$a_proficiency )

#	Sadly, the model seems to do a pretty horrible job of predicting
#	the values
ggplot( d, aes( a_proficiency, gam_yhat ) ) +
	geom_smooth( method="lm", se=F) + 
	geom_jitter( alpha=.03 ) +
	xlim(0,20)





################################################################
####
####	Effects on Proficiecies (binomial)
####	
####	Here, I convert post-proficiencies to binomial outcomes
####	so that I can use logistic regression.
####
####	Result Summary
####	The mindset conditions increase # of proficiencies
####	earned at a number of different benchmarks.
####	
################################################################



#	effect significant, p<.05 only when collapsed 
summary( glm( any_profs ~ p_proficiency + group , data=d, family=binomial ) )
summary( glm( any_profs ~ p_proficiency + three_group , data=d, family=binomial ) )
aggregate( any_profs ~ group, data = d , mean ) 

aggregate( any_profs ~ three_group, data = d , mean ) 

#	not significant at 2+
summary( glm( two_plus_profs ~ group + p_proficiency , data=d, family=binomial ) )
summary( glm( two_plus_profs ~ three_group + p_proficiency , data=d, family=binomial ) )
aggregate( two_plus_profs ~ three_group, data = d , mean ) 

#	not significant at 5+
summary( glm( five_plus_profs ~ group + p_proficiency, data=d, family=binomial ) )
summary( glm( five_plus_profs ~ three_group + p_proficiency , data=d, family=binomial ) )
summary( glm( five_plus_profs ~ gms + p_proficiency, data=d, family=binomial ) )
aggregate( five_plus_profs ~ group, data = d , mean ) 
aggregate( five_plus_profs ~ three_group, data = d , mean ) 

#	significant effect of the link condition 
summary( glm( ten_plus_profs ~ group + p_proficiency, data=d, family=binomial ) )
summary( glm( ten_plus_profs ~ three_group + p_proficiency, data=d, family=binomial ) )
aggregate( ten_plus_profs ~ group, data = d , mean ) 
aggregate( ten_plus_profs ~ three_group, data = d , mean ) 

#	significant effect of the link condition 
summary( glm( fifteen_plus_profs ~ group + p_proficiency, data=d, family=binomial ) )
summary( glm( fifteen_plus_profs ~ group_col + p_proficiency, data=d, family=binomial ) )
aggregate( fifteen_plus_profs ~ group, data = d , mean ) 

#	effect only significant when collapsed
summary( glm( twenty_plus_profs ~ group + p_proficiency, data=d, family=binomial ) )
summary( glm( twenty_plus_profs ~ group_col + p_proficiency, data=d, family=binomial ) )
aggregate( twenty_plus_profs ~ group, data = d , mean ) 
aggregate( twenty_plus_profs ~ three_groupr, data = d , mean ) 


################################################################
####
####	Exploring # Correct Distributions
####	
####	A zero-inflated negative binomial model suggests 	
####	positive effects of both mindset conditions.
####	But the predictions are very hard to interpret...
####	and the predictions are pretty terrible
####
################################################################

summary(d$a_correct)
summary(d$a_correct)

aggregate( p_correct ~ group , data = d , mean )
aggregate( a_correct ~ group , data = d , mean )
aggregate( delta_correct ~ group , data = d , mean )

ggplot( d, aes( a_correct ) )+
	geom_density() +
	xlim(0, 100) 
	
#	let's compare our own data to some negative binomial distributions
#	it looks potentially workable...
ggplot( ) + 
	xlim(0,50) +
	geom_density( aes(d$a_correct) ) +
	#geom_density( aes( rpois( 10000, .5 ) ), color="red" ) +
	#geom_density( aes( rnbinom( 10000, 1, .3 ) ), color="blue" ) +
	geom_density( aes( rnbinom( 10000, 2.2, .25) ), color="green" ) 


##	A zero-inflated negative binomial model predicting # correct

library(pscl)
zi_nb = zeroinfl( a_correct ~ group + p_correct | 1 , data = d, dist="negbin" )
summary(zi_nb)
	# Pearson residuals:
	     # Min       1Q   Median       3Q      Max 
	# -0.65045 -0.59820 -0.43979  0.05224 71.63244 
	
	# Count model coefficients (negbin with log link):
	                             # Estimate Std. Error  z value Pr(>|z|)    
	# (Intercept)                 3.909e+00  5.133e-03  761.495  < 2e-16 ***
	# grouppositive statement     5.697e-03  1.169e-02    0.487 0.625982    
	# groupscience statement      8.247e-04  1.165e-02    0.071 0.943568    
	# groupgrowth mindset         2.641e-02  1.165e-02    2.266 0.023455 *  
	# groupgrowth mindset + link  3.890e-02  1.169e-02    3.329 0.000872 ***
	# p_correct                   4.037e-03  3.225e-05  125.169  < 2e-16 ***
	# Log(theta)                 -8.602e-01  2.932e-03 -293.367  < 2e-16 ***
	
	# Zero-inflation model coefficients (binomial with logit link):
	            # Estimate Std. Error z value Pr(>|z|)  
	# (Intercept)  -14.210      6.151   -2.31   0.0209 *
	# ---
	# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
	
	# Theta = 0.4231 
	# Number of iterations in BFGS optimization: 35 
	# Log-likelihood: -9.263e+05 on 8 Df

#	the correlation between fitted and actual is reasonable, r=.3
	cor( d$a_correct, zi_nb$fitted.values )

#	This fit seems semi-reasonsable, r=.3.
#	However, the fitted values only use the counts -- not the zero probability
#	So I'm not sure how to make sense of the model predictions
#	You can get the probability of zero "at" a particular observed y with this command
#	e.g., here I'm asking the model's predicted probability of 0 for a no-header p_correct=0 person
#	who actually had 0:4 a_correct problems
l <- predict( zi_nb, data.frame( group="no header", p_correct=0 ), type="prob", at=0:4 )
l
predict( zi_nb, data.frame( group="no header", p_correct=0 ), type="count" )

#	see it plotted (note again the fitted values do not factor in the probability of zero!)
ggplot( ) + 
	geom_density( aes( d$a_correct ) ) +
	geom_density( aes( zi_nb$fitted.values ) , color="blue" ) +
	xlim(0, 100 )




################################################################
####
####	Permutation Tests with # Correct and Delta Correct
####	
####	Permutation tests show either trends in # correct or
####	significant one-tailed effects for change in correct.
####
################################################################

###	Permutation Test with # Correct

#	Contrast of GMS vs no header
#	Contrast of GMS vs no header
#	p=.058

	td <- d[ ! is.na( d$gms_vs_nohead ) , ]
	
	real_dif <- mean( td$a_correct[td$gms_vs_nohead] ) - mean( td$a_correct[ !td$gms_vs_nohead] )
	shuf_dif <- c()
	lapply( 1:1000, function(x){
		tgms <- sample( td$gms_vs_nohead, length(td$gms_vs_nohead) )
		shuf_dif <<- c( shuf_dif , 
			mean( td$a_correct[tgms %in% TRUE] ) - mean( td$a_correct[ tgms %in% FALSE] ) )
		return(c())
	})
	
	1 - mean( real_dif > shuf_dif )
	#	0.058

#	Contrast of GMS vs all controls
#	Contrast of GMS vs all controls
	real_dif <- mean( d$a_correct[d$gms] ) - mean( d$a_correct[ ! d$gms] )
	shuf_dif <- c()
	lapply( 1:1000, function(x){
		tgms <- sample( d$gms, length(d$gms) )
		shuf_dif <<- c( shuf_dif , mean( d$a_correct[tgms] ) - mean( d$a_correct[ ! tgms] ) )
		return(".")
	})
	
	1 - mean( real_dif > shuf_dif )
	#	0.04

###	Permutation Test with Change in # Correct

#	Contrast of GMS vs no header
#	Contrast of GMS vs no header
#	p=.025

	td <- d[ ! is.na( d$gms_vs_nohead ) , ]
	
	real_dif <- mean( td$delta_correct[td$gms_vs_nohead] ) - mean( td$delta_correct[ !td$gms_vs_nohead] )
	shuf_dif <- c()
	lapply( 1:1000, function(x){
		tgms <- sample( td$gms_vs_nohead, length(td$gms_vs_nohead) )
		shuf_dif <<- c( shuf_dif , 
			mean( td$delta_correct[tgms %in% TRUE] ) - mean( td$delta_correct[ tgms %in% FALSE] ) )
		return(c())
	})
	1 - mean( real_dif > shuf_dif )
	#	.025

#	Contrast of GMS vs all controls
#	Contrast of GMS vs all controls
	real_dif <- mean( d$delta_correct[d$gms] ) - mean( d$delta_correct[ ! d$gms] )
	shuf_dif <- c()
	lapply( 1:1000, function(x){
		tgms <- sample( d$gms, length(d$gms) )
		shuf_dif <<- c( shuf_dif , mean( d$delta_correct[tgms] ) - mean( d$delta_correct[ ! tgms] ) )
		return(".")
	})
	
	1 - mean( real_dif > shuf_dif )
	#	0.029






	
	
	