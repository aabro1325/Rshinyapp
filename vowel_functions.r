
library(car)



findSubjectParameters <- function(m, formants=c('F1','F2','F3','F4','F5'), one.set=FALSE, default_meas=0.325, 
                                  params=c('frequency', 'log_bandwidth'), collapse.maxformant=FALSE, verbose=FALSE){
    # Calculate means and standard devations for the purpose of normalizing vowel formants
    #
    # Args:
    #                  m : a data frame containing unnormalized formant measurements
    #           formants : the formants to include in the normalization 
    #
    # Returns a data frame containing formant means and standard devations for normalizing subjects' vowel spaces
    #
    if (!'subject'%in%names(m)){
        m$subject <- 'subject'
    }
    if (!'measurement'%in%names(m)){
        m$measurement <- default_meas
    }
    if (collapse.maxformant){
        m$max_formant <- 0
    }
    fstart <- proc.time()
    input_name <- paste(formants[1], params[1], sep='_')
    Fmeans <- data.frame(subject = aggregate(m[,input_name] ~ m$subject * m$phone * m$measurement * m$max_formant, FUN=mean)[,1],  
                           phone = aggregate(m[,input_name] ~ m$subject * m$phone * m$measurement * m$max_formant, FUN=mean)[,2],  
                     measurement = aggregate(m[,input_name] ~ m$subject * m$phone * m$measurement * m$max_formant, FUN=mean)[,3],  
                     max_formant = aggregate(m[,input_name] ~ m$subject * m$phone * m$measurement * m$max_formant, FUN=mean)[,4])
    for (param in params){
        for (i in 1:length(formants)){
            input_names <- paste(formants, param, sep='_')
            Fmeans_i <- aggregate(m[,input_names[i]] ~ m$subject * m$phone * m$measurement * m$max_formant, FUN=mean)
            names(Fmeans_i) <- c('subject', 'phone', 'measurement', 'max_formant', paste(formants[i], param, sep='_'))
            Fmeans <- merge(Fmeans, Fmeans_i, by=c('subject', 'phone', 'measurement', 'max_formant'))
            }
        }
    Fnorm <- data.frame(subject = aggregate(Fmeans[,input_name] ~ Fmeans$subject*Fmeans$measurement*Fmeans$max_formant, FUN=mean)[,1],  
                    measurement = aggregate(Fmeans[,input_name] ~ Fmeans$subject*Fmeans$measurement*Fmeans$max_formant, FUN=mean)[,2],  
                    max_formant = aggregate(Fmeans[,input_name] ~ Fmeans$subject*Fmeans$measurement*Fmeans$max_formant, FUN=mean)[,3])

    output_names <- c()
    for (param in params){
        mean_names <- c(paste(formants, param, 'mean', sep='_'))
        sd_names <- c(paste(formants, param, 'sd', sep='_'))
        output_names <- c(output_names, mean_names, sd_names)
        input_names <- paste(formants, param, sep='_')
        for (i in 1:length(formants)){
            Fnorm[,mean_names[i]] = aggregate(Fmeans[,input_names[i]] ~ Fmeans$subject*Fmeans$measurement*Fmeans$max_formant, FUN=mean)[,4]
            }
        for (i in 1:length(formants)){
            Fnorm[, sd_names [i]] = aggregate(Fmeans[,input_names[i]] ~ Fmeans$subject*Fmeans$measurement*Fmeans$max_formant, FUN=sd)[,4]
            }
        }
    if (verbose){
        print(paste('calculating parameter matrices for', length(unique(m$subject)), 'subjects,', length(unique(m$max_formant)), 
                  'max formants, and', length(unique(m$measurement)), 'measurement points took', paste(round((proc.time()-fstart)[3],3)), 'seconds'))
    } 
    if (one.set){
        Fnorm[,output_names]
    }else{
        Fnorm
    }
}


normalizeFormants <- function(m, pop_parameters, formant.param=NULL, formants=c('F1','F2','F3','F4','F5'), phones=levels(m$phone), best_maxformant=NULL, meas=7, collapse.maxformant=FALSE, params=c('frequency', 'log_bandwidth')){
    # Transform a subject's vowel measurements using Lobanov normalization and the population's parameters
    #
    # Args:
    #                  m : a data frame, including unnormalized formant measurements
    #     pop_parameters : formant means and standard deviations for the population
    #           formants : the formants to include in the normalization 
    #    best_maxformant : a max formant value to use for the measurments to be normalized
    #
    # Returns the same data frame with added columns for subject means, subject standard deviations, and normalized formant frequencies.
    #
    if (is.null(formant.param)){
        if (!'subject'%in%names(m)){
            m$subject <- 'subject'
            m$subject <- factor(m$subject)
        }
        if (is.null(best_maxformant)){
            formant.param <- findSubjectParameters(m, formants=formants, collapse.maxformant=collapse.maxformant, params=params)
        }else{
            formant.param <- findSubjectParameters(subset(m, max_formant==best_maxformant&phone%in%phones), params=params, formants=formants, 
                                                   collapse.maxformant=collapse.maxformant)
        }
    }
    if (!'measurement'%in%names(formant.param)){
        formant.param$measurement <- meas
    }
    for (param in params){
        #print(param)
        for (i in 1:length(formants)){
            formant <- formants[i]
            #print(formant)
            input_freq   <- m[,paste(formant, param, sep='_')]
            subject_mean <- formant.param[formant.param$measurement==meas,paste(formant, param, 'mean', sep='_')]
            subject_sd   <- formant.param[formant.param$measurement==meas,paste(formant, param, 'sd', sep='_')]
            pop_mean     <- pop_parameters[,paste(formant, param, 'mean', sep='_')]
            pop_sd       <- pop_parameters[,paste(formant, param, 'sd', sep='_')]
            #print (length(input_freq))
            #print (length(subject_mean))
            #print (subject_sd)
            #print (pop_sd)
            #print (pop_mean)
            m[,paste(formant, param, sep='n_')] <- ((input_freq - subject_mean) / subject_sd) * pop_sd + pop_mean
            }
        }
    m
    }

logPlotInfo <- function(fn='temp', infofile_path='plotinfo_temp.txt'){
    plotdata <- subset(formants_me, measurement==0.50)
    F1.anova <- anova(lm(F1_frequency~phone, data=plotdata))
    F2.anova <- anova(lm(F2_frequency~phone, data=plotdata))
    result <- data.frame(filename=fn, F1F=F1.anova[1,"F value"], F2F=F2.anova[1,"F value"])
    result$Fmean <- with(result, (F1F+F2F)/2)
    result$tokens <- length(unique(formants_me$token_id))    
    write.table(result, file=infofile_path, sep=',', quote=F, row.names=F)
}

plot.vowels <- function(m, param1, param2, group.by='phone', meas=NULL, mf=NULL, limits='global', size_param=FALSE, inches=FALSE, manual.xlim=FALSE, manual.ylim=FALSE, do.ellipses=TRUE, do.text=TRUE, noplot=FALSE, subject=NULL, main='', draw.zero=TRUE, replace.underscores=TRUE, rename.axes=TRUE, levels.to.highlight=NULL, label.big.circles=NULL, pch=19, col.override=NULL, overplot=FALSE, ellipse.lty=1, ellipse.lwd=2, plot_points=TRUE, log_axes='', diagonal=FALSE){
    # Plot vowel tokens on a single plot
    #
    # Args:
    #                  m : the data
    #               meas : the measurement point
    #             param1 : the parameter to plot on the x-axis
    #             param2 : the parameter to plot on the y-axis 
    #             limits : if 'global', use the  same axis limits for all speakers
    #         size_param : the parameter that will determine the size of the plot symbols. If FALSE, plot with points instead
    #             inches : controls the size of the symbols
    #        manual.xlim : manual x-axis limits
    #        manual.ylim : manual y-axis limits
    #        do.ellipses : whether to include vowel category ellipses in the plot
    #             noplot : don't plot
    #
    # Returns nothing
    #
    #
    # plot.vowels <- function(data, 'LD1', 'LD2', group.by='phone')
    #

    m <- m[!is.na(m[,param1]),]
    m <- m[!is.na(m[,param2]),]

    if (is.null(meas)&is.null(mf)){
        afsub <- m
        xlim <- rev(range(afsub[,param1])) 
        ylim <- rev(range(afsub[,param2]))
    }else{
        if (!'measurement'%in%names(m)){
            m$measurement <- meas[1]
        }
        if (meas=='all'){
            meas <- unique(m$measurement)
        }
        if (mf[1]=='all'){
            afsub <- subset(m, measurement%in%meas)
        }else{
            if (mf[1]=='best'){
                afsub <- subset(m, measurement%in%meas&max_formant==best_max)
            }else{
                afsub <- subset(m, measurement%in%meas&max_formant%in%mf)
            }
        }
        afsub <- subset(afsub, !is.na(afsub[,param1]))
        afsub <- subset(afsub, !is.na(afsub[,param2]))
        if (limits=='global'){
            xlim <- rev(range(na.omit(subset(m, measurement%in%meas)[,param1]))) 
            ylim <- rev(range(na.omit(subset(m, measurement%in%meas)[,param2])))
        }else{
            xlim <- rev(range(afsub[,param1])) 
            ylim <- rev(range(afsub[,param2]))
        }
        if (main==''){
            if (length(meas)>1){
                main <- subject
            }else{
                if (length(mf)==1){
                    main <- paste(subject, ', ', mf, ' Hz, ', meas, sep='')
                }else{
                    main <- paste(meas, sep='')
                }
            }
        }
    }
    if (manual.xlim[1]!=FALSE){
        xlim <- manual.xlim
    }
    if (manual.ylim[1]!=FALSE){
        ylim <- manual.ylim
    }
    if (is.null(subject) & !is.null(afsub$subject)){
        subject <- afsub$subject[1]
    }
    xlab <- param1
    ylab <- param2
    if (rename.axes){
        xlab <- gsub('F1n_', 'normalized F1 ', xlab)
        xlab <- gsub('F2n_', 'normalized F2 ', xlab)
        xlab <- gsub('F3n_', 'normalized F3 ', xlab)
        xlab <- gsub('F4n_', 'normalized F4 ', xlab)
        xlab <- gsub('F5n_', 'normalized F5 ', xlab)
        ylab <- gsub('F1n_', 'normalized F1 ', ylab)
        ylab <- gsub('F2n_', 'normalized F2 ', ylab)
        ylab <- gsub('F3n_', 'normalized F3 ', ylab)
        ylab <- gsub('F4n_', 'normalized F4 ', ylab)
        ylab <- gsub('F5n_', 'normalized F5 ', ylab)
        xlab <- gsub('frequency', 'frequency (Hz)', xlab)
        ylab <- gsub('frequency', 'frequency (Hz)', ylab)
    }
    if (replace.underscores){
        xlab <- gsub('_', ' ', xlab)
        ylab <- gsub('_', ' ', ylab)
    }
    if (noplot){
        plot(1, 1, type='n', xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, main=main, log=log_axes)
    }else{
        if (size_param==FALSE){
            if (!overplot){
                plot(1, 1, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type='n', main=main, log=log_axes)
                if (diagonal){
                    abline(0,1, col='gray')
                }
            }
            if (draw.zero){
                abline(h=0, col='gray')
                abline(v=0, col='gray')
            }
            if (is.null(levels.to.highlight)){
                if (is.null(col.override)){
                    colors <- rainbow(1.2*length(levels(afsub[,group.by])), s=1, v=0.9)
                }else{
                    colors <- col.override
                }
                if (plot_points){
                    points(afsub[,param1], afsub[,param2], pch=pch, cex=0.6, col=colors[c(afsub[,group.by])])
                }
            }else{
                afsubsub <- afsub[afsub[,group.by]%in%levels.to.highlight,]
                afnotsub <- afsub[!afsub[,group.by]%in%levels.to.highlight,]
                if (is.null(col.override)){
                    colors <- rainbow(1.2*length(levels(afsub[,group.by])), s=0.1, v=0.9)
                }else{
                    colors <- col.override
                }
                if (plot_points){
                    points(afnotsub[,param1], afnotsub[,param2], pch=pch, cex=0.6, col=colors[c(afnotsub[,group.by])])
                }
                if (is.null(col.override)){
                    colors <- rainbow(1.2*length(levels(afsub[,group.by])), s=1, v=0.9)
                }else{
                    colors <- col.override
                }
                if (plot_points){
                    points(afsubsub[,param1], afsubsub[,param2], pch=pch, cex=0.6, col=colors[c(afsubsub[,group.by])])
                }
            }
        }else{
            symbols(afsub[,param1], afsub[,param2], circles=afsub[,size_param], inches=inches,
                xlim=xlim, ylim=ylim, 
                xlab=xlab, ylab=ylab, 
                bg=rainbow(1.2*length(levels(afsub[,group.by])))[c(afsub[,group.by])],
                main=main)
            if (!is.null(label.big.circles)){
                big.circles <- subset(afsub, afsub[,size_param]>label.big.circles)
                text(big.circles[,param1], big.circles[,param2], labels=round(big.circles[,size_param],0), cex=0.5)
            }
        }
    }
    if (is.null(levels.to.highlight)){
        colors <- rainbow(1.2*length(levels(afsub[,group.by])), v=1, alpha=0.5)
    }else{
        #colors <- rainbow(1.2*length(levels(afsub[,group.by])), v=0.85, alpha=c(0,1)[1+as.numeric(levels(afsub[,group.by])%in%levels.to.highlight)])
        colors <- rainbow(1.2*length(levels(afsub[,group.by])), v=0.85, alpha=c(0.25,0.75)[1+as.numeric(levels(afsub[,group.by])%in%levels.to.highlight)])
    }
    for (i in 1:length(levels(afsub[,group.by]))){
        p <- levels(afsub[,group.by])[i]
        #if (length(subset(afsub, afsub[,group.by]==p)[,1])>1 & do.ellipses){
        if (length(subset(afsub, afsub[,group.by]==p)[,1])>2 & do.ellipses){
            dataEllipse(c(subset(afsub, afsub[,group.by]==p)[,param1]), c(subset(afsub, afsub[,group.by]==p)[,param2]), 
            center.pch=F, plot.points=F, levels=.682, add=TRUE, segments=102, robust=TRUE, 
            col=colors[i], lty=ellipse.lty, lwd=ellipse.lwd)
        #}else{
        #    print(paste('no ellipse for',p))
        }
    }   
    if (do.text){
        if (is.null(levels.to.highlight)){
            #colors <- rainbow(1.2*length(levels(afsub[,group.by])), s=0.9, v=0)
            colors <- rainbow(1.2*length(levels(afsub[,group.by])), s=0.7, v=0.5)
        }else{
            colors <- rainbow(1.2*length(levels(afsub[,group.by])), 
                                 #v=c(.6,0)[1+as.numeric(levels(afsub[,group.by])%in%levels.to.highlight)], 
                                 #alpha=c(1,1)[1+as.numeric(levels(afsub[,group.by])%in%levels.to.highlight)],
                                 #s=c(.4,1)[1+as.numeric(levels(afsub[,group.by])%in%levels.to.highlight)])
                                 s=c(.5,0.6)[1+as.numeric(levels(afsub[,group.by])%in%levels.to.highlight)], 
                                 v=c(.7,0.6)[1+as.numeric(levels(afsub[,group.by])%in%levels.to.highlight)],
                                 alpha=c(0.8,1)[1+as.numeric(levels(afsub[,group.by])%in%levels.to.highlight)])
        }
        
        for (i in 1:length(levels(afsub[,group.by]))){
            p <- levels(afsub[,group.by])[i]
            #phone.means1 <- aggregate(afsub[,param1] ~ afsub[,group.by], FUN=mean)
            #phone.means2 <- aggregate(afsub[,param2] ~ afsub[,group.by], FUN=mean)
            text(mean(afsub[afsub[,group.by]==p,param1]), mean(afsub[afsub[,group.by]==p,param2]), labels=p, cex=1, col=colors[i])
        }
        #text(aggregate(afsub[,param1] ~ afsub[,group.by], FUN=mean)[,2],
        #     aggregate(afsub[,param2] ~ afsub[,group.by], FUN=mean)[,2],
        #     aggregate(afsub[,param1] ~ afsub[,group.by], FUN=mean)[,1], cex=1, col=colors)
    }
}

set.limits <- function(){
    #SETS REASONABLE AXIS RANGES FOR NORMALIZED F1-F2 PLOTS SCALED BACK INTO Hz
    #SO THAT EVERYONE'S NORMALIZED VOWELS ARE PLOTTED ON THE SAME SCALES
    if (sum(grepl('n_',params))){
        return(list(x=c(2600,800), y=c(900,250)))
    }else{
        return(list(x=FALSE, y=FALSE))
    }
}

plain.vowelplot <- function(xlim=set.limits()$x, ylim=set.limits()$y){
    #USE plot.vowels() TO MAKE A BASIC VOWEL PLOT 
    plot.vowels(plotdata, params[1], params[2], main=main, manual.xlim=xlim, manual.ylim=ylim)
}

words.vowelplot <- function(xlim=set.limits()$x, ylim=set.limits()$y){
    #USE plot.vowels() TO MAKE A VOWEL PLOT WITH THE WORDS LABELED
    plot.vowels(plotdata, params[1], params[2], main=main, noplot=TRUE, do.text=FALSE, manual.xlim=xlim, manual.ylim=ylim)
    text(plotdata[,params], labels=plotdata$word, col=rainbow(1.2*length(levels(plotdata$phone)), s=0.5, v=0.5)[plotdata$phone], cex=0.6)
}

arrows.vowelplot <- function(xlim=set.limits()$x, ylim=set.limits()$y){
    #USE plot.vowels() TO MAKE A VOWEL PLOT WITH ARROWS FOR VOWELS AT TWO TIME POINTS
    plot.vowels(subset(plotdata, measurement==0.5), params[1], params[2], main=main, noplot=TRUE, do.text=TRUE, manual.xlim=xlim, manual.ylim=ylim, levels.to.highlight=c())
    plotdata$color1 <- rainbow(1.2*length(levels(plotdata$phone)), a=1, s=1, v=0.6)[plotdata$phone]
    plotdata$color2 <- rainbow(1.2*length(levels(plotdata$phone)), a=0.3, s=1, v=0.8)[plotdata$phone]
    for (ti in unique(plotdata$token_id)){
        token <- plotdata[plotdata$token_id==ti,]
        arrows(token[token$measurement==0.25,params[1]], token[token$measurement==0.25,params[2]], token[token$measurement==0.75,params[1]], token[token$measurement==0.75,params[2]], length=0.1, angle=30, col=token$color2, lty=1, lwd=1)
    }
    phone_means1 <- aggregate(plotdata[,params[1]]~plotdata$phone*plotdata$measurement, FUN=mean)
    phone_means2 <- aggregate(plotdata[,params[2]]~plotdata$phone*plotdata$measurement, FUN=mean)
     
    for (i in 1:length(unique(plotdata$phone))){
        p <- levels(plotdata$phone)[i]
        p_means <- c(phone_means1[phone_means1[,1]==p&phone_means1[,2]==0.25,3], phone_means2[phone_means2[,1]==p&phone_means2[,2]==0.25,3], phone_means1[phone_means1[,1]==p&phone_means1[,2]==0.75,3], phone_means2[phone_means2[,1]==p&phone_means2[,2]==0.75,3])
        arrows(p_means[1], p_means[2], p_means[3], p_means[4], length=0.1, angle=30, col=rainbow(1.2*length(levels(plotdata$phone)), s=1, v=0.7)[i], lty=1, lwd=2)
    }
}

context.vowelplot <- function(context_param, context_values, xlim=set.limits()$x, ylim=set.limits()$y){
    #USE plot.vowels() TO MAKE A VOWEL PLOT HIGHLIGHTING TOKENS IN A PARTICULAR CONTEXT
    plotdata.no_match <- plotdata[!plotdata[,context_param]%in%context_values,]
    plotdata.matching <- plotdata[plotdata[,context_param]%in%context_values,]
    plot.vowels(plotdata.no_match, params[1], params[2], main=main, manual.xlim=xlim, manual.ylim=ylim, pch=1, ellipse.lty=2, ellipse.lwd=1, levels.to.highlight=unique(plotdata.matching$phone))

    matching_means1 <- aggregate(plotdata.matching[,params[1]]~plotdata.matching$phone, FUN=mean)
    matching_means2 <- aggregate(plotdata.matching[,params[2]]~plotdata.matching$phone, FUN=mean)
    no_match_means1 <- aggregate(plotdata.no_match[,params[1]]~plotdata.no_match$phone, FUN=mean)
    no_match_means2 <- aggregate(plotdata.no_match[,params[2]]~plotdata.no_match$phone, FUN=mean)

    for (i in 1:length(unique(plotdata$phone))){
        p <- levels(plotdata$phone)[i]
        if (p%in%intersect(matching_means1[,1], no_match_means1[,1])){
            p_means <- c(no_match_means1[no_match_means1[,1]==p,2], matching_means1[matching_means1[,1]==p,2], no_match_means2[no_match_means2[,1]==p,2], matching_means2[matching_means2[,1]==p,2])
            print (p_means)
            lines(p_means[1:2], p_means[3:4], col=rainbow(1.2*length(levels(plotdata$phone)), s=0.75, v=0.85)[i])
        }
    }
    plot.vowels(plotdata.matching, params[1], params[2], main=main, overplot=TRUE, levels.to.highlight=unique(plotdata.matching$phone))


}


