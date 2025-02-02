



#LOAD FUNCTIONS USEFUL FOR PLOTTING VOWELS
source('formant_functions.r')
source('vowel_functions.r')
library(plyr)
library(shiny)

set.limits <- function(params){
    #SETS REASONABLE AXIS RANGES FOR NORMALIZED F1-F2 PLOTS SCALED BACK INTO Hz
    #SO THAT EVERYONE'S NORMALIZED VOWELS ARE PLOTTED ON THE SAME SCALES
    if (sum(grepl('n_',params))){
        return(list(x=c(2600,800), y=c(900,250)))
    }else{
        return(list(x=FALSE, y=FALSE))
    }
}

plain.vowelplot <- function(plotdata, params, xlim=FALSE, ylim=FALSE, main='', log_axes='', diagonal=FALSE){
    #USE plot.vowels() TO MAKE A BASIC VOWEL PLOT 
    plot.vowels(plotdata, params[1], params[2], main=main, manual.xlim=xlim, manual.ylim=ylim, do.ellipses=(params[1]!=params[2]), log_axes=log_axes, diagonal=diagonal)
}

words.vowelplot <- function(plotdata, params, xlim=FALSE, ylim=FALSE, main='', log_axes='', diagonal=FALSE){
    #USE plot.vowels() TO MAKE A VOWEL PLOT WITH THE WORDS LABELED
    plot.vowels(plotdata, params[1], params[2], main=main, noplot=TRUE, do.text=FALSE, manual.xlim=xlim, manual.ylim=ylim, do.ellipses=(params[1]!=params[2]), log_axes=log_axes, diagonal=diagonal)
    text(plotdata[,params], labels=plotdata$word, col=rainbow(1.2*length(levels(plotdata$phone)), s=0.5, v=0.5)[plotdata$phone], cex=0.6)
}

context.vowelplot <- function(context_param, context_values, plotdata, params, xlim=FALSE, ylim=FALSE, main='', log_axes='', diagonal=FALSE){
    #USE plot.vowels() TO MAKE A VOWEL PLOT HIGHLIGHTING TOKENS IN A PARTICULAR CONTEXT
    plotdata.no_match <- plotdata[!plotdata[,context_param]%in%context_values,]
    plotdata.matching <- plotdata[plotdata[,context_param]%in%context_values,]
    plot.vowels(plotdata.no_match, params[1], params[2], main=main, manual.xlim=xlim, manual.ylim=ylim, pch=1, ellipse.lty=2, ellipse.lwd=1, levels.to.highlight=unique(plotdata.matching$phone), log_axes=log_axes, diagonal=diagonal)

    matching_means1 <- aggregate(plotdata.matching[,params[1]]~plotdata.matching$phone, FUN=mean)
    matching_means2 <- aggregate(plotdata.matching[,params[2]]~plotdata.matching$phone, FUN=mean)
    no_match_means1 <- aggregate(plotdata.no_match[,params[1]]~plotdata.no_match$phone, FUN=mean)
    no_match_means2 <- aggregate(plotdata.no_match[,params[2]]~plotdata.no_match$phone, FUN=mean)

    # for (i in 1:length(unique(plotdata$phone))){
    for (i in 1:length(levels(plotdata$phone))){
        p <- levels(plotdata$phone)[i]
        print(p)
        if (p%in%intersect(matching_means1[,1], no_match_means1[,1])){
            p_means <- c(no_match_means1[no_match_means1[,1]==p,2], matching_means1[matching_means1[,1]==p,2], no_match_means2[no_match_means2[,1]==p,2], matching_means2[matching_means2[,1]==p,2])
            lines(p_means[1:2], p_means[3:4], col=rainbow(1.2*length(levels(plotdata$phone)), s=0.75, v=0.85)[i])
        }
    }
    plot.vowels(plotdata.matching, params[1], params[2], main=main, overplot=TRUE, levels.to.highlight=unique(plotdata.matching$phone), do.ellipses=(params[1]!=params[2]), log_axes=log_axes)
}


#USEFUL LISTS OF PHONES AND WORDS
vowels_sorted <- c('IY1','IH1','EY1','EH1','AE1','AY1','AW1','AA1','AO1','AH1','ER1','OY1','OW1','UH1','UW1')

some_allophones_sorted <- c("IY", "IH", "EY", "EH", "EHL", "AEN", "AE", "AA", "AO", "AH", "OW", "OWL", "UH", "UHL", "UW", "UWL")

simple_vowels <- c('IY1','IH1','EY1','EH1','AE1','AA1','AO1','AH1','OW1','UH1','UW1')
nasals <- c('M','N','NG')
liquids <- c('L','R')
function_words <- toupper(c('a', 'and', 'as', 'at', 'because', 'but', 'can', 'do', 'got', 'had', 'hadn\'t', 'he', 'he\'d', 'his', 'i', 'if', 'in', 'it', 'my', 'no', 'not', 'of', 'oh', 'on', 'out', 'she', 'so', 'some', 'such', 'the', 'to', 'too', 'was', 'which', 'where', 'you'))

txt_file_path <- 'get_formants_output/'


###########################################################################
# READ AND ORGANIZE THE FORMANT DATA
###########################################################################


# formants_everyone <- c()
formants_us <- list()
all_colnames <- c()
all_lefts <- c()
all_rights <- c()
# speaker_list <- c('Vancouver, WA','Raleigh (5yo)',paste(logfile$code[130:nrow(logfile)]))

txt_files <- list.files(txt_file_path)

for (fn in txt_files){

	# speaker <- fn
	# print(speaker)

  if (grepl('one_script_out',fn)){
    my_data <- read.csv(paste0(txt_file_path,fn), h=T, na.strings="--undefined--")
    
  }else{

  	#LOAD THE FILE AND LABEL THE COLUMNS.  EACH ROW HAS SIX MEASUREMENTS (F1 AND F2 AT 25%, 50%, AND 75% OF THE PHONE'S DURATION)
  	my_data <- read.csv(paste0(txt_file_path,fn), h=F, na.strings="--undefined--")
  	if (my_data[1,1]=='word'){
  		my_data <- read.csv(paste0(txt_file_path,fn), h=T, na.strings="--undefined--")
  		print ('text file has header')
  	}else{
  		
  		if (ncol(my_data)==10){
  			print ('labeled ten columns')
  			names(my_data) <- c('word','left','phone','right','F1_25','F2_25','F1_50','F2_50','F1_75','F2_75')
  		}else{
  			print ('labeled four columns')
  			names(my_data)[1:4] <- c('word','left','phone','right')
  		}
  	}

  	names(my_data)[1:4] <- c('word','left','phone','right')
  	my_data$token_id <- 1:nrow(my_data)

  }
# speaker textgrid  sound phonetier word_id token_id  leftword  word  rightword phone phonestart  phoneend  left2 left1 left  right right1  right2  total_formants  max_formant mdist sos energy  rsq1  rsq2  rsq3  rsq4  rsq5  F1_0  F1_25 F1_50 F1_75 F1_100  F2_0  F2_25 F2_50 F2_75 F2_100  F3_0  F3_25 F3_50 F3_75 F3_100





  for (sp in unique(my_data$speaker)){
  	formants_me <- my_data[my_data$speaker==sp,]
    # print(formants_me$phone)
  	#SUBSTITUTE OW FOR AO BEFORE R (TREATING WORDS LIKE "OR" AS HAVING THE PHONEME O, NOT OPEN O)
  	formants_me[formants_me$right=='R',]$phone <- gsub('AO', 'OW', formants_me[formants_me$right=='R',]$phone)
  	#MAKE COLUMNS TO INDICATE VOWELS, PRIMARY STRESS, AND IMPORTANT CONTEXTS
  	formants_me$vowel <- grepl('[012]', formants_me$phone)
  	formants_me$primary <- grepl('1', formants_me$phone) & !grepl('[012]', formants_me$right) & !toupper(formants_me$word) %in% function_words
  	formants_me$context <- ifelse(formants_me$right%in%nasals, 'prenasal', 
  		                   ifelse(formants_me$right%in%liquids, 'preliquid', 'other')) 

  	#INCLUDE ONLY VOWELS WITH PRIMARY STRESS IN THE DATA FRAME
  	# formants_me <- subset(formants_me, primary)
  	# formants_me$phone <- factor(formants_me$phone, levels=vowels_sorted)
    formants_me$phone <- factor(formants_me$phone, levels=c(vowels_sorted, setdiff(levels(formants_me$phone),vowels_sorted)))

  	#NORMALIZE VOWEL FORMANT FREQUENCIES AND THEN RESCALE BACK TO Hz USING BUCKEYE CORPUS VOWEL SPACE MEASUREMENTS

  	all_colnames <- c(all_colnames, colnames(formants_me))
  	all_rights <- c(all_rights, paste(formants_me$right))
  	all_lefts <- c(all_lefts, paste(formants_me$left))
  	# formants_me$speaker <- speaker
  	formants_us[[sp]] <- formants_me
  }
}

all_data_cols <- setdiff(unique(all_colnames), c("", "word", "left", "phone", "right", "token_id", "vowel", "primary", "context"))
all_context <- setdiff(unique(formants_me$context), "other")
all_rights <- sort(unique(all_rights))
all_lefts <- sort(unique(all_lefts))

#word','left','phone','right, 

#subset:
# vowel only
# primary stress only
# simple vowels only


ui <- fluidPage(

  titlePanel("vowel plots"),
  sidebarPanel(   
    selectInput(inputId ="speaker", label = "Choose the speaker", 
      # choices = names(formants_us), selected = names(formants_us)[1]),
      choices = names(formants_us), selected = 'Jeff'),
    selectInput(inputId ="param1", label = "Choose what to plot on the x-axis", 
      choices = c('nothing (boxplot)', all_data_cols), selected = 'F2_25'),
    selectInput(inputId ="param2", label = "Choose what to plot on the y-axis", 
      choices = all_data_cols, selected = 'F1_25'),
    radioButtons('bark_scale', label='Formant scale', choices = c('Hz','bark'), selected = 'Hz', inline = TRUE),
    # selectInput(inputId ="plot_type", label = "Choose the plot type", 
    #   choices = plot_types, selected = plot_types[1]), 
    checkboxInput("log_x", "Log x-axis", FALSE),
    checkboxInput("log_y", "Log y-axis", FALSE),
    checkboxInput("equal_axis", "Equal axis ranges", FALSE),
    checkboxInput("identical_axis", "Identical axes", FALSE),
    checkboxInput("show_diagonal", "Show diagonal", FALSE),
    checkboxInput("words", "Show words", FALSE),
    # selectizeInput(inputId ="contexts", label = "Choose contexts to highlight", 
    #   choices = unique(all_context), selected = c(), multiple=TRUE),
    checkboxInput("vowel_only", "Plot vowels only", TRUE),
    checkboxInput("simple_only", "Plot simple vowels only", TRUE),
    checkboxInput("primary_only", "Plot primary stressed vowels only", TRUE),
    checkboxInput("trim_outliers", "Trim outliers (>3 sd from phone mean)", FALSE),
    checkboxInput("trim_outliers2", "Trim outliers (>2 sd from phone mean)", FALSE),
    selectizeInput(inputId ="exclude_lefts", label = "Choose preceding contexts to exclude", 
      choices = unique(all_lefts), selected = c(), multiple=TRUE),
    selectizeInput(inputId ="exclude_rights", label = "Choose following contexts to exclude", 
      choices = unique(all_rights), selected = c(nasals, liquids), multiple=TRUE),
    selectizeInput(inputId ="lefts", label = "Choose preceding contexts to highlight", 
      choices = unique(all_lefts), selected = c(), multiple=TRUE),
    selectizeInput(inputId ="rights", label = "Choose following contexts to highlight", 
      choices = unique(all_rights), selected = c(), multiple=TRUE),
    # sliderInput("xrange", "Choose x-axis range", 
    #   min = 300, max = 4400, value = c(600,3000), sep='') ,
    # sliderInput("yrange", "Choose y-axis range", 
    #   min = 0, max = 2500, value = c(300,1500), sep='') 
    checkboxInput("manual_range", "Manual axis ranges", FALSE),
    sliderInput("x_range", "x-axis range", 
                min = 0, max = 3500, value = c(500,4500), sep=''),
    sliderInput("y_range", "y-axis range", 
                min = 0, max = 1500, value = c(200,1200), sep='')
  ),

  mainPanel( 
    plotOutput("plot") 
  )
)



server <- function(input, output, session) {


  observe({

    # print('m')
  	formants_me <- formants_us[[input$speaker]]
  	plotdata <- subset(formants_me, !left%in%input$exclude_lefts & !right%in%input$exclude_rights)
  	params <- c(input$param1, input$param2)

    # print('n')
    if (input$bark_scale=='bark'){
      # print (grep('[Ff][0-9]',params[1]))
      if (length(grep('[Ff][0-9]',params[1]))){
        plotdata[,params[1]] <- bark(plotdata[,params[1]])
      }
      # print (grep('[Ff][0-9]',params[2]))
      if (length(grep('[Ff][0-9]',params[2]))){
        plotdata[,params[2]] <- bark(plotdata[,params[2]])
      }
    }

    # print('o')
  	if (input$vowel_only){
  		plotdata <- subset(plotdata, vowel)
  	}
  	if (input$simple_only){
  		plotdata <- subset(plotdata, phone%in%simple_vowels)
  	}
  	if (input$primary_only){
  		plotdata <- subset(plotdata, primary)
  	}

    if (input$trim_outliers){
      plotdata <- trimParameter(plotdata, params, bywhat='phone', sds=3)
    }
  	if (input$trim_outliers2){
  		plotdata <- trimParameter(plotdata, params, bywhat='phone', sds=2)
  	}

    # print('p')
    if (params[1]!='nothing (boxplot)'){
      xrange <- range(plotdata[,params[1]], na.rm=T)
      yrange <- range(plotdata[,params[2]], na.rm=T)
      if (input$log_x){
        if (input$log_y){
          log_axes <- 'xy'
        }else{
          log_axes <- 'x'          
        }
      }else{
        if (input$log_y){
          log_axes <- 'y'
        }else{
          log_axes <- ''          
        }
      }
      if (input$manual_range){
        xrange = input$x_range
        yrange = input$y_range
      }else if (input$identical_axis){
          xrange <- range(c(xrange,yrange))
          yrange <- xrange
      }else if (input$equal_axis){
        # print (xrange)
        # print (yrange)
        if (abs(diff(xrange))>abs(diff(yrange))){
          yrange <- yrange + c(-1,1)*(abs(diff(xrange))-abs(diff(yrange)))
        }else{  
          xrange <- xrange + c(-1,1)*(abs(diff(yrange))-abs(diff(xrange)))
        }
      }
      if (length(grep('[Ff][0-9]',params[1]))){
        xrange <- rev(xrange)
      }
      if (length(grep('[Ff][0-9]',params[2]))){
        yrange <- rev(yrange)
      }
    }

    # print('q')
    output$plot <- renderPlot({
      # print('w')
      if (input$param1=='nothing (boxplot)'){
        # print ('x')
        if (length(input$lefts)){
          # print('y1')
          plotdata$allophone <- paste(plotdata$phone)
          plotdata$allophone[plotdata$left%in%input$lefts] <- paste0(plotdata$phone[plotdata$left%in%input$lefts], input$lefts[1])
          boxplot(plotdata[,input$param2] ~ plotdata$allophone, horizontal=TRUE, las=1, xlab=input$param2, ylab='phone', log=ifelse(input$log_y,'x',''))
        }else if (length(input$rights)){
          # print('y2')
          plotdata$allophone <- paste(plotdata$phone)
          plotdata$allophone[plotdata$right%in%input$rights] <- paste0(plotdata$phone[plotdata$right%in%input$rights], input$rights[1])
          boxplot(plotdata[,input$param2] ~ plotdata$allophone, horizontal=TRUE, las=1, xlab=input$param2, ylab='phone', log=ifelse(input$log_y,'x',''))
        }else{
          # print('y3')
          boxplot(plotdata[,input$param2] ~ plotdata$phone, horizontal=TRUE, las=1, xlab=input$param2, ylab='phone', log=ifelse(input$log_y,'x',''))
        }
        # print('z')
      }else{

    		if (length(input$lefts)){
    			context.vowelplot('left', input$lefts, plotdata, params, xlim=xrange, ylim=yrange, log_axes=log_axes, diagonal=input$show_diagonal)
    		}else if (length(input$rights)){
    			context.vowelplot('right', input$rights, plotdata, params, xlim=xrange, ylim=yrange, log_axes=log_axes, diagonal=input$show_diagonal)
    		}else{
    			if (input$words){
    				words.vowelplot(plotdata, params, xlim=xrange, ylim=yrange, log_axes=log_axes, diagonal=input$show_diagonal)
    			}else{
            plain.vowelplot(plotdata, params, xlim=xrange, ylim=yrange, log_axes=log_axes, diagonal=input$show_diagonal)
    			}
    	  }
      }
	  }, height=600, width=600)
  })
}

shinyApp(ui, server)
