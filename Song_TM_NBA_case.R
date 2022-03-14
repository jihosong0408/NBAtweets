#' Title: NLP HW 1
#' NAME: Jiho Song
#' Date: March 11 2022
#' 

# To limit errors please run this code
Sys.setlocale('LC_ALL','C')

# set your working directory
setwd("C:/Users/asuka/Desktop/Hult/2022 Spring/Text Analytics/Text-Mining-NLP/Case/Case I/Data")

# Load libraries
library(tm)
library(ggplot2)
library(ggthemes)
library(stringi)
library(qdap) 
library(wordcloud)
library(RColorBrewer)

### setting basic functions 
# make funcion to clean data
basicSubs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  return(x)
}

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create some stopwords using the 'SMART' lexicon and add team name 
stops <- c(stopwords('SMART'),'nba','basketball')

# load the case study data
oct19 <- read.csv('A_Oct2019.csv',encoding = 'UTF-8')
nov19 <- read.csv('B_Nov2019.csv',encoding = 'UTF-8')
dec19 <- read.csv('C_Dec2019.csv',encoding = 'UTF-8')
feb20 <- read.csv('E_Feb2020.csv',encoding = 'UTF-8')
mar20 <- read.csv('F_Mar2020.csv',encoding = 'UTF-8')
apr20 <- read.csv('G_Apr2020.csv',encoding = 'UTF-8')
may20 <- read.csv('H_May2020.csv',encoding = 'UTF-8')
jun20 <- read.csv('I_June2020.csv',encoding = 'UTF-8')
jul20 <- read.csv('J_July2020.csv',encoding = 'UTF-8')
aug20 <- read.csv('K_Aug2020.csv',encoding = 'UTF-8')
sep20 <- read.csv('L_Sep2020.csv',encoding = 'UTF-8')
oct20 <- read.csv('M_Oct2020.csv',encoding = 'UTF-8')

# make combined all data
total <- list(oct19,nov19,dec19,feb20,mar20,apr20,may20,jun20,jul20,aug20,sep20,oct20)

# set time, team , division list
time <- c('oct19','nov19','dec19','feb20','mar20','apr20','may20','jun20','jul20','aug20','sep20','oct20')
time2 <- seq(from = as.Date("2019-10-01"), to = as.Date("2020-10-03"), by = 'month')
time2 <- time2[-4]
time2

# team list 
team <- c( "Atlanta Hawks"  ,        "Boston Celtics"  ,       "Brooklyn Nets"   ,       "Charlotte Hornets"   ,   "Chicago Bulls"    ,      "Cleveland Cavaliers" ,  
            "Dallas Mavericks"  ,     "Denver Nuggets"   ,      "Detroit Pistons"  ,      "Golden State Warriors" , "Houston Rockets"  ,      "Indiana Pacers"     ,   
             "LA Clippers"        ,    "LA Lakers"          ,    "Memphis Grizzlies"   ,   "Miami Heat"        ,     "Milwaukee Bucks"  ,      "Minnesota Timberwolves",
             "New Orleans Pelicans",   "New York Knicks"   ,     "Oklahoma City Thunder" , "Orlando Magic"    ,      "Philadelphia Sixers" ,   "Phoenix Suns" ,         
             "Portland Trail Blazers", "Sacramento Kings"  ,     "San Antonio Spurs"  ,    "Toronto Raptors"  ,      "Utah Jazz"     ,         "Washington Wizards" ) 


# division list 
atlantic <- c("Boston Celtics", "Brooklyn Nets","New York Knicks", "Philadelphia Sixers","Toronto Raptors" )
central <- c("Chicago Bulls",  "Cleveland Cavaliers", "Detroit Pistons", "Indiana Pacers" , "Milwaukee Bucks" )
southeast <- c("Atlanta Hawks",  "Charlotte Hornets", "Miami Heat", "Orlando Magic","Washington Wizards" )
northwest <- c("Denver Nuggets", "Minnesota Timberwolves",  "Oklahoma City Thunder", "Portland Trail Blazers", "Utah Jazz" )
pacific <- c("Golden State Warriors" , "LA Clippers"  , "LA Lakers" , "Sacramento Kings" ,"Phoenix Suns"  )
southwest <- c("Dallas Mavericks","Houston Rockets" ,  "Memphis Grizzlies" ,"New Orleans Pelicans", "San Antonio Spurs")


##################### Checking data ###############
# set for loop to check the data dimension and columns name
for ( i in 1:length(total)){
  names <- names(total[[i]]) # check the columns name
  dim <- dim(total[[i]]) # check the dimension
  print(names)
  print(dim)
} # closing for loop 


########################## explain data ###################################
# number of data
#create empty data frame with 0 rows and 3 columns
data_size <- data.frame(matrix(ncol = 2, nrow = length(total)))

#add column names
colnames(data_size) <- c('time','n_tweets')

# add value on time columns
data_size$time <- time2

# for loop to calculate the number of tweets by month
for (i in 1: length(total)){
  data_size$n_tweets[i] <- nrow(total[[i]]) / 1000
} # closing for loop

# check the size  of data
# total number of tweets
sum(data_size$n_tweets)
# average # of tweets 
sum(data_size$n_tweets) / 12

# making bar chart
ggplot(data_size, aes(x=time, y=n_tweets)) +
   geom_bar(stat="identity", fill='skyblue') +
   theme_gdocs() + 
   labs(x = "Month", y = "Tweets(thousand)") +
   ggtitle("number of tweets")



##########  the percentage of brands mentioned in twitter  #################
# making function to calculate the percentage of Nike mentioned
per_nike <- function(x){
  number <- grep("nike", x$text, ignore.case=TRUE) # count rows mentioned brand
  length(number) # check the length
  (length(number) / nrow(x)) * 100 # calculate percentage
} # closing the function

# making function to calculate the percentage of Adidas mentioned
per_adi <- function(x){
  number <- grep("adidas", x$text, ignore.case=TRUE) # count rows mentioned brand
  length(number) # check the length
  (length(number) / nrow(x)) * 100  # calculate percentage
} # closing the function

#create empty data frame with 12 rows and 3 columns
brand <- data.frame(matrix(ncol = 3, nrow = 12))
#provide column names
colnames(brand) <- c('time','nike', 'adi')
# add value on time columns
brand$time <- time2

# using per_nike and per_adi function to calculate the percentage mentioned
for (i in 1:12){
  brand$nike[i] <- per_nike(total[[i]]) 
  brand$adi[i] <- per_adi(total[[i]]) 
} # closing for loop

# check the result 
summary(brand)

# choosing brand color
brand_color <- c("nike" = "orange", "adidas" = "blue")

# make a line plot (% of brands mentioned)
ggplot(brand, aes(x= time)) + 
  geom_line(aes(y = nike, color = 'nike'), size = 1) + 
  geom_line(aes(y = adi, color="adidas"), size = 1) +
  labs(x = "Month",  y = "(%)",  color = "Legend") +
  scale_color_manual(values = brand_color) +
  ggtitle("% of brands mentioned")


#################  popularity of divisions and teams ########################

################ which team is the most often mentioned. ###################
# install library 
library(plyr)

# make empty data frame with 30 rows and 12 columns
fre_team <- data.frame(matrix(ncol = length(total)  , nrow = 30 ))
#provide column names
colnames(fre_team) <- time2
# add value on time columns,
fre_team$team <- team

# for loop to count the number of tweet per team 
for ( i in 1: length(total)){
  fre_team[ ,i] <- count(total[[i]], "team")[ ,2]
  
} # closing for loop 

# make mean column for average tweets by teams
fre_team$mean <- rowMeans(fre_team[ ,1:12], na.rm=TRUE)

# make bar chart (average # of tweets by team)
ggplot(fre_team, aes(x = reorder(team, mean), y = mean,)) + 
  geom_bar(stat = "identity", fill = 'skyblue') + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none") +
  labs(x = "Team",  y = "Frequency",  color = "Legend",size = 0.5 ) +
  ggtitle("Average of tweet-Team") 

######################## division barplot ####################################
# for loop to make division column 
for (i in 1:nrow(fre_team)){
  if (fre_team$team[i] %in% atlantic ){
fre_team$division[i] <- "atlantic"
  } else if (fre_team$team[i] %in% central){
    fre_team$division[i] <- "central"
  } else if (fre_team$team[i] %in% southeast){
    fre_team$division[i] <- "southeast"
  } else if (fre_team$team[i] %in% northwest){
    fre_team$division[i] <- "northwest"
  }else if (fre_team$team[i] %in% pacific){
    fre_team$division[i] <- "pacific"
  } else if (fre_team$team[i] %in% southwest){
    fre_team$division[i] <- "southwest"
}
} # closing for loop


# make a line plot (number of division mentioned)
ggplot(fre_team, aes(x = reorder(division, mean/5), y = mean / 5, fill = division )) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none") +
  labs(x = "Division",  y = "Frequency") +
  ggtitle("Average # of tweets by Division") 




####################################### player  ##############################

#create empty list
dt_fre_pl <- vector(mode='list', length=length(total))


# for loop to clean all data 
for (i in 1: length(total)){
  dt_fre_pl[[i]] <- basicSubs(total[[i]]$text)
} # closing for loop
 
#create empty data frame with 12 rows and 3 columns
pl <- data.frame(matrix(ncol = 7, nrow = length(total)))
#provide column names
player_name <-c('lebron','giannis', 'kawhi','lillard','harden','curry','jordan')
colnames(pl) <- player_name

# make player summary that name mentioned (except james to aviod duplicate)
for (i in 1:length(total)){
  pl[i,1]  <- sum(stri_count(dt_fre_pl[[i]], regex ='lebron|james'))
  pl[i,2]  <- sum(stri_count(dt_fre_pl[[i]], regex ='giannis|antetokounmpo'))
  pl[i,3]  <- sum(stri_count(dt_fre_pl[[i]], regex ='kawhi|leonard'))
  pl[i,4]  <- sum(stri_count(dt_fre_pl[[i]], regex ='damian|lillard'))
  pl[i,5]  <- sum(stri_count(dt_fre_pl[[i]], regex ='james|harden'))
  pl[i,6]  <- sum(stri_count(dt_fre_pl[[i]], regex ='steph|curry'))
  pl[i,7]  <- sum(stri_count(dt_fre_pl[[i]], regex ='micheal|jordan'))
}


# average per player
lebron   <- mean(pl$lebron)
giannis  <- mean(pl$giannis)
kawhi    <- mean(pl$kawhi)
lillard  <- mean(pl$lillard)
harden   <- mean(pl$harden)
curry    <- mean(pl$curry)
jordan   <- mean(pl$jordan)

# Organize term objects into a data frame
player_Freq <- data.frame(terms = player_name,
                       freq  = c(lebron,giannis,kawhi,lillard,harden,curry,jordan))


# bar plot with average mentioned by player
ggplot(player_Freq, aes(x = reorder(terms, freq), y = freq, fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")+
  labs(x = "Player",  y = "Frequency",) +
  ggtitle("Average of tweets by player")


# set colors per player
pl_color <- c('lebron' = "navy",
              'giannis' = "green", 
              'kawhi' = "purple",
              'lillard' = "sky blue",
              'harden' = "orange",
              'curry' = "blue",
              'jordan' = "red")



# add value on time columns
pl$time <- time2

# make a line plot (number of players mentioned)
ggplot(pl, aes(x= time)) + 
  geom_line(aes(y = lebron, color = "lebron"), size = 1) + 
  geom_line(aes(y = giannis, color = "giannis"), size = 1) +
  geom_line(aes(y = kawhi, color = "kawhi"), size = 1) +
  geom_line(aes(y = lillard, color = "lillard"), size = 1) +
  geom_line(aes(y = harden, color = "harden"), size = 1) +
  geom_line(aes(y = curry, color = "curry"), size = 1) +
  geom_line(aes(y = jordan, color = "jordan"), size = 1) +
  labs(x = "Month",  y = "Frequency",  color = "Legend") +
  scale_color_manual(values = pl_color) +
  ggtitle("Number of tweets by player")



###################### Monthly keywords ################################
# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "),
         use.names = FALSE)
}

# setting sample size 
sample_size <- 0.01
# make empty index and sample list 
sample_idx <- c()
total_sam <- list()

#  for loop to make sample data 
for (i in 1:length(total)){
  sample_idx <- sample(1:nrow(total[[i]]), size = nrow(total[[i]]) * sample_size)
  total_sam[[i]] <-  total[[i]][sample_idx, ]
} # closing for loop

# Create some stopwords using the 'SMART' lexicon and add team name 
stops_2 <- c(stopwords('SMART'),'nba','basketball','boston','celtics',
             'brooklyn','nets',
             'newyork','knicks',
             'philadelphia','76ers',
             'toronto','raptors','chicago','bulls',
             'cleveland','cavaliers',
             'detroit','pistons',
             'indiana','pacers',
             'milwaukee','bucks','atlanta','hawks',
             'charlotte','hornets',
             'miami','heat',
             'orlando','magic',
             'washington','wizards','denver','nuggets',
             'minnesota','timberwolves',
             'oklahoma city','oklahoma','thunder',
             'portland','trail blazers', 'blazers',
             'utah','jazz','golden state','warriors',
             'los','clippers',
             'los angeles','lakers',
             'phoenix','suns',
             'scarmento','kings','dallas','mavericks',
             'houston','rockets',
             'memphis','grizzles',
             'new orleans','pelicans',
             'san antonio','spurs')


# make wordcloudmaker function
wordcloudmaker <- function(x){
  txtCorpus <- VCorpus(VectorSource(x$text))
    # Preprocess the corpus
  txtCorpus <- cleanCorpus(txtCorpus, stops_2)
    # Make bi-gram TDM according to the tokenize control & convert it to matrix
  wineTDM  <- TermDocumentMatrix(txtCorpus, 
                                 control=list(tokenize=bigramTokens))
  wineTDMm <- as.matrix(wineTDM)
  wineTDMv <- sort(rowSums(wineTDMm), decreasing = TRUE)
  wineDF   <- data.frame(word = names(wineTDMv), freq = wineTDMv)
  
 } # closing function

# make an enmpy list 
list_c <- list()
# for loop to use wordcloudmaker function with sample data
for (i in 1:length(total_sam)){
 list_c[[i]] <-  wordcloudmaker(total_sam[[i]]) 
 }

# check the color pallete
display.brewer.all()
# set the color 
keyword_color <- c("Purples", "Blues","Greens","Oranges","Purples", "Blues","Greens","Oranges","Purples", "Blues","Greens","Oranges")

# for loop to make a wordcloud 
for (i in 1:length(list_c)){
  pal <- brewer.pal(8, keyword_color[i])
  pal <- pal[-(1:2)]
  
  set.seed(1234)
  wordcloud(list_c[[i]]$word,
            list_c[[i]]$freq,
            max.words    = 50,
            random.order = FALSE,
            colors       = pal,
            scale        = c(2,1))
  
}

## End 