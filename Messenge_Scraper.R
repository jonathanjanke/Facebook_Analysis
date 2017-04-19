library(rvest)
library(ggplot2)
library(reshape2)

#for wordcloud:
library(tm)
library(SnowballC)
library(wordcloud)
# endfor wordcloud---

# parameters
par_DocURL <- "C://Users//jonat//OneDrive for Business//Dokumente//Projekte//Facebook Analysis//data//html//messages.htm"
# body > div.contents > div > div:nth-child(1)
par_xpath <- "/html/body/div[2]/div[1]"

## can obviously be automatically generated
par_own_name <- "Jonathan Janke"
par_own_id <- "100006439157107"
par_other_names <- list()

# scraping
message_html <- read_html(par_DocURL)
message_data <- message_html%>%html_nodes(xpath = '/html/body/div[2]/div[1]')%>%html_children()

user <- message_data%>%html_node('span.user')%>%html_text()
date <- message_data%>%html_node('span.meta')%>%html_text()

user_messages <- sapply(message_data,
             function(x) unique(x%>%html_children()%>%html_node('span.user')%>%html_text()))
# clean up user data
user_messages <- sapply(user_messages, function(x) x <<- x[!is.na(x)])
user_messages <- sapply(user_messages, function(x) x <- gsub("@facebook.com","", x))


temp.filter <- sapply(user_messages, function(x) length(x)>1)
user_messages_filtered <- user_messages[temp.filter]

temp_remove <- c(par_own_id, par_own_name)

temp_user_number_map = matrix(temp_remove, nrow = 1)
user_number_map <- filter_users(user_messages_filtered, temp_remove)
while (FALSE %in% sapply (user_number_map, function (x) x<- is.null(x))) {
  user_number_map <- filter_users(user_messages_filtered, temp_remove)
  temp_user_number_map <- rbind (temp_user_number_map, get_user_list (user_number_map))
  temp_remove <- as.vector(temp_user_number_map)
  user_number_map <- filter_users(user_messages_filtered, temp_remove)
}

# clean user information
test_message_users <- replace_number_by_user(user_messages_filtered, temp_user_number_map)

messages <- list()
for (i in 1: length(message_data)) {
  messages[[i]] <- conversation_info <- get_conversation_info(message_data[i], temp_user_number_map)
}

user_message_counts <- get_user_counts (messages)

#example plot for one conversation
user_message_count_i <- get_word_count(messages[[333]])
unpivot_user_message_count_i <- melt(user_message_count_i)
colnames(unpivot_user_message_count_i) <- c("user", "date", "count")
unpivot_user_message_count_i [, "date"] <- as.Date(as.POSIXct(unpivot_user_message_count_i [, "date"], origin="1970-01-01"))
con_plot <- ggplot(data = unpivot_user_message_count_i, aes(x=date, y= count, fill=user))
con_plot + geom_bar(stat="identity", position=position_dodge())+ scale_x_date(date_breaks="1 month") + geom_line()

user_message_count_i <- get_user_stat(messages[[333]])
unpivot_user_message_count_i <- melt(user_message_count_i)
unpivot_user_message_count_i
colnames(unpivot_user_message_count_i) <- c("user", "date", "count")
unpivot_user_message_count_i [, "date"] <- as.Date(as.POSIXct(unpivot_user_message_count_i [, "date"], origin="1970-01-01"))
con_plot <- ggplot(data = unpivot_user_message_count_i, aes(x=date, y= count, fill=user))
con_plot + geom_bar(stat="identity", position=position_dodge())+ scale_x_date(date_breaks="1 month")

user_u_word_count_i <- get_unique_word_count(messages[[333]])

#create_word_cloud <- function (message_i) {
  message_i <- messages[[333]]
  all_words <- ""
  for (i in 1: length(message_i[, "message_content"])) {
    all_words <- paste(all_words, message_i[i, "message_content"])
  }
  all_words <- iconv(all_words, "latin1", "ASCII", sub="")
  
  jeopCorpus <- Corpus(VectorSource(all_words))
  jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
  # remove punctuation
  jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
  #remove stopwords
  jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
  jeopCorpus <- tm_map(jeopCorpus, removeWords, length(x) < 3)
  #stemming
  jeopCorpus <- tm_map(jeopCorpus, stemDocument)
  #wordcloud
  wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)
  #return (?) 
#}

get_user_counts <- function (messages) {
  stats <- list()
  for (i in 1:length(messages)) {
    stats [[i]] <- get_user_count(messages[[i]])
  }
  return (stats)
}
  
get_user_count <- function (message_i) {

  message_i[, "message_date"] <- sapply (message_i[, "message_date"], function (x) (floor(as.numeric(x)/(60*60*24*7)))*(24*60*60*7))
  user <- unique (message_i[, "message_writer"])
  dates <- unique (message_i[, "message_date"])
  message_stats <- matrix(0, nrow=length(user), ncol=length(dates), dimnames=list(user,dates))
  for (i in 1: length(message_i[, "message_content"])) {
    message_stats[message_i[i, "message_writer"], message_i[i, "message_date"]] <- message_stats[message_i[i, "message_writer"], message_i[i, "message_date"]]  + 1
  }
  
  return (message_stats)
}

get_word_count <- function (message_i) {
  message_i[, "message_date"] <- sapply (message_i[, "message_date"], function (x) (floor(as.numeric(x)/(60*60*24*7)))*(24*60*60*7))
  user <- unique (message_i[, "message_writer"])
  dates <- unique (message_i[, "message_date"])
  message_stats <- matrix(0, nrow=length(user), ncol=length(dates), dimnames=list(user,dates))
  for (i in 1: length(message_i[, "message_content"])) {
    k<- strsplit(message_i[i, "message_content"], " ")
    l <- length(k$message_content)
    message_stats[message_i[i, "message_writer"], message_i[i, "message_date"]] <- message_stats[message_i[i, "message_writer"], message_i[i, "message_date"]]  + l
  }
  
  return (message_stats)
}

get_unique_word_count <- function (message_i) {
  # message_i[, "message_date"] <- sapply (message_i[, "message_date"], function (x) (floor(as.numeric(x)/(60*60*24*7)))*(24*60*60*7))
  user <- unique (message_i[, "message_writer"])
  # dates <- unique (message_i[, "message_date"])
  message_stats <- matrix(nrow=1, ncol=length(user), dimnames=list(NULL,user))
  all_words <- ""
  for (i in 1: length(message_i[, "message_content"])) {
    message_stats[1, message_i[i, "message_writer"]] <- paste(message_stats[1, message_i[i, "message_writer"]], message_i[i, "message_content"], sep=" ")
    all_words <- paste(all_words, message_i[i, "message_content"])
  }
  for (i in 1: ncol(message_stats)) {
    k<- as.vector(strsplit(message_stats[1, i], " "))
    f <- colnames(message_stats)[[i]]
    l <- length(unique(k[[f]]))
    message_stats[1,i] <- l
  }
  k<- as.vector(strsplit(all_words, " "))
  total_words <- length(unique(k[[1]]))
  total <- data.frame(total_words)
  message_stats <- cbind(message_stats, total)
  message_stats
  return (message_stats)
}

filter_users <- function (user_messages_filtered, temp_remove) {
  user_number_map <- sapply(user_messages_filtered, function (x) {
    x <- x[! x %in% temp_remove]
    if (length(x) == 2) {
      if (sum(!is.na(as.numeric(x)))==1) {
        x
      }
    }
  }
  )
}

get_user_list <- function (user_number_map) {
  temp_user_number_map <- user_number_map
  temp_user_number_map <- temp_user_number_map[!sapply(temp_user_number_map,is.null)]
  temp_user_number_map <- sapply(temp_user_number_map, sort)
  temp_user_number_map <- t(temp_user_number_map)
}

replace_number_by_user <- function (message_data, user_number_map) {
  for (i_message in 1:length(message_data)) {
    for (i_participant in 1: length(message_data[[i_message]])) {
      participant <- message_data[[i_message]][[i_participant]]
      if (!is.na(as.numeric(participant))) {
        # replace number with corresponding name
        ## check if number is contained in temp_user_number_map
        participant_index <- which(user_number_map[,1]==participant, arr.ind=T)
        if (length (participant_index)>0) {
          participant_name <- user_number_map[participant_index,2]
          message_data[[i_message]][[i_participant]] <- participant_name
        }
      }
    }
    message_data[[i_message]] <- unique(message_data[[i_message]])
  }
  return (message_data)
}

get_conversation_info <- function (message_data, number_user_map) {
  message_writer <- get_message_writer(message_data, number_user_map)
  message_content <- get_message_content(message_data, number_user_map)
  message_date <- get_message_dates (message_data, number_user_map)
  
  return (cbind(message_writer, message_date, message_content))
}

get_message_writer <- function (message_data, number_user_map) {
  return(clean_user_names(message_data%>%html_nodes("div")%>%html_nodes("span.user")%>%html_text(), number_user_map))
}

get_message_content <- function (message_data, number_user_map) {
  return(clean_text(message_data%>%html_nodes("p")%>%html_text()))
}

get_message_dates <- function (message_data, number_user_map) {
  return(clean_dates(message_data%>% html_nodes("div")%>%html_nodes("span.meta")%>%html_text()))
}

get_conversation_user <- function (message_data, number_user_map) {
  return (unique(get_conversation_info(message_data, number_user_map)[,1]))
}

clean_user_names <- function (writer, number_user_map) {
  writer <- gsub("@facebook.com","", writer)
  writer <- replace_number_by_user(writer, number_user_map)
  writer <- rev(writer)
  return (writer)
}

clean_dates <- function (date) {
  date <- date[!is.na(date)]
  date <- as.POSIXct(date, tz="Europe/London" ,format="%A, %B %d, %Y at %I:%M%p")
  date <- rev(date)
  return (date)
}

clean_text <- function (text) {
  text[text==""] <- "[img]"
  #text <- text[seq(2, length(text), 2)]
  rev(text)
}
