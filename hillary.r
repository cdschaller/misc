## load libraries, data, etc ----
library(tm)
library(dplyr)
library(magrittr)
library(lubridate)
library(igraph)
emails <- read.csv("/home/a630491/hclinton/Emails.csv", stringsAsFactors = FALSE)
persons <- read.csv("/home/a630491/hclinton/Persons.csv", stringsAsFactors = FALSE)
receivers <- read.csv("/home/a630491/hclinton/EmailReceivers.csv")

## cleaning ----

clean_text = function(col) {
  col <- tolower(col)
  col <- gsub('^h: | sid$', '', col)
  col <- gsub('[[:punct:][:space:]]+', ' ', col)
  col <- gsub('^ | $', '', col)
  return(col)
}

# rename cols for persons col
persons %<>% select(personid = Id, name = Name)
persons$name[grepl('millscd', persons$name)] <- 'Cheryl Mills'
persons$name[grepl('Phillip Crowley', persons$name)] <- 'Philip Crowley'
persons$name[grepl('sullivan', persons$name)] <- 'Jake Sullivan'
persons$name[grepl('abedinh', persons$name)] <- 'Huma Abedin'
persons$name[grepl('jilotylc', persons$name)] <- 'Lauren Jiloty'
persons$name[grepl('valmorou', persons$name)] <- 'Lona Valmoro'

# aggregate receivers to email level, attach names
receivers %<>%
  select(emailid = EmailId,
         personid = PersonId) %>%
  left_join(., persons)

em_receivers <- receivers %>%
  group_by(emailid) %>%
  summarise(num_sent_to = n(),
            receiver_names = paste(name, collapse = ', '),
            receiver_ids = paste(personid, collapse = ', '))

# lowercase emails colnames
colnames(emails) <- tolower(colnames(emails))

# select/clean cols in emails, attach to/from names
email_text <- {emails %>%
  mutate(body_text = clean_text(extractedbodytext),
         raw_text = clean_text(rawtext)) %>%
  select(emailid = id, body_text, raw_text)}
emails %<>% {mutate(md_datesent = as.Date(metadatadatesent),
         md_datereleased = as.Date(metadatadatereleased),
         ex_datereleased = as.Date(extracteddatereleased, '%m/%d/%Y'),
         release_type = gsub('^.*IN ', '', extractedreleaseinpartorfull),
         subject = clean_text(metadatasubject)) %>%
  select(emailid = id,
         subject,
         personid = senderpersonid,
         md_datesent,
         md_datereleased,
         ex_datesent = extracteddatesent,
         ex_datereleased,
         release_type) %>%
  left_join(., persons) %>%
  select(emailid:release_type, sender_name = name) %>%
  left_join(., em_receivers)}
gc()

# create wordcloud of given field ----
cust_stop <- {c('call', 'schedule', 'update', 'fyi', 'talk', 'today', 'tomorrow',
                'new', 'can', 'calls', 'meeting', 'draft', 'memo', 'just', 'list',
                'office', 'will', 'latest', 'letter', 'news', 'talks', 'monday',
                'friday', 'autoreply', 'called', 'question', 'says', '', 'sid',
                'unclassified', 'state', 'case', 'sent', 'department', 'doc',
                'date', 'subject', 'message', 'com', 'time', 'one', 'said',
                'gov', 'also', 'may', 'part', 'know', 'say', 'like', 'see', 'now',
                'since', 'group', 'another', 'make', 'told', 'let', ';', 'well',
                'back', 'year', 'years', 'going', 'even', 'much')
}
wc = function(col, n, cust_stop = cust_stop) {
  col <- tolower(col)
  words <- strsplit(col, '[[:digit:][:punct:][:space:]]+')
  words <- unlist(words)
  words <- words[!(words %in% c(stopwords('en'), cust_stop))]
  words <- words[nchar(words) > 2]
  words <- table(words)
  words <- data.frame(words)
  words <- words[order(words$Freq, decreasing = TRUE),]
  cloud <- wordcloud(words$words, words$Freq, scale = c(4.5,0.2),
                     max.words = n, random.order = FALSE,
                     rot.per = 0.35, use.r.layout = FALSE,
                     colors = brewer.pal(8, 'Dark2'))
  return(cloud)
}

# sentiment analysis ----
afinn <- read.delim('~/misc/nlp/AFINN-111.txt', head = FALSE, sep = '\t')
email_text$sentiment <-  unlist(lapply(
  strsplit(x = email_text$body_text, split = '[[:space:]]+'),
  function(x) {
    m <- afinn$V2[match(x, afinn$V1)]
    s <- sum(m, na.rm = TRUE)
    return(s)}))

# hillary clinton email network ----
N <- 100
tofrom <- emails %>%
  select(emailid, senderid = personid, sender_name) %>%
  full_join(., receivers) %>%
  select(emailid, sender_name, receiver_name = name) %>%
  filter(complete.cases(.),
         sender_name == 'Hillary Clinton' | receiver_name == 'Hillary Clinton') %>%
  left_join(., select(email_text, emailid, sentiment)) %>%
  group_by(sender_name, receiver_name) %>%
  summarise(cnt = n(),
            sentiment = mean(sentiment, na.rm = T))

hillarygraph = function(min = 10, direction,
                        title = 'Hillary Email Graph', ..., df = tofrom) {
  direction <- tolower(direction)
  if ( !(direction %in% c('to', 'from', 'both')) )
    stop('Invalid direction.')
  df %<>% filter(cnt >= min)
  if (direction != 'both')
    df %<>% filter(
      ifelse(direction == 'to', receiver_name, sender_name) == 'Hillary Clinton')
  gdf <- graph.data.frame(df)
  gdf$title <- title
  
  vcolors <- ifelse(V(gdf)$name == 'Hillary Clinton', 'blue',
                    ifelse(E(gdf)$sentiment < 0, 'red', 'dark green'))
  vsizes <- ifelse(V(gdf)$name == 'Hillary Clinton', 33, sqrt(E(gdf)$cnt)*1.75)
  V(gdf)$label.cex <- 0.8
  par(bg = 'dark grey')
  p <- plot(gdf, layout = layout.fruchterman.reingold,
            main = gdf$title,
            vertex.label = V(gdf)$name,
            vertex.size = vsizes,
            vertex.color = vcolors,
            vertex.frame.color = 'dark grey',
            vertex.label.color = 'white',
            vertex.label.family = 'sans',
            edge.width = 2,
            edge.color = 'white',
            edge.arrow.size = 0, ...)
  return(p)
}

# who curses the most ----

words <- strsplit(email_text$body_text, '[[:space:]]+')
curses <- lapply(words, function(x) x[x %in% cursewords])
keep <- unlist(lapply(curses, length)) > 0
curses <- unlist(lapply(curses[keep], function(x) paste(x, collapse = ', ')))
curses <- data.frame(emailid = emails$emailid[keep], name = emails$sender_name[keep], curses = curses)

# sentiment over time ----
sentiment_trend <- emails %>%
  mutate(sentiment = email_text$sentiment,
         yearmonth = as.Date(format(md_datesent, '%Y-%m-01'))) %>%
  filter(sender_name == 'Hillary Clinton') %>%
  group_by(yearmonth) %>%
  summarise(cnt = n(),
            total_sentiment = sum(sentiment),
            avg_sentiment = mean(sentiment))

# change in word usage over time ----
freq <- unlist(strsplit(email_text$body_text, '[[:space:]]+'))
freq <- table(freq)
freq <- freq[!(names(freq) %in% cust_stop) &
               !(names(freq) %in% stopwords('en')) &
               freq > 400 & nchar(names(freq)) > 3]
freq <- names(freq)