pagerank = function(log_df, threshold = 30) {
  
  # get necessary columns from log_df
  c <- c('enrollment_id', 'object', 'time')
  dt <- data.table(log_df[,c], key = "enrollment_id")
  
  # rank each record for each enrollment
  dt <- dt[,transform(.SD, order = rank(time, ties.method = "first")), by = enrollment_id]
  dt2 <- as.data.frame(dt)
  
  # create order2 for self join
  dt$order2 <- dt$order - 1
  dt <- as.data.frame(dt)
  
  # self join
  dt %<>%
    select(enrollment_id, object, time, order2) %>%
    inner_join(dt, ., by = c("enrollment_id" = "enrollment_id", "order" = "order2"))
  
  # clean records and create spent feature
  dt <- dt[,c( 'enrollment_id', 'object.x', 'object.y', 'time.x', 'time.y', 'order', 'order2')]
  dt %<>% mutate(time.y = ymd_hms(gsub("T", " ", time.y)),
                 time.x = ymd_hms(gsub("T", " ", time.x)),
                 spent = as.numeric(time.y - time.x),
                 order2 = order2+2)
  
  # function to define each session
  sessions = function(dat, thresh) {
    n <- nrow(dat)
    x <- 1
    s <- rep(0, n)
    t <- dat$spent
    for (i in 1:n) {
      x <- ifelse(t[i] < thresh, x, x+1)
      s[i] <- x
    }
    dat$session <- s
    return(dat)
  }
  enrolls <- unique(dt$enrollment_id)
  
  registerDoMC(cores=35)
  dt <- foreach(i = enrolls) %dopar% sessions(filter(dt, enrollment_id == i), 60*threshold)
  dt <- rbind.fill(dt)
  
  detach("package:lubridate", unload=TRUE)
  detach("package:ggplot2", unload=TRUE)
  detach("package:plyr", unload=TRUE)
  library(dplyr)
  
  dt2 %<>% left_join(., dt, by = c('enrollment_id' = 'enrollment_id', 'object' = 'object.y', 'time' = 'time.y', 'order' = 'order2')) %>%
    select(enrollment_id, object, time, order, session) %>%
    left_join(., dt, by = c('enrollment_id' = 'enrollment_id', 'object' = 'object.x', 'time' = 'time.x', 'order' = 'order')) %>%
    mutate(session = ifelse(is.na(session.x), session.y, session.x)) %>%
    select(enrollment_id, object, time, order, session, spent)
  dt2 %<>% group_by(enrollment_id, session) %>%
    summarise(order = max(order),
              last = 'Y') %>%
    left_join(dt2, ., by = c('enrollment_id' = 'enrollment_id', 'session' = 'session', 'order' = 'order'))
  dt2$last[is.na(dt2$last)] <- 'N'
  dt2 %<>% mutate(esess = paste(enrollment_id, session, sep = '_'))
  
  dt2 <- data.table(dt2, key = 'esess')
  dt2 <- dt2[,transform(.SD, session_order = rank(time, ties.method = "first")), by = esess]
  dt2 %<>% mutate(session_order2 = session_order - 1)
  dt2 <- as.data.frame(dt2)
  dt2 %<>%
    select(esess, session_order2, object) %>%
    left_join(dt2, ., by = c("esess" = "esess", "session_order" = "session_order2")) %>%
    select(object.x, object.y, enrollment_id, session, time, spent, last, session_order, order)
  
  links <- dt2 %>%
    filter(!is.na(object.y)) %>%
    select(object.x, object.y) %>%
    graph.data.frame(.) %>%
    page.rank(.)
  dt2 %>%
    filter(!is.na(object.y)) %>%
    select(object.x, object.y) %>%
    graph.data.frame(.) %>% plot(.)
  links <- data.frame(object.x = names(links$vector), pagerank = links$vector)
  rownames(links) <- 1:nrow(links)
  dt2 %<>% left_join(., links)
  
  return(dt2)
  
}