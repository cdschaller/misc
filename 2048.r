sim_2048 = function(limit = Inf, tree_depth, two_rate = 0.9, num_cores = 30) {
  # setup ----
  library(dplyr)
  library(magrittr)
  library(foreach)
  library(doMC)
  registerDoMC(cores = num_cores) # register cores
  two <- two_rate
  depth <- tree_depth
  directions <- c('right', 'up', 'left', 'down') # THIS ORDER MATTERS
  twofour <- 0:1
  position <- 1:16
  
  # functions ----
  # function to get all permutations for given depth
  make.perms = function(dir, tf, posits, depth) {
    
    dir <- data.frame(directions = dir)
    tf <- data.frame(twofour = tf)
    posits <- data.frame(position = posits)
    
    p <- merge(dir, tf, by = NULL, all = TRUE)
    p <- merge(p, posits, by = NULL, all = TRUE)
    p$directions <- as.character(p$directions)
    perms <- p
    if (depth == 1) {
      colnames(perms) <- paste0(colnames(perms), 1)
      return(perms)
    }
    for (i in 2:depth) {
      perms <- merge(perms, p, by = NULL, all = TRUE)
    }
    
    n <- sort(rep(1:depth, 3))
    c <- colnames(p)
    c <- paste0(rep(c, depth), n)
    colnames(perms) <- c
    gc()
    return(perms)
  }
  
  # function for adding new numbers
  # two = 1 will always return a 2
  # two = 0 will always return a 4
  # pos = NA will randomly select a position
  # pos = N will force the position
  new_num = function(g, two = two, pos = NA) {
    
    z <- g == 0 # flag zeros
    if( !any(z) ) # if no zeros, return
      return(g)
    empty <- which(z) # get positions of zeros
    
    # if position unspecified, randomly choose
    if (is.na(pos))
      pos <- ifelse(length(empty) == 1, empty, sample(empty, 1))
    
    if ( !z[pos] )
      stop('Position already populated.')
    
    num <- ((runif(1) > two) + 1)*2
    g[pos] <- num
    return(g)
  }
  
  # swiping function
  swipe = function(g, move) {
    # functions for swiping
    rotate = function(x, turns) {
      if (turns %% 4 == 0)
        return(x)
      if (turns == 3) {
        x <- t(apply(x, 2, rev))
        turns <- turns - 1
      }
      if (turns == 2)
        return(x[4:1, 4:1])
      if (turns == 1)
        x <- t(apply(x, 2, rev))
      return(x)
    }
    rowswipe = function(row) {
      row <- row[row != 0]
      l <- length(row)
      if ( l < 2 )
        return( c(rep(0, 4-l), row) )
      for (i in l:2) {
        if (row[i] == row[i-1]) {
          row[i] <- row[i]*2
          row <- row[-(i-1)]
          row <- c(0, row)
        }
      }
      return( c(rep(0, 4-length(row)), row) )
    }
    
    # move lookup
    turns <- match(move, directions) - 1
    
    # rotate to swipeable position
    g <- rotate(g, turns)
    
    # swipe all 4 rows
    newg <- lapply(1:4, function(x) rowswipe(g[x,]))
    g <- do.call('rbind', newg)
    
    # turn back to original position
    g <- rotate(g, 4-turns)
    return(g)
  }
  
  # function to calculate a score for the grid
  score = function(g, m) {
    
    # max of grid
    sg <- sort(g, decreasing = TRUE)
    mx <- sg[1]
    mx2 <- sg[2]
    mx3 <- sg[3]
    
    # plus for max in corner
    corner <- mx %in% g[c(1, 4), c(1, 4)]
    
    # big #s in middle
    mid <- g[c(2, 3), c(2, 3)]
    middle <- mx %in% mid
    middle2 <- mx2 %in% mid
    middle3 <- mx3 %in% mid
    
    # top 2 in corners
    c1 <- g[c(1, 2, 5)]
    c2 <- g[c(3, 4, 8)]
    c3 <- g[c(9, 13, 14)]
    c4 <- g[c(12, 15, 16)]
    topcorner <-
      (mx %in% c1 & mx2 %in% c1) |
      (mx %in% c1 & mx2 %in% c1) |
      (mx %in% c1 & mx2 %in% c1) |
      (mx %in% c1 & mx2 %in% c1)
    
    # plus for every zero
    zeros <- sum(g == 0)
    
    # minus for every non monotonic row/column
    monotone = function(seq) {
      if (sum(seq) > 0) {
        seq <- seq[seq > 0]
        monotone <- !identical(sort(seq, decreasing = TRUE), seq) & !identical(sort(seq), seq)
      } else {monotone <- FALSE}
      return(monotone)
    }
    nonmons <- sum(apply(g, 2, monotone) + apply(g, 1, monotone))
    
    # weight and add
    score <-
      corner*400 +
      zeros*30 -
      nonmons*30 -
      !corner*200 -
      middle*150 -
      middle2*125 -
      middle3*100 +
      topcorner*250 -
      !topcorner*100
    return(score)
  }
  
  # function to run each combination
  run = function(g, v, d) {
    
    # if terminal node reached, score g
    if (d == 0)
      return(score(g))
    
    vv <- v[1:3] # grab the set for this iteration
    vv1 <- as.character(vv[1])
    vv2 <- as.integer(vv[2])
    vv3 <- as.integer(vv[3])
    if (identical(swipe(g, vv[1]), g))
      return(0) # path is invalid, return 0
    
    g <- swipe(g, vv[1])
    
    if (g[vv3] != 0)
      return(0) # path is invalid, return 0
    
    g <- new_num(g, vv2, vv3)
    
    # check if g is playable
    x <- sum(sapply(directions, function(x) identical(swipe(g, x), g)))
    if ( x == 4 )
      return(-99999999) # results in game over
    
    # keep iterating
    run(g, v[-(1:3)], d - 1)
  }
  
  # function to choose move
  choose = function(c, g = names(p), d = depth) {
    if ( d == 0 ) # reached terminal node
      return(as.character(c$directions1))
    
    dd <- d*3-2   # select columns
    gg <- g[1:dd] # to group by
    
    # control for likelihood of 4 vs 2
    tfcol <- which(g == paste0('twofour', d))
    c$score <- c$score * abs(unlist(c[,tfcol]) + two - 1)
    
    # get expected value of each move at each node
    c1 <- c %>%
      group_by_(.dots = gg) %>%
      summarise(score = mean(score))
    
    # choose best move at each node
    if (ncol(c1) > 2) {
      c2 <- c1 %>%
        group_by_(.dots = gg[-dd]) %>%
        summarise(score = max(score)) %>%
        invisible(inner_join(., c1))
    } else {
      c2 <- c1[c1$score == max(c1$score),]
    }
    
    # reiterate
    choose(c2, gg, d-1)
  }
  
  ## ----
  
  # get all possible permutations
  p <- make.perms(directions, twofour, position, depth)
  
  # create empty grid
  grid <- matrix(0, 4, 4)
  # create opening grid
  grid <- new_num(new_num(grid, two), two)
  
  # determine possible moves
  canmove <- sapply(directions, function(x) ifelse(identical(grid, swipe(grid, x)), 'foo', x))
  continue <- sum(canmove == 'foo') != 4
  N <- 1
  
  while (continue & N <= limit) {
    print(grid)
    
    # filter out illegal move scenarios
    combos <- filter(p, directions1 %in% canmove)
    
    # for each legal move, filter out impossible placement positions
    canmove <- canmove[canmove != 'foo']
    for (j in canmove) {
      tgrid <- swipe(grid, j)
      cantplace <- which(tgrid != 0)
      combos <- filter(combos, !(directions1 == j & position1 %in% cantplace))
    }
    
    # get scores for each permutation in `combos`
    registerDoMC(cores = num_cores) # register cores
    s <- foreach(i = iter(combos, by = 'row')) %dopar% run(grid, i, depth)
    s <- unlist(s)
    combos$score <- s
    
    # remove impossible scenarios
    combos %<>% filter(score != 0)
    
    # choose best move
    move <- choose(combos, names(p), depth)
    move <- sample(move, 1)
    
    # execute moves
    grid <- swipe(grid, move)
    grid <- new_num(grid, two)
    
    # determine possible moves
    canmove <- sapply(directions, function(x) ifelse(identical(grid, swipe(grid, x)), 'foo', x))
    # game over?
    continue <- sum(canmove == 'foo') != 4
    
    N <- N + 1
    
  }
  ## ----
  return(grid)
}
