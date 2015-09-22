encrypt = function(message, seed = NA) {
  characters <- t(t(c("A", "a", "B", "b", "C", "c",
                      "D", "d", "E", "e", "F", "f",
                      "G", "g", "H", "h", "I", "i",
                      "J", "j", "K", "k", "L", "l",
                      "M", "m", "N", "n", "O", "o",
                      "P", "p", "Q", "q", "R", "r", "S", "s",
                      "T", "t", "U", "u", "V", "v",
                      "W", "w", "X", "x", "Y", "y",
                      "Z", "z", "1", "2", "3", "4",
                      "5", "6", "7", "8", "9", "0",
                      "!", "@", "#", "$", "%", "^",
                      "&", "*", "(", ")", " ", "-", "?", ".")))
  throwins <- t(t(c("A", "a", "B", "b", "C", "c",
                    "D", "d", "E", "e", "F", "f",
                    "G", "g", "H", "h", "I", "i",
                    "J", "j", "K", "k", "L", "l",
                    "M", "m", "N", "n", "O", "o",
                    "P", "p", "Q", "q", "R", "r", "S", "s",
                    "T", "t", "U", "u", "V", "v",
                    "W", "w", "X", "x", "Y", "y",
                    "Z", "z", "!", "@", "#",
                    "%", " ")))
	rownames(throwins) <- throwins
	numchars <- length(throwins)
	
	m <- data.frame(strsplit(message, ""))
	m <- data.frame(m, rep(0, length(m)))
	colnames(m) <- c("char", "code")
	
	l <- dim(m)[1]
	ll <- l*1000
	
	r <- matrix(0, ll, 2)
	r[,1] <- 1:ll
	
	seed <- ifelse(is.na(seed), round(runif(1, 1, 1000000), digits = 0), seed)
	
	set.seed(seed)
	
	r[,2] <- round(runif(ll, 1, length(characters)), digits = 0)
	r[,2] <- characters[r[,2]]
	
	set.seed(seed)
	delimiters <- sample(throwins, numchars/2, replace = FALSE)
	mixers <- throwins[-match(delimiters, throwins)]
	r <- data.frame(r)
  m <- data.frame(m)
  seeds <- rep(seed:(seed+l-1), l)
  
	for (i in 1:l) {
		a <- r[which(as.character(r[,2]) == as.character(m[i,1])),]
		set.seed(seeds[i])
    bb <- as.character(sample(a[,1],1))
    set.seed(seeds[i])
		m[i,2] <- paste(bb, sample(delimiters, 1, replace = TRUE), sep = "")
    r <- r[-as.numeric(bb),]
	}
  
	encrypt <- paste(m[,2], collapse="")
  encryptl <- nchar(encrypt)
	encrypt <- t(t(data.frame(strsplit(encrypt, ""))))
  
	rands <- 1:encryptl+l
	set.seed(seed)
	rands <- sample(rands, length(rands)/2, replace = FALSE)
	rands <- sort(rands, decreasing = FALSE)
	rands <- data.frame(rands, rands)
	set.seed(seed)
	rands[,2] <- sample(mixers, dim(rands)[1], replace = TRUE)
  encryptl <- dim(rands)[1] + encryptl
  
  en <- 1:encryptl
  en <- en[-rands[,1]]
  encrypt <- data.frame(en, encrypt)
  rands <- data.frame(rands)
  colnames(encrypt) <- c("a", "b")
	colnames(rands) <- c("a", "b")
  new <- rbind(rands, encrypt)
  encryption <- matrix(0, encryptl, 2)
  encryption[,1] <- 1:encryptl
  
  for (i in 1:encryptl) {
    encryption[i,2] <- new[which(new[,1] == i), 2]
  }
  
  encryption <- paste(encryption[,2], collapse = "")
  
  set.seed(seed)
  encryption <- paste(encryption, sample(delimiters, 1, replace = FALSE), as.character(seed), sep = "")
  
  set.seed(1)
  encryption <- paste(as.character(l), sample(delimiters, 1, replace = FALSE), encryption, sep = "")
  set.seed(NULL)
	return(encryption)
}
decrypt = function(encryption) {
  cat('Who are you?')
  response <- readline()
  if (encrypt(tolower(response), 630491) != '15q9950T5294Q4476QRftR13366X1m4i143v102bp8up@24CLgLlC62s29pj@76#mFYmPY1COp@pF1342mRpEb57@0U5WW3G09B5676w4271q11618E7323KT630491') {
    stop('Access denied.')
  }
  cat('Access granted.. decrypting..')
  cat('\n\n')
  
  characters <- t(t(c("A", "a", "B", "b", "C", "c",
                      "D", "d", "E", "e", "F", "f",
                      "G", "g", "H", "h", "I", "i",
                      "J", "j", "K", "k", "L", "l",
                      "M", "m", "N", "n", "O", "o",
                      "P", "p", "Q", "q", "R", "r", "S", "s",
                      "T", "t", "U", "u", "V", "v",
                      "W", "w", "X", "x", "Y", "y",
                      "Z", "z", "1", "2", "3", "4",
                      "5", "6", "7", "8", "9", "0",
                      "!", "@", "#", "$", "%", "^",
                      "&", "*", "(", ")", " ", "-", "?", ".")))
  throwins <- t(t(c("A", "a", "B", "b", "C", "c",
                    "D", "d", "E", "e", "F", "f",
                    "G", "g", "H", "h", "I", "i",
                    "J", "j", "K", "k", "L", "l",
                    "M", "m", "N", "n", "O", "o",
                    "P", "p", "Q", "q", "R", "r", "S", "s",
                    "T", "t", "U", "u", "V", "v",
                    "W", "w", "X", "x", "Y", "y",
                    "Z", "z", "!", "@", "#",
                    "%", " ")))
  numchars <- length(throwins)
  temp <- encryption
  for (i in 1:length(throwins)) {
    temp <- gsub(throwins[i], "A", temp)
  }
  temp <- t(t(data.frame(strsplit(temp, "A"))))
  seed <- as.numeric(temp[length(temp)])
  l <- as.numeric(temp[1])
  
  n <- nchar(seed)
  nn <- nchar(l)
  encryption <- substring(encryption, 1, nchar(encryption)-(n+1))
  encryption <- substring(encryption, nn+2, nchar(encryption))
  
  set.seed(seed)
  delimiters <- sample(throwins, numchars/2, replace = FALSE)
  mixers <- throwins[-match(delimiters, throwins)]
  
  for (i in 1:length(mixers)) {
    encryption <- gsub(mixers[i], "", encryption)
  }
  for (i in 1:length(delimiters)) {
    encryption <- gsub(delimiters[i], " ", encryption)
  }
  encryption <- t(t(data.frame(strsplit(encryption, " "))))
  
  ll <- l*1000
  
  r <- matrix(0, ll, 2)
  r[,1] <- 1:ll
  set.seed(seed)
  r[,2] <- round(runif(ll, 1, length(characters)), digits = 0)
  r[,2] <- characters[r[,2]]
  r <- data.frame(r)
  encryption <- data.frame(encryption, rep(0, l))
  encryption <- as.matrix(encryption)
  lookups <- as.numeric(encryption[,1])
  
  message <- paste(r[lookups,2], collapse = "")
  set.seed(NULL)
  return(message)
}