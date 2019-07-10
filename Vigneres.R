code <- "CUDRYHSODBODGRZAFDNRFCRQTELCTHNVXSOHSGNNBZNSRRQHVROOCLNTWHRELHHPELNGIOEWHRPOQHRAFOZSUGHRUHWNVTUHSBQOSEEAMAZLNODBODGRDWRDLGKYYRNQRNODNXHRUHACSLVHDULSTHNVXSGRMNQYCUOOOEZVHVVIAYEAWIBQSVQCYXDRWHRVPRHDBPEGHRNQDGKEPRWPDTPKEE"

for (n in 4:10) {
  for (i in 1:(nchar(code)-n)) {
    pos <- substr(code,i,i+n-1)
    for (j in (i+1):(nchar(code)-n+1)){
      temp <- substr(code,j,j+n-1)
      if (pos == temp) {
        print(c(i, pos, j, temp,j-i))
      }
    }
  }
}

#102, 120, 48
intersect(intersect(divisors(102), divisors(48)),divisors(120))

freq <- matrix(data = 0, nrow = 26, ncol = 6)
rownames(freq) <- LETTERS
for (i in 1:nchar(code)) {
  row <- substr(code,i,i)
  col <- (i - 1) %% 6 + 1
  freq[row,col] <- freq[row,col] + 1
}
freq
freq <- as.data.frame(freq)
ggplot(data=freq, aes(x=rownames(freq), y=freq[,1])) + geom_bar(stat="identity")
ggplot(data=freq, aes(x=rownames(freq), y=freq[,2])) + geom_bar(stat="identity")
ggplot(data=freq, aes(x=rownames(freq), y=freq[,3])) + geom_bar(stat="identity")
ggplot(data=freq, aes(x=rownames(freq), y=freq[,4])) + geom_bar(stat="identity")
ggplot(data=freq, aes(x=rownames(freq), y=freq[,5])) + geom_bar(stat="identity")
ggplot(data=freq, aes(x=rownames(freq), y=freq[,6])) + geom_bar(stat="identity")

freq2 <- matrix(data = 0, nrow = 26, ncol = 3)
rownames(freq2) <- LETTERS
for (i in 1:nchar(code)) {
  row <- substr(code,i,i)
  col <- (i - 1) %% 3 + 1
  freq2[row,col] <- freq2[row,col] + 1
}
freq2
freq2 <- as.data.frame(freq2)
ggplot(data=freq2, aes(x=rownames(freq2), y=freq2[,1])) + geom_bar(stat="identity")
ggplot(data=freq2, aes(x=rownames(freq2), y=freq2[,2])) + geom_bar(stat="identity")
ggplot(data=freq2, aes(x=rownames(freq2), y=freq2[,3])) + geom_bar(stat="identity")


code2 <- substr(code,1,1)
for (i in 2:nchar(code)){
  l <- substr(code,i,i)
  fill <- l
  if ((i-1) %% 3 == 1) {
    fill <- LETTERS[(match(l,LETTERS)+13) %% 26]
  }
  if ((i-1) %% 3 == 2) {
    fill <- LETTERS[(match(l,LETTERS)+23) %% 26]
  }
  code2 <- paste(code2,fill)
}
code2
#C H A R L E S B A B B A G E W A S A N E C C E N T R I C G E N I U S B E S T K N O W N F O R D E V E L O P I N G T H E B L U E P R I N T F O R T H E M O D E R N C O M P U T E R H E W A S T H E S O N O F B E N J A M I N B A B B A G E A W E A L T H Y L O N D O N B A N K E R H E A P P L I E D H I S G E N I U S T O M A N Y P R O B L E M S H I S I N V E N T I O N S I N C L U D E T H E S P E E D O M E T E R A N D T H E C O W C A T C H E R
