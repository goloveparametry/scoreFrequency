# Analysing score frequencies on HT a FT 2003-2012 in Gambrinus Liga compared to Europe's Big 5 leagues.
# Inspired by http://www.optasportspro.com/en/about/optapro-blog/posts/2013/guest-blog-big-5-league-scores-half-time-and-full-time.aspx

# Packages required
require("XML")
require("ggplot2")
require("plyr")
require("reshape2")

# Data domain
domain <- "http://www.gambrinusliga.cz/rozpis-zapasu/"

# Loop pointer (ie. season)
s <- c(2003:2013)

# FETCH DATA FROM WEB TO CSV FILE

for (i in s) { # Loop over seasons
  
  # Build URL
  url <- paste(domain, i, ".html?type=2", sep = "") 
  print(url)
  
  # Scrape all links with match details
  u <- xpathSApply(htmlParse(url),"//a[@title!='Informace o klubu']", function(x) c(xmlAttrs(x)[["href"]]))
  u <- u[grepl("/zapas/",u)]
  
  
  for (j in u) { # Loop over matches urls
    
    # Build URL
    j <- paste("http://www.gambrinusliga.cz/", j, sep = "")
    print(j)
    
    # Scrape match details (Home Team, Away Team, HT Score, FT Score etc.)
    v <- xpathSApply(htmlParse(j, encoding="UTF-8"),"//div[@class='span14']", xmlValue)
    v <- gsub("\n",",",v)
    
    # Write line to CSV
    write(v, file="data/output.txt", append=TRUE)
    #break
  }  
  #break
}

# LOAD DATASET FROM CSV FILE

# R Settings
options(stringsAsFactors = FALSE)

# Load CSV file data to dataframe
df <- read.csv(file = "data/output.txt", header = F, )

# Rearrange and rename the columns 
df <- df[, c(1,3,2,4)]
colnames(df) <- c("home","away","ft","ht")

# Add season identifier
df$season <- rep(2003:2013, each=240) # 240 matches x 11 seasons = 2640 observations

# Split score to columns
df$ft1 <- as.integer(lapply(strsplit(as.character(df$ft), "\\:"), "[", 1))
df$ft2 <- as.integer(lapply(strsplit(as.character(df$ft), "\\:"), "[", 2))

df$ht1 <- as.integer(lapply(strsplit(as.character(df$ht), "\\:"), "[", 1))
df$ht2 <- as.integer(lapply(strsplit(as.character(df$ht), "\\:"), "[", 2))

# Evaluate outcomes HT a FT

df$outcomeHT[df$ht1 == df$ht2] <- "D"
df$outcomeHT[df$ht1 < df$ht2] <- "L"
df$outcomeHT[df$ht1 > df$ht2] <- "W"

df$outcomeFT[df$ft1 == df$ft2] <- "D"
df$outcomeFT[df$ft1 < df$ft2] <- "L"
df$outcomeFT[df$ft1 > df$ft2] <- "W"


# MANUAL IMPORT EXTERNAL DATA FROM http://www.optasportspro.com/en/about/optapro-blog/posts/2013/guest-blog-big-5-league-scores-half-time-and-full-time.aspx

# Full time score distribution
df.dist.ft <- data.frame(
  scoreFT = c("1:1", "1:0", "2:1", "0:0", "2:0", "0:1", "1:2", "2:2", "0:2", "3:1"),
  premierLeagueFT = c(11.5, 10.6, 9.3, 8.5, 8.2, 7.1, 6.3, 5.0, 4.5, 4.6),
  laLigaFT = c(12.0, 11.6, 9.1, 7.2, 8.1, 6.7, 6.5, 4.0, 4.8, 4.3),
  ligue1FT = c(14.1, 12.9, 8.3, 11.1, 9.3, 7.3, 5.6, 4.3, 4.6, 4.1),
  bundesligaFT = c(11.8, 8.6, 9.0, 6.7, 7.3, 6.1, 6.4, 5.5, 4.8, 4.4),
  serieAFT = c(12.3, 11.7, 8.9, 8.7, 8.5, 6.6, 6.0, 5.7, 4.8, 4.8)
)

# Half time score distribution
df.dist.ht <- data.frame(
  scoreHT = c("0:0", "1:0", "0:1", "1:1", "2:0", "0:2", "2:1", "1:2", "3:0", "3:1"),
  premierLeagueHT = c(30.8, 21.2, 15.2, 10.3, 7.8, 3.6, 3.4, 2.2, 1.9, 0.6),
  laLigaHT = c(30.5, 20.5, 15.0, 11.4, 7.0, 3.9, 3.8, 2.1, 1.8, 1.0),
  ligue1HT = c(35.3, 23.3, 13.8, 9.9, 6.6, 3.3, 2.6, 1.7, 1.1, 0.5),
  bundesligaHT = c(27.7, 20.5, 15.6, 11.6, 7.2, 4.5, 3.3, 2.5, 2.4, 1.3),
  serieAHT = c(32.8, 21.5, 14.6, 10.6, 6.6, 3.2, 3.8, 1.7, 1.6, 0.9)
)

# ANALYZE and COMPARE GL with BIG 5 LEAUES

# Full time score distribution in Gambrinus Liga (absolute)
#sort(table(df$ft), decreasing=T)

# Full time score distribution in Gambrinus Liga (relative)
gambrinusLigaFT <- round(100*sort(table(df$ft), decreasing=T)/nrow(df),1)

# Joined with external data
df.dist.ft <- merge(df.dist.ft, data.frame(gambrinusLigaFT), by.x="scoreFT", by.y=0, all.x=T)
df.dist.ft.ord <- df.dist.ft[with(df.dist.ft, order(-gambrinusLigaFT)),]

# Half time score distribution in Gambrinus Liga (absolute)
#sort(table(df$ht), decreasing=T)

# Half time score distribution in Gambrinus Liga (relative)
gambrinusLigaHT <- round(100*sort(table(df$ht), decreasing=T)/nrow(df),1)

# Joined with external data
df.dist.ht <- merge(df.dist.ht, data.frame(gambrinusLigaHT), by.x="scoreHT", by.y=0, all.x=T)
df.dist.ht.ord <- df.dist.ht[with(df.dist.ht, order(-gambrinusLigaHT)),]

# Get sum of columns for each league
unlist(lapply(df.dist.ft, function(x) if(is.numeric(x)) sum(x, na.rm=T)))
unlist(lapply(df.dist.ht, function(x) if(is.numeric(x)) sum(x, na.rm=T)))

# Outcome change
nrow(df[df$ht == df$ft,])

df$scoreChange[df$ht == df$ft] <- F
df$scoreChange[grepl("[0-9]:[0-9]", df$ht) & df$ht != df$ft] <- T

df$outcomeChange[df$outcomeHT == df$outcomeFT] <- F
df$outcomeChange[df$outcomeHT != df$outcomeFT] <- T

table(df$outcomeChange, df$ht)

# VISUAL

# Bar chart GL HT
qplot(x = scoreHT, 
      weight = value, 
      data = melt(df.dist.ht)[melt(df.dist.ht)$variable == "gambrinusLigaHT" & melt(df.dist.ft)$scoreFT %in% c("0:0", "1:0", "0:1", "1:1", "2:0","0:2"),], 
      geom = "bar", 
      ylab = "HT score distribution (percent)") +  facet_grid(.~variable)

# Bar chart GL FT
qplot(x = scoreFT, 
      weight = value, 
      data = melt(df.dist.ft)[melt(df.dist.ft)$variable == "gambrinusLigaFT" & melt(df.dist.ft)$scoreFT %in% c("0:0", "1:0", "0:1", "1:1", "2:0","0:2"),], 
      geom = "bar", 
      ylab = "FT score distribution (percent)") +  facet_grid(.~variable)

# Bar chart Comparision HT
qplot(x = scoreHT, 
      weight = value, 
      data = melt(df.dist.ht)[melt(df.dist.ht)$scoreHT %in% c("0:0", "1:0", "0:1", "1:1", "2:0","0:2"),], 
      geom = "bar", 
      ylab = "HT score distribution (percent)", 
      fill = scoreHT) +  facet_grid(.~variable)


# Bar chart Comparision FT
qplot(x = scoreFT, 
      weight = value, 
      data = melt(df.dist.ft)[melt(df.dist.ft)$scoreFT %in% c("0:0", "1:0", "0:1", "1:1", "2:0","0:2"),], 
      geom = "bar", 
      ylab = "FT score distribution (percent)", 
      fill = scoreFT) +  facet_grid(.~variable)

# EXPORT 

write.csv(df.dist.ht, "data/dist-ht.csv", row.names = F)
write.csv(df.dist.ft, "data/dist-ft.csv", row.names = F)

