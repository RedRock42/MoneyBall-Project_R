#MoneyBall Project
#The Oakland Athletics' 2002 season was the team's 35th in Oakland, California. It was also the 102nd season in franchise history. The Athletics finished first in the American League West with a record of 103-59.
#Because of the team's smaller revenues, Oakland is forced to find players undervalued by the market, and their system for finding value in undervalued players has proven itself thus far. This approach brought the A's to the playoffs in 2002 and 2003.
#The primary goal of this project is to use 2016 baseball statistics from Sean Lahaman's Website (http://www.seanlahman.com/baseball-archive/statistics/) to find replacement players for the Toronto BlueJays. The Toronto Blue Jays are in the American League East division along with NY Yankees and Boston Red Soxs, needless to say I have my work cut out for me! 


#Check out the Batting Data 
head(Batting)


#Check the structure of the data and fix NaN values
str(Batting)
Batting$SF <- as.numeric(as.character(Batting$SF))
Batting$GIDP <- as.numeric(as.character(Batting$GIDP))


#Call specific columns with the $ symbol
#Call the head() of the first 5 rows of the AB(At Bats) column
head(Batting$AB)
head(Batting$`2B`)


#Calculate Needed Statistics 
#Batting Average
#On Base Percentage
#Slugging Percentage
Batting[is.na(Batting)] <- 0
Batting[is.nan(Batting)] <- NULL


#Create a new column for the Batting Avg called BA
Batting$BA <- Batting$H / Batting$AB
tail(Batting$BA,5)


#Create a new column for On Base Percentage (OBP) 
Batting$OBP <- (Batting$H + Batting$BB + Batting$HBP)/(Batting$AB + Batting$BB + Batting$HBP + Batting$SF)
head(Batting$OBP)


#Create a new column for the Slugging Percentage(SLG)
#For the SLG, calculate 1B(singles) using this formula: 1B = H-2B-3B-HR
Batting$`1B` <- (Batting$H - Batting$`2B` - Batting$`3B` - Batting$HR)
head(Batting$`1B`)


#Merging Salary Data with Batting Data
#I don't just want the best players, I want the most undervalued players, meaning I will also need to know current salary information!
head(Salaries$salary)
summary(Batting)


#Batting data goes all the back to 1871! Need to remove data prior to 1985 so that it matches the Salary dataset.
Batting <- subset(Batting,yearID >= 1985)
summary(Batting)


#Key players Blue Jays lost in 2016 offseason: 1B Edwin Encarnacion, OF Michael Saunders, C Josh Thole
#Since a player's position is also very import to consider, I also added the Fielding data set
#Merge the batting data with the salary and Fielding data! Since there are players playing multiple years, the data will have repetitions of playerIDs for multiple years, meaning I will have merge both players and years.
Fielding <- subset(Fielding,yearID >= 1985)
summary(Fielding)
summary(Fielding$POS)

fuse1 <- merge(Batting,Salaries,by=c('playerID','yearID'))
fuse2 <- merge(fuse1,Fielding, by=c('playerID','yearID'))
summary(fuse2)
View(fuse2)

lost_players <- subset(fuse2,playerID %in% c('encared01','tholejo01','saundmi01'))
lost_players <- subset(lost_players,yearID == 2016)
lost_players <- lost_players[,c('playerID','POS','H','2B','3B','HR','OBP','SLG','BA','AB')]


#Find Replacement Players for the key three players Toronto lost in 2016 offseason! 
#Constraints:
#1. The total combined salary of the three players can not exceed 15 million dollars.
#2. Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
#3. Their mean OBP had to equal to or greater than the mean OBP of the lost players
#4. Key playing positions must be considered- looking for a great 1B and Catcher! 

#First only grab available players from year 2016
avail.players <- filter(fuse2,yearID==2016)


#plot to check where the salary cap should be with respect to OBP
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()


#Looks like there is no point in paying above 8 million. There are also a lot of players with OBP==0. So I'll get rid of them too.
avail.players <- filter(avail.players,salary<8000000,OBP>0)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()


#The total AB of the lost players is 1209, meaning I should probably cut off my avail.players at 1209/3= 403 AB.
avail.players <- filter(avail.players,AB >= 403)


#Now sort the players by OBP.
recruits <- head(arrange(avail.players,desc(OBP)),10)
recruits <- recruits[,c('playerID','POS','OBP','AB','salary')]
View(recruits)


#Rusults:
#bryankr01 plays OF & 1B, have him play OF
#beltbr01 plays 1B and although has a lower AB number, his OBP is nearly almost .4! 
#No Catcher on this list, need to re-evaluate specifically for catchers. 
#Thus far, Salary = $6,852,000, meaning I have $8,148,000 left to find an awesome Catcher!


#Hunt for a Catcher
avail.catchers<- filter(fuse2,yearID==2016 & POS=="C")
ggplot(avail.catchers,aes(x=OBP,y=salary)) + geom_point()
avail.catchers <- filter(avail.catchers,salary<8000000,OBP>0)
ggplot(avail.catchers,aes(x=OBP,y=salary)) + geom_point()
avail.catchers <- filter(avail.catchers,AB >= 300)

catcher <- head(arrange(avail.catchers,desc(OBP)),10)
catcher<- catcher[,c('playerID','POS','OBP','AB','salary')]


#Toronto Blue Jays new catcher is Jonathan Lucroy!
#Acquired by the Texans for $4.5 million. He is a veteran catcher who had an excellent performance with the Brewers.Hoping this trade will give him the confidence he needs to get him back to his top performance.


#Summary
#Spent $10,952,000 recruiting 3 key undervalued players(Kris Bryant, Brandon Belt,and Jonathan Lucroy) that will take the Toronto Blue Jays to the 2017 MLB Pennant!!
final_recruits <- subset(fuse2,playerID %in% c('bryankr01','beltbr01','lucrojo01'))
final_recruits<- final_recruits[,c('playerID','POS','OBP','AB','salary')]
View(final_recruits)



