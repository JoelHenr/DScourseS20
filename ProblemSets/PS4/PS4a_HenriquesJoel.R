library("jsonlite")
system("wget -O nfl.json 'http://api.fantasy.nfl.com/v1/players/stats?statsType=seasonStats&season=2010&week=1&format=json'")
mydf <- fromJSON("nfl.json")
class(mydf$players)
head(mydf,5)


