curl --silent http://feeds.digg.com/digg/container/technology/popular.rss http://feeds.digg.com/digg/container/science/popular.rss http://feeds.digg.com/digg/container/gaming/popular.rss | grep -e "<title>digg.com: Stories" -e "<link>http://feeds.digg.com" | sed -e '/<title>digg.com/s//slashdot.org/g' -e '/<\/title>/s///g' -e '/<link>/s///g' -e '/<\/link>/s///g'
