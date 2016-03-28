## Description: Look! It's Politikon data!
## Author: Gonzalo Rivero
## Date: 21-Mar-2016 11:11

Sys.setlocale("LC_TIME", "es_ES.UTF-8") 

library(RMySQL)
library(scales)
library(dplyr)
library(reshape2)
library(tm)
library(stm)
library(textcat)
library(splines)

quartzFonts(avenir = c("Avenir Book", "Avenir Black",
                       "Avenir Book Oblique", "Avenir Black Oblique"))

## Data loading
mydb <- dbConnect(MySQL(),
                  user='root',
                  password='password',
                  dbname='politikon_es',
                  host='localhost')

rs <- dbSendQuery(mydb, "select * from wp_qy8g3n_posts")
posts <- fetch(rs, n=-1)

## Only published posts
posts <- posts[posts$post_type == "post" & posts$post_status == "publish",]
## Only politikon
posts$major <- gsub("http://w{0,3}\\.?([a-z-]*)\\..*", "\\1", posts$guid)
posts <- posts[posts$major == "politikon", ]

## Clean up HTML
clean_html <- function(x) {
    return(gsub("<.*?>|nbsp", "", x))
}

clean_newlines <- function(x) {
    return(gsub("\n|\r|\t| ", " ", x))
}

clean_extrawhitespace <- function(x) {
    return(gsub(" {2,}", "", x))
}

posts <- posts %>%
    mutate(post_content = clean_newlines(clean_html(tolower(post_content))),
           language = textcat(as.character(post_content)))
posts <- posts[posts$language == "spanish", ]

## Distribution over time
posts$post_date <- strptime(posts$post_date, "%Y-%m-%d %H:%M:%S")

## Select only period posts
posts <- posts[as.Date(posts$post_date) >= as.Date("2011-01-01") & 
               as.Date(posts$post_date) <= as.Date("2016-01-01"), ]

posts$dweek <- as.numeric((posts$post_date - min(posts$post_date))/(24*60*60)) %/% 7

posts$mbins <- format(posts$post_date, format="%Y-%b")

## Group at week level
evol <- posts[, c("mbins", "post_author", "dweek")] %>%
    mutate(monthbins=format(mbins)) %>%
    group_by(dweek) %>%
    summarize(n=length(mbins))

## Trend
tmodel <- lm(n ~ s(dweek), data=evol)
evol$tmodel <- predict(tmodel)

## Evolution of posts over time
png("../img/evolution-time.png", width=12, height=8, units="in", res=300)
par(family = "avenir")
plot(as.numeric(as.factor(evol$dweek)), evol$n,
     type="l",
     col="red",
     bty="n",
     lwd=2,
     xaxt="n",
     xlab="",
     ylab="")
lines(as.numeric(as.factor(evol$dweek)), evol$tmodel,
      col="blue",
      lty=3,
      lwd=2)
abline(h=seq(0, 100, by=20),
       v=seq(0, 260, length.out=7),
       lty=2,
       col="gray",
       lwd=0.5)
title(main="Cuánto hemos escrito en estos 5 años",
      xlab="Fecha",
      ylab="Número de entradas",
      cex.main=1.7,
      cex.lab=1.4)
mtext("Gonzalo Rivero | Politikon.es",
      at=235, side=1, padj=5, cex=1,
      col="gray40")
labs <- unique(format(sort(as.Date(posts$post_date)), "%b/%Y"))[seq(0, 60, 10)]
axis(1, at=seq(0, 260, length.out=7),
     labels=c("dic/2010", labs))
dev.off()

#### Content ####
tdm <- Corpus(VectorSource(posts$post_content))
tdm <- tm_map(tdm, removePunctuation)
tdm <- tm_map(tdm, removeNumbers)
tdm <- tm_map(tdm, stripWhitespace)
tdm <- tm_map(tdm, removeWords, stopwords("spanish"))
tdm <- tm_map(tdm, stemDocument, language="spanish")

adtm <- DocumentTermMatrix(tdm)
adtm <- removeSparseTerms(adtm, 0.9) ## Not many options here!

#### Hack my way aroud STM's very opinionated input ####
make_metadata <- function(tdm, dtm, data) {
    for (i in 1:ncol(data)) {
        NLP::meta(tdm, colnames(data)[i]) <- data[, i]
    }
    metadata <- NLP::meta(tdm)[unique(dtm$i),]
    return(metadata)
}

posts$monthtime <- as.numeric(as.factor(format(posts$post_date, "%Y-%b")))

slamout <- stm:::read.slam(adtm)
vocab <- as.character(slamout$vocab)
kept <- (1:nrow(posts) %in% unique(adtm$i))
metadata <- make_metadata(tdm, adtm, posts[, c("monthtime", "post_author")])

predocs <- list(documents=slamout$documents,
            vocab=vocab,
            meta=metadata, 
        docs.removed = which(!kept))

processed <- prepDocuments(predocs$documents, predocs$vocab, predocs$meta)

## Model with fixed number of topics
base_model <- stm(processed$documents,
                  processed$vocab,
                  K=12,
                  prevalence = ~ s(monthtime),
                  max.em.its=75,
                  data=processed$meta,
                  init.type="Spectral")
saveRDS(base_model, "../dta/base-model.RDS")

base_model <- readRDS("../dta/base-model.RDS")

## Search in topic space
## storage <- searchK(processed$documents,
##                    processed$vocab,
##                    K=c(5, 7, 10, 12, 15, 25, 50),
##                    prevalence =~ s(monthtime),
##                    data=processed$meta)

## This is me interpreting things
temas <- c("Actualidad", "Economía", "Historia y libros", "Trenes", "Partidos políticos",
           "Política americana", "Comunicación", "Integración europea", "Instituciones",
           "Mercado laboral", "Comportamiento electoral", "Políticas públicas")

source("cloud.R") ## Because the stm function sucks
## Plot all topics in a dotplot
png("../img/topics.png", width=14, height=12, units="in", res=260)
op <- par(mfrow = c(4, 3),
          oma = c(3, 5, 5, 3),          
          mgp = c(3.5, 2, 0),    
          mar = c(5, 5, 2, 5),
          xpd = NA,
          family = "avenir")
for (i in 1:12) {
    mycloud <- cloud(base_model, topic = i,
                     scale = c(2,.25),
                     thresh=0.95,
                     type="model",
                     max.words=20)
    mydata <- mycloud[order(mycloud$freq, decreasing=TRUE),][1:7, ]
    mydata$words <- factor(mydata$words, mydata$words)
    plot(x=mydata$freq, y=as.numeric(mydata$words),
         bty="n", yaxt="n", xaxt="n", ylab="", col="blue", xlab="",
         pch=19, cex=1.3)
    abline(h=1:7, col="gray", lty=2, lwd=0.5)
    axis(2, at=1:7, labels=as.character(mydata$words), las=1, cex.axis=1.5)
    axis(1, cex.axis=1.5)	
    if (i %in% 10:12) {
        title(temas[i], xlab="Frecuencia", cex.main=1.6, cex.lab=1.4)
    } else {
        title(temas[i], cex.main=1.6, cex.lab=1.4)
    }
}
mtext("Distribución de los temas", side = 3, line = 1, outer = TRUE, cex=1.7)
mtext("Gonzalo Rivero | Politikon.es",
      at=12200, side=1, padj=6, cex=1,
      col="gray40")
par(op)
dev.off()

## Prevalence over time
png("../img/evolution-topics.png", width=10, height=8, units="in", res=260)
op <- par(mfrow = c(4, 3),
          oma = c(3, 5, 5, 3),          
          mgp = c(3.5, 2, 0),    
          mar = c(3.5, 3, 2, 2),
          xpd = NA,
          family = "avenir")
for (i in 1:12) {
    prep <- estimateEffect(c(i) ~ s(monthtime),
                           base_model,
                           metadata = predocs$meta,
                           uncertainty = "None")    
    plot.estimateEffect(prep,
                        covariate = "monthtime",
                        model = base_model,
                        method = "continuous",
                        xlab = "",
                        linecol = "blue",
                        ylim = c(0, .2),
                        printlegend = FALSE,
                        bty="n",
                        lwd=3,
                        cex.axis=1.2,
                        ylab="",
                        panel.first = grid())    
    ## axis(side=1)
    if (i %in% 10:12) {
        title(temas[i], xlab="Mes", ylab="", cex.main=1.5, cex.lab=1.4)
    }
    if (i %in% c(1, 4, 7, 10)) {
        title(temas[i], xlab="", ylab="Prevalencia", cex.main=1.5, cex.lab=1.4)
    }
    else {
        title(temas[i], xlab="", ylab="", cex.main=1.5, cex.lab=1.4)
    }
}
mtext("Gonzalo Rivero | Politikon.es",
      at=35, side=1, padj=6.5, cex=0.9,
      col="gray40")
mtext("Evolución de los temas", side = 3, line = 1, outer = TRUE, cex=1.4)
par(op)
dev.off()
