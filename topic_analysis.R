download.file('http://erdos.ucd.ie/files/europarl/europarl-data-speeches.zip',
              'europarl-data-speeches.zip')
download.file('http://erdos.ucd.ie/files/europarl/europarl-metadata.zip',
              'europarl-metadata.zip')

# extract speeches and metadata
unzip('europarl-data-speeches.zip')
unzip('europarl-metadata.zip')
install.packages("readtext")
library(readtext)
setwd("D:/CODING/R/UNSIAP Training")
# recursively get file paths for speeches from 09-12
speeches_paths <- list.files(path = c('europarl-data-speeches/2009',
                                      'europarl-data-speeches/2010'),
                             recursive = T, full.names = T)
# read in speeches
speeches <- readtext(speeches_paths)

install.packages("quanteda")
library(quanteda)
?library(quanteda)
speeches <- corpus(speeches)

metadoc(speeches, field = 'type') <- 'European Parliament Speech'
install.packages("lubridate")
# year function
library(lubridate)
# read in speech docvars
require(data.table)
speeches_dv<-as.data.frame(fread('europarl-metadata/europarl-documents-metadata.tsv'))
class(speeches_dv)

# check numbering date
table(nchar((speeches_dv$date) != 8 | nchar(speeches_dv$date) != 8))
# subset metadata to 2009-20012
speeches_dv <- speeches_dv[year(speeches_dv$date) >= 2009 &
                             year(speeches_dv$date) <= 2010, ]

# read in MEP docvars
MEP_dv <- read.delim('europarl-metadata/europarl-meps-metadata.tsv', sep = '\t')
# merge MEP docvars onto speech metadata
dv <- merge(speeches_dv, MEP_dv, all.x = T,
            by.x = 'mep_ids', by.y = 'mep_id')

# merge docvars onto corpus
docvars(speeches) <- dv

# inspect first entries
head(docvars(speeches))


EPP_corp <- corpus_subset(speeches, group_shortname == 'EPP')

texts(EPP_corp)[5]
kwic(speeches, 'hockey', window = 7)

speeches_df <- summary(speeches, n = ndoc(speeches))

# ggplots
library(ggplot2) 
# plot density of tokens by country
ggplot(data = speeches_df, aes(x = Tokens, fill = country)) +
  geom_density(alpha = .25, linetype = 0) +
  theme_bw() +
  coord_cartesian(xlim = c(0, 1500)) +
  theme(legend.position = 'right',
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank())


ggplot(data = speeches_df, aes(x = ymd(date), y = Tokens, color = group_shortname)) +
  geom_smooth(alpha = .6, linetype = 1, se = F, method = 'loess') +
  theme_bw() +
  theme(legend.position = 'right',
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank())

# count number of missing observations in each document variable
apply(speeches$documents, 2, function(x) sum(is.na(x)))

# drop any texts with missing document variables
speeches <- corpus_subset(speeches, !is.na(country_short))

# count number of missing observations in each document variable again
apply(speeches$documents, 2, function(x) sum(is.na(x)))

speeches_sub <- corpus_sample(speeches, size = floor(ndoc(speeches) / 10))

# create document feature matrix from corpus
speeches_dfm <- dfm(speeches_sub, tolower = T, stem = T,
                    remove = stopwords('english'), remove_punct = T)
# view top 16 tokens
topfeatures(speeches_dfm, 16)

install.packages("stm")
library(stm)
speeches_stm <- convert(speeches_dfm, to = 'stm')
install.packages("plotrem")
plotRemoved(speeches_stm$documents, lower.thresh = seq(1, 25, by = 1))

speeches_stm <- prepDocuments(speeches_stm$documents,   
                              speeches_stm$vocab, 
                              speeches_stm$meta, lower.thresh = 5)

fit_stm <- stm(documents = speeches_stm$documents, vocab = speeches_stm$vocab,
               K = 15, prevalence = ~ country + group, seed = 374075,
               data = speeches_stm$meta, sigma.prior = .1)

labelTopics(fit_stm, n = 10)

par(mfrow = c(1,2))
plot(fit_stm, type = 'labels', topics = c(2, 4:6), labeltype = 'frex', main = 'FREX')
plot(fit_stm, type = 'labels', topics = c(2, 4:6), labeltype = 'score', main = 'score')

par(mfrow = c(1,1))
for (i in 1:length(unique(dv$group))) plot(fit_stm, type = 'perspectives',
                                           topics = c(5,6), labeltype = 'frex',
                                           covarlevels = unique(dv$group)[i],
                                           text.cex = .75, main = unique(dv$group)[i])

thoughts15 <- findThoughts(fit_stm, texts = speeches_sub$documents$texts,
                           topics = 14, n = 7)$docs[[1]]

plotQuote(thoughts15, width = 95, text.cex = .65, maxwidth = 275)

cloud(fit_stm, topic = 8)

plot(topicCorr(fit_stm))

est_stm <- estimateEffect( ~ country + group, fit_stm, metadata = speeches_sub$documents)

plot(est_stm, covariate = 'group', topics = 5:6, model = fit_stm, cov.value1 = as.character(unique(speeches_sub$documents$group)[7]), cov.value2 = as.character(unique(speeches_sub$documents$group)[5]), method = 'difference')

plot(est_stm, covariate = 'group', topics = 2, model = fit_stm, method = 'pointestimate')

data.frame(sort(table(speeches_sub$documents$group), decreasing = T))

custom <- c("english", "euro", "issue")

speeches_dfm <- dfm(speeches_sub, tolower = T, stem = T, remove = stopwords(custom), remove_punct = T)
