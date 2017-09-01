#setwd(getwd())

require(NLP)
require(openNLP)
require(rJava)

#some text
s <-  paste(c("Hey Amlan,I went through the ER diagram.","It was well designed keeping the future scope in concern.","But for scalability the better option is to keep the user's pattern,preference, tracking in a different database NoSQL."),collapse = "")
s <- as.String(s)

#Need sentance and work annotation
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s,list(sent_token_annotator, word_token_annotator))
pos_tag_annotator <-  Maxent_POS_Tag_Annotator()

a3 <- annotate(s,pos_tag_annotator,a2)
a3
a3w <- subset(a3,type == "word")
tags <- sapply(a3w$features, '[[', "POS")
tags
table(tags)

#Extract token/POS pairs
print(sprintf("%s%s", s[a3w],tags))
