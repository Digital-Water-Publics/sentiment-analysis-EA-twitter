
library(ore)
library(dplyr)

emoji_src <- "https://raw.githubusercontent.com/laurenancona/twimoji/gh-pages/twitterEmojiProject/emoticon_conversion_noGraphic.csv"
emoji_fil <- basename(emoji_src)
if (!file.exists(emoji_fil)) download.file(emoji_src, emoji_fil)

emoji <- read.csv(emoji_fil, header=FALSE, stringsAsFactors = FALSE)
emoji_regex <- sprintf("(%s)", paste0(emoji$V2, collapse="|"))
compiled <- ore(emoji_regex)


chat <- "Be prepared <U+26A1><U+FE0F><U+26C8><U+FE0F><U+0001F327><U+FE0F><U+2614><U+FE0F> "
chat = data.frame(text = chat, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)

which(grepl(emoji_regex, chat, useBytes = TRUE))

chat_emoji_lines <- chat[which(grepl(emoji_regex, chat, useBytes = TRUE))]

found_emoji <- ore.search(compiled, chat_emoji_lines, all=TRUE)
emoji_matches <- matches(found_emoji)

str(emoji_matches, 1)
## List of 254
##  $ : chr [1:4] "\U0001f600" "\U0001f600" "\U0001f44d" "\U0001f44d"
##  $ : chr "\U0001f648"
##  $ : chr [1:2] "\U0001f44d" "\U0001f44d"
##  $ : chr "\U0001f602"
##  $ : chr [1:3] "\U0001f602" "\U0001f602" "\U0001f602"
##  $ : chr [1:4] "\U0001f44c" "\U0001f44c" "\U0001f44c" "\U0001f44c"
##  $ : chr [1:6] "\U0001f602" "\U0001f602" "\U0001f602" "\U0001f602" ...
##  $ : chr "\U0001f600"
##  $ : chr [1:5] "\U0001f604" "\U0001f604" "\U0001f604" "\U0001f603" ...
##  $ : chr "\U0001f44d"
## ...

data_frame(
  V2 = flatten_chr(emoji_matches) %>% 
    map(charToRaw) %>% 
    map(as.character) %>% 
    map(toupper) %>% 
    map(~sprintf("\\x%s", .x)) %>% 
    map_chr(paste0, collapse="")
) %>% 
  left_join(emoji) %>% 
  count(V3, sort=TRUE)
