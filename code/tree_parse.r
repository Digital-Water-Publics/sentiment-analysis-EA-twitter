library(spacyr)
parsedtxt = spacy_parse(
  "please can you explain how you grant a permit to a company run by a man with a criminal record and why charges against him were dropped for polluting our rivers with cyanide",
  pos = TRUE,
  lemma = TRUE,
  dependency = TRUE,
  nounphrase = TRUE,
  multithread = TRUE
)
library(rsyntax)
tokens = as_tokenindex(parsedtxt)
plot_tree(tokens, token, lemma)
