#' Match first sentence to paragraphs (rw_match_first)
#'
#' Input is a text file with multiple paragraphs.
#' Output is a text file for each paragraph with the first
#' sentence removed and with the first sentences listed
#' in random order.
#'
#' @param infile A text file containing several paragraphs.
#' @param title Title of text (e.g. paper reference).
#' @param part Part of the text (e.g. paper section).
#' Used in filenames.
#' @param project Project nickname for use in filenames
#' @param parabreak The characters that designate paragraph
#' breaks in the input
#' @return A text file of paragraphs with first sentences
#'  removed and random list of first sentences
#' @export
#'
rw_match_first = function(infile, title = "title", part = "section", project = "name", parabreak = "\r\n")
{
  paragraphs = tokenize_paragraphs(infile, parabreak, simplify = FALSE)
  paragraph_vec = paragraphs[[1]]
  sent_all = lapply(paragraphs, tokenize_sentences)
  sent_all = sent_all[[1]]
  num.para = length(paragraph_vec)
  no.first.vector = NULL
  first.vector = NULL


  for (index in 1:num.para)
  {
    sentence_vec = sent_all[[index]]
    sentence_vec = trimws(sentence_vec)
    num.sentences = length(sentence_vec)
    first.sent = paste(sentence_vec[1], "\n")
    no.first = NULL
    for (sent in 2:num.sentences)
    {
      no.first = paste(no.first, sentence_vec[sent], sep = " ")
    }
    no.first = paste(no.first, "\n")
    no.first.vector = append(no.first.vector, no.first)
    first.vector = append(first.vector, first.sent)
  }
  first.vector = sample(first.vector)
  match.first.vector = append(no.first.vector, "First sentences in random order\n\n")
  match.first.vector = append(match.first.vector, first.vector)
  nr <- num.para*2 + 1
  match.first.matrix <- as.matrix(match.first.vector, nr, 1)
  file.name = paste(project, part, "Match_First.txt", sep = "_")
  cname = paste(title, "\n\n", part, "\n", "Topic sentences removed", "\n\n", sep ="")
  colnames(match.first.matrix) = c(cname)
  write.table(match.first.matrix, file.name, row.names = T, col.names=T, sep = "\t", quote = F)
}
