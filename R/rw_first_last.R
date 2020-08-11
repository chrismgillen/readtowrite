#' First and last sentences (rw_first_last)
#'
#' Input is a text file with multiple paragraphs.
#' Output is a text file in which only the first and last
#' sentences of each paragraph are printed.
#'
#' @param infile A text file containing several paragraphs.
#' @param title Title of text (e.g. paper reference).
#' @param part Part of the text (e.g. paper section).
#' Used in filenames.
#' @param project Project nickname for use in filenames
#' @param parabreak The characters that designate paragraph
#' breaks in the input
#' @return A text file with paragraphs in random order
#' @export
#'
rw_first_last = function(infile, title = "title", part = "section", project = "name", parabreak = "\r\n")
{
  paragraphs = tokenize_paragraphs(infile, parabreak, simplify = FALSE)
  paragraph_vec = paragraphs[[1]]
  sent_all = lapply(paragraphs, tokenize_sentences)
  sent_all = sent_all[[1]]
  num.para = length(paragraph_vec)
  first.last.vector = NULL

  for (index in 1:num.para)
  {
    sentence_vec = sent_all[[index]]
    sentence_vec = trimws(sentence_vec)
    num.sentences = length(sentence_vec)
    first.sent = sentence_vec[1]
    last.sent = sentence_vec[num.sentences]
    first.last = paste(first.sent, "  [...]  ", last.sent, "\n")
    first.last.vector = append(first.last.vector, first.last)
  }

  nr <- num.para
  first.last.matrix <- matrix(first.last.vector, nr, 1)
  file.name = paste(project, part, "First_Last.txt", sep = "_")
  cname = paste(title, "\n\n", part, "\n", "First and Last Sentences", "\n\n", sep ="")
  colnames(first.last.matrix) = c(cname)
  write.table(first.last.matrix, file.name, row.names = T, col.names=T, sep = "\t", quote = F)
}
