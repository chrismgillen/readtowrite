#' Remove first sentence of paragraph (rw_remove_first)
#'
#' Input is a text file with multiple paragraphs.
#' Output is a text file for each paragraph with the first
#' sentence removed.
#'
#' @param infile A text file containing several paragraphs.
#' @param title Title of text (e.g. paper reference).
#' @param part Part of the text (e.g. paper section).
#' Used in filenames.
#' @param project Project nickname for use in filenames
#' @param parabreak The characters that designate paragraph
#' breaks in the input
#' @return A text file with paragraphs without first sentence
#' @export
#'

rw_remove_first = function(infile, title = "title", part = "section", project = "name", parabreak = "\r\n")
{
  paragraphs = tokenize_paragraphs(infile, parabreak, simplify = FALSE)
  paragraph_vec = paragraphs[[1]]
  sent_all = lapply(paragraphs, tokenize_sentences)
  sent_all = sent_all[[1]]
  num.para = length(paragraph_vec)
  no.first.vector = NULL



  for (index in 1:num.para)
  {
    sentence_vec = sent_all[[index]]
    sentence_vec = trimws(sentence_vec)
    num.sentences = length(sentence_vec)
    no.first = NULL
    for (sent in 2:num.sentences)
    {
      no.first = paste(no.first, sentence_vec[sent], sep = " ")
    }
    no.first = paste(no.first, "\n")
    no.first.vector = append(no.first.vector, no.first)
  }

  nr <- num.para
  no.first.matrix <- as.matrix(no.first.vector, nr, 1)
  file.name = paste(project, part, "No_First.txt", sep = "_")
  cname = paste(title, "\n\n", part, "\n", "Topic sentences removed", "\n\n", sep ="")
  colnames(no.first.matrix) = c(cname)
  write.table(no.first.matrix, file.name, row.names = T, col.names=T, sep = "\t", quote = F)
}
