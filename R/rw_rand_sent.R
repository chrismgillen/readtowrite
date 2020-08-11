#' Randomize sentences (rw_rand_sent)
#'
#' Input is a text file with multiple paragraphs.
#' Output is a text file for each paragraph with the sentence
#' order randomized.
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
rw_rand_sent = function(infile, title = "title", part = "section", project = "name", parabreak = "\r\n")
{
  paragraphs = tokenize_paragraphs(infile, parabreak, simplify = FALSE)
  paragraph_vec = paragraphs[[1]]
  sent_all = lapply(paragraphs, tokenize_sentences)
  sent_all = sent_all[[1]]
  rand_sent = lapply(sent_all, sample)
  for (index in 1:length(paragraph_vec))
  {
    sentence_vec = rand_sent[[index]]
    sentence_vec = trimws(sentence_vec)
    sentence_vec = paste(sentence_vec, "\n")
    sentence_rand = sample(sentence_vec)
    file.name = paste(project, part, "Para", index, "Rand_Sent.txt", sep = "_")
    sent_nr <- length(sentence_vec)
    sent_matrix <- matrix(sentence_rand, sent_nr, 1)
    cname = paste(title, "\n\n", part, "\n", "Paragraph number = ", index, "\n", "Random sentences", "\n\n", sep ="")
    colnames(sent_matrix) = c(cname)
    write.table(sent_matrix, file.name, row.names = T, col.names=T, sep = "\t", quote = F)
  }
}

