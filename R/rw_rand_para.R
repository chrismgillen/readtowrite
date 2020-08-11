#' Randomize paragraphs (rw_rand_para)
#'
#' Input is a text file with multiple paragraphs.
#' Output is a text file with paragraph order
#' randomized.
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


rw_rand_para = function(infile, title = "title", part = "section", project = "name", parabreak = "\r\n")
{
  paragraphs = tokenize_paragraphs(infile, parabreak, simplify = FALSE)
  paragraph_vec = paragraphs[[1]]
  paragraph_vec = paste(paragraph_vec, "\n")
  para_rand = sample(paragraph_vec)
  nr = length(para_rand)
  para_matrix <- matrix(para_rand, nr, 1)
  cname = paste(title, "\n\n", part, "\n", "Random paragraphs", "\n\n", sep ="")
  colnames(para_matrix) = c(cname)
  para.file.name = paste(project, part, "Rand_Para.txt", sep = "_")
  write.table(para_matrix, para.file.name, row.names = T, col.names=T, sep = "\t", quote = F)
}
