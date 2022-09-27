#' Alphabetizer Alphabetize using Unificado or Azümchefe order 
#' 
#' This functions allows you to sort a list of words by changing the letters
#' into a numeric sequence. For use, follow examples.
#' 
#' @param string A character vector in Unificado or Azümchefe.
#' 
#' @param input A writing system such as "AMU" (Unificado) or "AZU" (Azümchefe)
#' 
#' The argument "AMU" corresponds to the alphabetical order followed
#' in _Augusta, F. J. de(2017), Diccionario Mapudungún - Español. UCT
#' Ediciones_, therefore apostrophes must be used in the input text for
#' interdentals: t’, n’, l’
#' 
#'  Defaults to AMU
#' 
#' @author Aldo Berrios, \email{aldoberrios@@gmail.cl}
#' 
#' @examples
#' 
#' 
#' list_amu <- c("ñaña", "n’ome", "mi", "ngürü", "nu")
#' list_azu <- c("ñaña", "nhome", "mi", "gürü", "nu")
#' data <- data.frame(list_azu, list_amu)
#' data[alphabetizer(list_amu),]
#' data[alphabetizer(list_azu, input = "AZU"),]
#' 
#' @export alphabetizer

alphabetizer <- function (string, input = "AMU") {

# Change all text to lowercase
  string <- tolower(string)

if (input == "AMU") {
    string <- gsub("ch", "1", string)
    string <- gsub("l’", "9", string)
    string <- gsub("ll", "A", string)
    string <- gsub("n’", "D", string)
    string <- gsub("ng", "F", string)
    string <- gsub("sh", "K", string)
    string <- gsub("tr", "N", string)
    string <- gsub("t’", "M", string)
    string <- gsub("a",  "0", string)
    string <- gsub("d",  "2", string)
    string <- gsub("e",  "3", string)
    string <- gsub("f",  "4", string)
    string <- gsub("g",  "5", string)
    string <- gsub("i",  "6", string)
    string <- gsub("k",  "7", string)
    string <- gsub("l",  "8", string)
    string <- gsub("m",  "B", string)
    string <- gsub("n",  "C", string)
    string <- gsub("ñ",  "E", string)
    string <- gsub("o",  "G", string)
    string <- gsub("p",  "H", string)
    string <- gsub("r",  "I", string)
    string <- gsub("s",  "J", string)
    string <- gsub("t",  "L", string)
    string <- gsub("u",  "O", string)
    string <- gsub("ü",  "P", string)
    string <- gsub("w",  "Q", string)
    string <- gsub("y",  "R", string)
}

if (input == "AZU") {
    string <- gsub("ch", "4", string)
    string <- gsub("nh", "A", string)
    string <- gsub("tx", "B", string)
    string <- gsub("lh", "G", string)
    string <- gsub("ll", "K", string)
    string <- gsub("th", "Q", string)
    string <- gsub("t’", "S", string)
    string <- gsub("sh", "R", string)
    string <- gsub("a",  "0", string)
    string <- gsub("z",  "1", string)
    string <- gsub("ü",  "2", string)
    string <- gsub("m",  "3", string)
    string <- gsub("e",  "5", string)
    string <- gsub("f",  "6", string)
    string <- gsub("i",  "7", string)
    string <- gsub("k",  "8", string)
    string <- gsub("t",  "9", string)
    string <- gsub("o",  "C", string)
    string <- gsub("y",  "D", string)
    string <- gsub("q",  "E", string)
    string <- gsub("g",  "F", string)
    string <- gsub("ñ",  "H", string)
    string <- gsub("r",  "I", string)
    string <- gsub("s",  "J", string)
    string <- gsub("p",  "L", string)
    string <- gsub("u",  "M", string)
    string <- gsub("w",  "N", string)
    string <- gsub("l",  "O", string)
    string <- gsub("n",  "P", string)
  }

    return(order(string))

}
