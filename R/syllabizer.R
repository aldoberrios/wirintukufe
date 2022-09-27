#' syllabizer separates an IPA text into syllables
#' 
#' 'syllabizer' - A function that separates Mapudungun words written in the
#' International Phonetic Alphabet into syllables. It makes use of the
#' straigthforward (C)V(C) syllabic pattern of Mapudungun to find
#' syllable boundaries. It does not parse texts written
#' in the Alfabeto Mapuche Unificado, so the function phonemizer() must be
#' used before, in order to obtain an IPA text.
#' 
#' @author Aldo Berrios, \email{aldoberrios@@gmail.cl}
#' 
#' @param string A character vector.
#' 
#' @examples
#' # Change any text into IPA with phonemizer():
#' text <- phonemizer("trafmapuche")
#' 
#' # Separate the syllables using syllabizer():
#' syllabizer(text)
#' > [1] "ʈ͡ʂaf.ma.pu.t͡ʃe"
#' 
#' @export syllabizer

syllabizer <- function (string) {

# Convert digraphs into simple characters for syllabification
  string <- gsub("t͡ʃ", "ʧ", string)
  string <- gsub("ʈ͡ʂ", "ŧ", string)
  string <- gsub("t̪",   "ţ", string)
  string <- gsub("n̪",   "ņ", string)
  string <- gsub("l̪",   "ļ", string)

  string   <- gsub("([aeiouə])", ".\\1.", string)
  string   <- gsub("\\.([pţtʧŧkmņnɲŋfθsʃɻjɣwļlʎ])\\.([aeiouə])\\.", ".\\1\\2.", string)
  string   <- gsub("([pţtʧŧkmņnɲŋfθsʃɻjɣwļlʎ])([pţtʧŧkmņnɲŋfθsʃɻjɣwļlʎ])", "\\1.\\2", string)
  string   <- gsub("([aeiouə])\\.([pţtʧŧkmņnɲŋfθsʃɻjɣwļlʎ])\\.([pţtʧŧkmņnɲŋfθsʃɻjɣwļlʎ])", "\\1\\2.\\3", string)
  string   <- gsub("([aeiouə])\\.([pţtʧŧkmņnɲŋfθsʃɻjɣwļlʎ])$", "\\1\\2", string)
  string   <- gsub("^([pţtʧŧkmņnɲŋfθsʃɻjɣwļlʎ])\\.([aeiouə])", "\\1\\2", string)
  string   <- gsub("\\.([pţtʧŧkmņnɲŋfθsʃɻjɣwļlʎ])\\.([aeiouə])", ".\\1\\2", string)
  string   <- gsub("\\.\\.([aeiouə])", ".\\1", string)
  string   <- gsub("^\\.", "", string)
  string   <- gsub("\\.$", "", string)

# Reverts simple into character
  string <- gsub("ʧ", "t͡ʃ", string)
  string <- gsub("ŧ", "ʈ͡ʂ", string)
  string <- gsub("ţ", "t̪",   string)
  string <- gsub("ņ", "n̪",   string)
  string <- gsub("ļ", "l̪",   string)

  return(string)

}
