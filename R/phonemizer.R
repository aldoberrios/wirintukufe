#' phonemizer Convert AMU text into IPA
#' 
#' This functions changes text written in the Alfabeto Mapuche Unificado into
#' the International Phonetic Alphabet. If the text is written in another
#' alphabet, function graphemizer could be used first (see examples below).
#' 
#' @param string A character vector.
#' 
#' @param clean strips text of typographic characters, such as "?", ",", spaces, etc.
#'  Defaults to TRUE"
#' 
#' @param simple If TRUE, digraphs such as "ch", "tr", etc. are changed into
#'  special unicode characters instead of combination of graphs. This is
#'  useful in order to operate on individual character strings, for example
#'  for frequency counts or manipulation on a group of characters.
#' 
#'  "t͡ʃ"  changes into "ʧ" U02A7
#' 
#'  "ʈ͡ʂ"  changes into "ŧ" U0167
#' 
#'  "t̪"    changes into "ţ" U0163
#' 
#'  "n̪"    changes into "ņ" U0146
#' 
#'  "l̪"    changes into "ļ" U013C
#' 
#'  Defaults to FALSE
#' 
#' @param output refers to the phonological analysis that should be considered
#'  for the IPA output. All interdental sounds are kept independent of their
#'  status in the original analyses.  Defaults to "Urrea".
#' 
#' "Sadowsky" corresponds to Coastal Mapudungun (Lafkenche) as reported in
#'  Sadowsky, S., Painequeo, J. H., Salamanca, G., & Avelino, H.
#'  (2013). Illustrations of the IPA: Mapudungun. Journal of the
#'  International Phonetic Association, 43(01), 87–96.
#' 
#' "Salas" corresponds to Central Mapudungun as reported in Salas, A.
#'  (1976). Esbozo fonológico del mapuθuŋu, lengua de los mapuče o araucanos
#'  de Chile central. Estudios filológicos, 11, 143–154.
#' 
#' "Smeets" corresponds to Central Mapudungun as reported in Smeets, I.
#'  (2008). A Grammar of Mapuche. Mouton de Gruyter. Interdentals are kept
#' 
#' "Salamanca" corresponds to Pewenche variety as reported in  Salamanca, G.
#'  (1997). Fonología del pehuenche hablado en el Alto Bío Bío. En RLA.
#'  Revista de lingüística teórica y aplicada (Vol. 35, pp. 113–124).
#' 
#' "Urrea" corresponds to Mapudungun spoken in the mountain ranges of the
#'  Araucania Region, as reported in Urrea, P., & Salamanca, G.
#'  (2021). Fonemas segmentales del mapudungun hablado en Icalma y
#'  configuración de un perfil fonético-fonológico del cordón cordillerano de
#'  habla mapuche-pewenche. Logos: Revista de Lingüística, Filosofía y
#'  Literatura, 31(2), 220–236.
#' 
#' @author Aldo Berrios, \email{aldoberrios@@gmail.cl}
#' 
#' @examples
#' 
#' # Change any string into IPA.
#' phonemizer("trafmapuche")
#' > [1] "ʈ͡ʂafmaput͡ʃe"
#' 
#' # Change any string into simple IPA
#' phonemizer("trafmapuche", clean = F, simple = T)
#' > [1] "ʈafmapuʧe"
#' 
#' # Remove typographic characters
#' phonemizer("¿trafmapuche-ngeyu?", clean = T)
#' > [1] "ʈ͡ʂafmaput͡ʃeŋeju"
#' 
#' # Change text in other alphabets:
#' phonemizer(graphemizer("Txafmapuce geyu?"), input="RAG", output="AMU")
#' 
#' @export phonemizer

phonemizer <- function (string, clean = T, simple = F, output = "Urrea") {

  string <- tolower(string)

# Conditions for "clean" which removes non-word characters
# Convert to simple by default
  string <- gsub("ng",    "ŋ", string)
  string <- gsub("t͡ʃ",   "ʧ", string)
  string <- gsub("ʈ͡ʂ",   "ŧ", string)
  string <- gsub("t̪",     "ţ", string)
  string <- gsub("n̪",     "ņ", string)
  string <- gsub("l̪",     "ļ", string)
  string <- gsub("ch",    "ʧ", string)
  string <- gsub("tr",    "ŧ", string)
  string <- gsub("t̯",     "ţ", string)
  string <- gsub("n̯",     "ņ", string)
  string <- gsub("l̯",     "ļ", string)
  string <- gsub("ṯ",     "ţ", string)
  string <- gsub("ṉ",     "ņ", string)
  string <- gsub("ḻ",     "ļ", string)
  string <- gsub("t[’']", "ţ", string)
  string <- gsub("n[’']", "ņ", string)
  string <- gsub("l[’']", "ļ", string)
  string <- gsub("sh",    "ʃ", string)
  string <- gsub("ll",    "ʎ", string)
  string <- gsub("ü",     "ə", string)
  string <- gsub("ñ",     "ɲ", string)
  string <- gsub("d",     "θ", string)
  string <- gsub("g",     "ɣ", string)
  string <- gsub("r",     "ɻ", string)
  string <- gsub("y",     "j", string)

# Eliminate non-word characters
  if (clean == TRUE) {string <- gsub("\\W", "", string)}

  # Revert values into proper IPA symbols
  if (simple == FALSE) {
  string <- gsub("ʧ", "t͡ʃ" ,string)
  string <- gsub("ŧ", "ʈ͡ʂ" ,string)
  string <- gsub("ţ", "t̪"   ,string)
  string <- gsub("ņ", "n̪"   ,string)
  string <- gsub("ļ", "l̪"   ,string)
  }

  if (output == "Sadowsky") {
  string <- gsub("i",    "ɪ", string)
  string <- gsub("e",    "ë", string)
  string <- gsub("ə",    "ɘ", string)
  string <- gsub("a",    "a̝", string)
  string <- gsub("o",    "ö", string)
  string <- gsub("u",    "ʊ", string)
  string <- gsub("ɻ",    "ʐ", string)
   }

  if (output == "Smeets") {
  string <- gsub("ə",    "ɨ", string)
  string <- gsub("ɣ",    "ɣ̞", string)
   }

  if (output == "Salas") {
  string <- gsub("ə",    "ɯ", string)
   }

  if (output == "Salamanca") {
  string <- gsub("ə",    "ɯ", string)
  string <- gsub("f",    "v", string)
  string <- gsub("θ",    "ð", string)
   }

  if (output == "Urrea") {
   }

  return(string)
}

