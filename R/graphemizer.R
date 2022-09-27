#' Convert any text in Mapudungun between Mapuche alphabets
#' 
#' This functions changes a text written in any Mapuche alphabet into another
#' alphabet. Mapuche alphabets supported: Alfabeto Mapuche Unificado,
#' Raguileo and Azümchefe.
#' 
#' @param input  Script in which the original text is written. Defaults to AMU
#' 
#' AMU = Alfabeto Mapuche Unificado
#' RAG = Alfabeto Raguileo
#' AZU = Alfabeto Azümchefe
#'  
#' @param output Script in which the output text should be written. Defaults to AZU.
#' 
#' @author Aldo Berrios, \email{aldoberrios@@gmail.cl}
#' 
#' @examples
#' 
#' graphemizer("Tüfachi domo l’awen’tukey tañi küme tremoam.", output = "AZU")
#' > [1] "Tüfachi zomo lhawenhtukey tañi küme txemoam."
#' 
#' graphemizer("Kiñe mapuce wenxu nvniey kiñe caja tañi kuwv mew.", input = "RAG", output = "AZU")
#' > [1] "Kiñe mapuche wentxu nüniey kiñe challa tañi kuwü mew."
#' 
#' # Several dental graphemes are supported in AMU:
#' graphemizer(c("pal’u", "pal'u", "paḻu", "pal̯u"), output = "RAG")
#' > [1] "pabu" "pabu" "pabu" "pabu"
#' 
#' graphemizer(c("pal’u", "pal'u", "paḻu", "pal̯u"), output = "AZU")
#' [1] "palhu" "palhu" "palhu" "palhu"
#' 
#' @export graphemizer


graphemizer <- function (string, input = "AMU", output = "AZU") {

# Changes into Simple IPA: 
# Mid central.............. Ə ə
# Dental plosive........... Ţ ţ
# Postalveolar affricate... Č č
# Retroflex affricate...... Ŧ ŧ
# Dental nasal............. Ņ ņ
# Velar Nasal.............. Ŋ ŋ
# Dental fricative......... Ð ð
# Velar approximate........ Ģ ģ
# Alveolar lateral......... Ĺ ĺ
# Dental lateral........... Ļ ļ
# Palatal lateral.......... Ľ ľ

# Check acceptable arguments for output:
if (output != "AMU" & output != "AZU" & output != "RAG" ) {
stop("Argument ‘output’ only accepts: \"AMU\", \"AZU\" or \"RAG\"")
  }

# Check acceptable arguments for input:
if (input != "AMU" & input != "AZU" & input != "RAG" ) {
stop("Argument ‘input’ only accepts: \"AMU\", \"AZU\" or \"RAG\"")
  }

if (input == "AZU" | input == "AMU") {
    string <- gsub("Ü",            "Ə",    string) # Schwa - Uppcase
    string <- gsub("ü",            "ə",    string) # Schwa - Lowcase
    string <- gsub("CH([AEIOUƏ])", "Č\\1", string) # AfrAlv - AllCase
    string <- gsub("Ch",           "Č",    string) # AfrAlv - UppCase
    string <- gsub("ch",           "č",    string) # AfrAlv - LowCase
    string <- gsub("SH",           "Ś",    string) # AfrAlv - MixCase !Nuevo
    string <- gsub("SH([AEIOUƏ])", "Ś\\1", string) # FriAlP - AllCase !Nuevo
    string <- gsub("Sh",           "Ś",    string) # FriAlP - UppCase !Nuevo
    string <- gsub("sh",           "ś",    string) # FriAlP - LowCase !Nuevo
    string <- gsub("SH",           "Ś",    string) # FriAlP - MixCase !Nuevo
    string <- gsub("LL-LL",        "ĽĽ",   string) # Lpal-Lpal - UppCase
    string <- gsub("LL-L",         "ĽĹ",   string) # Lpal-Lalv - UppCase
    string <- gsub("L-LL",         "ĹĽ",   string) # Lalv-Lpal - UppCase
    string <- gsub("L-L",          "ĹĹ",   string) # Lalv-Lalv - UppCase
    string <- gsub("ll-l",         "ľĺ",   string) # Lpal-Lalv - LowCase
    string <- gsub("l-ll",         "ĺľ",   string) # Lalv-Lpal - LowCase
    string <- gsub("ll-ll",        "ľľ",   string) # Lpal-Lpal - LowCase
    string <- gsub("l-l",          "ĺĺ",   string) # Lalv-Lalv - LowCase
    string <- gsub("LL([AEIOUƏ])", "Ľ\\1", string) # VelApr - AllCase
    string <- gsub("LL\\b",        "Ľ",    string) # VelApr - Endword
    string <- gsub("Ll",           "Ľ",    string) # VelApr - UppCase
    string <- gsub("ll",           "ľ",    string) # velApr - LowCase
    string <- gsub("LL",           "Ľ",    string) # velApr - MixCase

}

if (input == "AMU") {
    string <- gsub("TR([AEIOUƏ])", "Ŧ\\1", string) # AlvAfr - AllCase
    string <- gsub("Tr",           "Ŧ",    string) # AlvAfr - UppCase
    string <- gsub("tr",           "ŧ",    string) # AlvAfr - LowCase
    string <- gsub("TR",           "Ŧ",    string) # AlvAfr - MixCase
    string <- gsub("D([AEIOUƏ])",  "Ð\\1", string) # DenFri - AllCase
    string <- gsub("D",            "Ð",    string) # DenFri - UppCase
    string <- gsub("d",            "ð",    string) # DenFri - LowCase
    string <- gsub("NG([AEIOUƏ])", "Ŋ\\1", string) # VelNas - AllCase
    string <- gsub("NG\\b",        "Ŋ",    string) # VelApr - Endword
    string <- gsub("Ng",           "Ŋ",    string) # VelNas - UppCase
    string <- gsub("ng",           "ŋ",    string) # velNas - LowCase
    string <- gsub("NG",           "Ŋ",    string) # velNas - MixCase
    string <- gsub("G([AEIOUƏ])",  "Ģ\\1", string) # VelApr - AllCase
    string <- gsub("G",            "Ģ",    string) # VelApr - UppCase
    string <- gsub("g",            "ģ",    string) # velApr - LowCase

# All other dentals
    string <- gsub("T[’']([AEIOUƏ])",     "Ţ\\1", string) # DenPlo - AllCase
    string <- gsub("T[’']",               "Ţ",    string) # DenPlo - UppCase
    string <- gsub("t[’']",               "ţ",    string) # DenPlo - Lowcase
    string <- gsub("T[\u032a]([AEIOUƏ])", "Ţ\\1", string) # DenPlo - Uppcase - IPA (Subscript bridge)
    string <- gsub("T[\u032a]",           "Ţ",    string) # DenPlo - Uppcase - IPA (Subscript bridge)
    string <- gsub("t[\u032a]",           "ţ",    string) # DenPlo - Lowcase - IPA (Subscript bridge)
    string <- gsub("T[\u032F]([AEIOUƏ])", "Ţ\\1", string) # DenPlo - Uppcase - Subscript arch
    string <- gsub("T[\u032F]",           "Ţ",    string) # DenPlo - Uppcase - Subscript arch
    string <- gsub("t[\u032F]",           "ţ",    string) # DenPlo - Lowcase - Subscript arch
    string <- gsub("N[’']([AEIOUƏ])",     "Ņ\\1", string) # DenNas - AllCase
    string <- gsub("N[’']",               "Ņ",    string) # DenNas - UppCase
    string <- gsub("n[’']",               "ņ",    string) # DenNas - Lowcase
    string <- gsub("N[\u032a]([AEIOUƏ])", "Ņ\\1", string) # DenNas - Uppcase - IPA (Subscript bridge)
    string <- gsub("N[\u032a]",           "Ņ",    string) # DenNas - Uppcase - IPA (Subscript bridge)
    string <- gsub("n[\u032a]",           "ņ",    string) # DenNas - Lowcase - IPA (Subscript bridge)
    string <- gsub("N[\u032F]([AEIOUƏ])", "Ņ\\1", string) # DenNas - Uppcase - Subscript arch
    string <- gsub("N[\u032F]",           "Ņ",    string) # DenNas - Uppcase - Subscript arch
    string <- gsub("n[\u032F]",           "ņ",    string) # DenNas - Lowcase - Subscript arch
    string <- gsub("L[’']([AEIOUƏ])",     "Ļ\\1", string) # DenLat - AllCase
    string <- gsub("L[’']",               "Ļ",    string) # DenLat - UppCase
    string <- gsub("l[’']",               "ļ",    string) # DenLat - Lowcase
    string <- gsub("L[\u032a]([AEIOUƏ])", "Ļ\\1", string) # DenLat - Uppcase - IPA (Subscript bridge)
    string <- gsub("L[\u032a]",           "Ļ",    string) # DenLat - Uppcase - IPA (Subscript bridge)
    string <- gsub("l[\u032a]",           "ļ",    string) # DenLat - Lowcase - IPA (Subscript bridge)
    string <- gsub("L[\u032F]([AEIOUƏ])", "Ļ\\1", string) # DenLat - Uppcase - Subscript arch
    string <- gsub("L[\u032F]",           "Ļ",    string) # DenLat - Uppcase - Subscript arch
    string <- gsub("l[\u032F]",           "ļ",    string) # DenLat - Lowcase - Subscript arch
    string <- gsub("Ṯ([AEIOUƏ])",         "Ţ\\1", string) # DenPlo - Allcase - Underbar
    string <- gsub("Ṉ([AEIOUƏ])",         "Ņ\\1", string) # DenNas - Allcase - Underbar
    string <- gsub("Ḻ([AEIOUƏ])",         "Ļ\\1", string) # DenLat - Allcase - Underbar
    string <- gsub("Ṯ",                   "Ţ",    string) # DenPlo - Uppcase - Underbar
    string <- gsub("Ṉ",                   "Ņ",    string) # DenNas - Uppcase - Underbar
    string <- gsub("Ḻ",                   "Ļ",    string) # DenLat - Uppcase - Underbar
    string <- gsub("ṯ",                   "ţ",    string) # DenPlo - Lowcase - Underbar
    string <- gsub("ṉ",                   "ņ",    string) # DenNas - Lowcase - Underbar
    string <- gsub("ḻ",                   "ļ",    string) # DenLat - Lowcase - Underbar
}

if (input == "AZU") {
    string <- gsub("TX([AEIOUƏ])", "Ŧ\\1", string) # AlvAfr - AllCase
    string <- gsub("Tx",           "Ŧ",    string) # AlvAfr - UppCase
    string <- gsub("tx",           "ŧ",    string) # AlvAfr - LowCase
    string <- gsub("TX",           "Ŧ",    string) # AlvAfr - MixCase
    string <- gsub("Z([AEIOUƏ])",  "Ð\\1", string) # DenAfr - AllCase
    string <- gsub("Z",            "Ð",    string) # DenAfr - UppCase
    string <- gsub("z",            "ð",    string) # DenAfr - LowCase
    string <- gsub("G([AEIOUƏ])",  "Ŋ\\1", string) # VelNas - AllCase
    string <- gsub("G",            "Ŋ",    string) # VelNas - UppCase
    string <- gsub("g",            "ŋ",    string) # velNas - LowCase
    string <- gsub("Q([AEIOUƏ])",  "Ģ\\1", string) # VelApr - AllCase
    string <- gsub("Q",            "Ģ",    string) # VelApr - UppCase
    string <- gsub("q",            "ģ",    string) # VelApr - LowCase
    string <- gsub("TH([AEIOUƏ])", "Ţ\\1", string) # DenPlo - AllCase
    string <- gsub("Th",           "Ţ",    string) # DenPlo - UppCase
    string <- gsub("th",           "ţ",    string) # DenPlo - Lowcase
    string <- gsub("TH",           "Ţ",    string) # DenPlo - MixCase
    string <- gsub("NH([AEIOUƏ])", "Ņ\\1", string) # DenNas - AllCase
    string <- gsub("NH\\b",        "Ņ",    string) # VelApr - Endword
    string <- gsub("Nh",           "Ņ",    string) # DenNas - UppCase
    string <- gsub("nh",           "ņ",    string) # DenNas - Lowcase
    string <- gsub("NH",           "Ņ",    string) # DenNas - MixCase
    string <- gsub("LH([AEIOUƏ])", "Ļ\\1", string) # DenLat - AllCase
    string <- gsub("LH\\b",        "Ļ",    string) # VelApr - Endword
    string <- gsub("Lh",           "Ļ",    string) # DenLat - UppCase
    string <- gsub("lh",           "ļ",    string) # DenLat - Lowcase
    string <- gsub("LH",           "Ļ",    string) # DenLat - MixCase
}

if (input == "RAG") {
    string <- gsub("V",           "Ə",    string) # Schwa - Uppcase
    string <- gsub("v",           "ə",    string) # Schwa - Lowcase
    string <- gsub("C([AEIOUƏ])", "Č\\1", string) # AlvAfr - AllCase
    string <- gsub("X([AEIOUƏ])", "Ŧ\\1", string) # AlvAfr - AllCase
    string <- gsub("C",           "Č",    string) # AlvAfr - UppCase
    string <- gsub("X",           "Ŧ",    string) # AlvAfr - UppCase
    string <- gsub("c",           "č",    string) # AlvAfr - LowCase
    string <- gsub("x",           "ŧ",    string) # AlvAfr - LowCase
    string <- gsub("Z([AEIOUƏ])", "Ð\\1", string) # DenAfr - AllCase
    string <- gsub("Z",           "Ð",    string) # DenAfr - UppCase
    string <- gsub("z",           "ð",    string) # DenAfr - LowCase
    string <- gsub("G([AEIOUƏ])", "Ŋ\\1", string) # VelNas - AllCase
    string <- gsub("G",           "Ŋ",    string) # VelNas - UppCase
    string <- gsub("g",           "ŋ",    string) # velNas - LowCase
    string <- gsub("Q([AEIOUƏ])", "Ģ\\1", string) # VelApr - AllCase
    string <- gsub("Q",           "Ģ",    string) # VelApr - UppCase
    string <- gsub("q",           "ģ",    string) # velApr - LowCase
    string <- gsub("JL",          "ĽĹ",   string) # PaL-AlL - UppCase
    string <- gsub("LJ",          "ĹĽ",   string) # AlL-PaL - UppCase
    string <- gsub("JJ",          "ĽĽ",   string) # PaL-PaL - UppCase
    string <- gsub("LL",          "ĹĹ",   string) # AlL-AlL - UppCase
    string <- gsub("jl",          "ľĺ",   string) # PaL-AlL - LowCase
    string <- gsub("lj",          "ĺľ",   string) # AlL-PaL - LowCase
    string <- gsub("jj",          "ľľ",   string) # PaL-PaL - LowCase
    string <- gsub("ll",          "ĺĺ",   string) # AlL-AlL - LowCase
    string <- gsub("J([AEIOUƏ])", "Ľ\\1", string) # PalLat - AllCase
    string <- gsub("J",           "Ľ",    string) # PalLat - UppCase
    string <- gsub("j",           "ľ",    string) # PalLat - LowCase
    string <- gsub("H([AEIOUƏ])", "Ņ\\1", string) # DenNas - AllCase
    string <- gsub("H",           "Ņ",    string) # DenNas - UppCase
    string <- gsub("h",           "ņ",    string) # DenNas - Lowcase
    string <- gsub("B([AEIOUƏ])", "Ļ\\1", string) # DenLat - AllCase
    string <- gsub("B",           "Ļ",    string) # DenLat - UppCase
    string <- gsub("b",           "ļ",    string) # DenLat - Lowcase
}

  if (output == "AMU" | output == "AZU") {
    string <- gsub("Ə",           "Ü",     string) # Schwa - Uppcase
    string <- gsub("ə",           "ü",     string) # Schwa - Lowcase
    string <- gsub("Č([AEIOUÜ])", "CH\\1", string) # AlvAfr - AllCase
    string <- gsub("Č",           "Ch",    string) # AlvAfr - UppCase
    string <- gsub("č",           "ch",    string) # AlvAfr - LowCase
    string <- gsub("Ś([AEIOUÜ])", "SH\\1", string) # FriAlv - AllCase !Nuevo
    string <- gsub("Ś",           "Sh",    string) # FriAlP - UppCase !Nuevo
    string <- gsub("ś",           "sh",    string) # FriAlP - LowCase !Nuevo
    string <- gsub("ĽĹ",          "LL-L",  string) # PaL-AlL - UppCase
    string <- gsub("ĹĽ",          "L-LL",  string) # AlL-PaL - UppCase
    string <- gsub("ĽĽ",          "LL-LL", string) # PaL-PaL - UppCase
    string <- gsub("ĹĹ",          "L-L",   string) # AlL-AlL - UppCase
    string <- gsub("ľĺ",          "ll-l",  string) # PaL-AlL - LowCase
    string <- gsub("ĺľ",          "l-ll",  string) # AlL-PaL - LowCase
    string <- gsub("ľľ",          "ll-ll", string) # PaL-PaL - LowCase
    string <- gsub("ĺĺ",          "l-l",   string) # AlL-AlL - LowCase
    string <- gsub("Ľ([AEIOUÜ])", "LL\\1", string) # PalLat - AllCase
    string <- gsub("Ľ\\b",        "LL",    string) # PalLat - Endword
    string <- gsub("Ľ",           "Ll",    string) # PalLat - UppCase
    string <- gsub("ľ",           "ll",    string) # PalLat - LowCase
}

  if (output == "AMU") {
    string <- gsub("Ŧ([AEIOUÜ])", "TR\\1", string) # AlvAfr - AllCase
    string <- gsub("Ŧ",           "Tr",    string) # AlvAfr - UppCase
    string <- gsub("ŧ",           "tr",    string) # AlvAfr - LowCase
    string <- gsub("Ð([AEIOUÜ])", "D\\1",  string) # DenAfr - AllCase
    string <- gsub("Ð",           "D",     string) # DenAfr - UppCase
    string <- gsub("ð",           "d",     string) # DenAfr - LowCase
    string <- gsub("Ŋ([AEIOUÜ])", "NG\\1", string) # VelNas - AllCase
    string <- gsub("Ŋ\\b",        "NG",    string) # VelNas - Endword
    string <- gsub("Ŋ",           "Ng",    string) # VelNas - UppCase
    string <- gsub("ŋ",           "ng",    string) # VelNas - LowCase
    string <- gsub("Ģ([AEIOUÜ])", "G\\1",  string) # VelApr - AllCase
    string <- gsub("Ģ",           "G",     string) # VelApr - UppCase
    string <- gsub("ģ",           "g",     string) # VelApr - LowCase
    string <- gsub("Ţ([AEIOUÜ])", "T’\\1", string) # DenPlo - AllCase
    string <- gsub("Ţ",           "T’",    string) # DenPlo - UppCase
    string <- gsub("ţ",           "t’",    string) # DenPlo - Lowcase
    string <- gsub("Ņ([AEIOUÜ])", "N’\\1", string) # DenNas - AllCase
    string <- gsub("Ņ\\b",        "N’",    string) # DenNas - Endword
    string <- gsub("Ņ",           "N’",    string) # DenNas - UppCase
    string <- gsub("ņ",           "n’",    string) # DenNas - Lowcase
    string <- gsub("Ļ([AEIOUÜ])", "L’\\1", string) # DenLat - AllCase
    string <- gsub("Ļ\\b",        "L’",    string) # DenLat - Endword
    string <- gsub("Ļ",           "L’",    string) # DenLat - UppCase
    string <- gsub("ļ",           "l’",    string) # DenLat - Lowcase
  }

  if (output == "AZU") {

    string <- gsub("Ŧ([AEIOUÜ])",  "TX\\1", string) # AlvAfr - AllCase
    string <- gsub("Ŧ",            "Tx",    string) # AlvAfr - UppCase
    string <- gsub("ŧ",            "tx",    string) # AlvAfr - LowCase
    string <- gsub("Ð([AEIOUÜ])",  "Z\\1",  string) # DenAfr - AllCase
    string <- gsub("Ð",            "Z",     string) # DenAfr - UppCase
    string <- gsub("ð",            "z",     string) # DenAfr - LowCase
    string <- gsub("Ŋ([AEIOUÜ])",  "G\\1",  string) # VelNas - AllCase
    string <- gsub("Ŋ",            "G",     string) # VelNas - UppCase
    string <- gsub("ŋ",            "g",     string) # VelNas - LowCase
    string <- gsub("Ģ([AEIOUÜ])",  "Q\\1",  string) # VelApr - AllCase
    string <- gsub("Ģ",            "Q",     string) # VelApr - UppCase
    string <- gsub("ģ",            "q",     string) # velApr - LowCase
    string <- gsub("Ţ([AEIOUÜ])",  "TH\\1", string) # PloDen - AllCase
    string <- gsub("Ţ",            "Th",    string) # PloDen - UppCase
    string <- gsub("ţ",            "th",    string) # PloDen - Lowcase
    string <- gsub("Ņ([AEIOUÜ])",  "NH\\1", string) # NasDen - AllCase
    string <- gsub("Ņ\\b",         "NH",    string) # VelApr - Endword
    string <- gsub("Ņ",            "Nh",    string) # NasDen - UppCase
    string <- gsub("ņ",            "nh",    string) # NasDen - Lowcase
    string <- gsub("Ļ([AEIOUÜ])",  "LH\\1", string) # LatDen - AllCase
    string <- gsub("Ļ\\b",         "LH",    string) # VelApr - Endword
    string <- gsub("Ļ",            "Lh",    string) # LatDen - UppCase
    string <- gsub("ļ",            "lh",    string) # LatDen - Lowcase
  }

  if (output == "RAG") {
    string <- gsub("Ə",           "V",    string) # Schwa  - Uppcase
    string <- gsub("ə",           "v",    string) # Schwa  - Lowcase
    string <- gsub("Č([AEIOUV])", "C\\1", string) # AfrAlv - AllCase
    string <- gsub("Č",           "C",    string) # AfrAlv - UppCase
    string <- gsub("č",           "c",    string) # AfrAlv - LowCase
    string <- gsub("Ŧ([AEIOUV])", "X\\1", string) # AfrRet - AllCase
    string <- gsub("Ŧ",           "X",    string) # AfrRet - UppCase
    string <- gsub("ŧ",           "x",    string) # AfrRet - LowCase
    string <- gsub("Ś([AEIOUV])", "S\\1", string) # AlvAfr - AllCase !Nuevo
    string <- gsub("Ś",           "S",    string) # AlvAfr - UppCase !Nuevo
    string <- gsub("ś",           "s",    string) # AlvAfr - LowCase !Nuevo
    string <- gsub("Ð([AEIOUV])", "Z\\1", string) # AfrDen - AllCase
    string <- gsub("Ð",           "Z",    string) # AfrDen - UppCase
    string <- gsub("ð",           "z",    string) # AfrDen - LowCase
    string <- gsub("Ŋ([AEIOUV])", "G\\1", string) # NasVel - AllCase
    string <- gsub("Ŋ",           "G",    string) # NasVel - UppCase
    string <- gsub("ŋ",           "g",    string) # Nasvel - LowCase
    string <- gsub("Ģ([AEIOUV])", "Q\\1", string) # AprVel - AllCase
    string <- gsub("Ģ",           "Q",    string) # AprVel - UppCase
    string <- gsub("ģ",           "q",    string) # Aprvel - LowCase
    string <- gsub("ĽĹ",          "JL",   string) # PaL-AlL - UppCase
    string <- gsub("ĹĽ",          "LJ",   string) # AlL-PaL - UppCase
    string <- gsub("ĽĽ",          "JJ",   string) # PaL-PaL - UppCase
    string <- gsub("ĹĹ",          "LL",   string) # AlL-AlL - UppCase
    string <- gsub("ľĺ",          "jl",   string) # PaL-AlL - LowCase
    string <- gsub("ĺľ",          "lj",   string) # AlL-PaL - LowCase
    string <- gsub("ľľ",          "jj",   string) # PaL-PaL - LowCase
    string <- gsub("ĺĺ",          "ll",   string) # AlL-AlL - LowCase
    string <- gsub("Ľ([AEIOUV])", "J\\1", string) # PalLat - AllCase
    string <- gsub("Ľ",           "J",    string) # PalLat - UppCase
    string <- gsub("ľ",           "j",    string) # PalLat - LowCase
    string <- gsub("Ţ([AEIOUV])", "T\\1", string) # DenPlo - AllCase
    string <- gsub("Ţ",           "T",    string) # DenPlo - UppCase
    string <- gsub("ţ",           "t",    string) # DenPlo - Lowcase
    string <- gsub("Ņ([AEIOUV])", "H\\1", string) # DenNas - AllCase
    string <- gsub("Ņ",           "H",    string) # DenNas - UppCase
    string <- gsub("ņ",           "h",    string) # DenNas - Lowcase
    string <- gsub("Ļ([AEIOUV])", "B\\1", string) # DenLat - AllCase
    string <- gsub("Ļ",           "B",    string) # DenLat - UppCase
    string <- gsub("ļ",           "b",    string) # DenLat - Lowcase
  }

  return(string)

}
