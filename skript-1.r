# Wissenschaftlicher Bericht
# Input: Erstsemesterbefragung_WiSe22.csv
# Output: -
setwd("~/Desktop")

# Einlesen des Datensatzes
dat <- read.table("Erstsemesterbefragung_WiSe22.csv", header = TRUE, sep = ";", na.strings = c("-9"))

# Löschen der Zeilen, in denen keine Antworten gegeben wurden
dat <- dat[rowSums(is.na(dat[, 3:ncol(dat)])) != (ncol(dat) - 2), ]

# Funktion recode() definieren
recode <- function(vec, alt, neu){
  for (i in seq_along(alt)){
    vec[vec == alt[i]] <- neu[i]
  }
  vec
}

# Geschlecht umcodieren
dat$Geschlecht <- recode(dat$Geschlecht, alt = c(1, 2, 3), neu = c("m", "w", "d"))

# Studiengang umcodieren
dat$Studiengang <- recode(dat$Studiengang, alt = c(1, 2, 3, 4), neu = c("ID7", "IW7", "OM7", "WI7"))

# Teilnehmende pro Studiengang nach dem Löschen der Zeilen
table(dat$Studiengang)

# Mittelwert der Spalten zu LIST_Stu bilden
dat$LIST_Stu <- rowMeans(dat[, c("LIST_Stu1", "LIST_Stu2", "LIST_Stu3", "LIST_Stu4")], na.rm = TRUE)

# Hypothese 1: Männer nutzen Lehrveranstaltungen weniger häufig als Frauen als Wissensquelle
h1 <- t.test(dat$LIST_Stu[dat$Geschlecht == "m"], dat$LIST_Stu[dat$Geschlecht == "w"], alternative = "less")
print(h1)

# Hypothese 2: Männer nutzen Gespräche mit Kommiliton:innen weniger häufig als Frauen als Wissensquelle
h2 <- t.test(dat$Quelle_Gespraeche[dat$Geschlecht == "m"], dat$Quelle_Gespraeche[dat$Geschlecht == "w"], alternative = "less")
print(h2)

# Hypothese 3: Männer nutzen Bücher weniger häufig als Frauen als Wissensquelle
h3 <- t.test(dat$Quelle_Buecher[dat$Geschlecht == "m"], dat$Quelle_Buecher[dat$Geschlecht == "w"], alternative = "less")
print(h3)

# Hypothese 4: Unterschied zwischen männlichen und weiblichen Studierenden bei der Nutzungshäufigkeit von Erklärvideos
h4 <- t.test(dat$Quelle_Videos[dat$Geschlecht == "m"], dat$Quelle_Videos[dat$Geschlecht == "w"])
print(h4)
