# Wissenschaftlicher Bericht
# Input: Erstsemesterbefragung_WiSe22.csv
# Output: -
setwd("~/Desktop")

dat <- read.table("Erstsemesterbefragung_WiSe22.csv",
                  header = T, sep = ";",
                  na.strings = c("-9"))

# Löschen der Zeilen die außer der Semesterangabe *nur* NAs/ keine Antworten enthalten
dat <- dat[rowSums(is.na(dat[,2:65])) != (ncol(dat)-1), ]

# Funktion recode(): Variablen rekodieren
recode <- function(vec, alt, neu){
  for(i in seq_along(alt)) vec[vec == alt[i]] <- neu[i]; vec
}

dat$Geschlecht <- recode(dat$Geschlecht, alt = c(1, 2, 3), neu = c("m", "w", "d"))
dat$Studiengang <- recode(dat$Studiengang, alt = c(1, 2, 3, 4),
                          neu = c("ID7", "IW7", "OM7", "WI7"))
