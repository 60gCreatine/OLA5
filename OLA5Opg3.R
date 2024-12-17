library(readr) # tror ikke denne behøves
data <- read.csv("regnskaber_industri_transport_byg_5_25000_ansatte_anonym.csv", 
         header = TRUE, sep = ";", fileEncoding = "ISO-8859-1")

sum(is.na(data)) # Se antallet af NAs
    # 292481


  #### Opgave 3 – Spørgeskema og Cumulative Link Models ####
    ##### Opgave 3.1 – Illustration af spørgsmålet #####
      # Hent data fra filen ”regnskaber_industri_transport_byg_5_25000_ansatte_anonym” ind i R og lav en grafisk
      # illustration af fordelingen af svarene på spørgsmålet: ”Hvordan ser du mulighederne for at låne penge til din
      # virksomhed?”

# Se mulige svar
unique(data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
# Kan se at der er 2 forskellige: dårlig og dårlige
data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("\\bDårlig\\b","Dårlige",data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
unique(data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)

# Frekvenstabel
data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- factor(
  data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.,
  levels = c("Meget gode", "Gode", "Neutrale", "Dårlige", "Meget dårlige","Ved ikke"))

freq <- as.data.frame(table(data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.),decreasing = T)
colnames(freq) <- c("Svar", "Frekvens")

# ny df uden 'Ved ikke'
df <- subset(df, Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. != "Ved ikke")
# Går fra 4484 til 4433
df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- factor(
  df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.,
  levels = c("Meget gode", "Gode", "Neutrale", "Dårlige", "Meget dårlige"))
df$gruppering <- ifelse(df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. %in% c("Dårlige", "Meget dårlige"), 
                        "Negativ",
                        ifelse(df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. %in% c("Gode", "Meget gode"),
                               "Positiv",
                               "Neutral"))
freq <- as.data.frame(table(
  Svar = df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.,
  Gruppering = df$gruppering
  ))
colnames(freq) <- c("Svar","Gruppering" ,"Frekvens")
freq <- freq[freq$Frekvens > 0, ]
#freq$Svar <- factor(freq$Svar, levels = c("Meget gode", "Gode", "Neutrale", "Dårlige", "Meget dårlige"))


# Plot over frekvens
library(ggplot2)

# Beregn procenter 
freq$Procent <- round((freq$Frekvens / sum(freq$Frekvens)) * 100,1)

# Plot med procentangivelser på y-aksen
ggplot(freq, aes(x = Svar, y = Procent, fill = Gruppering)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "72% af virksomhederne er positivt stemt når det kommer til muligheden om at låne penge",
    x = "Svar",
    y = "Procent (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + # Sæt interval på y-aksen
  scale_fill_manual(
    values = c("Positiv" = "#A3E1CE",   # Farve til Positiv
               "Neutral" = "#FAB958",   # Farve til Neutral
               "Negativ" = "#002E6D")   # Farve til Negativ
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

##### Opgave 3.2 – Cumulative Link Models #####
# Lav en Cumulative Link Model for spørgsmålet ”Hvordan ser du mulighederne for at låne penge til din
# virksomhed?”. I må selv vurdere, hvilke forklarende variable I ønsker i jeres model. Vær opmærksom på, at I
# kan finde inspiration til forklarende variable i artiklerne ”Historisk nemt at låne penge” og ”Gode muligheder
# for at låne penge i hele landet”.



##### Opgave 3.3 – Illustration af forklarende variable #####
# Lav en grafisk illustration af den ene af jeres forklarende variable og spørgsmålet 
# ”Hvordan ser du mulighederne for at låne penge til din virksomhed?”. 
# (hint: se figur på side 6 i artiklen: ”Gode muligheder for at låne penge i hele landet”)



