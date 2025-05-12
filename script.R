# Tema 5: Detekcija zavaravanja GNSS prijamnika provjerom navigacijske poruke u okruženju R

# Zadatak: U programskom okruženju R razviti programsku podršku za učitavanje para
# navigacijskih poruka te provjeru njihove identičnosti.
# U slučaju detekcije razlike (zavaravanja), kreirati tekstualnu datoteku s ispisom 
# razlika između sadržaja dviju datoteka.

rm(list = ls())  # Očisti radni prostor 

# Postavljanje radnog direktorija gdje se nalaze navigacijske datoteke
setwd("C:/Users/Lana Milicevic/Projects/gnss-spoofing-detection")

# Definicija putanja do dviju navigacijskih datoteka u RINEX formatu
# TODO: zamijeniti pravim fileovima
file1 <- file.path("messages", "nav_file1.rnx")
file2 <- file.path("messages", "nav_file2.rnx")

# Funkcija za parsiranje RINEX navigacijske datoteke
parse_nav_file <- function(file_path) {
  lines <- readLines(file_path)  # Učitavanje svih linija iz datoteke
  
  # Pronalazak kraja zaglavlja ("END OF HEADER")
  header_end <- grep("END OF HEADER", lines)
  if (length(header_end) == 0) stop("Zaglavlje nije ispravno: 'END OF HEADER' nije pronađen.")
  
  # Izdvajanje navigacijskih podataka (linije nakon zaglavlja)
  data_lines <- lines[(header_end + 1):length(lines)]
  
  # Grupiranje podataka u blokove od 8 linija (standardno za RINEX 3.04)
  block_size <- 8
  num_blocks <- floor(length(data_lines) / block_size)  # Ukupan broj blokova
  
  nav_blocks <- list()  # Inicijalizacija prazne liste za spremanje blokova
  
  # Parsiranje svakog 8-linijskog bloka i dodavanje u listu
  for (i in 0:(num_blocks - 1)) {
    block_start <- i * block_size + 1
    block_end <- block_start + block_size - 1
    block <- data_lines[block_start:block_end]
    
    # Ekstrakcija ID-a satelita (prva 3 znaka prve linije)
    sat_id <- substr(block[1], 1, 3)
    # Ekstrakcija vremenske oznake satelitske poruke
    timestamp <- substr(block[1], 4, 43)
    key <- paste(sat_id, timestamp)  # Jedinstveni ključ za blok
    
    nav_blocks[[key]] <- block  # Spremanje bloka pod pripadajući ključ
  }
  
  return(nav_blocks)  # Vraća listu svih blokova poruka
}

# Funkcija za usporedbu dviju navigacijskih datoteka
compare_nav_data <- function(file1, file2, output_file = "structured_diff_output.txt") {
  nav1 <- parse_nav_file(file1)  # Parsiranje prve datoteke
  nav2 <- parse_nav_file(file2)  # Parsiranje druge datoteke
  
  all_keys <- union(names(nav1), names(nav2))  # Svi jedinstveni ključevi poruka iz obje datoteke
  diffs_found <- FALSE  # Zastavica za praćenje postojanja razlika
  con <- file(output_file, "w")  # Otvaranje izlazne datoteke za pisanje
  
  # Petlja kroz sve blokove prema ključevima
  for (key in all_keys) {
    block1 <- nav1[[key]]
    block2 <- nav2[[key]]
    
    # Ako neki blok nedostaje ili se blokovi razlikuju
    if (is.null(block1) || is.null(block2)) {
      cat("=== Razlika za", key, "===\n", file = con)
      cat("Blok nedostaje u jednoj od datoteka.\n\n", file = con)
      diffs_found <- TRUE
    } else {
      # Usporedba linija unutar bloka
      for (i in seq_along(block1)) {
        if (block1[i] != block2[i]) {
          cat("=== Razlika za", key, "===\n", file = con)
          cat("Linija", i, "\n", file = con)
          cat("Datoteka 1:", block1[i], "\n", file = con)
          cat("Datoteka 2:", block2[i], "\n\n", file = con)
          diffs_found <- TRUE
        }
      }
    }
  }
  
  close(con)  # Zatvaranje izlazne datoteke
  
  # Ako su razlike pronađene, ispis poruke korisniku
  if (diffs_found) {
    cat("Razlike su pronađene. Izlaz je zapisan u", output_file, "\n")
  } else {
    file.remove(output_file)  # Brisanje datoteke ako nema razlika
    cat("Nema razlika. Izlazna datoteka nije kreirana.\n")
  }
}

# Pokretanje funkcije za usporedbu
compare_nav_data(file1, file2)
