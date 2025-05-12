# Tema 5: Detekcija zavaravanja GNSS prijamnika provjerom navigacijske poruke u okruženju R

# Zadatak: U programskom okruženju R razviti programsku podršku za učitavanje para
# navigacijskih poruka te provjeru njihove identičnosti.
# U slučaju detekcije razlike (zavaravanja), kreirati tekstualnu datoteku s ispisom 
# razlika između sadržaja dviju datoteka.

# Inicijalizacija radnog prostora
rm(list = ls())

# Postavljanje radnog direktorija
setwd("C:/Users/Lana Milicevic/Projects/gnss-spoofing-detection")
file1 <- file.path("messages", "nav_file1.rnx")
file2 <- file.path("messages", "nav_file2.rnx")

# Parsiranje zaglavlja navigacijske datoteke
parse_nav_header <- function(file_path) {
  lines <- readLines(file_path)
  header_lines <- lines[1:grep("END OF HEADER", lines)]
  return(header_lines)
}

# Normalizacija sadržaja linije uklanjanjem viška razmaka
normalize_line <- function(line) {
  gsub("\\s+", " ", trimws(line))
}

# Parsiranje navigacijskih blokova nakon zaglavlja
parse_nav_file <- function(file_path) {
  lines <- readLines(file_path)
  header_end <- grep("END OF HEADER", lines)
  if (length(header_end) == 0) stop("Završetak zaglavlja nije pronađen (nema 'END OF HEADER').")
  
  data_lines <- lines[(header_end + 1):length(lines)]
  block_size <- 8
  num_blocks <- floor(length(data_lines) / block_size)
  
  nav_blocks <- list()
  for (i in 0:(num_blocks - 1)) {
    block_start <- i * block_size + 1
    block_end <- block_start + block_size - 1
    block <- data_lines[block_start:block_end]
    
    sat_id <- substr(block[1], 1, 3)
    timestamp <- substr(block[1], 4, 43)
    key <- paste(sat_id, timestamp)
    
    nav_blocks[[key]] <- block
  }
  
  return(nav_blocks)
}

# Dohvaćanje opisa navigacijskog parametra na temelju indeksa linije
get_param_name <- function(line_index) {
  param_map <- c(
    "Satelitski sat: pomak, drift, ubrzanje drifta",
    "Orbitalni parametri: sqrt(A), ekscentricitet",
    "Orbitalni parametri: ephemeris vrijeme, srednja anomalija",
    "Orbitalni parametri: argument perigeja, brzina RAAN",
    "Orbitalni parametri: inklinacija, brzina inklinacije",
    "Korekcijski parametri: Cuc, Cus, itd.",
    "Korekcijski parametri: Crc, Crs, itd.",
    "Status satelita: zdravlje, točnost, ostalo"
  )
  return(param_map[line_index])
}

# Provedba usporedbe sadržaja dviju navigacijskih datoteka
compare_nav_data <- function(file1, file2, output_file = "usporedba_razlika.txt") {
  nav1 <- parse_nav_file(file1)
  nav2 <- parse_nav_file(file2)
  
  all_keys <- sort(union(names(nav1), names(nav2)))
  diffs_found <- FALSE
  diff_count <- 0
  con <- file(output_file, "w")
  
  for (key in all_keys) {
    block1 <- nav1[[key]]
    block2 <- nav2[[key]]
    
    # Detekcija izostanka bloka u jednoj od datoteka
    if (is.null(block1) || is.null(block2)) {
      cat("=== Razlika za", key, "===\n", file = con)
      cat("Blok nije pronađen u jednoj od datoteka.\n\n", file = con)
      diffs_found <- TRUE
      diff_count <- diff_count + 1
    } else {
      # Usporedba pojedinačnih linija unutar bloka
      for (i in seq_along(block1)) {
        line1 <- normalize_line(block1[i])
        line2 <- normalize_line(block2[i])
        
        if (line1 != line2) {
          param <- get_param_name(i)
          cat(sprintf("=== Razlika za %s ===\n", key), file = con)
          cat(sprintf("Linija %d - %s\n", i, param), file = con)
          cat("Datoteka 1:", line1, "\n", file = con)
          cat("Datoteka 2:", line2, "\n\n", file = con)
          diffs_found <- TRUE
          diff_count <- diff_count + 1
        }
      }
      
      # Provjera razlike u vremenskoj oznaci unutar bloka
      t1 <- substr(block1[1], 4, 23)
      t2 <- substr(block2[1], 4, 23)
      if (t1 != t2) {
        cat("Razlika u vremenskoj oznaci bloka za", key, "\n\n", file = con)
        diffs_found <- TRUE
      }
    }
  }
  
  close(con)
  
  if (diffs_found) {
    cat(diff_count, "razlika pronađeno. Rezultati su zapisani u", output_file, "\n")
  } else {
    file.remove(output_file)
    cat("Razlike nisu pronađene. Izlazna datoteka nije kreirana.\n")
  }
}

# Poziv funkcije za usporedbu datoteka
compare_nav_data(file1, file2)
