# Tema 5: Detekcija zavaravanja GNSS prijamnika provjerom navigacijske poruke u okruženju R

# Zadatak: U programskom okruženju R razviti programsku podršku za učitavanje para
# navigacijskih poruka te provjeru njihove identičnosti.
# U slučaju detekcije razlike (zavaravanja), kreirati tekstualnu datoteku s ispisom 
# razlika između sadržaja dviju datoteka.

# Inicijalizacija radnog prostora
rm(list = ls())

# Učitavanje knjižnica
library(shiny)

# Učitavanje svih datoteka navigacijskih poruka iz direktorija "messages"
# Filtrira samo datoteke s relevantnim ekstenzijama
get_nav_files <- function() {
  files <- list.files("messages", pattern = "\\.(14n|17n|rnx|txt)$", full.names = FALSE)
  sort(files)
}

# Funkcija koja normalizira razmake u tekstu za usporedbu
normalize_line <- function(line) {
  gsub("\\s+", " ", trimws(line))
}

# Funkcija za dohvat opisa parametra na temelju indeksa linije unutar bloka
get_param_name <- function(i) {
  c(
    "Pomak satelitskog sata, drift, ubrzanje drifta",
    "Sqrt(A), ekscentricitet",
    "Vrijeme ephemerisa, srednja anomalija",
    "Argument perigeja, brzina RAAN",
    "Inklinacija, brzina inklinacije",
    "Korekcijski parametri: Cuc, Cus itd.",
    "Korekcijski parametri: Crc, Crs itd.",
    "Status satelita: zdravlje, točnost, ostalo"
  )[i]
}

# Parsiranje navigacijske datoteke u blokove nakon zaglavlja
parse_nav_file <- function(file_path) {
  lines <- readLines(file_path)
  header_end <- grep("END OF HEADER", lines)
  if (length(header_end) == 0) stop("Zaglavlje nije pronađeno.")
  
  data_lines <- lines[(header_end + 1):length(lines)]
  block_size <- 8
  num_blocks <- floor(length(data_lines) / block_size)
  
  nav_blocks <- list()
  
  for (i in 0:(num_blocks - 1)) {
    start <- i * block_size + 1
    block <- data_lines[start:(start + 7)]
    
    sat_id <- substr(block[1], 1, 3)              # Identifikator satelita (znakovi 1-3)
    timestamp <- substr(block[1], 4, 43)          # Vremenska oznaka bloka (znakovi 4-43)
    key <- paste(sat_id, timestamp)               # Ključ za pohranu bloka u listu
    
    nav_blocks[[key]] <- block
  }
  
  return(nav_blocks)
}

# Funkcija za usporedbu dviju navigacijskih datoteka i vraćanje sažetka razlika
compare_nav_data_summary <- function(file1, file2) {
  nav1 <- parse_nav_file(file1)
  nav2 <- parse_nav_file(file2)
  
  all_keys <- sort(union(names(nav1), names(nav2)))
  
  removed_blocks <- c()
  added_blocks <- c()
  modified_diffs <- c()
  
  for (key in all_keys) {
    block1 <- nav1[[key]]
    block2 <- nav2[[key]]
    
    if (is.null(block1)) {
      removed_blocks <- c(removed_blocks, paste("Uklonjen blok:", key))
    } else if (is.null(block2)) {
      added_blocks <- c(added_blocks, paste("Dodan blok:", key))
    } else {
      for (i in seq_along(block1)) {
        if (normalize_line(block1[i]) != normalize_line(block2[i])) {
          modified_diffs <- c(modified_diffs, paste("Razlika u bloku", key, "- linija", i, ":", get_param_name(i)))
        }
      }
    }
  }
  
  list(
    uklonjeni = removed_blocks,
    dodani = added_blocks,
    modificirani = modified_diffs
  )
}

# UI definicija Shiny aplikacije
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #sidebarPanel .btn {
        display: block;
        width: 100%;
        margin-bottom: 10px;
      }
      #fullscreen-button {
        position: fixed;
        top: 10px;
        right: 10px;
        z-index: 9999;
      }
    ")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        const requestFullscreen = () => {
          const elem = document.documentElement;
          if (elem.requestFullscreen) {
            elem.requestFullscreen();
          } else if (elem.webkitRequestFullscreen) {
            elem.webkitRequestFullscreen();
          } else if (elem.msRequestFullscreen) {
            elem.msRequestFullscreen();
          }
        };
        
        // Pokušaj fullscreen na prvi klik korisnika
        document.addEventListener('click', function() {
          if(!document.fullscreenElement) {
            requestFullscreen();
          }
        }, { once: true });
      });
    "))
  ),
  
  titlePanel("GNSS Zavaravanje - Usporedba navigacijskih poruka"),
  
  sidebarLayout(
    sidebarPanel(
      id = "sidebarPanel",
      selectInput("file1", "Originalna datoteka:", choices = get_nav_files()),
      selectInput("file2", "Modificirana datoteka:", choices = get_nav_files()),
      actionButton("compare", "Usporedi", class = "btn-primary"),
      br(), br(),
      uiOutput("show_buttons_ui")
    ),
    
    mainPanel(
      verbatimTextOutput("diff_summary"),
      verbatimTextOutput("diff_details")
    )
  )
)

# Server logika aplikacije
server <- function(input, output, session) {
  # Ažuriranje liste dostupnih datoteka
  observe({
    files <- get_nav_files()
    updateSelectInput(session, "file1", choices = files)
    updateSelectInput(session, "file2", choices = files)
  })
  
  diffs <- reactiveValues(data = NULL, view = "")
  
  # Usporedba datoteka na klik gumba
  observeEvent(input$compare, {
    file1_path <- file.path("messages", input$file1)
    file2_path <- file.path("messages", input$file2)
    
    res <- tryCatch({
      compare_nav_data_summary(file1_path, file2_path)
    }, error = function(e) {
      list(error = paste("Greška:", e$message))
    })
    
    diffs$data <- res
    diffs$view <- ""
  })
  
  # Sažetak razlika prikazan korisniku
  output$diff_summary <- renderText({
    data <- diffs$data
    if (is.null(data)) return("Odaberite datoteke za usporedbu i kliknite 'Usporedi' za početak analize.")
    if (!is.null(data$error)) return(data$error)
    
    ukupno_uklonjeni <- length(data$uklonjeni)
    ukupno_dodani <- length(data$dodani)
    ukupno_modificirani <- length(data$modificirani)
    
    paste0(
      "--- SAŽETAK ---\n\n",
      "Uklonjeni blokovi: ", ukupno_uklonjeni, "\n",
      "Dodani blokovi: ", ukupno_dodani, "\n",
      "Modificirani blokovi: ", ukupno_modificirani, "\n\n",
      "Za detalje o određenoj vrsti blokova kliknite na odgovarajući gumb iz izbornika."
    )
  })
  
  # Prikaz gumba za detalje uklonjenih, dodanih i modificiranih blokova
  output$show_buttons_ui <- renderUI({
    if (is.null(diffs$data) || !is.null(diffs$data$error)) return(NULL)
    
    tagList(
      actionButton("show_removed", "Prikaži uklonjene blokove"),
      actionButton("show_added", "Prikaži dodane blokove"),
      actionButton("show_modified", "Prikaži modificirane blokove")
    )
  })
  
  # Upravljanje prikazom detalja po odabiru tipa razlika
  observeEvent(input$show_removed, {
    diffs$view <- "removed"
  })
  
  observeEvent(input$show_added, {
    diffs$view <- "added"
  })
  
  observeEvent(input$show_modified, {
    diffs$view <- "modified"
  })
  
  # Prikaz detalja o razlikama
  output$diff_details <- renderText({
    data <- diffs$data
    if (is.null(data) || !is.null(data$error)) return("")
    
    # Tekstualna objašnjenja svake vrste modifikacije
    theory_text <- switch(diffs$view,
                          removed = paste(
                            "\tUklonjeni blokovi mogu značiti gubitak signala ili jamming - aktivno ometanje GNSS signala,\n",
                            "\tšto rezultira nestankom određenih navigacijskih podataka.\n\n"
                          ),
                          added = paste(
                            "\tDodani blokovi mogu ukazivati na spoofing - ubačene lažne navigacijske informacije\n",
                            "\tkoje služe za zavaravanje GNSS prijemnika i prikaz netočnih podataka.\n\n"
                          ),
                          modified = paste(
                            "\tModificirani blokovi predstavljaju izmjene postojećih podataka, što može biti znak sofisticiranih\n",
                            "\tnapada poput spoofinga ili pokušaja prikrivanja tragova zavaravanja.\n\n"
                          ),
                          ""
    )
    
    # Sadržaj - konkretni ispisi po tipu razlike
    content <- switch(diffs$view,
                      
                      # -- UKLONJENI BLOKOVI --
                      removed = {
                        if (length(data$uklonjeni) == 0) {
                          "Nema uklonjenih blokova."
                        } else {
                          header <- sprintf("%-10s | %-40s\n", "Satelit", "Timestamp")
                          separator <- paste(rep("-", 55), collapse = "")
                          
                          rows <- sapply(data$uklonjeni, function(line) {
                            # Ekstrakcija satelita i vremenske oznake
                            matches <- regmatches(line, regexec("Uklonjen blok:\\s+(\\S+)\\s+(.*)", line))[[1]]
                            if (length(matches) == 3) {
                              sprintf("%-10s | %-40s", matches[2], matches[3])
                            } else {
                              line
                            }
                          })
                          
                          paste(header, paste0(separator, "\n"), paste(rows, collapse = "\n"))
                        }
                      },
                      
                      # -- DODANI BLOKOVI --
                      added = {
                        if (length(data$dodani) == 0) {
                          "Nema dodanih blokova."
                        } else {
                          header <- sprintf("%-10s | %-40s\n", "Satelit", "Timestamp")
                          separator <- paste(rep("-", 55), collapse = "")
                          
                          rows <- sapply(data$dodani, function(line) {
                            matches <- regmatches(line, regexec("Dodan blok:\\s+(\\S+)\\s+(.*)", line))[[1]]
                            if (length(matches) == 3) {
                              sprintf("%-10s | %-40s", matches[2], matches[3])
                            } else {
                              line
                            }
                          })
                          
                          paste(header, paste0(separator, "\n"), paste(rows, collapse = "\n"))
                        }
                      },
                      
                      # -- MODIFICIRANI BLOKOVI --
                      modified = {
                        if (length(data$modificirani) == 0) {
                          "Nema modificiranih blokova."
                        } else {
                          header <- sprintf("%-10s | %-40s | %s\n", "Satelit", "Timestamp", "Parametar")
                          separator <- paste(rep("-", 90), collapse = "")
                          
                          rows <- sapply(data$modificirani, function(line) {
                            # Izvlači sve informacije iz linije: blok, satelit, timestamp, broj linije, ime parametra
                            matches <- regmatches(line, regexec("bloku\\s+(\\S+)\\s+(.*)\\s+-\\s+linija\\s+(\\d+)\\s+:\\s+(.*)", line))[[1]]
                            if (length(matches) == 5) {
                              sprintf("%-10s | %-40s | %s", matches[2], matches[3], matches[5])
                            } else {
                              line
                            }
                          })
                          
                          paste(header, paste0(separator, "\n"), paste(rows, collapse = "\n"))
                        }
                      },
                      
                      "" # default
    )
    
    # Spoji teorijsko objašnjenje i konkretne razlike
    paste0(theory_text, content)
  })
}

# Pokretanje aplikacije
shinyApp(ui, server)

