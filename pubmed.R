# Load required packages
if (!requireNamespace("rentrez", quietly = TRUE)) install.packages("rentrez")
if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")

library(rentrez)
library(xml2)

# STEP 1: Define PubMed query with title, MeSH, and abstract exclusions
query <- paste0(
  "(",
  "colchicine[Title] AND (",
  "((\"single-blind method\"[MeSH Terms] OR \"double-blind method\"[MeSH Terms] ",
  "OR \"single blind procedure\"[Title/Abstract]) ",
  "OR (\"cross-over studies\"[MeSH Terms] OR \"crossover procedure\"[Title/Abstract]) ",
  "OR (\"placebo\"[Title/Abstract] OR \"placebo effect\"[MeSH Terms] OR ",
  "\"clinical trials as topic\"[MeSH Terms]) ",
  "OR (\"randomized controlled trial\"[Publication Type]) ",
  "OR (\"clinical trial\"[Publication Type])) ",
  "NOT (\"case reports\"[Publication Type] OR \"letter\"[Publication Type] ",
  "OR \"abstract report\"[Title/Abstract]) ",
  "NOT (\"systematic review\"[Publication Type] OR \"meta-analysis\"[Publication Type]) ",
  "AND \"humans\"[MeSH Terms] ",
  "AND (2020/01/01:3000[PDAT])",
  ")",
  ") NOT (",
  "\"covid\"[Title] OR \"gout\"[Title] OR \"osteo*\"[Title] ",
  "OR covid[MeSH Terms] OR gout[MeSH Terms] ",
  "OR osteoporosis[Abstract] OR osteoarthritis[Abstract] OR osteomyelitis[Abstract]",
  ")"
)

# STEP 2: Run PubMed search with dynamic result count
res_init <- entrez_search(db = "pubmed", term = query, retmax = 0)
total <- res_init$count
cat("Total matching articles:", total, "\n")

res <- entrez_search(db = "pubmed", term = query, retmax = total, sort = "pub date")
pmids <- res$ids
writeLines(pmids, "pubmed_ids.txt")
cat("Saved", length(pmids), "PubMed IDs to pubmed_ids.txt\n")

# STEP 3: Fetch XML for all results
xml_raw <- entrez_fetch(db = "pubmed", id = pmids, rettype = "xml", parsed = FALSE)
xml_doc <- read_xml(xml_raw)

# STEP 4: Parse XML into structured metadata
records <- xml_find_all(xml_doc, ".//PubmedArticle")

extract_text <- function(x, xpath) {
  node <- xml_find_first(x, xpath)
  if (inherits(node, "xml_missing")) return(NA)
  xml_text(node)
}

parse_article <- function(node) {
  pmid     <- extract_text(node, ".//PMID")
  title    <- extract_text(node, ".//ArticleTitle")
  journal  <- extract_text(node, ".//Journal/Title")
  year     <- extract_text(node, ".//PubDate/Year")
  abstract <- paste(xml_text(xml_find_all(node, ".//Abstract/AbstractText")), collapse = " ")
  
  authors <- xml_find_all(node, ".//AuthorList/Author")
  author_names <- sapply(authors, function(author) {
    fname <- xml_text(xml_find_first(author, "ForeName"))
    lname <- xml_text(xml_find_first(author, "LastName"))
    
    if (!nzchar(fname) || !nzchar(lname)) return(NA)
    
    initials <- paste0(substr(unlist(strsplit(fname, " ")), 1, 1), collapse = "")
    paste0(lname, ", ", initials)
  })
  author_str <- paste(na.omit(author_names), collapse = "; ")
  
  list(PMID = pmid, Title = title, Abstract = abstract, Authors = author_str, Journal = journal, Year = year)
}

parsed <- lapply(records, parse_article)
df <- do.call(rbind.data.frame, parsed)

# STEP 5: Save CSV
write.csv(df, "pubmed_citations.csv", row.names = FALSE)
cat("Saved metadata to pubmed_citations.csv\n")

# STEP 6: Format and save RIS
ris_entries <- apply(df, 1, function(row) {
  paste0(
    "TY  - JOUR\n",
    "ID  - ", row["PMID"], "\n",
    "TI  - ", row["Title"], "\n",
    "AU  - ", gsub("; ", "\nAU  - ", row["Authors"]), "\n",
    "JO  - ", row["Journal"], "\n",
    "PY  - ", row["Year"], "\n",
    "AB  - ", row["Abstract"], "\n",
    "ER  -"
  )
})
writeLines(ris_entries, "pubmed_citations.ris")
cat("Saved RIS with formatted authors and abstracts to pubmed_citations.ris\n")
