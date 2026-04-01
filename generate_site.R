sheet_url <- "https://docs.google.com/spreadsheets/d/1M8WHvt5UC-RrP9EnlKUXeE2jmh_RnYsY0fIhV_TA3Zo/gviz/tq?tqx=out:csv"
html_path <- "index.html"

fetch_comments <- function(url) {
  comments <- tryCatch(
    suppressWarnings(read.csv(url, check.names = FALSE, stringsAsFactors = FALSE)),
    error = function(e) NULL
  )

  if (is.null(comments)) {
    stop("Could not download comments from the public Google Sheet.")
  }

  comments
}

clean_text <- function(x, fallback = "") {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- trimws(x)
  x[x == ""] <- fallback
  x
}

html_escape <- function(x) {
  htmltools::htmlEscape(x, attribute = FALSE)
}

js_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("'", "\\\\'", x)
  x <- gsub("\n", " ", x, fixed = TRUE)
  x <- gsub("\r", " ", x, fixed = TRUE)
  x
}

rating_to_stars <- function(x) {
  score <- suppressWarnings(as.numeric(x))
  score[is.na(score)] <- 0
  stars <- pmax(1, pmin(5, round(score / 2)))
  vapply(
    stars,
    function(s) paste0(strrep("★", s), strrep("☆", 5 - s)),
    character(1)
  )
}

build_table_rows <- function(data) {
  apply(data, 1, function(row) {
    cells <- paste0("<td>", html_escape(row), "</td>", collapse = "")
    paste0("<tr>", cells, "</tr>")
  })
}

comments <- fetch_comments(sheet_url)

table_data <- data.frame(
  Movie = clean_text(comments[["What was the title of the movie we watched?"]], "Not listed"),
  Rating = paste0(clean_text(comments[["Please rate the movie overall:"]], "Not listed"), "/10"),
  `Favorite Part` = clean_text(comments[["What was your single favorite part or scene?"]], "No comment provided."),
  `Confusing or Disliked Part` = clean_text(
    comments[["Was there anything about the movie you particularly disliked or found confusing?"]],
    "No comment provided."
  ),
  Tone = clean_text(comments[["What was the emotional tone of the movie?"]], "Not listed"),
  `Next Movie Suggestion` = clean_text(
    comments[["Any suggestions for the next movie?"]],
    "No suggestion provided."
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

table_header <- paste0("<th>", html_escape(names(table_data)), "</th>", collapse = "")
table_rows <- paste(build_table_rows(table_data), collapse = "\n")

movie_names <- clean_text(comments[["What was the title of the movie we watched?"]], "Not listed")
movie_scores <- clean_text(comments[["Please rate the movie overall:"]], "Not listed")
numeric_scores <- suppressWarnings(as.numeric(movie_scores))
ratings_df <- data.frame(
  movie = movie_names,
  score = numeric_scores,
  stringsAsFactors = FALSE
)
ratings_df <- ratings_df[!is.na(ratings_df$score) & ratings_df$movie != "Not listed", ]

if (nrow(ratings_df) > 0) {
  rating_summary <- aggregate(score ~ movie, data = ratings_df, FUN = mean)
  rating_summary <- rating_summary[order(-rating_summary$score, rating_summary$movie), ]
  rating_summary <- head(rating_summary, 3)
  rating_list_items <- vapply(
    seq_len(nrow(rating_summary)),
    function(i) {
      movie <- rating_summary$movie[i]
      mean_score <- sprintf("%.1f/10", rating_summary$score[i])
      paste0("<li><strong>", i, ". ", html_escape(movie), "</strong>: ", html_escape(mean_score), "</li>")
    },
    character(1)
  )
  rating_list_html <- paste(rating_list_items, collapse = "\n")
} else {
  rating_list_html <- "<li>No ratings yet.</li>"
}

favorite_quotes <- clean_text(
  comments[["What was your single favorite part or scene?"]],
  "No comment provided."
)
confusing_quotes <- clean_text(
  comments[["Was there anything about the movie you particularly disliked or found confusing?"]],
  "No comment provided."
)

movie_titles <- clean_text(comments[["What was the title of the movie we watched?"]], "Movie Club")
movie_stars <- rating_to_stars(comments[["Please rate the movie overall:"]])

favorite_entries <- paste0(
  "{movie: '", js_escape(movie_titles),
  "', stars: '", js_escape(movie_stars),
  "', prompt: 'favorite part",
  "', quote: '", js_escape(favorite_quotes),
  "', signature: '-- Anonymous'}"
)
favorite_entries <- favorite_entries[
  favorite_quotes != "No comment provided." & nchar(trimws(favorite_quotes)) >= 7
]

confusing_entries <- paste0(
  "{movie: '", js_escape(movie_titles),
  "', stars: '", js_escape(movie_stars),
  "', prompt: 'confusing or disliked part",
  "', quote: '", js_escape(confusing_quotes),
  "', signature: '-- Anonymous'}"
)
confusing_entries <- confusing_entries[
  confusing_quotes != "No comment provided." & nchar(trimws(confusing_quotes)) >= 7
]

quote_entries <- c(favorite_entries, confusing_entries)

selected_quote <- list(
  movie = "Movie Club",
  stars = "",
  prompt = "",
  quote = "No quote yet.",
  signature = ""
)

if (length(quote_entries) > 0) {
  quote_data <- data.frame(
    movie = c(
      movie_titles[favorite_quotes != "No comment provided." & nchar(trimws(favorite_quotes)) >= 7],
      movie_titles[confusing_quotes != "No comment provided." & nchar(trimws(confusing_quotes)) >= 7]
    ),
    stars = c(
      movie_stars[favorite_quotes != "No comment provided." & nchar(trimws(favorite_quotes)) >= 7],
      movie_stars[confusing_quotes != "No comment provided." & nchar(trimws(confusing_quotes)) >= 7]
    ),
    prompt = c(
      rep("favorite part", sum(favorite_quotes != "No comment provided." & nchar(trimws(favorite_quotes)) >= 7)),
      rep("confusing or disliked part", sum(confusing_quotes != "No comment provided." & nchar(trimws(confusing_quotes)) >= 7))
    ),
    quote = c(
      favorite_quotes[favorite_quotes != "No comment provided." & nchar(trimws(favorite_quotes)) >= 7],
      confusing_quotes[confusing_quotes != "No comment provided." & nchar(trimws(confusing_quotes)) >= 7]
    ),
    signature = "-- Anonymous",
    stringsAsFactors = FALSE
  )

  week_index <- as.integer(format(Sys.Date(), "%V"))
  selected_row <- ((week_index - 1) %% nrow(quote_data)) + 1
  selected_quote <- as.list(quote_data[selected_row, ])
}

page <- paste0(
  "<!DOCTYPE html>\n",
  "<html lang='en'>\n",
  "<head>\n",
  "  <meta charset='utf-8'>\n",
  "  <meta name='viewport' content='width=device-width, initial-scale=1'>\n",
  "  <title>Movie Club</title>\n",
  "  <style>\n",
  "    :root {\n",
  "      --bg: #141210;\n",
  "      --paper: #1c1916;\n",
  "      --ink: #efe7da;\n",
  "      --muted: #b9ab98;\n",
  "      --line: #3d352d;\n",
  "      --accent: #d0a36d;\n",
  "      --accent-soft: #6d5846;\n",
  "    }\n",
  "    * { box-sizing: border-box; }\n",
  "    body {\n",
  "      margin: 0;\n",
  "      font-family: 'Charter', 'Iowan Old Style', Georgia, 'Times New Roman', serif;\n",
  "      color: var(--ink);\n",
  "      background: #141210;\n",
  "      min-height: 100vh;\n",
  "    }\n",
  "    main {\n",
  "      max-width: 1160px;\n",
      "      margin: 0 auto;\n",
  "      padding: 52px 24px 72px;\n",
  "      display: grid;\n",
  "      grid-template-columns: minmax(0, 1.25fr) minmax(280px, 0.75fr);\n",
  "      gap: 36px;\n",
  "      align-items: start;\n",
  "    }\n",
  "    section {\n",
  "      background: var(--paper);\n",
  "      border: 1px solid var(--line);\n",
  "      padding: 24px;\n",
  "      margin-bottom: 0;\n",
  "    }\n",
  "    .hero {\n",
  "      grid-column: 1 / -1;\n",
  "    }\n",
  "    .comments-section {\n",
  "      grid-column: 1 / -1;\n",
  "    }\n",
  "    h1, h2 {\n",
  "      margin-top: 0;\n",
  "      font-weight: normal;\n",
  "      line-height: 1.15;\n",
  "    }\n",
  "    .hero-title {\n",
  "      text-align: center;\n",
  "      margin-bottom: 16px;\n",
  "    }\n",
  "    h1 {\n",
  "      font-family: 'Avenir Next Condensed', 'Avenir Next', 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;\n",
  "      font-size: clamp(2.2rem, 5vw, 2.9rem);\n",
  "      font-weight: 650;\n",
  "      text-transform: lowercase;\n",
  "      letter-spacing: 0.04em;\n",
  "      margin-bottom: 10px;\n",
  "    }\n",
  "    .hero-subtitle {\n",
  "      color: var(--muted);\n",
  "      font-size: 1rem;\n",
      "      letter-spacing: 0.02em;\n",
  "      margin: 0;\n",
  "    }\n",
  "    h2 {\n",
  "      font-family: 'Avenir Next', 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;\n",
  "      font-size: 1.05rem;\n",
  "      font-weight: 650;\n",
  "      letter-spacing: 0.05em;\n",
  "      margin-bottom: 16px;\n",
  "      text-transform: lowercase;\n",
  "    }\n",
  "    p, li {\n",
  "      font-size: 1.02rem;\n",
  "      line-height: 1.72;\n",
  "    }\n",
  "    .eyebrow, .note { color: var(--muted); }\n",
  "    .quote-box {\n",
  "      margin: 0 auto 28px;\n",
  "      padding: 0.35rem 0 0.35rem 0.85rem;\n",
  "      border-left: 3px solid var(--accent);\n",
  "      max-width: 640px;\n",
  "      text-align: left;\n",
  "    }\n",
  "    .quote-text {\n",
  "      margin: 0;\n",
  "      font-size: 1.28rem;\n",
  "      font-style: italic;\n",
  "      line-height: 1.6;\n",
  "    }\n",
  "    .quote-movie {\n",
  "      display: block;\n",
  "      font-size: 0.94rem;\n",
  "      color: var(--accent);\n",
  "      text-transform: uppercase;\n",
  "      letter-spacing: 0.12em;\n",
  "      margin-bottom: 8px;\n",
  "    }\n",
  "    .quote-stars {\n",
  "      color: var(--accent);\n",
  "      letter-spacing: 0.2em;\n",
  "      margin-bottom: 14px;\n",
  "    }\n",
  "    .quote-prompt {\n",
  "      margin-bottom: 8px;\n",
  "      color: var(--muted);\n",
  "      font-size: 0.88rem;\n",
  "      letter-spacing: 0.03em;\n",
  "    }\n",
  "    .quote-label {\n",
  "      display: block;\n",
  "      margin-top: 10px;\n",
  "      color: var(--muted);\n",
  "      font-size: 0.88rem;\n",
  "      letter-spacing: 0.04em;\n",
  "    }\n",
  "    .intro {\n",
  "      margin-bottom: 0.8rem;\n",
  "      font-size: clamp(1.18rem, 1.7vw, 1.42rem);\n",
  "      line-height: 1.6;\n",
  "    }\n",
  "    .hero-columns {\n",
  "      display: grid;\n",
  "      grid-template-columns: minmax(0, 1.15fr) minmax(260px, 0.85fr);\n",
  "      gap: 28px;\n",
  "      align-items: start;\n",
  "    }\n",
  "    .hero-copy p:first-child {\n",
  "      margin-top: 0;\n",
  "    }\n",
  "    .hero-copy .note {\n",
  "      font-size: clamp(1rem, 1.2vw, 1.12rem);\n",
  "      line-height: 1.68;\n",
  "    }\n",
  "    .hero-side {\n",
  "      border-top: 1px solid var(--line);\n",
  "      padding-top: 6px;\n",
  "    }\n",
  "    .details {\n",
  "      display: grid;\n",
  "      grid-template-columns: 1fr;\n",
  "      gap: 14px;\n",
  "      margin: 0 0 22px;\n",
  "    }\n",
  "    .detail {\n",
  "      border-top: 1px solid var(--line);\n",
  "      padding-top: 12px;\n",
  "    }\n",
  "    .label {\n",
  "      display: block;\n",
  "      color: var(--muted);\n",
  "      font-size: 0.82rem;\n",
  "      text-transform: uppercase;\n",
  "      letter-spacing: 0.1em;\n",
  "    }\n",
  "    .value {\n",
  "      display: block;\n",
  "      margin-top: 6px;\n",
  "      font-size: 1.06rem;\n",
  "    }\n",
  "    a { color: var(--accent); }\n",
  "    a:hover { color: #6c241b; }\n",
  "    ul {\n",
      "      padding-left: 20px;\n",
  "      margin-bottom: 0;\n",
  "    }\n",
  "    .rating-list {\n",
  "      margin: 0;\n",
  "      padding-left: 20px;\n",
  "    }\n",
  "    .rating-list li {\n",
  "      margin-bottom: 10px;\n",
  "    }\n",
  "    .table-wrap {\n",
  "      overflow-x: auto;\n",
  "    }\n",
  "    table {\n",
  "      width: 100%;\n",
  "      border-collapse: collapse;\n",
  "      min-width: 720px;\n",
  "    }\n",
  "    th, td {\n",
  "      border-bottom: 1px solid var(--line);\n",
  "      padding: 12px 10px;\n",
  "      text-align: left;\n",
  "      vertical-align: top;\n",
  "      font-size: 0.96rem;\n",
  "      line-height: 1.5;\n",
  "    }\n",
  "    th {\n",
  "      background: transparent;\n",
  "      font-weight: normal;\n",
  "      color: var(--muted);\n",
  "    }\n",
  "    tbody tr:hover {\n",
  "      background: rgba(208, 163, 109, 0.06);\n",
  "    }\n",
  "    @media (max-width: 640px) {\n",
  "      main {\n",
  "        padding: 24px 14px 48px;\n",
  "        grid-template-columns: 1fr;\n",
  "        gap: 18px;\n",
  "      }\n",
  "      .hero-columns {\n",
  "        grid-template-columns: 1fr;\n",
  "        gap: 18px;\n",
  "      }\n",
  "      section {\n",
  "        padding: 20px;\n",
  "      }\n",
  "    }\n",
  "  </style>\n",
  "</head>\n",
  "<body>\n",
  "  <main>\n",
  "    <section class='hero'>\n",
  "      <div class='hero-title'>\n",
  "        <h1>Movie Club @ UMD, Ling</h1>\n",
  "        <p class='hero-subtitle'>Some Tuesdays always at 7pm</p>\n",
  "      </div>\n",
  "      <div class='quote-box'>\n",
  "        <span class='quote-movie'>quote of the week: ", html_escape(selected_quote$movie), "</span>\n",
  "        <div class='quote-stars'>", html_escape(selected_quote$stars), "</div>\n",
  "        <div class='quote-prompt'>", if (selected_quote$prompt != "") paste0("from the ", html_escape(selected_quote$prompt), " question") else "", "</div>\n",
  "        <p class='quote-text'>\"", html_escape(selected_quote$quote), "\"</p>\n",
  "        <span class='quote-label'>", html_escape(selected_quote$signature), "</span>\n",
  "      </div>\n",
  "      <div class='hero-columns'>\n",
  "        <div class='hero-copy'>\n",
  "          <p class='intro'>I host movie nights on some Tuesdays. The selection is made by me. There is usually a short post-movie survey. You are free to suggest anything, including movies, and I am free to ignore it.</p>\n",
  "          <p class='note'>This is copied in spirit from Sadie's old UC Santa Cruz movie club, apparently once reserved for Sadie's friends only. Credit where it is due: <a href='https://sadiralewis.github.io/'>Sadie</a>.</p>\n",
  "        </div>\n",
  "        <div class='hero-side'>\n",
  "          <div class='details'>\n",
  "            <div class='detail'>\n",
  "              <span class='label'>When</span>\n",
  "              <span class='value'>Tuesdays at 7:00 PM</span>\n",
  "            </div>\n",
  "            <div class='detail'>\n",
  "              <span class='label'>Address</span>\n",
  "              <span class='value'>5018 Berwyn Rd, College Park, MD 20740</span>\n",
  "            </div>\n",
  "            <div class='detail'>\n",
  "              <span class='label'>Drinks</span>\n",
  "              <span class='value'>BYOB, though I have some booze</span>\n",
  "            </div>\n",
  "          </div>\n",
  "          <p><a href='https://forms.gle/F5SQT92it5Cwmgym7'>RSVP and general interest form</a></p>\n",
  "          <p class='note'>Fill it out even if you are not going to be there but want to be informed. I will stop emailing everyone after this.</p>\n",
  "        </div>\n",
  "      </div>\n",
  "    </section>\n",
  "    <section>\n",
  "      <h2>how it works</h2>\n",
  "      <ul>\n",
  "        <li>You are free to invite anyone along with you.</li>\n",
  "        <li>Eating during the movie is allowed if you know how to eat.</li>\n",
  "        <li>There is a post-movie survey after the screening.</li>\n",
  "      </ul>\n",
  "    </section>\n",
  "    <section>\n",
  "      <h2>scoreboard</h2>\n",
  "      <ul class='rating-list'>\n",
  rating_list_html, "\n",
  "      </ul>\n",
  "    </section>\n",
  "    <section class='comments-section'>\n",
  "      <h2>comments from past screenings</h2>\n",
  "      <div class='table-wrap'>\n",
  "        <table>\n",
  "          <thead>\n",
  "            <tr>", table_header, "</tr>\n",
  "          </thead>\n",
  "          <tbody>\n",
  table_rows, "\n",
  "          </tbody>\n",
  "        </table>\n",
  "      </div>\n",
  "    </section>\n",
  "  </main>\n",
  "</body>\n",
  "</html>\n"
)

writeLines(page, html_path, useBytes = TRUE)
message("Saved site to ", html_path)
