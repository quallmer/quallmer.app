#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @import shiny
#' @import bslib
#' @importFrom irr kripp.alpha kappam.fleiss kappa2
#' @importFrom stats na.omit

# -------------------------------
# Helpers
# -------------------------------

#' @noRd
na_to_empty <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

#' @noRd
read_data_file <- function(path, name) {
  if (grepl("\\.rds$", name, ignore.case = TRUE)) {
    readRDS(path)
  } else if (grepl("\\.csv$", name, ignore.case = TRUE)) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    cli::cli_abort("Unsupported file type. Please select a {.file .rds} or {.file .csv} file.")
  }
}

#' @noRd
preview_head <- function(df, n = 10) utils::head(df, n)

#' @noRd
escape_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

#' @noRd
highlight_query <- function(text, query) {
  if (is.null(query) || !nzchar(query)) {
    return(htmltools::HTML(htmltools::htmlEscape(text)))
  }
  esc  <- escape_regex(query)
  safe <- htmltools::htmlEscape(text)
  marked <- gsub(paste0("(", esc, ")"), "<mark>\\1</mark>", safe,
                 ignore.case = TRUE, perl = TRUE)
  htmltools::HTML(marked)
}

#' @noRd
prepare_comparison_data <- function(df, unit_id_col, coder_cols, code_var_name = "code") {
  # Transform wide-format data (one column per coder) into list of data frames
  # suitable for qlm_compare()
  lapply(stats::setNames(coder_cols, coder_cols), function(coder_name) {
    data.frame(
      .id = df[[unit_id_col]],
      code = df[[coder_name]],
      stringsAsFactors = FALSE
    )
  })
}

# -------------------------------
# Human check module (UI + server)
# -------------------------------

#' @noRd
humancheck_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        :root { --max-panel-height: 900px; --bluish: #5A6F8F; }
        #sidebar-box, #main-text-box {
          overflow-y: auto;
          max-height: var(--max-panel-height);
          padding-right: 10px;
        }
        .btn-primary {
          background-color: var(--bluish) !important;
          border-color: var(--bluish) !important;
        }
        .btn-secondary {
          background-color: #6c757d !important;
          border-color: #6c757d !important;
          color: #fff !important;
        }
        #main-text {
          font-size: 0.9rem;
          background: #f8f9fa;
          border-radius: 8px;
          padding: 10px;
        }
        .small-box {
          max-height: 180px;
          overflow-y: auto;
          background: #fff;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 6px;
          margin-bottom: 6px;
        }
        .text-box {
          max-height: 280px;
          overflow-y: auto;
          background: #fff;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 8px;
          margin-bottom: 8px;
          font-size: 0.85rem;
          white-space: pre-wrap;
          word-wrap: break-word;
        }
        .score-box {
          max-height: 80px;
          overflow-y: auto;
          background: #f8f9fa;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 4px 8px;
          margin-bottom: 6px;
          font-weight: 600;
        }
        .llm-section {
          margin-bottom: 12px;
          padding-bottom: 8px;
          border-bottom: 1px solid #eee;
        }
        .llm-section:last-child {
          border-bottom: none;
        }
        .llm-label {
          font-size: 0.75rem;
          color: #6c757d;
          margin-bottom: 2px;
        }
        .progress-bar {
          height: 5px;
          background: var(--bluish);
          border-radius: 3px;
          margin-bottom: 6px;
        }
        mark {
          background-color: #ffea70;
          padding: 0 2px;
        }
      ")),
      # Arrow keys + Enter to search-next (with debounce to prevent stuck scrolling)
      tags$script(HTML(sprintf("
        (function() {
          var lastKeyTime = 0;
          var debounceMs = 150;
          document.addEventListener('keydown', function(e) {
            var now = Date.now();
            if (now - lastKeyTime < debounceMs) return;
            if (document.activeElement.tagName === 'INPUT' ||
                document.activeElement.tagName === 'TEXTAREA') return;
            lastKeyTime = now;
            if (e.key === 'ArrowRight') Shiny.setInputValue('%s', Math.random());
            if (e.key === 'ArrowLeft') Shiny.setInputValue('%s', Math.random());
          });
        })();
        $(document).on('keydown', '#%s', function(e){
          if (e.key === 'Enter') {
            e.preventDefault();
            Shiny.setInputValue('%s', Math.random());
          }
        });
      ", ns("next_text"), ns("prev_text"), ns("search"), ns("search_next"))))
    ),
    fluidRow(
      column(
        width = 4,
        div(
          id = "sidebar-box",
          uiOutput(ns("meta_ui")),
          hr(),
          uiOutput(ns("progress_ui")),
          fluidRow(
            column(
              4,
              actionButton(ns("prev_text"), "<- Previous",
                           class = "btn btn-light w-100")
            ),
            column(
              4,
              actionButton(ns("next_text"), "Next ->",
                           class = "btn btn-light w-100")
            ),
            column(
              4,
              actionButton(ns("manual_save"), "Save Now",
                           class = "btn btn-secondary w-100")
            )
          ),
          hr(),
          uiOutput(ns("llm_side")),     # LLM output / justification (LLM mode)
          uiOutput(ns("mode_fields")),  # Score (manual) or Status (LLM)
          textAreaInput(ns("comments"), "Comments", "",
                        rows = 3, placeholder = "Type your notes..."),
          textAreaInput(ns("examples"), "Examples (optional)", "",
                        rows = 3, placeholder = "Paste or type examples..."),
          verbatimTextOutput(ns("status_msg"))
        )
      ),
      column(
        width = 8,
        div(
          id = "main-text-box",
          # Search controls in main panel
          div(
            class = "d-flex gap-2 align-items-end",
            div(
              style = "flex:1;",
              textInput(ns("search"), "Search in text", value = "",
                        placeholder = "Type keyword...")
            ),
            actionButton(ns("search_prev"), "Find Prev", class = "btn btn-light"),
            actionButton(ns("search_next"), "Find Next", class = "btn btn-light")
          ),
          br(),
          uiOutput(ns("text_ui"))
        )
      )
    )
  )
}

#' @noRd
humancheck_server <- function(
    id,
    data,
    text_col,
    blind = reactive(TRUE),
    llm_output_cols = reactive(NULL),    # vector of LLM output column names
    llm_evidence_cols = reactive(NULL),  # vector of justification column names
    llm_score_cols = reactive(NULL),     # vector of score column names
    original_file_name = reactive("data.csv"),
    meta_cols = reactive(character())
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(df = NULL, n = 0L, text_vec = NULL)
    current_index <- reactiveVal(1L)

    assessed_path <- reactive({
      base_path <- req(original_file_name())
      out_dir   <- dirname(base_path) # always the quallmer_coding folder
      file.path(
        out_dir,
        paste0(
          tools::file_path_sans_ext(basename(base_path)),
          "_assessed.rds"
        )
      )
    })

    # Atomic save of full dataframe + last_index
    save_now <- function() {
      if (is.null(rv$df)) return(invisible(NULL))
      sp <- tryCatch(assessed_path(), error = function(e) NULL)
      if (is.null(sp) || !nzchar(sp)) return(invisible(NULL))
      dir.create(dirname(sp), recursive = TRUE, showWarnings = FALSE)
      tmp <- tempfile("assessed_", tmpdir = dirname(sp), fileext = ".rds")
      payload <- list(
        version    = 4L,
        last_index = current_index(),
        df         = rv$df
      )
      ok <- tryCatch({
        saveRDS(payload, tmp, compress = "gzip")  # gzip is much faster than xz
        if (!file.rename(tmp, sp)) {
          if (file.copy(tmp, sp, overwrite = TRUE)) {
            unlink(tmp)
          } else {
            cli::cli_abort("File copy operation failed.")
          }
        }
        TRUE
      }, error = function(e) FALSE)
      if (!ok && file.exists(tmp)) try(unlink(tmp), silent = TRUE)
      invisible(NULL)
    }

    # Load/merge progress when inputs change
    observeEvent(
      list(
        data(), text_col(), blind(),
        llm_output_cols(), llm_evidence_cols(), llm_score_cols(),
        original_file_name(), meta_cols()
      ),
      {
        df  <- req(data())
        txt <- req(text_col())
        if (!is.data.frame(df)) return()
        if (!txt %in% names(df)) {
          showNotification(
            sprintf("Text column '%s' not found in data.", txt),
            type     = "error",
            duration = 5
          )
          return()
        }
        n <- nrow(df)

        # Ensure IDs and columns exist
        if (!".row_id" %in% names(df)) {
          df$.row_id <- sprintf("row_%s", seq_len(n))
        }

        # Split comments/examples by mode; keep legacy columns if present
        if (!"comments_manual" %in% names(df))  df$comments_manual  <- ""
        if (!"examples_manual" %in% names(df))  df$examples_manual  <- ""
        if (!"comments_llm" %in% names(df))     df$comments_llm     <- ""
        if (!"examples_llm" %in% names(df))     df$examples_llm     <- ""
        df$comments_manual <- na_to_empty(df$comments_manual)
        df$examples_manual <- na_to_empty(df$examples_manual)
        df$comments_llm    <- na_to_empty(df$comments_llm)
        df$examples_llm    <- na_to_empty(df$examples_llm)

        if (!"status" %in% names(df)) {
          df$status <- rep("Unmarked", n)
        } else {
          df$status <- na_to_empty(df$status)
        }
        if (!"score" %in% names(df)) {
          df$score <- rep(NA_real_, n)
        }
        if (!"revised_score" %in% names(df)) {
          df$revised_score <- rep(NA_real_, n)
        }

        # Merge saved progress (safe, no unknown column warnings)
        saved_last <- NA_integer_
        sp <- tryCatch(assessed_path(), error = function(e) NULL)
        if (!is.null(sp) && file.exists(sp)) {
          saved    <- tryCatch(readRDS(sp), error = function(e) NULL)
          saved_df <- NULL
          if (is.list(saved) && "df" %in% names(saved)) saved_df <- saved$df
          if (is.data.frame(saved))                      saved_df <- saved
          if (!is.null(saved_df) && ".row_id" %in% names(saved_df)) {
            keep <- intersect(
              c(
                ".row_id",
                "comments_manual", "examples_manual",
                "comments_llm",    "examples_llm",
                "comments",        "examples",  # legacy
                "status",          "score",     "revised_score"
              ),
              names(saved_df)
            )
            m <- dplyr::left_join(
              df,
              saved_df[, keep, drop = FALSE],
              by     = ".row_id",
              suffix = c("", ".saved")
            )
            getm <- function(nm) if (nm %in% names(m)) m[[nm]] else NULL

            # Manual
            df$comments_manual <- na_to_empty(dplyr::coalesce(
              getm("comments_manual.saved"),
              df$comments_manual,
              getm("comments")
            ))
            df$examples_manual <- na_to_empty(dplyr::coalesce(
              getm("examples_manual.saved"),
              df$examples_manual,
              getm("examples")
            ))
            # LLM
            df$comments_llm <- na_to_empty(dplyr::coalesce(
              getm("comments_llm.saved"),
              df$comments_llm,
              getm("comments")
            ))
            df$examples_llm <- na_to_empty(dplyr::coalesce(
              getm("examples_llm.saved"),
              df$examples_llm,
              getm("examples")
            ))
            # Status/score
            df$status <- na_to_empty(dplyr::coalesce(
              getm("status.saved"),
              df$status,
              getm("status")
            ))
            if ("score.saved" %in% names(m)) {
              df$score <- dplyr::coalesce(
                getm("score.saved"),
                df$score,
                getm("score")
              )
            }
            if ("revised_score.saved" %in% names(m)) {
              df$revised_score <- dplyr::coalesce(
                getm("revised_score.saved"),
                df$revised_score,
                getm("revised_score")
              )
            }
          }
          if (is.list(saved) && is.numeric(saved$last_index)) {
            saved_last <- as.integer(saved$last_index)
          }
        }

        rv$df       <- df
        rv$n        <- n
        rv$text_vec <- as.character(df[[txt]])

        # Start index (mode-aware)
        if (isTRUE(blind())) {
          coded <- nzchar(df$comments_manual) |
            nzchar(df$examples_manual) |
            !is.na(df$score)
        } else {
          coded <- nzchar(df$comments_llm) |
            nzchar(df$examples_llm) |
            !(df$status %in% c("", "Unmarked")) |
            !is.na(df$revised_score)
        }
        start_idx <- if (is.finite(saved_last) && !is.na(saved_last)) {
          max(1L, min(saved_last, n))
        } else if (any(coded)) {
          max(which(coded))
        } else {
          1L
        }
        move_and_refresh(start_idx)
      },
      ignoreInit = FALSE,
      priority   = 10
    )

    # LLM side panel in sidebar - supports multiple columns
    output$llm_side <- renderUI({
      req(rv$df)
      if (isTRUE(blind())) return(NULL)

      out_cols  <- llm_output_cols()
      just_cols <- llm_evidence_cols()
      score_cols <- llm_score_cols()
      i <- current_index()
      df_names <- names(rv$df)

      # Filter to valid columns
      valid_out  <- if (!is.null(out_cols)) out_cols[out_cols %in% df_names] else character()
      valid_just <- if (!is.null(just_cols)) just_cols[just_cols %in% df_names] else character()
      valid_score <- if (!is.null(score_cols)) score_cols[score_cols %in% df_names] else character()

      if (length(valid_out) == 0) {
        return(tagList(
          h5("LLM outputs"),
          div(
            class = "small-box",
            span("Select at least one LLM output column.",
                 style = "color:#dc3545")
          )
        ))
      }

      # Build UI elements for each column type
      output_elements <- lapply(valid_out, function(col) {
        val <- na_to_empty(rv$df[[col]][i])
        div(
          class = "llm-section",
          div(class = "llm-label", col),
          div(class = "text-box", val)
        )
      })

      justification_elements <- if (length(valid_just) > 0) {
        lapply(valid_just, function(col) {
          val <- na_to_empty(rv$df[[col]][i])
          div(
            class = "llm-section",
            div(class = "llm-label", paste0(col, " (justification)")),
            div(class = "text-box", val)
          )
        })
      } else NULL

      score_elements <- if (length(valid_score) > 0) {
        lapply(valid_score, function(col) {
          val <- rv$df[[col]][i]
          val_str <- if (is.na(val)) "NA" else as.character(val)
          div(
            class = "llm-section",
            div(class = "llm-label", paste0(col, " (score)")),
            div(class = "score-box", val_str)
          )
        })
      } else NULL

      tagList(
        h5("LLM outputs"),
        output_elements,
        if (length(valid_just) > 0) tagList(
          h5("Justifications", style = "margin-top: 12px;"),
          justification_elements
        ),
        if (length(valid_score) > 0) tagList(
          h5("Scores", style = "margin-top: 12px;"),
          score_elements
        )
      )
    })

    # Mode-specific fields (score vs status)
    output$mode_fields <- renderUI({
      if (isTRUE(blind())) {
        numericInput(ns("score"), "Score", value = NA, step = 1)
      } else {
        tagList(
          radioButtons(
            ns("status_sel"), "Status",
            choices  = c("Valid", "Invalid", "Unmarked"),
            selected = "Unmarked", inline = TRUE
          ),
          numericInput(ns("revised_score"), "Revised Score", value = NA, step = 1)
        )
      }
    })

    # Meta table
    output$meta_ui <- renderUI({
      req(rv$df)
      cols <- meta_cols()
      if (length(cols) == 0) return(NULL)
      i  <- current_index()
      df <- rv$df
      tags$div(
        h5("Metadata"),
        tags$table(
          class = "table table-sm table-bordered",
          do.call(
            tags$tbody,
            lapply(cols, function(cn) {
              tags$tr(
                tags$th(cn),
                tags$td(na_to_empty(df[[cn]][i]))
              )
            })
          )
        )
      )
    })

    # Text + progress (highlight search query)
    output$text_ui <- renderUI({
      req(rv$text_vec, rv$n)
      q       <- input$search
      cur_txt <- rv$text_vec[current_index()]
      tagList(
        tags$div(
          class = "progress-bar",
          style = sprintf(
            "width:%s%%;",
            round(100 * current_index() / max(1, rv$n), 1)
          )
        ),
        tags$div(id = "main-text", highlight_query(cur_txt, q))
      )
    })

    # Sync UI to row (mode-aware comments/examples)
    move_and_refresh <- function(to) {
      if (is.null(rv$n) || is.na(to)) return()
      to <- max(1L, min(to, rv$n))
      current_index(to)
      i <- to
      if (isTRUE(blind())) {
        updateTextAreaInput(
          session, "comments",
          value = na_to_empty(rv$df$comments_manual[i])
        )
        updateTextAreaInput(
          session, "examples",
          value = na_to_empty(rv$df$examples_manual[i])
        )
        updateNumericInput(
          session, "score",
          value = suppressWarnings(as.numeric(rv$df$score[i]))
        )
      } else {
        updateTextAreaInput(
          session, "comments",
          value = na_to_empty(rv$df$comments_llm[i])
        )
        updateTextAreaInput(
          session, "examples",
          value = na_to_empty(rv$df$examples_llm[i])
        )
        updateRadioButtons(
          session, "status_sel",
          selected = na_to_empty(rv$df$status[i] %||% "Unmarked")
        )
        updateNumericInput(
          session, "revised_score",
          value = suppressWarnings(as.numeric(rv$df$revised_score[i]))
        )
      }
      output$progress_ui <- renderUI({
        p(
          sprintf(
            "Progress: %d / %d (%.1f%%)",
            current_index(), rv$n,
            100 * current_index() / max(1, rv$n)
          )
        )
      })
    }

    # Persist edits (mode-aware)
    observeEvent(input$comments, {
      if (is.null(rv$df)) return()
      i <- current_index()
      if (isTRUE(blind())) {
        rv$df$comments_manual[i] <- na_to_empty(input$comments %||% "")
      } else {
        rv$df$comments_llm[i] <- na_to_empty(input$comments %||% "")
      }
      save_now()
    }, ignoreInit = TRUE)

    observeEvent(input$examples, {
      if (is.null(rv$df)) return()
      i <- current_index()
      if (isTRUE(blind())) {
        rv$df$examples_manual[i] <- na_to_empty(input$examples %||% "")
      } else {
        rv$df$examples_llm[i] <- na_to_empty(input$examples %||% "")
      }
      save_now()
    }, ignoreInit = TRUE)

    observeEvent(input$score, {
      if (!isTRUE(blind()) || is.null(rv$df)) return()
      i <- current_index()
      rv$df$score[i] <- suppressWarnings(as.numeric(input$score))
      save_now()
    }, ignoreInit = TRUE)

    observeEvent(input$revised_score, {
      if (isTRUE(blind()) || is.null(rv$df)) return()
      i <- current_index()
      rv$df$revised_score[i] <- suppressWarnings(as.numeric(input$revised_score))
      save_now()
    }, ignoreInit = TRUE)

    observeEvent(input$status_sel, {
      if (isTRUE(blind()) || is.null(rv$df)) return()
      i <- current_index()
      rv$df$status[i] <- na_to_empty(input$status_sel %||% "Unmarked")
      save_now()
    }, ignoreInit = TRUE)

    # Search utilities
    find_matches <- function(q) {
      if (is.null(q) || !nzchar(q)) {
        integer(0)
      } else {
        which(grepl(q, rv$text_vec, ignore.case = TRUE))
      }
    }

    # Move on query change to first match
    observeEvent(input$search, {
      q <- input$search
      if (!nzchar(q)) return()
      idxs <- find_matches(q)
      if (length(idxs)) move_and_refresh(min(idxs))
    }, ignoreInit = TRUE)

    # Buttons navigation
    observeEvent(input$search_next, {
      idxs <- find_matches(input$search)
      if (!length(idxs)) {
        showNotification("No matches found.", type = "warning")
        return()
      }
      cur   <- current_index()
      nexts <- idxs[idxs > cur]
      goto  <- if (length(nexts)) min(nexts) else min(idxs) # wrap
      move_and_refresh(goto)
    }, ignoreInit = TRUE)

    observeEvent(input$search_prev, {
      idxs <- find_matches(input$search)
      if (!length(idxs)) {
        showNotification("No matches found.", type = "warning")
        return()
      }
      cur   <- current_index()
      prevs <- idxs[idxs < cur]
      goto  <- if (length(prevs)) max(prevs) else max(idxs) # wrap
      move_and_refresh(goto)
    }, ignoreInit = TRUE)

    # Navigation
    observeEvent(input$next_text, {
      if (!is.null(rv$n) && current_index() < rv$n) {
        save_now()
        move_and_refresh(current_index() + 1L)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$prev_text, {
      if (!is.null(rv$n) && current_index() > 1L) {
        save_now()
        move_and_refresh(current_index() - 1L)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$manual_save, ignoreInit = TRUE, handlerExpr = save_now)

    list(current_index = reactive(current_index()))
  })
}

# -------------------------------
# Main App
# -------------------------------

#' Launch the Quallmer Interactive App
#'
#' Starts the Shiny app for manual coding, LLM checking,
#' and validation / agreement calculation.
#'
#' - In LLM mode, you can also select metadata columns.
#' - In Validation mode, select unit ID and coder columns (no text column),
#'   and optionally specify a gold-standard coder.
#'
#' @param base_dir Base directory for saving uploaded files and progress.
#'   Defaults to current working directory. Use \code{tempdir()} for temporary
#'   storage (e.g., in examples or tests), but note that data will be lost when
#'   the R session ends.
#'
#' @return A shiny.appobj
#' @export
qlm_app <- function(base_dir = getwd()) {
  ui <- fluidPage(
    theme = bs_theme(
      version        = 5,
      bootswatch     = "flatly",
      base_font      = font_google("Inter", local = TRUE),
      heading_font   = font_google("Inter", local = TRUE),
      code_font      = font_collection("Fira Mono", "Consolas"),
      "font-size-base" = ".85rem",
      "primary"      = "#5A6F8F"
    ),
    tags$head(tags$style(HTML("
      .app-title {
        font-size: 1.35rem;
        font-weight: 700;
        margin: 6px 0 12px 0;
      }
    "))),
    div(class = "app-title", strong("quallmer app")),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("1. Specify your data folder"),
        textInput(
          "folder_name",
          "Folder name (required):",
          value = "",
          placeholder = "e.g., your_name_coding"
        ),
        helpText(
          tags$small("Specify a unique folder name to save your coding work. This prevents overwriting other users' data.")
        ),
        hr(),
        h4("2. Select a data file"),
        fileInput(
          "file",
          "Choose a data file (.rds or .csv):",
          accept = c(".rds", ".csv")
        ),
        div(
          style = "margin-top:-6px; color:#6c757d;",
          textOutput("loaded_file_name")
        ),
        actionButton(
          "reset_btn", "Save and reset",
          class = "btn btn-secondary w-100"
        ),
        hr(),
        h4("3. Choose mode"),
        radioButtons(
          "mode", "Mode:",
          choices = c(
            "Manual coding (blind)" = "blind",
            "Checking LLM outputs"  = "llm",
            "Validation scores"     = "agreement"
          ),
          selected = "blind"
        ),
        hr(),
        uiOutput("column_selectors"),
        br(),
        helpText(
          "Progress is saved to *_assessed.rds in your specified folder"
        )
      ),
      mainPanel(uiOutput("main_content"))
    )
  )

  server <- function(input, output, session) {
    dataset   <- reactiveVal(NULL)
    last_file <- reactiveVal(NULL)

    state_path <- reactive({
      folder <- input$folder_name
      if (is.null(folder) || !nzchar(trimws(folder))) return(NULL)
      file.path(base_dir, trimws(folder), ".app_state.rds")
    })
    hc         <- NULL

    get_hc_index <- function() {
      if (is.null(hc)) return(NA_integer_)
      idx <- tryCatch(isolate(hc$current_index()), error = function(e) NA_integer_)
      if (is.null(idx)) NA_integer_ else as.integer(idx)
    }

    save_state <- function() {
      sp <- tryCatch(state_path(), error = function(e) NULL)
      if (is.null(sp)) return(invisible(NULL))

      st <- list(
        folder_name        = isolate(input$folder_name),
        last_file          = tryCatch(isolate(last_file()), error = function(e) NULL),
        mode               = input$mode,
        text_col           = isolate(input$text_col),
        meta_cols          = isolate(input$meta_cols),
        llm_output_cols    = isolate(input$llm_output_cols),
        llm_evidence_cols  = isolate(input$llm_evidence_cols),
        llm_score_cols     = isolate(input$llm_score_cols),
        unit_id_col        = isolate(input$unit_id_col),
        coder_cols         = isolate(input$coder_cols),
        agreement_has_gold = isolate(input$agreement_has_gold),
        gold_col           = isolate(input$gold_col),
        measurement_level  = isolate(input$measurement_level),
        hc_last_index      = get_hc_index()
      )
      dir.create(dirname(sp), recursive = TRUE, showWarnings = FALSE)
      try(saveRDS(st, sp), silent = TRUE)
    }

    # Restore folder name and state
    observe({
      # Try to find a previously used folder
      folders <- list.dirs(base_dir, recursive = FALSE, full.names = FALSE)
      folders_with_state <- folders[sapply(folders, function(f) {
        file.exists(file.path(base_dir, f, ".app_state.rds"))
      })]

      # If there's exactly one folder with state, restore it
      if (length(folders_with_state) == 1 && is.null(dataset())) {
        folder <- folders_with_state[1]
        sp <- file.path(base_dir, folder, ".app_state.rds")

        if (file.exists(sp)) {
          st <- tryCatch(readRDS(sp), error = function(e) NULL)

          if (!is.null(st)) {
            # Restore folder name
            if (!is.null(st$folder_name) && nzchar(st$folder_name)) {
              updateTextInput(session, "folder_name", value = st$folder_name)
            }

            # Restore last file
            if (!is.null(st$last_file) && file.exists(st$last_file)) {
              obj <- tryCatch(
                read_data_file(st$last_file, basename(st$last_file)),
                error = function(e) {
                  showNotification(e$message, type = "error")
                  NULL
                }
              )
              if (!is.null(obj) && is.data.frame(obj)) {
                dataset(obj)
                last_file(st$last_file)
              }
              if (!is.null(obj) && !is.data.frame(obj)) {
                showNotification(
                  "Wrong format: please upload a .rds or .csv that contains a data frame.",
                  type = "error"
                )
              }
            }

            # Restore mode
            if (!is.null(st$mode)) {
              updateRadioButtons(session, "mode", selected = st$mode)
            }
          }
        }
      }
    })

    output$loaded_file_name <- renderText({
      p <- last_file()
      if (is.null(p)) "No file loaded" else paste("Loaded:", basename(p))
    })

    # Persist upload locally to user-specified folder
    observeEvent(input$file, {
      req(input$file)

      # Validate folder name is provided
      folder <- input$folder_name
      if (is.null(folder) || !nzchar(trimws(folder))) {
        showNotification(
          "Please specify a folder name before uploading a file.",
          type = "error",
          duration = 5
        )
        return()
      }

      coding_dir <- file.path(base_dir, trimws(folder))
      dir.create(coding_dir, showWarnings = FALSE, recursive = TRUE)
      dest <- normalizePath(
        file.path(coding_dir, input$file$name),
        mustWork = FALSE
      )
      if (!isTRUE(file.copy(input$file$datapath, dest, overwrite = TRUE))) {
        showNotification("Could not persist uploaded file.", type = "error")
        return()
      }
      obj <- tryCatch(
        read_data_file(dest, basename(dest)),
        error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        }
      )
      if (is.null(obj)) return()
      if (!is.data.frame(obj)) {
        showNotification(
          "Wrong format: please upload a .rds or .csv that contains a data frame.",
          type = "error"
        )
        return()
      }
      dataset(obj)
      last_file(dest)
      save_state()
    }, ignoreInit = TRUE)

    # Save and reset
    observeEvent(input$reset_btn, {
      sp <- tryCatch(state_path(), error = function(e) NULL)
      if (!is.null(sp) && file.exists(sp)) try(unlink(sp), silent = TRUE)
      dataset(NULL)
      last_file(NULL)
      updateTextInput(session, "folder_name", value = "")
      showNotification("App reset. You can load a new file now.", type = "message")
    })

    # Column selectors UI
    output$column_selectors <- renderUI({
      req(dataset())
      if (!is.data.frame(dataset())) return(NULL)
      cols <- names(dataset())
      sp <- tryCatch(state_path(), error = function(e) NULL)
      st <- if (!is.null(sp) && file.exists(sp)) {
        tryCatch(readRDS(sp), error = function(e) NULL)
      } else {
        NULL
      }
      mode <- input$mode

      tagList(
        h4("4. Select columns"),
        if (mode %in% c("blind", "llm")) {
          tagList(
            selectInput(
              "text_col", "Text column:", choices = cols,
              selected = {
                sel <- if (!is.null(st) && !is.null(st$text_col)) st$text_col else NULL
                if (!is.null(sel) && sel %in% cols) sel else cols[[1]]
              }
            ),
            if (mode == "blind") {
              selectInput(
                "meta_cols", "Metadata columns (optional):",
                choices = cols, multiple = TRUE,
                selected = if (!is.null(st) && !is.null(st$meta_cols)) {
                  intersect(st$meta_cols, cols)
                } else NULL
              )
            } else {
              tagList(
                selectInput(
                  "llm_output_cols", "LLM output columns (select one or more):",
                  choices = cols,
                  multiple = TRUE,
                  selected = {
                    sel <- if (!is.null(st) && !is.null(st$llm_output_cols)) {
                      st$llm_output_cols
                    } else if (!is.null(st) && !is.null(st$llm_output_col)) {
                      st$llm_output_col  # backward compat
                    } else NULL
                    if (!is.null(sel)) intersect(sel, cols) else cols[[1]]
                  }
                ),
                selectInput(
                  "llm_evidence_cols", "LLM justification columns (optional):",
                  choices = cols,
                  multiple = TRUE,
                  selected = {
                    sel <- if (!is.null(st) && !is.null(st$llm_evidence_cols)) {
                      st$llm_evidence_cols
                    } else if (!is.null(st) && !is.null(st$llm_evidence_col) &&
                               !identical(st$llm_evidence_col, "None")) {
                      st$llm_evidence_col  # backward compat
                    } else NULL
                    if (!is.null(sel)) intersect(sel, cols) else NULL
                  }
                ),
                selectInput(
                  "llm_score_cols", "LLM score columns (optional, numeric):",
                  choices = cols,
                  multiple = TRUE,
                  selected = if (!is.null(st) && !is.null(st$llm_score_cols)) {
                    intersect(st$llm_score_cols, cols)
                  } else NULL
                ),
                helpText(
                  tags$small("Select multiple columns to compare outputs from different LLMs.")
                ),
                selectInput(
                  "meta_cols", "Metadata columns (optional):",
                  choices = cols, multiple = TRUE,
                  selected = if (!is.null(st) && !is.null(st$meta_cols)) {
                    intersect(st$meta_cols, cols)
                  } else NULL
                )
              )
            }
          )
        } else if (mode == "agreement") {
          tagList(
            selectInput(
              "unit_id_col", "Unit ID column:", choices = cols,
              selected = if (!is.null(st) &&
                             !is.null(st$unit_id_col) &&
                             st$unit_id_col %in% cols) {
                st$unit_id_col
              } else NULL
            ),
            selectInput(
              "coder_cols", "Coder columns (multiple):",
              choices = cols, multiple = TRUE,
              selected = if (!is.null(st) && !is.null(st$coder_cols)) {
                intersect(st$coder_cols, cols)
              } else NULL
            ),
            checkboxInput(
              "agreement_has_gold",
              "Gold-standard data available",
              value = if (!is.null(st) && !is.null(st$agreement_has_gold)) {
                isTRUE(st$agreement_has_gold)
              } else FALSE
            ),
            selectInput(
              "measurement_level",
              "Measurement level:",
              choices = c("nominal", "ordinal", "interval", "ratio"),
              selected = if (!is.null(st) && !is.null(st$measurement_level)) {
                st$measurement_level
              } else "nominal"
            ),
            helpText(
              tags$small(
                tags$strong("Nominal:"), " unordered categories (e.g., topics, sentiment)", tags$br(),
                tags$strong("Ordinal:"), " ordered categories (e.g., ratings 1-5, Likert scales)", tags$br(),
                tags$strong("Interval:"), " numeric with equal intervals (e.g., temperature, dates)", tags$br(),
                tags$strong("Ratio:"), " numeric with true zero (e.g., counts, word length)"
              )
            ),
            uiOutput("gold_ui")
          )
        }
      )
    })

    # Separate UI for gold-standard selector so it updates reliably
    output$gold_ui <- renderUI({
      req(dataset(), input$mode == "agreement")
      if (!isTRUE(input$agreement_has_gold)) return(NULL)
      cols <- names(dataset())
      sp <- tryCatch(state_path(), error = function(e) NULL)
      st <- if (!is.null(sp) && file.exists(sp)) {
        tryCatch(readRDS(sp), error = function(e) NULL)
      } else {
        NULL
      }
      selected_gold <- if (!is.null(st) && !is.null(st$gold_col) &&
                           st$gold_col %in% cols) {
        st$gold_col
      } else NULL

      selectInput(
        "gold_col", "Gold-standard coder column:",
        choices  = cols,
        selected = selected_gold
      )
    })

    # Data preview
    output$data_preview <- renderTable({
      req(dataset())
      if (!is.data.frame(dataset())) return(NULL)
      preview_head(dataset())
    })

    # Human check server
    observe({
      req(dataset(), is.data.frame(dataset()))
      mode <- input$mode
      if (mode %in% c("blind", "llm")) {
        txt <- input$text_col
        if (is.null(txt) || !(txt %in% names(dataset()))) return()
        if (mode == "llm") req(input$llm_output_cols)
        hc <<- humancheck_server(
          id   = "hc",
          data = reactive(dataset()),
          text_col = reactive(txt),
          blind    = reactive(mode == "blind"),
          llm_output_cols = reactive(if (mode == "llm") input$llm_output_cols else NULL),
          llm_evidence_cols = reactive(if (mode == "llm") input$llm_evidence_cols else NULL),
          llm_score_cols = reactive(if (mode == "llm") input$llm_score_cols else NULL),
          original_file_name = reactive({
            lf <- last_file()
            if (is.null(lf) || !nzchar(lf)) "quallmer_coding/unknown.csv" else lf
          }),
          meta_cols = reactive(if (mode %in% c("blind", "llm")) input$meta_cols else character())
        )
      }
    })

    # Persist state on changes
    observeEvent(input$folder_name,        save_state, ignoreInit = TRUE)
    observeEvent(input$mode,               save_state, ignoreInit = FALSE)
    observeEvent(input$text_col,           save_state, ignoreInit = TRUE)
    observeEvent(input$meta_cols,          save_state, ignoreInit = TRUE)
    observeEvent(input$llm_output_cols,    save_state, ignoreInit = TRUE)
    observeEvent(input$llm_evidence_cols,  save_state, ignoreInit = TRUE)
    observeEvent(input$llm_score_cols,     save_state, ignoreInit = TRUE)
    observeEvent(input$unit_id_col,        save_state, ignoreInit = TRUE)
    observeEvent(input$coder_cols,         save_state, ignoreInit = TRUE)
    observeEvent(input$agreement_has_gold, save_state, ignoreInit = TRUE)
    observeEvent(input$gold_col,           save_state, ignoreInit = TRUE)
    observeEvent(input$measurement_level,  save_state, ignoreInit = TRUE)
    observeEvent({
      if (!is.null(hc)) hc$current_index()
    }, save_state, ignoreInit = TRUE)

    # Main content UI
    output$main_content <- renderUI({
      if (is.null(dataset())) {
        tagList(
          h3("Welcome to the quallmer app"),
          p("Step 1: Specify your data folder (required)"),
          p("Step 2: Choose a file"),
          p("Step 3: Select mode"),
          p("Step 4: Select appropriate columns."),
          hr(),
          p(
            strong("Try the sample data:"),
            "Upload the sample file from",
            code("inst/extdata/sample_data.rds")
          ),
          hr(),
          h4("File Preview:"),
          tableOutput("data_preview")
        )
      } else if (!is.data.frame(dataset())) {
        tagList(
          h4("Loaded object is not a data frame."),
          p("Please upload a .rds or .csv containing a data frame.")
        )
      } else if (input$mode %in% c("blind", "llm")) {
        humancheck_ui("hc")
      } else if (input$mode == "agreement") {
        tagList(
          h4("Validation scores:"),
          tableOutput("icr_summary"),
          uiOutput("icr_interpret"),
          downloadButton("export_icr", "Export scores")
        )
      }
    })

    # -------------------------------
    # Validation / agreement mode (using qlm_compare() and qlm_validate())
    # -------------------------------

    # icr_result() returns a list with:
    #   - kind = "icr"   + data = named list of metrics
    #   - kind = "gold"  + data = data.frame (per-coder metrics)
    #   - kind = "message" + message = reason
    icr_result <- reactive({
      req(dataset(), is.data.frame(dataset()), input$mode == "agreement")
      df         <- dataset()
      unit_id    <- input$unit_id_col
      coder_cols <- input$coder_cols

      # Validate inputs with helpful error messages
      if (is.null(unit_id)) {
        return(list(
          kind = "message",
          message = "Please select a Unit ID column."
        ))
      }

      if (!unit_id %in% names(df)) {
        return(list(
          kind = "message",
          message = sprintf("Selected Unit ID column '%s' not found in dataset. Available columns: %s",
                            unit_id, paste(names(df), collapse = ", "))
        ))
      }

      if (is.null(coder_cols) || length(coder_cols) == 0) {
        return(list(
          kind = "message",
          message = "Please select at least 2 coder columns."
        ))
      }

      if (length(coder_cols) < 2L) {
        return(list(
          kind = "message",
          message = sprintf("Please select at least 2 coder columns. You selected: %d", length(coder_cols))
        ))
      }

      missing_cols <- coder_cols[!coder_cols %in% names(df)]
      if (length(missing_cols) > 0) {
        return(list(
          kind = "message",
          message = sprintf("Selected coder columns not found in dataset: %s. Available columns: %s",
                            paste(missing_cols, collapse = ", "),
                            paste(names(df), collapse = ", "))
        ))
      }

      # Gold-standard mode?
      if (isTRUE(input$agreement_has_gold)) {
        gold <- input$gold_col
        if (is.null(gold) || !nzchar(gold) || !(gold %in% coder_cols)) {
          return(list(
            kind    = "message",
            message = "Select a gold-standard coder column that is one of the coder columns."
          ))
        }

        res_gold <- tryCatch({
          # Identify non-gold coders
          non_gold_coders <- setdiff(coder_cols, gold)

          # Create gold standard data frame
          gold_df <- data.frame(
            .id = df[[unit_id]],
            code = df[[gold]],
            stringsAsFactors = FALSE
          )

          # Validate each non-gold coder against gold standard
          level <- input$measurement_level
          confusion_matrices <- list()

          results <- lapply(non_gold_coders, function(coder_name) {
            # Create prediction data frame
            pred_df <- data.frame(
              .id = df[[unit_id]],
              code = df[[coder_name]],
              stringsAsFactors = FALSE
            )

            # Call qlm_validate
            validation <- quallmer::qlm_validate(
              x = pred_df,
              gold = gold_df,
              by = code,
              level = level,
              average = "macro"
            )

            # Store confusion matrix for nominal level
            if (level == "nominal" && !is.null(validation$confusion)) {
              confusion_matrices[[coder_name]] <<- validation$confusion
            }

            # Extract metrics based on measurement level
            if (level == "nominal") {
              data.frame(
                coder = coder_name,
                accuracy = validation$accuracy,
                precision = validation$precision,
                recall = validation$recall,
                f1 = validation$f1,
                stringsAsFactors = FALSE
              )
            } else if (level == "ordinal") {
              data.frame(
                coder = coder_name,
                rho = validation$rho,
                tau = validation$tau,
                mae = validation$mae,
                stringsAsFactors = FALSE
              )
            } else if (level %in% c("interval", "ratio")) {
              data.frame(
                coder = coder_name,
                r = validation$r,
                icc = validation$icc,
                mae = validation$mae,
                rmse = validation$rmse,
                stringsAsFactors = FALSE
              )
            }
          })

          # Combine results into single data frame
          metrics_df <- do.call(rbind, results)

          # Return both metrics and confusion matrices
          list(
            metrics = metrics_df,
            confusion_matrices = if (length(confusion_matrices) > 0) confusion_matrices else NULL
          )
        }, error = function(e) e)

        if (inherits(res_gold, "error")) {
          return(list(
            kind    = "message",
            message = paste("Error during gold-standard validation:", res_gold$message)
          ))
        }

        return(list(kind = "gold", data = res_gold))
      }

      # Default: inter-rater reliability using qlm_compare()
      res_icr <- tryCatch({
        # Check for missing data and warn user
        n_total <- nrow(df)
        missing_counts <- sapply(coder_cols, function(col) sum(is.na(df[[col]])))
        total_missing <- sum(missing_counts > 0)

        if (total_missing > 0) {
          missing_info <- paste(
            sapply(names(missing_counts[missing_counts > 0]), function(col) {
              sprintf("%s: %d NAs", col, missing_counts[col])
            }),
            collapse = ", "
          )
          showNotification(
            sprintf("Warning: Missing values detected (%s). These will be excluded from analysis.",
                    missing_info),
            type = "warning",
            duration = 10
          )
        }

        # Transform wide-format data into list of data frames for qlm_compare()
        coder_dfs <- prepare_comparison_data(df, unit_id, coder_cols)

        # Call qlm_compare with all coder data frames
        comparison <- do.call(
          quallmer::qlm_compare,
          c(coder_dfs, list(by = quote(code), level = input$measurement_level, tolerance = 0))
        )

        # Extract metrics into a named list format
        level <- input$measurement_level
        metrics <- list()

        if (level == "nominal") {
          metrics$alpha_nominal <- comparison$alpha_nominal
          metrics$kappa <- comparison$kappa
          metrics$kappa_type <- comparison$kappa_type
          metrics$percent_agreement <- comparison$percent_agreement
        } else if (level == "ordinal") {
          metrics$alpha_ordinal <- comparison$alpha_ordinal
          if (!is.null(comparison$kappa_weighted)) metrics$kappa_weighted <- comparison$kappa_weighted
          metrics$w <- comparison$w
          metrics$rho <- comparison$rho
          metrics$percent_agreement <- comparison$percent_agreement
        } else if (level %in% c("interval", "ratio")) {
          alpha_name <- paste0("alpha_", level)
          metrics[[alpha_name]] <- comparison[[alpha_name]]
          metrics$icc <- comparison$icc
          metrics$r <- comparison$r
          metrics$percent_agreement <- comparison$percent_agreement
        }

        metrics$subjects <- comparison$subjects
        metrics$raters <- comparison$raters

        metrics
      }, error = function(e) e)

      if (inherits(res_icr, "error")) {
        return(list(
          kind    = "message",
          message = paste("Error during inter-rater reliability calculation:", res_icr$message)
        ))
      }

      list(kind = "icr", data = res_icr)
    })

    output$icr_summary <- renderTable({
      req(input$mode == "agreement")
      res <- icr_result()

      # Nothing configured yet
      if (is.null(res)) {
        return(data.frame(
          metric = "message",
          value  = "Select a unit ID and at least two coder columns.",
          stringsAsFactors = FALSE
        ))
      }

      if (res$kind == "message") {
        return(data.frame(
          metric = "message",
          value  = res$message,
          stringsAsFactors = FALSE
        ))
      }

      if (res$kind == "icr") {
        # Named list -> long data.frame
        lst <- res$data
        return(data.frame(
          metric = names(lst),
          value  = unlist(lst, use.names = FALSE),
          stringsAsFactors = FALSE
        ))
      }

      if (res$kind == "gold") {
        # Return metrics data.frame
        return(res$data$metrics)
      }

      # Fallback
      data.frame(
        metric = "message",
        value  = "Unknown result type.",
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE)

    output$icr_interpret <- renderUI({
      req(input$mode == "agreement")
      res <- icr_result()
      if (is.null(res)) return(NULL)

      # ----- Inter-rater reliability interpretation -----
      if (res$kind == "icr") {
        lst <- res$data
        level <- input$measurement_level

        # Build interpretation based on measurement level
        interpretation_items <- list()

        # Krippendorff's alpha (available for all levels)
        alpha_name <- paste0("alpha_", level)
        alpha <- suppressWarnings(as.numeric(lst[[alpha_name]]))
        if (length(alpha) == 0) alpha <- NA_real_

        txt_alpha <- if (is.na(alpha)) {
          "Krippendorff's alpha unavailable."
        } else if (alpha >= 0.8) {
          sprintf("Krippendorff's alpha (%s) > 0.80 indicates good reliability.", level)
        } else if (alpha >= 0.67) {
          sprintf("Krippendorff's alpha (%s) between 0.67 and 0.80 is acceptable for tentative conclusions.", level)
        } else {
          sprintf("Krippendorff's alpha (%s) < 0.67 indicates low reliability.", level)
        }
        interpretation_items[[length(interpretation_items) + 1]] <- tags$li(txt_alpha)

        # Kappa (nominal and ordinal)
        if (level %in% c("nominal", "ordinal")) {
          kappa <- suppressWarnings(as.numeric(lst[["kappa"]]))
          if (length(kappa) == 0) kappa <- NA_real_
          kappa_type <- lst[["kappa_type"]]

          # Landis & Koch for kappa
          lk <- function(k) {
            if (is.na(k)) "unavailable"
            else if (k < 0)    "poor"
            else if (k < 0.20) "slight"
            else if (k < 0.40) "fair"
            else if (k < 0.60) "moderate"
            else if (k < 0.80) "substantial"
            else               "almost perfect"
          }

          kappa_label <- if (!is.null(kappa_type) && nzchar(kappa_type)) {
            paste0(kappa_type, " kappa")
          } else if (level == "ordinal") {
            "Weighted kappa"
          } else {
            "Kappa"
          }

          txt_kappa <- if (is.na(kappa)) {
            sprintf("%s unavailable.", kappa_label)
          } else {
            sprintf("%s = %.2f (%s agreement).", kappa_label, kappa, lk(kappa))
          }
          interpretation_items[[length(interpretation_items) + 1]] <- tags$li(txt_kappa)
        }

        # ICC (interval/ratio)
        if (level %in% c("interval", "ratio")) {
          icc <- suppressWarnings(as.numeric(lst[["icc"]]))
          if (length(icc) == 0) icc <- NA_real_
          txt_icc <- if (is.na(icc)) {
            "ICC unavailable."
          } else if (icc >= 0.75) {
            sprintf("ICC = %.2f indicates excellent reliability.", icc)
          } else if (icc >= 0.60) {
            sprintf("ICC = %.2f indicates good reliability.", icc)
          } else if (icc >= 0.40) {
            sprintf("ICC = %.2f indicates fair reliability.", icc)
          } else {
            sprintf("ICC = %.2f indicates poor reliability.", icc)
          }
          interpretation_items[[length(interpretation_items) + 1]] <- tags$li(txt_icc)
        }

        # Correlation metrics
        if (level == "ordinal") {
          rho <- suppressWarnings(as.numeric(lst[["rho"]]))
          if (length(rho) == 0) rho <- NA_real_
          if (!is.na(rho)) {
            interpretation_items[[length(interpretation_items) + 1]] <- tags$li(
              sprintf("Spearman's rho = %.2f (rank correlation)", rho)
            )
          }
        } else if (level %in% c("interval", "ratio")) {
          r <- suppressWarnings(as.numeric(lst[["r"]]))
          if (length(r) == 0) r <- NA_real_
          if (!is.na(r)) {
            interpretation_items[[length(interpretation_items) + 1]] <- tags$li(
              sprintf("Pearson's r = %.2f (linear correlation)", r)
            )
          }
        }

        return(tagList(
          br(),
          tags$p("Interpretation guidance:"),
          tags$ul(interpretation_items)
        ))
      }

      # ----- Gold-standard interpretation -----
      if (res$kind == "gold") {
        df <- res$data$metrics
        if (!nrow(df)) return(NULL)

        level <- input$measurement_level
        interpretation_items <- list()
        # Helper to safely format values (handle length-zero and NA)
        fmt <- function(x) {
          if (length(x) == 0 || is.na(x)) "NA" else sprintf("%.2f", x)
        }

        score_label <- function(x) {
          if (length(x) == 0 || is.na(x)) "unavailable"
          else if (x >= 0.90)             "excellent"
          else if (x >= 0.80)             "good"
          else if (x >= 0.70)             "fair"
          else                            "low"
        }

        if (level == "nominal") {
          # Nominal: accuracy, precision, recall, F1
          m_acc  <- suppressWarnings(mean(df$accuracy, na.rm = TRUE))
          m_prec <- suppressWarnings(mean(df$precision, na.rm = TRUE))
          m_rec  <- suppressWarnings(mean(df$recall, na.rm = TRUE))
          m_f1   <- suppressWarnings(mean(df$f1, na.rm = TRUE))

          interpretation_items <- list(
            tags$li(sprintf(
              "Accuracy (mean across coders = %s) measures the share of units where the coder matches the gold-standard label. Values above ~0.80 are typically considered good, above ~0.90 excellent.",
              fmt(m_acc)
            )),
            tags$li(sprintf(
              "Macro precision (mean = %s) is the average, across classes, of how often a predicted label is correct when it is used. Low precision means many false positives.",
              fmt(m_prec)
            )),
            tags$li(sprintf(
              "Macro recall (mean = %s) is the average, across classes, of how many gold-standard instances are successfully recovered. Low recall means many false negatives.",
              fmt(m_rec)
            )),
            tags$li(sprintf(
              "Macro F1 (mean = %s) is the harmonic mean of macro precision and macro recall. It summarizes the balance between missing true cases and producing false alarms.",
              fmt(m_f1)
            )),
            tags$li(sprintf(
              "Overall, these scores are %s for accuracy, %s for precision, %s for recall, and %s for F1.",
              score_label(m_acc),
              score_label(m_prec),
              score_label(m_rec),
              score_label(m_f1)
            ))
          )
        } else if (level == "ordinal") {
          # Ordinal: rho, tau, MAE
          m_rho <- suppressWarnings(mean(df$rho, na.rm = TRUE))
          m_tau <- suppressWarnings(mean(df$tau, na.rm = TRUE))
          m_mae <- suppressWarnings(mean(df$mae, na.rm = TRUE))

          interpretation_items <- list(
            tags$li(sprintf(
              "Spearman's rho (mean = %s) measures rank correlation between coders and gold standard. Values closer to 1 indicate better agreement.",
              fmt(m_rho)
            )),
            tags$li(sprintf(
              "Kendall's tau (mean = %s) is another rank correlation measure, less sensitive to outliers. Values closer to 1 indicate better agreement.",
              fmt(m_tau)
            )),
            tags$li(sprintf(
              "Mean Absolute Error (mean = %s) measures the average distance between coder ratings and gold standard. Lower values indicate better agreement.",
              fmt(m_mae)
            ))
          )
        } else if (level %in% c("interval", "ratio")) {
          # Interval/Ratio: r, ICC, MAE, RMSE
          m_r <- suppressWarnings(mean(df$r, na.rm = TRUE))
          m_icc <- suppressWarnings(mean(df$icc, na.rm = TRUE))
          m_mae <- suppressWarnings(mean(df$mae, na.rm = TRUE))
          m_rmse <- suppressWarnings(mean(df$rmse, na.rm = TRUE))

          interpretation_items <- list(
            tags$li(sprintf(
              "Pearson's r (mean = %s) measures linear correlation between coders and gold standard. Values closer to 1 indicate better agreement.",
              fmt(m_r)
            )),
            tags$li(sprintf(
              "ICC (mean = %s) measures consistency between coders and gold standard. Values >0.75 are excellent, >0.60 good, >0.40 fair.",
              fmt(m_icc)
            )),
            tags$li(sprintf(
              "Mean Absolute Error (mean = %s) measures average distance between coder values and gold standard. Lower values indicate better agreement.",
              fmt(m_mae)
            )),
            tags$li(sprintf(
              "Root Mean Squared Error (mean = %s) penalizes larger errors more than MAE. Lower values indicate better agreement.",
              fmt(m_rmse)
            ))
          )
        }

        return(tagList(
          br(),
          tags$p(sprintf("Interpretation guidance (gold-standard comparison, %s level):", level)),
          tags$ul(interpretation_items)
        ))
      }

      NULL
    })

    output$export_icr <- downloadHandler(
      filename = function() "validate_results.csv",
      content = function(file) {
        res <- icr_result()

        df <- if (is.null(res)) {
          data.frame(
            message = "No valid selection.",
            stringsAsFactors = FALSE
          )
        } else if (res$kind == "message") {
          data.frame(
            message = res$message,
            stringsAsFactors = FALSE
          )
        } else if (res$kind == "icr") {
          as.data.frame(as.list(res$data), stringsAsFactors = FALSE)
        } else if (res$kind == "gold") {
          # For gold-standard validation, export metrics and confusion matrices
          metrics_df <- res$data$metrics
          confusion_matrices <- res$data$confusion_matrices

          if (!is.null(confusion_matrices) && length(confusion_matrices) > 0) {
            # Create a comprehensive export with confusion matrices
            # Write metrics first
            cat("Validation Metrics\n", file = file)
            utils::write.table(metrics_df, file = file, append = TRUE,
                               sep = ",", row.names = FALSE)

            # Add confusion matrices for each coder
            for (coder_name in names(confusion_matrices)) {
              cat("\n\nConfusion Matrix -", coder_name, "\n", file = file, append = TRUE)
              cm <- as.data.frame.table(confusion_matrices[[coder_name]])
              names(cm) <- c("Predicted", "Actual", "Count")
              utils::write.table(cm, file = file, append = TRUE,
                                 sep = ",", row.names = FALSE)
            }
            return()  # Exit early since we handled the file writing
          } else {
            metrics_df
          }
        } else {
          data.frame(
            message = "Unknown result type.",
            stringsAsFactors = FALSE
          )
        }

        utils::write.csv(df, file, row.names = FALSE)
      }
    )
  }

  shinyApp(ui, server)
}

# Run the app if executed directly
if (identical(environment(), globalenv()) &&
    !length(commandArgs(trailingOnly = TRUE))) {
  validate_app()
}
