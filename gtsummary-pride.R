library(gtsummary)
library(gt)

devtools::source_gist("c241f193b61d302a060bfb6fa79a9bd5")


tbl <-
  trial %>%
  select(age, grade, trt) %>%
  tbl_summary(by = trt, missing = "no") %>%
  add_n() %>%
  add_p() %>%
  modify_footnote(everything() ~ NA) %>%
  as_gt() %>%
  tab_header("Build a {gtsummary} Table")
tbl
tbl %>%
  gt_rainbow_stripes()



gt_rainbow_stripes <- function(x, rep_n = 5) {
  x <-
    gt::cols_width(x, label ~ gt::px(130)) %>%
    gt::tab_header(
      title = x[["_heading"]]$title,
      subtitle = "But Make it G - A - Y"
    ) %>%
    gt::tab_options(heading.subtitle.font.size = 20,
                    heading.subtitle.font.weight = "bolder")

  nrow <- nrow(x[["_data"]])

  # define rainbow colors
  rainbox_hex_original <-
    c(purple = "#CC99C9",
      blue = "#9EC1CF",
      green = "#9EE09E",
      yellow = "#FDFD97",
      orange = "#FEB144",
      red = "#FF6663")

  url_flags <-
    c(transgender = "https://upload.wikimedia.org/wikipedia/commons/b/b0/Transgender_Pride_flag.svg",
      gender_fluid = "https://upload.wikimedia.org/wikipedia/commons/b/b8/Genderfluidity_Pride-Flag.svg",
      bisexual = "https://upload.wikimedia.org/wikipedia/commons/2/2a/Bisexual_Pride_Flag.svg",
      pride = "https://upload.wikimedia.org/wikipedia/commons/4/48/Gay_Pride_Flag.svg")


  # function to shift the rainbow vector
  shifter <- function(x, n) {
    n <- n %% length(x)
    if (n == 0) x else c(tail(x, -n), head(x, n))
  }

  for (i in seq_len(length(rainbox_hex_original) * rep_n + 1)) {
    rainbox_hex <-
      rainbox_hex_original %>%
      shifter(i - 1) %>%
      rev() %>%
      rep_len(nrow + 1)

    html_flags <-
      url_flags %>%
      shifter(i - 1) %>%
      rep_len(6) %>%
      purrr::map_chr(~gt::web_image(url = .x, height = gt::px(17))) %>%
      paste(collapse = " ")


    expr_source_note <-
      rlang::expr(
        gt::tab_source_note(
          gt::html(paste(html_flags, "<strong>", "Happy Pride!", "</strong>", html_flags))
        )
      ) %>%
      list()


    header_strip <-
      rlang::expr(
        gt::tab_style(style = gt::cell_fill(color = rainbox_hex[1]),
                      locations = gt::cells_column_labels())
      ) %>%
      list()

    body_stripes <-
      seq_len(nrow) %>%
      purrr::map(
        ~rlang::expr(
          gt::tab_style(style = gt::cell_fill(color = rainbox_hex[!!.x + 1]),
                        locations = gt::cells_body(rows = !!.x))
        )
      )

    tbl <-
      ifelse(as.logical(i %% 2),
             list(c(header_strip, body_stripes)),
             list(c(header_strip, body_stripes, expr_source_note))) %>%
      purrr::flatten() %>%
      purrr::reduce(
        .f = function(x, y) rlang::expr(!!x %>% !!y),
        .init = rlang::expr(x)
      ) %>%
      eval()
    print(tbl)
    # gt::gtsave(tbl, paste0("gt_pride_", i, ".png"))
  }

  tbl
}

tbl %>%
  gt_rainbow_stripes(1)

