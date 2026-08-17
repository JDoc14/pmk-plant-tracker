# ============================================================
# PMK Civil Engineering - Automated Report Emails
#
# Standalone script, separate from app.R on purpose - it does NOT
# need the Shiny app to be running. It reads straight from the same
# Google Sheet the app syncs to (Inventory / Invoices / Plant
# History tabs), so as long as sync is turned on in app.R, this
# script always has current data to work from.
#
# Three modes, picked by the first command-line argument:
#   Rscript send_reports.R daily     -> urgent MOT/Warranty reminders
#   Rscript send_reports.R weekly    -> weekly activity report
#   Rscript send_reports.R monthly   -> monthly spend/fleet report
#
# ---- FIRST TIME SETUP ----
#   install.packages(c("googlesheets4", "dplyr", "blastula"))
#
# 1. SERVICE ACCOUNT - use the SAME dedicated service account you
#    set up for app.R's Sheets sync (a separate one from any other
#    app). It just needs Viewer access to the Sheet this time, since
#    this script only reads.
#
# 2. EMAIL SENDING - this uses blastula + Gmail SMTP. One-time setup:
#      library(blastula)
#      create_smtp_creds_file(
#        file = "email_creds",
#        user = "your-sending-address@gmail.com",
#        provider = "gmail"
#      )
#    This asks for a Gmail "App Password" (not your normal password -
#    generate one at myaccount.google.com/apppasswords, needs 2-Step
#    Verification turned on first) and saves it encrypted to a file
#    called "email_creds" in this folder. Never put the password
#    directly in this script.
#    (Using a proper transactional email service like SendGrid or
#    Postmark instead of Gmail is worth it once volume/reliability
#    matters more - same blastula::smtp_send() call, different
#    creds setup. Fine to start with Gmail.)
#
# 3. SCHEDULING - this script does nothing by itself; something has
#    to run it on a timer. Options, roughly easiest to most robust:
#      a) macOS cron/launchd on a Mac that's reliably on at the
#         scheduled time (simplest, but silently does nothing if
#         the Mac is asleep/off).
#      b) A small always-on Linux VM (e.g. a $5-6/month box) with a
#         real crontab - the standard approach once (a) isn't good
#         enough.
#      c) Posit Connect, if you ever move the app there - has
#         built-in scheduled execution for exactly this.
#    Example crontab (edit with `crontab -e`) for option (a)/(b):
#      0 7 * * *   cd "/path/to/PMK App" && /usr/local/bin/Rscript send_reports.R daily   >> logs/daily.log   2>&1
#      0 7 * * 1   cd "/path/to/PMK App" && /usr/local/bin/Rscript send_reports.R weekly  >> logs/weekly.log  2>&1
#      0 7 1 * *   cd "/path/to/PMK App" && /usr/local/bin/Rscript send_reports.R monthly >> logs/monthly.log 2>&1
#    (7am daily, 7am every Monday, 7am on the 1st of the month. Make
#    a "logs" folder next to this script first, or drop the log
#    redirection.)
#
# ---- RUNNING LOCALLY vs DEPLOYED ----
# Same rule as app.R: if this ever runs anywhere other than your own
# machine (a VM, Posit Connect, etc.) reached via a public GitHub
# repo, do NOT commit sheets_service_account.json or email_creds to
# it. Set two environment variables in whatever's running this
# instead - GOOGLE_SHEETS_KEY_JSON (paste the whole .json key file
# contents) and SMTP_PASSWORD (the Gmail App Password) - and the
# code below will use those automatically. Locally, it just falls
# back to the files, same as before.
# ============================================================
library(googlesheets4)
library(dplyr)
library(blastula)

# ---- CONFIG - separate service account/Sheet from any other app ----
SHEETS_SERVICE_ACCOUNT_JSON <- "sheets_service_account.json"
SHEETS_SPREADSHEET_ID <- "1ige-Yigs_Qp8aWRBxPy9fR3_sJjhe5fZO3ZrUbCQmZQ"
EMAIL_CREDS_FILE <- "email_creds"
EMAIL_FROM <- "your-sending-address@gmail.com"
REPORT_RECIPIENTS <- c("jack.doc13@outlook.com")  # add more addresses as needed

key_json_env <- Sys.getenv("GOOGLE_SHEETS_KEY_JSON", unset = "")
if (nzchar(key_json_env)) {
  tmp_key <- tempfile(fileext = ".json")
  writeLines(key_json_env, tmp_key)
  gs4_auth(path = tmp_key)
} else {
  gs4_auth(path = SHEETS_SERVICE_ACCOUNT_JSON)
}

# ---- Data loading ----
load_inventory <- function() read_sheet(SHEETS_SPREADSHEET_ID, sheet = "Inventory", col_types = "c")
load_invoices  <- function() {
  d <- read_sheet(SHEETS_SPREADSHEET_ID, sheet = "Invoices", col_types = "c")
  if (nrow(d) > 0) d$Amount <- as.numeric(d$Amount)
  d
}
load_history   <- function() read_sheet(SHEETS_SPREADSHEET_ID, sheet = "Plant History", col_types = "c")

# ---- Date helper - same loose DD/MM/YY(YY) parsing as app.R ----
parse_flex_date <- function(x) {
  out <- as.Date(rep(NA, length(x)))
  for (i in seq_along(x)) {
    v <- trimws(x[i])
    if (is.na(v) || v == "") next
    d <- suppressWarnings(as.Date(v, format = "%d/%m/%Y"))
    if (is.na(d)) d <- suppressWarnings(as.Date(v, format = "%d/%m/%y"))
    out[i] <- d
  }
  out
}
due_within <- function(inventory_df, days) {
  df <- inventory_df
  df$MOTParsed <- parse_flex_date(df$MOTDue)
  df$WarrantyParsed <- parse_flex_date(df$WarrantyEndDate)
  today <- Sys.Date()
  mot_due <- df[!is.na(df$MOTParsed) & df$MOTParsed >= today & df$MOTParsed <= today + days, ]
  if (nrow(mot_due) > 0) { mot_due$DueType <- "MOT"; mot_due$DueDate <- mot_due$MOTParsed }
  warr_due <- df[!is.na(df$WarrantyParsed) & df$WarrantyParsed >= today & df$WarrantyParsed <= today + days, ]
  if (nrow(warr_due) > 0) { warr_due$DueType <- "Warranty"; warr_due$DueDate <- warr_due$WarrantyParsed }
  out <- bind_rows(mot_due, warr_due)
  if (nrow(out) > 0) out <- out %>% arrange(DueDate)
  out
}
overdue <- function(inventory_df) {
  df <- inventory_df
  df$MOTParsed <- parse_flex_date(df$MOTDue)
  df$WarrantyParsed <- parse_flex_date(df$WarrantyEndDate)
  today <- Sys.Date()
  mot_over <- df[!is.na(df$MOTParsed) & df$MOTParsed < today, ]
  if (nrow(mot_over) > 0) { mot_over$DueType <- "MOT"; mot_over$DueDate <- mot_over$MOTParsed }
  warr_over <- df[!is.na(df$WarrantyParsed) & df$WarrantyParsed < today, ]
  if (nrow(warr_over) > 0) { warr_over$DueType <- "Warranty"; warr_over$DueDate <- warr_over$WarrantyParsed }
  out <- bind_rows(mot_over, warr_over)
  if (nrow(out) > 0) out <- out %>% arrange(DueDate)
  out
}
due_table_html <- function(d) {
  if (nrow(d) == 0) return("<p>Nothing to flag.</p>")
  d <- d %>% mutate(
    Item = ifelse(is.na(Machine) | Machine == "", ItemID, Machine),
    `PMK/Reg` = ifelse(!is.na(PMK_Number) & PMK_Number != "", PMK_Number, Registration)
  )
  rows <- paste0(
    "<tr><td>", d$Item, "</td><td>", d$`PMK/Reg`, "</td><td>", d$DueType, "</td><td>", as.character(d$DueDate), "</td></tr>",
    collapse = ""
  )
  paste0(
    "<table style='border-collapse:collapse;width:100%;font-size:14px;'>",
    "<tr style='background:#0B4D3A;color:#fff;text-align:left;'><th style='padding:6px;'>Item</th><th style='padding:6px;'>PMK/Reg</th><th style='padding:6px;'>Type</th><th style='padding:6px;'>Due Date</th></tr>",
    rows, "</table>"
  )
}

# ---- Report builders ----
build_daily_email <- function() {
  inv_df <- load_inventory()
  due7 <- due_within(inv_df, 7)
  overdue_items <- overdue(inv_df)
  compose_email(
    body = md(paste0(
      "## PMK Plant Tracker - Daily Reminder\n\n",
      "**", Sys.Date(), "**\n\n",
      if (nrow(overdue_items) > 0) paste0("### Overdue (act now)\n", due_table_html(overdue_items), "\n\n") else "",
      "### Due for MOT or Warranty within 7 days\n", due_table_html(due7)
    ))
  )
}
build_weekly_email <- function() {
  inv_df <- load_inventory()
  invoices_df <- load_invoices()
  history_df <- load_history()
  week_start <- Sys.Date() - as.integer(format(Sys.Date(), "%u")) + 1
  week_end <- week_start + 6
  invoices_df$DateParsed <- suppressWarnings(as.Date(invoices_df$Date))
  wr_invoices <- invoices_df[!is.na(invoices_df$DateParsed) & invoices_df$DateParsed >= week_start & invoices_df$DateParsed <= week_end, ]
  history_df$DateOnly <- suppressWarnings(as.Date(substr(history_df$DateTime, 1, 10)))
  wr_history <- history_df[!is.na(history_df$DateOnly) & history_df$DateOnly >= week_start & history_df$DateOnly <= week_end, ]
  due7 <- due_within(inv_df, 7)
  n_invoices <- nrow(wr_invoices)
  spend <- if (n_invoices > 0) sum(wr_invoices$Amount, na.rm = TRUE) else 0
  compose_email(
    body = md(paste0(
      "## PMK Plant Tracker - Weekly Report\n\n",
      "**Week of ", week_start, " to ", week_end, "**\n\n",
      "Invoices logged: **", n_invoices, "** (£", sprintf("%.2f", spend), ")  \n",
      "History entries logged: **", nrow(wr_history), "**\n\n",
      "### Due for MOT or Warranty within 7 days\n", due_table_html(due7)
    ))
  )
}
build_monthly_email <- function() {
  inv_df <- load_inventory()
  invoices_df <- load_invoices()
  invoices_df$DateParsed <- suppressWarnings(as.Date(invoices_df$Date))
  month_start <- as.Date(format(Sys.Date(), "%Y-%m-01"))
  month_end <- seq(month_start, by = "1 month", length.out = 2)[2] - 1
  mr_invoices <- invoices_df[!is.na(invoices_df$DateParsed) & invoices_df$DateParsed >= month_start & invoices_df$DateParsed <= month_end, ]
  total_spend <- if (nrow(mr_invoices) > 0) sum(mr_invoices$Amount, na.rm = TRUE) else 0
  top_companies <- if (nrow(mr_invoices) > 0) {
    mr_invoices %>% group_by(Company) %>% summarise(Total = sum(Amount, na.rm = TRUE), Invoices = n()) %>%
      arrange(desc(Total)) %>% head(5)
  } else NULL
  top_companies_html <- if (is.null(top_companies) || nrow(top_companies) == 0) "<p>No invoices logged this month.</p>" else {
    rows <- paste0("<tr><td>", top_companies$Company, "</td><td>£", sprintf("%.2f", top_companies$Total), "</td><td>", top_companies$Invoices, "</td></tr>", collapse = "")
    paste0("<table style='border-collapse:collapse;width:100%;font-size:14px;'>",
           "<tr style='background:#0B4D3A;color:#fff;text-align:left;'><th style='padding:6px;'>Company</th><th style='padding:6px;'>Total</th><th style='padding:6px;'>Invoices</th></tr>",
           rows, "</table>")
  }
  fleet_summary <- inv_df %>% group_by(Category) %>%
    summarise(Total = n(), Active = sum(Active == "Yes"), `On Hire` = sum(OnHire == "Yes"))
  fleet_html <- paste0("<table style='border-collapse:collapse;width:100%;font-size:14px;'>",
    "<tr style='background:#0B4D3A;color:#fff;text-align:left;'><th style='padding:6px;'>Category</th><th style='padding:6px;'>Total</th><th style='padding:6px;'>Active</th><th style='padding:6px;'>On Hire</th></tr>",
    paste0("<tr><td>", fleet_summary$Category, "</td><td>", fleet_summary$Total, "</td><td>", fleet_summary$Active, "</td><td>", fleet_summary$`On Hire`, "</td></tr>", collapse = ""),
    "</table>")
  due30 <- due_within(inv_df, 30)
  compose_email(
    body = md(paste0(
      "## PMK Plant Tracker - Monthly Report\n\n",
      "**", format(month_start, "%B %Y"), "**\n\n",
      "Total spend: **£", sprintf("%.2f", total_spend), "**  \n",
      "Invoices logged: **", nrow(mr_invoices), "**\n\n",
      "### Top Suppliers\n", top_companies_html, "\n\n",
      "### Fleet Summary\n", fleet_html, "\n\n",
      "### Due for MOT or Warranty within 30 days\n", due_table_html(due30)
    ))
  )
}

# ---- Dispatch ----
args <- commandArgs(trailingOnly = TRUE)
report_type <- if (length(args) >= 1) tolower(args[1]) else "weekly"
email <- switch(report_type,
  "daily" = build_daily_email(),
  "weekly" = build_weekly_email(),
  "monthly" = build_monthly_email(),
  stop("Unknown report type '", report_type, "' - use daily, weekly, or monthly.")
)
subject <- switch(report_type,
  "daily" = paste0("PMK Plant Tracker - Daily Reminder - ", Sys.Date()),
  "weekly" = paste0("PMK Plant Tracker - Weekly Report - ", Sys.Date()),
  "monthly" = paste0("PMK Plant Tracker - Monthly Report - ", format(Sys.Date(), "%B %Y"))
)
smtp_password_env <- Sys.getenv("SMTP_PASSWORD", unset = "")
smtp_creds <- if (nzchar(smtp_password_env)) {
  creds_envvar(user = EMAIL_FROM, pass_envvar = "SMTP_PASSWORD", provider = "gmail")
} else {
  creds_file(EMAIL_CREDS_FILE)
}
smtp_send(
  email,
  from = EMAIL_FROM,
  to = REPORT_RECIPIENTS,
  subject = subject,
  credentials = smtp_creds
)
message("Sent ", report_type, " report to: ", paste(REPORT_RECIPIENTS, collapse = ", "))
