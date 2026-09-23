# ============================================================
# PMK Civil Engineering - Plant & Invoice Tracker (VERSION 7)
#
# What's new this version:
#  - Inventory List rebuilt to match the real fleet spreadsheet:
#    Category > Sub-Category > item, shown as nested drop-down
#    accordions so ~150 items don't turn into a giant wall of text.
#  - Real inventory data imported (155 items across Excavator,
#    Breaker, Trailer, Misc, Vehicle categories). No more mock data.
#  - Admins can now Add, Edit, and Delete plant items directly from
#    the Inventory List (small Edit/Delete links on each row) -
#    the separate Admin tab has been folded into Inventory List.
#  - Plant Whereabouts (Gang Sheets) updated to match the new
#    Category/Sub-Category structure.
#  - Invoices module (Company/Date/Amount/Reference + optional
#    Invoice/Account/Document/SPEN numbers) unchanged from last
#    version.
#
# A note on the imported data: dates were mapped by column
# position from the pasted spreadsheet (Date Purchased / Warranty
# End Date / MOT Due). Worth a quick spot-check of a few rows via
# the new Edit button since a couple of rows only had one date
# filled in and column alignment for those is easy to get wrong
# when pasting from Excel.
#
# ---- FIRST TIME SETUP ----
#   install.packages(c("shiny", "shinymanager", "bslib", "dplyr",
#                       "plotly", "scales", "googlesheets4"))
# Then open this file in RStudio and click "Run App".
#
# ---- GOOGLE SHEETS SYNC SETUP (one-time, does NOT need RStudio) ----
# The app is the only place anyone edits data - every change here
# pushes out to a Google Sheet automatically so the team can view or
# build reports from it without touching the app. The Sheet itself
# is NOT editable input for the app (edits made directly in the
# Sheet will just get overwritten on the next sync).
#   1. In Google Cloud Console, create a NEW project just for this
#      app (don't reuse an existing project/service account from
#      another app, even if you've done this before elsewhere -
#      keep them separate) and enable the "Google Sheets API".
#   2. IAM & Admin > Service Accounts > Create Service Account.
#   3. Open that service account > Keys > Add Key > Create new key
#      (JSON) - this downloads a .json file. Keep it private, same
#      rules as a password. Put it in this same folder as app.R and
#      name it "sheets_service_account.json" (or update the path
#      below to match whatever you called it).
#   4. Create (or reuse) a Google Sheet for PMK data, then Share it
#      with the service account's email - it looks like
#      "xxxx@your-project.iam.gserviceaccount.com" and is inside the
#      JSON file under "client_email" - give it Editor access.
#   5. Copy the Sheet's ID out of its URL: the long string between
#      "/d/" and "/edit". Paste it into SHEETS_SPREADSHEET_ID below.
#   6. Set SHEETS_SYNC_ENABLED to TRUE below once 1-5 are done. Leave
#      it FALSE to run fully offline (e.g. while testing locally).
#
# ---- RUNNING LOCALLY vs DEPLOYED (Posit Connect Cloud) ----
# Locally, the two lines below just point at a JSON key file sitting
# next to app.R - simplest for testing.
# Once this goes on Posit Connect Cloud, the GitHub repo it deploys
# from is PUBLIC, so the JSON key file must NEVER be committed to it.
# Instead, set an environment variable called GOOGLE_SHEETS_KEY_JSON
# in Connect Cloud's "Environment variables" settings for this app,
# pasting the ENTIRE contents of the .json key file as its value.
# The code below automatically prefers that env var when it's set,
# and only falls back to the local file for local testing.
#
# The starting inventory list is no longer typed into this file
# either (same reason - keeps real fleet/driver data out of the
# public repo). It lives in initial_inventory_seed.csv, which is
# listed in .gitignore so it never gets pushed to GitHub. Once
# Sheets sync is switched on, the app loads its starting data
# from the Google Sheet instead of that CSV anyway - the CSV is
# purely a local fallback for running the app before Sheets is
# set up.
# ============================================================
library(shiny)
library(shinymanager)
library(bslib)
library(dplyr)
library(plotly)
library(scales)
library(grid)  # base R package (ships with every R install) - used for
                # the PDF report; deliberately NOT gridExtra, which
                # Posit Connect Cloud's fixed package set doesn't include.
SHEETS_SYNC_ENABLED <- TRUE
SHEETS_SERVICE_ACCOUNT_JSON <- "sheets_service_account.json"
SHEETS_SPREADSHEET_ID <- "1ige-Yigs_Qp8aWRBxPy9fR3_sJjhe5fZO3ZrUbCQmZQ"
if (SHEETS_SYNC_ENABLED) {
  library(googlesheets4)
  key_json_env <- Sys.getenv("GOOGLE_SHEETS_KEY_JSON", unset = "")
  if (nzchar(key_json_env)) {
    # Deployed: env var holds the raw JSON key contents - write it to
    # a temp file for this R session only, never saved to disk in the repo.
    tmp_key <- tempfile(fileext = ".json")
    writeLines(key_json_env, tmp_key)
    googlesheets4::gs4_auth(path = tmp_key)
  } else {
    # Local testing: read the JSON key file straight off disk.
    googlesheets4::gs4_auth(path = SHEETS_SERVICE_ACCOUNT_JSON)
  }
}
sync_to_sheets <- function(df, tab_name) {
  if (!SHEETS_SYNC_ENABLED) return(invisible(NULL))
  tryCatch({
    googlesheets4::sheet_write(df, ss = SHEETS_SPREADSHEET_ID, sheet = tab_name)
    TRUE
  }, error = function(e) {
    message("Sheets sync (", tab_name, ") failed: ", conditionMessage(e))
    FALSE
  })
}
# Loads the starting Inventory/Invoices/Plant History data. Prefers
# the live Google Sheet (so app restarts on a host don't lose data -
# without this, every restart would silently reset to empty/seed
# data even though the Sheet still has everything). Falls back to
# the local CSV seed (inventory only) if Sheets sync is off, or to
# an empty table if a Sheets read fails (e.g. tab not created yet).
# ---------------------------------------------------------------
# CONCURRENT-EDIT MERGE
# Sync used to overwrite a whole Sheet tab with this session's own
# in-memory copy. With two people logged in at once, whoever saved last
# silently wiped anything the other had added since they logged in - no
# error, no warning, and nobody notices until something's missing.
#
# Now every sync re-reads the tab and applies only what THIS session
# actually changed on top of what's already there. What changed is
# worked out by diffing the live table against a baseline snapshot
# (taken at load, and refreshed after each successful sync) rather than
# by asking each edit path to declare itself - so a new edit path
# physically cannot forget to opt in, which is the failure mode a
# hand-maintained dirty-list would have had.
#
# Two people editing DIFFERENT rows now both keep their work. Two people
# editing the SAME row is still last-writer-wins, but only for that one
# row instead of the entire table.
#
# These three are deliberately pure functions of their arguments (no
# Sheets calls, no reactives) so the merge logic can be tested directly.
# ---------------------------------------------------------------
row_signature <- function(df) {
  if (nrow(df) == 0) return(character(0))
  do.call(paste, c(lapply(df, as.character), sep = "\r"))
}
# Keys whose row is new or altered since the baseline, and keys the
# baseline had that have since been deleted here.
diff_keys <- function(local_df, baseline_df, key_col) {
  lk <- as.character(local_df[[key_col]])
  bk <- as.character(baseline_df[[key_col]])
  ls <- row_signature(local_df)
  bs <- row_signature(baseline_df); names(bs) <- bk
  is_new <- !(lk %in% bk)
  # bs[lk] is NA for new keys, but is_new is already TRUE there and
  # TRUE || NA is TRUE, so no NA survives into the result.
  altered <- is_new | (ls != bs[lk])
  list(touched = unique(lk[altered]), deleted = setdiff(bk, lk))
}
# Keyed tables (Inventory, Invoices, Plant History, GangMeta): remote
# wins for every row this session didn't touch.
merge_rows <- function(remote_df, local_df, baseline_df, key_col) {
  d <- diff_keys(local_df, baseline_df, key_col)
  keep <- !(as.character(remote_df[[key_col]]) %in% c(d$touched, d$deleted))
  mine <- local_df[as.character(local_df[[key_col]]) %in% d$touched, , drop = FALSE]
  out <- dplyr::bind_rows(remote_df[keep, , drop = FALSE], mine)
  if (nrow(out) > 0) out <- out[order(as.character(out[[key_col]])), , drop = FALSE]
  rownames(out) <- NULL
  list(df = out, changed = length(d$touched) > 0 || length(d$deleted) > 0)
}
# Append-only tables (Notifications): nothing is ever edited or removed,
# so anything this session added that isn't upstream yet gets appended.
merge_append <- function(remote_df, local_df, baseline_df) {
  added <- local_df[!(row_signature(local_df) %in% row_signature(baseline_df)), , drop = FALSE]
  extra <- added[!(row_signature(added) %in% row_signature(remote_df)), , drop = FALSE]
  out <- dplyr::bind_rows(remote_df, extra)
  rownames(out) <- NULL
  list(df = out, changed = nrow(extra) > 0)
}
# Plain name lists (Gangers, Companies): apply this session's additions
# and removals to whatever the Sheet currently holds.
merge_names <- function(remote_names, local_names, baseline_names) {
  added <- setdiff(local_names, baseline_names)
  removed <- setdiff(baseline_names, local_names)
  out <- sort(unique(c(setdiff(remote_names, removed), added)))
  list(names = out, changed = length(added) > 0 || length(removed) > 0)
}
# Reads one tab, normalised to the expected columns. Returns an empty
# frame if the tab simply doesn't exist yet (first run), and NULL if the
# read genuinely failed - the caller must NOT write in that case, since
# merging against a blank would delete everyone else's rows.
read_sheet_tab <- function(tab_name, cols) {
  out <- tryCatch(
    googlesheets4::read_sheet(SHEETS_SPREADSHEET_ID, sheet = tab_name, col_types = "c"),
    error = function(e) e
  )
  if (inherits(out, "condition")) {
    nms <- tryCatch(googlesheets4::sheet_names(SHEETS_SPREADSHEET_ID), error = function(e) NULL)
    if (!is.null(nms) && !(tab_name %in% nms)) {
      return(setNames(as.data.frame(matrix(character(0), ncol = length(cols)),
                                    stringsAsFactors = FALSE), cols))
    }
    message("Sheets read (", tab_name, ") failed: ", conditionMessage(out))
    return(NULL)
  }
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  for (col in cols) if (!col %in% names(out)) out[[col]] <- ""
  out <- out[, cols, drop = FALSE]
  out[is.na(out)] <- ""
  out
}
normalise_invoice_amounts <- function(df) {
  df$Amount <- suppressWarnings(as.numeric(df$Amount))
  df$Amount[is.na(df$Amount)] <- 0
  df
}
load_initial_data <- function(seed_df, tab_name, sheet_cols) {
  if (SHEETS_SYNC_ENABLED) {
    out <- tryCatch(googlesheets4::read_sheet(SHEETS_SPREADSHEET_ID, sheet = tab_name, col_types = "c"), error = function(e) NULL)
    if (!is.null(out) && nrow(out) > 0) {
      for (col in sheet_cols) if (!col %in% names(out)) out[[col]] <- ""
      out <- out[, sheet_cols, drop = FALSE]
      out[is.na(out)] <- ""
      return(as.data.frame(out, stringsAsFactors = FALSE))
    }
  }
  seed_df
}
# ---------------------------------------------------------------
# LOGIN CREDENTIALS
# Real names and passwords must never be hardcoded here (this file
# lives in a public GitHub repo). Each login is read from its own
# environment variable instead, same pattern as GOOGLE_SHEETS_KEY_JSON
# above.
#   1. In Posit Connect Cloud, open this app's "Environment variables"
#      settings.
#   2. Add one variable per login, named PMK_LOGIN_<USERNAME> (all
#      caps), with the value "password|role|Display Name". E.g. for
#      the "sean" login:
#        Name:  PMK_LOGIN_SEAN
#        Value: yourNewPassword123|Admin|Full Name Here
#   3. Every PMK_LOGIN_* variable that exists becomes a login, and the
#      part after the prefix is the username - PMK_LOGIN_SEAN is the
#      "sean" login. Nothing is hardcoded here, so adding or removing
#      someone is purely a variable change in Connect Cloud: add one to
#      create a login, delete one to switch that login off. No code
#      change and no republish needed for either.
#      Current logins: sean (Admin), jack (Admin), kevin (Boss),
#      evan (Plantman). The
#      "Boss" role has the exact same access as Admin everywhere in
#      the app. The "Plantman" role can access and edit Inventory
#      List and Plant Whereabouts, plus just the Ganger List card on
#      Admin - nothing else (no Invoices/Reports/Job Cards/Plant
#      Analysis).
# For local testing (RStudio on your own computer), create a file
# called credentials_local.R next to app.R (it's in .gitignore, so it
# never gets committed) defining a `credentials` data.frame with the
# same user/password/role/name columns as before.
# ---------------------------------------------------------------
build_credentials_from_env <- function() {
  # Discovered from the environment rather than from a hardcoded list -
  # a list here would go stale the moment someone is added or removed in
  # Connect Cloud, and it did.
  env <- Sys.getenv()
  keys <- names(env)[startsWith(names(env), "PMK_LOGIN_")]
  rows <- lapply(keys, function(k) {
    u <- tolower(sub("^PMK_LOGIN_", "", k))
    raw <- env[[k]]
    if (!nzchar(u) || !nzchar(raw)) return(NULL)
    parts <- strsplit(raw, "\\|")[[1]]
    if (length(parts) < 3) return(NULL)
    data.frame(user = u, password = parts[1], role = trimws(parts[2]), name = trimws(parts[3]),
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}
credentials <- build_credentials_from_env()
if (is.null(credentials)) {
  if (file.exists("credentials_local.R")) {
    source("credentials_local.R", local = TRUE)
  } else {
    stop(
      "No login credentials configured. Either set the PMK_LOGIN_* ",
      "environment variables in Posit Connect Cloud (see the setup ",
      "notes just above this in app.R), or create credentials_local.R ",
      "for local testing (it's gitignored, so it stays off GitHub)."
    )
  }
}
CATEGORY_OPTIONS <- c("Excavator", "Breaker", "Trailer", "Misc", "Vehicle")
SUBCATEGORY_MAP <- list(
  "Excavator" = c("Large Excavator", "Small Excavator"),
  "Breaker"   = c("8T Breaker", "6T Breaker", "1T Breaker"),
  "Trailer"   = c("Trailer", "Dump Trailer"),
  "Misc"      = c("Telehandler", "Tractor", "Other", "Trailer + Winches", "Dumper"),
  "Vehicle"   = c("Van", "PMK Vehicle", "Tar Squad", "Hiab", "Flatbed", "7.5T")
)
CATEGORY_COLOUR <- function(cat) {
  switch(cat,
         "Excavator" = "#22405B",
         "Breaker"   = "#B5541A",
         "Trailer"   = "#6B4C9A",
         "Misc"      = "#5B6770",
         "Vehicle"   = "#3E7C59",
         "#5B6770"
  )
}
# ---------------------------------------------------------------
# GANG SHEETS / GANGERS - currently switched off
# Plant Whereabouts (gang sheets), the Ganger list, and the Gang field
# shown on plant items are HIDDEN, not deleted. Every bit of the code is
# still here and the data is untouched: the Gang column stays on each
# inventory item, GangMeta keeps syncing to the Sheet, and gang
# assignments already recorded stay exactly as they are.
#
# Flip this back to TRUE and the whole lot reappears as it was - the
# Plant Whereabouts sub-tab, its Home quick link, the Ganger List card
# on Admin, the Gang line on an item's page, and the Gang column in
# Admin's "Machines With No Driver" table. Nothing else needs changing.
# ---------------------------------------------------------------
GANG_FEATURES_ENABLED <- FALSE
ENTRY_TYPES <- c("Driver Assigned", "Hours Updated", "Damage", "Refurbished", "Mechanic Work", "Note", "Service Inspection", "Job Card", "Truck Service")
# Everything that can actually appear in Plant History, for filtering
# and analysis. "Invoice" isn't in ENTRY_TYPES because it's never picked
# by hand - it's created automatically when an invoice is logged against
# an item - but it's still a real entry type once it's there.
ALL_ENTRY_TYPES <- sort(unique(c(ENTRY_TYPES, "Invoice")))
# "Invoice" is deliberately NOT in this list - Invoice history entries
# are only ever created automatically from the Add Invoice form (see
# ni_submit), which fills in Company/Amount/etc. Exposing it as a
# manually pickable type here would show a bare, mismatched form with
# none of those fields.
# Matches "Plant/Trailer Safety Inspection Checklist Form 32" - grouped
# exactly as the paper form's sections, items 1-22.
SERVICE_CHECKLIST <- list(
  "Ground Level Items" = c("Oil Level", "Water Level", "Hydraulic Oil", "Fuel Level",
                           "Wheels/Tyres/Tracks", "Wheel Nuts/Track Tension", "Coupling/Jockey Wheel/Legs",
                           "Breakaway Cable/Hoses/Rams", "Body Condition/Cab Security/Glass/Mirrors",
                           "Doors/Locks/Ramps/Floor/Wings/Seat/Seat Belt"),
  "Electrical" = c("Conditions of Wiring/Couplings", "Battery Security/Terminals",
                   "Lights/Switches/Ignition", "Wipers/Washers/Horn", "Safety Devices/Stop Switch"),
  "Chassis" = c("Condition/Corrosion/Nuts/Bolts"),
  "Brakes" = c("Condition/Corrosion/Nuts/Bolts", "Efficiency %"),
  "Suspension" = c("Springs/Shock Absorbers", "Attachment of Units/Bump Stops U/Bolts"),
  "Attachments/Rammer" = c("Bucket/Safety Pins/Oil Leaks", "Warning Notices")
)
# Flat 1-22 view of SERVICE_CHECKLIST, so each item has a stable number,
# a unique id (item text alone isn't unique - "Condition/Corrosion/Nuts/
# Bolts" is both 16 under Chassis and 17 under Brakes) and a display
# label. Used by the Non Applicable picker and the per-item defect boxes.
SERVICE_CHECKLIST_FLAT <- local({
  out <- list(); n <- 0
  for (sec in names(SERVICE_CHECKLIST)) {
    for (it in SERVICE_CHECKLIST[[sec]]) {
      n <- n + 1
      out[[length(out) + 1]] <- list(
        n = n, section = sec, item = it,
        id = paste0(make.names(sec), "__", n),
        long_label = paste0(n, ". ", sec, " - ", it)
      )
    }
  }
  out
})
SERVICE_ITEM_CHOICES <- setNames(
  vapply(SERVICE_CHECKLIST_FLAT, function(x) x$id, character(1)),
  vapply(SERVICE_CHECKLIST_FLAT, function(x) x$long_label, character(1))
)
# Matches the Logistics UK "Maintenance Inspection - Motor Vehicles" pad -
# a separate, much longer checklist from Form 32 above, used for the
# 6-weekly HGV/truck safety inspection rather than plant/trailer.
TRUCK_SERVICE_CHECKLIST <- list(
  "Inside Cab" = c("Engine MIL", "Reagent (AdBlue)", "DfT Plate - Condition/Details",
                   "Speed Limiter Plate - Condition/Details", "Seat Belts and Supplementary Restraint Systems",
                   "Cab Floor and Steps", "Seats", "Other Seats and Crew Amenities", "Mirrors (Internal)",
                   "View to Front", "Condition of Glass (Screen/Windows)", "Windscreen Wipers and Washers",
                   "Speedometer/Tachograph - Operation/Seals", "Engine Tachometer", "Audible Warning - Horn",
                   "Driving Controls", "Steering Control - Free Play", "Steering Wheel - Security/Condition",
                   "Steering Column", "Anti-Theft Locks", "Pressure/Vacuum - Warning", "Pressure/Vacuum - Build Up",
                   "Other Gauges - Warning Devices", "Hand Lever/Electronic Park Brake Control", "Service Brake Pedal",
                   "Service Brake Operation/Anti-Lock Warning", "Hand Operated Brake Control Valves",
                   "Electrical Wiring/Equipment/Switches", "Front Fog and Aux Lamp Switches",
                   "Panel/Interior Lamps and Switches", "Cab Heater/Demister/Air Conditioning"),
  "Cab Exterior" = c("Bumper (Front)", "Condition of Wings/Spray Suppression/Wheel Arches (Front)",
                     "Cab Panels, Trim and Heated Mirrors", "Cab Security Including Tilt Warning",
                     "Cab Doors Including Hinges, Locks", "Cab Floor and Steps (Exterior Access)",
                     "Mirrors and Indirect Vision Devices (External)", "Front Lamps (Side Lamps) and Outline Markers",
                     "Headlamp Cleaning Device", "Day Time Running Lamps",
                     "Headlamps - Operation/Aim/Adjustment Mechanisms", "Front Fog Lamps/Spot Lamps/Dim-Dip Device"),
  "Engine Compartment" = c("Engine/Transmission Mountings", "Oil Leaks", "Fuel Tanks and Systems",
                           "Exhaust Systems", "Exhaust Brake", "Radiator Mounting", "Cooling System", "Fan, Generator, Aux Belts",
                           "Fuel Pump Linkage Seals and EDC Equipment", "Speed Limiter - Condition/Seals/Linkage",
                           "Injectors, Pipes, Filters", "Air Intake System - Turbocharger/Intercooler and Filters",
                           "Air Compressor - Exhauster - Drive Belts"),
  "Ground Level" = c("Road Wheels and Hubs", "Sideguards, Rear Underrun Devices and Bumper Bars",
                     "Spare Wheel Carrier (and Spare Wheel)", "Vehicle to Trailer Coupling",
                     "Wings/Spray Suppression (Rear)/Wheel Arches - Condition", "Security/Condition of Body",
                     "Demountable Bodies - Chassis Mounted Equipment", "Security of Body, Containers and Crane Support Legs",
                     "Tipping Gear - Hydraulic Rams, Pivots and Safety Devices", "Tailboard Hoists", "Cranes, Gantries",
                     "Other Ancillary Equipment"),
  "Under/Alongside Vehicle" = c("Chassis - Condition", "Electrical Wiring and Equipment Including Batteries",
                                "Electrical Connections for Trailer", "Oil Leaks (Underside)", "Fuel Tanks and Systems (Underside)",
                                "Exhaust System (Underside)", "Suspension Pins and Bushes - Condition",
                                "Suspension Units and Linkages - Condition", "Spring Units, Linkages and Sub-Frames - Security",
                                "Shock Absorbers", "Wheel Bearings and Seals (Rear)", "Axles/Stub Axles and Wheel Bearings",
                                "Steering Mechanism", "Steering Alignment", "Power Steering and Fluid Level", "Axle Alignment",
                                "Clutch Operation", "Gearbox and Bell Housing", "Change Speed Mechanism", "Power Take-Off",
                                "Final Drive", "2-Speed Shift Mechanism", "Differential Lock - Traction Control",
                                "Load Transfer/Axle Lift Device", "Transmission - Drive Line/Mountings"),
  "Brakes" = c("Electronic Braking System/Electronic Stability Control - Operation/Warning",
               "Hydraulic Fluid Level", "Mechanical Brake Components", "Drums and Linings/Discs and Pads",
               "Brake Actuators and Adjusters", "Brake Systems and Components",
               "Trailer Couplings, Hoses and Function of Self-Sealing Valves", "Load Sensing/Anti-Lock System",
               "Air Brake Anti-Freeze Device", "Anti-Lock Device - Operation/Warning", "Operation of Supply Dump Valve",
               "Operation of Multi-Circuit Protection", "Additional Braking Devices"),
  "Lamps, Markings & Bodywork" = c("Rear Markings and Conspicuity Markings",
                                   "Rear Lamps, Outline Markers and Number Plate Lamp", "Rear Fog Lamps Including Warning Device",
                                   "Reflectors (Side and Rear)", "Direction/Hazard Indicators Including Warning Device",
                                   "Side Marker Lamps", "Stop Lamps", "Reversing Lamps", "Position Lamps, Headlamps and Warning Device",
                                   "Other Lamps", "Paintwork and Livery - Condition"),
  "Licences & Other" = c("Licences", "Legal Writing", "Registration Plates", "Other Dangerous Defects",
                         "Exhaust Emission"),
  "Tyres" = c("Size and Type of Tyres", "Condition of Tyres")
)
# ---------------------------------------------------------------
# DATE HELPERS - inventory dates are free-text (DD/MM/YY or
# DD/MM/YYYY) rather than strict Date columns, since that's how
# they came in off the spreadsheet. These helpers parse them
# loosely for the Weekly/Monthly Reports below.
# ---------------------------------------------------------------
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
floor_to_monday <- function(d) { d - (as.integer(format(d, "%u")) - 1) }
# Escapes a single quote for safe embedding inside a JS string literal
# delimited by single quotes (used for the Delete links' onclick=
# handlers below) - without this, a name like "Charlie O'Donnel"
# breaks the embedded JavaScript and makes that Delete link a no-op.
# strwrap() treats a single "\n" as ordinary whitespace, so multi-line
# history descriptions (every Service Inspection, Job Card etc) came out
# of the PDF as one run-on paragraph. This keeps each line on its own
# line and only wraps the ones that are too long.
wrap_lines <- function(x, width) {
  if (is.na(x) || x == "") return(character(0))
  unlist(lapply(strsplit(x, "\n", fixed = TRUE)[[1]], function(l) {
    w <- strwrap(l, width = width)
    if (length(w) == 0) "" else w
  }))
}
js_escape_sq <- function(x) gsub("'", "\\\\'", x, fixed = TRUE)
# ---------------------------------------------------------------
# COMPANY LIST - suppliers/garages used for the invoice Company
# dropdown, managed by Admin (add/delete), same pattern as Gangers.
# Deliberately empty - real supplier names live only in the Google
# Sheet / Admin UI, not hardcoded in this file (it lives in a public
# GitHub repo).
# ---------------------------------------------------------------
companies_seed <- data.frame(Name = character(0), stringsAsFactors = FALSE)
# ---------------------------------------------------------------
# INVENTORY - local fallback seed only, loaded from
# initial_inventory_seed.csv (real fleet data lives there, not in
# this script, so it never ends up in the public GitHub repo). This
# CSV is listed in .gitignore. If it's missing (e.g. a fresh clone
# of the repo before Sheets sync is set up), the app just starts
# with an empty inventory - use Add Item, or set up Sheets sync, to
# bring data in.
# ---------------------------------------------------------------
INVENTORY_SEED_CSV <- "initial_inventory_seed.csv"
inventory_cols <- c("ItemID", "Category", "SubCategory", "Machine", "PMK_Number",
                    "Registration", "SerialNumber", "Driver", "Location", "Gang",
                    "DatePurchased", "WarrantyEndDate", "MOTDue", "Active", "Notes",
                    "TruckServiceRequired", "Hours")
inventory_seed <- if (file.exists(INVENTORY_SEED_CSV)) {
  df <- read.csv(INVENTORY_SEED_CSV, colClasses = "character", stringsAsFactors = FALSE)
  df[is.na(df)] <- ""
  # TruckServiceRequired is a newer column - default existing rows to "No"
  # if the CSV predates it, so old exports still load fine.
  if (!"TruckServiceRequired" %in% names(df)) df$TruckServiceRequired <- "No"
  # Hours is a newer column too - default existing rows to blank
  # (unknown) rather than 0, so it's obvious it's never been logged.
  if (!"Hours" %in% names(df)) df$Hours <- ""
  df[, inventory_cols]
} else {
  setNames(data.frame(matrix(character(0), ncol = length(inventory_cols))), inventory_cols)
}
gangs_seed <- character(0)
# Ganger (foreman) name list - shows as a dropdown when creating/editing
# a gang sheet, managed by Admin (add/delete). Gang sheet metadata
# (which Ganger + Location a gang sheet has) lives separately since a
# gang sheet can exist with zero plant items ticked.
# Deliberately empty - real staff names are personal data and must
# never be hardcoded into this file (it lives in a public GitHub
# repo). The Ganger list lives only in the Google Sheet / Admin UI,
# same as Inventory and everything else real.
gangers_seed <- data.frame(Name = character(0), stringsAsFactors = FALSE)
gang_meta_seed <- data.frame(Gang = character(0), Ganger = character(0), Location = character(0), stringsAsFactors = FALSE)
# Admin-only activity log - "wee notices" of who did what (added a
# history entry, added/edited/deleted a plant item, created/updated a
# gang sheet), so Admin can see at a glance what non-Admin logins have
# been doing without digging through Inventory/Whereabouts directly.
notifications_seed <- data.frame(Time = character(0), User = character(0), Role = character(0), Action = character(0), stringsAsFactors = FALSE)
plant_history_seed <- data.frame(
  ItemID = character(0), DateTime = character(0), EntryType = character(0),
  Description = character(0), RecordedBy = character(0), InvoiceID = character(0),
  EntryID = character(0), LinkedEntryID = character(0), stringsAsFactors = FALSE
)
# Invoices - matches the real invoice spreadsheet columns. Category/
# SubCategory/Reference together link an invoice to a specific
# inventory item (same Category>SubCategory structure as Inventory,
# Reference is that item's PMK Number/Registration/Serial Number).
# InvoiceID is our own key (not from the spreadsheet) so Edit/Delete
# and the linked Plant History entry can target one specific invoice.
invoices_seed <- data.frame(
  InvoiceID = character(0),
  Company = character(0), Invoice_Number = character(0), Account_Number = character(0),
  Document_Number = character(0), Date = character(0), Amount = numeric(0),
  Description = character(0), SPEN_Order_Number = character(0),
  Category = character(0), SubCategory = character(0),
  Reference_PMK_Number = character(0), LoggedBy = character(0), stringsAsFactors = FALSE
)
# ---------------------------------------------------------------
# UI helpers
# ---------------------------------------------------------------
subcats_for <- function(cat, df) {
  predefined <- SUBCATEGORY_MAP[[cat]]
  present <- unique(df$SubCategory[df$Category == cat])
  present <- present[present != "" & !is.na(present)]
  union(predefined, present)
}
item_identifier <- function(row) {
  if (row$PMK_Number != "") row$PMK_Number
  else if (row$Registration != "") row$Registration
  else if (row$SerialNumber != "") row$SerialNumber
  else row$ItemID
}
# Sorts a data frame of inventory rows into "natural" order by
# identifier (PMK 1, PMK 2, ... PMK 10 - not the alphabetical 1, 10,
# 2), within whatever grouping the caller already filtered to (e.g.
# one Category>Sub-Category). Without this, newly added/edited items
# always land at the bottom, since they're just appended to the data.
natural_sort_rows <- function(df) {
  if (nrow(df) == 0) return(df)
  ids <- vapply(seq_len(nrow(df)), function(i) item_identifier(df[i, ]), character(1))
  nums <- vapply(ids, function(s) {
    m <- regmatches(s, regexpr("[0-9]+$", s))
    if (length(m) == 0) NA_real_ else as.numeric(m)
  }, numeric(1))
  prefix <- trimws(sub("[0-9]+$", "", ids))
  df[order(prefix, is.na(nums), nums, ids), ]
}
# Loosely normalises an identifier for matching purposes only (never
# for display/storage) - strips spaces/punctuation, upper-cases, and
# drops a leading "PMK" word. People type references by hand (e.g.
# "PMK 2", "pmk-2", "PMK2") rather than always picking from the
# dropdown, and none of those should fail to match plain "2".
normalize_ref <- function(x) {
  x <- toupper(trimws(ifelse(is.na(x), "", x)))
  x <- gsub("[^A-Z0-9]", "", x)
  x <- sub("^PMK", "", x)
  x
}
# Named choices for a selectizeInput - value is the item's
# identifier (PMK Number/Registration/Serial Number, whichever it
# has), label adds the machine name so it's recognisable in a
# dropdown. Used to link Invoices and Job Cards to a specific item.
items_for_picker <- function(cat, subcat, df) {
  rows <- natural_sort_rows(df[df$Category == cat & df$SubCategory == subcat, ])
  if (nrow(rows) == 0) return(character(0))
  ids <- vapply(seq_len(nrow(rows)), function(i) item_identifier(rows[i, ]), character(1))
  labels <- ifelse(rows$Machine != "", paste0(ids, " - ", rows$Machine), ids)
  setNames(ids, labels)
}
# entry_counts: optional named integer vector (ItemID -> number of
# History entries). When supplied, the box shows an "Entries:" line
# alongside Driver/Location.
item_row <- function(row, r, clickable = TRUE, show_actions = TRUE, entry_counts = NULL) {
  div(
    class = "plant-row",
    style = paste0("border-left-color:", CATEGORY_COLOUR(row$Category), ";"),
    role = if (clickable) "button" else NULL,
    tabindex = if (clickable) "0" else NULL,
    onclick = if (clickable) sprintf("Shiny.setInputValue('item_click', '%s', {priority:'event'})", row$ItemID) else NULL,
    onkeypress = if (clickable) "if(event.key==='Enter'||event.key===' '){this.click()}" else NULL,
    div(class = "d-flex justify-content-between align-items-center flex-wrap",
        div(
          span(class = "plate", item_identifier(row)),
          p(class = "text-muted mb-0 mt-1", ifelse(row$Machine == "", "(no machine name)", row$Machine))
        ),
        div(class = "text-end",
            p(class = "mb-1", paste("Driver:", ifelse(row$Driver == "", "Unassigned", row$Driver))),
            if (row$Location != "") p(class = "mb-1 text-muted", style = "font-size:0.8rem;", paste("Location:", row$Location)),
            if (!is.null(entry_counts)) p(class = "mb-1 text-muted", style = "font-size:0.8rem;",
                                          paste("Entries:", { n <- entry_counts[row$ItemID]; if (is.na(n)) 0L else n })),
            span(class = paste0("badge ", ifelse(row$Active == "Yes", "bg-success", "bg-secondary")), row$Active),
            if (r %in% c("Admin", "Boss", "Plantman") && show_actions) div(style = "margin-top:6px;",
                                                  tags$a(href = "#", style = "font-size:0.8rem; margin-right:10px;",
                                                         onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('edit_item_click', '%s', {priority:'event'}); return false;", row$ItemID),
                                                         "Edit"),
                                                  tags$a(href = "#", style = "font-size:0.8rem; color:#9C2B2B;",
                                                         onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('delete_item_click', '%s', {priority:'event'}); return false;", row$ItemID),
                                                         "Delete")
            )
        )
    )
  )
}
metric_card <- function(value, label, colour = "#0B4D3A") {
  div(class = "metric-card", style = paste0("border-top-color:", colour, ";"),
      div(class = "metric-value", value),
      div(class = "metric-label", label)
  )
}
nested_inventory_accordion <- function(base_id, df, r, show_actions = TRUE, clickable = TRUE, entry_counts = NULL) {
  cat_panels <- lapply(CATEGORY_OPTIONS, function(cat) {
    cat_rows <- df[df$Category == cat, ]
    subcats <- subcats_for(cat, df)
    sub_panels <- lapply(subcats, function(sub) {
      sub_rows <- natural_sort_rows(cat_rows[cat_rows$SubCategory == sub, ])
      accordion_panel(
        title = paste0(sub, " (", nrow(sub_rows), ")"),
        value = paste0(cat, "___", sub),
        if (nrow(sub_rows) == 0) p(class = "text-muted mb-0", "None.")
        else tagList(lapply(seq_len(nrow(sub_rows)), function(i) item_row(sub_rows[i, ], r, clickable = clickable, show_actions = show_actions, entry_counts = entry_counts)))
      )
    })
    accordion_panel(
      title = paste0(cat, " (", nrow(cat_rows), ")"),
      value = cat,
      if (length(sub_panels) == 0) p(class = "text-muted mb-0", "None in this category.")
      else do.call(accordion, c(list(id = paste0(base_id, "_sub_", make.names(cat))), sub_panels))
    )
  })
  do.call(accordion, c(list(id = base_id, open = FALSE), cat_panels))
}
# ---------------------------------------------------------------
# PRINTABLE JOB CARD
# Job Card entries are stored as a flat "Key: value" description (see
# build_jobcard_desc), so printing one means reading those fields back
# out. Values can run to several lines - the work descriptions are free
# text areas, and Additional Comments is deliberately written last - so
# the parser treats any line that doesn't start with a known key as a
# continuation of the field it's currently reading.
# ---------------------------------------------------------------
JOBCARD_KEYS <- c("Machine", "Job No.", "Depot", "Date Started", "Odometer/Hours",
                  "Work To Be Done", "Work Carried Out", "Time Taken", "Done By",
                  "Date Completed", "Location", "Price", "Additional Comments")
parse_entry_fields <- function(desc, keys) {
  out <- setNames(as.list(rep("", length(keys))), keys)
  if (is.null(desc) || is.na(desc) || desc == "") return(out)
  cur <- NA_character_
  for (ln in strsplit(desc, "\n", fixed = TRUE)[[1]]) {
    hit <- NA_character_
    for (k in keys) if (startsWith(ln, paste0(k, ":"))) { hit <- k; break }
    if (!is.na(hit)) {
      cur <- hit
      out[[cur]] <- trimws(substring(ln, nchar(hit) + 2))
    } else if (!is.na(cur)) {
      out[[cur]] <- paste0(out[[cur]], "\n", ln)
    }
  }
  lapply(out, trimws)
}
# One A4 portrait sheet per Job Card, drawn with base grid only - no
# gridExtra, same constraint as the rest of the reporting. Sections grow
# with their content and spill onto a second page rather than being
# truncated, so a long write-up still prints in full.
generate_jobcard_pdf <- function(file, entry, item) {
  f <- parse_entry_fields(entry$Description, JOBCARD_KEYS)
  nz <- function(x, d = "-") if (is.null(x) || length(x) == 0 || is.na(x) || trimws(x) == "") d else trimws(x)
  GREEN <- "#0B4D3A"; GOLD <- "#C9A227"; SLATE <- "#5B6770"; INK <- "#12241C"
  LINE <- "#C9C6BC"; BAND <- "#EDEAE1"
  WRAP <- 96; BOTTOM <- 0.075
  pdf(file, width = 8.27, height = 11.69)
  on.exit(dev.off(), add = TRUE)
  txt <- function(s, x, y, cex = 9, col = INK, face = "plain", just = c("left", "top"))
    grid.text(s, x = unit(x, "npc"), y = unit(y, "npc"), just = just,
              gp = gpar(fontsize = cex, col = col, fontface = face, lineheight = 1.25))
  rct <- function(x, y, w, h, fill = NA, col = LINE, lwd = 0.7)
    grid.rect(x = unit(x, "npc"), y = unit(y, "npc"), width = unit(w, "npc"), height = unit(h, "npc"),
              just = c("left", "top"), gp = gpar(fill = fill, col = col, lwd = lwd))
  hrule <- function(y, x0 = 0.07, x1 = 0.93)
    grid.lines(x = unit(c(x0, x1), "npc"), y = unit(c(y, y), "npc"), gp = gpar(col = LINE, lwd = 0.7))
  fld <- function(x, y, w, h, label, value, vcex = 9.5) {
    rct(x, y, w, h)
    txt(toupper(label), x + 0.012, y - 0.008, 6.2, SLATE, "bold")
    txt(value, x + 0.012, y - h + 0.011, vcex, INK, just = c("left", "bottom"))
  }
  header <- function(cont = FALSE) {
    grid.newpage()
    grid.rect(gp = gpar(fill = "#FFFFFF", col = NA))
    rct(0, 1, 1, 0.085, fill = GREEN, col = NA)
    rct(0, 0.915, 1, 0.006, fill = GOLD, col = NA)
    txt("PMK CIVIL ENGINEERING LTD", 0.07, 0.972, 13, "#FFFFFF", "bold")
    txt("Process 4 - Plant and Equipment", 0.07, 0.945, 8, "#D7E3DC")
    txt(if (cont) "JOB CARD (CONT.)" else "JOB CARD", 0.93, 0.972, 15, GOLD, "bold", just = c("right", "top"))
    txt(paste("Ref", nz(entry$EntryID)), 0.93, 0.946, 8, "#D7E3DC", just = c("right", "top"))
  }
  footer <- function() {
    hrule(0.055)
    txt(paste0("Logged by ", nz(entry$RecordedBy), " on ", nz(entry$DateTime),
               "  |  ", nz(item$Machine), "  |  Serial ", nz(item$SerialNumber)),
        0.07, 0.045, 7, SLATE)
    txt("PMK Plant Tracker", 0.93, 0.045, 7, SLATE, just = c("right", "top"))
  }
  sec_lines <- function(body) { l <- wrap_lines(nz(body), WRAP); if (length(l) == 0) "-" else l }
  # Box heights are MEASURED from the rendered text rather than estimated
  # from point size - an estimate was close enough for a short job card
  # and silently overflowed the box on a long one.
  th <- function(s, cex = 9)
    convertHeight(grobHeight(textGrob(s, gp = gpar(fontsize = cex, lineheight = 1.25))), "npc", valueOnly = TRUE)
  header()
  rh <- 0.046; y <- 0.885
  fld(0.07, y, 0.29, rh, "PMK Plant Number", nz(item_identifier(item)), 11)
  fld(0.36, y, 0.30, rh, "Make & Type", nz(item$Machine))
  fld(0.66, y, 0.27, rh, "Registration", nz(item$Registration))
  y <- y - rh
  fld(0.07, y, 0.29, rh, "Job No.", nz(f[["Job No."]]), 11)
  fld(0.36, y, 0.30, rh, "Depot", nz(f[["Depot"]]))
  fld(0.66, y, 0.27, rh, "Odometer / Hours", nz(f[["Odometer/Hours"]]))
  y <- y - rh
  fld(0.07, y, 0.29, rh, "Date Started", nz(f[["Date Started"]]))
  fld(0.36, y, 0.30, rh, "Date Completed", nz(f[["Date Completed"]]))
  fld(0.66, y, 0.27, rh, "Category", paste0(nz(item$Category), " > ", nz(item$SubCategory)))
  cursor <- y - rh - 0.016
  L1 <- th("A"); LH <- th("A\nA") - L1        # one line, and each extra line
  block_h <- function(n) L1 + max(n - 1, 0) * LH
  draw_sec <- function(top, heading, lines, cont = FALSE) {
    h <- max(block_h(length(lines)), block_h(3)) + 0.020
    rct(0.07, top, 0.86, 0.028, fill = BAND, col = BAND)
    txt(paste0(toupper(heading), if (cont) " (CONT.)" else ""), 0.082, top - 0.008, 7.5, GREEN, "bold")
    rct(0.07, top - 0.028, 0.86, h)
    txt(paste(lines, collapse = "\n"), 0.082, top - 0.041, 9)
    top - 0.028 - h - 0.014
  }
  # Fills the page, then carries the rest onto the next one under a
  # "(CONT.)" heading - so a long write-up is never cut off.
  place <- function(cursor, heading, lines) {
    cont <- FALSE
    repeat {
      avail <- cursor - 0.028 - 0.020 - BOTTOM
      if (avail < block_h(3)) { footer(); header(cont = TRUE); cursor <- 0.885; next }
      fits <- max(1L, as.integer(floor((avail - L1) / LH)) + 1L)
      if (length(lines) <= fits) return(draw_sec(cursor, heading, lines, cont))
      cursor <- draw_sec(cursor, heading, lines[seq_len(fits)], cont)
      lines <- lines[-seq_len(fits)]
      cont <- TRUE
      footer(); header(cont = TRUE); cursor <- 0.885
    }
  }
  secs <- list(
    list("Description of work to be done", sec_lines(f[["Work To Be Done"]])),
    list("Description of work carried out", sec_lines(f[["Work Carried Out"]])),
    list("Additional comments", sec_lines(f[["Additional Comments"]]))
  )
  for (s in secs) cursor <- place(cursor, s[[1]], s[[2]])
  y <- cursor - 0.006
  if (y - 0.175 < 0.055) { footer(); header(cont = TRUE); y <- 0.885 }
  fld(0.07, y, 0.29, rh, "Time Taken", nz(f[["Time Taken"]]))
  fld(0.36, y, 0.30, rh, "Work Done By", nz(f[["Done By"]]))
  fld(0.66, y, 0.27, rh, "Driver", nz(item$Driver, "Unassigned"))
  y <- y - rh - 0.013
  rct(0.07, y, 0.86, 0.028, fill = BAND, col = BAND)
  txt("SIGN-OFF", 0.082, y - 0.008, 7.5, GREEN, "bold")
  y2 <- y - 0.028
  rct(0.07, y2, 0.43, 0.080); rct(0.50, y2, 0.43, 0.080)
  txt("SIGNATURE - WORK CARRIED OUT BY", 0.082, y2 - 0.008, 6.2, SLATE, "bold")
  txt("SIGNATURE - CHECKED BY", 0.512, y2 - 0.008, 6.2, SLATE, "bold")
  hrule(y2 - 0.066, 0.082, 0.485); hrule(y2 - 0.066, 0.512, 0.915)
  txt(nz(f[["Done By"]]), 0.082, y2 - 0.071, 8, SLATE)
  txt("Date", 0.485, y2 - 0.071, 8, SLATE, just = c("right", "top"))
  footer()
  invisible(NULL)
}
# ---------------------------------------------------------------
# MAIN APP UI
# ---------------------------------------------------------------
app_ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#F5F6F2", fg = "#12241C",
    primary = "#C9A227", secondary = "#5B6770",
    success = "#3E7C59", warning = "#C9A227", danger = "#9C2B2B",
    base_font = font_google("Inter"),
    heading_font = font_google("Oswald")
  ),
  tags$head(
    tags$style(HTML("
      body { background:#F5F6F2; }
      .app-header {
        background:linear-gradient(135deg,#0B4D3A,#1E8A5F);
        color:#fff; padding:14px 20px; border-bottom:5px solid #C9A227;
        margin-bottom:16px;
      }
      .app-header h4 { font-family:'Oswald',sans-serif; text-transform:uppercase;
        letter-spacing:1.5px; margin:0; }
      .app-header .role-tag { font-size:0.8rem; opacity:0.9; }
      .app-header img { height:38px; margin-right:12px; vertical-align:middle; }
      .plate { display:inline-block; font-family:monospace; background:#0B4D3A;
        color:#C9A227; letter-spacing:1px; padding:3px 9px; border-radius:3px;
        font-weight:600; }
      .plant-row { cursor:pointer; background:#fff; border:1px solid #E2DFD6;
        border-left:5px solid #ccc; border-radius:4px; padding:12px 14px;
        margin-bottom:8px; transition:0.1s; }
      .plant-row:hover { border-color:#C9A227; }
      .plant-row:focus { outline:3px solid #C9A227; outline-offset:-3px; }
      .browse-tile { cursor:pointer; background:#fff; border:1px solid #E2DFD6; border-top:4px solid #0B4D3A;
        border-radius:6px; padding:18px 14px; margin-bottom:14px; text-align:center; transition:0.1s; }
      .browse-tile:hover { border-color:#C9A227; border-top-color:#C9A227; box-shadow:0 2px 6px rgba(0,0,0,0.08); }
      .browse-tile:focus { outline:3px solid #C9A227; outline-offset:-3px; }
      .browse-tile .tile-title { font-family:'Oswald',sans-serif; font-size:1.05rem; color:#0B4D3A; text-transform:uppercase; letter-spacing:0.5px; }
      .browse-tile .tile-count { color:#5B6770; font-size:0.85rem; margin-top:4px; }
      h3,h4,h5,h6 { font-family:'Oswald',sans-serif; }
      .gang-card { background:#fff; border:1px solid #E2DFD6; border-radius:4px;
        padding:14px; margin-bottom:12px; }
      .admin-card { background:#fff; border:1px solid #E2DFD6; border-radius:4px;
        padding:14px; margin-bottom:14px; }
      .invoice-card { background:#fff; border:1px solid #E2DFD6; border-radius:4px;
        padding:12px 14px; margin-bottom:8px; border-left:5px solid #C9A227; }
      .invoice-ref { font-family:monospace; font-weight:600; color:#0B4D3A; }
      .tag-chip { display:inline-block; background:#F0EEE6; border:1px solid #E2DFD6;
        border-radius:3px; padding:2px 8px; font-size:0.78rem; margin-right:4px; }
      .tag-chip.na { color:#999; font-style:italic; }
      .metric-card { background:#fff; border:1px solid #E2DFD6; border-top:4px solid #0B4D3A;
        border-radius:4px; padding:14px; text-align:center; }
      .metric-value { font-family:'Oswald',sans-serif; font-size:1.7rem; font-weight:700; color:#0B4D3A; }
      .metric-label { color:#5B6770; font-size:0.85rem; text-transform:uppercase; letter-spacing:0.5px; }
      .chart-card { background:#fff; border:1px solid #E2DFD6; border-radius:4px;
        padding:14px; margin-bottom:14px; }
      .history-item { border-left:3px solid #0B4D3A; padding:8px 12px; margin-bottom:8px;
        background:#F8F7F3; border-radius:4px; }
      .hero-logo { text-align:center; padding:30px 0 10px 0; }
      .hero-logo img { height:150px; }
      .hero-title { text-align:center; font-family:'Oswald',sans-serif; text-transform:uppercase;
        letter-spacing:2px; color:#0B4D3A; margin-top:8px; }
    "))
  ),
  div(class = "app-header",
      fluidRow(
        column(8,
               tags$img(src = "pmk_logo.webp"),
               h4("PMK Civil Engineering - Plant Tracker", style = "display:inline-block; vertical-align:middle;")
        ),
        column(4, style = "text-align:right;",
               span(class = "role-tag", textOutput("who_label", inline = TRUE)),
               actionButton("logout_btn", "Log out", class = "btn-sm btn-outline-light ms-2")
        )
      )
  ),
  div(style = "padding:0 16px;", uiOutput("main_ui"))
)
ui <- secure_app(app_ui, enable_admin = FALSE)
# ---------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------
server <- function(input, output, session) {
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  role <- reactive({ req(res_auth$role); res_auth$role })
  user_name <- reactive({ req(res_auth$name); res_auth$name })
  output$who_label <- renderText(paste0(user_name(), " (", role(), ")"))
  observeEvent(input$logout_btn, session$reload())
  # Loaded once when a session starts. If Sheets sync is on, this
  # pulls the live data back from the Sheet so restarts (which every
  # host does periodically) don't wipe the app back to empty/seed -
  # the Sheet is genuinely the persistent copy, not just a mirror.
  invoices_loaded <- load_initial_data(invoices_seed, "Invoices",
                                       c("InvoiceID", "Company", "Invoice_Number", "Account_Number", "Document_Number", "Date",
                                         "Amount", "Description", "SPEN_Order_Number", "Category", "SubCategory",
                                         "Reference_PMK_Number", "LoggedBy"))
  invoices_loaded$Amount <- suppressWarnings(as.numeric(invoices_loaded$Amount))
  invoices_loaded$Amount[is.na(invoices_loaded$Amount)] <- 0
  # InvoiceID is a newer column - back-fill unique IDs for any rows
  # loaded from a Sheet that predates it, so Edit/Delete always has
  # something unambiguous to target.
  if (nrow(invoices_loaded) > 0) {
    blank_id <- is.na(invoices_loaded$InvoiceID) | invoices_loaded$InvoiceID == ""
    if (any(blank_id)) {
      existing_nums <- suppressWarnings(as.integer(gsub("INV-", "", invoices_loaded$InvoiceID[!blank_id])))
      start_n <- if (all(is.na(existing_nums))) 1 else max(existing_nums, na.rm = TRUE) + 1
      invoices_loaded$InvoiceID[blank_id] <- sprintf("INV-%04d", seq(start_n, length.out = sum(blank_id)))
    }
  }
  inventory_loaded <- load_initial_data(inventory_seed, "Inventory", inventory_cols)
  inventory_data <- reactiveVal(inventory_loaded)
  gangers_loaded <- load_initial_data(gangers_seed, "Gangers", c("Name"))
  ganger_list <- reactiveVal(sort(unique(gangers_loaded$Name[gangers_loaded$Name != ""])))
  companies_loaded <- load_initial_data(companies_seed, "Companies", c("Name"))
  company_list <- reactiveVal(sort(unique(companies_loaded$Name[companies_loaded$Name != ""])))
  # Company pickers should never come up empty just because nobody's
  # populated the Admin > Company List card - fall back to every
  # company name that's actually been used on an invoice already, so
  # existing suppliers are always selectable/typeable even before
  # anyone curates the admin list.
  company_choices_all <- function() {
    sort(unique(c(company_list(), invoices_data()$Company[!is.na(invoices_data()$Company) & invoices_data()$Company != ""])))
  }
  notifications_loaded <- load_initial_data(notifications_seed, "Notifications", c("Time", "User", "Role", "Action"))
  notifications_log <- reactiveVal(notifications_loaded)
  # Called after a qualifying action (add history entry, add/edit/
  # delete plant item, create/update gang sheet) - appends one row,
  # newest first isn't done here since the table/CSV both sort on
  # display instead.
  log_notification <- function(action) {
    new_row <- data.frame(Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), User = user_name(), Role = role(), Action = action, stringsAsFactors = FALSE)
    notifications_log(bind_rows(notifications_log(), new_row))
  }
  gang_meta_loaded <- load_initial_data(gang_meta_seed, "GangMeta", c("Gang", "Ganger", "Location"))
  gang_meta <- reactiveVal(gang_meta_loaded)
  # gang_list used to just start empty every session (a bug - gang
  # sheet cards would vanish after a restart even though the Gang
  # assignments were still intact in Inventory). Now derived from
  # both the loaded Inventory Gang column and GangMeta, so it also
  # picks up gang sheets that have zero items ticked.
  initial_gangs <- sort(unique(c(
    gang_meta_loaded$Gang,
    { g <- inventory_loaded$Gang; g[!is.na(g) & g != ""] }
  )))
  gang_list <- reactiveVal(initial_gangs)
  plant_history_loaded <- load_initial_data(plant_history_seed, "Plant History",
                                            c("ItemID", "DateTime", "EntryType", "Description", "RecordedBy", "InvoiceID", "EntryID", "LinkedEntryID"))
  # EntryID is a newer column - back-fill unique IDs for any rows
  # loaded from a Sheet that predates it, so entries can be linked
  # together (see the "Link" action on each History entry).
  if (nrow(plant_history_loaded) > 0) {
    blank_eid <- is.na(plant_history_loaded$EntryID) | plant_history_loaded$EntryID == ""
    if (any(blank_eid)) {
      existing_nums <- suppressWarnings(as.integer(gsub("HIST-", "", plant_history_loaded$EntryID[!blank_eid])))
      start_n <- if (all(is.na(existing_nums))) 1 else max(existing_nums, na.rm = TRUE) + 1
      plant_history_loaded$EntryID[blank_eid] <- sprintf("HIST-%04d", seq(start_n, length.out = sum(blank_eid)))
    }
  }
  plant_history <- reactiveVal(plant_history_loaded)
  invoices_data <- reactiveVal(invoices_loaded)
  inv_view <- reactiveVal("list")        # "list" or "detail", for Inventory tab
  inv_selected <- reactiveVal(NULL)
  inv_browse_cat <- reactiveVal(NULL)    # Inventory List tile drill-down: NULL = category tiles
  inv_browse_sub <- reactiveVal(NULL)    # NULL = sub-category tiles (once a category is picked)
  editing_item <- reactiveVal(NULL)
  editing_gang <- reactiveVal(NULL)
  editing_invoice <- reactiveVal(NULL)
  editing_history_entry <- reactiveVal(NULL)
  inv <- reactive({ invoices_data() %>% mutate(DateParsed = as.Date(Date)) })
  # Filtered view used only by the Invoices tab's Overview/Analysis/
  # Companies/All Invoices sub-tabs (shared date + company + plant
  # item filter bar at the top of that tab). Home page trends and the
  # Weekly/Monthly Reports keep using the unfiltered inv() - they
  # already have their own date scoping.
  inv_filtered <- reactive({
    d <- inv()
    if (!is.null(input$inv_filter_dates) && length(input$inv_filter_dates) == 2 && !anyNA(input$inv_filter_dates)) {
      d <- d[!is.na(d$DateParsed) & d$DateParsed >= input$inv_filter_dates[1] & d$DateParsed <= input$inv_filter_dates[2], ]
    }
    if (!is.null(input$inv_filter_company) && length(input$inv_filter_company) > 0) {
      d <- d[d$Company %in% input$inv_filter_company, ]
    }
    if (!is.null(input$inv_filter_item) && length(input$inv_filter_item) > 0) {
      d <- d[d$Reference_PMK_Number %in% input$inv_filter_item, ]
    }
    d
  })
  # ---- Google Sheets sync ----
  # Debounced so a burst of edits (e.g. ticking 10 gang checkboxes)
  # becomes one sync, not ten. Each sync re-reads its tab and merges this
  # session's changes into it rather than overwriting the whole thing -
  # see the CONCURRENT-EDIT MERGE notes at the top of this file.
  sheets_last_synced <- reactiveVal(NULL)
  sheets_last_error <- reactiveVal(NULL)
  # What the Sheet held as of this session's load, and after each
  # successful sync. Diffing the live table against this is how the app
  # knows which rows are ours to push.
  #
  # Note these are set to OUR table after a sync, not to the merged
  # result. That's deliberate: rows other people added are then in
  # neither our table nor our baseline, so we never claim them as ours
  # and never report them as deleted. The trade-off is that this session
  # won't see their rows until it reloads - stale view, but no data loss,
  # which is the right way round.
  inventory_baseline <- reactiveVal(inventory_loaded)
  invoices_baseline <- reactiveVal(invoices_loaded)
  history_baseline <- reactiveVal(plant_history_loaded)
  gang_meta_baseline <- reactiveVal(gang_meta_loaded)
  notifications_baseline <- reactiveVal(notifications_loaded)
  ganger_baseline <- reactiveVal(sort(unique(gangers_loaded$Name[gangers_loaded$Name != ""])))
  company_baseline <- reactiveVal(sort(unique(companies_loaded$Name[companies_loaded$Name != ""])))
  inventory_debounced <- debounce(inventory_data, 4000)
  invoices_debounced <- debounce(invoices_data, 4000)
  history_debounced <- debounce(plant_history, 4000)
  ganger_debounced <- debounce(ganger_list, 4000)
  company_debounced <- debounce(company_list, 4000)
  gang_meta_debounced <- debounce(gang_meta, 4000)
  notifications_debounced <- debounce(notifications_log, 4000)
  sheets_write_ok <- function(df, tab_name, label) {
    ok <- sync_to_sheets(df, tab_name)
    if (!isTRUE(ok)) {
      sheets_last_error(paste0(label, " sync failed at ", format(Sys.time(), "%H:%M:%S"),
                               " - your changes are still here in the app and will retry on the next edit."))
      return(FALSE)
    }
    sheets_last_synced(Sys.time()); sheets_last_error(NULL)
    TRUE
  }
  # A failed READ must never fall through to a write: merging against a
  # blank would hand back an empty tab and delete everyone's data. Skip,
  # surface it, and let the next edit retry.
  sheets_read_failed <- function(label) {
    sheets_last_error(paste0(label, " sync skipped at ", format(Sys.time(), "%H:%M:%S"),
                             " - couldn't read the Sheet, so nothing was overwritten. It'll retry on the next edit."))
    invisible(FALSE)
  }
  sync_keyed <- function(tab_name, cols, key_col, local_df, baseline_rv,
                         normalise = identity, label = tab_name) {
    if (!SHEETS_SYNC_ENABLED) return(invisible(NULL))
    remote <- read_sheet_tab(tab_name, cols)
    if (is.null(remote)) return(sheets_read_failed(label))
    m <- merge_rows(normalise(remote), local_df, baseline_rv(), key_col)
    if (m$changed && !sheets_write_ok(normalise(m$df), tab_name, label)) return(invisible(FALSE))
    baseline_rv(local_df)
    invisible(TRUE)
  }
  sync_name_list <- function(tab_name, local_names, baseline_rv, label = tab_name) {
    if (!SHEETS_SYNC_ENABLED) return(invisible(NULL))
    remote <- read_sheet_tab(tab_name, c("Name"))
    if (is.null(remote)) return(sheets_read_failed(label))
    remote_names <- sort(unique(remote$Name[!is.na(remote$Name) & remote$Name != ""]))
    m <- merge_names(remote_names, local_names, baseline_rv())
    if (m$changed && !sheets_write_ok(data.frame(Name = m$names, stringsAsFactors = FALSE), tab_name, label)) {
      return(invisible(FALSE))
    }
    baseline_rv(local_names)
    invisible(TRUE)
  }
  sync_notifications <- function(local_df) {
    if (!SHEETS_SYNC_ENABLED) return(invisible(NULL))
    cols <- c("Time", "User", "Role", "Action")
    remote <- read_sheet_tab("Notifications", cols)
    if (is.null(remote)) return(sheets_read_failed("Notifications"))
    m <- merge_append(remote, local_df, notifications_baseline())
    if (m$changed && !sheets_write_ok(m$df, "Notifications", "Notifications")) return(invisible(FALSE))
    notifications_baseline(local_df)
    invisible(TRUE)
  }
  sync_inventory_now <- function() {
    sync_keyed("Inventory", inventory_cols, "ItemID", inventory_data(), inventory_baseline)
  }
  sync_invoices_now <- function() {
    sync_keyed("Invoices", names(invoices_seed), "InvoiceID", invoices_data(), invoices_baseline,
               normalise = normalise_invoice_amounts)
  }
  sync_history_now <- function() {
    sync_keyed("Plant History", names(plant_history_seed), "EntryID", plant_history(), history_baseline,
               label = "Plant History")
  }
  sync_gang_meta_now <- function() {
    sync_keyed("GangMeta", c("Gang", "Ganger", "Location"), "Gang", gang_meta(), gang_meta_baseline,
               label = "Gang sheet details")
  }
  run_full_sync <- function() {
    res <- c(
      sync_inventory_now(),
      sync_invoices_now(),
      sync_history_now(),
      sync_name_list("Gangers", ganger_list(), ganger_baseline, "Ganger list"),
      sync_name_list("Companies", company_list(), company_baseline, "Company list"),
      sync_gang_meta_now(),
      sync_notifications(notifications_log())
    )
    # Having nothing to push is a successful check, not a failure - stamp
    # the time and clear any stale error so "Sync Now" doesn't keep
    # reporting a problem that's already been resolved.
    if (length(res) == 0 || all(res)) {
      sheets_last_synced(Sys.time()); sheets_last_error(NULL)
    }
  }
  observeEvent(inventory_debounced(), { sync_inventory_now() }, ignoreInit = TRUE)
  observeEvent(invoices_debounced(), { sync_invoices_now() }, ignoreInit = TRUE)
  observeEvent(history_debounced(), { sync_history_now() }, ignoreInit = TRUE)
  observeEvent(gang_meta_debounced(), { sync_gang_meta_now() }, ignoreInit = TRUE)
  observeEvent(ganger_debounced(), {
    sync_name_list("Gangers", ganger_debounced(), ganger_baseline, "Ganger list")
  }, ignoreInit = TRUE)
  observeEvent(company_debounced(), {
    sync_name_list("Companies", company_debounced(), company_baseline, "Company list")
  }, ignoreInit = TRUE)
  observeEvent(notifications_debounced(), { sync_notifications(notifications_debounced()) }, ignoreInit = TRUE)
  next_item_id <- function() {
    ids <- inventory_data()$ItemID
    nums <- suppressWarnings(as.integer(gsub("ITEM-", "", ids)))
    nums <- nums[!is.na(nums)]
    n <- if (length(nums) == 0) 1 else max(nums) + 1
    sprintf("ITEM-%04d", n)
  }
  next_invoice_id <- function() {
    ids <- invoices_data()$InvoiceID
    nums <- suppressWarnings(as.integer(gsub("INV-", "", ids)))
    nums <- nums[!is.na(nums)]
    n <- if (length(nums) == 0) 1 else max(nums) + 1
    sprintf("INV-%04d", n)
  }
  next_entry_id <- function() {
    ids <- plant_history()$EntryID
    nums <- suppressWarnings(as.integer(gsub("HIST-", "", ids)))
    nums <- nums[!is.na(nums)]
    n <- if (length(nums) == 0) 1 else max(nums) + 1
    sprintf("HIST-%04d", n)
  }
  # A history entry's "counterpart" link, in either direction: either
  # this entry points at another one (LinkedEntryID), or another entry
  # points back at this one. Returns NA if there's no link.
  linked_entry_id_for <- function(entry_id, ph) {
    row <- ph[ph$EntryID == entry_id, ]
    if (nrow(row) > 0 && !is.na(row$LinkedEntryID[1]) && row$LinkedEntryID[1] != "") return(row$LinkedEntryID[1])
    back <- ph[!is.na(ph$LinkedEntryID) & ph$LinkedEntryID == entry_id, ]
    if (nrow(back) > 0) return(back$EntryID[1])
    NA_character_
  }
  # Items due for MOT or Warranty within the given number of days -
  # used by both the Home page snapshot and the Reports tab.
  due_within <- function(days) {
    df <- inventory_data()
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
  # Truck Service due tracker - for every item flagged
  # TruckServiceRequired == "Yes", finds the most recent "Truck
  # Service" history entry and works out when the next one is due
  # (6 weeks / 42 days later). Items never serviced are flagged as
  # overdue straight away. Used by Home page and Reports tab.
  truck_service_due <- function(days = NULL) {
    df <- inventory_data()
    flagged <- df[df$TruckServiceRequired == "Yes", ]
    if (nrow(flagged) == 0) return(data.frame())
    h <- plant_history()
    h <- h[h$EntryType == "Truck Service", ]
    if (nrow(h) > 0) {
      h$DateOnly <- parse_flex_date(substr(h$DateTime, 1, 10))
    }
    today <- Sys.Date()
    rows <- lapply(seq_len(nrow(flagged)), function(i) {
      row <- flagged[i, ]
      item_hist <- if (nrow(h) > 0) h[h$ItemID == row$ItemID & !is.na(h$DateOnly), ] else h[0, ]
      if (nrow(item_hist) == 0) {
        last_date <- as.Date(NA)
        due_date <- today - 1
        status <- "Never Serviced"
      } else {
        last_date <- max(item_hist$DateOnly)
        due_date <- last_date + 42
        status <- if (due_date < today) "Overdue" else if (due_date <= today + 7) "Due Soon" else "OK"
      }
      data.frame(
        ItemID = row$ItemID, Machine = row$Machine, PMK_Number = row$PMK_Number,
        Registration = row$Registration,
        LastServiced = if (is.na(last_date)) "Never" else as.character(last_date),
        DueDate = due_date, Status = status,
        stringsAsFactors = FALSE
      )
    })
    out <- bind_rows(rows)
    if (nrow(out) > 0) out <- out %>% arrange(DueDate)
    if (!is.null(days)) out <- out[out$DueDate <= today + days, ]
    out
  }
  # ---- Duplicate-plant watcher ----
  # Flags the same physical item (matched on Category + Sub-Category
  # + PMK Number, so per-category numbering like two different
  # "PMK 1"s in different categories is NOT a false match) showing
  # up under two different drivers - usually means a double entry
  # or a mix-up on the Whereabouts/Inventory side.
  seen_dupe_keys <- reactiveVal(character(0))
  observeEvent(inventory_data(), {
    df <- inventory_data()
    candidates <- df[df$PMK_Number != "" & !is.na(df$PMK_Number), ]
    if (nrow(candidates) == 0) return()
    dupes <- candidates %>%
      group_by(Category, SubCategory, PMK_Number) %>%
      filter(n() > 1 && n_distinct(Driver) > 1) %>%
      ungroup()
    if (nrow(dupes) == 0) return()
    keys <- unique(paste(dupes$Category, dupes$SubCategory, dupes$PMK_Number, sep = "||"))
    new_keys <- setdiff(keys, seen_dupe_keys())
    if (length(new_keys) == 0) return()
    msgs <- vapply(new_keys, function(k) {
      parts <- strsplit(k, "\\|\\|")[[1]]
      grp <- dupes[dupes$Category == parts[1] & dupes$SubCategory == parts[2] & dupes$PMK_Number == parts[3], ]
      drivers <- paste(unique(grp$Driver), collapse = " vs ")
      paste0(parts[1], " > ", parts[2], " > PMK ", parts[3], ": different drivers (", drivers, ")")
    }, character(1))
    showNotification(
      HTML(paste0("<strong>Possible duplicate plant detected:</strong><br>", paste(msgs, collapse = "<br>"))),
      type = "warning", duration = 15
    )
    seen_dupe_keys(union(seen_dupe_keys(), new_keys))
  }, ignoreInit = FALSE)
  # -------------------------------------------------------------
  # MAIN UI - built once per role (NOT on every data change, so
  # saving/adding things no longer bounces you back to Home)
  # -------------------------------------------------------------
  output$main_ui <- renderUI({
    r <- role()
    tabs <- list()
    tabs[["Home"]] <- uiOutput("home_tab_content")
    tabs[["Plant"]] <- uiOutput("plant_tab_content")
    if (r %in% c("Admin", "Boss", "Kevin")) tabs[["Invoices"]] <- uiOutput("invoices_tab_content")
    if (r %in% c("Admin", "Boss", "Mechanic")) tabs[["Job Cards & Inspections"]] <- uiOutput("jobcards_tab_content")
    # Plantman's only Admin panel is the Ganger List, so with gang
    # features off they'd get an empty Admin tab - hide it for them.
    if (r %in% c("Admin", "Boss") || (GANG_FEATURES_ENABLED && r == "Plantman")) tabs[["Admin"]] <- uiOutput("admin_tab_content")
    if (r %in% c("Admin", "Boss")) tabs[["Notifications"]] <- uiOutput("notifications_tab_content")
    do.call(tabsetPanel, c(
      list(id = "main_tabs", selected = "Home"),
      lapply(names(tabs), function(nm) tabPanel(nm, tabs[[nm]])),
      list(type = "tabs")
    ))
  })
  # ---- Plant tab - houses Inventory List, Plant Whereabouts and
  # Plant Analysis as sub-tabs. Built once per role() change, same
  # "stable outer dependency" pattern as main_ui - each sub-tab's own
  # narrow uiOutput still updates live underneath it.
  output$plant_tab_content <- renderUI({
    r <- role()
    sub <- list()
    sub[["Inventory List"]] <- uiOutput("inventory_tab_content")
    if (GANG_FEATURES_ENABLED && r %in% c("Admin", "Boss", "Mechanic", "Agent", "Plantman")) sub[["Plant Whereabouts"]] <- uiOutput("whereabouts_tab_content")
    if (r %in% c("Admin", "Boss", "Mechanic")) sub[["Plant Analysis"]] <- uiOutput("plant_analysis_tab_content")
    tagList(
      br(),
      do.call(tabsetPanel, c(
        list(type = "pills"),
        lapply(names(sub), function(nm) tabPanel(nm, sub[[nm]]))
      ))
    )
  })
  # -------------------------------------------------------------
  # HOME PAGE - big centred logo, snapshot, quick links
  # -------------------------------------------------------------
  output$home_tab_content <- renderUI({
    r <- role()
    df <- inventory_data()
    n_plant <- nrow(df)
    n_history <- nrow(plant_history())
    n_invoices_home <- if (r %in% c("Admin", "Boss", "Kevin")) nrow(invoices_data()) else NA
    due_soon <- due_within(30)
    n_due_soon <- nrow(due_soon)
    ts_due <- truck_service_due(14)
    n_ts_due <- nrow(ts_due)
    quick_links <- c("Inventory List")
    if (GANG_FEATURES_ENABLED && r %in% c("Admin", "Boss", "Mechanic", "Agent", "Plantman")) quick_links <- c(quick_links, "Plant Whereabouts")
    if (r %in% c("Admin", "Boss", "Kevin")) quick_links <- c(quick_links, "Invoices")
    if (r %in% c("Admin", "Boss", "Kevin")) quick_links <- c(quick_links, "Reports")
    if (r %in% c("Admin", "Boss", "Mechanic")) quick_links <- c(quick_links, "Job Cards & Inspections")
    if (r %in% c("Admin", "Boss", "Mechanic")) quick_links <- c(quick_links, "Plant Analysis")
    if (r %in% c("Admin", "Boss", "Plantman")) quick_links <- c(quick_links, "Admin")
    tagList(
      div(class = "hero-logo",
          tags$img(src = "pmk_logo.webp"),
          h3(class = "hero-title", "PMK Civil Engineering")
      ),
      h4(paste0("Welcome, ", user_name(), "."), style = "text-align:center;"),
      p(class = "text-muted", style = "text-align:center;", paste0("Logged in as ", r, ". Here's a quick snapshot.")),
      br(),
      fluidRow(
        column(4, metric_card(n_plant, "Plant Items")),
        column(4, metric_card(n_history, "Total History Entries")),
        column(4, metric_card(n_invoices_home, "Total Invoices"))
      ),
      fluidRow(style = "margin-top:10px;",
        column(6, metric_card(n_ts_due, "Truck Service Due (14 days)", colour = if (n_ts_due > 0) "#9C2B2B" else "#3E7C59")),
        column(6, metric_card(n_due_soon, "MOT/Warranty Due (30 days)", colour = if (n_due_soon > 0) "#9C2B2B" else "#3E7C59"))
      ),
      if (n_due_soon > 0) tagList(
        br(),
        h6("Due for MOT or Warranty in the next 30 days", style = "text-align:center;"),
        div(class = "chart-card", tableOutput("home_due_table"))
      ),
      if (n_ts_due > 0) tagList(
        br(),
        h6("Truck Service Due Within 14 Days", style = "text-align:center;"),
        div(class = "chart-card", tableOutput("home_truckservice_table"))
      ),
      if (r %in% c("Admin", "Boss", "Kevin")) tagList(
        br(),
        h6("Invoice Highlights", style = "text-align:center;"),
        div(class = "chart-card", tableOutput("home_invoice_highlights"))
      ),
      br(),
      fluidRow(
        column(6, h6("Trends")),
        column(6, style = "text-align:right;",
               selectInput("home_chart_period", NULL, choices = c("Weekly", "Monthly"), selected = "Weekly", width = "160px"))
      ),
      fluidRow(
        column(6, div(class = "chart-card", h6("History Entries Logged"), plotlyOutput("home_history_trend_plot", height = 260))),
        if (r %in% c("Admin", "Boss", "Kevin")) column(6, div(class = "chart-card", h6("Invoice Spend"), plotlyOutput("home_invoice_trend_plot", height = 260)))
      ),
      br(),
      h6("Quick links", style = "text-align:center;"),
      div(style = "text-align:center;",
          lapply(quick_links, function(tab_name) {
            actionButton(paste0("goto_", gsub("[^A-Za-z0-9]", "", tab_name)), tab_name,
                         class = "btn-outline-secondary btn-sm me-2 mb-2")
          })
      )
    )
  })
  output$home_due_table <- renderTable({
    d <- due_within(30)
    if (nrow(d) == 0) return(data.frame(Message = "Nothing due within 30 days."))
    d %>% transmute(Item = ifelse(Machine == "", ItemID, Machine),
                    `PMK/Reg` = ifelse(PMK_Number != "", PMK_Number, Registration),
                    Type = DueType, `Due Date` = as.character(DueDate))
  })
  output$home_truckservice_table <- renderTable({
    d <- truck_service_due(14)
    if (nrow(d) == 0) return(data.frame(Message = "Nothing due within 14 days."))
    d %>% transmute(Item = ifelse(Machine == "", ItemID, Machine),
                    `PMK/Reg` = ifelse(PMK_Number != "", PMK_Number, Registration),
                    `Last Serviced` = LastServiced, `Due Date` = as.character(DueDate), Status)
  })
  # ---- Home page trend charts (weekly/monthly toggle) ----
  home_period_bucket <- function(dates) {
    period <- if (!is.null(input$home_chart_period)) input$home_chart_period else "Weekly"
    if (period == "Monthly") as.Date(format(dates, "%Y-%m-01")) else floor_to_monday(dates)
  }
  output$home_history_trend_plot <- renderPlotly({
    h <- plant_history()
    if (nrow(h) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    h$DateOnly <- as.Date(substr(h$DateTime, 1, 10))
    h <- h[!is.na(h$DateOnly), ]
    if (nrow(h) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    h$Bucket <- home_period_bucket(h$DateOnly)
    agg <- h %>% group_by(Bucket) %>% summarise(Count = n(), .groups = "drop") %>% arrange(Bucket)
    p <- ggplot(agg, aes(x = Bucket, y = Count, group = 1, text = paste0(Count, " entrie(s)"))) +
      geom_line(color = "#0B4D3A", linewidth = 1.1) + geom_point(color = "#C9A227", size = 3) +
      labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  output$home_invoice_trend_plot <- renderPlotly({
    d <- inv()
    if (nrow(d) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    d <- d[!is.na(d$DateParsed), ]
    if (nrow(d) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    d$Bucket <- home_period_bucket(d$DateParsed)
    agg <- d %>% group_by(Bucket) %>% summarise(Total = sum(Amount, na.rm = TRUE), .groups = "drop") %>% arrange(Bucket)
    p <- ggplot(agg, aes(x = Bucket, y = Total, group = 1, text = paste0("£", round(Total, 2)))) +
      geom_line(color = "#9C2B2B", linewidth = 1.1) + geom_point(color = "#C9A227", size = 3) +
      scale_y_continuous(labels = label_dollar(prefix = "£")) + labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  # ---- Invoice highlights (Home page) ----
  # Six quick "most/highest" facts pulled from all invoices logged
  # so far. Item-level stats group on Category/SubCategory/Reference
  # together - that's the same identifier picked from the actual
  # Inventory list when the invoice was added, so it lines up with a
  # real item rather than relying on free text matching up by chance.
  invoice_highlights <- reactive({
    d <- inv()
    if (nrow(d) == 0) return(NULL)
    this_month_start <- as.Date(format(Sys.Date(), "%Y-%m-01"))
    last_30_start <- Sys.Date() - 29
    ref_stats <- d %>% filter(Reference_PMK_Number != "" & !is.na(Reference_PMK_Number)) %>%
      group_by(Category, SubCategory, Reference_PMK_Number) %>%
      summarise(Count = n(), Total = sum(Amount, na.rm = TRUE), .groups = "drop")
    most_invoiced <- if (nrow(ref_stats) > 0) (ref_stats %>% arrange(desc(Count), desc(Total)))[1, ] else NULL
    most_costly_item <- if (nrow(ref_stats) > 0) (ref_stats %>% arrange(desc(Total)))[1, ] else NULL
    recent <- d %>% filter(!is.na(DateParsed) & DateParsed >= last_30_start)
    supplier_freq_recent <- recent %>% group_by(Company) %>% summarise(Count = n()) %>% arrange(desc(Count))
    top_supplier_recent <- if (nrow(supplier_freq_recent) > 0) supplier_freq_recent[1, ] else NULL
    supplier_spend <- d %>% group_by(Company) %>% summarise(Total = sum(Amount, na.rm = TRUE)) %>% arrange(desc(Total))
    top_supplier_spend <- if (nrow(supplier_spend) > 0) supplier_spend[1, ] else NULL
    top_invoice_all <- d %>% arrange(desc(Amount)) %>% head(1)
    this_month <- d %>% filter(!is.na(DateParsed) & DateParsed >= this_month_start)
    top_invoice_month <- if (nrow(this_month) > 0) this_month %>% arrange(desc(Amount)) %>% head(1) else NULL
    list(
      most_invoiced = most_invoiced, most_costly_item = most_costly_item,
      top_supplier_recent = top_supplier_recent, top_supplier_spend = top_supplier_spend,
      top_invoice_all = top_invoice_all, top_invoice_month = top_invoice_month
    )
  })
  output$home_invoice_highlights <- renderTable({
    h <- invoice_highlights()
    if (is.null(h)) return(data.frame(Message = "No invoices logged yet."))
    rows <- list()
    rows[["Most invoiced item (all-time)"]] <- if (!is.null(h$most_invoiced))
      paste0(h$most_invoiced$Category, " > ", h$most_invoiced$SubCategory, " > ", h$most_invoiced$Reference_PMK_Number,
             " - ", h$most_invoiced$Count, " invoice(s), £", sprintf("%.2f", h$most_invoiced$Total), " total")
    else "-"
    rows[["Most expensive item overall (all-time)"]] <- if (!is.null(h$most_costly_item))
      paste0(h$most_costly_item$Category, " > ", h$most_costly_item$SubCategory, " > ", h$most_costly_item$Reference_PMK_Number,
             " - £", sprintf("%.2f", h$most_costly_item$Total), " total across ", h$most_costly_item$Count, " invoice(s)")
    else "-"
    rows[["Most frequent supplier (last 30 days)"]] <- if (!is.null(h$top_supplier_recent))
      paste0(h$top_supplier_recent$Company, " - ", h$top_supplier_recent$Count, " invoice(s)")
    else "None in the last 30 days"
    rows[["Top supplier by spend (all-time)"]] <- if (!is.null(h$top_supplier_spend))
      paste0(h$top_supplier_spend$Company, " - £", sprintf("%.2f", h$top_supplier_spend$Total))
    else "-"
    rows[["Highest single invoice (all-time)"]] <- if (nrow(h$top_invoice_all) > 0)
      paste0("£", sprintf("%.2f", h$top_invoice_all$Amount), " - ", h$top_invoice_all$Company, ", ", h$top_invoice_all$Date)
    else "-"
    rows[["Highest single invoice (this month)"]] <- if (!is.null(h$top_invoice_month) && nrow(h$top_invoice_month) > 0)
      paste0("£", sprintf("%.2f", h$top_invoice_month$Amount), " - ", h$top_invoice_month$Company, ", ", h$top_invoice_month$Date)
    else "None yet this month"
    data.frame(Highlight = names(rows), Details = unlist(rows, use.names = FALSE), stringsAsFactors = FALSE, row.names = NULL)
  })
  observe({
    lapply(c("Inventory List", "Plant Whereabouts", "Invoices", "Reports", "Job Cards & Inspections", "Plant Analysis", "Admin"), function(tab_name) {
      input_id <- paste0("goto_", gsub("[^A-Za-z0-9]", "", tab_name))
      observeEvent(input[[input_id]], {
        updateTabsetPanel(session, "main_tabs", selected = tab_name)
      }, ignoreInit = TRUE)
    })
  })
  # -------------------------------------------------------------
  # INVENTORY LIST - nested Category > Sub-Category accordions,
  # with Admin Add/Edit/Delete built straight into the list.
  # -------------------------------------------------------------
  output$inventory_tab_content <- renderUI({
    if (inv_view() == "list") inventory_list_ui(role()) else inventory_detail_ui(inv_selected(), role())
  })
  browse_tile <- function(id, title, count, click_input) {
    div(class = "browse-tile", role = "button", tabindex = "0",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})", click_input, js_escape_sq(id)),
        onkeypress = "if(event.key==='Enter'||event.key===' '){this.click()}",
        div(class = "tile-title", title),
        div(class = "tile-count", paste0(count, " item(s)"))
    )
  }
  inventory_list_ui <- function(r) {
    df <- inventory_data()
    tagList(
      br(),
      fluidRow(
        column(8, p(class = "text-muted",
                    if (r %in% c("Admin", "Boss", "Plantman")) "Click a category, then a sub-category, to find an item. Click an item for its full history, or use Edit/Delete."
                    else if (r == "Mechanic") "Click a category, then a sub-category, to find an item and log history."
                    else "Click a category, then a sub-category, to view item details."
        )),
        column(4, style = "text-align:right;",
               if (r %in% c("Admin", "Boss", "Plantman")) actionButton("add_item_btn", "+ Add New Item", class = "btn-primary btn-sm"))
      ),
      if (nrow(df) == 0) div(class = "alert alert-secondary", "No plant items yet - add some above.")
      else if (is.null(inv_browse_cat())) {
        # Level 1: Category tiles
        tagList(
          fluidRow(lapply(CATEGORY_OPTIONS, function(cat) {
            cnt <- nrow(df[df$Category == cat, ])
            column(3, browse_tile(cat, cat, cnt, "inv_browse_cat_click"))
          }))
        )
      } else if (is.null(inv_browse_sub())) {
        # Level 2: Sub-Category tiles within the chosen Category
        cat <- inv_browse_cat()
        cat_rows <- df[df$Category == cat, ]
        subs <- subcats_for(cat, df)
        tagList(
          actionButton("inv_browse_back_cat", "< Back to Categories", class = "btn-link mb-2"),
          h5(cat),
          if (length(subs) == 0) div(class = "alert alert-secondary", "No sub-categories under this category yet.")
          else fluidRow(lapply(subs, function(sub) {
            cnt <- nrow(cat_rows[cat_rows$SubCategory == sub, ])
            column(3, browse_tile(sub, sub, cnt, "inv_browse_sub_click"))
          }))
        )
      } else {
        # Level 3: item list for the chosen Category > Sub-Category
        cat <- inv_browse_cat(); sub <- inv_browse_sub()
        rows <- natural_sort_rows(df[df$Category == cat & df$SubCategory == sub, ])
        tagList(
          actionButton("inv_browse_back_sub", "< Back to Sub-Categories", class = "btn-link mb-2"),
          h5(paste0(cat, " > ", sub)),
          if (nrow(rows) == 0) div(class = "alert alert-secondary", "No items in this sub-category yet.")
          else tagList(lapply(seq_len(nrow(rows)), function(i) item_row(rows[i, ], r, clickable = TRUE, show_actions = TRUE, entry_counts = history_counts())))
        )
      }
    )
  }
  observeEvent(input$inv_browse_cat_click, {
    inv_browse_cat(input$inv_browse_cat_click)
    inv_browse_sub(NULL)
  })
  observeEvent(input$inv_browse_sub_click, {
    inv_browse_sub(input$inv_browse_sub_click)
  })
  observeEvent(input$inv_browse_back_cat, {
    inv_browse_cat(NULL)
    inv_browse_sub(NULL)
  })
  observeEvent(input$inv_browse_back_sub, {
    inv_browse_sub(NULL)
  })
  observeEvent(input$item_click, {
    inv_selected(input$item_click)
    inv_view("detail")
  })
  observeEvent(input$inv_back, {
    inv_view("list")
    inv_selected(NULL)
  })
  inventory_detail_ui <- function(iid, r) {
    df <- inventory_data()
    row <- df[df$ItemID == iid, ]
    if (nrow(row) == 0) return(tagList(br(), p("Item not found.")))
    row <- row[1, ]
    hist <- plant_history() %>% filter(ItemID == iid) %>% arrange(desc(DateTime))
    tagList(
      br(),
      actionButton("inv_back", "< Back to Inventory List", class = "btn-link mb-2"),
      div(class = "card p-3 mb-3", style = paste0("border-top:4px solid ", CATEGORY_COLOUR(row$Category), ";"),
          div(class = "d-flex justify-content-between align-items-start flex-wrap",
              span(class = "plate", style = "font-size:1.3rem; padding:6px 14px;", item_identifier(row)),
              if (r %in% c("Admin", "Boss", "Plantman")) div(
                actionButton("detail_edit_btn", "Edit", class = "btn-outline-secondary btn-sm me-2"),
                actionButton("detail_delete_btn", "Delete", class = "btn-outline-danger btn-sm")
              )
          ),
          p(class = "text-muted mt-2", paste0(ifelse(row$Machine == "", "(no machine name)", row$Machine),
                                              "  |  ", row$Category, " > ", row$SubCategory)),
          fluidRow(
            column(4,
                   p(strong("PMK Number: "), ifelse(row$PMK_Number == "", "-", row$PMK_Number)),
                   p(strong("Registration: "), ifelse(row$Registration == "", "-", row$Registration)),
                   p(strong("Serial Number: "), ifelse(row$SerialNumber == "", "-", row$SerialNumber)),
                   p(strong("Hours: "), ifelse(is.null(row$Hours) || is.na(row$Hours) || row$Hours == "", "Not logged", row$Hours))
            ),
            column(4,
                   p(strong("Driver: "), ifelse(row$Driver == "", "Unassigned", row$Driver)),
                   p(strong("Location: "), ifelse(row$Location == "", "-", row$Location)),
                   if (GANG_FEATURES_ENABLED) p(strong("Gang: "), ifelse(row$Gang == "", "Not assigned", row$Gang)),
                   p(strong("History Entries: "), nrow(hist))
            ),
            column(4,
                   p(strong("Date Purchased: "), ifelse(row$DatePurchased == "", "-", row$DatePurchased)),
                   p(strong("Warranty End: "), ifelse(row$WarrantyEndDate == "", "-", row$WarrantyEndDate)),
                   p(strong("MOT Due: "), ifelse(row$MOTDue == "", "-", row$MOTDue))
            )
          ),
          p(strong("Active: "), row$Active),
          p(strong("Notes: "), ifelse(row$Notes == "", "-", row$Notes)),
          if (r %in% c("Admin", "Boss", "Mechanic", "Plantman")) actionButton("inv_add_entry_btn", "+ Add History Entry", class = "btn-primary btn-sm")
      ),
      div(class = "d-flex justify-content-between align-items-center flex-wrap mb-2",
          h5("History", class = "mb-0"),
          if (nrow(hist) > 0) div(
            downloadButton("item_history_csv", "Download History (CSV)", class = "btn-outline-secondary btn-sm me-2"),
            downloadButton("item_history_pdf", "Download History (PDF)", class = "btn-outline-secondary btn-sm")
          )
      ),
      if (nrow(hist) == 0) div(class = "alert alert-info", "No history yet for this item.")
      else tagList(lapply(seq_len(nrow(hist)), function(i) {
        h <- hist[i, ]
        linked_id <- if (!is.na(h$EntryID) && h$EntryID != "") linked_entry_id_for(h$EntryID, plant_history()) else NA_character_
        linked_row <- if (!is.na(linked_id)) plant_history()[plant_history()$EntryID == linked_id, ] else plant_history()[0, ]
        div(class = "history-item", style = if (nrow(linked_row) > 0) "border-left-color:#C9A227;" else NULL,
            div(class = "d-flex justify-content-between align-items-start flex-wrap",
                div(strong(h$EntryType), span(class = "text-muted", paste0(" - ", h$DateTime))),
                div(
                  # Printing is read-only, so it isn't gated on the editing roles.
                  if (h$EntryType == "Job Card" && !is.na(h$EntryID) && h$EntryID != "")
                    tags$a(href = "#", style = "font-size:0.8rem; margin-right:10px;",
                           onclick = sprintf("Shiny.setInputValue('print_entry_click', '%s', {priority:'event'}); return false;", h$EntryID),
                           "Print"),
                  if (r %in% c("Admin", "Boss", "Mechanic", "Plantman") && !is.na(h$EntryID) && h$EntryID != "") tagList(
                  if (nrow(linked_row) > 0) tags$a(href = "#", style = "font-size:0.8rem; color:#9C2B2B; margin-right:10px;",
                                                   onclick = sprintf("Shiny.setInputValue('unlink_entry_click', '%s', {priority:'event'}); return false;", h$EntryID),
                                                   "Unlink")
                  else tags$a(href = "#", style = "font-size:0.8rem; margin-right:10px;",
                              onclick = sprintf("Shiny.setInputValue('link_entry_click', '%s', {priority:'event'}); return false;", h$EntryID),
                              "Link to another entry"),
                  tags$a(href = "#", style = "font-size:0.8rem; margin-right:10px;",
                         onclick = sprintf("Shiny.setInputValue('edit_entry_click', '%s', {priority:'event'}); return false;", h$EntryID),
                         "Edit"),
                  tags$a(href = "#", style = "font-size:0.8rem; color:#9C2B2B;",
                         onclick = sprintf("Shiny.setInputValue('delete_entry_click', '%s', {priority:'event'}); return false;", h$EntryID),
                         "Delete")
                  )
                )
            ),
            if (nrow(linked_row) > 0) p(class = "mb-1", style = "font-size:0.85rem; color:#8a6d00;",
                                        paste0("Linked to: ", linked_row$EntryType[1], " - ", linked_row$DateTime[1])),
            tagList(lapply(strsplit(h$Description, "\n")[[1]], function(ln) p(class = "mb-1", ln))),
            p(class = "mb-0 text-muted", style = "font-size:0.85rem;", paste("By:", h$RecordedBy))
        )
      }))
    )
  }
  observeEvent(input$inv_add_entry_btn, {
    iid <- inv_selected()
    cur_row <- inventory_data()[inventory_data()$ItemID == iid, ]
    cur_cat <- if (nrow(cur_row) > 0) cur_row$Category[1] else CATEGORY_OPTIONS[1]
    cur_subs <- subcats_for(cur_cat, inventory_data())
    cur_sub <- if (nrow(cur_row) > 0 && cur_row$SubCategory[1] %in% cur_subs) cur_row$SubCategory[1]
    else if (length(cur_subs) > 0) cur_subs[1] else NULL
    cur_items <- if (!is.null(cur_sub)) items_for_picker(cur_cat, cur_sub, inventory_data()) else character(0)
    cur_item_id <- if (nrow(cur_row) > 0) item_identifier(cur_row[1, ]) else NULL
    # Choices for optionally linking this new entry to an existing one
    # on the same item (e.g. a Job Card done for the same job as an
    # Invoice already logged).
    existing_entries <- plant_history()[plant_history()$ItemID == iid & !is.na(plant_history()$EntryID) & plant_history()$EntryID != "", ]
    link_choices <- if (nrow(existing_entries) == 0) c("None" = "") else
      c("None" = "", setNames(existing_entries$EntryID, paste0(existing_entries$EntryType, " - ", existing_entries$DateTime)))
    # Fresh Form 32 state - bumping the token gives this inspection its own
    # per-item defect/fault input ids, so nothing carries over from the last one.
    sv_open(TRUE); sv_ready(FALSE); sv_fault_n(1); sv_token(sv_token() + 1)
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = paste("Add Entry -", iid), size = "l",
      selectInput("ih_type", "Entry Type", choices = ENTRY_TYPES),
      conditionalPanel("input.ih_type == 'Driver Assigned'",
                       textInput("ih_driver", "New Driver")),
      conditionalPanel("input.ih_type == 'Hours Updated'",
                       numericInput("ih_hours", "New Hours Reading", value = NA)),
      conditionalPanel("input.ih_type == 'Damage' || input.ih_type == 'Refurbished' || input.ih_type == 'Mechanic Work' || input.ih_type == 'Note'",
                       textAreaInput("ih_desc", "Description", rows = 3)),
      conditionalPanel("input.ih_type == 'Mechanic Work'",
                       checkboxInput("ih_subcontractor", "Subcontractor work (creates an Invoice too)", value = FALSE),
                       conditionalPanel("input.ih_subcontractor == true",
                                        selectizeInput("ih_sub_company", "Subcontractor Company *", choices = company_choices_all(),
                                                       options = list(create = TRUE, placeholder = "Select or type a company name")),
                                        numericInput("ih_sub_amount", "Amount (£) *", value = NA)
                       )
      ),
      # ---- Service Inspection: mirrors Form 32, Issue B (Sept 2026) ----
      # The 22 checklist items are unchanged from Issue A. What Issue B
      # added: the header ID block, per-item Defects Found/Rectified By,
      # an N/A rating alongside Serviceable, the numbered Fault Details
      # table, tyre tread/pressures, and inspector/supervisor sign-off.
      conditionalPanel("input.ih_type == 'Service Inspection'",
                       h5("Machine"),
                       p(class = "text-muted mb-1", "Defaults to the item you're viewing - change these if the inspection is actually for a different machine. Whatever's picked here is the item the entry gets filed against."),
                       fluidRow(
                         column(4, selectInput("sv_category", "Category *", choices = CATEGORY_OPTIONS, selected = cur_cat)),
                         column(4, selectizeInput("sv_subcategory", "Sub-Category *", choices = cur_subs, selected = cur_sub)),
                         column(4, selectizeInput("sv_item", "PMK Number/Reg/Serial *", choices = cur_items, selected = cur_item_id,
                                                  options = list(create = TRUE)))
                       ),
                       hr(),
                       h5("Inspection Details"),
                       fluidRow(
                         column(6, dateInput("sv_outward_date", "Outward Inspection Date", value = Sys.Date())),
                         column(6, dateInput("sv_inward_date", "Inward Inspection Date", value = Sys.Date()))
                       ),
                       # Pre-filled from the picked item's inventory record (and
                       # re-filled if the picker above is changed) - all three stay
                       # editable in case the paper form says something different.
                       fluidRow(
                         column(4, textInput("sv_fleet_chassis", "Fleet/Chassis Number",
                                             value = if (nrow(cur_row) > 0) cur_row$SerialNumber[1] else "")),
                         column(4, textInput("sv_plant_number", "PMK Plant Number",
                                             value = if (nrow(cur_row) > 0) cur_row$PMK_Number[1] else "")),
                         column(4, textInput("sv_make_type", "Make & Type",
                                             value = if (nrow(cur_row) > 0) cur_row$Machine[1] else ""))
                       ),
                       fluidRow(
                         column(6, textInput("sv_next_due", "Next Service/Inspection Due", placeholder = "e.g. March 2026")),
                         column(6, textInput("sv_reviewed_by", "Reviewed By"))
                       ),
                       fluidRow(
                         column(6, dateInput("sv_date_in", "Date In Workshop", value = Sys.Date())),
                         column(6, dateInput("sv_date_out", "Date Out Workshop", value = Sys.Date()))
                       ),
                       hr(),
                       h5("Checklist"),
                       p(class = "text-muted", "Everything defaults to serviceable (S) - untick anything needing repair (R). Use the Non Applicable list below for anything this machine doesn't have (N/A)."),
                       tagList(lapply(names(SERVICE_CHECKLIST), function(sec) {
                         tagList(
                           strong(sec),
                           checkboxGroupInput(paste0("sv_chk_", make.names(sec)), NULL,
                                              choices = SERVICE_CHECKLIST[[sec]], selected = SERVICE_CHECKLIST[[sec]])
                         )
                       })),
                       selectizeInput("sv_na_items", "Non Applicable (N/A) - items this machine doesn't have",
                                      choices = SERVICE_ITEM_CHOICES, multiple = TRUE,
                                      options = list(placeholder = "Leave blank unless something doesn't apply")),
                       # Appears only for items actually marked for repair - see
                       # output$sv_defect_boxes. Keeps the form short when nothing's wrong.
                       uiOutput("sv_defect_boxes"),
                       textAreaInput("sv_defects", "Defects Found (general)", rows = 2,
                                     placeholder = "Optional - anything not tied to one specific item above"),
                       textAreaInput("sv_rectified_by", "Rectified By (general)", placeholder = "Optional", rows = 2),
                       hr(),
                       h5("Fault Details"),
                       p(class = "text-muted", "Numbered faults, what was done about them, and who rectified them. Blank rows are ignored."),
                       uiOutput("sv_fault_rows_ui"),
                       div(class = "mb-2",
                           actionButton("sv_add_fault_row", "+ Add fault row", class = "btn-outline-secondary btn-sm me-2"),
                           actionButton("sv_remove_fault_row", "- Remove last row", class = "btn-outline-secondary btn-sm")
                       ),
                       hr(),
                       h5("Tyres"),
                       p(class = "text-muted mb-1", "Tread Depth"),
                       fluidRow(
                         column(4, textInput("sv_tread_1", NULL)),
                         column(4, textInput("sv_tread_2", NULL)),
                         column(4, textInput("sv_tread_3", NULL))
                       ),
                       fluidRow(
                         column(4, textInput("sv_tread_4", NULL)),
                         column(4, textInput("sv_tread_5", NULL)),
                         column(4, textInput("sv_tread_6", NULL))
                       ),
                       p(class = "text-muted mb-1", "Pressures"),
                       fluidRow(
                         column(4, textInput("sv_press_1", NULL)),
                         column(4, textInput("sv_press_2", NULL)),
                         column(4, textInput("sv_press_3", NULL))
                       ),
                       fluidRow(
                         column(4, textInput("sv_press_4", NULL)),
                         column(4, textInput("sv_press_5", NULL)),
                         column(4, textInput("sv_press_6", NULL))
                       ),
                       hr(),
                       h5("Sign-off"),
                       fluidRow(
                         column(6, textInput("sv_inspector_name", "Name of Inspector")),
                         column(6, textInput("sv_supervisor_name", "Name of Supervisor"))
                       ),
                       checkboxInput("sv_supervisor_confirm",
                                     "Supervisor considers the above defects rectified satisfactorily and this machine to be in a safe condition to operate",
                                     value = FALSE),
                       p(class = "text-muted", style = "font-size:0.8rem;",
                         "Note: it is always the responsibility of the Operator that the machine is in a safe condition before being used.")
      ),
      # ---- Job Card: mirrors the RHA Job Card Pad ----
      conditionalPanel("input.ih_type == 'Job Card'",
                       p(class = "text-muted mb-1", "Defaults to the item you're viewing - change these if the job card is actually for a different machine."),
                       fluidRow(
                         column(4, selectInput("jc_category", "Category *", choices = CATEGORY_OPTIONS, selected = cur_cat)),
                         column(4, selectizeInput("jc_subcategory", "Sub-Category *", choices = cur_subs, selected = cur_sub)),
                         column(4, selectizeInput("jc_item", "PMK Number/Reg/Serial *", choices = cur_items, selected = cur_item_id,
                                                  options = list(create = TRUE)))
                       ),
                       fluidRow(
                         column(6, textInput("jc_job_no", "Job No.")),
                         column(6, textInput("jc_depot", "Depot"))
                       ),
                       fluidRow(
                         column(6, dateInput("jc_date_started", "Date Started", value = Sys.Date())),
                         column(6, textInput("jc_odometer", "Odometer/Hours Reading"))
                       ),
                       textAreaInput("jc_work_to_do", "Description of Work To Be Done", rows = 3),
                       textAreaInput("jc_work_done", "Description of Work Carried Out", rows = 3),
                       fluidRow(
                         column(4, textInput("jc_time_taken", "Time Taken", placeholder = "e.g. 3 hrs")),
                         column(4, textInput("jc_done_by", "Done By")),
                         column(4, dateInput("jc_date_completed", "Date Completed", value = Sys.Date()))
                       )
      ),
      # ---- Truck Service: mirrors the Logistics UK Maintenance
      # Inspection pad - separate, longer checklist from Form 32,
      # used for the 6-weekly HGV inspection cycle. ----
      conditionalPanel("input.ih_type == 'Truck Service'",
                       fluidRow(
                         column(4, textInput("ts_reg", "Reg No.", value = if (nrow(cur_row) > 0) cur_row$Registration[1] else "")),
                         column(4, textInput("ts_odometer", "Odometer Reading")),
                         column(4, textInput("ts_inspector", "Inspected By"))
                       ),
                       fluidRow(
                         column(6, dateInput("ts_date", "Inspection Date", value = Sys.Date())),
                         column(6, textInput("ts_next_due", "Next Inspection Due", placeholder = "auto-filled, 6 weeks from today"))
                       ),
                       p(class = "text-muted", "Everything defaults to serviceable - untick anything that needs attention."),
                       tagList(lapply(names(TRUCK_SERVICE_CHECKLIST), function(sec) {
                         tagList(
                           strong(sec),
                           checkboxGroupInput(paste0("ts_chk_", make.names(sec)), NULL,
                                              choices = TRUCK_SERVICE_CHECKLIST[[sec]], selected = TRUCK_SERVICE_CHECKLIST[[sec]])
                         )
                       })),
                       textAreaInput("ts_tyre_notes", "Tyre Tread/Pressure/Age Notes",
                                     placeholder = "e.g. tread depths, tyre age (DOT) codes, anything replaced", rows = 2),
                       textAreaInput("ts_defects", "Defects/Items Requiring Attention", rows = 2,
                                     placeholder = "Optional - only needed if something's unticked above"),
                       textAreaInput("ts_rectified_by", "Rectified By", placeholder = "Optional", rows = 2)
      ),
      hr(),
      p(class = "text-muted mb-1", "Optional, applies to any entry type."),
      selectInput("ih_link", "Link to another entry for this item", choices = link_choices,
                  selected = ""),
      fluidRow(
        column(6, textInput("ih_location", "Location")),
        column(6, numericInput("ih_price", "Price (£)", value = NA))
      ),
      textAreaInput("ih_comments", "Additional Comments", rows = 3,
                    placeholder = "Optional - anything else worth recording. Appears in its own box on the printed sheet."),
      dateInput("ih_date", "Date (used to sort this entry in History)", value = Sys.Date()),
      footer = tagList(modalButton("Cancel"), actionButton("ih_submit", "Save Entry", class = "btn-primary"))
    ))
  })
  observeEvent(input$jc_category, {
    subs <- subcats_for(input$jc_category, inventory_data())
    updateSelectizeInput(session, "jc_subcategory", choices = subs,
                         selected = if (length(subs) > 0) subs[1] else character(0))
  }, ignoreInit = TRUE)
  observeEvent(input$jc_subcategory, {
    req(input$jc_category)
    items <- items_for_picker(input$jc_category, input$jc_subcategory, inventory_data())
    updateSelectizeInput(session, "jc_item", choices = items)
  }, ignoreInit = TRUE)
  # Same cascade for the Service Inspection machine picker.
  observeEvent(input$sv_category, {
    subs <- subcats_for(input$sv_category, inventory_data())
    updateSelectizeInput(session, "sv_subcategory", choices = subs,
                         selected = if (length(subs) > 0) subs[1] else character(0))
  }, ignoreInit = TRUE)
  observeEvent(input$sv_subcategory, {
    req(input$sv_category)
    items <- items_for_picker(input$sv_category, input$sv_subcategory, inventory_data())
    updateSelectizeInput(session, "sv_item", choices = items)
  }, ignoreInit = TRUE)
  # Picking a different machine re-fills the three ID fields from that
  # item's inventory record, so they can't be left describing the machine
  # that happened to be open when the form was started.
  observeEvent(input$sv_item, {
    req(input$sv_category, input$sv_subcategory, input$sv_item)
    mid <- find_item_id(input$sv_category, input$sv_subcategory, input$sv_item, inventory_data())
    if (is.na(mid)) return()
    row <- inventory_data()[inventory_data()$ItemID == mid, ]
    if (nrow(row) == 0) return()
    updateTextInput(session, "sv_fleet_chassis", value = row$SerialNumber[1])
    updateTextInput(session, "sv_plant_number", value = row$PMK_Number[1])
    updateTextInput(session, "sv_make_type", value = row$Machine[1])
  }, ignoreInit = TRUE)
  # Looks up the ItemID a Job Card's Category/Sub-Category/identifier
  # picker points at, so the entry files against that machine rather
  # than whichever item's page happened to be open when it was added.
  find_item_id <- function(cat, subcat, identifier, df) {
    norm_target <- normalize_ref(identifier)
    if (norm_target == "" || nrow(df) == 0) return(NA_character_)
    # Primary match: Category + Sub-Category + reference, same as
    # what's on the invoice/job card form. Every side is normalised
    # (trimmed, case-insensitive for cat/subcat; trimmed/upper/no
    # punctuation/no leading "PMK" for the reference) so whitespace,
    # capitalisation, or "PMK 2" vs "2" vs "pmk-2" don't break it.
    df_cat <- toupper(trimws(df$Category))
    df_subcat <- toupper(trimws(df$SubCategory))
    norm_cat <- toupper(trimws(cat))
    norm_subcat <- toupper(trimws(subcat))
    rows <- df[df_cat == norm_cat & df_subcat == norm_subcat, ]
    if (nrow(rows) > 0) {
      ids <- vapply(seq_len(nrow(rows)), function(i) item_identifier(rows[i, ]), character(1))
      hit <- rows$ItemID[normalize_ref(ids) == norm_target]
      if (length(hit) > 0) return(hit[1])
    }
    # Fall back to a fleet-wide search on the reference alone - PMK
    # numbers/registrations are effectively unique across the whole
    # inventory, so this still finds the right item even if
    # Category/Sub-Category on the form doesn't line up with how the
    # item is actually filed.
    all_ids <- vapply(seq_len(nrow(df)), function(i) item_identifier(df[i, ]), character(1))
    hit <- df$ItemID[normalize_ref(all_ids) == norm_target]
    if (length(hit) > 0) hit[1] else NA_character_
  }
  # ---- Service Inspection (Form 32, Issue B) form state ----
  # sv_token gives each inspection its own set of dynamically-rendered
  # input ids. Without it, the defect/fault boxes are server-rendered (not
  # torn down with the modal like the static inputs are) and last week's
  # typing would still be sitting there next time the form is opened.
  sv_open <- reactiveVal(FALSE)
  sv_ready <- reactiveVal(FALSE)
  sv_fault_n <- reactiveVal(1)
  sv_token <- reactiveVal(0)
  sv_nz <- function(x, default = "") {
    if (is.null(x) || length(x) == 0) return(default)
    if (is.na(x[1])) return(default)
    as.character(x[1])
  }
  # An empty checkboxGroupInput reads back as NULL, which is also what it
  # reads as before the modal's inputs have registered - so "everything in
  # this section is unticked" and "the form isn't up yet" look identical.
  # Once any section has reported in, we know the form is live and NULL
  # genuinely means all-unticked.
  observe({
    if (!isTRUE(sv_open())) return()
    vals <- lapply(names(SERVICE_CHECKLIST), function(sec) input[[paste0("sv_chk_", make.names(sec))]])
    if (any(!vapply(vals, is.null, logical(1)))) sv_ready(TRUE)
  })
  sv_na_ids <- reactive({ if (is.null(input$sv_na_items)) character(0) else input$sv_na_items })
  # Items marked for repair: unticked AND not marked Non Applicable.
  # N/A wins, so flagging something as not-applicable doesn't also make it
  # look like a failed check.
  sv_flagged <- reactive({
    if (!isTRUE(sv_ready())) return(list())
    na_ids <- sv_na_ids()
    out <- list()
    for (e in SERVICE_CHECKLIST_FLAT) {
      if (e$id %in% na_ids) next
      ticked <- input[[paste0("sv_chk_", make.names(e$section))]]
      if (!(e$item %in% ticked)) out[[length(out) + 1]] <- e
    }
    out
  })
  output$sv_defect_boxes <- renderUI({
    flagged <- sv_flagged()
    if (length(flagged) == 0) return(NULL)
    tok <- sv_token()
    tagList(
      div(class = "alert alert-warning", style = "padding:8px 12px;",
          paste0(length(flagged), " item(s) marked for repair - record the defect and who rectified it.")),
      lapply(flagged, function(e) {
        def_id <- paste0("sv_def_", tok, "_", e$n)
        rect_id <- paste0("sv_rect_", tok, "_", e$n)
        div(style = "border-left:3px solid #C9A227; padding-left:10px; margin-bottom:6px;",
            strong(e$long_label),
            fluidRow(
              column(8, textInput(def_id, "Defects Found", value = isolate(sv_nz(input[[def_id]])))),
              column(4, textInput(rect_id, "Rectified By", value = isolate(sv_nz(input[[rect_id]]))))
            )
        )
      })
    )
  })
  output$sv_fault_rows_ui <- renderUI({
    n <- sv_fault_n(); tok <- sv_token()
    tagList(lapply(seq_len(n), function(i) {
      f_id <- paste0("sv_fault_", tok, "_", i)
      a_id <- paste0("sv_fault_action_", tok, "_", i)
      r_id <- paste0("sv_fault_rect_", tok, "_", i)
      fluidRow(
        column(1, div(style = "padding-top:32px; font-weight:600;", i)),
        column(5, textInput(f_id, if (i == 1) "Fault Details" else NULL, value = isolate(sv_nz(input[[f_id]])))),
        column(3, textInput(a_id, if (i == 1) "Action Taken" else NULL, value = isolate(sv_nz(input[[a_id]])))),
        column(3, textInput(r_id, if (i == 1) "Rectified By" else NULL, value = isolate(sv_nz(input[[r_id]]))))
      )
    }))
  })
  observeEvent(input$sv_add_fault_row, { sv_fault_n(sv_fault_n() + 1) })
  observeEvent(input$sv_remove_fault_row, { if (sv_fault_n() > 1) sv_fault_n(sv_fault_n() - 1) })
  build_service_desc <- function() {
    tok <- sv_token()
    na_ids <- sv_na_ids()
    lines <- c(
      paste0("Machine: ", sv_nz(input$sv_category, "-"), " > ", sv_nz(input$sv_subcategory, "-"), " > ", sv_nz(input$sv_item, "-")),
      paste0("Outward Inspection Date: ", as.character(input$sv_outward_date)),
      paste0("Inward Inspection Date: ", as.character(input$sv_inward_date)),
      paste0("Fleet/Chassis Number: ", sv_nz(input$sv_fleet_chassis, "-")),
      paste0("PMK Plant Number: ", sv_nz(input$sv_plant_number, "-")),
      paste0("Make & Type: ", sv_nz(input$sv_make_type, "-")),
      paste0("Next Service/Inspection Due: ", sv_nz(input$sv_next_due, "-")),
      paste0("Date In Workshop: ", as.character(input$sv_date_in), "  |  Date Out Workshop: ", as.character(input$sv_date_out))
    )
    na_entries <- Filter(function(e) e$id %in% na_ids, SERVICE_CHECKLIST_FLAT)
    flagged <- list()
    for (e in SERVICE_CHECKLIST_FLAT) {
      if (e$id %in% na_ids) next
      ticked <- input[[paste0("sv_chk_", make.names(e$section))]]
      if (!(e$item %in% ticked)) flagged[[length(flagged) + 1]] <- e
    }
    n_total <- length(SERVICE_CHECKLIST_FLAT)
    n_na <- length(na_entries); n_flagged <- length(flagged)
    if (n_flagged == 0 && n_na == 0) {
      lines <- c(lines, paste0("All ", n_total, " checklist items serviceable."))
    } else {
      lines <- c(lines, paste0("Checklist: ", n_total - n_na - n_flagged, " serviceable (S), ",
                               n_flagged, " for repair (R), ", n_na, " non applicable (N/A), of ", n_total, "."))
    }
    if (n_flagged > 0) {
      lines <- c(lines, "Items flagged for repair (R):")
      for (e in flagged) {
        lines <- c(lines, paste0("  - ", e$long_label,
                                 " | Defect: ", sv_nz(input[[paste0("sv_def_", tok, "_", e$n)]], "-"),
                                 " | Rectified By: ", sv_nz(input[[paste0("sv_rect_", tok, "_", e$n)]], "-")))
      }
    }
    if (n_na > 0) {
      lines <- c(lines, "Non Applicable (N/A):")
      for (e in na_entries) lines <- c(lines, paste0("  - ", e$long_label))
    }
    if (trimws(sv_nz(input$sv_defects)) != "") lines <- c(lines, paste0("Defects Found (general): ", input$sv_defects))
    if (trimws(sv_nz(input$sv_rectified_by)) != "") lines <- c(lines, paste0("Rectified By (general): ", input$sv_rectified_by))
    fault_lines <- c()
    for (i in seq_len(sv_fault_n())) {
      f <- trimws(sv_nz(input[[paste0("sv_fault_", tok, "_", i)]]))
      a <- trimws(sv_nz(input[[paste0("sv_fault_action_", tok, "_", i)]]))
      rb <- trimws(sv_nz(input[[paste0("sv_fault_rect_", tok, "_", i)]]))
      if (f == "" && a == "" && rb == "") next
      fault_lines <- c(fault_lines, paste0("  ", length(fault_lines) + 1, ". ", if (f == "") "-" else f,
                                           " | Action Taken: ", if (a == "") "-" else a,
                                           " | Rectified By: ", if (rb == "") "-" else rb))
    }
    if (length(fault_lines) > 0) lines <- c(lines, "Fault Details:", fault_lines)
    tread <- vapply(1:6, function(i) trimws(sv_nz(input[[paste0("sv_tread_", i)]])), character(1))
    press <- vapply(1:6, function(i) trimws(sv_nz(input[[paste0("sv_press_", i)]])), character(1))
    if (any(tread != "")) lines <- c(lines, paste0("Tyres - Tread Depth: ", paste(tread[tread != ""], collapse = ", ")))
    if (any(press != "")) lines <- c(lines, paste0("Tyres - Pressures: ", paste(press[press != ""], collapse = ", ")))
    if (trimws(sv_nz(input$sv_inspector_name)) != "") lines <- c(lines, paste0("Name of Inspector: ", input$sv_inspector_name))
    if (trimws(sv_nz(input$sv_reviewed_by)) != "") lines <- c(lines, paste0("Reviewed By: ", input$sv_reviewed_by))
    if (trimws(sv_nz(input$sv_supervisor_name)) != "") lines <- c(lines, paste0("Name of Supervisor: ", input$sv_supervisor_name))
    if (isTRUE(input$sv_supervisor_confirm)) {
      lines <- c(lines, "Supervisor considers the above defects rectified satisfactorily and this machine to be in a safe condition to operate.")
    }
    paste(lines, collapse = "\n")
  }
  build_jobcard_desc <- function() {
    lines <- c(paste0("Machine: ", input$jc_category, " > ", input$jc_subcategory, " > ", input$jc_item))
    if (!is.null(input$jc_job_no) && trimws(input$jc_job_no) != "") lines <- c(lines, paste0("Job No.: ", input$jc_job_no))
    if (!is.null(input$jc_depot) && trimws(input$jc_depot) != "") lines <- c(lines, paste0("Depot: ", input$jc_depot))
    lines <- c(lines, paste0("Date Started: ", as.character(input$jc_date_started)))
    if (!is.null(input$jc_odometer) && trimws(input$jc_odometer) != "") lines <- c(lines, paste0("Odometer/Hours: ", input$jc_odometer))
    lines <- c(lines,
               paste0("Work To Be Done: ", ifelse(is.null(input$jc_work_to_do) || input$jc_work_to_do == "", "-", input$jc_work_to_do)),
               paste0("Work Carried Out: ", ifelse(is.null(input$jc_work_done) || input$jc_work_done == "", "-", input$jc_work_done))
    )
    if (!is.null(input$jc_time_taken) && trimws(input$jc_time_taken) != "") lines <- c(lines, paste0("Time Taken: ", input$jc_time_taken))
    if (!is.null(input$jc_done_by) && trimws(input$jc_done_by) != "") lines <- c(lines, paste0("Done By: ", input$jc_done_by))
    lines <- c(lines, paste0("Date Completed: ", as.character(input$jc_date_completed)))
    paste(lines, collapse = "\n")
  }
  build_truckservice_desc <- function() {
    next_due <- if (!is.null(input$ts_next_due) && trimws(input$ts_next_due) != "") {
      trimws(input$ts_next_due)
    } else {
      as.character(input$ts_date + 42)
    }
    lines <- c(
      paste0("Reg No.: ", ifelse(is.null(input$ts_reg) || input$ts_reg == "", "-", input$ts_reg)),
      paste0("Odometer Reading: ", ifelse(is.null(input$ts_odometer) || input$ts_odometer == "", "-", input$ts_odometer)),
      paste0("Inspected By: ", ifelse(is.null(input$ts_inspector) || input$ts_inspector == "", "-", input$ts_inspector)),
      paste0("Next Inspection Due (6 weeks): ", next_due)
    )
    failed <- c()
    for (sec in names(TRUCK_SERVICE_CHECKLIST)) {
      all_items <- TRUCK_SERVICE_CHECKLIST[[sec]]
      ticked <- input[[paste0("ts_chk_", make.names(sec))]]
      not_ticked <- setdiff(all_items, ticked)
      if (length(not_ticked) > 0) failed <- c(failed, paste0(sec, ": ", paste(not_ticked, collapse = ", ")))
    }
    if (length(failed) > 0) {
      lines <- c(lines, "Items flagged (not serviceable):", paste0("  - ", failed))
    } else {
      lines <- c(lines, "All checklist items serviceable.")
    }
    if (!is.null(input$ts_tyre_notes) && trimws(input$ts_tyre_notes) != "") lines <- c(lines, paste0("Tyre Notes: ", input$ts_tyre_notes))
    if (!is.null(input$ts_defects) && trimws(input$ts_defects) != "") lines <- c(lines, paste0("Defects Found: ", input$ts_defects))
    if (!is.null(input$ts_rectified_by) && trimws(input$ts_rectified_by) != "") lines <- c(lines, paste0("Rectified By: ", input$ts_rectified_by))
    paste(lines, collapse = "\n")
  }
  observeEvent(input$ih_submit, {
    iid <- inv_selected()
    # Job Cards can point at a different machine than the one whose
    # page the modal was opened from - resolve that here so the
    # entry files against the right item, falling back to the
    # current item if the picker doesn't match anything.
    if (input$ih_type == "Job Card") {
      matched <- find_item_id(input$jc_category, input$jc_subcategory, input$jc_item, inventory_data())
      if (!is.na(matched)) iid <- matched
    }
    if (input$ih_type == "Service Inspection") {
      matched <- find_item_id(input$sv_category, input$sv_subcategory, input$sv_item, inventory_data())
      if (!is.na(matched)) iid <- matched
    }
    desc <- if (input$ih_type == "Driver Assigned") input$ih_driver
    else if (input$ih_type == "Hours Updated") {
      req(input$ih_hours, !is.na(input$ih_hours))
      paste0(format(input$ih_hours, big.mark = ","), " hours")
    }
    else if (input$ih_type == "Service Inspection") build_service_desc()
    else if (input$ih_type == "Job Card") build_jobcard_desc()
    else if (input$ih_type == "Truck Service") build_truckservice_desc()
    else input$ih_desc
    req(desc, desc != "")
    # Subcontractor Mechanic Work also creates a real Invoice, tagged
    # onto this same History entry (rather than a second entry) - so
    # it counts towards spend/Invoice Analysis/reports like any other
    # invoice, and Editing/Deleting the invoice from the Invoices tab
    # keeps this History entry in sync the same way it already does
    # for ordinary auto-logged Invoice entries.
    is_subcontractor <- input$ih_type == "Mechanic Work" && isTRUE(input$ih_subcontractor)
    if (is_subcontractor) {
      if (is.null(input$ih_sub_company) || trimws(input$ih_sub_company) == "") {
        showNotification("Subcontractor Company is required.", type = "error"); return()
      }
      if (is.null(input$ih_sub_amount) || is.na(input$ih_sub_amount)) {
        showNotification("Amount is required for subcontractor work.", type = "error"); return()
      }
    }
    extra <- c()
    if (!is.null(input$ih_location) && trimws(input$ih_location) != "") extra <- c(extra, paste0("Location: ", input$ih_location))
    if (!is.null(input$ih_price) && !is.na(input$ih_price)) extra <- c(extra, paste0("Price: £", sprintf("%.2f", input$ih_price)))
    if (is_subcontractor) extra <- c(extra, paste0("Company: ", trimws(input$ih_sub_company)),
                                     paste0("Amount: £", sprintf("%.2f", input$ih_sub_amount)))
    # Kept last on purpose: the printed sheet takes everything after this
    # marker as the comment, so a multi-line comment stays intact.
    if (!is.null(input$ih_comments) && trimws(input$ih_comments) != "") {
      extra <- c(extra, paste0("Additional Comments: ", trimws(input$ih_comments)))
    }
    if (length(extra) > 0) desc <- paste(c(desc, extra), collapse = "\n")
    invoice_id_for_entry <- NA_character_
    if (is_subcontractor) {
      item_row <- inventory_data()[inventory_data()$ItemID == iid, ]
      invoice_id_for_entry <- next_invoice_id()
      new_invoice <- data.frame(
        InvoiceID = invoice_id_for_entry,
        Company = trimws(input$ih_sub_company),
        Invoice_Number = "", Account_Number = "", Document_Number = "",
        Date = as.character(input$ih_date),
        Amount = input$ih_sub_amount,
        Description = desc,
        SPEN_Order_Number = "",
        Category = if (nrow(item_row) > 0) item_row$Category[1] else "",
        SubCategory = if (nrow(item_row) > 0) item_row$SubCategory[1] else "",
        Reference_PMK_Number = if (nrow(item_row) > 0) item_identifier(item_row[1, ]) else iid,
        LoggedBy = user_name(),
        stringsAsFactors = FALSE
      )
      invoices_data(bind_rows(invoices_data(), new_invoice))
    }
    new_entry <- data.frame(
      ItemID = iid,
      DateTime = paste(as.character(input$ih_date), format(Sys.time(), "%H:%M")),
      EntryType = input$ih_type, Description = desc, RecordedBy = user_name(),
      InvoiceID = invoice_id_for_entry,
      EntryID = next_entry_id(),
      LinkedEntryID = if (!is.null(input$ih_link) && input$ih_link != "") input$ih_link else NA_character_,
      stringsAsFactors = FALSE
    )
    plant_history(bind_rows(plant_history(), new_entry))
    df <- inventory_data()
    if (input$ih_type == "Driver Assigned") df$Driver[df$ItemID == iid] <- input$ih_driver
    if (input$ih_type == "Hours Updated") df$Hours[df$ItemID == iid] <- as.character(input$ih_hours)
    if (!is.null(input$ih_location) && trimws(input$ih_location) != "") df$Location[df$ItemID == iid] <- trimws(input$ih_location)
    inventory_data(df)
    removeModal()
    if (is_subcontractor) showNotification("Entry saved, and logged as an Invoice too.", type = "message")
    else showNotification("Entry saved.", type = "message")
    item_row_for_log <- df[df$ItemID == iid, ]
    log_notification(paste0("Logged '", input$ih_type, "' on ",
                            if (nrow(item_row_for_log) > 0) item_identifier(item_row_for_log[1, ]) else iid))
  })
  # ---- Link / Unlink History entries ----
  # Retroactively links two existing entries on the same item (e.g. a
  # Job Card and the Invoice for that same job), separate from the
  # optional "link at creation time" picker in Add History Entry.
  observeEvent(input$link_entry_click, {
    eid <- input$link_entry_click
    ph <- plant_history()
    row <- ph[ph$EntryID == eid, ]
    req(nrow(row) == 1)
    others <- ph[ph$ItemID == row$ItemID[1] & ph$EntryID != eid & !is.na(ph$EntryID) & ph$EntryID != "", ]
    removeModal()  # ensure any stale modal is torn down before opening a new one
    if (nrow(others) == 0) {
      showNotification("No other entries for this item to link to yet.", type = "warning")
      return()
    }
    choices <- setNames(others$EntryID, paste0(others$EntryType, " - ", others$DateTime))
    session$userData$linking_entry_id <- eid
    showModal(modalDialog(
      title = paste("Link", row$EntryType[1], "-", row$DateTime[1], "to..."),
      selectInput("link_entry_target", "Link to", choices = choices),
      footer = tagList(modalButton("Cancel"), actionButton("link_entry_submit", "Save Link", class = "btn-primary"))
    ))
  })
  observeEvent(input$link_entry_submit, {
    eid <- session$userData$linking_entry_id
    req(eid, input$link_entry_target)
    ph <- plant_history()
    ph$LinkedEntryID[ph$EntryID == eid] <- input$link_entry_target
    plant_history(ph)
    removeModal()
    showNotification("Entries linked.", type = "message")
  })
  observeEvent(input$unlink_entry_click, {
    eid <- input$unlink_entry_click
    ph <- plant_history()
    # The link could be stored on this entry or on its counterpart -
    # clear whichever side actually holds it.
    ph$LinkedEntryID[!is.na(ph$LinkedEntryID) & ph$LinkedEntryID == eid] <- NA_character_
    ph$LinkedEntryID[ph$EntryID == eid] <- NA_character_
    plant_history(ph)
    showNotification("Entries unlinked.", type = "message")
  })
  # ---- Edit / Delete History entries ----
  # Deliberately a lightweight edit (date + the full description text)
  # rather than reopening the original type-specific form (Service
  # Inspection, Job Card, Truck Service etc all flatten their checklist
  # answers into Description when saved, so there's no clean way back
  # into the structured fields) - this covers fixing a typo, a wrong
  # date, or a mistaken entry, which is what Edit/Delete here is for.
  # For "Invoice" entries specifically: editing/deleting here only
  # touches the History record, not the Invoice itself - use the
  # Invoice's own Edit/Delete on the Invoices tab for that.
  observeEvent(input$edit_entry_click, {
    eid <- input$edit_entry_click
    row <- plant_history()[plant_history()$EntryID == eid, ]
    req(nrow(row) == 1)
    editing_history_entry(eid)
    date_part <- tryCatch(as.Date(strsplit(row$DateTime[1], " ")[[1]][1]), error = function(e) Sys.Date())
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = paste("Edit Entry -", row$EntryType[1]),
      dateInput("eh_date", "Date", value = date_part),
      textAreaInput("eh_desc", "Description", value = row$Description[1], rows = 8),
      footer = tagList(modalButton("Cancel"), actionButton("eh_submit", "Save Changes", class = "btn-primary"))
    ))
  })
  observeEvent(input$eh_submit, {
    eid <- editing_history_entry(); req(eid)
    req(input$eh_desc, trimws(input$eh_desc) != "")
    ph <- plant_history()
    row <- ph[ph$EntryID == eid, ]
    req(nrow(row) == 1)
    old_time <- strsplit(row$DateTime[1], " ")[[1]]
    time_part <- if (length(old_time) > 1) old_time[2] else format(Sys.time(), "%H:%M")
    ph$DateTime[ph$EntryID == eid] <- paste(as.character(input$eh_date), time_part)
    ph$Description[ph$EntryID == eid] <- input$eh_desc
    plant_history(ph)
    editing_history_entry(NULL)
    removeModal()
    showNotification("Entry updated.", type = "message")
  })
  observeEvent(input$delete_entry_click, {
    session$userData$pending_delete_entry <- input$delete_entry_click
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = "Remove this entry?",
      "This history entry will be permanently removed. This cannot be undone.",
      footer = tagList(modalButton("Cancel"), actionButton("confirm_delete_entry", "Yes, remove", class = "btn-danger"))
    ))
  })
  observeEvent(input$confirm_delete_entry, {
    eid <- session$userData$pending_delete_entry
    ph <- plant_history()
    # Clear any link pointing at the entry being removed, from either side.
    ph$LinkedEntryID[!is.na(ph$LinkedEntryID) & ph$LinkedEntryID == eid] <- NA_character_
    ph <- ph[ph$EntryID != eid, ]
    plant_history(ph)
    removeModal()
    showNotification("Entry removed.", type = "message")
  })
  # ---- Print a single Job Card ----
  # Read-only throughout: nothing here writes to plant_history() or
  # inventory_data(), it only reads the entry and renders a PDF.
  printing_entry <- reactiveVal(NULL)
  observeEvent(input$print_entry_click, {
    eid <- input$print_entry_click
    row <- plant_history()[plant_history()$EntryID == eid, ]
    req(nrow(row) == 1)
    printing_entry(eid)
    item <- inventory_data()[inventory_data()$ItemID == row$ItemID[1], ]
    label <- if (nrow(item) > 0) item_identifier(item[1, ]) else row$ItemID[1]
    removeModal()
    showModal(modalDialog(
      title = paste0("Print Job Card - ", label),
      p("A one-page A4 sheet: machine details, the work requested and carried out, any additional comments, and space for signatures. A long write-up runs onto a second page rather than being cut off."),
      downloadButton("entry_pdf", "Download Job Card (PDF)", class = "btn-primary"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  printing_parts <- reactive({
    eid <- printing_entry(); req(eid)
    row <- plant_history()[plant_history()$EntryID == eid, ]
    req(nrow(row) == 1)
    item <- inventory_data()[inventory_data()$ItemID == row$ItemID[1], ]
    req(nrow(item) > 0)
    list(entry = row[1, ], item = item[1, ])
  })
  output$entry_pdf <- downloadHandler(
    filename = function() {
      p <- printing_parts()
      paste0("pmk_job_card_", gsub("[^A-Za-z0-9]+", "_", item_identifier(p$item)), "_", p$entry$EntryID, ".pdf")
    },
    content = function(file) {
      p <- printing_parts()
      generate_jobcard_pdf(file, p$entry, p$item)
    }
  )
  # ---- Entry counts + per-item history download ----
  # Named ItemID -> count, shared by every plant box so the whole list
  # is counted once per history change rather than once per box.
  history_counts <- reactive({
    h <- plant_history()
    if (nrow(h) == 0) return(setNames(integer(0), character(0)))
    tb <- table(h$ItemID)
    setNames(as.integer(tb), names(tb))
  })
  item_history_row <- reactive({
    iid <- inv_selected(); req(iid)
    row <- inventory_data()[inventory_data()$ItemID == iid, ]
    req(nrow(row) > 0)
    row[1, ]
  })
  item_history_export <- reactive({
    iid <- inv_selected(); req(iid)
    h <- plant_history()[plant_history()$ItemID == iid, , drop = FALSE]
    h <- h[order(h$DateTime, decreasing = TRUE), , drop = FALSE]
    data.frame(`Date/Time` = h$DateTime, Type = h$EntryType,
               Description = ifelse(is.na(h$Description), "", h$Description),
               `Recorded By` = h$RecordedBy, check.names = FALSE, stringsAsFactors = FALSE)
  })
  item_history_file_stub <- function() {
    gsub("[^A-Za-z0-9]+", "_", item_identifier(item_history_row()))
  }
  output$item_history_csv <- downloadHandler(
    filename = function() paste0("pmk_history_", item_history_file_stub(), "_", Sys.Date(), ".csv"),
    content = function(file) write.csv(item_history_export(), file, row.names = FALSE)
  )
  output$item_history_pdf <- downloadHandler(
    filename = function() paste0("pmk_history_", item_history_file_stub(), "_", Sys.Date(), ".pdf"),
    content = function(file) {
      row <- item_history_row()
      pdf(file, width = 11.69, height = 8.27)  # A4 landscape, same as the full report
      on.exit(dev.off(), add = TRUE)
      title <- paste0(item_identifier(row), if (row$Machine != "") paste0(" - ", row$Machine) else "", " - History")
      subtitle <- paste0(row$Category, " > ", row$SubCategory,
                         "  |  Driver: ", if (row$Driver == "") "Unassigned" else row$Driver,
                         "  |  ", nrow(item_history_export()), " entries",
                         "  |  Generated ", format(Sys.time(), "%d %b %Y, %H:%M"))
      draw_report_section(title, subtitle, item_history_export(),
                          col_widths = c(1.2, 1.1, 5.2, 1.1), wrap_cols = "Description",
                          wrap_chars = 105, max_weight = 30)
    }
  )
  # ---- Add / Edit item form ----
  item_form_ui <- function(prefill = NULL) {
    all_subcats <- sort(unique(c(unlist(SUBCATEGORY_MAP), inventory_data()$SubCategory)))
    all_subcats <- all_subcats[all_subcats != ""]
    g <- function(field, default = "") if (is.null(prefill)) default else prefill[[field]]
    tagList(
      fluidRow(
        column(6, selectInput("if_category", "Category *", choices = CATEGORY_OPTIONS, selected = g("Category", CATEGORY_OPTIONS[1]))),
        column(6, selectizeInput("if_subcategory", "Sub-Category *", choices = all_subcats,
                                 selected = g("SubCategory"),
                                 options = list(create = TRUE, placeholder = "Select or type a sub-category")))
      ),
      textInput("if_machine", "Machine", value = g("Machine"), placeholder = "e.g. 8t CAT 308"),
      fluidRow(
        column(4, textInput("if_pmk", "PMK Number/Weld", value = g("PMK_Number"))),
        column(4, textInput("if_reg", "Registration", value = g("Registration"))),
        column(4, textInput("if_serial", "Serial Number", value = g("SerialNumber")))
      ),
      fluidRow(
        column(4, textInput("if_driver", "Driver", value = g("Driver"), placeholder = "e.g. Spare, or a name")),
        column(4, textInput("if_location", "Location", value = g("Location"), placeholder = "e.g. Yard, Hawick")),
        column(4, textInput("if_hours", "Hours", value = g("Hours"), placeholder = "e.g. 1250"))
      ),
      # Only relevant when editing an existing item (a brand new item
      # has no history yet) - appears as soon as the Driver field is
      # actually changed from what it was, so it's not clutter for
      # every other field on the form.
      if (!is.null(prefill)) conditionalPanel(
        condition = paste0("input.if_driver !== '", js_escape_sq(g("Driver")), "'"),
        checkboxInput("if_log_driver_history", "Also log this as a new \"Driver Assigned\" history entry", value = TRUE)
      ),
      fluidRow(
        column(4, textInput("if_datepurch", "Date Purchased", value = g("DatePurchased"), placeholder = "DD/MM/YY")),
        column(4, textInput("if_warranty", "Warranty End Date", value = g("WarrantyEndDate"), placeholder = "DD/MM/YY")),
        column(4, textInput("if_mot", "MOT Due", value = g("MOTDue"), placeholder = "DD/MM/YY"))
      ),
      selectInput("if_active", "Active", choices = c("Yes", "No"), selected = g("Active", "Yes")),
      selectInput("if_truckservice", "Requires 6-Weekly Truck Service Inspection?",
                  choices = c("No", "Yes"), selected = g("TruckServiceRequired", "No")),
      textAreaInput("if_notes", "Notes", value = g("Notes"), rows = 2)
    )
  }
  observeEvent(input$add_item_btn, {
    editing_item(NULL)
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = "Add New Item", size = "l",
      item_form_ui(),
      footer = tagList(modalButton("Cancel"), actionButton("item_form_submit", "Save Item", class = "btn-primary"))
    ))
  })
  observeEvent(input$edit_item_click, {
    iid <- input$edit_item_click
    df <- inventory_data()
    row <- df[df$ItemID == iid, ]
    req(nrow(row) == 1)
    editing_item(iid)
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = paste("Edit Item -", item_identifier(row[1, ])), size = "l",
      item_form_ui(prefill = as.list(row[1, ])),
      footer = tagList(modalButton("Cancel"), actionButton("item_form_submit", "Save Changes", class = "btn-primary"))
    ))
  })
  observeEvent(input$detail_edit_btn, {
    iid <- inv_selected()
    df <- inventory_data()
    row <- df[df$ItemID == iid, ]
    req(nrow(row) == 1)
    editing_item(iid)
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = paste("Edit Item -", item_identifier(row[1, ])), size = "l",
      item_form_ui(prefill = as.list(row[1, ])),
      footer = tagList(modalButton("Cancel"), actionButton("item_form_submit", "Save Changes", class = "btn-primary"))
    ))
  })
  observeEvent(input$item_form_submit, {
    cat <- input$if_category
    sub <- trimws(input$if_subcategory)
    if (is.null(cat) || cat == "") { showNotification("Category is required.", type = "error"); return() }
    if (is.null(sub) || sub == "") { showNotification("Sub-Category is required.", type = "error"); return() }
    row_data <- data.frame(
      Category = cat, SubCategory = sub, Machine = input$if_machine,
      PMK_Number = input$if_pmk, Registration = input$if_reg, SerialNumber = input$if_serial,
      Driver = input$if_driver, Location = input$if_location, Hours = input$if_hours,
      DatePurchased = input$if_datepurch, WarrantyEndDate = input$if_warranty, MOTDue = input$if_mot,
      Active = input$if_active, Notes = input$if_notes,
      TruckServiceRequired = input$if_truckservice,
      stringsAsFactors = FALSE
    )
    df <- inventory_data()
    if (is.null(editing_item())) {
      row_data$ItemID <- next_item_id()
      row_data$Gang <- ""
      df <- bind_rows(df, row_data)
      showNotification("Item added.", type = "message")
      log_notification(paste0("Added plant item ", item_identifier(row_data)))
    } else {
      iid <- editing_item()
      keep_cols <- c("ItemID", "Gang")
      existing <- df[df$ItemID == iid, keep_cols]
      old_driver_val <- trimws(df$Driver[df$ItemID == iid][1])
      row_data$ItemID <- existing$ItemID
      row_data$Gang <- existing$Gang
      df <- df[df$ItemID != iid, ]
      df <- bind_rows(df, row_data)
      showNotification("Item updated.", type = "message")
      log_notification(paste0("Updated plant item ", item_identifier(row_data)))
      # Only the edit form has if_log_driver_history (Add New Item
      # doesn't render it) - box only shows once Driver's actually
      # been changed, and only logs if it's still ticked and the new
      # value isn't blank, so unticking or clearing it back out
      # doesn't leave a stray entry.
      new_driver_val <- trimws(input$if_driver)
      if (isTRUE(input$if_log_driver_history) && new_driver_val != "" && !identical(old_driver_val, new_driver_val)) {
        log_driver_assigned_entries(iid, new_driver_val)
      }
    }
    inventory_data(df)
    editing_item(NULL)
    removeModal()
  })
  observeEvent(input$delete_item_click, {
    session$userData$pending_delete_item <- input$delete_item_click
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = "Remove this item?",
      "This item will be permanently removed from Inventory. This cannot be undone.",
      footer = tagList(modalButton("Cancel"), actionButton("confirm_delete_item", "Yes, remove", class = "btn-danger"))
    ))
  })
  observeEvent(input$detail_delete_btn, {
    session$userData$pending_delete_item <- inv_selected()
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = "Remove this item?",
      "This item will be permanently removed from Inventory. This cannot be undone.",
      footer = tagList(modalButton("Cancel"), actionButton("confirm_delete_item", "Yes, remove", class = "btn-danger"))
    ))
  })
  observeEvent(input$confirm_delete_item, {
    iid <- session$userData$pending_delete_item
    df <- inventory_data()
    old_row <- df[df$ItemID == iid, ]
    df <- df[df$ItemID != iid, ]; inventory_data(df)
    if (identical(inv_selected(), iid)) { inv_view("list"); inv_selected(NULL) }
    removeModal()
    showNotification("Item removed.", type = "message")
    if (nrow(old_row) > 0) log_notification(paste0("Deleted plant item ", item_identifier(old_row[1, ])))
  })
  # -------------------------------------------------------------
  # PLANT WHEREABOUTS - "Gang Sheets"
  # -------------------------------------------------------------
  output$whereabouts_tab_content <- renderUI({ whereabouts_ui(role()) })
  whereabouts_ui <- function(r) {
    df <- inventory_data()
    tagList(
      br(),
      p(class = "text-muted",
        "Gang sheets persist week to week - edit an existing one instead of recreating it. ",
        "Plant already assigned to a gang won't show up as an option when editing a different one, so double-booking isn't possible."),
      fluidRow(
        column(6, h5("Gang Sheets")),
        column(6, style = "text-align:right;",
               downloadButton("gang_sheets_download", "Download (CSV)", class = "btn-outline-secondary btn-sm me-2"),
               if (r %in% c("Admin", "Boss", "Plantman")) actionButton("new_gang_sheet_btn", "+ Create New Gang Sheet", class = "btn-primary btn-sm"))
      ),
      if (r %in% c("Admin", "Boss", "Plantman") && length(gang_list()) > 0) div(
        style = "margin:10px 0 18px;",
        actionButton("bulk_edit_gangs_btn", "Edit All Gang Assignments", class = "btn-warning w-100",
                     style = "font-weight:600; padding:12px; font-size:1.05rem;")
      ),
      if (length(gang_list()) == 0) div(class = "alert alert-secondary", "No gang sheets yet.")
      else {
        gang_panel_for <- function(g) {
          g_rows <- df[df$Gang == g, ]
          meta_row <- gang_meta()[gang_meta()$Gang == g, ]
          ganger_nm <- if (nrow(meta_row) > 0) meta_row$Ganger[1] else ""
          loc_nm <- if (nrow(meta_row) > 0) meta_row$Location[1] else ""
          detail_bits <- c(
            if (!is.na(ganger_nm) && ganger_nm != "") paste0("Ganger: ", ganger_nm),
            if (!is.na(loc_nm) && loc_nm != "") paste0("Location: ", loc_nm)
          )
          panel_title <- paste0(g, " - ", nrow(g_rows), " item(s)",
                                 if (length(detail_bits) > 0) paste0(" (", paste(detail_bits, collapse = " | "), ")") else "")
          accordion_panel(
            title = panel_title, value = g,
            if (r %in% c("Admin", "Boss", "Plantman")) div(class = "mb-2",
                tags$a(href = "#", style = "font-size:0.85rem; margin-right:12px;",
                       onclick = sprintf("Shiny.setInputValue('edit_gang_click', '%s', {priority:'event'}); return false;", js_escape_sq(g)),
                       "Edit"),
                tags$a(href = "#", style = "font-size:0.85rem; color:#9C2B2B;",
                       onclick = sprintf("Shiny.setInputValue('delete_gang_click', '%s', {priority:'event'}); return false;", js_escape_sq(g)),
                       "Delete")
            ),
            if (nrow(g_rows) == 0) p(class = "text-muted mb-0", "No plant assigned.")
            else tagList(lapply(seq_len(nrow(g_rows)), function(i) item_row(g_rows[i, ], r, clickable = FALSE, show_actions = TRUE, entry_counts = history_counts())))
          )
        }
        # Group gang sheets by Location, so crews on the same site sit
        # together rather than one long undifferentiated list -
        # gangs with no Location set fall into their own bucket at
        # the end rather than being hidden.
        gm <- gang_meta()
        loc_of <- setNames(vapply(gang_list(), function(g) {
          row <- gm[gm$Gang == g, ]
          loc <- if (nrow(row) > 0) row$Location[1] else ""
          if (is.na(loc) || trimws(loc) == "") "No Location Set" else trimws(loc)
        }, character(1)), gang_list())
        all_locs <- unique(unname(loc_of))
        all_locs <- c(sort(setdiff(all_locs, "No Location Set")), intersect(all_locs, "No Location Set"))
        tagList(lapply(all_locs, function(loc) {
          gangs_here <- names(loc_of)[loc_of == loc]
          tagList(
            h6(loc, style = "margin-top:16px; color:#5B6770; text-transform:uppercase; letter-spacing:0.5px; font-size:0.85rem;"),
            do.call(accordion, c(list(id = paste0("gang_sheets_accordion_", make.names(loc)), open = FALSE),
                                  lapply(gangs_here, gang_panel_for)))
          )
        }))
      },
      br(),
      h5("Unassigned Plant"),
      p(class = "text-muted", "Available to add to a gang sheet."),
      {
        leftover <- df[df$Gang == "" | is.na(df$Gang), ]
        if (nrow(leftover) == 0) div(class = "alert alert-secondary", "Everything is assigned to a gang.")
        else nested_inventory_accordion("unassigned_accordion", leftover, r, show_actions = TRUE, clickable = FALSE,
                                        entry_counts = history_counts())
      }
    )
  }
  # One row per Gang, one column per plant type - Location and Gang
  # first, then Small Excavator/Pecker-Breaker/Trailer pulled out to
  # the front since those are the types crews check for first, then
  # every other sub-category follows automatically (grouped by
  # Category, in CATEGORY_OPTIONS order) so a new item type just
  # slots into its own column with no further changes needed here.
  # "Pecker/Breaker" rolls up all of 8T/6T/1T Breaker into one column
  # - gangs care whether they have a breaker, not the tonnage.
  gang_export_columns <- function() {
    priority <- list(
      "Small Excavator" = list(match = function(rows) rows$SubCategory == "Small Excavator"),
      "Pecker/Breaker"  = list(match = function(rows) rows$Category == "Breaker"),
      "Trailer"         = list(match = function(rows) rows$SubCategory == "Trailer")
    )
    used_subcats <- c("Small Excavator", SUBCATEGORY_MAP[["Breaker"]], "Trailer")
    other_subcats <- unlist(lapply(CATEGORY_OPTIONS, function(cat) setdiff(SUBCATEGORY_MAP[[cat]], used_subcats)),
                             use.names = FALSE)
    other <- setNames(lapply(other_subcats, function(sc) list(match = function(rows) rows$SubCategory == sc)), other_subcats)
    c(priority, other)
  }
  gang_sheet_export_data <- reactive({
    df <- inventory_data()
    gm <- gang_meta()
    gl <- gang_list()
    if (length(gl) == 0) return(data.frame(Message = "No gang sheets yet."))
    cols <- gang_export_columns()
    rows <- lapply(gl, function(g) {
      g_rows <- df[df$Gang == g, ]
      meta_row <- gm[gm$Gang == g, ]
      loc_nm <- if (nrow(meta_row) > 0) meta_row$Location[1] else ""
      if (is.na(loc_nm) || trimws(loc_nm) == "") loc_nm <- "No Location Set"
      row <- data.frame(Location = loc_nm, Gang = g, stringsAsFactors = FALSE)
      for (col_name in names(cols)) {
        matched <- if (nrow(g_rows) == 0) g_rows else g_rows[cols[[col_name]]$match(g_rows), , drop = FALSE]
        row[[col_name]] <- if (nrow(matched) == 0) "" else
          paste(vapply(seq_len(nrow(matched)), function(i) item_identifier(matched[i, ]), character(1)), collapse = ", ")
      }
      row
    })
    out <- do.call(rbind, rows)
    out[order(out$Location, out$Gang), ]
  })
  output$gang_sheets_download <- downloadHandler(
    filename = function() paste0("pmk_gang_sheets_", Sys.Date(), ".csv"),
    content = function(file) write.csv(gang_sheet_export_data(), file, row.names = FALSE)
  )
  # elsewhere_df (optional) is plant assigned to a DIFFERENT gang -
  # shown at the bottom of each sub-category as a second, clearly
  # labelled checkbox group so a mis-assigned item can be swapped
  # straight onto this gang instead of having to un-assign it first.
  # Ticking one just moves it - the save handlers already set Gang
  # on every ticked ItemID, whichever gang it came from.
  gang_sheet_form <- function(pool_df, selected_ids = character(0), elsewhere_df = NULL) {
    if (is.null(elsewhere_df)) elsewhere_df <- pool_df[0, ]
    lapply(CATEGORY_OPTIONS, function(cat) {
      cat_rows <- pool_df[pool_df$Category == cat, ]
      elsewhere_cat <- elsewhere_df[elsewhere_df$Category == cat, ]
      subcats <- union(subcats_for(cat, pool_df), subcats_for(cat, elsewhere_cat))
      sub_panels <- lapply(subcats, function(sub) {
        sub_rows <- natural_sort_rows(cat_rows[cat_rows$SubCategory == sub, ])
        elsewhere_rows <- natural_sort_rows(elsewhere_cat[elsewhere_cat$SubCategory == sub, ])
        accordion_panel(
          title = paste0(sub, " (", nrow(sub_rows), " available)"),
          value = paste0(cat, "___", sub),
          tagList(
            if (nrow(sub_rows) == 0 && nrow(elsewhere_rows) == 0) p(class = "text-muted mb-0", "None available.")
            else if (nrow(sub_rows) > 0) checkboxGroupInput(paste0("gang_form_", make.names(cat), "_", make.names(sub)), NULL,
                                    choices = setNames(sub_rows$ItemID, paste0(ifelse(sub_rows$Machine == "", "(no machine name)", sub_rows$Machine), " - ", vapply(seq_len(nrow(sub_rows)), function(i) item_identifier(sub_rows[i, ]), character(1)))),
                                    selected = intersect(sub_rows$ItemID, selected_ids)),
            if (nrow(elsewhere_rows) > 0) tagList(
              div(class = "text-muted", style = "font-size:0.8rem; margin:4px 0 2px;",
                  "Already assigned to another gang - ticking one swaps it onto this gang instead:"),
              checkboxGroupInput(paste0("gang_form_swap_", make.names(cat), "_", make.names(sub)), NULL,
                                 choices = setNames(elsewhere_rows$ItemID,
                                                    paste0(ifelse(elsewhere_rows$Machine == "", "(no machine name)", elsewhere_rows$Machine), " - ",
                                                           vapply(seq_len(nrow(elsewhere_rows)), function(i) item_identifier(elsewhere_rows[i, ]), character(1)),
                                                           "  [currently on ", elsewhere_rows$Gang, "]")))
            )
          )
        )
      })
      accordion_panel(
        title = paste0(cat, " (", nrow(cat_rows), " available)"),
        value = cat,
        if (length(sub_panels) == 0) p(class = "text-muted mb-0", "None available.")
        else do.call(accordion, c(list(id = paste0("gang_form_sub_", make.names(cat))), sub_panels))
      )
    })
  }
  collect_ticked_items <- function() {
    df <- inventory_data()
    pairs <- unique(df[, c("Category", "SubCategory")])
    unlist(lapply(seq_len(nrow(pairs)), function(i) {
      cat <- pairs$Category[i]; sub <- pairs$SubCategory[i]
      c(input[[paste0("gang_form_", make.names(cat), "_", make.names(sub))]],
        input[[paste0("gang_form_swap_", make.names(cat), "_", make.names(sub))]])
    }))
  }
  ganger_choices <- function() setNames(c("", ganger_list()), c("None", ganger_list()))
  # When creating a brand new gang sheet, picking a Ganger suggests
  # that same name as the Gang Name too (only if the name field is
  # still blank, so it doesn't clobber anything already typed) - so
  # the gang sheet is labelled after whoever's actually running it,
  # and Plant Whereabouts shows one consistent name rather than an
  # arbitrary "Gang D" next to a separate Ganger.
  observeEvent(input$gang_form_ganger, {
    if (!is.null(input$gang_form_ganger) && input$gang_form_ganger != "" &&
        (is.null(input$gang_form_name) || trimws(input$gang_form_name) == "")) {
      updateTextInput(session, "gang_form_name", value = input$gang_form_ganger)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$new_gang_sheet_btn, {
    df <- inventory_data()
    unassigned <- df[df$Gang == "" | is.na(df$Gang), ]
    elsewhere <- df[df$Gang != "" & !is.na(df$Gang), ]
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = "Create New Gang Sheet", size = "l",
      fluidRow(
        column(6, textInput("gang_form_name", "Gang Name", placeholder = "e.g. Gang D")),
        column(6, selectInput("gang_form_ganger", "Ganger (optional)", choices = ganger_choices(), selected = ""))
      ),
      textInput("gang_form_location", "Location (optional)", placeholder = "e.g. Site name / postcode"),
      p(class = "text-muted", "Tick which unassigned plant belongs to this gang. Plant already on another gang sheet is listed at the bottom of each section in case it was assigned by mistake - ticking it moves it here."),
      do.call(accordion, c(list(id = "gang_form_accordion", open = TRUE), gang_sheet_form(unassigned, elsewhere_df = elsewhere))),
      footer = tagList(modalButton("Cancel"), actionButton("gang_form_submit_new", "Create Gang Sheet", class = "btn-primary"))
    ))
  })
  observeEvent(input$edit_gang_click, {
    g <- input$edit_gang_click
    editing_gang(g)
    df <- inventory_data()
    pool <- df[df$Gang == g | df$Gang == "" | is.na(df$Gang), ]
    elsewhere <- df[df$Gang != "" & !is.na(df$Gang) & df$Gang != g, ]
    current <- df$ItemID[df$Gang == g]
    meta_row <- gang_meta()[gang_meta()$Gang == g, ]
    cur_ganger <- if (nrow(meta_row) > 0) meta_row$Ganger[1] else ""
    cur_location <- if (nrow(meta_row) > 0) meta_row$Location[1] else ""
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = paste("Edit Gang Sheet -", g), size = "l",
      fluidRow(
        column(6, selectInput("gang_form_ganger_edit", "Ganger (optional)", choices = ganger_choices(), selected = cur_ganger)),
        column(6, textInput("gang_form_location_edit", "Location (optional)", value = cur_location, placeholder = "e.g. Site name / postcode"))
      ),
      p(class = "text-muted", "Tick which plant belongs to this gang. Plant on another gang sheet is listed at the bottom of each section in case it was assigned by mistake - ticking it moves it here."),
      do.call(accordion, c(list(id = "gang_form_accordion_edit", open = TRUE), gang_sheet_form(pool, current, elsewhere_df = elsewhere))),
      footer = tagList(modalButton("Cancel"), actionButton("gang_form_submit_edit", "Save Gang Sheet", class = "btn-primary"))
    ))
  })
  save_gang_meta <- function(name, ganger, location) {
    gm <- gang_meta()
    gm <- gm[gm$Gang != name, ]
    gm <- bind_rows(gm, data.frame(Gang = name, Ganger = ganger, Location = trimws(location), stringsAsFactors = FALSE))
    gang_meta(gm)
  }
  # Logs a "Driver Assigned" Plant History entry for each item id
  # whose Driver actually changed to this ganger - shared by the
  # single gang sheet Create/Edit flows below so filling in Plant
  # Whereabouts leaves the same audit trail as assigning a driver
  # directly from an item's own history. Reserves a block of entry
  # ids up front rather than calling next_entry_id() per row, since
  # that only reflects plant_history() as of before this call and
  # would hand out duplicate ids across a loop.
  log_driver_assigned_entries <- function(ids, ganger) {
    if (length(ids) == 0) return(invisible())
    existing_nums <- suppressWarnings(as.integer(gsub("HIST-", "", plant_history()$EntryID)))
    existing_nums <- existing_nums[!is.na(existing_nums)]
    start_n <- if (length(existing_nums) == 0) 1 else max(existing_nums) + 1
    new_entries <- lapply(seq_along(ids), function(idx) {
      data.frame(ItemID = ids[idx], DateTime = format(Sys.time(), "%Y-%m-%d %H:%M"),
                 EntryType = "Driver Assigned", Description = ganger, RecordedBy = user_name(),
                 InvoiceID = NA_character_, EntryID = sprintf("HIST-%04d", start_n + idx - 1), LinkedEntryID = NA_character_,
                 stringsAsFactors = FALSE)
    })
    plant_history(bind_rows(plant_history(), do.call(rbind, new_entries)))
  }
  observeEvent(input$gang_form_submit_new, {
    name <- trimws(input$gang_form_name)
    req(name, name != "")
    if (name %in% gang_list()) { showNotification("That gang name already exists - use Edit instead.", type = "error"); return() }
    ticked <- collect_ticked_items()
    ganger <- trimws(input$gang_form_ganger)
    df <- inventory_data()
    old_driver <- setNames(df$Driver, df$ItemID)
    df$Gang[df$ItemID %in% ticked] <- name
    # Driver mirrors the gang's Ganger - plant assigned to this gang
    # is being driven/run by whoever's the Ganger, so keep Driver in
    # sync automatically instead of having to set it twice.
    if (ganger != "") df$Driver[df$ItemID %in% ticked] <- ganger
    # Location mirrors the gang sheet's Location too - the plant
    # ticked into this gang is physically wherever the gang is
    # working, so Plant Inventory should show that without having to
    # be set separately.
    location <- trimws(input$gang_form_location)
    if (location != "") df$Location[df$ItemID %in% ticked] <- location
    inventory_data(df)
    gang_list(c(gang_list(), name))
    save_gang_meta(name, ganger, input$gang_form_location)
    if (ganger != "") {
      driver_changed <- ticked[vapply(ticked, function(id) !identical(trimws(old_driver[[id]]), ganger), logical(1))]
      log_driver_assigned_entries(driver_changed, ganger)
    }
    removeModal()
    showNotification(paste0("Gang sheet '", name, "' created with ", length(ticked), " item(s)."), type = "message")
    log_notification(paste0("Created gang sheet '", name, "' with ", length(ticked), " item(s)"))
  })
  observeEvent(input$gang_form_submit_edit, {
    name <- editing_gang(); req(name)
    ticked <- collect_ticked_items()
    ganger <- trimws(input$gang_form_ganger_edit)
    df <- inventory_data()
    old_driver <- setNames(df$Driver, df$ItemID)
    df$Gang[df$Gang == name & !(df$ItemID %in% ticked)] <- ""
    df$Gang[df$ItemID %in% ticked] <- name
    # Keep Driver in sync with the gang's Ganger for whatever's
    # currently ticked - covers both newly added items and the
    # Ganger being changed on items already in the gang.
    if (ganger != "") df$Driver[df$ItemID %in% ticked] <- ganger
    # Keep Location in sync with the gang sheet's Location for
    # whatever's currently ticked too - covers newly added items and
    # the gang's Location being changed/updated later.
    location_edit <- trimws(input$gang_form_location_edit)
    if (location_edit != "") df$Location[df$ItemID %in% ticked] <- location_edit
    inventory_data(df)
    save_gang_meta(name, ganger, input$gang_form_location_edit)
    if (ganger != "") {
      driver_changed <- ticked[vapply(ticked, function(id) !identical(trimws(old_driver[[id]]), ganger), logical(1))]
      log_driver_assigned_entries(driver_changed, ganger)
    }
    removeModal(); editing_gang(NULL)
    showNotification(paste0("Gang sheet '", name, "' updated."), type = "message")
    log_notification(paste0("Updated gang sheet '", name, "' (", length(ticked), " item(s) assigned)"))
  })
  # ---- Bulk "Edit All Gang Assignments" ----
  # Rebuild every gang sheet at once: every gang gets its own blank
  # Ganger/Location + plant picker (same picker as Create/Edit, just
  # one per gang, namespaced by input ID so they don't collide), and
  # nothing starts pre-ticked. Only items you actually tick under a
  # gang get moved there - anything you leave unticked everywhere
  # keeps its current assignment untouched, so a missed item can't
  # get silently unassigned. Nothing is written to Inventory until
  # Save/Sync.
  bulk_gang_form_for <- function(gid, pool_df) {
    lapply(CATEGORY_OPTIONS, function(cat) {
      cat_rows <- pool_df[pool_df$Category == cat, ]
      subcats <- subcats_for(cat, pool_df)
      sub_panels <- lapply(subcats, function(sub) {
        sub_rows <- natural_sort_rows(cat_rows[cat_rows$SubCategory == sub, ])
        accordion_panel(
          title = paste0(sub, " (", nrow(sub_rows), ")"),
          value = paste0(cat, "___", sub),
          if (nrow(sub_rows) == 0) p(class = "text-muted mb-0", "None.")
          else checkboxGroupInput(paste0("bulk_", gid, "_", make.names(cat), "_", make.names(sub)), NULL,
                                  choices = setNames(sub_rows$ItemID, paste0(ifelse(sub_rows$Machine == "", "(no machine name)", sub_rows$Machine), " - ", vapply(seq_len(nrow(sub_rows)), function(i) item_identifier(sub_rows[i, ]), character(1)))),
                                  selected = character(0))
        )
      })
      accordion_panel(
        title = paste0(cat, " (", nrow(cat_rows), ")"),
        value = cat,
        if (length(sub_panels) == 0) p(class = "text-muted mb-0", "None.")
        else do.call(accordion, c(list(id = paste0("bulk_sub_", gid, "_", make.names(cat))), sub_panels))
      )
    })
  }
  collect_bulk_ticked_items <- function(gid) {
    df <- inventory_data()
    pairs <- unique(df[, c("Category", "SubCategory")])
    unlist(lapply(seq_len(nrow(pairs)), function(i) {
      cat <- pairs$Category[i]; sub <- pairs$SubCategory[i]
      input[[paste0("bulk_", gid, "_", make.names(cat), "_", make.names(sub))]]
    }))
  }
  observeEvent(input$bulk_edit_gangs_btn, {
    gl <- gang_list()
    req(length(gl) > 0)
    df <- inventory_data()
    removeModal()
    showModal(modalDialog(
      title = "Edit All Gang Assignments", size = "l", easyClose = FALSE,
      div(class = "alert alert-warning",
          "Every gang below starts blank - nothing is ticked and no Ganger/Location is pre-filled, even though the real assignments still exist until you save. Tick each gang's plant and re-enter its Ganger/Location, then hit Save/Sync at the bottom. Anything you leave unticked under every gang keeps its current assignment - nothing changes for it."),
      tagList(lapply(gl, function(g) {
        gid <- make.names(g)
        tagList(
          h5(g, style = "margin-top:18px;"),
          fluidRow(
            column(6, selectInput(paste0("bulk_ganger_", gid), "Ganger (optional)", choices = ganger_choices(), selected = "")),
            column(6, textInput(paste0("bulk_location_", gid), "Location (optional)", value = "", placeholder = "e.g. Site name / postcode"))
          ),
          do.call(accordion, c(list(id = paste0("bulk_accordion_", gid), open = FALSE), bulk_gang_form_for(gid, df)))
        )
      })),
      footer = tagList(modalButton("Cancel"), actionButton("bulk_edit_save", "Save / Sync", class = "btn-primary"))
    ))
  })
  observeEvent(input$bulk_edit_save, {
    gl <- gang_list()
    df <- inventory_data()
    old_gang <- setNames(df$Gang, df$ItemID)
    assignments <- list()
    all_ticked <- character(0)
    conflicts <- character(0)
    for (g in gl) {
      gid <- make.names(g)
      ticked <- collect_bulk_ticked_items(gid)
      assignments[[g]] <- ticked
      conflicts <- union(conflicts, intersect(all_ticked, ticked))
      all_ticked <- union(all_ticked, ticked)
    }
    if (length(conflicts) > 0) {
      labels <- vapply(conflicts, function(id) {
        row <- df[df$ItemID == id, ]
        if (nrow(row) > 0) item_identifier(row[1, ]) else id
      }, character(1))
      showNotification(paste0("These item(s) are ticked under more than one gang - untick the duplicate(s) before saving: ", paste(labels, collapse = ", ")),
                        type = "error", duration = 12)
      return()
    }
    gang_gangers <- setNames(character(0), character(0))
    for (g in gl) {
      gid <- make.names(g)
      ticked <- assignments[[g]]
      ganger <- trimws(input[[paste0("bulk_ganger_", gid)]])
      location <- trimws(input[[paste0("bulk_location_", gid)]])
      gang_gangers[g] <- ganger
      df$Gang[df$ItemID %in% ticked] <- g
      if (ganger != "") df$Driver[df$ItemID %in% ticked] <- ganger
      if (location != "") df$Location[df$ItemID %in% ticked] <- location
      save_gang_meta(g, ganger, location)
    }
    # Anything not ticked under any gang is left exactly as it was
    # (df already holds its current Gang) - a missed item keeps its
    # existing assignment instead of silently losing it.
    # Log a history entry only for items whose gang actually changed -
    # nothing for plant that stayed put.
    changed_ids <- df$ItemID[vapply(df$ItemID, function(id) !identical(old_gang[[id]], df$Gang[df$ItemID == id][1]), logical(1))]
    if (length(changed_ids) > 0) {
      # next_entry_id() reads plant_history() live - calling it once
      # per new row here would hand out the SAME id to every row since
      # plant_history() isn't updated until after this loop. Reserve a
      # block of ids up front and increment locally instead.
      existing_nums <- suppressWarnings(as.integer(gsub("HIST-", "", plant_history()$EntryID)))
      existing_nums <- existing_nums[!is.na(existing_nums)]
      start_n <- if (length(existing_nums) == 0) 1 else max(existing_nums) + 1
      new_entries <- lapply(seq_along(changed_ids), function(idx) {
        id <- changed_ids[idx]
        entry_id <- sprintf("HIST-%04d", start_n + idx - 1)
        new_g <- df$Gang[df$ItemID == id][1]
        if (new_g != "") {
          ganger <- gang_gangers[[new_g]]
          if (!is.null(ganger) && ganger != "") {
            data.frame(ItemID = id, DateTime = format(Sys.time(), "%Y-%m-%d %H:%M"),
                       EntryType = "Driver Assigned", Description = ganger, RecordedBy = user_name(),
                       InvoiceID = NA_character_, EntryID = entry_id, LinkedEntryID = NA_character_,
                       stringsAsFactors = FALSE)
          } else {
            data.frame(ItemID = id, DateTime = format(Sys.time(), "%Y-%m-%d %H:%M"),
                       EntryType = "Note", Description = paste0("Moved to gang ", new_g), RecordedBy = user_name(),
                       InvoiceID = NA_character_, EntryID = entry_id, LinkedEntryID = NA_character_,
                       stringsAsFactors = FALSE)
          }
        } else {
          data.frame(ItemID = id, DateTime = format(Sys.time(), "%Y-%m-%d %H:%M"),
                     EntryType = "Note", Description = "Removed from gang assignment", RecordedBy = user_name(),
                     InvoiceID = NA_character_, EntryID = entry_id, LinkedEntryID = NA_character_,
                     stringsAsFactors = FALSE)
        }
      })
      plant_history(bind_rows(plant_history(), do.call(rbind, new_entries)))
    }
    inventory_data(df)
    removeModal()
    showNotification(paste0("All gang assignments saved and synced - ", length(changed_ids), " item(s) moved."), type = "message")
    log_notification(paste0("Bulk-edited all gang assignments (", length(changed_ids), " item(s) moved)"))
  })
  observeEvent(input$delete_gang_click, {
    g <- input$delete_gang_click
    session$userData$pending_delete_gang <- g
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = "Delete this gang sheet?",
      paste0("'", g, "' will be deleted. Any plant assigned to it becomes unassigned (not deleted)."),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_delete_gang", "Yes, delete", class = "btn-danger"))
    ))
  })
  observeEvent(input$confirm_delete_gang, {
    g <- session$userData$pending_delete_gang
    gang_list(setdiff(gang_list(), g))
    df <- inventory_data(); df$Gang[df$Gang == g] <- ""; inventory_data(df)
    gm <- gang_meta(); gang_meta(gm[gm$Gang != g, ])
    removeModal()
    showNotification(paste0("'", g, "' deleted. Its plant is now unassigned."), type = "message")
  })
  # -------------------------------------------------------------
  # INVOICES
  # -------------------------------------------------------------
  output$invoices_tab_content <- renderUI({ invoices_ui(role()) })
  invoices_ui <- function(r) {
    all_dates <- suppressWarnings(as.Date(invoices_data()$Date))
    all_dates <- all_dates[!is.na(all_dates)]
    date_min <- if (length(all_dates) > 0) min(all_dates) else Sys.Date() - 365
    date_max <- if (length(all_dates) > 0) max(all_dates) else Sys.Date()
    # Named so the dropdown can show "PMK 107 - Excavator > Small
    # Excavator" (category/sub-category greyed out via the render JS
    # below) while the filter itself still matches on just the
    # reference number.
    inv_items_df <- unique(invoices_data()[invoices_data()$Reference_PMK_Number != "",
                                            c("Reference_PMK_Number", "Category", "SubCategory")])
    inv_items_df <- inv_items_df[order(inv_items_df$Reference_PMK_Number), ]
    item_choices <- setNames(
      inv_items_df$Reference_PMK_Number,
      paste0(inv_items_df$Reference_PMK_Number, " - ", inv_items_df$Category, " > ", inv_items_df$SubCategory)
    )
    item_render_js <- I("{
      option: function(item, escape) {
        var parts = item.label.split(' - ');
        var main = escape(parts.shift());
        var rest = parts.join(' - ');
        var sub = rest ? '<span style=\"color:#9a9a9a;font-size:0.85em;\"> - ' + escape(rest) + '</span>' : '';
        return '<div>' + main + sub + '</div>';
      },
      item: function(item, escape) {
        var parts = item.label.split(' - ');
        var main = escape(parts.shift());
        var rest = parts.join(' - ');
        var sub = rest ? '<span style=\"color:#9a9a9a;font-size:0.85em;\"> - ' + escape(rest) + '</span>' : '';
        return '<div>' + main + sub + '</div>';
      }
    }")
    tagList(
      br(),
      p(class = "text-muted", if (r %in% c("Admin", "Boss")) "Add and view invoices." else "View access - Kevin's role."),
      fluidRow(
        column(8, NULL),
        column(4, style = "text-align:right;",
               if (r %in% c("Admin", "Boss")) actionButton("add_invoice_btn", "+ Add Invoice", class = "btn-primary btn-sm"))
      ),
      div(class = "chart-card",
          p(class = "text-muted mb-2", "Filters below apply to Overview, Analysis, Companies and All Invoices."),
          fluidRow(
            column(4, dateRangeInput("inv_filter_dates", "Date range", start = date_min, end = date_max)),
            column(4, selectizeInput("inv_filter_company", "Company", choices = company_choices_all(), multiple = TRUE,
                                     options = list(placeholder = "All companies"))),
            column(4, selectizeInput("inv_filter_item", "Plant Item", choices = item_choices, multiple = TRUE,
                                     options = list(placeholder = "All items", render = item_render_js)))
          )
      ),
      tabsetPanel(
        type = "pills",
        tabPanel("Overview", overview_ui()),
        tabPanel("Analysis", invoice_analysis_ui()),
        tabPanel("Companies", companies_ui()),
        tabPanel("All Invoices", all_invoices_ui())
      )
    )
  }
  overview_ui <- function() {
    tagList(
      br(),
      fluidRow(
        column(4, metric_card(textOutput("total_spend_val", inline = TRUE), "Total Spend")),
        column(4, metric_card(textOutput("n_invoices_val", inline = TRUE), "Total Invoices")),
        column(4, metric_card(textOutput("avg_invoice_val", inline = TRUE), "Average Invoice"))
      ),
      br(),
      fluidRow(
        column(6, div(class = "chart-card", h6("Monthly Spending Trend"), plotlyOutput("monthly_trend_plot", height = 280))),
        column(6, div(class = "chart-card", h6("Top Companies by Spend"), plotlyOutput("top_companies_plot", height = 280)))
      )
    )
  }
  companies_ui <- function() {
    tagList(
      br(),
      div(class = "chart-card", h6("Spend by Supplier"), plotlyOutput("company_plot", height = 320)),
      div(class = "chart-card", h6("Supplier Summary"), tableOutput("company_table"))
    )
  }
  all_invoices_ui <- function() {
    tagList(br(), downloadButton("download_all_csv", "Download All (CSV)", class = "btn-primary btn-sm mb-3"),
            uiOutput("invoice_cards"))
  }
  tag_or_na <- function(label, value) {
    if (is.na(value) || value == "") span(class = "tag-chip na", paste0(label, ": -"))
    else span(class = "tag-chip", paste0(label, ": ", value))
  }
  invoice_card <- function(row, r) {
    div(class = "invoice-card",
        div(class = "d-flex justify-content-between align-items-start flex-wrap",
            div(span(class = "invoice-ref", row$Reference_PMK_Number), span(class = "text-muted", paste0(" - ", row$Date, " - ", row$Company))),
            div(class = "d-flex align-items-center",
                span(class = paste0("badge ", ifelse(row$Amount < 0, "bg-warning", "bg-success")),
                     paste0("£", formatC(row$Amount, format = "f", digits = 2))),
                if (r %in% c("Admin", "Boss")) tags$a(href = "#", style = "font-size:0.85rem; margin-left:12px;",
                                         onclick = sprintf("Shiny.setInputValue('edit_invoice_click', '%s', {priority:'event'}); return false;", row$InvoiceID),
                                         "Edit"),
                if (r %in% c("Admin", "Boss")) tags$a(href = "#", style = "font-size:0.85rem; color:#9C2B2B; margin-left:10px;",
                                         onclick = sprintf("Shiny.setInputValue('delete_invoice_click', '%s', {priority:'event'}); return false;", row$InvoiceID),
                                         "Delete")
            )
        ),
        if (!is.na(row$Description) && row$Description != "") p(class = "mb-1 mt-2", em(row$Description)),
        div(
          tag_or_na("Category", row$Category), tag_or_na("Sub-Category", row$SubCategory),
          tag_or_na("Invoice No.", row$Invoice_Number),
          # Shown only where it exists (invoices logged before the field
          # was retired) rather than a "-" chip on every new one.
          if (!is.na(row$Account_Number) && row$Account_Number != "") tag_or_na("Account No.", row$Account_Number),
          tag_or_na("Document No.", row$Document_Number), tag_or_na("SPEN/Order No.", row$SPEN_Order_Number),
          tag_or_na("Logged By", row$LoggedBy)
        )
    )
  }
  output$invoice_cards <- renderUI({
    df <- inv_filtered()
    if (nrow(df) == 0) return(div(class = "alert alert-secondary", "No invoices logged yet, or none match the current filters."))
    df <- df %>% arrange(desc(as.Date(Date)))
    r <- role()
    tagList(lapply(seq_len(nrow(df)), function(i) invoice_card(df[i, ], r)))
  })
  # ---- Add/Edit Invoice form ----
  # Shared by both Add and Edit - editing_invoice() holds the
  # InvoiceID being edited (NULL means this is a brand new invoice).
  invoice_form_ui <- function(prefill = NULL) {
    g <- function(field, default = "") if (is.null(prefill) || is.null(prefill[[field]]) || is.na(prefill[[field]])) default else as.character(prefill[[field]])
    init_cat <- g("Category", CATEGORY_OPTIONS[1])
    init_subs <- subcats_for(init_cat, inventory_data())
    init_sub <- g("SubCategory", if (length(init_subs) > 0) init_subs[1] else "")
    init_items <- if (init_sub != "") items_for_picker(init_cat, init_sub, inventory_data()) else character(0)
    init_ref <- g("Reference_PMK_Number")
    if (init_ref != "" && !(init_ref %in% init_items)) init_items <- c(init_items, init_ref)
    company_choices <- company_choices_all()
    init_company <- g("Company")
    if (init_company != "" && !(init_company %in% company_choices)) company_choices <- c(company_choices, init_company)
    init_amount <- if (is.null(prefill)) NA else suppressWarnings(as.numeric(prefill[["Amount"]]))
    init_date <- if (g("Date") != "") as.Date(g("Date")) else Sys.Date()
    tagList(
      selectizeInput("ni_company", "Company *", choices = company_choices, selected = init_company,
                     options = list(create = TRUE, placeholder = "Select or type a company name")),
      fluidRow(
        column(6, dateInput("ni_date", "Date *", value = init_date)),
        column(6, numericInput("ni_amount", "Amount (£) *", value = init_amount))
      ),
      p(class = "text-muted mb-1", "Which item is this invoice for? Matches it up with Inventory."),
      fluidRow(
        column(6, selectInput("ni_category", "Category *", choices = CATEGORY_OPTIONS, selected = init_cat)),
        column(6, selectizeInput("ni_subcategory", "Sub-Category *", choices = init_subs, selected = init_sub))
      ),
      selectizeInput("ni_reference", "Item (PMK Number/Registration/Serial Number) *", choices = init_items, selected = init_ref,
                     options = list(create = TRUE, placeholder = "Pick the item, or type a reference if it isn't in the system yet")),
      hr(),
      p(class = "text-muted", "Everything below is optional."),
      # Account Number is deliberately NOT captured here. It's a property
      # of the supplier, not of each invoice - the same number repeated on
      # every row from that company - so recording it per invoice stored
      # the one semi-sensitive field on this form hundreds of times over
      # for no extra information. The column is kept (see invoices_seed)
      # so everything already recorded is preserved and still exports;
      # it's just no longer collected or added to.
      fluidRow(
        column(6, textInput("ni_invoice_number", "Invoice Number", value = g("Invoice_Number"))),
        column(6, textInput("ni_document_number", "Document Number", value = g("Document_Number")))
      ),
      fluidRow(
        column(6, textInput("ni_spen", "SPEN/Order Number", value = g("SPEN_Order_Number")))
      ),
      textAreaInput("ni_description", "Description", value = g("Description"), rows = 3)
    )
  }
  observeEvent(input$add_invoice_btn, {
    editing_invoice(NULL)
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = "Add Invoice", size = "l",
      invoice_form_ui(),
      footer = tagList(modalButton("Cancel"), actionButton("ni_submit", "Save Invoice", class = "btn-primary"))
    ))
  })
  observeEvent(input$edit_invoice_click, {
    iid <- input$edit_invoice_click
    df <- invoices_data()
    row <- df[df$InvoiceID == iid, ]
    req(nrow(row) == 1)
    editing_invoice(iid)
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = paste("Edit Invoice -", row$Reference_PMK_Number[1]), size = "l",
      invoice_form_ui(prefill = as.list(row[1, ])),
      footer = tagList(modalButton("Cancel"), actionButton("ni_submit", "Save Changes", class = "btn-primary"))
    ))
  })
  observeEvent(input$ni_category, {
    subs <- subcats_for(input$ni_category, inventory_data())
    updateSelectizeInput(session, "ni_subcategory", choices = subs,
                         selected = if (length(subs) > 0) subs[1] else character(0))
  }, ignoreInit = TRUE)
  observeEvent(input$ni_subcategory, {
    req(input$ni_category)
    items <- items_for_picker(input$ni_category, input$ni_subcategory, inventory_data())
    updateSelectizeInput(session, "ni_reference", choices = items)
  }, ignoreInit = TRUE)
  observeEvent(input$ni_submit, {
    company <- trimws(input$ni_company)
    reference <- trimws(input$ni_reference)
    if (is.null(company) || company == "") { showNotification("Company is required.", type = "error"); return() }
    if (is.na(input$ni_amount)) { showNotification("Amount is required.", type = "error"); return() }
    if (is.null(input$ni_category) || input$ni_category == "") { showNotification("Category is required.", type = "error"); return() }
    if (is.null(input$ni_subcategory) || input$ni_subcategory == "") { showNotification("Sub-Category is required.", type = "error"); return() }
    if (reference == "") { showNotification("Item (PMK Number/Registration/Serial Number) is required.", type = "error"); return() }
    matched_iid <- find_item_id(input$ni_category, input$ni_subcategory, reference, inventory_data())
    message(sprintf("[Invoice->History match] cat='%s' subcat='%s' ref='%s' -> matched_iid=%s",
                    input$ni_category, input$ni_subcategory, reference,
                    ifelse(is.na(matched_iid), "NA (no match)", matched_iid)))
    existing_id <- editing_invoice()
    this_id <- if (is.null(existing_id)) next_invoice_id() else existing_id
    # Keep the original "who logged it" on edits (the person saving a
    # correction isn't necessarily who originally entered it) - only
    # brand new invoices get today's logged-in user.
    df <- invoices_data()
    prior_row <- if (!is.null(existing_id)) df[df$InvoiceID == existing_id, ] else df[0, ]
    logged_by <- if (nrow(prior_row) > 0 && !is.na(prior_row$LoggedBy[1]) && prior_row$LoggedBy[1] != "") prior_row$LoggedBy[1] else user_name()
    new_invoice <- data.frame(
      InvoiceID = this_id,
      Company = company,
      Invoice_Number = input$ni_invoice_number,
      # Carried through untouched from the existing row (blank for new
      # invoices) - the form no longer offers it, but editing an old
      # invoice must not quietly wipe what's already there.
      Account_Number = if (nrow(prior_row) > 0 && !is.na(prior_row$Account_Number[1])) prior_row$Account_Number[1] else "",
      Document_Number = input$ni_document_number,
      Date = as.character(input$ni_date),
      Amount = input$ni_amount,
      Description = input$ni_description,
      SPEN_Order_Number = input$ni_spen,
      Category = input$ni_category,
      SubCategory = input$ni_subcategory,
      Reference_PMK_Number = reference,
      LoggedBy = logged_by,
      stringsAsFactors = FALSE
    )
    if (!is.null(existing_id)) df <- df[df$InvoiceID != existing_id, ]
    invoices_data(bind_rows(df, new_invoice))
    # Also log this as a Plant History entry against the matched item,
    # so an invoice shows up in that item's history without having to
    # add it twice. On edit, drop the old linked entry first (it's
    # tagged with this same InvoiceID) so editing doesn't leave a
    # stale duplicate behind.
    ph <- plant_history()
    # Preserve the old entry's EntryID/LinkedEntryID across an edit, so
    # a link someone made to/from this entry doesn't silently break.
    old_link_row <- if (!is.null(existing_id)) ph[!is.na(ph$InvoiceID) & ph$InvoiceID == existing_id, ] else ph[0, ]
    kept_entry_id <- if (nrow(old_link_row) > 0) old_link_row$EntryID[1] else next_entry_id()
    kept_linked_id <- if (nrow(old_link_row) > 0) old_link_row$LinkedEntryID[1] else NA_character_
    if (!is.null(existing_id)) ph <- ph[is.na(ph$InvoiceID) | ph$InvoiceID != existing_id, ]
    if (!is.na(matched_iid)) {
      # Mirrors every field captured on the invoice itself, so the
      # History entry and the Invoice record tell the same story.
      inv_desc_lines <- c(
        paste0("Company: ", company),
        paste0("Amount: £", sprintf("%.2f", input$ni_amount)),
        paste0("Date: ", as.character(input$ni_date)),
        paste0("Item: ", input$ni_category, " > ", input$ni_subcategory, " > ", reference)
      )
      if (!is.null(input$ni_invoice_number) && trimws(input$ni_invoice_number) != "") inv_desc_lines <- c(inv_desc_lines, paste0("Invoice Number: ", trimws(input$ni_invoice_number)))
      if (!is.null(input$ni_document_number) && trimws(input$ni_document_number) != "") inv_desc_lines <- c(inv_desc_lines, paste0("Document Number: ", trimws(input$ni_document_number)))
      if (!is.null(input$ni_spen) && trimws(input$ni_spen) != "") inv_desc_lines <- c(inv_desc_lines, paste0("SPEN/Order Number: ", trimws(input$ni_spen)))
      if (!is.null(input$ni_description) && trimws(input$ni_description) != "") inv_desc_lines <- c(inv_desc_lines, paste0("Description: ", trimws(input$ni_description)))
      inv_history_entry <- data.frame(
        ItemID = matched_iid,
        DateTime = paste(as.character(input$ni_date), format(Sys.time(), "%H:%M")),
        EntryType = "Invoice", Description = paste(inv_desc_lines, collapse = "\n"), RecordedBy = user_name(),
        InvoiceID = this_id, EntryID = kept_entry_id, LinkedEntryID = kept_linked_id,
        stringsAsFactors = FALSE
      )
      ph <- bind_rows(ph, inv_history_entry)
    }
    plant_history(ph)
    editing_invoice(NULL)
    removeModal()
    if (is.na(matched_iid)) {
      showNotification(paste0("Invoice saved, but no inventory item matched '", reference, "' - it won't show in that item's History. Check the reference matches a PMK Number/Registration/Serial Number."), type = "warning", duration = 10)
    } else {
      showNotification(if (is.null(existing_id)) "Invoice saved and logged to that item's History." else "Invoice updated.", type = "message")
    }
  })
  observeEvent(input$delete_invoice_click, {
    session$userData$pending_delete_invoice <- input$delete_invoice_click
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = "Delete this invoice?",
      "This invoice will be permanently removed, along with its linked Plant History entry. This cannot be undone.",
      footer = tagList(modalButton("Cancel"), actionButton("confirm_delete_invoice", "Yes, delete", class = "btn-danger"))
    ))
  })
  observeEvent(input$confirm_delete_invoice, {
    iid <- session$userData$pending_delete_invoice
    df <- invoices_data(); df <- df[df$InvoiceID != iid, ]; invoices_data(df)
    ph <- plant_history(); ph <- ph[is.na(ph$InvoiceID) | ph$InvoiceID != iid, ]; plant_history(ph)
    removeModal()
    showNotification("Invoice deleted.", type = "message")
  })
  gg_theme <- theme_minimal(base_family = "sans") + theme(text = element_text(color = "#12241C"), panel.grid.minor = element_blank())
  output$total_spend_val <- renderText({ dollar(sum(inv_filtered()$Amount, na.rm = TRUE), prefix = "£") })
  output$n_invoices_val <- renderText({ nrow(inv_filtered()) })
  output$avg_invoice_val <- renderText({
    d <- inv_filtered()
    if (nrow(d) == 0) "£0.00" else dollar(mean(d$Amount, na.rm = TRUE), prefix = "£")
  })
  output$monthly_trend_plot <- renderPlotly({
    d <- inv_filtered()
    if (nrow(d) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    monthly <- d %>% mutate(YearMonth = format(DateParsed, "%Y-%m")) %>%
      group_by(YearMonth) %>% summarise(Total = sum(Amount, na.rm = TRUE)) %>% arrange(YearMonth)
    p <- ggplot(monthly, aes(x = YearMonth, y = Total, group = 1, text = paste0("£", round(Total, 2)))) +
      geom_line(color = "#0B4D3A", linewidth = 1.1) + geom_point(color = "#C9A227", size = 3) +
      scale_y_continuous(labels = label_dollar(prefix = "£")) + labs(x = NULL, y = NULL) +
      gg_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })
  output$top_companies_plot <- renderPlotly({
    d <- inv_filtered()
    if (nrow(d) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    c_data <- d %>% group_by(Company) %>% summarise(Total = sum(Amount, na.rm = TRUE)) %>% arrange(desc(Total)) %>% head(10)
    p <- ggplot(c_data, aes(x = reorder(Company, Total), y = Total, text = paste0("£", round(Total, 2)))) +
      geom_col(fill = "#3E7C59") + coord_flip() + scale_y_continuous(labels = label_dollar(prefix = "£")) +
      labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  output$company_plot <- renderPlotly({
    d <- inv_filtered()
    if (nrow(d) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    c_data <- d %>% group_by(Company) %>% summarise(Total = sum(Amount, na.rm = TRUE)) %>% arrange(desc(Total))
    p <- ggplot(c_data, aes(x = reorder(Company, Total), y = Total, text = paste0("£", round(Total, 2)))) +
      geom_col(fill = "#5B6770") + coord_flip() + scale_y_continuous(labels = label_dollar(prefix = "£")) +
      labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  output$company_table <- renderTable({
    d <- inv_filtered()
    if (nrow(d) == 0) return(data.frame(Message = "No invoices logged yet, or none match the current filters."))
    d %>% group_by(Company) %>%
      summarise(`Total Spend (£)` = sprintf("%.2f", sum(Amount, na.rm = TRUE)),
                `Invoices` = n(), `Avg Invoice (£)` = sprintf("%.2f", mean(Amount, na.rm = TRUE))) %>%
      arrange(desc(`Total Spend (£)`))
  })
  output$download_all_csv <- downloadHandler(
    filename = function() paste0("pmk_invoices_", Sys.Date(), ".csv"),
    content = function(file) write.csv(inv_filtered(), file, row.names = FALSE)
  )
  # -------------------------------------------------------------
  # REPORTS - Weekly and Monthly snapshots pulled from Inventory
  # and Invoices. Admin/Kevin only, same access as Invoices.
  # -------------------------------------------------------------
  # Reports now lives inside the Admin tab (as its own accordion
  # panel) rather than as a standalone top-level tab - reports_ui()
  # is called directly from admin_tab_content further down.
  reports_ui <- function(r) {
    tagList(
      br(),
      p(class = "text-muted", "Snapshots pulled live from Inventory and Invoices - nothing to maintain separately."),
      tabsetPanel(
        type = "pills",
        tabPanel("Weekly Report", weekly_report_ui()),
        tabPanel("Monthly Report", monthly_report_ui())
      )
    )
  }
  invoice_analysis_ui <- function() {
    tagList(
      br(),
      p(class = "text-muted", "Ranks invoiced items by how often they're invoiced and by total £ spent. Filter by Category/Sub-Category, or leave both on 'All' for the whole fleet."),
      fluidRow(
        column(4, selectInput("ia_category", "Category", choices = c("All", CATEGORY_OPTIONS), selected = "All")),
        column(4, selectizeInput("ia_subcategory", "Sub-Category", choices = "All", selected = "All")),
        column(4, div(style = "margin-top:24px;", downloadButton("ia_download", "Download (CSV)", class = "btn-primary btn-sm")))
      ),
      fluidRow(
        column(6, div(class = "chart-card", h6("Most Frequently Invoiced"), plotlyOutput("ia_count_plot", height = 340))),
        column(6, div(class = "chart-card", h6("Most Expensive (Total £)"), plotlyOutput("ia_spend_plot", height = 340)))
      )
    )
  }
  # ---- Full PDF report (Plant & Drivers / History / Invoices) ----
  # Built with ONLY base R's pdf() device + the base `grid` package
  # (no gridExtra, no rmarkdown/pandoc/LaTeX) - `grid` ships with
  # every R installation, so this can never fail to install the way
  # gridExtra did on Posit Connect Cloud's fixed package set. Tables
  # are drawn by hand: a viewport per cell, positioned via grid.layout.
  # Renders one table. Columns named in wrap_cols get their text
  # wrapped onto multiple lines (via strwrap + embedded \n, which
  # grid.text renders natively) instead of being truncated with "...",
  # so long free-text fields like Description stay fully readable.
  # Row heights are "null" units sized to each row's line count, so
  # a 5-line description gets a tall row and a 1-line one stays thin.
  draw_grid_table <- function(df, col_widths = NULL, wrap_cols = character(0), wrap_chars = 50,
                              y = 0.5, height = 0.85, fontsize = 8, header_fontsize = 9) {
    nr <- nrow(df); nc <- ncol(df)
    if (is.null(col_widths)) col_widths <- rep(1, nc)
    cell_lines <- vector("list", max(nr, 0))
    row_weights <- numeric(max(nr, 0))
    if (nr > 0) {
      for (i in seq_len(nr)) {
        lines_per_col <- vector("list", nc)
        maxlines <- 1
        for (j in seq_len(nc)) {
          val <- as.character(df[i, j])
          if (is.na(val)) val <- "-"
          if (names(df)[j] %in% wrap_cols) {
            wrapped <- wrap_lines(val, wrap_chars)
            if (length(wrapped) == 0) wrapped <- "-"
          } else {
            wrapped <- if (nchar(val) > 40) paste0(substr(val, 1, 37), "...") else val
          }
          lines_per_col[[j]] <- wrapped
          maxlines <- max(maxlines, length(wrapped))
        }
        cell_lines[[i]] <- lines_per_col
        row_weights[i] <- maxlines
      }
    }
    pushViewport(viewport(y = unit(y, "npc"), height = unit(height, "npc"), width = unit(0.96, "npc")))
    heights <- unit(c(1.3, if (nr > 0) row_weights else numeric(0)), "null")
    widths <- unit(col_widths, "null")
    pushViewport(viewport(layout = grid.layout(nrow = nr + 1, ncol = nc, heights = heights, widths = widths)))
    col_names <- names(df)
    for (j in seq_len(nc)) {
      pushViewport(viewport(layout.pos.row = 1, layout.pos.col = j))
      grid.rect(gp = gpar(fill = "#0B4D3A", col = "white"))
      grid.text(col_names[j], x = unit(0.03, "npc"), hjust = 0, gp = gpar(col = "white", fontsize = header_fontsize, fontface = "bold"))
      popViewport()
    }
    if (nr > 0) {
      for (i in seq_len(nr)) {
        fill <- if (i %% 2 == 1) "#FFFFFF" else "#F4F2EC"
        for (j in seq_len(nc)) {
          pushViewport(viewport(layout.pos.row = i + 1, layout.pos.col = j))
          grid.rect(gp = gpar(fill = fill, col = "#E2DFD6"))
          cell_text <- paste(cell_lines[[i]][[j]], collapse = "\n")
          grid.text(cell_text, x = unit(0.03, "npc"), y = unit(0.9, "npc"), hjust = 0, vjust = 1,
                    gp = gpar(fontsize = fontsize, lineheight = 0.95))
          popViewport()
        }
      }
    }
    popViewport(2)
  }
  # Draws one PDF page: a title (plus optional subtitle) with a data
  # frame rendered as a table underneath.
  draw_report_page <- function(title, subtitle, df, col_widths = NULL, wrap_cols = character(0), wrap_chars = 50) {
    grid.newpage()
    grid.text(title, x = unit(0.02, "npc"), y = unit(0.97, "npc"), just = c("left", "top"),
              gp = gpar(fontsize = 15, fontface = "bold", col = "#0B4D3A"))
    if (!is.null(subtitle) && subtitle != "") {
      grid.text(subtitle, x = unit(0.02, "npc"), y = unit(0.925, "npc"), just = c("left", "top"),
                gp = gpar(fontsize = 9, col = "#5B6770"))
    }
    draw_grid_table(df, col_widths = col_widths, wrap_cols = wrap_cols, wrap_chars = wrap_chars, y = 0.44, height = 0.8)
  }
  # Splits a data frame across as many PDF pages as it needs so wide
  # sections (155 plant items, a month of invoices) don't get cut off
  # or squeezed onto one unreadable page. Paginates on a "weight"
  # budget (1 per line, so a wrapped 4-line description counts as 4)
  # rather than a flat row count, since wrapped rows vary in height.
  draw_report_section <- function(title, subtitle, df, col_widths = NULL, wrap_cols = character(0),
                                  wrap_chars = 50, max_weight = 24) {
    if (is.null(df) || nrow(df) == 0) {
      draw_report_page(title, subtitle, data.frame(Message = "No records for this period."))
      return(invisible())
    }
    n <- nrow(df)
    weights <- vapply(seq_len(n), function(i) {
      m <- 1
      for (cn in wrap_cols) {
        v <- as.character(df[[cn]][i])
        if (!is.na(v)) m <- max(m, length(wrap_lines(v, wrap_chars)))
      }
      m
    }, numeric(1))
    pages <- list(); cur <- integer(0); cur_w <- 0
    for (i in seq_len(n)) {
      w <- weights[i]
      if (cur_w + w > max_weight && length(cur) > 0) {
        pages[[length(pages) + 1]] <- cur
        cur <- integer(0); cur_w <- 0
      }
      cur <- c(cur, i); cur_w <- cur_w + w
    }
    if (length(cur) > 0) pages[[length(pages) + 1]] <- cur
    np <- length(pages)
    for (p in seq_len(np)) {
      idx <- pages[[p]]
      page_title <- if (np > 1) paste0(title, "  (page ", p, " of ", np, ")") else title
      draw_report_page(page_title, subtitle, df[idx, , drop = FALSE], col_widths, wrap_cols, wrap_chars)
    }
  }
  # Category display order for the report's grouped sections - not
  # the same as CATEGORY_OPTIONS (which is alphabetical-ish for the
  # picker dropdowns); this is the order requested for reports
  # specifically, with anything unrecognised tacked on the end.
  REPORT_CATEGORY_ORDER <- c("Excavator", "Trailer", "Breaker", "Misc", "Vehicle")
  # Renders one page of a grouped table: a normal column header row,
  # then a mix of full-width category/sub-category band rows and
  # normal data rows, all sized via the same "null" unit row-height
  # trick as draw_grid_table so wrapped rows still grow correctly.
  draw_grouped_table_page <- function(title, subtitle, page_items, col_names, col_widths,
                                      wrap_cols, wrap_chars, fontsize = 8, header_fontsize = 9) {
    grid.newpage()
    grid.text(title, x = unit(0.02, "npc"), y = unit(0.97, "npc"), just = c("left", "top"),
              gp = gpar(fontsize = 15, fontface = "bold", col = "#0B4D3A"))
    if (!is.null(subtitle) && subtitle != "") {
      grid.text(subtitle, x = unit(0.02, "npc"), y = unit(0.925, "npc"), just = c("left", "top"),
                gp = gpar(fontsize = 9, col = "#5B6770"))
    }
    nc <- length(col_names)
    n_items <- length(page_items)
    if (is.null(col_widths)) col_widths <- rep(1, nc)
    weights <- vapply(page_items, function(it) it$weight, numeric(1))
    pushViewport(viewport(y = unit(0.44, "npc"), height = unit(0.8, "npc"), width = unit(0.96, "npc")))
    heights <- unit(c(1.3, weights), "null")
    widths <- unit(col_widths, "null")
    pushViewport(viewport(layout = grid.layout(nrow = n_items + 1, ncol = nc, heights = heights, widths = widths)))
    for (j in seq_len(nc)) {
      pushViewport(viewport(layout.pos.row = 1, layout.pos.col = j))
      grid.rect(gp = gpar(fill = "#0B4D3A", col = "white"))
      grid.text(col_names[j], x = unit(0.03, "npc"), hjust = 0, gp = gpar(col = "white", fontsize = header_fontsize, fontface = "bold"))
      popViewport()
    }
    data_row_idx <- 0
    for (i in seq_len(n_items)) {
      it <- page_items[[i]]
      r <- i + 1
      if (it$type == "cat") {
        pushViewport(viewport(layout.pos.row = r, layout.pos.col = 1:nc))
        grid.rect(gp = gpar(fill = it$colour, col = NA))
        grid.text(it$label, x = unit(0.012, "npc"), hjust = 0, gp = gpar(col = "white", fontsize = 11, fontface = "bold"))
        popViewport()
      } else if (it$type == "sub") {
        pushViewport(viewport(layout.pos.row = r, layout.pos.col = 1:nc))
        grid.rect(gp = gpar(fill = "#EDEAE1", col = NA))
        grid.text(it$label, x = unit(0.025, "npc"), hjust = 0, gp = gpar(col = "#3A3A3A", fontsize = 9, fontface = "bold"))
        popViewport()
      } else {
        data_row_idx <- data_row_idx + 1
        fill <- if (data_row_idx %% 2 == 1) "#FFFFFF" else "#F4F2EC"
        row_df <- it$row
        for (j in seq_len(nc)) {
          pushViewport(viewport(layout.pos.row = r, layout.pos.col = j))
          grid.rect(gp = gpar(fill = fill, col = "#E2DFD6"))
          val <- as.character(row_df[[j]])
          if (is.na(val)) val <- "-"
          if (col_names[j] %in% wrap_cols) {
            wrapped <- strwrap(val, width = wrap_chars)
            if (length(wrapped) == 0) wrapped <- "-"
            cell_text <- paste(wrapped, collapse = "\n")
          } else {
            cell_text <- if (nchar(val) > 40) paste0(substr(val, 1, 37), "...") else val
          }
          grid.text(cell_text, x = unit(0.03, "npc"), y = unit(0.9, "npc"), hjust = 0, vjust = 1,
                    gp = gpar(fontsize = fontsize, lineheight = 0.95))
          popViewport()
        }
      }
    }
    popViewport(2)
  }
  # Groups a table by Category then Sub-Category (df must have those
  # two exact columns, even if they're not in display_cols) with
  # coloured band rows between groups instead of one long flat table -
  # "excavators, then trailers, breakers etc" rather than everything
  # interleaved by date. Category/Sub-Category columns are dropped
  # from the row display itself since the band above each group
  # already says what they are - no need to repeat it on every row.
  # Paginates on the same weight budget as draw_report_section, with
  # a header row never left as the last thing on a page.
  draw_grouped_report_section <- function(title, subtitle, df, display_cols, col_widths = NULL,
                                          wrap_cols = character(0), wrap_chars = 50, max_weight = 20,
                                          category_order = REPORT_CATEGORY_ORDER) {
    if (is.null(df) || nrow(df) == 0) {
      draw_report_page(title, subtitle, data.frame(Message = "No records for this period."))
      return(invisible())
    }
    cats_present <- unique(df$Category)
    cats_ordered <- c(intersect(category_order, cats_present), setdiff(sort(cats_present), category_order))
    plan <- list()
    for (cat in cats_ordered) {
      cat_rows <- df[df$Category == cat, ]
      subs_present <- unique(cat_rows$`Sub-Category`)
      subs_ordered <- if (!is.null(SUBCATEGORY_MAP[[cat]])) {
        c(intersect(SUBCATEGORY_MAP[[cat]], subs_present), setdiff(subs_present, SUBCATEGORY_MAP[[cat]]))
      } else sort(subs_present)
      plan[[length(plan) + 1]] <- list(type = "cat", label = toupper(cat), colour = CATEGORY_COLOUR(cat), weight = 1.4)
      for (sub in subs_ordered) {
        sub_rows <- cat_rows[cat_rows$`Sub-Category` == sub, , drop = FALSE]
        if (nrow(sub_rows) == 0) next
        plan[[length(plan) + 1]] <- list(type = "sub", label = paste0(sub, " (", nrow(sub_rows), ")"), weight = 1.2)
        for (i in seq_len(nrow(sub_rows))) {
          w <- 1
          for (cn in wrap_cols) {
            v <- as.character(sub_rows[[cn]][i])
            if (!is.na(v)) w <- max(w, length(strwrap(v, width = wrap_chars)))
          }
          plan[[length(plan) + 1]] <- list(type = "data", row = sub_rows[i, display_cols, drop = FALSE], weight = w)
        }
      }
    }
    pages <- list(); cur <- list(); cur_w <- 0
    for (idx in seq_along(plan)) {
      item <- plan[[idx]]
      if (cur_w + item$weight > max_weight && length(cur) > 0) {
        pages[[length(pages) + 1]] <- cur
        cur <- list(); cur_w <- 0
      }
      cur[[length(cur) + 1]] <- item
      cur_w <- cur_w + item$weight
      if (item$type != "data" && cur_w >= max_weight * 0.85) {
        pages[[length(pages) + 1]] <- cur
        cur <- list(); cur_w <- 0
      }
    }
    if (length(cur) > 0) pages[[length(pages) + 1]] <- cur
    np <- length(pages)
    for (p in seq_len(np)) {
      page_title <- if (np > 1) paste0(title, "  (page ", p, " of ", np, ")") else title
      draw_grouped_table_page(page_title, subtitle, pages[[p]], display_cols, col_widths, wrap_cols, wrap_chars)
    }
  }
  # Draws one or two ggplot objects side by side as a static image on
  # their own PDF page - ggplot objects are grid-compatible grobs, so
  # this reuses the exact same charts as the interactive tabs without
  # needing plotly.
  draw_chart_page <- function(title, subtitle, plots, labels) {
    grid.newpage()
    grid.text(title, x = unit(0.02, "npc"), y = unit(0.97, "npc"), just = c("left", "top"),
              gp = gpar(fontsize = 15, fontface = "bold", col = "#0B4D3A"))
    if (!is.null(subtitle) && subtitle != "") {
      grid.text(subtitle, x = unit(0.02, "npc"), y = unit(0.925, "npc"), just = c("left", "top"),
                gp = gpar(fontsize = 9, col = "#5B6770"))
    }
    n <- length(plots)
    pushViewport(viewport(y = unit(0.42, "npc"), height = unit(0.78, "npc"), width = unit(0.96, "npc")))
    pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = n)))
    for (i in seq_len(n)) {
      pushViewport(viewport(layout.pos.row = 1, layout.pos.col = i))
      grid.text(labels[i], y = unit(0.98, "npc"), gp = gpar(fontsize = 10, fontface = "bold", col = "#12241C"))
      pushViewport(viewport(y = unit(0.45, "npc"), height = unit(0.86, "npc")))
      grid.draw(ggplotGrob(plots[[i]]))
      popViewport()
      popViewport()
    }
    popViewport(2)
  }
  # ---- Job Cards & Inspections tile grid, reimplemented for the PDF ----
  # output$jg_grid in the app is built from raw HTML divs, which can't
  # be embedded in a grid-based PDF, so this redraws the same
  # week-by-item colour grid using grid.rect() instead.
  draw_tile_legend <- function(y = 0.07) {
    items <- list(c("Service Inspection", "#3E7C59"), c("Job Card", "#D9A400"),
                  c("Mechanic Work", "#D6598E"), c("Invoice", "#3A6EA5"),
                  c("Multiple", "#7A4F79"), c("Nothing logged", "#E2E2E2"))
    n <- length(items)
    for (i in seq_len(n)) {
      x0 <- (i - 1) / n + 0.01
      grid.rect(x = unit(x0, "npc"), y = unit(y, "npc"), width = unit(0.012, "npc"), height = unit(0.018, "npc"),
                just = "left", gp = gpar(fill = items[[i]][2], col = NA))
      grid.text(items[[i]][1], x = unit(x0 + 0.016, "npc"), y = unit(y, "npc"), just = "left",
                gp = gpar(fontsize = 7, col = "#5B6770"))
    }
  }
  # page_items is a mix of list(type="cat"/"sub", label=, colour=) band
  # markers and list(type="item", row=) plant rows, same "plan" shape
  # as the grouped table pages above - bands span the full width
  # (label column + every week column), item rows draw the label cell
  # plus one coloured square per week as before.
  draw_grouped_tile_grid_page <- function(title, subtitle, page_items, weeks, ev) {
    grid.newpage()
    grid.text(title, x = unit(0.02, "npc"), y = unit(0.97, "npc"), just = c("left", "top"),
              gp = gpar(fontsize = 15, fontface = "bold", col = "#0B4D3A"))
    if (!is.null(subtitle) && subtitle != "") {
      grid.text(subtitle, x = unit(0.02, "npc"), y = unit(0.925, "npc"), just = c("left", "top"),
                gp = gpar(fontsize = 9, col = "#5B6770"))
    }
    draw_tile_legend()
    n_rows <- length(page_items); n_weeks <- length(weeks)
    pushViewport(viewport(x = unit(0.02, "npc"), y = unit(0.87, "npc"), width = unit(0.96, "npc"),
                          height = unit(0.75, "npc"), just = c("left", "top")))
    pushViewport(viewport(layout = grid.layout(
      nrow = n_rows + 1, ncol = n_weeks + 1,
      widths = unit(c(1.7, rep(1, n_weeks)), c("inches", rep("null", n_weeks))),
      heights = unit(c(0.3, rep(1, n_rows)), c("inches", rep("null", n_rows)))
    )))
    for (w in seq_len(n_weeks)) {
      if (n_weeks <= 12 || w %% 4 == 1) {
        pushViewport(viewport(layout.pos.row = 1, layout.pos.col = w + 1))
        grid.text(format(weeks[w], "%d %b"), gp = gpar(fontsize = 6, col = "#8a8a8a"), rot = if (n_weeks > 12) 45 else 0)
        popViewport()
      }
    }
    for (i in seq_len(n_rows)) {
      it <- page_items[[i]]
      r <- i + 1
      if (it$type == "cat") {
        pushViewport(viewport(layout.pos.row = r, layout.pos.col = 1:(n_weeks + 1)))
        grid.rect(gp = gpar(fill = it$colour, col = NA))
        grid.text(it$label, x = unit(0.008, "npc"), hjust = 0, gp = gpar(col = "white", fontsize = 9, fontface = "bold"))
        popViewport()
      } else if (it$type == "sub") {
        pushViewport(viewport(layout.pos.row = r, layout.pos.col = 1:(n_weeks + 1)))
        grid.rect(gp = gpar(fill = "#EDEAE1", col = NA))
        grid.text(it$label, x = unit(0.018, "npc"), hjust = 0, gp = gpar(col = "#3A3A3A", fontsize = 7.5, fontface = "bold"))
        popViewport()
      } else {
        row <- it$row
        pushViewport(viewport(layout.pos.row = r, layout.pos.col = 1))
        grid.text(item_identifier(row), x = unit(0.02, "npc"), hjust = 0, gp = gpar(fontsize = 7, fontface = "bold"))
        popViewport()
        for (w in seq_len(n_weeks)) {
          wk <- weeks[w]
          matches <- if (nrow(ev) == 0) ev else ev[ev$ItemID == row$ItemID & ev$Week == wk, ]
          types_present <- if (nrow(matches) == 0) character(0) else intersect(names(JG_TYPE_COLOURS), unique(matches$EntryType))
          col <- if (length(types_present) == 0) "#E2E2E2"
                 else if (length(types_present) == 1) JG_TYPE_COLOURS[[types_present[1]]]
                 else "#7A4F79"
          pushViewport(viewport(layout.pos.row = r, layout.pos.col = w + 1))
          grid.rect(width = unit(0.78, "npc"), height = unit(0.78, "npc"), gp = gpar(fill = col, col = NA))
          popViewport()
        }
      }
    }
    popViewport(2)
  }
  # Groups the tile grid by Category then Sub-Category, same banding
  # approach as draw_grouped_report_section, and paginates by row
  # count (band rows count as rows too) so a header never ends up
  # alone at the bottom of a page.
  draw_tile_grid_section <- function(title, subtitle, items, weeks, ev, max_rows = 22,
                                     category_order = REPORT_CATEGORY_ORDER) {
    if (is.null(items) || nrow(items) == 0) {
      draw_report_page(title, subtitle, data.frame(Message = "No plant items match this view."))
      return(invisible())
    }
    cats_present <- unique(items$Category)
    cats_ordered <- c(intersect(category_order, cats_present), setdiff(sort(cats_present), category_order))
    plan <- list()
    for (cat in cats_ordered) {
      cat_rows <- items[items$Category == cat, ]
      subs_present <- unique(cat_rows$SubCategory)
      subs_ordered <- if (!is.null(SUBCATEGORY_MAP[[cat]])) {
        c(intersect(SUBCATEGORY_MAP[[cat]], subs_present), setdiff(subs_present, SUBCATEGORY_MAP[[cat]]))
      } else sort(subs_present)
      plan[[length(plan) + 1]] <- list(type = "cat", label = toupper(cat), colour = CATEGORY_COLOUR(cat))
      for (sub in subs_ordered) {
        sub_rows <- natural_sort_rows(cat_rows[cat_rows$SubCategory == sub, ])
        if (nrow(sub_rows) == 0) next
        plan[[length(plan) + 1]] <- list(type = "sub", label = paste0(sub, " (", nrow(sub_rows), ")"))
        for (i in seq_len(nrow(sub_rows))) {
          plan[[length(plan) + 1]] <- list(type = "item", row = sub_rows[i, , drop = FALSE])
        }
      }
    }
    pages <- list(); cur <- list()
    for (idx in seq_along(plan)) {
      it <- plan[[idx]]
      if (length(cur) >= max_rows) {
        pages[[length(pages) + 1]] <- cur
        cur <- list()
      }
      cur[[length(cur) + 1]] <- it
      if (it$type != "item" && length(cur) >= max_rows - 1) {
        pages[[length(pages) + 1]] <- cur
        cur <- list()
      }
    }
    if (length(cur) > 0) pages[[length(pages) + 1]] <- cur
    np <- length(pages)
    for (p in seq_len(np)) {
      page_title <- if (np > 1) paste0(title, "  (page ", p, " of ", np, ")") else title
      draw_grouped_tile_grid_page(page_title, subtitle, pages[[p]], weeks, ev)
    }
  }
  weeks_in_range <- function(start_date, end_date) {
    w0 <- floor_to_monday(start_date); w1 <- floor_to_monday(end_date)
    if (w1 < w0) w1 <- w0
    seq(w0, w1, by = "1 week")
  }
  weeks_last_52 <- function(end_date) {
    w1 <- floor_to_monday(end_date)
    rev(seq(w1, by = "-1 week", length.out = 52))
  }
  generate_full_report_pdf <- function(file, kind, period_label, period_start, period_end,
                                       inv_data, history_df, invoices_df) {
    pdf(file, width = 11.69, height = 8.27)  # A4 landscape - plenty of room for wide tables
    on.exit(dev.off(), add = TRUE)
    # Looks up an item's readable identifier + Category/Sub-Category
    # from its ItemID, for the History Entries table below.
    item_lookup <- function(item_id) {
      row <- inv_data[inv_data$ItemID == item_id, ]
      if (nrow(row) == 0) return(c(item_id, "-", "-"))
      lbl <- item_identifier(row[1, ])
      c(lbl, if (row$Category[1] == "") "-" else row$Category[1], if (row$SubCategory[1] == "") "-" else row$SubCategory[1])
    }
    # Most recent "Driver Assigned" entry date for an item, for the
    # Plant & Drivers "Driver Since" column - all-time lookup (not
    # period-scoped), matching that section's live-snapshot nature.
    ph_all <- plant_history()
    driver_since_lookup <- function(item_id) {
      h <- ph_all[ph_all$ItemID == item_id & ph_all$EntryType == "Driver Assigned", ]
      if (nrow(h) == 0) return("-")
      d <- as.Date(substr(h$DateTime, 1, 10))
      d <- d[!is.na(d)]
      if (length(d) == 0) return("-")
      as.character(max(d))
    }
    # ---- Cover page ----
    grid.newpage()
    grid.text("PMK CIVIL ENGINEERING", y = unit(0.78, "npc"), gp = gpar(fontsize = 26, fontface = "bold", col = "#0B4D3A"))
    grid.text(paste0(kind, " Report - ", period_label), y = unit(0.71, "npc"), gp = gpar(fontsize = 16, col = "#12241C"))
    grid.text(paste0(format(period_start, "%d %b %Y"), " to ", format(period_end, "%d %b %Y")),
              y = unit(0.66, "npc"), gp = gpar(fontsize = 11, col = "#5B6770"))
    grid.text(paste0("Generated ", format(Sys.time(), "%d %b %Y, %H:%M")), y = unit(0.61, "npc"), gp = gpar(fontsize = 9, col = "#999999"))
    metrics_df <- data.frame(
      Metric = c("Total Spend", "Service Inspections", "Job Cards", "Invoices"),
      Value = c(dollar(sum(invoices_df$Amount, na.rm = TRUE), prefix = "£"),
                as.character(sum(history_df$EntryType == "Service Inspection")),
                as.character(sum(history_df$EntryType == "Job Card")),
                as.character(nrow(invoices_df))),
      stringsAsFactors = FALSE
    )
    pushViewport(viewport(y = unit(0.4, "npc"), height = unit(0.22, "npc"), width = unit(0.4, "npc")))
    draw_grid_table(metrics_df, y = 0.5, height = 1)
    popViewport()
    # ---- Plant & Drivers (live snapshot, not period-scoped - who's
    # currently driving what doesn't have a month attached to it) ----
    plant_out <- natural_sort_rows(inv_data[inv_data$Active == "Yes", ])
    if (nrow(plant_out) > 0) {
      plant_out <- data.frame(
        Item = vapply(seq_len(nrow(plant_out)), function(i) item_identifier(plant_out[i, ]), character(1)),
        Machine = ifelse(plant_out$Machine == "", "-", plant_out$Machine),
        Category = plant_out$Category, `Sub-Category` = plant_out$SubCategory,
        Driver = ifelse(is.na(plant_out$Driver) | plant_out$Driver == "", "-", plant_out$Driver),
        `Driver Since` = ifelse(is.na(plant_out$Driver) | plant_out$Driver == "", "-",
                                vapply(plant_out$ItemID, driver_since_lookup, character(1))),
        Location = ifelse(is.na(plant_out$Location) | plant_out$Location == "", "-", plant_out$Location),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
    draw_grouped_report_section("Plant & Drivers", "Current snapshot as of report generation - not limited to this period.",
                                plant_out, display_cols = c("Item", "Machine", "Driver", "Driver Since", "Location"),
                                col_widths = c(1.1, 1.2, 1.3, 1, 1.2), max_weight = 22)
    # ---- History Entries logged in the period ----
    hist_out <- history_df
    if (nrow(hist_out) > 0) {
      hist_out <- hist_out[order(hist_out$DateTime, decreasing = TRUE), ]
      info <- t(vapply(hist_out$ItemID, item_lookup, character(3)))
      hist_out <- data.frame(
        `Date/Time` = hist_out$DateTime, Item = info[, 1], Type = hist_out$EntryType,
        Category = info[, 2], `Sub-Category` = info[, 3],
        Description = ifelse(is.na(hist_out$Description) | hist_out$Description == "", "-", hist_out$Description),
        `Recorded By` = hist_out$RecordedBy,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
    draw_grouped_report_section(paste0("History Entries - ", period_label), NULL, hist_out,
                                display_cols = c("Date/Time", "Item", "Type", "Description", "Recorded By"),
                                col_widths = c(1.3, 1.2, 1.1, 4, 1.2), wrap_cols = "Description", wrap_chars = 70, max_weight = 16)
    # ---- Invoices logged in the period ----
    inv_out <- invoices_df
    if (nrow(inv_out) > 0) {
      inv_out <- inv_out[order(inv_out$Date), ]
      inv_out <- data.frame(
        Date = inv_out$Date, Company = inv_out$Company, Item = inv_out$Reference_PMK_Number,
        Category = ifelse(inv_out$Category == "", "-", inv_out$Category),
        `Sub-Category` = ifelse(inv_out$SubCategory == "", "-", inv_out$SubCategory),
        Description = ifelse(is.na(inv_out$Description) | inv_out$Description == "", "-", inv_out$Description),
        `Amount (£)` = sprintf("%.2f", inv_out$Amount),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
    draw_grouped_report_section(paste0("Invoices - ", period_label), NULL, inv_out,
                                display_cols = c("Date", "Company", "Item", "Description", "Amount (£)"),
                                col_widths = c(1, 1.5, 1.2, 3.3, 1), wrap_cols = "Description", wrap_chars = 55, max_weight = 16)
    if (nrow(invoices_df) > 0) {
      grid.newpage()
      grid.text(paste0("Total Invoiced This Period: ", dollar(sum(invoices_df$Amount, na.rm = TRUE), prefix = "£")),
                gp = gpar(fontsize = 16, fontface = "bold", col = "#0B4D3A"))
    }
    # ---- Invoice Analysis, scoped to this period ----
    if (nrow(invoices_df) > 0) {
      ia_agg <- invoices_df %>% group_by(Reference_PMK_Number) %>%
        summarise(Count = n(), Total = sum(Amount, na.rm = TRUE), .groups = "drop") %>% arrange(desc(Total))
      ia_agg$Label <- ia_agg$Reference_PMK_Number
      p_count <- ggplot(head(ia_agg %>% arrange(desc(Count)), 10), aes(x = reorder(Label, Count), y = Count)) +
        geom_col(fill = "#0B4D3A") + coord_flip() + labs(x = NULL, y = NULL) + gg_theme
      p_spend <- ggplot(head(ia_agg, 10), aes(x = reorder(Label, Total), y = Total)) +
        geom_col(fill = "#9C2B2B") + coord_flip() + scale_y_continuous(labels = label_dollar(prefix = "£")) +
        labs(x = NULL, y = NULL) + gg_theme
      draw_chart_page(paste0("Invoice Analysis - ", period_label), "Most frequently invoiced and most expensive plant items this period.",
                      list(p_count, p_spend), c("Most Frequently Invoiced", "Most Expensive (Total £)"))
    }
    # ---- Plant Analysis, scoped to this period ----
    if (nrow(history_df) > 0) {
      pa_all <- history_df %>% group_by(ItemID) %>% summarise(Count = n(), .groups = "drop") %>% arrange(desc(Count))
      pa_all$Label <- vapply(pa_all$ItemID, function(iid) item_lookup(iid)[1], character(1))
      pa_jc_raw <- history_df[history_df$EntryType == "Job Card", ]
      pa_jc <- if (nrow(pa_jc_raw) == 0) NULL else {
        agg <- pa_jc_raw %>% group_by(ItemID) %>% summarise(Count = n(), .groups = "drop") %>% arrange(desc(Count))
        agg$Label <- vapply(agg$ItemID, function(iid) item_lookup(iid)[1], character(1))
        agg
      }
      p_hist <- ggplot(head(pa_all, 10), aes(x = reorder(Label, Count), y = Count)) +
        geom_col(fill = "#0B4D3A") + coord_flip() + labs(x = NULL, y = NULL) + gg_theme
      if (!is.null(pa_jc)) {
        p_jc <- ggplot(head(pa_jc, 10), aes(x = reorder(Label, Count), y = Count)) +
          geom_col(fill = "#D9A400") + coord_flip() + labs(x = NULL, y = NULL) + gg_theme
        draw_chart_page(paste0("Plant Analysis - ", period_label), "Which plant items were logged against most this period.",
                        list(p_hist, p_jc), c("Most Frequent History Entries", "Most Job Cards"))
      } else {
        draw_chart_page(paste0("Plant Analysis - ", period_label), "Which plant items were logged against most this period.",
                        list(p_hist), c("Most Frequent History Entries"))
      }
    }
    # ---- Job Cards & Inspections tile grids - placed last ----
    ev_full <- plant_history()
    ev_full <- ev_full[ev_full$EntryType %in% names(JG_TYPE_COLOURS), ]
    if (nrow(ev_full) > 0) {
      ev_full$DateOnly <- as.Date(substr(ev_full$DateTime, 1, 10))
      ev_full$Week <- floor_to_monday(ev_full$DateOnly)
    }
    month_weeks <- weeks_in_range(period_start, period_end)
    year_weeks <- weeks_last_52(period_end)
    tile_items_all <- natural_sort_rows(inv_data[inv_data$Active == "Yes", ])
    draw_tile_grid_section(paste0("Job Cards & Inspections - ", period_label),
                           "All active plant, this period only.",
                           tile_items_all, month_weeks, ev_full, max_rows = 22)
    tile_items_etb <- natural_sort_rows(inv_data[inv_data$Active == "Yes" & inv_data$Category %in% c("Excavator", "Trailer", "Breaker"), ])
    draw_tile_grid_section("Excavators, Trailers & Breakers - This Period",
                           paste0(period_label, " only. Same legend as above."),
                           tile_items_etb, month_weeks, ev_full, max_rows = 22)
    draw_tile_grid_section("Excavators, Trailers & Breakers - Last 12 Months",
                           "Full rolling year. Same legend as above.",
                           tile_items_etb, year_weeks, ev_full, max_rows = 22)
  }
  weekly_report_ui <- function() {
    recorded_by_choices <- c("All", sort(unique(c(
      plant_history()$RecordedBy[plant_history()$RecordedBy != ""],
      invoices_data()$LoggedBy[!is.na(invoices_data()$LoggedBy) & invoices_data()$LoggedBy != ""]
    ))))
    tagList(
      br(),
      fluidRow(
        column(4, dateInput("wr_week_start", "Week starting (Monday)", value = floor_to_monday(Sys.Date()))),
        column(4, selectInput("wr_recorded_by", "Report by input (who logged it)", choices = recorded_by_choices, selected = "All")),
        column(4, div(style = "margin-top:24px;",
                      downloadButton("wr_pdf_download", "Download Full Report (PDF)", class = "btn-warning btn-sm mb-2 w-100"),
                      downloadButton("wr_download", "Download Weekly Invoices (CSV)", class = "btn-primary btn-sm w-100")))
      ),
      fluidRow(
        column(3, metric_card(textOutput("wr_invoice_count", inline = TRUE), "Invoices This Week")),
        column(3, metric_card(textOutput("wr_spend", inline = TRUE), "Spend This Week")),
        column(3, metric_card(textOutput("wr_history_count", inline = TRUE), "History Entries")),
        column(3, metric_card(textOutput("wr_due_count", inline = TRUE), "MOT/Warranty Due (7 days)", colour = "#9C2B2B"))
      ),
      fluidRow(style = "margin-top:10px;",
               column(3, offset = 9, metric_card(textOutput("wr_ts_due_count", inline = TRUE), "Truck Service Due (7 days)", colour = "#9C2B2B"))
      ),
      br(),
      fluidRow(
        column(6, div(class = "chart-card", h6("Invoices by Category This Week"), tableOutput("wr_invoice_category_table"))),
        column(6, div(class = "chart-card", h6("History Entries by Category This Week"), tableOutput("wr_history_category_table")))
      ),
      div(class = "chart-card", h6("Invoices Logged This Week"), tableOutput("wr_invoices_table")),
      div(class = "chart-card", h6("History Entries This Week"), tableOutput("wr_history_table")),
      div(class = "chart-card", h6("Due for MOT or Warranty Within 7 Days"), tableOutput("wr_due_table")),
      div(class = "chart-card", h6("Truck Service Due Within 7 Days"), tableOutput("wr_truckservice_table"))
    )
  }
  monthly_report_ui <- function() {
    this_month_start <- as.Date(format(Sys.Date(), "%Y-%m-01"))
    month_starts <- rev(seq(this_month_start, by = "-1 month", length.out = 12))
    choices_vals <- format(month_starts, "%Y-%m")
    choices_labels <- format(month_starts, "%B %Y")
    recorded_by_choices <- c("All", sort(unique(c(
      plant_history()$RecordedBy[plant_history()$RecordedBy != ""],
      invoices_data()$LoggedBy[!is.na(invoices_data()$LoggedBy) & invoices_data()$LoggedBy != ""]
    ))))
    tagList(
      br(),
      fluidRow(
        column(4, selectInput("mr_month", "Month", choices = setNames(choices_vals, choices_labels), selected = format(Sys.Date(), "%Y-%m"))),
        column(4, selectInput("mr_recorded_by", "Report by input (who logged it)", choices = recorded_by_choices, selected = "All")),
        column(4, div(style = "margin-top:24px;",
                      downloadButton("mr_pdf_download", "Download Full Report (PDF)", class = "btn-warning btn-sm mb-2 w-100"),
                      downloadButton("mr_download", "Download Monthly Invoices (CSV)", class = "btn-primary btn-sm w-100")))
      ),
      fluidRow(
        column(3, metric_card(textOutput("mr_total_spend", inline = TRUE), "Total Spend")),
        column(3, metric_card(textOutput("mr_invoice_count", inline = TRUE), "Invoices")),
        column(3, metric_card(textOutput("mr_avg_invoice", inline = TRUE), "Average Invoice")),
        column(3, metric_card(textOutput("mr_change_val", inline = TRUE), "vs Previous Month"))
      ),
      fluidRow(style = "margin-top:10px;",
               column(3, offset = 9, metric_card(textOutput("mr_history_count", inline = TRUE), "History Entries"))
      ),
      br(),
      div(class = "chart-card", h6("Daily Spend This Month"), plotlyOutput("mr_trend_plot", height = 280)),
      fluidRow(
        column(6, div(class = "chart-card", h6("Invoices by Category This Month"), tableOutput("mr_invoice_category_table"))),
        column(6, div(class = "chart-card", h6("History Entries by Category This Month"), tableOutput("mr_history_category_table")))
      ),
      div(class = "chart-card", h6("Top Companies This Month"), tableOutput("mr_company_table")),
      div(class = "chart-card", h6("Fleet Summary by Category"), tableOutput("mr_fleet_table")),
      div(class = "chart-card", h6("History Entries This Month"), tableOutput("mr_history_table")),
      div(class = "chart-card", h6("Due for MOT or Warranty Within 30 Days"), tableOutput("mr_due_table")),
      div(class = "chart-card", h6("Truck Service Due Within 30 Days"), tableOutput("mr_truckservice_table"))
    )
  }
  # ---- Weekly report data ----
  week_start <- reactive({ req(input$wr_week_start); as.Date(input$wr_week_start) })
  week_end <- reactive({ week_start() + 6 })
  wr_invoices <- reactive({
    d <- inv()
    if (nrow(d) == 0) return(d)
    d <- d[!is.na(d$DateParsed) & d$DateParsed >= week_start() & d$DateParsed <= week_end(), ]
    if (!is.null(input$wr_recorded_by) && input$wr_recorded_by != "All") d <- d[!is.na(d$LoggedBy) & d$LoggedBy == input$wr_recorded_by, ]
    d
  })
  wr_history <- reactive({
    h <- plant_history()
    if (nrow(h) == 0) return(h)
    h$DateOnly <- as.Date(substr(h$DateTime, 1, 10))
    h <- h[!is.na(h$DateOnly) & h$DateOnly >= week_start() & h$DateOnly <= week_end(), ]
    if (!is.null(input$wr_recorded_by) && input$wr_recorded_by != "All") h <- h[!is.na(h$RecordedBy) & h$RecordedBy == input$wr_recorded_by, ]
    h
  })
  wr_due <- reactive({ due_within(7) })
  wr_ts_due <- reactive({ truck_service_due(7) })
  output$wr_invoice_count <- renderText({ nrow(wr_invoices()) })
  output$wr_spend <- renderText({ dollar(sum(wr_invoices()$Amount, na.rm = TRUE), prefix = "£") })
  output$wr_history_count <- renderText({ nrow(wr_history()) })
  output$wr_due_count <- renderText({ nrow(wr_due()) })
  output$wr_ts_due_count <- renderText({ nrow(wr_ts_due()) })
  output$wr_invoices_table <- renderTable({
    d <- wr_invoices()
    if (nrow(d) == 0) return(data.frame(Message = "No invoices logged this week."))
    d %>% transmute(Date, Company, `Amount (£)` = sprintf("%.2f", Amount),
                    Item = paste0(Category, " > ", SubCategory, " > ", Reference_PMK_Number))
  })
  output$wr_history_table <- renderTable({
    d <- wr_history()
    if (nrow(d) == 0) return(data.frame(Message = "No history entries this week."))
    d %>% transmute(Item = ItemID, `Date/Time` = DateTime, Type = EntryType, Description, `Recorded By` = RecordedBy)
  })
  output$wr_invoice_category_table <- renderTable({
    d <- wr_invoices()
    if (nrow(d) == 0) return(data.frame(Message = "No invoices logged this week."))
    d %>% group_by(Category) %>% summarise(Invoices = n(), `Total (£)` = sprintf("%.2f", sum(Amount, na.rm = TRUE))) %>%
      arrange(desc(Invoices))
  })
  output$wr_history_category_table <- renderTable({
    h <- wr_history()
    if (nrow(h) == 0) return(data.frame(Message = "No history entries this week."))
    df_inv <- inventory_data()
    h$Category <- df_inv$Category[match(h$ItemID, df_inv$ItemID)]
    h$Category[is.na(h$Category) | h$Category == ""] <- "Unknown"
    h %>% group_by(Category) %>% summarise(Entries = n()) %>% arrange(desc(Entries))
  })
  output$wr_due_table <- renderTable({
    d <- wr_due()
    if (nrow(d) == 0) return(data.frame(Message = "Nothing due within 7 days."))
    d %>% transmute(Item = ifelse(Machine == "", ItemID, Machine),
                    `PMK/Reg` = ifelse(PMK_Number != "", PMK_Number, Registration),
                    Type = DueType, `Due Date` = as.character(DueDate))
  })
  output$wr_truckservice_table <- renderTable({
    d <- wr_ts_due()
    if (nrow(d) == 0) return(data.frame(Message = "Nothing due within 7 days."))
    d %>% transmute(Item = ifelse(Machine == "", ItemID, Machine),
                    `PMK/Reg` = ifelse(PMK_Number != "", PMK_Number, Registration),
                    `Last Serviced` = LastServiced, `Due Date` = as.character(DueDate), Status)
  })
  output$wr_download <- downloadHandler(
    filename = function() paste0("pmk_weekly_report_", week_start(), ".csv"),
    content = function(file) write.csv(wr_invoices(), file, row.names = FALSE)
  )
  output$wr_pdf_download <- downloadHandler(
    filename = function() paste0("pmk_weekly_report_", week_start(), ".pdf"),
    content = function(file) {
      generate_full_report_pdf(file, "Weekly", paste0("Week of ", format(week_start(), "%d %b %Y")),
                               week_start(), week_end(), inventory_data(), wr_history(), wr_invoices())
    }
  )
  # ---- Invoice Analysis (Reports > Invoice Analysis) ----
  # Ranks items by invoice count and by total spend, same Category/
  # Sub-Category filter pattern used everywhere else. Reuses
  # find_item_id() (the same matching Invoice->History already relies
  # on) so slightly-different reference text for the same item still
  # groups together, rather than splitting "PMK 2" and "PMK-2" apart.
  observeEvent(input$ia_category, {
    if (is.null(input$ia_category) || input$ia_category == "All") {
      updateSelectizeInput(session, "ia_subcategory", choices = "All", selected = "All")
    } else {
      subs <- subcats_for(input$ia_category, inventory_data())
      updateSelectizeInput(session, "ia_subcategory", choices = c("All", subs), selected = "All")
    }
  }, ignoreInit = TRUE)
  invoice_item_agg <- reactive({
    d <- inv_filtered()
    if (!is.null(input$ia_category) && input$ia_category != "All") d <- d[d$Category == input$ia_category, ]
    if (!is.null(input$ia_subcategory) && input$ia_subcategory != "All") d <- d[d$SubCategory == input$ia_subcategory, ]
    empty <- data.frame(MatchedID = character(0), Label = character(0), Category = character(0),
                         SubCategory = character(0), Count = integer(0), Total = numeric(0), stringsAsFactors = FALSE)
    if (nrow(d) == 0) return(empty)
    df_inv <- inventory_data()
    d$MatchedID <- vapply(seq_len(nrow(d)), function(i) {
      mid <- find_item_id(d$Category[i], d$SubCategory[i], d$Reference_PMK_Number[i], df_inv)
      if (is.na(mid)) paste0("Unmatched: ", d$Reference_PMK_Number[i]) else mid
    }, character(1))
    label_for <- function(mid) {
      if (startsWith(mid, "Unmatched: ")) return(mid)
      row <- df_inv[df_inv$ItemID == mid, ]
      if (nrow(row) == 0) return(mid)
      id <- item_identifier(row[1, ])
      if (row$Machine[1] != "") paste0(id, " - ", row$Machine[1]) else id
    }
    agg <- d %>% group_by(MatchedID) %>%
      summarise(Count = n(), Total = sum(Amount, na.rm = TRUE),
                Category = dplyr::first(Category), SubCategory = dplyr::first(SubCategory), .groups = "drop")
    agg$Label <- vapply(agg$MatchedID, label_for, character(1))
    agg %>% arrange(desc(Total))
  })
  output$ia_count_plot <- renderPlotly({
    a <- invoice_item_agg()
    if (nrow(a) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    top <- a %>% arrange(desc(Count)) %>% head(10)
    p <- ggplot(top, aes(x = reorder(Label, Count), y = Count, text = paste0(Count, " invoice(s)"))) +
      geom_col(fill = "#0B4D3A") + coord_flip() + labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  output$ia_spend_plot <- renderPlotly({
    a <- invoice_item_agg()
    if (nrow(a) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    top <- a %>% arrange(desc(Total)) %>% head(10)
    p <- ggplot(top, aes(x = reorder(Label, Total), y = Total, text = paste0("£", round(Total, 2)))) +
      geom_col(fill = "#9C2B2B") + coord_flip() + scale_y_continuous(labels = label_dollar(prefix = "£")) +
      labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  output$ia_download <- downloadHandler(
    filename = function() paste0("pmk_invoice_analysis_", Sys.Date(), ".csv"),
    content = function(file) {
      a <- invoice_item_agg() %>%
        transmute(Item = Label, Category, SubCategory, `Invoice Count` = Count, `Total Spend (£)` = sprintf("%.2f", Total))
      write.csv(a, file, row.names = FALSE)
    }
  )
  # ---- Monthly report data ----
  month_range <- reactive({
    req(input$mr_month)
    start <- as.Date(paste0(input$mr_month, "-01"))
    end <- seq(start, by = "1 month", length.out = 2)[2] - 1
    list(start = start, end = end)
  })
  prev_month_range <- reactive({
    mr <- month_range()
    prev_start <- seq(mr$start, by = "-1 month", length.out = 2)[2]
    list(start = prev_start, end = mr$start - 1)
  })
  mr_invoices <- reactive({
    d <- inv(); mr <- month_range()
    if (nrow(d) == 0) return(d)
    d <- d[!is.na(d$DateParsed) & d$DateParsed >= mr$start & d$DateParsed <= mr$end, ]
    if (!is.null(input$mr_recorded_by) && input$mr_recorded_by != "All") d <- d[!is.na(d$LoggedBy) & d$LoggedBy == input$mr_recorded_by, ]
    d
  })
  mr_prev_invoices <- reactive({
    d <- inv(); pr <- prev_month_range()
    if (nrow(d) == 0) return(d)
    d <- d[!is.na(d$DateParsed) & d$DateParsed >= pr$start & d$DateParsed <= pr$end, ]
    if (!is.null(input$mr_recorded_by) && input$mr_recorded_by != "All") d <- d[!is.na(d$LoggedBy) & d$LoggedBy == input$mr_recorded_by, ]
    d
  })
  mr_due <- reactive({ due_within(30) })
  mr_ts_due <- reactive({ truck_service_due(30) })
  mr_history <- reactive({
    h <- plant_history()
    if (nrow(h) == 0) return(h)
    mr <- month_range()
    h$DateOnly <- as.Date(substr(h$DateTime, 1, 10))
    h <- h[!is.na(h$DateOnly) & h$DateOnly >= mr$start & h$DateOnly <= mr$end, ]
    if (!is.null(input$mr_recorded_by) && input$mr_recorded_by != "All") h <- h[!is.na(h$RecordedBy) & h$RecordedBy == input$mr_recorded_by, ]
    h
  })
  output$mr_history_count <- renderText({ nrow(mr_history()) })
  output$mr_total_spend <- renderText({ dollar(sum(mr_invoices()$Amount, na.rm = TRUE), prefix = "£") })
  output$mr_invoice_count <- renderText({ nrow(mr_invoices()) })
  output$mr_avg_invoice <- renderText({
    d <- mr_invoices()
    if (nrow(d) == 0) "£0.00" else dollar(mean(d$Amount, na.rm = TRUE), prefix = "£")
  })
  output$mr_change_val <- renderText({
    cur <- sum(mr_invoices()$Amount, na.rm = TRUE)
    prev <- sum(mr_prev_invoices()$Amount, na.rm = TRUE)
    if (prev == 0) return("n/a")
    pct <- (cur - prev) / abs(prev) * 100
    paste0(ifelse(pct >= 0, "+", ""), sprintf("%.1f", pct), "%")
  })
  output$mr_trend_plot <- renderPlotly({
    d <- mr_invoices()
    if (nrow(d) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    daily <- d %>% group_by(DateParsed) %>% summarise(Total = sum(Amount, na.rm = TRUE)) %>% arrange(DateParsed)
    p <- ggplot(daily, aes(x = DateParsed, y = Total, text = paste0("£", round(Total, 2)))) +
      geom_col(fill = "#0B4D3A") + scale_y_continuous(labels = label_dollar(prefix = "£")) +
      labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  output$mr_company_table <- renderTable({
    d <- mr_invoices()
    if (nrow(d) == 0) return(data.frame(Message = "No invoices logged this month."))
    d %>% group_by(Company) %>%
      summarise(`Total (£)` = sprintf("%.2f", sum(Amount, na.rm = TRUE)), Invoices = n()) %>%
      arrange(desc(`Total (£)`))
  })
  output$mr_fleet_table <- renderTable({
    df <- inventory_data()
    df %>% group_by(Category) %>%
      summarise(Total = n(), Active = sum(Active == "Yes"), Inactive = sum(Active == "No"))
  })
  output$mr_invoice_category_table <- renderTable({
    d <- mr_invoices()
    if (nrow(d) == 0) return(data.frame(Message = "No invoices logged this month."))
    d %>% group_by(Category) %>% summarise(Invoices = n(), `Total (£)` = sprintf("%.2f", sum(Amount, na.rm = TRUE))) %>%
      arrange(desc(Invoices))
  })
  output$mr_history_category_table <- renderTable({
    h <- mr_history()
    if (nrow(h) == 0) return(data.frame(Message = "No history entries this month."))
    df_inv <- inventory_data()
    h$Category <- df_inv$Category[match(h$ItemID, df_inv$ItemID)]
    h$Category[is.na(h$Category) | h$Category == ""] <- "Unknown"
    h %>% group_by(Category) %>% summarise(Entries = n()) %>% arrange(desc(Entries))
  })
  output$mr_history_table <- renderTable({
    d <- mr_history()
    if (nrow(d) == 0) return(data.frame(Message = "No history entries this month."))
    d %>% transmute(Item = ItemID, `Date/Time` = DateTime, Type = EntryType, Description, `Recorded By` = RecordedBy)
  })
  output$mr_due_table <- renderTable({
    d <- mr_due()
    if (nrow(d) == 0) return(data.frame(Message = "Nothing due within 30 days."))
    d %>% transmute(Item = ifelse(Machine == "", ItemID, Machine),
                    `PMK/Reg` = ifelse(PMK_Number != "", PMK_Number, Registration),
                    Type = DueType, `Due Date` = as.character(DueDate))
  })
  output$mr_truckservice_table <- renderTable({
    d <- mr_ts_due()
    if (nrow(d) == 0) return(data.frame(Message = "Nothing due within 30 days."))
    d %>% transmute(Item = ifelse(Machine == "", ItemID, Machine),
                    `PMK/Reg` = ifelse(PMK_Number != "", PMK_Number, Registration),
                    `Last Serviced` = LastServiced, `Due Date` = as.character(DueDate), Status)
  })
  output$mr_download <- downloadHandler(
    filename = function() paste0("pmk_monthly_report_", input$mr_month, ".csv"),
    content = function(file) write.csv(mr_invoices(), file, row.names = FALSE)
  )
  output$mr_pdf_download <- downloadHandler(
    filename = function() paste0("pmk_monthly_report_", input$mr_month, ".pdf"),
    content = function(file) {
      mr <- month_range()
      generate_full_report_pdf(file, "Monthly", format(mr$start, "%B %Y"),
                               mr$start, mr$end, inventory_data(), mr_history(), mr_invoices())
    }
  )
  # -------------------------------------------------------------
  # JOB CARDS & INSPECTIONS - weekly tick-box grid. Green square =
  # Service Inspection logged that week for that item, yellow = Job
  # Card, split = both, grey = nothing logged. Admin/Mechanic only -
  # same access as who can actually add these entry types.
  # -------------------------------------------------------------
  output$jobcards_tab_content <- renderUI({ jobcards_ui(role()) })
  jobcards_ui <- function(r) {
    tagList(
      br(),
      p(class = "text-muted",
        "Green = Service Inspection, yellow = Job Card, pink = Mechanic Work, blue = Invoice, logged that week - split square = more than one. Last 52 weeks. Click any coloured square to see exactly what was logged. ",
        "Shown grouped by Category/Sub-Category by default - pick a Category (and optionally Sub-Category) to narrow it down, or download the full log below."),
      fluidRow(
        column(4, selectInput("jg_category", "Category", choices = c("All", CATEGORY_OPTIONS), selected = "All")),
        column(4, selectizeInput("jg_subcategory", "Sub-Category", choices = "All", selected = "All")),
        column(4, div(style = "margin-top:24px;", downloadButton("jg_download", "Download (CSV)", class = "btn-primary btn-sm")))
      ),
      # Collapsible, and shut by default - it's a "go looking" tool rather
      # than something you want between you and the grid every visit. The
      # count in the title keeps it useful while it's closed, and Bootstrap
      # only hides the panel body rather than removing it, so the controls
      # inside still drive the grid below even when it's collapsed.
      accordion(
        id = "jg_overdue_accordion", open = FALSE,
        accordion_panel(
          title = tagList("Plant with nothing logged",
                          span(class = "text-muted", style = "font-weight:400;",
                               textOutput("jg_overdue_count_label", inline = TRUE))),
          value = "jg_overdue",
          p(class = "text-muted mb-2",
            "Which machines haven't been seen for a while. Pick what counts as having been seen - the same entry types the grid shows - and how far back to look."),
          fluidRow(
            column(4, selectizeInput("jg_ov_types", "Counts as having been seen",
                                     choices = names(JG_TYPE_COLOURS), selected = "Service Inspection",
                                     multiple = TRUE, options = list(placeholder = "Service Inspection"))),
            column(3, selectInput("jg_ov_window", "Nothing logged in the last",
                                  choices = c("6 weeks", "3 months", "6 months", "12 months", "Custom (weeks)"),
                                  selected = "6 months")),
            column(2, conditionalPanel("input.jg_ov_window == 'Custom (weeks)'",
                                       numericInput("jg_ov_weeks", "Weeks", value = 26, min = 1, step = 1))),
            column(3, div(style = "margin-top:24px;",
                          downloadButton("jg_overdue_download", "Download (CSV)", class = "btn-primary btn-sm")))
          ),
          checkboxInput("jg_ov_only", "Show only these items in the grid below", value = FALSE),
          uiOutput("jg_overdue_summary"),
          div(style = "max-height:320px; overflow-y:auto;", tableOutput("jg_overdue_table"))
        )
      ),
      div(class = "chart-card", style = "overflow-x:auto;", uiOutput("jg_grid")),
      div(style = "display:flex; gap:16px; margin-top:6px; font-size:11px; color:#666; flex-wrap:wrap;",
          div(style = "display:flex; align-items:center; gap:5px;", span(style = "width:12px;height:12px;background:#3E7C59;border-radius:2px;display:inline-block;"), "Service Inspection"),
          div(style = "display:flex; align-items:center; gap:5px;", span(style = "width:12px;height:12px;background:#D9A400;border-radius:2px;display:inline-block;"), "Job Card"),
          div(style = "display:flex; align-items:center; gap:5px;", span(style = "width:12px;height:12px;background:#D6598E;border-radius:2px;display:inline-block;"), "Mechanic Work"),
          div(style = "display:flex; align-items:center; gap:5px;", span(style = "width:12px;height:12px;background:#3A6EA5;border-radius:2px;display:inline-block;"), "Invoice"),
          div(style = "display:flex; align-items:center; gap:5px;", span(style = "width:12px;height:12px;background:#E2E2E2;border-radius:2px;display:inline-block;"), "Nothing logged")
      )
    )
  }
  observeEvent(input$jg_category, {
    if (is.null(input$jg_category) || input$jg_category == "All") {
      updateSelectizeInput(session, "jg_subcategory", choices = "All", selected = "All")
    } else {
      subs <- subcats_for(input$jg_category, inventory_data())
      updateSelectizeInput(session, "jg_subcategory", choices = c("All", subs), selected = "All")
    }
  }, ignoreInit = TRUE)
  # Category/Sub-Category only. This is the population the "nothing
  # logged" check measures against, and it deliberately does NOT apply
  # that check itself - jg_filtered_items does, and it depends on this,
  # so putting both in one reactive would make it depend on its own
  # result.
  jg_base_items <- reactive({
    df <- inventory_data()
    if (!is.null(input$jg_category) && input$jg_category != "All") df <- df[df$Category == input$jg_category, , drop = FALSE]
    if (!is.null(input$jg_subcategory) && input$jg_subcategory != "All") df <- df[df$SubCategory == input$jg_subcategory, , drop = FALSE]
    natural_sort_rows(df)
  })
  jg_filtered_items <- reactive({
    df <- jg_base_items()
    ids <- jg_visible_ids()
    if (!is.null(ids)) df <- df[df$ItemID %in% ids, , drop = FALSE]
    df
  })
  # ---- "Nothing logged" / not serviced ----
  jg_ov_types <- reactive({
    t <- input$jg_ov_types
    if (is.null(t) || length(t) == 0) "Service Inspection" else t
  })
  jg_ov_days <- reactive({
    w <- if (!is.null(input$jg_ov_window)) input$jg_ov_window else "6 months"
    switch(w,
           "6 weeks" = 42, "3 months" = 91, "6 months" = 182, "12 months" = 365,
           { n <- input$jg_ov_weeks; if (is.null(n) || is.na(n) || n < 1) 182 else as.integer(n) * 7 })
  })
  # Active plant whose most recent entry of the chosen type(s) is older
  # than the window - plus anything that has never had one at all, which
  # is the case actually worth finding.
  jg_overdue <- reactive({
    items <- jg_base_items()
    items <- items[items$Active == "Yes", , drop = FALSE]
    empty <- data.frame(ItemID = character(0), Item = character(0), Machine = character(0),
                        Category = character(0), `Sub-Category` = character(0),
                        `Last Logged` = character(0), `Days Since` = integer(0),
                        check.names = FALSE, stringsAsFactors = FALSE)
    if (nrow(items) == 0) return(empty)
    cutoff <- Sys.Date() - jg_ov_days()
    h <- plant_history()
    h <- h[h$EntryType %in% jg_ov_types(), , drop = FALSE]
    if (nrow(h) > 0) {
      h$DateOnly <- as.Date(substr(h$DateTime, 1, 10))
      h <- h[!is.na(h$DateOnly), , drop = FALSE]
    }
    last_by <- if (nrow(h) == 0) setNames(as.Date(character(0)), character(0)) else
      do.call(c, lapply(split(h$DateOnly, h$ItemID), max))
    last_for <- function(id) if (id %in% names(last_by)) last_by[[id]] else as.Date(NA)
    keep <- vapply(items$ItemID, function(id) { d <- last_for(id); is.na(d) || d < cutoff }, logical(1))
    items <- items[keep, , drop = FALSE]
    if (nrow(items) == 0) return(empty)
    lastd <- vapply(items$ItemID, function(id) { d <- last_for(id); if (is.na(d)) NA_character_ else as.character(d) }, character(1))
    days <- as.integer(Sys.Date() - as.Date(lastd))
    out <- data.frame(
      ItemID = items$ItemID,
      Item = vapply(seq_len(nrow(items)), function(i) item_identifier(items[i, ]), character(1)),
      Machine = ifelse(items$Machine == "", "-", items$Machine),
      Category = items$Category,
      `Sub-Category` = items$SubCategory,
      `Last Logged` = ifelse(is.na(lastd), "Never", lastd),
      # Numeric, with NA for never-logged, so it sorts as a number in the
      # CSV rather than as text. The on-screen table renders NA as "-".
      `Days Since` = days,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    # Never-logged first (they're the worst case), then longest gap down.
    sort_key <- ifelse(is.na(days), Inf, days)
    out <- out[order(-sort_key, out$Item), , drop = FALSE]
    rownames(out) <- NULL
    out
  })
  jg_visible_ids <- reactive({
    if (!isTRUE(input$jg_ov_only)) return(NULL)
    ov <- jg_overdue()
    if (nrow(ov) == 0) character(0) else ov$ItemID
  })
  jg_ov_window_label <- reactive({
    w <- if (!is.null(input$jg_ov_window)) input$jg_ov_window else "6 months"
    if (w == "Custom (weeks)") paste0(jg_ov_days() %/% 7, " weeks") else w
  })
  output$jg_overdue_summary <- renderUI({
    ov <- jg_overdue()
    base <- jg_base_items()
    total <- nrow(base[base$Active == "Yes", , drop = FALSE])
    kinds <- paste(jg_ov_types(), collapse = " / ")
    if (nrow(ov) == 0) div(class = "alert alert-success mb-2",
      paste0("All ", total, " active item(s) have had ", kinds, " logged within the last ", jg_ov_window_label(), "."))
    else div(class = "alert alert-warning mb-2",
      paste0(nrow(ov), " of ", total, " active item(s) have had no ", kinds,
             " logged in the last ", jg_ov_window_label(), "."))
  })
  output$jg_overdue_count_label <- renderText({
    n <- nrow(jg_overdue())
    if (n == 0) " - nothing outstanding" else paste0(" - ", n, " outstanding")
  })
  jg_overdue_table_data <- function() {
    ov <- jg_overdue()
    ov[, setdiff(names(ov), "ItemID"), drop = FALSE]
  }
  output$jg_overdue_table <- renderTable({
    if (nrow(jg_overdue()) == 0) return(data.frame(Message = "Nothing outstanding for these filters."))
    jg_overdue_table_data()
  }, na = "-")
  output$jg_overdue_download <- downloadHandler(
    filename = function() paste0("pmk_nothing_logged_", Sys.Date(), ".csv"),
    content = function(file) write.csv(jg_overdue_table_data(), file, row.names = FALSE)
  )
  jg_weeks <- reactive({
    this_week <- floor_to_monday(Sys.Date())
    rev(seq(this_week, by = "-1 week", length.out = 52))
  })
  jg_events <- reactive({
    h <- plant_history()
    h <- h[h$EntryType %in% names(JG_TYPE_COLOURS), ]
    if (nrow(h) == 0) return(h)
    h$DateOnly <- as.Date(substr(h$DateTime, 1, 10))
    h$Week <- floor_to_monday(h$DateOnly)
    h
  })
  # Colour per entry type shown on the grid - a single square gets a
  # diagonal-stripe gradient if more than one type happened the same
  # week for the same item, rather than only having room for two.
  # Blue for Invoice - an invoice logged against an item creates a
  # history entry automatically, so the grid shows spend on a machine
  # next to the work done on it. Everything downstream (the grid, the
  # click pop-up, the CSV, the PDF tile grids) reads this one list.
  JG_TYPE_COLOURS <- c("Service Inspection" = "#3E7C59", "Job Card" = "#D9A400",
                       "Mechanic Work" = "#D6598E", "Invoice" = "#3A6EA5")
  jg_cell_style <- function(types_present) {
    if (length(types_present) == 0) return("#E2E2E2")
    if (length(types_present) == 1) return(JG_TYPE_COLOURS[[types_present[1]]])
    cols <- JG_TYPE_COLOURS[types_present]
    n <- length(cols)
    stops <- vapply(seq_len(n), function(i) {
      pct1 <- round((i - 1) / n * 100); pct2 <- round(i / n * 100)
      paste0(cols[i], " ", pct1, "%,", cols[i], " ", pct2, "%")
    }, character(1))
    paste0("linear-gradient(135deg,", paste(stops, collapse = ","), ")")
  }
  # Builds just the week-header row + one row per item (no legend -
  # that's shown once, statically, below the whole grid) - shared by
  # both the flat filtered view and each Sub-Category panel in the
  # default grouped view.
  jg_grid_rows_ui <- function(items, weeks, ev) {
    n_weeks <- length(weeks)
    header_cells <- lapply(seq_len(n_weeks), function(i) {
      lbl <- if (i %% 4 == 1) format(weeks[i], "%d %b") else ""
      div(style = "width:16px; font-size:9px; color:#8a8a8a; text-align:center; flex-shrink:0;", lbl)
    })
    row_divs <- lapply(seq_len(nrow(items)), function(ridx) {
      row <- items[ridx, ]
      label <- item_identifier(row)
      cells <- lapply(seq_len(n_weeks), function(w) {
        wk <- weeks[w]
        matches <- if (nrow(ev) == 0) ev else ev[ev$ItemID == row$ItemID & ev$Week == wk, ]
        types_present <- if (nrow(matches) == 0) character(0) else intersect(names(JG_TYPE_COLOURS), unique(matches$EntryType))
        bg <- jg_cell_style(types_present)
        title_txt <- paste0(label, " - week of ", format(wk, "%d %b %Y"),
                             if (length(types_present) > 0) paste0(" - ", paste(types_present, collapse = ", ")) else "")
        # Only squares with something logged are clickable - they open
        # the actual entries for that item/week (see jg_cell_click).
        has_entries <- length(types_present) > 0
        div(title = title_txt,
            style = paste0("width:16px; height:16px; border-radius:3px; background:", bg, "; flex-shrink:0;",
                           if (has_entries) " cursor:pointer;" else ""),
            onclick = if (has_entries) sprintf("Shiny.setInputValue('jg_cell_click', {item:'%s', week:'%s'}, {priority:'event'})",
                                               js_escape_sq(row$ItemID), format(wk, "%Y-%m-%d")) else NULL)
      })
      div(style = "display:flex; align-items:center; margin-bottom:3px;",
          div(style = "width:100px; font-size:11px; font-weight:600; flex-shrink:0;", label),
          div(style = "display:flex; gap:3px;", cells)
      )
    })
    tagList(
      div(style = "display:flex; margin-bottom:4px;",
          div(style = "width:100px; flex-shrink:0;"),
          div(style = "display:flex; gap:3px;", header_cells)
      ),
      row_divs
    )
  }
  # Clicking a coloured square pops up exactly what was logged for that
  # item that week - every Service Inspection / Job Card / Mechanic Work
  # entry, in full, without leaving the grid.
  observeEvent(input$jg_cell_click, {
    cl <- input$jg_cell_click
    req(cl$item, cl$week)
    wk <- as.Date(cl$week)
    ev <- jg_events()
    hits <- if (nrow(ev) == 0) ev else ev[ev$ItemID == cl$item & !is.na(ev$Week) & ev$Week == wk, , drop = FALSE]
    if (nrow(hits) > 0) hits <- hits[order(hits$DateTime), , drop = FALSE]
    inv_row <- inventory_data()[inventory_data()$ItemID == cl$item, ]
    label <- if (nrow(inv_row) > 0) item_identifier(inv_row[1, ]) else cl$item
    machine <- if (nrow(inv_row) > 0 && inv_row$Machine[1] != "") paste0(" - ", inv_row$Machine[1]) else ""
    removeModal()
    showModal(modalDialog(
      title = paste0(label, machine, " - week of ", format(wk, "%d %b %Y")),
      size = "m", easyClose = TRUE,
      if (nrow(hits) == 0) p(class = "text-muted", "Nothing logged for this week.")
      else tagList(lapply(seq_len(nrow(hits)), function(i) {
        h <- hits[i, ]
        desc <- if (is.na(h$Description)) "" else h$Description
        div(class = "history-item", style = paste0("border-left-color:", JG_TYPE_COLOURS[[h$EntryType]], ";"),
            div(strong(h$EntryType), span(class = "text-muted", paste0(" - ", h$DateTime))),
            tagList(lapply(strsplit(desc, "\n", fixed = TRUE)[[1]], function(ln) p(class = "mb-1", ln))),
            p(class = "mb-0 text-muted", style = "font-size:0.85rem;", paste("By:", h$RecordedBy))
        )
      })),
      footer = modalButton("Close")
    ))
  })
  output$jg_grid <- renderUI({
    weeks <- jg_weeks()
    ev <- jg_events()
    cat_sel <- if (!is.null(input$jg_category)) input$jg_category else "All"
    if (cat_sel == "All") {
      # Default view: grouped by Category > Sub-Category, same
      # drill-down pattern as Inventory List / gang sheet forms.
      df <- inventory_data()
      ids <- jg_visible_ids()
      if (!is.null(ids)) df <- df[df$ItemID %in% ids, , drop = FALSE]
      if (nrow(df) == 0) return(div(class = "alert alert-secondary",
                                    if (is.null(ids)) "No plant items yet."
                                    else "Nothing outstanding - no items match the 'nothing logged' filter."))
      cat_panels <- lapply(CATEGORY_OPTIONS, function(cat) {
        cat_rows <- df[df$Category == cat, ]
        subs <- subcats_for(cat, df)
        sub_panels <- lapply(subs, function(sub) {
          sub_rows <- natural_sort_rows(cat_rows[cat_rows$SubCategory == sub, ])
          accordion_panel(
            title = paste0(sub, " (", nrow(sub_rows), ")"), value = paste0(cat, "___", sub),
            if (nrow(sub_rows) == 0) p(class = "text-muted mb-0", "None.")
            else jg_grid_rows_ui(sub_rows, weeks, ev)
          )
        })
        accordion_panel(
          title = paste0(cat, " (", nrow(cat_rows), ")"), value = cat,
          if (length(sub_panels) == 0) p(class = "text-muted mb-0", "None in this category.")
          else do.call(accordion, c(list(id = paste0("jg_cat_accordion_", make.names(cat))), sub_panels))
        )
      })
      do.call(accordion, c(list(id = "jg_top_accordion", open = FALSE), cat_panels))
    } else {
      items <- jg_filtered_items()
      if (nrow(items) == 0) return(div(class = "alert alert-secondary", "No items match this filter."))
      jg_grid_rows_ui(items, weeks, ev)
    }
  })
  jg_download_data <- reactive({
    items <- jg_filtered_items()
    if (nrow(items) == 0) return(data.frame(Message = "No items match this filter."))
    ev <- jg_events()
    ev2 <- if (nrow(ev) == 0) ev else ev[ev$ItemID %in% items$ItemID, ]
    if (nrow(ev2) == 0) return(data.frame(Message = "No Job Cards or Service Inspections logged for this filter yet."))
    id_lookup <- setNames(vapply(seq_len(nrow(items)), function(i) item_identifier(items[i, ]), character(1)), items$ItemID)
    ev2$Item <- id_lookup[ev2$ItemID]
    ev2 %>% transmute(Item, `Week Commencing` = as.character(Week), `Entry Type` = EntryType,
                       `Date/Time` = DateTime, `Recorded By` = RecordedBy) %>%
      arrange(Item, `Week Commencing`)
  })
  output$jg_download <- downloadHandler(
    filename = function() paste0("pmk_jobcards_inspections_", Sys.Date(), ".csv"),
    content = function(file) write.csv(jg_download_data(), file, row.names = FALSE)
  )
  # -------------------------------------------------------------
  # PLANT ANALYSIS - ranks plant by activity: which items get logged
  # against most, and which need the most Job Cards. Admin/Mechanic
  # only, same access as Job Cards & Inspections.
  # -------------------------------------------------------------
  output$plant_analysis_tab_content <- renderUI({ plant_analysis_ui(role()) })
  plant_analysis_ui <- function(r) {
    tagList(
      br(),
      p(class = "text-muted",
        "Ranks plant by how much gets logged against it. Everything below follows the filters - narrow by period, category, sub-category or entry type and the tiles, both charts and the table all move together."),
      div(class = "chart-card",
          fluidRow(
            column(3, selectInput("pa_period", "Period",
                                  choices = c("Last 6 Weeks", "Last 6 Months", "Last 12 Months", "All Time", "Custom range"),
                                  selected = "Last 6 Months")),
            column(3, conditionalPanel(
              "input.pa_period == 'Custom range'",
              dateRangeInput("pa_dates", "Date range", start = Sys.Date() - 182, end = Sys.Date()))),
            column(3, selectInput("pa_category", "Category", choices = c("All", CATEGORY_OPTIONS), selected = "All")),
            column(3, selectizeInput("pa_subcategory", "Sub-Category", choices = "All", selected = "All"))
          ),
          fluidRow(
            column(6, selectizeInput("pa_types", "Entry types", choices = ALL_ENTRY_TYPES, selected = ALL_ENTRY_TYPES,
                                     multiple = TRUE, options = list(placeholder = "All entry types"))),
            column(3, numericInput("pa_min_jc", "Min. job cards", value = 0, min = 0, step = 1)),
            column(3, numericInput("pa_top_n", "Show top", value = 10, min = 3, max = 50, step = 5))
          )
      ),
      fluidRow(
        column(4, metric_card(textOutput("pa_n_items", inline = TRUE), "Plant Matching")),
        column(4, metric_card(textOutput("pa_n_entries", inline = TRUE), "Entries In Period")),
        column(4, metric_card(textOutput("pa_n_jobcards", inline = TRUE), "Job Cards In Period"))
      ),
      br(),
      fluidRow(
        column(6, div(class = "chart-card", h6("Most Frequent History Entries"), plotlyOutput("pa_history_plot", height = 340))),
        column(6, div(class = "chart-card", h6("Most Job Cards"), plotlyOutput("pa_jobcard_plot", height = 340)))
      ),
      div(class = "chart-card",
          fluidRow(
            column(8, h6("Every matching item, most active first")),
            column(4, style = "text-align:right;",
                   downloadButton("pa_download", "Download (CSV)", class = "btn-primary btn-sm"))
          ),
          p(class = "text-muted", style = "font-size:0.85rem;",
            "Items with nothing logged in the period are included too - they sit at the bottom on zero, which is usually the interesting end."),
          div(style = "max-height:420px; overflow-y:auto;", tableOutput("pa_table"))
      )
    )
  }
  observeEvent(input$pa_category, {
    if (is.null(input$pa_category) || input$pa_category == "All") {
      updateSelectizeInput(session, "pa_subcategory", choices = "All", selected = "All")
    } else {
      subs <- subcats_for(input$pa_category, inventory_data())
      updateSelectizeInput(session, "pa_subcategory", choices = c("All", subs), selected = "All")
    }
  }, ignoreInit = TRUE)
  pa_range <- reactive({
    period <- if (!is.null(input$pa_period)) input$pa_period else "Last 6 Months"
    end <- Sys.Date()
    if (period == "Custom range") {
      req(input$pa_dates, length(input$pa_dates) == 2, !anyNA(input$pa_dates))
      return(list(start = as.Date(input$pa_dates[1]), end = as.Date(input$pa_dates[2])))
    }
    start <- switch(period,
                    "Last 6 Weeks" = end - 41,
                    "Last 12 Months" = end - 364,
                    "All Time" = as.Date("1900-01-01"),
                    end - 182)
    list(start = start, end = end)
  })
  # The plant the filters select - the population every count below is
  # measured against, including the ones with nothing logged.
  pa_items <- reactive({
    df <- inventory_data()
    if (!is.null(input$pa_category) && input$pa_category != "All") df <- df[df$Category == input$pa_category, , drop = FALSE]
    if (!is.null(input$pa_subcategory) && input$pa_subcategory != "All") df <- df[df$SubCategory == input$pa_subcategory, , drop = FALSE]
    natural_sort_rows(df)
  })
  # pa_history_all() is the period/plant slice WITHOUT the entry-type
  # picker applied; pa_history() adds it. Job card counts come from the
  # former on purpose, so unticking "Job Card" narrows the entries chart
  # without emptying the job cards chart and the min-job-cards filter
  # underneath it.
  pa_history_all <- reactive({
    h <- plant_history()
    if (nrow(h) == 0) return(h)
    rg <- pa_range()
    h$DateOnly <- as.Date(substr(h$DateTime, 1, 10))
    h <- h[!is.na(h$DateOnly) & h$DateOnly >= rg$start & h$DateOnly <= rg$end, , drop = FALSE]
    h[h$ItemID %in% pa_items()$ItemID, , drop = FALSE]
  })
  pa_history <- reactive({
    h <- pa_history_all()
    types <- input$pa_types
    if (nrow(h) == 0 || is.null(types) || length(types) == 0) return(h)
    h[h$EntryType %in% types, , drop = FALSE]
  })
  pa_count_by_item <- function(h) {
    if (nrow(h) == 0) return(setNames(integer(0), character(0)))
    tb <- table(h$ItemID)
    setNames(as.integer(tb), names(tb))
  }
  pa_top_n <- reactive({
    n <- input$pa_top_n
    if (is.null(n) || is.na(n) || n < 1) 10L else min(as.integer(n), 50L)
  })
  pa_summary <- reactive({
    items <- pa_items()
    empty <- data.frame(ItemID = character(0), Item = character(0), Machine = character(0),
                        Category = character(0), `Sub-Category` = character(0),
                        Entries = integer(0), `Job Cards` = integer(0), `Last Entry` = character(0),
                        check.names = FALSE, stringsAsFactors = FALSE)
    if (nrow(items) == 0) return(empty)
    h <- pa_history(); hall <- pa_history_all()
    ent <- pa_count_by_item(h)
    jc <- pa_count_by_item(hall[hall$EntryType == "Job Card", , drop = FALSE])
    last <- if (nrow(h) == 0) setNames(character(0), character(0)) else
      vapply(split(h$DateOnly, h$ItemID), function(d) as.character(max(d)), character(1))
    n_of <- function(v, id) { x <- v[id]; if (is.na(x)) 0L else as.integer(x) }
    out <- data.frame(
      ItemID = items$ItemID,
      Item = vapply(seq_len(nrow(items)), function(i) item_identifier(items[i, ]), character(1)),
      Machine = ifelse(items$Machine == "", "-", items$Machine),
      Category = items$Category,
      `Sub-Category` = items$SubCategory,
      Entries = vapply(items$ItemID, function(id) n_of(ent, id), integer(1)),
      `Job Cards` = vapply(items$ItemID, function(id) n_of(jc, id), integer(1)),
      `Last Entry` = vapply(items$ItemID, function(id) { x <- last[id]; if (is.na(x)) "-" else x }, character(1)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    min_jc <- input$pa_min_jc
    if (!is.null(min_jc) && !is.na(min_jc) && min_jc > 0) out <- out[out$`Job Cards` >= min_jc, , drop = FALSE]
    out <- out[order(-out$Entries, -out$`Job Cards`, out$Item), , drop = FALSE]
    rownames(out) <- NULL
    out
  })
  pa_chart_data <- function(col) {
    a <- pa_summary()
    a$Value <- a[[col]]
    a <- a[a$Value > 0, , drop = FALSE]
    if (nrow(a) == 0) return(a)
    a <- head(a[order(-a$Value), , drop = FALSE], pa_top_n())
    a$Label <- ifelse(a$Machine == "-", a$Item, paste0(a$Item, " - ", a$Machine))
    a
  }
  output$pa_n_items <- renderText({ nrow(pa_summary()) })
  output$pa_n_entries <- renderText({ sum(pa_summary()$Entries) })
  output$pa_n_jobcards <- renderText({ sum(pa_summary()$`Job Cards`) })
  output$pa_history_plot <- renderPlotly({
    a <- pa_chart_data("Entries")
    if (nrow(a) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    p <- ggplot(a, aes(x = reorder(Label, Value), y = Value, text = paste0(Value, " entrie(s)"))) +
      geom_col(fill = "#0B4D3A") + coord_flip() + labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  output$pa_jobcard_plot <- renderPlotly({
    a <- pa_chart_data("Job Cards")
    if (nrow(a) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    p <- ggplot(a, aes(x = reorder(Label, Value), y = Value, text = paste0(Value, " job card(s)"))) +
      geom_col(fill = "#D9A400") + coord_flip() + labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  pa_table_data <- function() {
    a <- pa_summary()
    a[, setdiff(names(a), "ItemID"), drop = FALSE]
  }
  output$pa_table <- renderTable({
    a <- pa_summary()
    if (nrow(a) == 0) return(data.frame(Message = "No plant matches these filters."))
    pa_table_data()
  })
  output$pa_download <- downloadHandler(
    filename = function() paste0("pmk_plant_analysis_", Sys.Date(), ".csv"),
    content = function(file) write.csv(pa_table_data(), file, row.names = FALSE)
  )

  # -------------------------------------------------------------
  # ADMIN - control panel, not an edit surface. All data edits stay
  # inline where the data lives (Inventory List, Whereabouts,
  # Invoices); this tab is just Google Sheets sync status.
  # -------------------------------------------------------------
  # Built once per role (role() only changes on login/logout, not on
  # every data change, so this doesn't reintroduce the "typing gets
  # wiped mid-edit" bug the cards below were split out to avoid) so
  # the Plantman role sees just the Ganger List card - nothing else on
  # this tab is any of their business. Each card's changeable content
  # still lives in its own narrow uiOutput/tableOutput, so adding a
  # Company/Ganger never tears down the whole accordion.
  output$admin_tab_content <- renderUI({
    r <- role()
    panels <- list()
    if (r %in% c("Admin", "Boss")) panels[["Google Sheets Sync"]] <- accordion_panel("Google Sheets Sync", value = "Google Sheets Sync",
      div(class = "admin-card",
          p(class = "text-muted",
            if (SHEETS_SYNC_ENABLED)
              "The app is the only place data gets edited. Every change here pushes out to the Google Sheet automatically - the Sheet is a live read-only mirror, not an input."
            else
              "Sync is currently turned off (SHEETS_SYNC_ENABLED is FALSE in app.R). See the setup notes at the top of app.R to turn it on."
          ),
          fluidRow(
            column(4, metric_card(if (SHEETS_SYNC_ENABLED) "On" else "Off", "Sync Status",
                                  colour = if (SHEETS_SYNC_ENABLED) "#3E7C59" else "#5B6770")),
            column(4, metric_card(textOutput("admin_last_synced", inline = TRUE), "Last Synced")),
            column(4, div(style = "padding-top:14px;", actionButton("admin_sync_now", "Sync Now", class = "btn-primary btn-sm")))
          ),
          uiOutput("admin_sync_error_ui")
      )
    )
    if (r %in% c("Admin", "Boss")) panels[["Machines With No Driver"]] <- accordion_panel("Machines With No Driver", value = "Machines With No Driver",
      div(class = "admin-card",
          p(class = "text-muted", "Active plant with nobody currently assigned - worth double-checking these."),
          uiOutput("admin_no_driver_ui")
      )
    )
    if (r %in% c("Admin", "Boss")) panels[["Staff Activity (This Week)"]] <- accordion_panel("Staff Activity (This Week)", value = "Staff Activity (This Week)",
      div(class = "admin-card",
          p(class = "text-muted", "Quick count of History entries and Invoices logged by each person this week. For a specific week/month or person, use the 'Report by input' filter on the Reports tab."),
          tableOutput("admin_staff_activity_table")
      )
    )
    if (r %in% c("Admin", "Boss")) panels[["Company List"]] <- accordion_panel("Company List", value = "Company List",
      div(class = "admin-card",
          p(class = "text-muted", "Suppliers/garages available in the Company dropdown when adding an invoice or logging subcontractor mechanic work."),
          fluidRow(
            column(8, textInput("admin_company_new", NULL, placeholder = "e.g. Arnold Clark")),
            column(4, actionButton("admin_company_add", "+ Add", class = "btn-primary btn-sm"))
          ),
          uiOutput("admin_company_list_ui")
      )
    )
    if (GANG_FEATURES_ENABLED && r %in% c("Admin", "Boss", "Plantman")) panels[["Ganger List"]] <- accordion_panel("Ganger List", value = "Ganger List",
      div(class = "admin-card",
          p(class = "text-muted", "Names available in the Ganger dropdown when creating or editing a gang sheet."),
          fluidRow(
            column(8, textInput("admin_ganger_new", NULL, placeholder = "e.g. John Smith")),
            column(4, actionButton("admin_ganger_add", "+ Add", class = "btn-primary btn-sm"))
          ),
          uiOutput("admin_ganger_list_ui")
      )
    )
    if (r %in% c("Admin", "Boss")) panels[["Reports"]] <- accordion_panel("Reports", value = "Reports",
      div(class = "admin-card", reports_ui(r))
    )
    tagList(
      br(),
      do.call(accordion, c(list(id = "admin_accordion", open = names(panels)[1]), unname(panels)))
    )
  })
  output$admin_sync_error_ui <- renderUI({
    if (!is.null(sheets_last_error())) div(class = "alert alert-danger mt-3", sheets_last_error())
  })
  output$admin_no_driver_ui <- renderUI({
    df <- inventory_data()
    unassigned <- df[df$Active == "Yes" & (is.na(df$Driver) | df$Driver == ""), ]
    if (nrow(unassigned) == 0) div(class = "alert alert-secondary mb-0", "Every active item has a driver assigned.")
    else tagList(
      div(class = "alert alert-warning", paste0(nrow(unassigned), " active item(s) with no driver.")),
      tableOutput("admin_no_driver_table")
    )
  })
  output$admin_company_list_ui <- renderUI({
    if (length(company_list()) == 0) p(class = "text-muted mb-0", "No companies added yet.")
    else tagList(lapply(company_list(), function(nm) {
      div(class = "d-flex justify-content-between align-items-center", style = "padding:4px 0; border-bottom:1px solid #eee;",
          span(nm),
          tags$a(href = "#", style = "font-size:0.85rem; color:#9C2B2B;",
                 onclick = sprintf("Shiny.setInputValue('delete_company_click', '%s', {priority:'event'}); return false;", js_escape_sq(nm)),
                 "Delete")
      )
    }))
  })
  output$admin_ganger_list_ui <- renderUI({
    if (length(ganger_list()) == 0) p(class = "text-muted mb-0", "No gangers added yet.")
    else tagList(lapply(ganger_list(), function(nm) {
      div(class = "d-flex justify-content-between align-items-center", style = "padding:4px 0; border-bottom:1px solid #eee;",
          span(nm),
          tags$a(href = "#", style = "font-size:0.85rem; color:#9C2B2B;",
                 onclick = sprintf("Shiny.setInputValue('delete_ganger_click', '%s', {priority:'event'}); return false;", js_escape_sq(nm)),
                 "Delete")
      )
    }))
  })
  output$admin_last_synced <- renderText({
    t <- sheets_last_synced()
    if (is.null(t)) "Never" else format(t, "%d %b %H:%M")
  })
  output$admin_no_driver_table <- renderTable({
    df <- inventory_data()
    unassigned <- df[df$Active == "Yes" & (is.na(df$Driver) | df$Driver == ""), ]
    if (nrow(unassigned) == 0) return(data.frame(Message = "Every active item has a driver assigned."))
    unassigned <- natural_sort_rows(unassigned)
    out <- unassigned %>% transmute(Item = ifelse(Machine == "", ItemID, Machine),
                                    `PMK/Reg` = ifelse(PMK_Number != "", PMK_Number, Registration),
                                    Category, `Sub-Category` = SubCategory,
                                    Gang = ifelse(Gang == "", "Not assigned", Gang))
    if (!GANG_FEATURES_ENABLED) out$Gang <- NULL
    out
  })
  output$admin_staff_activity_table <- renderTable({
    week_s <- floor_to_monday(Sys.Date())
    week_e <- week_s + 6
    h <- plant_history()
    h_counts <- data.frame(Person = character(0), `History Entries` = integer(0), check.names = FALSE)
    if (nrow(h) > 0) {
      h$DateOnly <- as.Date(substr(h$DateTime, 1, 10))
      h <- h[!is.na(h$DateOnly) & h$DateOnly >= week_s & h$DateOnly <= week_e & h$RecordedBy != "", ]
      if (nrow(h) > 0) h_counts <- h %>% group_by(Person = RecordedBy) %>% summarise(`History Entries` = n(), .groups = "drop")
    }
    inv_d <- invoices_data()
    i_counts <- data.frame(Person = character(0), Invoices = integer(0))
    if (nrow(inv_d) > 0) {
      inv_d$DateParsed <- suppressWarnings(as.Date(inv_d$Date))
      inv_d <- inv_d[!is.na(inv_d$DateParsed) & inv_d$DateParsed >= week_s & inv_d$DateParsed <= week_e &
                       !is.na(inv_d$LoggedBy) & inv_d$LoggedBy != "", ]
      if (nrow(inv_d) > 0) i_counts <- inv_d %>% group_by(Person = LoggedBy) %>% summarise(Invoices = n(), .groups = "drop")
    }
    out <- full_join(h_counts, i_counts, by = "Person")
    if (nrow(out) == 0) return(data.frame(Message = "Nothing logged by anyone yet this week."))
    out$`History Entries`[is.na(out$`History Entries`)] <- 0
    out$Invoices[is.na(out$Invoices)] <- 0
    out %>% mutate(Total = `History Entries` + Invoices) %>% arrange(desc(Total)) %>% select(-Total)
  })
  observeEvent(input$admin_sync_now, {
    if (!SHEETS_SYNC_ENABLED) { showNotification("Sync is turned off - see the setup notes at the top of app.R.", type = "warning"); return() }
    run_full_sync()
    if (is.null(sheets_last_error())) showNotification("Synced to Google Sheets.", type = "message")
    else showNotification(sheets_last_error(), type = "error")
  })
  observeEvent(input$admin_ganger_add, {
    nm <- trimws(input$admin_ganger_new)
    req(nm, nm != "")
    if (nm %in% ganger_list()) { showNotification("That name is already on the Ganger list.", type = "error"); return() }
    ganger_list(sort(c(ganger_list(), nm)))
    updateTextInput(session, "admin_ganger_new", value = "")
    showNotification(paste0("Added '", nm, "' to the Ganger list."), type = "message")
  })
  observeEvent(input$delete_ganger_click, {
    nm <- input$delete_ganger_click
    ganger_list(setdiff(ganger_list(), nm))
    showNotification(paste0("'", nm, "' removed from the Ganger list."), type = "message")
  })
  observeEvent(input$admin_company_add, {
    nm <- trimws(input$admin_company_new)
    req(nm, nm != "")
    if (nm %in% company_list()) { showNotification("That company is already on the list.", type = "error"); return() }
    company_list(sort(c(company_list(), nm)))
    updateTextInput(session, "admin_company_new", value = "")
    showNotification(paste0("Added '", nm, "' to the Company list."), type = "message")
  })
  observeEvent(input$delete_company_click, {
    nm <- input$delete_company_click
    company_list(setdiff(company_list(), nm))
    showNotification(paste0("'", nm, "' removed from the Company list."), type = "message")
  })
  # -------------------------------------------------------------
  # NOTIFICATIONS - Admin-only activity feed (see log_notification()
  # above). Deliberately no clear/delete UI yet - kept as a running
  # log for now, tidied up manually via the Google Sheet.
  # -------------------------------------------------------------
  output$notifications_tab_content <- renderUI({ notifications_ui() })
  notifications_ui <- function() {
    tagList(
      br(),
      fluidRow(
        column(8, p(class = "text-muted", "What non-Admin logins have been doing - newest first.")),
        column(4, style = "text-align:right;", downloadButton("notifications_download", "Download (CSV)", class = "btn-outline-secondary btn-sm"))
      ),
      tableOutput("notifications_table")
    )
  }
  notifications_sorted <- reactive({
    n <- notifications_log()
    if (nrow(n) == 0) return(n)
    n[order(n$Time, decreasing = TRUE), ]
  })
  output$notifications_table <- renderTable({
    n <- notifications_sorted()
    if (nrow(n) == 0) return(data.frame(Message = "No activity logged yet."))
    n
  })
  output$notifications_download <- downloadHandler(
    filename = function() paste0("pmk_notifications_", Sys.Date(), ".csv"),
    content = function(file) write.csv(notifications_sorted(), file, row.names = FALSE)
  )
}
shinyApp(ui, server)