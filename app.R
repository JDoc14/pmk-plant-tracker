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
#   3. Do this for: PMK_LOGIN_SEAN, PMK_LOGIN_JACK,
#      PMK_LOGIN_MECHANIC1, PMK_LOGIN_KEVIN, PMK_LOGIN_AGENT1,
#      PMK_LOGIN_GUEST. Roles are Admin / Admin / Mechanic / Kevin /
#      Agent / Guest respectively.
# For local testing (RStudio on your own computer), create a file
# called credentials_local.R next to app.R (it's in .gitignore, so it
# never gets committed) defining a `credentials` data.frame with the
# same user/password/role/name columns as before.
# ---------------------------------------------------------------
build_credentials_from_env <- function() {
  users <- c("sean", "jack", "mechanic1", "kevin", "agent1", "guest")
  rows <- lapply(users, function(u) {
    raw <- Sys.getenv(paste0("PMK_LOGIN_", toupper(u)), unset = "")
    if (!nzchar(raw)) return(NULL)
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
ENTRY_TYPES <- c("Driver Assigned", "Hours Updated", "Damage", "Refurbished", "Mechanic Work", "Note", "Service Inspection", "Job Card", "Truck Service")
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
# ---------------------------------------------------------------
# COMPANY LIST - suppliers/garages used for the invoice Company
# dropdown. Add or remove names here as your supplier list changes.
# ---------------------------------------------------------------
COMPANY_LIST <- c(
  "Arnold Clark", "Astrak", "Dingbro", "Hydraquip", "HTS Spares", "Hydralink",
  "Indespension", "Zenith", "Jepson & Co", "Motus Commercials", "MV Commercial",
  "NAPA", "Quickshift", "Redpath Tyres", "Scot JCB", "Rudden Mechanicals",
  "Briggs Equipment", "HRN", "Netherton Tractors", "LGH Winches", "McNicoll",
  "MPG", "Palfinger UK", "Pirtek", "Scotia", "Stirling Trailer Centre",
  "Logistics UK", "Finning CAT", "West of Scotland Engineering", "Scania",
  "Kaen Yuill Recovery", "Emtelle", "McCluskey Glazing", "Euro",
  "A M Philips Trucktech"
)
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
                    "DatePurchased", "WarrantyEndDate", "MOTDue", "Active", "OnHire", "Notes",
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
item_row <- function(row, r, clickable = TRUE, show_actions = TRUE) {
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
            span(class = paste0("badge ", ifelse(row$Active == "Yes", "bg-success", "bg-secondary")), row$Active),
            " ",
            if (row$OnHire == "Yes") span(class = "badge bg-warning", "On Hire"),
            if (r == "Admin" && show_actions) div(style = "margin-top:6px;",
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
nested_inventory_accordion <- function(base_id, df, r, show_actions = TRUE, clickable = TRUE) {
  cat_panels <- lapply(CATEGORY_OPTIONS, function(cat) {
    cat_rows <- df[df$Category == cat, ]
    subcats <- subcats_for(cat, df)
    sub_panels <- lapply(subcats, function(sub) {
      sub_rows <- natural_sort_rows(cat_rows[cat_rows$SubCategory == sub, ])
      accordion_panel(
        title = paste0(sub, " (", nrow(sub_rows), ")"),
        value = paste0(cat, "___", sub),
        if (nrow(sub_rows) == 0) p(class = "text-muted mb-0", "None.")
        else tagList(lapply(seq_len(nrow(sub_rows)), function(i) item_row(sub_rows[i, ], r, clickable = clickable, show_actions = show_actions)))
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
  editing_item <- reactiveVal(NULL)
  editing_gang <- reactiveVal(NULL)
  editing_invoice <- reactiveVal(NULL)
  editing_history_entry <- reactiveVal(NULL)
  inv <- reactive({ invoices_data() %>% mutate(DateParsed = as.Date(Date)) })
  # ---- Google Sheets sync ----
  # One-way, app -> Sheet. Debounced so a burst of edits (e.g.
  # ticking 10 gang checkboxes) becomes one write, not ten.
  sheets_last_synced <- reactiveVal(NULL)
  sheets_last_error <- reactiveVal(NULL)
  inventory_debounced <- debounce(inventory_data, 4000)
  invoices_debounced <- debounce(invoices_data, 4000)
  history_debounced <- debounce(plant_history, 4000)
  ganger_debounced <- debounce(ganger_list, 4000)
  gang_meta_debounced <- debounce(gang_meta, 4000)
  run_full_sync <- function() {
    ok <- c(
      sync_to_sheets(inventory_data(), "Inventory"),
      sync_to_sheets(invoices_data(), "Invoices"),
      sync_to_sheets(plant_history(), "Plant History"),
      sync_to_sheets(data.frame(Name = ganger_list(), stringsAsFactors = FALSE), "Gangers"),
      sync_to_sheets(gang_meta(), "GangMeta")
    )
    if (all(ok)) { sheets_last_synced(Sys.time()); sheets_last_error(NULL) }
    else sheets_last_error(paste0("Sync failed at ", format(Sys.time(), "%H:%M:%S"), " - check the R console for details."))
  }
  observeEvent(inventory_debounced(), {
    if (!SHEETS_SYNC_ENABLED) return()
    ok <- sync_to_sheets(inventory_debounced(), "Inventory")
    if (ok) sheets_last_synced(Sys.time()) else sheets_last_error(paste0("Inventory sync failed at ", format(Sys.time(), "%H:%M:%S")))
  }, ignoreInit = TRUE)
  observeEvent(invoices_debounced(), {
    if (!SHEETS_SYNC_ENABLED) return()
    ok <- sync_to_sheets(invoices_debounced(), "Invoices")
    if (ok) sheets_last_synced(Sys.time()) else sheets_last_error(paste0("Invoices sync failed at ", format(Sys.time(), "%H:%M:%S")))
  }, ignoreInit = TRUE)
  observeEvent(history_debounced(), {
    if (!SHEETS_SYNC_ENABLED) return()
    ok <- sync_to_sheets(history_debounced(), "Plant History")
    if (ok) sheets_last_synced(Sys.time()) else sheets_last_error(paste0("Plant History sync failed at ", format(Sys.time(), "%H:%M:%S")))
  }, ignoreInit = TRUE)
  observeEvent(ganger_debounced(), {
    if (!SHEETS_SYNC_ENABLED) return()
    ok <- sync_to_sheets(data.frame(Name = ganger_debounced(), stringsAsFactors = FALSE), "Gangers")
    if (ok) sheets_last_synced(Sys.time()) else sheets_last_error(paste0("Ganger list sync failed at ", format(Sys.time(), "%H:%M:%S")))
  }, ignoreInit = TRUE)
  observeEvent(gang_meta_debounced(), {
    if (!SHEETS_SYNC_ENABLED) return()
    ok <- sync_to_sheets(gang_meta_debounced(), "GangMeta")
    if (ok) sheets_last_synced(Sys.time()) else sheets_last_error(paste0("Gang sheet details sync failed at ", format(Sys.time(), "%H:%M:%S")))
  }, ignoreInit = TRUE)
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
    tabs[["Inventory List"]] <- uiOutput("inventory_tab_content")
    if (r %in% c("Admin", "Mechanic", "Agent")) tabs[["Plant Whereabouts"]] <- uiOutput("whereabouts_tab_content")
    if (r %in% c("Admin", "Kevin")) tabs[["Invoices"]] <- uiOutput("invoices_tab_content")
    if (r %in% c("Admin", "Kevin")) tabs[["Reports"]] <- uiOutput("reports_tab_content")
    if (r %in% c("Admin", "Mechanic")) tabs[["Job Cards & Inspections"]] <- uiOutput("jobcards_tab_content")
    if (r == "Admin") tabs[["Admin"]] <- uiOutput("admin_tab_content")
    do.call(tabsetPanel, c(
      list(id = "main_tabs", selected = "Home"),
      lapply(names(tabs), function(nm) tabPanel(nm, tabs[[nm]])),
      list(type = "tabs")
    ))
  })
  # -------------------------------------------------------------
  # HOME PAGE - big centred logo, snapshot, quick links
  # -------------------------------------------------------------
  output$home_tab_content <- renderUI({
    r <- role()
    df <- inventory_data()
    n_plant <- nrow(df); n_active <- sum(df$Active == "Yes"); n_hire <- sum(df$OnHire == "Yes")
    n_gangs <- length(gang_list())
    n_invoices_home <- if (r %in% c("Admin", "Kevin")) nrow(invoices_data()) else NA
    due_soon <- due_within(30)
    n_due_soon <- nrow(due_soon)
    ts_due <- truck_service_due(7)
    n_ts_due <- nrow(ts_due)
    quick_links <- c("Inventory List")
    if (r %in% c("Admin", "Mechanic", "Agent")) quick_links <- c(quick_links, "Plant Whereabouts")
    if (r %in% c("Admin", "Kevin")) quick_links <- c(quick_links, "Invoices")
    if (r %in% c("Admin", "Kevin")) quick_links <- c(quick_links, "Reports")
    if (r %in% c("Admin", "Mechanic")) quick_links <- c(quick_links, "Job Cards & Inspections")
    if (r == "Admin") quick_links <- c(quick_links, "Admin")
    tagList(
      div(class = "hero-logo",
          tags$img(src = "pmk_logo.webp"),
          h3(class = "hero-title", "PMK Civil Engineering")
      ),
      h4(paste0("Welcome, ", user_name(), "."), style = "text-align:center;"),
      p(class = "text-muted", style = "text-align:center;", paste0("Logged in as ", r, ". Here's a quick snapshot.")),
      br(),
      fluidRow(
        column(3, metric_card(n_plant, "Plant Items")),
        column(3, metric_card(n_active, "Active", colour = "#3E7C59")),
        column(3, metric_card(n_hire, "On Hire", colour = "#C9A227")),
        column(3, metric_card(n_due_soon, "MOT/Warranty Due (30 days)", colour = if (n_due_soon > 0) "#9C2B2B" else "#3E7C59"))
      ),
      if (r %in% c("Admin", "Kevin")) fluidRow(style = "margin-top:10px;",
                                               column(3, offset = 6, metric_card(n_ts_due, "Truck Service Due (7 days)", colour = if (n_ts_due > 0) "#9C2B2B" else "#3E7C59")),
                                               column(3, metric_card(n_invoices_home, "Invoices Logged"))
      ) else fluidRow(style = "margin-top:10px;",
                      column(3, offset = 6, metric_card(n_ts_due, "Truck Service Due (7 days)", colour = if (n_ts_due > 0) "#9C2B2B" else "#3E7C59")),
                      column(3, metric_card(n_gangs, "Gangs"))
      ),
      if (n_due_soon > 0) tagList(
        br(),
        h6("Due for MOT or Warranty in the next 30 days", style = "text-align:center;"),
        div(class = "chart-card", tableOutput("home_due_table"))
      ),
      if (n_ts_due > 0) tagList(
        br(),
        h6("Truck Service Due Within 7 Days", style = "text-align:center;"),
        div(class = "chart-card", tableOutput("home_truckservice_table"))
      ),
      if (r %in% c("Admin", "Kevin")) tagList(
        br(),
        h6("Invoice Highlights", style = "text-align:center;"),
        div(class = "chart-card", tableOutput("home_invoice_highlights"))
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
    d <- truck_service_due(7)
    if (nrow(d) == 0) return(data.frame(Message = "Nothing due within 7 days."))
    d %>% transmute(Item = ifelse(Machine == "", ItemID, Machine),
                    `PMK/Reg` = ifelse(PMK_Number != "", PMK_Number, Registration),
                    `Last Serviced` = LastServiced, `Due Date` = as.character(DueDate), Status)
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
    lapply(c("Inventory List", "Plant Whereabouts", "Invoices", "Reports", "Admin"), function(tab_name) {
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
  inventory_list_ui <- function(r) {
    df <- inventory_data()
    tagList(
      br(),
      fluidRow(
        column(8, p(class = "text-muted",
                    if (r == "Admin") "Click a category, then a sub-category, to find an item. Click an item for its full history, or use Edit/Delete."
                    else if (r == "Mechanic") "Click a category, then a sub-category, to find an item and log history."
                    else "Click a category, then a sub-category, to view item details."
        )),
        column(4, style = "text-align:right;",
               if (r == "Admin") actionButton("add_item_btn", "+ Add New Item", class = "btn-primary btn-sm"))
      ),
      if (nrow(df) == 0) div(class = "alert alert-secondary", "No plant items yet - add some above.")
      else nested_inventory_accordion("inv_accordion", df, r, show_actions = TRUE, clickable = TRUE)
    )
  }
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
              if (r == "Admin") div(
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
                   p(strong("Gang: "), ifelse(row$Gang == "", "Not assigned", row$Gang))
            ),
            column(4,
                   p(strong("Date Purchased: "), ifelse(row$DatePurchased == "", "-", row$DatePurchased)),
                   p(strong("Warranty End: "), ifelse(row$WarrantyEndDate == "", "-", row$WarrantyEndDate)),
                   p(strong("MOT Due: "), ifelse(row$MOTDue == "", "-", row$MOTDue))
            )
          ),
          p(strong("Active: "), row$Active, " | ", strong("On Hire: "), row$OnHire),
          p(strong("Notes: "), ifelse(row$Notes == "", "-", row$Notes)),
          if (r %in% c("Admin", "Mechanic")) actionButton("inv_add_entry_btn", "+ Add History Entry", class = "btn-primary btn-sm")
      ),
      h5("History"),
      if (nrow(hist) == 0) div(class = "alert alert-info", "No history yet for this item.")
      else tagList(lapply(seq_len(nrow(hist)), function(i) {
        h <- hist[i, ]
        linked_id <- if (!is.na(h$EntryID) && h$EntryID != "") linked_entry_id_for(h$EntryID, plant_history()) else NA_character_
        linked_row <- if (!is.na(linked_id)) plant_history()[plant_history()$EntryID == linked_id, ] else plant_history()[0, ]
        div(class = "history-item", style = if (nrow(linked_row) > 0) "border-left-color:#C9A227;" else NULL,
            div(class = "d-flex justify-content-between align-items-start flex-wrap",
                div(strong(h$EntryType), span(class = "text-muted", paste0(" - ", h$DateTime))),
                if (r %in% c("Admin", "Mechanic") && !is.na(h$EntryID) && h$EntryID != "") div(
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
                                        selectizeInput("ih_sub_company", "Subcontractor Company *", choices = COMPANY_LIST,
                                                       options = list(create = TRUE, placeholder = "Select or type a company name")),
                                        numericInput("ih_sub_amount", "Amount (£) *", value = NA)
                       )
      ),
      # ---- Service Inspection: mirrors Form 32 ----
      conditionalPanel("input.ih_type == 'Service Inspection'",
                       fluidRow(
                         column(6, textInput("sv_next_due", "Next Inspection Due", placeholder = "e.g. March 2026")),
                         column(6, textInput("sv_reviewed_by", "Reviewed By"))
                       ),
                       fluidRow(
                         column(6, dateInput("sv_date_in", "Date In", value = Sys.Date())),
                         column(6, dateInput("sv_date_out", "Date Out", value = Sys.Date()))
                       ),
                       p(class = "text-muted", "Everything defaults to serviceable - untick anything that failed or needs attention."),
                       tagList(lapply(names(SERVICE_CHECKLIST), function(sec) {
                         tagList(
                           strong(sec),
                           checkboxGroupInput(paste0("sv_chk_", make.names(sec)), NULL,
                                              choices = SERVICE_CHECKLIST[[sec]], selected = SERVICE_CHECKLIST[[sec]])
                         )
                       })),
                       textAreaInput("sv_defects", "Defects Found", rows = 2, placeholder = "Optional - only needed if something's unticked above"),
                       textInput("sv_rectified_by", "Rectified By", placeholder = "Optional")
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
                       textInput("ts_rectified_by", "Rectified By", placeholder = "Optional")
      ),
      hr(),
      p(class = "text-muted mb-1", "Optional, applies to any entry type."),
      selectInput("ih_link", "Link to another entry for this item", choices = link_choices,
                  selected = ""),
      fluidRow(
        column(6, textInput("ih_location", "Location")),
        column(6, numericInput("ih_price", "Price (£)", value = NA))
      ),
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
  build_service_desc <- function() {
    lines <- c(
      paste0("Next Inspection Due: ", ifelse(is.null(input$sv_next_due) || input$sv_next_due == "", "-", input$sv_next_due)),
      paste0("Date In: ", as.character(input$sv_date_in), "  |  Date Out: ", as.character(input$sv_date_out))
    )
    failed <- c()
    for (sec in names(SERVICE_CHECKLIST)) {
      all_items <- SERVICE_CHECKLIST[[sec]]
      ticked <- input[[paste0("sv_chk_", make.names(sec))]]
      not_ticked <- setdiff(all_items, ticked)
      if (length(not_ticked) > 0) failed <- c(failed, paste0(sec, ": ", paste(not_ticked, collapse = ", ")))
    }
    if (length(failed) > 0) {
      lines <- c(lines, "Items flagged (not serviceable):", paste0("  - ", failed))
    } else {
      lines <- c(lines, "All 22 checklist items serviceable.")
    }
    if (!is.null(input$sv_defects) && trimws(input$sv_defects) != "") lines <- c(lines, paste0("Defects Found: ", input$sv_defects))
    if (!is.null(input$sv_rectified_by) && trimws(input$sv_rectified_by) != "") lines <- c(lines, paste0("Rectified By: ", input$sv_rectified_by))
    if (!is.null(input$sv_reviewed_by) && trimws(input$sv_reviewed_by) != "") lines <- c(lines, paste0("Reviewed By: ", input$sv_reviewed_by))
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
      fluidRow(
        column(4, textInput("if_datepurch", "Date Purchased", value = g("DatePurchased"), placeholder = "DD/MM/YY")),
        column(4, textInput("if_warranty", "Warranty End Date", value = g("WarrantyEndDate"), placeholder = "DD/MM/YY")),
        column(4, textInput("if_mot", "MOT Due", value = g("MOTDue"), placeholder = "DD/MM/YY"))
      ),
      fluidRow(
        column(6, selectInput("if_active", "Active", choices = c("Yes", "No"), selected = g("Active", "Yes"))),
        column(6, selectInput("if_onhire", "On Hire", choices = c("Yes", "No"), selected = g("OnHire", "No")))
      ),
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
      Active = input$if_active, OnHire = input$if_onhire, Notes = input$if_notes,
      TruckServiceRequired = input$if_truckservice,
      stringsAsFactors = FALSE
    )
    df <- inventory_data()
    if (is.null(editing_item())) {
      row_data$ItemID <- next_item_id()
      row_data$Gang <- ""
      df <- bind_rows(df, row_data)
      showNotification("Item added.", type = "message")
    } else {
      iid <- editing_item()
      keep_cols <- c("ItemID", "Gang")
      existing <- df[df$ItemID == iid, keep_cols]
      row_data$ItemID <- existing$ItemID
      row_data$Gang <- existing$Gang
      df <- df[df$ItemID != iid, ]
      df <- bind_rows(df, row_data)
      showNotification("Item updated.", type = "message")
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
    df <- inventory_data(); df <- df[df$ItemID != iid, ]; inventory_data(df)
    if (identical(inv_selected(), iid)) { inv_view("list"); inv_selected(NULL) }
    removeModal()
    showNotification("Item removed.", type = "message")
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
        column(8, h5("Gang Sheets")),
        column(4, style = "text-align:right;",
               if (r == "Admin") actionButton("new_gang_sheet_btn", "+ Create New Gang Sheet", class = "btn-primary btn-sm"))
      ),
      if (length(gang_list()) == 0) div(class = "alert alert-secondary", "No gang sheets yet.")
      else {
        gang_panels <- lapply(gang_list(), function(g) {
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
            if (r == "Admin") div(class = "mb-2",
                tags$a(href = "#", style = "font-size:0.85rem; margin-right:12px;",
                       onclick = sprintf("Shiny.setInputValue('edit_gang_click', '%s', {priority:'event'}); return false;", g),
                       "Edit"),
                tags$a(href = "#", style = "font-size:0.85rem; color:#9C2B2B;",
                       onclick = sprintf("Shiny.setInputValue('delete_gang_click', '%s', {priority:'event'}); return false;", g),
                       "Delete")
            ),
            if (nrow(g_rows) == 0) p(class = "text-muted mb-0", "No plant assigned.")
            else tagList(lapply(seq_len(nrow(g_rows)), function(i) item_row(g_rows[i, ], r, clickable = FALSE, show_actions = TRUE)))
          )
        })
        do.call(accordion, c(list(id = "gang_sheets_accordion", open = FALSE), gang_panels))
      },
      br(),
      h5("Unassigned Plant"),
      p(class = "text-muted", "Available to add to a gang sheet."),
      {
        leftover <- df[df$Gang == "" | is.na(df$Gang), ]
        if (nrow(leftover) == 0) div(class = "alert alert-secondary", "Everything is assigned to a gang.")
        else nested_inventory_accordion("unassigned_accordion", leftover, r, show_actions = TRUE, clickable = FALSE)
      }
    )
  }
  gang_sheet_form <- function(pool_df, selected_ids = character(0)) {
    lapply(CATEGORY_OPTIONS, function(cat) {
      cat_rows <- pool_df[pool_df$Category == cat, ]
      subcats <- subcats_for(cat, pool_df)
      sub_panels <- lapply(subcats, function(sub) {
        sub_rows <- natural_sort_rows(cat_rows[cat_rows$SubCategory == sub, ])
        accordion_panel(
          title = paste0(sub, " (", nrow(sub_rows), " available)"),
          value = paste0(cat, "___", sub),
          if (nrow(sub_rows) == 0) p(class = "text-muted mb-0", "None available.")
          else checkboxGroupInput(paste0("gang_form_", make.names(cat), "_", make.names(sub)), NULL,
                                  choices = setNames(sub_rows$ItemID, paste0(ifelse(sub_rows$Machine == "", "(no machine name)", sub_rows$Machine), " - ", vapply(seq_len(nrow(sub_rows)), function(i) item_identifier(sub_rows[i, ]), character(1)))),
                                  selected = intersect(sub_rows$ItemID, selected_ids))
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
      input[[paste0("gang_form_", make.names(cat), "_", make.names(sub))]]
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
    removeModal()  # ensure any stale modal is torn down before opening a new one
    showModal(modalDialog(
      title = "Create New Gang Sheet", size = "l",
      fluidRow(
        column(6, textInput("gang_form_name", "Gang Name", placeholder = "e.g. Gang D")),
        column(6, selectInput("gang_form_ganger", "Ganger (optional)", choices = ganger_choices(), selected = ""))
      ),
      textInput("gang_form_location", "Location (optional)", placeholder = "e.g. Site name / postcode"),
      p(class = "text-muted", "Tick which unassigned plant belongs to this gang."),
      do.call(accordion, c(list(id = "gang_form_accordion", open = TRUE), gang_sheet_form(unassigned))),
      footer = tagList(modalButton("Cancel"), actionButton("gang_form_submit_new", "Create Gang Sheet", class = "btn-primary"))
    ))
  })
  observeEvent(input$edit_gang_click, {
    g <- input$edit_gang_click
    editing_gang(g)
    df <- inventory_data()
    pool <- df[df$Gang == g | df$Gang == "" | is.na(df$Gang), ]
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
      p(class = "text-muted", "Tick which plant belongs to this gang. Plant assigned to other gangs isn't shown here."),
      do.call(accordion, c(list(id = "gang_form_accordion_edit", open = TRUE), gang_sheet_form(pool, current))),
      footer = tagList(modalButton("Cancel"), actionButton("gang_form_submit_edit", "Save Gang Sheet", class = "btn-primary"))
    ))
  })
  save_gang_meta <- function(name, ganger, location) {
    gm <- gang_meta()
    gm <- gm[gm$Gang != name, ]
    gm <- bind_rows(gm, data.frame(Gang = name, Ganger = ganger, Location = trimws(location), stringsAsFactors = FALSE))
    gang_meta(gm)
  }
  observeEvent(input$gang_form_submit_new, {
    name <- trimws(input$gang_form_name)
    req(name, name != "")
    if (name %in% gang_list()) { showNotification("That gang name already exists - use Edit instead.", type = "error"); return() }
    ticked <- collect_ticked_items()
    ganger <- trimws(input$gang_form_ganger)
    df <- inventory_data()
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
    removeModal()
    showNotification(paste0("Gang sheet '", name, "' created with ", length(ticked), " item(s)."), type = "message")
  })
  observeEvent(input$gang_form_submit_edit, {
    name <- editing_gang(); req(name)
    ticked <- collect_ticked_items()
    ganger <- trimws(input$gang_form_ganger_edit)
    df <- inventory_data()
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
    removeModal(); editing_gang(NULL)
    showNotification(paste0("Gang sheet '", name, "' updated."), type = "message")
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
    tagList(
      br(),
      p(class = "text-muted", if (r == "Admin") "Add and view invoices." else "View access - Kevin's role."),
      fluidRow(
        column(8, NULL),
        column(4, style = "text-align:right;",
               if (r == "Admin") actionButton("add_invoice_btn", "+ Add Invoice", class = "btn-primary btn-sm"))
      ),
      tabsetPanel(
        type = "pills",
        tabPanel("Overview", overview_ui()),
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
                if (r == "Admin") tags$a(href = "#", style = "font-size:0.85rem; margin-left:12px;",
                                         onclick = sprintf("Shiny.setInputValue('edit_invoice_click', '%s', {priority:'event'}); return false;", row$InvoiceID),
                                         "Edit"),
                if (r == "Admin") tags$a(href = "#", style = "font-size:0.85rem; color:#9C2B2B; margin-left:10px;",
                                         onclick = sprintf("Shiny.setInputValue('delete_invoice_click', '%s', {priority:'event'}); return false;", row$InvoiceID),
                                         "Delete")
            )
        ),
        if (!is.na(row$Description) && row$Description != "") p(class = "mb-1 mt-2", em(row$Description)),
        div(
          tag_or_na("Category", row$Category), tag_or_na("Sub-Category", row$SubCategory),
          tag_or_na("Invoice No.", row$Invoice_Number), tag_or_na("Account No.", row$Account_Number),
          tag_or_na("Document No.", row$Document_Number), tag_or_na("SPEN/Order No.", row$SPEN_Order_Number),
          tag_or_na("Logged By", row$LoggedBy)
        )
    )
  }
  output$invoice_cards <- renderUI({
    df <- invoices_data()
    if (nrow(df) == 0) return(div(class = "alert alert-secondary", "No invoices logged yet."))
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
    company_choices <- COMPANY_LIST
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
      fluidRow(
        column(6, textInput("ni_invoice_number", "Invoice Number", value = g("Invoice_Number"))),
        column(6, textInput("ni_account_number", "Account Number", value = g("Account_Number")))
      ),
      fluidRow(
        column(6, textInput("ni_document_number", "Document Number", value = g("Document_Number"))),
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
      Account_Number = input$ni_account_number,
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
      if (!is.null(input$ni_account_number) && trimws(input$ni_account_number) != "") inv_desc_lines <- c(inv_desc_lines, paste0("Account Number: ", trimws(input$ni_account_number)))
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
  output$total_spend_val <- renderText({ dollar(sum(inv()$Amount, na.rm = TRUE), prefix = "£") })
  output$n_invoices_val <- renderText({ nrow(inv()) })
  output$avg_invoice_val <- renderText({
    d <- inv()
    if (nrow(d) == 0) "£0.00" else dollar(mean(d$Amount, na.rm = TRUE), prefix = "£")
  })
  output$monthly_trend_plot <- renderPlotly({
    d <- inv()
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
    d <- inv()
    if (nrow(d) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    c_data <- d %>% group_by(Company) %>% summarise(Total = sum(Amount, na.rm = TRUE)) %>% arrange(desc(Total)) %>% head(10)
    p <- ggplot(c_data, aes(x = reorder(Company, Total), y = Total, text = paste0("£", round(Total, 2)))) +
      geom_col(fill = "#3E7C59") + coord_flip() + scale_y_continuous(labels = label_dollar(prefix = "£")) +
      labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  output$company_plot <- renderPlotly({
    d <- inv()
    if (nrow(d) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
    c_data <- d %>% group_by(Company) %>% summarise(Total = sum(Amount, na.rm = TRUE)) %>% arrange(desc(Total))
    p <- ggplot(c_data, aes(x = reorder(Company, Total), y = Total, text = paste0("£", round(Total, 2)))) +
      geom_col(fill = "#5B6770") + coord_flip() + scale_y_continuous(labels = label_dollar(prefix = "£")) +
      labs(x = NULL, y = NULL) + gg_theme
    ggplotly(p, tooltip = "text")
  })
  output$company_table <- renderTable({
    d <- inv()
    if (nrow(d) == 0) return(data.frame(Message = "No invoices logged yet."))
    d %>% group_by(Company) %>%
      summarise(`Total Spend (£)` = sprintf("%.2f", sum(Amount, na.rm = TRUE)),
                `Invoices` = n(), `Avg Invoice (£)` = sprintf("%.2f", mean(Amount, na.rm = TRUE))) %>%
      arrange(desc(`Total Spend (£)`))
  })
  output$download_all_csv <- downloadHandler(
    filename = function() paste0("pmk_invoices_", Sys.Date(), ".csv"),
    content = function(file) write.csv(invoices_data(), file, row.names = FALSE)
  )
  # -------------------------------------------------------------
  # REPORTS - Weekly and Monthly snapshots pulled from Inventory
  # and Invoices. Admin/Kevin only, same access as Invoices.
  # -------------------------------------------------------------
  output$reports_tab_content <- renderUI({ reports_ui(role()) })
  reports_ui <- function(r) {
    tagList(
      br(),
      p(class = "text-muted", "Snapshots pulled live from Inventory and Invoices - nothing to maintain separately."),
      tabsetPanel(
        type = "pills",
        tabPanel("Weekly Report", weekly_report_ui()),
        tabPanel("Monthly Report", monthly_report_ui()),
        tabPanel("Invoice Analysis", invoice_analysis_ui())
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
        column(4, div(style = "margin-top:24px;", downloadButton("wr_download", "Download Weekly Invoices (CSV)", class = "btn-primary btn-sm")))
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
        column(4, div(style = "margin-top:24px;", downloadButton("mr_download", "Download Monthly Invoices (CSV)", class = "btn-primary btn-sm")))
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
    d <- inv()
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
      summarise(Total = n(), Active = sum(Active == "Yes"), Inactive = sum(Active == "No"), `On Hire` = sum(OnHire == "Yes"))
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
        "Green = Service Inspection logged that week, yellow = Job Card, split square = both. Last 52 weeks. ",
        "Filter by Category/Sub-Category to keep the list manageable, or download the full log below."),
      fluidRow(
        column(4, selectInput("jg_category", "Category", choices = c("All", CATEGORY_OPTIONS), selected = "All")),
        column(4, selectizeInput("jg_subcategory", "Sub-Category", choices = "All", selected = "All")),
        column(4, div(style = "margin-top:24px;", downloadButton("jg_download", "Download (CSV)", class = "btn-primary btn-sm")))
      ),
      div(class = "chart-card", style = "overflow-x:auto;", uiOutput("jg_grid"))
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
  jg_filtered_items <- reactive({
    df <- inventory_data()
    if (!is.null(input$jg_category) && input$jg_category != "All") df <- df[df$Category == input$jg_category, ]
    if (!is.null(input$jg_subcategory) && input$jg_subcategory != "All") df <- df[df$SubCategory == input$jg_subcategory, ]
    natural_sort_rows(df)
  })
  jg_weeks <- reactive({
    this_week <- floor_to_monday(Sys.Date())
    rev(seq(this_week, by = "-1 week", length.out = 52))
  })
  jg_events <- reactive({
    h <- plant_history()
    h <- h[h$EntryType %in% c("Service Inspection", "Job Card"), ]
    if (nrow(h) == 0) return(h)
    h$DateOnly <- as.Date(substr(h$DateTime, 1, 10))
    h$Week <- floor_to_monday(h$DateOnly)
    h
  })
  output$jg_grid <- renderUI({
    items <- jg_filtered_items()
    if (nrow(items) == 0) return(div(class = "alert alert-secondary", "No items match this filter."))
    weeks <- jg_weeks()
    ev <- jg_events()
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
        has_insp <- nrow(matches) > 0 && any(matches$EntryType == "Service Inspection")
        has_job  <- nrow(matches) > 0 && any(matches$EntryType == "Job Card")
        bg <- if (has_insp && has_job) "linear-gradient(135deg,#3E7C59 50%,#D9A400 50%)"
        else if (has_insp) "#3E7C59"
        else if (has_job) "#D9A400"
        else "#E2E2E2"
        title_txt <- paste0(label, " - week of ", format(wk, "%d %b %Y"),
                             if (has_insp) " - Service Inspection" else "",
                             if (has_job) " - Job Card" else "")
        div(title = title_txt, style = paste0("width:16px; height:16px; border-radius:3px; background:", bg, "; flex-shrink:0;"))
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
      row_divs,
      div(style = "display:flex; gap:16px; margin-top:10px; font-size:11px; color:#666;",
          div(style = "display:flex; align-items:center; gap:5px;", span(style = "width:12px;height:12px;background:#3E7C59;border-radius:2px;display:inline-block;"), "Service Inspection"),
          div(style = "display:flex; align-items:center; gap:5px;", span(style = "width:12px;height:12px;background:#D9A400;border-radius:2px;display:inline-block;"), "Job Card"),
          div(style = "display:flex; align-items:center; gap:5px;", span(style = "width:12px;height:12px;background:#E2E2E2;border-radius:2px;display:inline-block;"), "Nothing logged")
      )
    )
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
  # ADMIN - control panel, not an edit surface. All data edits stay
  # inline where the data lives (Inventory List, Whereabouts,
  # Invoices); this tab is just Google Sheets sync status.
  # -------------------------------------------------------------
  output$admin_tab_content <- renderUI({
    tagList(
      br(),
      div(class = "admin-card",
          h5("Google Sheets Sync"),
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
          if (!is.null(sheets_last_error())) div(class = "alert alert-danger mt-3", sheets_last_error())
      ),
      div(class = "admin-card",
          h5("Machines With No Driver"),
          p(class = "text-muted", "Active plant with nobody currently assigned - worth double-checking these."),
          {
            df <- inventory_data()
            unassigned <- df[df$Active == "Yes" & (is.na(df$Driver) | df$Driver == ""), ]
            if (nrow(unassigned) == 0) div(class = "alert alert-secondary mb-0", "Every active item has a driver assigned.")
            else tagList(
              div(class = "alert alert-warning", paste0(nrow(unassigned), " active item(s) with no driver.")),
              tableOutput("admin_no_driver_table")
            )
          }
      ),
      div(class = "admin-card",
          h5("Staff Activity (This Week)"),
          p(class = "text-muted", "Quick count of History entries and Invoices logged by each person this week. For a specific week/month or person, use the 'Report by input' filter on the Reports tab."),
          tableOutput("admin_staff_activity_table")
      ),
      div(class = "admin-card",
          h5("Supplier List (Invoice Company dropdown)"),
          p(class = "text-muted", "Edit the COMPANY_LIST vector near the top of app.R to add or remove suppliers."),
          div(paste(COMPANY_LIST, collapse = ", "))
      ),
      div(class = "admin-card",
          h5("Ganger List"),
          p(class = "text-muted", "Names available in the Ganger dropdown when creating or editing a gang sheet."),
          fluidRow(
            column(8, textInput("admin_ganger_new", NULL, placeholder = "e.g. John Smith")),
            column(4, actionButton("admin_ganger_add", "+ Add", class = "btn-primary btn-sm"))
          ),
          if (length(ganger_list()) == 0) p(class = "text-muted mb-0", "No gangers added yet.")
          else tagList(lapply(ganger_list(), function(nm) {
            div(class = "d-flex justify-content-between align-items-center", style = "padding:4px 0; border-bottom:1px solid #eee;",
                span(nm),
                tags$a(href = "#", style = "font-size:0.85rem; color:#9C2B2B;",
                       onclick = sprintf("Shiny.setInputValue('delete_ganger_click', '%s', {priority:'event'}); return false;", nm),
                       "Delete")
            )
          }))
      )
    )
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
    unassigned %>% transmute(Item = ifelse(Machine == "", ItemID, Machine),
                             `PMK/Reg` = ifelse(PMK_Number != "", PMK_Number, Registration),
                             Category, `Sub-Category` = SubCategory,
                             Gang = ifelse(Gang == "", "Not assigned", Gang))
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
}
shinyApp(ui, server)