# PMK Plant Tracker - Going Live + Email Reports

Before deploying, three files in the project folder need updating (already done in your outputs folder, not yet copied to `~/Documents/PMK App/`):

- **app.R** - now loads its starting inventory from `initial_inventory_seed.csv` instead of having all 155 real fleet rows typed into the script, and loads Inventory/Invoices/Plant History from the Google Sheet on startup when sync is on (previously sync only wrote to the Sheet, never read back - meant every restart would have silently wiped the app back to empty).
- **send_reports.R** - now reads the Google service account key and SMTP password from environment variables when they're set, falling back to local files for testing.
- **.gitignore** - keeps the service account key, SMTP creds, and the CSV of real fleet data out of git.

This matters because Posit Connect Cloud - the platform you'll deploy to - publishes from a **public** GitHub repository. Anything committed there is visible to anyone. None of your credentials or real driver/fleet data should end up in it.

## 1. Set up Google Sheets sync

Already documented in the top of `app.R`, summarized here:

1. In Google Cloud Console, create a new project (separate from any other app) and enable the Google Sheets API.
2. IAM & Admin > Service Accounts > Create Service Account.
3. That service account > Keys > Add Key > Create new key (JSON). Save it as `sheets_service_account.json` next to `app.R`.
4. Create a Google Sheet, share it with the service account's email (found in the JSON under `client_email`), Editor access.
5. Copy the Sheet ID from its URL (the string between `/d/` and `/edit`) into `SHEETS_SPREADSHEET_ID` in `app.R`.
6. Set `SHEETS_SYNC_ENABLED <- TRUE`.
7. Run the app locally once (RStudio > Run App) and use it briefly - this pushes your real inventory into the Sheet for the first time. Check the Sheet has an Inventory tab with your 155 items before moving on.

Once the Sheet has data, the app no longer depends on the CSV or hardcoded data at all - it reads its starting state from the Sheet on every launch.

## 2. Push the code to GitHub

1. Create a **new GitHub repository** (public - required for Connect Cloud's free tier).
2. In RStudio: File > New Project > Version Control > Git, or `git init` in the project folder, then connect it to the new repo.
3. Before committing, confirm `.gitignore` is in place and run `git status` - `sheets_service_account.json`, `email_creds`, and `initial_inventory_seed.csv` must NOT appear as files to be added. If any of them show up, `.gitignore` isn't being picked up (usually means git was initialized before the file was added - run `git rm --cached <file>` to fix it).
4. In R console: `install.packages("rsconnect")` then `rsconnect::writeManifest()` in the project folder - this creates `manifest.json`, which tells Connect Cloud what R packages to install.
5. Commit everything except the gitignored files, and push.

## 3. Publish on Posit Connect Cloud

Posit Connect Cloud is the product shinyapps.io is being folded into - this is the correct place to deploy now rather than shinyapps.io.

1. Go to [connect.posit.cloud](https://connect.posit.cloud) and sign in (GitHub login is the simplest option, and installs the Posit GitHub App you'll need anyway).
2. Publish > Shiny > select your repository and branch.
3. Set the primary file to `app.R`.
4. Before finishing, open **Advanced settings** and add two environment variables:
   - `GOOGLE_SHEETS_KEY_JSON` - paste the entire contents of your `sheets_service_account.json` file as the value.
   - (Only needed here if you later also deploy `send_reports.R` as its own piece of content on Connect Cloud - see below.)
   These are encrypted at rest by Connect Cloud and never appear in your repo.
5. Publish. Connect Cloud builds and hosts the app; you get a URL to share with the team.
6. For future changes: edit locally, commit, push to GitHub, then hit "Republish" on the content's page in Connect Cloud.

## 4. Set up the email reports

`send_reports.R` (already written) sends daily MOT/warranty reminders, a weekly activity report, and a monthly spend/fleet report, reading straight from the same Google Sheet.

1. Gmail App Password: turn on 2-Step Verification on the sending Gmail account, then generate an app password at myaccount.google.com/apppasswords.
2. Locally, one-time: `library(blastula); create_smtp_creds_file(file = "email_creds", user = "your-address@gmail.com", provider = "gmail")` - paste the app password when prompted. This is only needed for local testing; it's gitignored.
3. Set `EMAIL_FROM` and `REPORT_RECIPIENTS` in `send_reports.R`.
4. Test locally: `Rscript send_reports.R daily` (or weekly/monthly) and confirm the email arrives.

### Scheduling it

Three options, in order of effort:

- **Cron on a machine that's reliably on** - simplest, but does nothing if the machine's asleep. Example crontab is in the comments at the top of `send_reports.R`.
- **A small always-on Linux VM** (~£5/month) with the same crontab - the standard fix once a laptop isn't reliable enough.
- **Posit Connect Cloud's own Schedule feature** - paid plans support scheduling content to run automatically (hourly/daily/weekly/monthly). I confirmed this exists for Connect Cloud, but I could not fully confirm from the docs whether a plain R script with a fully custom blastula HTML email behaves identically there to how it works on the separate enterprise "Posit Connect" product (where this pattern is explicitly documented). If you want to use this route, publish `send_reports.R` as its own piece of content on Connect Cloud and check the Schedule tab once it's up - if scripts aren't schedulable directly, wrapping the same logic in a Quarto document (`.qmd`) is the documented, confirmed path for scheduled content with custom email on Connect Cloud.

Given the uncertainty on that last point, I'd start with cron/a small VM - it's guaranteed to work and you're already set up for it - and treat Connect Cloud scheduling as something to try once the app itself is live and you're already in that dashboard.

## Order to actually do this in

1. Enable Sheets sync locally, run once, confirm the Sheet has your data.
2. Push to GitHub, confirm `.gitignore` worked.
3. Publish to Connect Cloud with the `GOOGLE_SHEETS_KEY_JSON` secret set.
4. Test the live app end to end (login, add an item, confirm it lands in the Sheet).
5. Set up `send_reports.R` locally, test each report type once.
6. Set up scheduling (cron first; revisit Connect Cloud's scheduler later if you want).

I can walk through any of these steps on your Mac directly (Google Cloud Console, git/GitHub setup, RStudio) whenever you're ready - just say which one.
