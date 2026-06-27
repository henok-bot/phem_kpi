# Dashboards & hosting — a practical walkthrough

This document explains the two dashboards in this project and gives step-by-step
instructions for putting them online. You will create the GitHub and Posit
accounts yourself; the commands you run are all here.

There are two products, and they suit different hosting:

| Product | File(s) | What it is | Best home |
|---|---|---|---|
| **Quarto dashboard** | `reports/kpi_dashboard.qmd` | A static interactive HTML page (plotly + leaflet + tables). Re-renders from the pipeline. | **GitHub Pages** (free, public) — and optionally Cloudflare Access for a private login. Also the foundation for a personal website. |
| **Shiny app** | `shiny/` | A live app with a login, reactive filters and a fast map. | **shinyapps.io** (Posit) — remote, password-protected, free tier. |

A simple way to think about it: the **Shiny app on shinyapps.io is your private,
login-protected dashboard reachable from anywhere**. The **Quarto dashboard is the
lightweight, free, GitHub-hosted version** and your route into building a personal
site. The free GitHub Pages is public, so put only aggregated results there, or
add the optional Cloudflare login described in Part B4.

---

## Part A — One-time setup on this server

### A1. R packages (already installed in this session)

```r
install.packages(c("shiny","plotly","leaflet","leaflet.extras","DT",
                   "shinymanager","rsconnect","shinycssloaders","shinyWidgets",
                   "htmltools"))
```

### A2. Tell git who you are (once per machine)

```bash
git config --global user.name  "Henok Tadesse"
git config --global user.email "drhenoktadesse@ephi.gov.et"
```

---

## Part B — The Quarto dashboard

### B1. Render it locally

```bash
quarto render reports/kpi_dashboard.qmd
# produces reports/kpi_dashboard.html — open it in a browser to check
```

The dashboard reads the pipeline's output (`output/`, `exports/tables/`) and the
woreda geometry in `assets/`. Re-run `scripts/run_all.R` first if the data changed.

### B2. Put the project on GitHub

You will do the account creation and the first push. Steps:

1. On <https://github.com> create the account, then create a **new empty repo**
   (no README, no .gitignore — the project already has files). Call it e.g.
   `phem-kpi`. Decide public or private now (see the privacy note above).

2. GitHub no longer accepts your account password from the command line. Create a
   **Personal Access Token (PAT)**: GitHub → *Settings* → *Developer settings* →
   *Personal access tokens* → *Tokens (classic)* → *Generate new token*, tick the
   **`repo`** scope, copy the token (you will paste it as the password when git
   asks). A token starting `ghp_...`.

3. From the project root, turn it into a repo and push. A `.gitignore` is already
   in place so raw data and rendered artefacts are not uploaded.

   ```bash
   cd /data/r_projects_cloud/misc_ephi/kpi_analysis
   git init
   git add .
   git commit -m "KPI analysis: pipeline, reports, dashboards"
   git branch -M main
   git remote add origin https://github.com/<your-username>/phem-kpi.git
   git push -u origin main          # username = your GitHub name, password = the PAT
   ```

   (Tip: to avoid pasting the PAT every push, run `git config --global
   credential.helper store` once — it then remembers it in a file.)

### B3. Publish the dashboard to GitHub Pages (free, public)

The simplest reliable way is Quarto's built-in publisher. It renders the document
and pushes the HTML to a special `gh-pages` branch that GitHub serves as a website.

```bash
quarto publish gh-pages reports/kpi_dashboard.qmd
```

The first time it asks for confirmation and may open a browser to authorise. When
it finishes it prints the public URL, of the form
`https://<your-username>.github.io/phem-kpi/kpi_dashboard.html`.

To update it later, re-run the same command after re-rendering.

> Alternative without the helper: render to a `docs/` folder
> (`quarto render reports/kpi_dashboard.qmd --output-dir ../docs`), commit and
> push, then in the repo *Settings → Pages* set the source to `main` / `/docs`.
> The `quarto publish` route is easier to start with.

### B4. Optional — make the static dashboard private with a login

GitHub Pages on a free plan is always public. If you need the *static* dashboard
behind a login as well, the free way is **Cloudflare Pages + Cloudflare Access**.

**Is Cloudflare paid?** No, not for this. You need a Cloudflare account (free to
create). Cloudflare Pages hosts static sites free. Cloudflare Access (part of
"Zero Trust") has a **Free plan for up to 50 users** that covers exactly this. The
Zero Trust signup screen may ask you to add a payment method to verify the
account; on the Free plan you are not charged. If you would rather not, skip this
section — your private dashboard need is already met by the Shiny app (Part C).

Steps, once you decide to use it:

1. Create a Cloudflare account at <https://dash.cloudflare.com>.
2. **Workers & Pages → Create → Pages → Connect to Git**, pick your GitHub repo.
   Set the build output to the folder holding the rendered site (e.g. `docs`), or
   upload the rendered HTML directly with *Direct Upload*. Cloudflare gives you a
   `*.pages.dev` URL.
3. **Zero Trust → Access → Applications → Add an application → Self-hosted**, point
   it at your `*.pages.dev` URL, and add a **policy**: *Allow* → emails you list
   (or a one-time PIN to any address you approve). Now visitors must verify by
   email before the page loads.

### B5. The personal-website ambition

A personal site is the same workflow at a slightly larger scale. Instead of one
`.qmd`, a Quarto **website** is a folder of `.qmd` pages with a `_quarto.yml` that
defines the navbar. You render it and `quarto publish gh-pages` the whole thing.
Start from <https://quarto.org/docs/websites/>. Everything you learn here —
git, PAT, Pages, optional Cloudflare login — carries straight over.

---

## Part C — The Shiny app on shinyapps.io

### C1. Refresh the app's data

The app reads small `.rds` files, not the raw pipeline. Build them once (and after
each data refresh):

```bash
Rscript shiny/prep_data.R        # writes shiny/data/*.rds and copies the logo
```

Run it locally to check before deploying:

```r
shiny::runApp("shiny", launch.browser = TRUE)   # log in with ephi / kpi2026
```

### C2. Create the shinyapps.io account and connect it

1. Sign up at <https://www.shinyapps.io> (free tier: 5 apps, ~25 active hours per
   month, which is plenty for an internal dashboard).
2. In the shinyapps.io dashboard: **Account → Tokens → Show → Copy to clipboard**.
   It gives you a ready-made `rsconnect::setAccountInfo(...)` line containing your
   name, token and secret. Run that line once in R on this server:

   ```r
   rsconnect::setAccountInfo(name   = "<your-account>",
                             token  = "<token>",
                             secret = "<secret>")
   ```

   This stores your credentials locally so future deploys need no login.

### C3. Deploy

```r
rsconnect::deployApp(
  appDir   = "shiny",
  appName  = "phem-kpi-dashboard",
  appTitle = "PHEM KPI Dashboard")
```

It bundles everything in `shiny/` (code + `data/` + `www/`), uploads it, and prints
the public URL, of the form
`https://<your-account>.shinyapps.io/phem-kpi-dashboard/`. The login page appears
first; users sign in with the accounts in `APP_CREDENTIALS`.

To update later, re-run `prep_data.R` (if data changed) then the same `deployApp`.

### C4. Managing the login

Users live in `APP_CREDENTIALS` in `shiny/global.R`. Add a row per person. For a
real deployment, do not keep passwords in the source that goes to GitHub. Two
options:

- Read them from environment variables set in the shinyapps.io dashboard
  (**Application → Settings → Environment variables**), e.g.
  `Sys.getenv("KPI_USERS")`, and build the data frame from that.
- Or use the `keyring` package locally.

At minimum, change the default passwords before sharing the URL.

---

## Part D — The quarterly refresh (both dashboards)

When a new quarter's data arrives and the pipeline has been updated (new
`raw/<period>/` folder and a `cfg_` block in `scripts/catalog.R`):

```bash
# 1. rebuild the analysis outputs
Rscript scripts/run_all.R --no-convert        # refreshes output/ + exports/ + reports

# 2. Quarto dashboard: re-publish
quarto publish gh-pages reports/kpi_dashboard.qmd

# 3. Shiny app: refresh its data and redeploy
Rscript shiny/prep_data.R
R -e 'rsconnect::deployApp("shiny", appName = "phem-kpi-dashboard")'
```

`scripts/run_all.R` already renders `kpi_dashboard.qmd` alongside the other
reports, so step 1 also produces a fresh `reports/kpi_dashboard.html` locally.

---

## Part E — Quick reference

| Task | Command |
|---|---|
| Render Quarto dashboard | `quarto render reports/kpi_dashboard.qmd` |
| Publish to GitHub Pages | `quarto publish gh-pages reports/kpi_dashboard.qmd` |
| Build Shiny data | `Rscript shiny/prep_data.R` |
| Run Shiny locally | `R -e 'shiny::runApp("shiny")'` |
| Deploy Shiny | `R -e 'rsconnect::deployApp("shiny", appName="phem-kpi-dashboard")'` |

**Common snags**

- *Logo missing:* it must be a local file. The Quarto dashboard uses
  `assets/ephi_logo.png`; the Shiny app uses `shiny/www/ephi_logo.png` (copied by
  `prep_data.R`). Never point at an external URL — that is what broke the old app.
- *git push rejected:* you used your password instead of the PAT, or the `repo`
  scope was not ticked when creating the token.
- *Shiny deploy fails on a package:* make sure the package is installed locally;
  `rsconnect` records your installed versions and rebuilds them on the server.
- *Map is blank:* check `assets/eth_adm3_geo.rds` exists and that
  `output/kpi_woreda_pcode.csv` shares the `adm3_pcode` key (it does by design).
