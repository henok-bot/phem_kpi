# assets/

Shared static assets used by both dashboards. These are committed (they are small
and needed to render/host), unlike raw data.

| File | What | Source |
|---|---|---|
| `ephi_logo.png` | EPHI logo (214×216) | copied from the EPHI image set; the local copy is the fix for the old "logo loaded from an external URL" bug |
| `ephi_anniversary.png` | EPHI anniversary logo (optional) | same set |
| `eth_adm3_geo.rds` | Ethiopia **woreda** geometry, 1,082 features, key `adm3_pcode` (`ET0101…`) | derived from the 2021/2024 CSA shapefile; joins directly to `output/kpi_woreda_pcode.csv` |
| `eth_adm1_geo.rds` | Ethiopia **region** borders | same shapefile |

To replace the logo, drop a new PNG here as `ephi_logo.png`, then re-run
`Rscript shiny/prep_data.R` (it copies the logo into `shiny/www/`). The Quarto
dashboard reads `assets/ephi_logo.png` directly.
