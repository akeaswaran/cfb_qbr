CFB QBR
---

Downloads and parses regular season ESPN QBR data from ESPN's API.

To use, just `cd` into the root directory of the repo and run `python qbr_scrape.py`.

`composite_qbr.csv` is updated weekly via GitHub action with the latest data from ESPN.

`qbr.R` has sample code to generate a GAM for QBR based on QB EPA (h/t [@guga31bb](https://github.com/guga31bb)).

Big thanks to:

* [@nntrn](https://github.com/nntrn) for the gist for NFL QBR data: https://gist.github.com/nntrn/ee26cb2a0716de0947a0a4e9a157bc1c#athletes
* [@jthomasmock](https://github.com/jthomasmock) for a simpler link https://github.com/jthomasmock/espnscrapeR/blob/master/R/get_nfl_qbr.R#L66


