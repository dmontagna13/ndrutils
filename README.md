# ndrutils <small>(High‑throughput plate utilities)</small>

**ndrutils** is a lightweight toolkit for people who work with medium throughput data.  
It streamlines three repetitive chores:

* **Parsing instrument output**  
  &nbsp;&nbsp;• *HP/Tecan d300* “Print Map” XML ⇒ tidy table  
  &nbsp;&nbsp;• *Revvity Envision Nexus* plate reader CSV ⇒ tidy table

* **Cleaning edge artefacts** with `drop_edges()`.

* **Publishing‑ready plots** with a
  color‑blind‑safe variant of `ggplot2::theme_classic()`.

---

## Installation

```r
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")

remotes::install_github("yourGitHubName/ndrutils")   # change to your user/org

```
