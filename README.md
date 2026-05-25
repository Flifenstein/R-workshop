# R for social scientists — a hands-on workshop

A self-built R programming and data analysis workshop, designed for social sciences students with little or no programming experience. Co-created with [@wouterstoter](https://github.com/wouterstoter).

**Live site:** [flifenstein.github.io/R-workshop](https://flifenstein.github.io/R-workshop/)

## What's in it

Three hands-on tutorials that walk students through real datasets:

- **COVID-19** — basic data wrangling and visualization using public COVID data
- **Netflix viewing history** — exploring personal data using R's tidyverse
- **WhatsApp chat analysis** — text data manipulation on personal message exports

Plus cheat sheets and presentation slides used during the sessions.

## Why we made it

Most R tutorials assume you're already comfortable with programming. We wanted something different: an introduction aimed at students from non-technical backgrounds who needed to do data analysis in their coursework, with examples from data they'd recognise from their own lives.

## Repo structure

| File | What it is |
|---|---|
| `index.Rmd` | Landing page |
| `covid.Rmd`, `netflix.Rmd`, `whatsapp.Rmd` | Three tutorial walkthroughs |
| `*.R` | Standalone R scripts for each tutorial |
| `docs/` | Built site output (deployed via GitHub Pages) |
| `_site.yml` | R Markdown site configuration |

## Built with

R Markdown · the tidyverse · GitHub Pages

## Status

Created in 2022. Not actively maintained, but still useful as a learning resource — feel free to fork or adapt.
