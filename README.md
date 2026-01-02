

<!-- Badges -->
![GitHub code size in bytes](https://img.shields.io/github/languages/code-sizeemg)
---

Package in early stages of development.

## Installation

This is an R package -> install from ![GitHub repo](https://github.com/ekznw/ekznwr) using R devtools.

```R
devtools::install_github('ekznw/ekznwr')
```

## GIS Metadata

```{.mermaid}
graph TD
    A[Drop and drag GIS data files] --> B{Support checks}
    B -->|Yes| C[
        Auto
     metadata
     extraction
    ]
    B -->|No| D{Manual Overide?}
    D -->|Yes| A
    D -->|No| E[
        Manual
        metadata
        entry
        ]
    C --> E
```