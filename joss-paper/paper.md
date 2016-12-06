---
title: "Eclipse Golo"
tags:
  - R
  - Parser combinator
authors:
  - name: Chapman Siu
    orcid: 0000-0002-2089-3796
    affiliation: 1
affiliations:
  - name: Chapman Siu
    index: 1
date: 6 December 2016
bibliography: paper.bib
---

# Summary

Ramble is a parser combinator for the [`R`](https://www.r-project.org/) [@R] language using the higher order functions. The combinatory parsing approached used in Ramble mirrors approaches used in functional languages such as Miranda [@Hutton1992], and is able to handle ambiguous grammars, and provide full backtracking if it is needed.

Ramble is capable of going beyond simply parsing, even adding semantic actions, allowing their results to be manipulated in any way we please.

# References
