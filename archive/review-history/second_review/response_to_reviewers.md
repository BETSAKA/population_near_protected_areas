# Response to reviewers — ERL-124325

**Manuscript:** "How Many People Live Near Protected Areas in Developing Countries? Estimates from Gridded Population Data (2000–2020)"

We thank the referees for their constructive comments. Below we respond to each point; referee comments are in italics and our responses follow. Changes in the manuscript are referenced by section.

---

## Important note: correction of a computation issue and reimplementation in R

While preparing the additional decomposition requested by Referee 3, we identified a technical issue in the original (Google Earth Engine, GEE) computation that was not raised by the reviewers but that we considered necessary to correct before resubmission. We flagged this to the editorial office and were granted a short extension.

The issue had two components:

1. **Double counting of overlapping buffer zones.** In the GEE workflow, the 2020 "all PAs" cross-section was assembled by summing two independently generated layers (PAs with a recorded designation year and PAs with a missing designation year). Where the 10 km buffer rings of these two layers overlapped, population and area were counted twice, inflating the "inside or within 10 km" figures.

2. **Cross-border attribution.** PAs were not filtered to each country's own territory, so PAs located in neighbouring countries could contribute population to a country's totals where buffers crossed borders.

Because access to GEE has recently been restricted, we reimplemented the entire population-extraction pipeline in R (`terra`, `exactextractr`, `sf`), which also improves reproducibility. The corrected pipeline (i) computes the 2020 all-PA cross-section directly from a single joint mask, with mutually exclusive PA interiors and buffer rings and an exclusive category hierarchy (strict → non-strict → unknown), and (ii) filters PAs to each country by ISO3 code.

**Effect on results.** Aggregate magnitudes and all qualitative conclusions are unchanged, but the "inside or within 10 km" figures are lower than in the first-revision GEE version, mainly because of the corrected double counting. The population *inside* PAs is essentially unchanged. Specifically, for 2020 across the 75 LMICs (excluding India):

- Inside PAs: **60.8 million (2.5%)** (previously ~61 million, 2.5%).
- Inside or within 10 km: **732 million (29.5%)** (previously ~902 million, 36.6%).
- 2000 (PAs with recorded designation year): **19 million (1.1%)** inside and **258 million (15.4%)** inside or within 10 km.

A further consequence, relevant to Referee 3's comment on national shares, is that no country's exposure share now exceeds 100% (the earlier values above 100%, e.g. Palestine, were artefacts of cross-border attribution). The Methods (main text) and Supplementary Materials Section A now describe the R workflow and this correction; the replication package on GitHub/Software Heritage includes both the R code and the superseded GEE code.

---

## Referee 1

*The authors have performed a useful analysis of an important question... I have no further comments.*

We thank Referee 1 for the positive assessment.

---

## Referee 3

*I would like to see a bit more nuance ... whether there are more people living within close proximity to non-strict PAs because there are more non-strict PAs ... or because of some other settlement pattern ... e.g. people within 10 km proximity / sq km of strict PA vs non-strict PA.*

We now address this directly with an area-standardized decomposition (new paragraph in Results, "Population magnitudes"; new standardized table in the Supplementary Materials). After dividing nearby population by PA area, non-strict PAs remain somewhat more populated than strict PAs (**149 vs 118** people inside or within 10 km per km² of PA), but the larger *raw* population near non-strict PAs is driven mainly by scale: non-strict PAs are both more numerous (**2,564 vs 1,160** sites) and slightly more extensive (**1.39 vs 1.35** million km²). We now state that the raw difference reflects primarily PA number and area, with only a modest additional contribution from higher nearby population intensity.

*It also might be worth detailing a bit more what the actual differences are between the two PA types — briefly.*

We added a short characterization in the Discussion ("PA categories matter for interpreting impacts"): strict categories (IUCN I–III) are typically designated as areas where human habitation and extraction are prohibited or tightly limited, whereas non-strict categories (IUCN IV–VI) explicitly accommodate settlement and sustainable use.

*Abstract – Ln 9: total global population that these 75 LMICs represent.*

The abstract now states the combined population of the 75 LMICs (about **2.48 billion** people in 2020).

*Abstract – Ln 11: provide a simple justification for excluding India (e.g. "India, representing 36% of LMIC's population, is excluded…").*

The abstract now reads "India, which represents about 36% of the LMIC population, is excluded because its national protected areas are not publicly available in the WDPA."

*Abstract – Ln 14: show the numbers and percentages for 2000.*

The abstract now reports the 2000 figures alongside 2020: **19 million (1.1%)** inside and **258 million (15.4%)** inside or within 10 km.

*Pg 1, Ln 36 – Pg 2, Ln 13: this paragraph belongs in Methods; the Intro need only state "within a reference buffer commonly used in the literature".*

The exact proximity definitions are now given in Methods ("Study Area"), and the Introduction retains only the concept, using the phrasing "within a reference buffer commonly used in the literature".

*Pg 4, Ln 11: what % of the population did the 17.9 million represent?*

The corresponding percentage is now reported throughout (inside PAs in 2000 corresponds to **1.1%** of the 2000 sample population; the 2020 inside-PA figure of 60.8 million corresponds to **2.5%**).

*Table 1: why are the numbers "inside Strict PAs only", "inside Non-strict PAs only" and "inside Unknown category PAs only" not included?*

Table 1 has been extended to include inside-only rows by category. For 2020, of the 60.8 million people inside PAs, **10.4 million** are inside strict PAs, **19.0 million** inside non-strict PAs, and **31.5 million** inside PAs of unrecorded IUCN category; the corresponding inside-or-within-10 km rows are **159 / 207 / 366 million**.

*Pg 9, Ln 13-15: is this because there are fewer strict protection regimes, or a fundamental difference in settlement patterns? ... many strict PAs do not allow populations to live within their boundaries.*

We now tease these apart. The standardization shows the difference is mainly one of scale (number and area of PAs) rather than of nearby population intensity. We also note explicitly that the near-absence of residents inside strict PAs is partly by design, since strict categories commonly prohibit human habitation.

*Pg 9, Ln 16-20: this functional difference has been shown in ecological studies (Kittle et al. 2018, Sri Lankan leopard).*

We added this result and citation to the Discussion: habitat suitability for the Sri Lankan leopard is improved by proximity to strictly protected areas but unaffected by less strictly protected areas where entry and extraction are permitted (Kittle et al. 2018), illustrating that management category differences are functionally meaningful beyond human populations.

---

We believe these revisions, together with the corrected and now fully reproducible R pipeline, substantially strengthen the manuscript, and we thank the referees again for their input.
