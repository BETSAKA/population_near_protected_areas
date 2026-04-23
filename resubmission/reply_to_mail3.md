# Diagnosis of mail3 and proposed fixes

**From**: Joanna Bewley (IOP Publishing)
**Issues raised**:
1. The clean anonymised PDF contains **two Table 1's**.
2. The un-anonymised .docx (both attached and uploaded) also has two Table 1's.
3. **"Figure 1 has the caption under figure 3"** — Figure 1 appears without a proper caption; the caption the editor expected on Figure 1 seems to be attached to (or be confused with) Figure 3.

---

## Diagnosis

### Issue A — Two "Table 1" labels

Running `grep -E "Figure|Table" code/reviewed_produce_table_and_figures.R` revealed a typo:

| Object built | Saved title (before fix) | Saved title (after fix) |
|---|---|---|
| `table_1` → `table_1.docx` | `Table 1: Population inside or within 10 km of protected areas in 2020` | (unchanged) |
| `table_2` → `table_2.docx` | **`Table 1: Implied population magnitudes by evaluation design`** ← typo | `Table 2: Implied population magnitudes by evaluation design` |

So both standalone table files carried a "Table 1:" prefix. That is the first source of the editor's "two Table 1" observation.

**But there is a second, more structural issue.** The revised manuscript body loads **only `table_2.rds`** (the Implied population magnitudes table) and refers to it in prose as *"Table 1"*. The content of `table_1.rds` ("Population inside or within 10 km of protected areas in 2020") is **never rendered** in the main manuscript body — only inline numbers from its underlying data (`t1_data`) are cited. Yet `table_1.docx` was still uploaded as a separate file. So there are effectively three places labelled "Table 1":

1. The embedded table in the manuscript body (labelled Table 1 by Word numbering; content = Implied population magnitudes)
2. The uploaded file `table_1.docx` (Population inside or within 10 km...)
3. The uploaded file `table_2.docx` (gt title starts with "Table 1:" because of the typo)

### Issue B — Figure 1 has no caption on the image

In [code/reviewed_produce_table_and_figures.R](code/reviewed_produce_table_and_figures.R), every figure except Figure 1 uses `labs(title = "Figure N: ...", subtitle = "...")`. Figure 1 was built with only a `subtitle`, no `title`:

```r
# Before
labs(
  subtitle = "75 LMICs (excl. India), GHSL estimates, PAs with recorded designation year",
  ...
)
```

So `figure_1.png` showed only the subtitle in tiny type under the faceted bars, with no "Figure 1: ..." header. In the submitted PDF/DOCX the first image therefore appeared caption-less, while Figures 2 and 3 carried proper "Figure 2: ..." / "Figure 3: ..." headings — which is very likely what Joanna meant by "Figure 1 has the caption under figure 3".

---

## Fixes already applied in the repository

1. [code/reviewed_produce_table_and_figures.R](code/reviewed_produce_table_and_figures.R): gt title of `table_2` corrected from `"Table 1: Implied …"` to `"Table 2: Implied …"`.
2. [code/reviewed_produce_table_and_figures.R](code/reviewed_produce_table_and_figures.R): `figure_1` now has a proper `title = "Figure 1: Population inside and within 10 km of PAs by IUCN category (2000 vs 2020)"`.
3. The R script was rerun, producing refreshed `results/figure_1.png`, `results/table_2.docx`, etc.
4. Both Quarto documents were re-rendered to .docx with the corrected figures/tables embedded.

## Outstanding decision — what to upload in the "table" slots

The revised manuscript body has **one** numbered table ("Table 1: Implied population magnitudes by evaluation design") and **three** numbered figures (Figures 1–3). Two options:

- **Option 1 (simplest, recommended)**: upload a single Table 1 file = the *Implied population magnitudes* table. Do not upload a separate `table_1.docx` with the "Population inside or within 10 km" content. Remove the old `figure_4.png`, `figure_5.png`, `table_s3.docx` from the submission, since they are not referenced in the revised manuscript or supplementary materials.
- **Option 2**: re-introduce an explicit Table 1 in the manuscript body = *Population inside or within 10 km of PAs in 2020* (content from `t1_data`) and have the *Implied population magnitudes* table become Table 2. This restores the initial structure but requires editing the qmd.

**⚠️ The user needs to decide between Option 1 and Option 2 before I finalise the upload set.**

## Reply draft for Joanna

Proposed reply body once the option is chosen (I'll adapt to the choice):

> Dear Joanna,
>
> Many thanks for spotting these issues. I have traced them to two bugs in the scripts that produce the figure and table files, and a leftover legacy file in the submission:
>
> 1. In the R code that renders each standalone table, the gt title of the "Implied population magnitudes…" table inadvertently started with "Table 1:" instead of "Table 2:". Combined with a residual standalone `table_1.docx` from the earlier submission, this produced the two "Table 1" labels you saw. The revised manuscript in fact contains a single numbered table in the body.
> 2. `figure_1.png` was generated without a "Figure 1:" title header (only a subtitle). This is why the caption for Figure 1 appeared to go missing while Figures 2 and 3 carried "Figure 2:" / "Figure 3:" headers correctly.
>
> Attached, please find the corrected and regenerated files: one consolidated manuscript source (.docx) with the author information, a refreshed `figure_1.png`, `figure_2.png`, `figure_3.png`, `figure_s1.png`, `figure_s2.png`, a single `table_1.docx` (Implied population magnitudes by evaluation design), and `table_s1.docx`, `table_s2.docx`. The supplementary source (.docx) and PDF have also been updated.
>
> Please let me know if anything else needs adjusting.
>
> Kind regards,
> Florent Bédécarrats
