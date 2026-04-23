# Resubmission package — ERL-124325

Last rebuilt: 2026-04-17.

## What changed vs. the previously uploaded submission

1. The YAML header of [reviewed_PA_impact_population_counts_LMICs.qmd](../reviewed_PA_impact_population_counts_LMICs.qmd) and [reviewed_Supplementary_Materials.qmd](../reviewed_Supplementary_Materials.qmd) now contains the corresponding author information (name, affiliation, ORCID, email). The previous field `author: "Anonymized for review"` was replaced.
2. `reviewed_PA_impact_population_counts_LMICs.docx` and `reviewed_Supplementary_Materials.docx` in this folder were re-rendered from the updated Quarto sources (`quarto render ... --to docx`). The author block is visible directly under the title.
3. All other figures, tables, PDFs and the justification statement are unchanged and copied as-is from `documentation/full_submission/`.

## Contents of this folder

Main manuscript:
- `reviewed_PA_impact_population_counts_LMICs.docx` — **new source file with author info** (answers mail2)
- `reviewed_Justification_statement.docx` — unchanged

Supplementary material:
- `reviewed_Supplementary_Materials.pdf` — unchanged (previously submitted version)
- `reviewed_Supplementary_Materials.docx` — re-rendered with author info (upload optional; useful if ERL wants the supplementary source too)

Figures (main text):
- `figure_1.png`, `figure_2.png`, `figure_3.png`, `figure_4.png`, `figure_5.png`

Tables (main text):
- `table_1.docx`, `table_2.docx`

Supplementary figures:
- `figure_s1.png`, `figure_s2.png`

Supplementary tables:
- `table_s1.docx`, `table_s2.docx`, `table_s3.docx`

Replies to the editor:
- `reply_to_mail1.md` — titles and descriptions for all supplementary files (DOI registration)
- `reply_to_mail2.md` — cover note for the new source file with author information

## Step-by-step instructions

### Step 1 — Verify the updated manuscript source

1. Open `reviewed_PA_impact_population_counts_LMICs.docx` in Word.
2. Check that under the title you see:
   *"Florent Bédécarrats (IRD, Université Paris-Saclay, UMI SOURCE, Guyancourt, France; ORCID 0000-0003-1001-5540; corresponding author: florent.bedecarrats@ird.fr)"*
3. If you want a different layout (e.g., affiliation on a separate line, superscript numbers), tell me and I will customise the `Author` paragraph style in `data/Word format template.docx`.

### Step 2 — Reply to mail2 (source file with author info)

1. Log in to ScholarOne at <https://mc04.manuscriptcentral.com/erl-iop>.
2. Go to your Author Centre → manuscripts returned to author (ERL-124325).
3. Replace the existing main source file with the new `reviewed_PA_impact_population_counts_LMICs.docx` from this folder. Keep file designation as the main manuscript source.
4. (Recommended) also upload `reviewed_Supplementary_Materials.docx` from this folder alongside the existing PDF, so that the supplementary source is also non-anonymized.
5. Reply to the editor's email using the content of `reply_to_mail2.md` (or paste it as a comment inside the ScholarOne submission, whichever the system requires). The essential confirmation is: "the updated source file containing the author information has been uploaded".

### Step 3 — Reply to mail1 (supplementary titles & descriptions)

1. Reply to the email from Jenny Albinus (letter ref: `HYP:SuppDataInfo`).
2. Paste the six entries from `reply_to_mail1.md`, one per supplementary file.
3. Each title is already within the 30-character limit; each description is within the 30-word limit. Only the six supplementary items (Supplementary Materials PDF, Figures S1–S2, Tables S1–S3) require this metadata — figures/tables embedded in the main text do not.
4. **Before sending**, please check/adjust the descriptions:
   - `reply_to_mail1.md` contains a placeholder description for Table S3 ("Sensitivity of 2020 headline estimates to alternative assumptions ..."). Confirm it matches what Table S3 actually shows in your submission, or send me the correct wording.

### Step 4 — Keep the repository consistent

The two rendered .docx files in the root (`reviewed_PA_impact_population_counts_LMICs.docx`, `reviewed_Supplementary_Materials.docx`) have also been refreshed. Commit the YAML change in the two .qmd files (and the resubmission/ folder if you wish) so the repo stays in sync with what you uploaded.

```bash
git add reviewed_PA_impact_population_counts_LMICs.qmd \
        reviewed_Supplementary_Materials.qmd \
        resubmission
git commit -m "Add author information to manuscript and supplementary sources (ERL-124325)"
```

## Checklist — editor requests satisfied

### mail1 (HYP:SuppDataInfo)
- [x] Short title (≤ 30 chars) provided for each supplementary file
- [x] Short description (≤ 30 words) provided for each supplementary file
- [x] Exact filename provided for each supplementary file
- [ ] Reply sent from your ERL author account (to be done by you — Step 3)

### mail2 (SYS: OnHold:Au)
- [x] Source file re-rendered with full author information (name, affiliation, ORCID, corresponding email) visible under the title
- [x] Same author information added to the supplementary source
- [ ] New source file uploaded in ScholarOne (to be done by you — Step 2)
- [ ] Confirmation sent to the editorial office (to be done by you — Step 2)
