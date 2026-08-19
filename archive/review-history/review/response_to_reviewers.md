# Response to reviewers

## Preamble to the editor

We are grateful for the opportunity to resubmit this manuscript following the reject-and-resubmit decision. We have used it to undertake a substantial rewriting, both in analytical scope and conceptual framing.

The most significant change concerns the treatment of India. During the revision process, we investigated why India's PA coverage in our data was so low relative to commonly cited figures. We ralized that the World Database on Protected Areas (WDPA) contains no nationally designated protected areas for India -- neither in the May 2021 release used in our analysis nor in the March 2026 release. The WDPA records only internationally designated sites for India (Ramsar wetlands, World Heritage sites, and UNESCO-MAB Biosphere Reserves), covering approximately 0.5% of India's territory, whereas India's actual PA network exceeds 900 nationally designated sites covering roughly 5% of its land area. This data gap, which has been acknowledged in the literature (Wani et al. 2025; Zhang et al. 2025), means that including India in a global analysis based on the WDPA would produce erroneous results. India has accordingly been excluded from the analysis, reducing the sample from 76 to 75 LMICs.

This discovery has a direct bearing on Referee 2's Comment 2, which questioned the initial manuscript's treatment of India. The reviewer was right to be sceptical: what we had framed as an "influence diagnostic" was in fact an artefact of missing data. The exclusion of India is, we believe, a more honest and defensible choice than reporting results that are mechanically depressed by a known WDPA data gap.

Other major changes include: reframing the paper to speak directly to the PA impact evaluation literature; decomposing all results by IUCN management category; replacing WorldPop with GHSL as primary dataset; removing all policy recommendations; and adding a detailed treatment of temporal uncertainty from missing designation years. Each change is described below in response to the relevant reviewer comment.

## Referee 1

**Comment 1** (West Bank and Gaza / PAs adjacent to high-income countries)

We thank the reviewer for drawing attention to boundary cases such as Palestine. This question proceeds from how the geoBoundaries dataset refers to these territories and it had also raised our concern early in the project. We reported it as an issue to the geoBoundaries maintainers in Feb. 2025 ([geoBoundaries GitHub issue #4206](https://github.com/wmgeolab/geoBoundaries/issues/4206)), but they replied that this naming reflects their institutional referencing conventions. Following the reviewer remark and in accordance with the UN convention, we now refer to "Palestine (West Bank and Gaza)" in the revised manuscript.

Regarding protected areas located near national borders, we clarified in the Methods section (Analytical approach) that spatial computations are conducted within national boundaries and that proximity buffers are truncated at international borders -- whether neighboring countries are low-, lower-middle-, or higher-income. This rule, consistent with the assumption that populations are nationally confined, ensures that all computations remain within the territorial limits of each country included in the analysis.

**Comment 2** (data and code sharing)

We thank the reviewer for this suggestion. The initial manuscript already indicated that "the GEE JavaScript code, R code, and output dataset are included in the replication package available online (Anonymized)." In the revised version, we make this reference explicit by providing the repository links. The full replication package is available on GitHub (https://github.com/BETSAKA/population_near_protected_areas) and permanently archived on Software Heritage (swh:1:dir:2c8237e16e74d94043e37899aed0de6f262ed420). It contains all scripts and outputs required to reproduce the analysis and figures.

**Comment 3** (move Table 1 to appendix)

We thank the reviewer for this suggestion. In the revised manuscript, the detailed country-level table has been moved to the Supplementary Materials (now Table S1). Aggregate population magnitudes are now provided inline in the Results section. In addition, the main text includes a new table that translates common spatial exposure definitions from the impact evaluation literature (in-out comparisons, 10 km buffers, by IUCN category) into the implied population sizes. This restructuring -- combined with the broader reorganization prompted by Referees 2 and 3 -- substantially improves the readability of the main text while preserving the country-level detail in the appendix.

**Comment 4** (scale data points to total population in Figure 3.3)

The figure set has been substantially restructured in response to Referees 2 and 3, who required a decomposition by IUCN management category. The growth scatter plot the reviewer refers to (initial section 3.3) and the static 2020 scatter with population-scaled dots (initial section 3.4) have both been replaced. Population exposure is now presented through stacked bar charts decomposed by PA category (Figure 1) and a country-level lollipop chart showing temporal change (Figure 2). The reviewer's suggestion to convey population size through dot scaling is no longer directly applicable given the new figure design, but we have aimed to address the underlying concern for visual informativeness through these revised figures.

**Comment 5** (swap sections 3.3 and 3.4)

The section structure has been fundamentally reorganized in the revised manuscript to accommodate the IUCN category decomposition and the new framing around impact evaluation exposure definitions. The original sections 3.3  and 3.4  have been replaced. The revised Results section proceeds as follows: population magnitudes with IUCN decomposition, change over time by PA category (Figure 1), uncertainty from missing designation years, cross-country variation (Figure 2), and robustness of GHSL vs WorldPop estimates (Figure 3). The reviewer's original suggestion about presenting the static cross-section before the temporal change is no longer directly applicable given this new structure, which leads with aggregate magnitudes and then unpacks their temporal and cross-country components.

**Comment 6** (add references for WorldPop validation)

This was an omission on our part. We now cite the three studies we relied on to characterize the accuracy of gridded population datasets: Chen et al. (2020), Leyk et al. (2019), and Thomson et al. (2022). These references appear in both the Data Sources subsection and the Data Limitations discussion.

## Referee 2

- Comment 1. unvalidated population data and unclear DHS validation:

We agree that the initial manuscript gave an unclear impression of our data strategy. The initial version used WorldPop as the primary dataset and attempted a validation exercise using DHS geolocated survey data, which the reviewer rightly identified as circular given the 10 km GPS displacement applied to DHS clusters.

In the revised manuscript, we have made two changes. First, we now use GHSL as our primary dataset -- identified by Chen et al. (2020) as the closest to administrative census totals among four globally harmonized gridded datasets -- and use WorldPop as a robustness check. The concordance between the two (Figure 3, Table S2) supports the conclusion that our population orders of magnitude are robust to the choice of gridded source, even though point estimates for individual countries may differ. Second, the DHS validation exercise has been removed entirely. 

- Comment 2. Treatment of India:

The reviewer's scepticism about our initial treatment of India was well founded. In the original manuscript, we presented results "with and without India" as an influence diagnostic. During the revision, we noticed that the literature commonly cited a PA extent of 5% of Indian territory and investigated why India's PA coverage in our data was so low (approximately 0.5% of its territory).

We realized that the WDPA contains **zero nationally designated protected areas for India**. We verified this in both the May 2021 release (used in our analysis) and the March 2026 release. The WDPA records only internationally designated sites for India: 75 Ramsar wetlands, 8 World Heritage sites, and 7 UNESCO-MAB Biosphere Reserves. India's actual PA network -- comprising over 900 nationally designated sites (national parks, wildlife sanctuaries, community reserves, and conservation reserves) totalling roughly 170,000 km² -- is entirely absent from the database. This gap is documented in the literature: Wani et al. (2025) note that India has indicated the process of data submission to the WDPA is in progress, and Zhang et al. (2025) acknowledge that the absence of updated WDPA data for India may underestimate conservation extent in global analyses.

Because India accounts for approximately 36% of the total LMIC population, including it with incomplete PA data artificially biaised our results. What we had initially framed as "India's high leverage on aggregates" was in fact a data completeness issue. India has therefore been excluded entirely from the revised analysis. The sample covers 75 LMICs.

In the revised manuscript, the India exclusion is explained in the Methods section (with references to the WDPA data gap) and discussed as a limitation in the Discussion. We do not present any "with and without India" comparisons.

- 3. conflation of proximity with impact and unwarranted policy recommendations:

We accept this criticism. The revised manuscript has been substantially restructured to eliminate any conflation between spatial proximity and socio-economic impact. The paper is now explicitly framed as quantifying the demographic scale implied by exposure definitions used in the impact evaluation literature. It does not assess conservation impacts and does not make policy recommendations. A new Discussion section ("Scope and interpretive boundaries") states explicitly that the paper does not estimate welfare effects, does not rank conservation policies, and does not treat spatial proximity as evidence of being affected by a PA. The 10 km buffer is presented as a common reference perimeter in the literature, not as a known threshold for social or ecological effects. The title itself has been changed to reflect this reframing.

- Comment 4: omission of indigenous populations and traditional communities

We acknowledge that the initial manuscript failed to mention indigenous peoples and traditional communities. A new paragraph in the Discussion now addresses this limitation. It notes that indigenous peoples -- estimated at 476 million people worldwide (UNDESA, 2021) -- are disproportionately represented among populations living within or adjacent to protected areas and often bear the most direct consequences of conservation management (Schleicher et al. 2019). It also states that our gridded population data cannot identify these groups and that the aggregate magnitudes we report encompass communities with very different relationships to protected areas -- from urban residents to indigenous groups whose livelihoods and territorial rights may be directly affected by PA governance. We agree that disaggregating these populations remains an important challenge for the field, and we state this as an explicit limitation.

## Referee 3 

- General assessment: analysis limited by absence of PA type and recommendations not grounded in data

We appreciate the constructive framing of the report and have followed reviewer's guidance closely. The revised manuscript implemented the third option outlined in the report: we now include PA type within the analysis, decomposing all population estimates by IUCN management category (strict: I-III, non-strict: IV-VI, and PAs whose IUCN category is not recorded), following the classification grouping used by Leberger et al. (2020). This decomposition is carried through all figures and tables, making visible that the majority of PA-adjacent populations in LMICs lives near non-strict or unrecorded-category PAs.

At the same time, we followed the first option's injunction to limit discussion to what the data actually show. All policy recommendations have been removed from the revised manuscript. The discussion now focuses on what the composition statistics imply for interpreting impact evaluation results (that the populations implicitly defined by spatial exposure criteria are large, heterogeneous across PA management categories, and dominated by non-strict protection regimes) without making claims about what those impacts are.

Regarding the suggestion to compare with upper-middle and high-income countries: we agree this would be informative but consider it beyond the scope of this paper, which is anchored in the impact evaluation literature on LMICs. We note this as a direction for future work.

- Pg 2 Ln 32-36 / confusing statement about South Sudan and Timor-Leste

The revised manuscript now states simply: "South Sudan and Timor-Leste did not exist as sovereign states at the beginning of our analysis period (2000), so we did not include them in the analysis."

- Pg 3 Ln 24 / Accounting for different levels of protection

The revised manuscript now decomposes all results by IUCN management category: strict (Ia-III), non-strict (IV-VI), and PAs whose IUCN category is not recorded. This decomposition appears in Table 1, Figures 1 and 2, Figure S1, and is discussed in a dedicated section ("PA categories matter for interpreting impacts"). We agree with the panel member that these categories correspond to very different management regimes in terms of their implications for local populations. We state clearly that assessing what these regimes imply for local welfare is the domain of impact evaluation studies, not the purpose of this paper.

- Pg 14 Ln 29 /Broken reference got Naidoo et al. 2019

This was a rendering error in the initial submission. It has been corrected.

- Pg 15 Ln35-37 / Recommendations not grounded in data 

We accept this criticism entirely. The revised manuscript contains no policy recommendations. The Discussion section discusses what the compositional shift toward non-strict and unclassified PAs implies for interpreting the heterogeneity of findings in the impact evaluation literature, but does not prescribe policies. A dedicated section ("Scope and interpretive boundaries") states explicitly that the paper does not assess whether living near a PA makes people better or worse off, and does not treat all PAs as having equivalent implications for local populations.

## Summary of major changes

The manuscript has been substantially rewritten. The main changes relative to the initial submission are:

1. Reframing: The paper is no longer framed as a study of populations "affected by" conservation. It now positions itself explicitly as quantifying the demographic scale implied by spatial exposure definitions used in the PA impact evaluation literature. The title has been changed accordingly.

2. India excluded due to WDPA data gap: India has been excluded from the analysis because the WDPA contains zero nationally designated PAs for this country. This was discovered during the revision and is documented in the Methods and Discussion. The sample covers 75 LMICs.

3. IUCN category decomposition: All results are now decomposed by IUCN management category groupings (strict, non-strict, and PAs whose IUCN category is not recorded in the WDPA), following Leberger et al. (2020). This responds to Referees 2 and 3's criticism that treating all PAs as homogeneous was insufficient.

4. Primary dataset: GHSL replaces WorldPop as the primary population dataset, following Chen et al. (2020). WorldPop is used as a robustness check.

5. DHS validation removed: The initial DHS-based validation exercise has been removed.

6. Policy recommendations removed: All prescriptive policy language has been removed. The Discussion is limited to implications for interpreting impact evaluation results.

7. Indigenous and traditional communities: A new paragraph acknowledges that gridded data cannot identify indigenous peoples and traditional communities, who are disproportionately represented among PA-adjacent populations.

8. Temporal uncertainty: A new supplementary figure (Figure S2) and accompanying discussion make visible the uncertainty introduced by missing designation years  in the WDPA.

9. Missing status year: In the initial submission, we had inadvertently treated STATUS_YR = 0 (meaning "not reported") as an early designation date, overestimating the PA network confirmed as existing before 2000. This has been corrected: PAs with STATUS_YR = 0 are now treated as missing temporal metadata throughout.

10. Data description in supplementary material: A new Section of the supplementary material documents the GEE processing pipeline, variable definitions, and WDPA field conventions used in the analysis.
