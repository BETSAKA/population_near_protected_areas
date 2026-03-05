// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
)

#show: doc => article(
  title: [How Many People Live Near Protected Areas in Developing Countries? Estimates from Gridded Population Data (2000--2020)],
  authors: (
    ( name: [Florent Bédécarrats],
      affiliation: [],
      email: [] ),
    ),
  abstract: [Impact evaluations of protected areas routinely define "local populations" through spatial proximity, yet the demographic scale implied by these definitions is almost never quantified. We estimate population counts inside protected areas and within 10 km of their boundaries for 75 low- and lower-middle-income countries (LMICs) in 2000 and 2020, using the World Database on Protected Areas and globally harmonized gridded population data (GHSL, with WorldPop as robustness check). India is excluded because its national protected areas are not publicly available in the WDPA. Results are decomposed by IUCN management category (strict and non-strict). In 2020, approximately 61 million people (2.5% of their combined population) lived inside protected areas and over 900 million (36.6%) lived inside or within 10 km of their boundaries. These figures are substantially larger than those estimated for 2000, reflecting both PA expansion and demographic growth. Most PA-adjacent populations are located near non-strict protected areas (IUCN categories IV to VI) rather than strict protection regimes (IUCN categories I to III). These population orders of magnitude are relevant for interpreting the scope of PA impact evaluations but do not themselves constitute impact estimates.],
  abstract-title: "Abstract",
  sectionnumbering: "1.1.a",
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

= Introduction
<introduction>
Protected areas (PAs) were primarily established to achieve ecological purposes (Maxwell et al. 2020). Their implications for human populations have nevertheless been a persistent concern in conservation research (Adams et al. 2004), particularly in low and lower middle income countries where rural households account for a large share of the population and global poverty (Castañeda et al. 2018; Byerlee and Janvry 2008). The expansion of PAs makes this issue increasingly salient: from 2000 to 2023, global terrestrial PA coverage expanded by 62%, reaching 17.6% of land area (UNEP-WCMC and IUCN 2024). This growth is set to accelerate with the COP15 commitment to protect 30% of terrestrial land by 2030 (2022).

In the literature assessing the social and economic implications of protected areas, the effects of conservation are consistently framed as local in nature. Yet what constitutes a "local population" is rarely made explicit in demographic terms, even though these definitions implicitly determine the scale to which empirical results apply and can be compared. Two dominant notions of locality recur. One focuses on populations living inside protected areas, directly subject to restrictions and management regimes. The other extends locality to populations living outside but near protected areas, on the premise that social effects attenuate with distance, reflecting spatial limits to resource use and interaction documented in earlier work (West, Igoe, and Brockington 2006).

A 10 km buffer around PA boundaries has emerged as a widely used threshold. It was the inclusion criteria in the widely cited systematic review on of conservation outcomes by Oldekop et al. (2016). It is also pervasive in individual empirical studies. In one of the few global-scale assessments, Naidoo et al. (2019) analyzed conservation wellbeing outcomes across 34 low- and middle-income countries with Demographic and Health Surveys, defining exposure to protected areas as residing at 10 km or less from PAs. Fisher et al. (2024) used Afrobarometer data to assess social wellbeing, explicitly classifying households based on their distance to protected area boundaries, including a 10 km threshold. Among smaller-scale impact evaluations, spatial definitions vary but converge around similar thresholds. Kandel et al. (Kandel et al. 2022) meta-analyze 30 counterfactual studies. Eleven define exposure through in--out comparisons between populations residing inside protected areas and those outside; two of these explicitly exclude populations living within 10 km and 20 km, respectively, from the control group because they may be affected by spillover effects. Ten studies use distance-based or buffer definitions, five of which refer to a 10 km threshold. Nine rely on administrative units that overlap protected areas to varying degrees, reflecting the spatial resolution of available socioeconomic data. Across all these designs, the population sizes implied by exposure definitions are not reported.

This paper addresses this gap by quantifying the demographic scale of conservation exposure definitions. Rather than assessing conservation impacts or making claims about them, it asks a simpler and different question: when studies of PA social impacts define "local populations" spatially, how many people does this actually correspond to, and how has this magnitude evolved since 2000, as protected area systems have expanded? Using globally harmonized PA boundaries and gridded population data, the paper quantifies population counts inside PAs and within a reference buffer commonly used in the literature, and documents how these numbers vary across countries, protected area categories, and time. Providing population orders of magnitude is necessary for interpreting the scope and external validity of estimated conservation social impacts.

= Methods
<methods>
== Study Area
<study-area>
The analysis focuses on countries classified by the World Bank as low-income or lower-middle-income economies in 2020 (Vaggi 2017). South Sudan and Timor-Leste did not exist as sovereign states at the beginning of our analysis period (2000), so we did not include them in the analysis.

India is excluded because data on its nationally designated protected areas are restricted from public access in the WDPA. The WDPA records approximately 990 protected areas for India, but only 90 internationally designated sites (Ramsar wetlands, World Heritage sites, UNESCO-MAB Biosphere Reserves) are publicly available, covering approximately 0.5% of India's land area; data on the remaining 900 nationally designated sites cannot be viewed or downloaded (UNEP-WCMC 2026). Including these restricted sites, India's terrestrial PA coverage reaches approximately 7.7%. India has stated that the process of making its data publicly accessible is in progress (Wani et al. 2025), and this data gap has been noted in global assessments (Zhang et al. 2025). Because India accounts for approximately 36% of the total LMIC population, including it with incomplete spatial data would mechanically depress aggregate PA coverage and exposure statistics. The analysis therefore covers 75 LMICs.

The population of interest is defined following spatial exposure concepts commonly used in the literature evaluating the impacts of protected areas. Two reference perimeters are considered: residence inside protected areas, and residence within a 10 km buffer around protected area boundaries. These perimeters are used as illustrative thresholds to quantify the population sizes implied by commonly used definitions of "local populations" in conservation impact studies, not as claims about the spatial extent or magnitude of actual conservation impacts.

== Data Sources
<data-sources>
National boundaries were drawn from geoBoundaries version 6.0.0, a standardized and openly available global dataset designed for cross-country comparative analysis (Runfola et al. 2020).

Protected area boundaries, status, and designation year are obtained from the World Database on Protected Areas (WDPA), compiled by UNEP-WCMC in collaboration with IUCN (Bingham et al. 2019; UNEP-WCMC and IUCN 2023). We use the May 2021 WDPA release, which underpinned the final assessment of Aichi Target 11 under the Convention on Biological Diversity (CBD) Strategic Plan for Biodiversity 2011--2020 (UNEP-WCMC and IUCN 2021). This snapshot reflects the culmination of reporting efforts by CBD member countries for the 2020 milestones, and is therefore more likely to be complete, while limiting the inclusion of post-2020 designations recorded with missing status years.

Population estimates are drawn from two globally harmonized gridded datasets. The Global Human Settlement Layer (GHSL) combines census-based population counts with remote sensing and ancillary data to produce gridded estimates at 250 m resolution (Freire et al. 2016). WorldPop applies a machine-learning approach trained on census data and spatial covariates to generate population estimates at 100 m resolution (Stevens et al. 2015). Both datasets have been evaluated against geolocated census data in contexts where such data are available (Leyk et al. 2019; Chen et al. 2020; Thomson et al. 2022). GHSL is used as the main reference dataset, while WorldPop is used to assess the robustness of population orders of magnitude.

== Analytical approach
<analytical-approach>
We used Google Earth Engine (GEE), a cloud-based platform for large-scale environmental analysis (Gorelick et al. 2017), to process high-resolution population estimates within and around protected areas across the 75 countries in 2000 and 2020.

Protected areas were filtered to retain only designated, established, or inscribed sites, excluding UNESCO-MAB Biosphere Reserves -- as is common practice in conservation analyses due to their often limited legal protection (Hanson 2022; Coetzer, Witkowski, and Erasmus 2014) -- and purely marine protected areas, since this study focuses on terrestrial populations. Protected areas were classified into three groups based on reported IUCN management categories, following Lebergers et al. (2020): strict protected areas (status I to III), non-strict protected areas (status IV to VI), and protected areas whose IUCN category is not recorded in the WDPA. Protected areas were considered as existing by the end of the study period if they had a reported designation year less than or equal to 2020 or if the designation year was missing in the May 2021 WDPA release. For analyses of change over time, protected areas with missing designation year were excluded to avoid ambiguity in temporal classification.

For each designation-period class and protected area category, binary raster masks were constructed and expanded using a 10 km buffer to represent proximity-based reference perimeters. Population counts and protected area surface were computed through zonal aggregation within national boundaries. Spatial computations are conducted within national boundaries, and proximity buffers around protected areas are truncated at international borders.

We conducted the final analyses in R (R Core Team 2023). The complete replication package, including the GEE JavaScript code, R code, and output dataset, is available on GitHub (https:\/\/github.com/BETSAKA/population\_near\_protected\_areas) and archived on Software Heritage (swh:1:dir:2c8237e16e74d94043e37899aed0de6f262ed420).

= Results
<results>
== Population magnitudes
<population-magnitudes>
Restricting to populations living inside protected areas refers to a relatively small population: 61 million people, or 2.5% of the combined population of these 75 countries. Extending the exposure definition to include people living within 10 km of PA boundaries increases the encompassed population roughly fifteenfold, to 902 million (36.6%). For comparison, in 2000 (restricted to PAs with recorded designation years), 17.9 million people lived inside PAs and 301.1 million inside or within 10 km (18.2% of the population).

The decomposition by PA category (Figure 1) shows that populations adjacent to non-strict PAs (IUCN categories IV to VI) substantially exceed those adjacent to strict PAs (IUCN categories I to III): 244 million inside or within 10 km of non-strict PAs, compared with 196 million near strict PAs. An additional 462 million people live near PAs whose IUCN category is not recorded in the WDPA.

To connect these magnitudes to the impact evaluation literature, Table 1 translates common spatial exposure definitions into population orders of magnitude.

#{set text(font: ("system-ui", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji") , size: 8.25pt); table(
  columns: 3,
  align: (left,center,center,),
  table.header(table.cell(align: center, colspan: 3, fill: rgb("#ffffff"))[#set text(size: 10.5pt , weight: "regular" , fill: rgb("#333333")); Table 2: Implied population magnitudes by evaluation design],
    table.cell(align: center, colspan: 3, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 0.85em , weight: "regular" , fill: rgb("#333333")); All PAs in 2020 (incl. unknown designation year), 75 LMICs (excl. India), GHSL population estimates],
    table.cell(align: bottom + left, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); Evaluation design (exposure definition)], table.cell(align: bottom + center, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); Population (millions)], table.cell(align: bottom + center, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); % of LMIC population],),
  table.hline(),
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[In-out comparison (inside PAs only)], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[60.7], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[2.5],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Inside PAs or within 10 km], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[902.1], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[36.6],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Strict PAs only (IUCN Ia-III): inside or within 10 km], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[196.1], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[8.0],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Non-strict PAs only (IUCN IV-VI): inside or within 10 km], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[243.5], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[9.9],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[Unknown category PAs: inside or within 10 km], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[462.5], table.cell(align: horizon + center, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[18.8],
  table.hline(),
  table.footer(table.cell(colspan: 3)[Source: WDPA May 2021, GHSL 2020. \'Within 10 km\' includes populations inside PAs.],),
)}
An in-out comparison -- where "treated" populations are those residing inside PAs -- implies a study population of approximately 61 million people (2.5% of the population of these 75 countries). When the spatial definition extends to populations within 10 km, the implied population rises to 902 million (36.6%). Restricting to strict PAs only yields 196 million, while non-strict PAs contribute 244 million. These numbers do not appear in the studies reviewed by Kandel et al. (2022), yet they define the implicit scope to which estimated impacts apply.

== Change over time by PA category
<change-over-time-by-pa-category>
Comparing 2020 with 2000 is subject to an important caveat: designation years are missing for 690 out of 3,154 PAs in the WDPA (across the 75 LMICs studied), so the set of PAs that can be confirmed as existing before 2001 is a subset of those known to exist by 2020. The 2000-2020 comparison is therefore restricted to PAs with recorded designation years and should be interpreted as indicative of broad trends rather than a precise measure of change.

With this caveat in mind, Figure 1 shows how PA-adjacent populations evolved between 2000 and 2020 across PA categories, separately for populations inside PAs and within the 10 km buffer.

#box(image("results/figure_1.png"))

Growth in exposed populations occurred across all three PA categories. The net change (2020 minus 2000) for the 75 LMICs combined (Figure S1, Supplementary Materials) shows that the population inside or within 10 km of non-strict PAs grew by approximately 108 million, that inside or within 10 km of PAs with unrecorded IUCN category by 111 million, and that near strict PAs by 77 million.

This overall increase reflects two distinct mechanisms: population growth near PAs that already existed by 2000, and the designation of new PAs between 2000 and 2020. Using national population growth rates to approximate what 2000-era PA footprints would have contained under 2020 population levels, we estimate that roughly 53% of the increase is attributable to demographic growth around pre-existing PAs, and 47% to PA network expansion. The two mechanisms contribute in approximately equal measure at the global LMIC level, though their relative importance varies substantially across countries.

== Uncertainty from missing designation years
<uncertainty-from-missing-designation-years>
The WDPA records a designation year for each protected area, but this field is not reported for 690 of the 3,154 PAs in our 75 LMICs (see Supplementary Materials, Section A). These PAs are known to exist, since they appear in the May 2021 WDPA release, but they cannot be unambiguously assigned to a specific designation period. Figure S2 (Supplementary Materials) shows, for each country, the gap between two counts: one restricted to PAs whose designation year is explicitly recorded as falling on or before 2020, and one that also includes PAs with missing designation year. The gap represents the uncertainty attributable to incomplete temporal metadata in the WDPA.

For 54 of the 75 countries, the gap is below 5 percentage points, indicating that the large majority of PAs have recorded designation years. However, in several countries -- notably those where IUCN categories are missing for large protected areas -- the gap exceeds 10 percentage points. For these countries, population figures are sensitive to how missing designation years are treated.

== Cross-country variation
<cross-country-variation>
Figure 2 displays, for each country, the percentage of the national population residing inside PAs or within 10 km, in 2000 and 2020. The wide cross-country variation -- from less than 1% to over 90% -- underscores that aggregate statistics mask considerable heterogeneity. In 66 of the 75 countries, the share increased between 2000 and 2020 (blue segments), consistent with PA expansion. A small number of countries experienced slight declines, typically where PA coverage was stable while urban population growth reduced the relative weight of PA-adjacent populations. Full country-level detail is provided in Table S1 (Supplementary Materials).

#box(image("results/figure_2.png"))

== Robustness: GHSL vs WorldPop
<robustness-ghsl-vs-worldpop>
Figure 3 compares GHSL and WorldPop estimates of the percentage of national population inside PAs and within 10 km, for all PAs present in the 2020 WDPA release. Points cluster around the 1:1 line, indicating broad agreement between the two datasets at the country level.

#figure([
#box(image("results/figure_3.png"))
], caption: figure.caption(
separator: "", 
position: bottom, 
[
#block[
]
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-figure_3>


Discrepancies are most pronounced for populations inside PAs, where the very small absolute population counts amplify relative differences between the two datasets' spatial allocation models. For the broader 10 km buffer perimeter, agreement is substantially stronger. Table S2 (Supplementary Materials) lists the countries where the absolute difference between GHSL and WorldPop exceeds 5 percentage points and the relative difference (computed as the absolute difference divided by the GHSL estimate) exceeds 10%. These discrepancies do not alter the order-of-magnitude conclusions: both datasets indicate hundreds of millions of people residing near protected areas in LMICs.

= Discussion
<discussion>
== Demographic scale and the impact evaluation literature
<demographic-scale-and-the-impact-evaluation-literature>
The central finding of this paper is that the populations encompassed by standard spatial exposure definitions in PA impact evaluations are large -- on the order of several hundred million people. Among PAs with recorded designation years, these populations are substantially larger in 2020 than in 2000, driven both by PA expansion itself and by population increase in areas where PAs were already present.

These population orders of magnitude are absent from the impact evaluation literature - whether in meta-analyses such as Kandel et al. (2022) or in large-scale assessments such as Naidoo et al. (2019) and Fisher et al. (2024). Yet they define the scope to which estimated impacts implicitly apply. When a study estimates the welfare effect of living within 10 km of a PA in a given country, and that effect is used to inform policy discussions about PA expansion, it is relevant to know that the population meeting this exposure criterion across LMICs numbers in the hundreds of millions - and that this population is growing.

This matters for at least three reasons. First, it sets a demographic frame for external validity: a treatment effect estimated on a sample of a few thousand households in a handful of countries implicitly speaks to a reference population that is orders of magnitude larger. Second, it reveals compositional heterogeneity: the "local population" implied by a 10 km buffer near a strict national park in a sparsely populated area is demographically very different from that implied by the same buffer near a densely inhabited community conservation area. Third, the heterogeneity of findings in the impact evaluation literature may itself partly reflect the heterogeneity of the populations being studied -- populations that differ not only across countries but across PA management categories, as our decomposition shows.

== PA categories matter for interpreting impacts
<pa-categories-matter-for-interpreting-impacts>
The decomposition by IUCN management category reveals that populations adjacent to non-strict PAs (IUCN IV-VI) substantially exceed those adjacent to strict protection regimes (IUCN I-III). This pattern held in both 2000 and 2020.

This has direct relevance for interpreting impact evaluation results. The term "protected area" encompasses management regimes with vastly different implications for local populations -- from national parks that prohibit entry to community conservation areas that allow resource extraction -- yet comparisons of impacts across management categories remain rare (Kandel et al. 2022; Oldekop et al. 2016). Our results show that the demographic weight of non-strict PAs exceeds that of strictly protected areas in LMICs. Studies that treat all PAs as a homogeneous treatment condition may therefore conflate very different exposure regimes. PAs whose IUCN category is not recorded in the WDPA represent an additional source of ambiguity: for these areas, even the management regime is unknown.

Assessing what these different management regimes imply for local welfare is the domain of impact evaluation studies, not the purpose of this paper. What we document is that the population living near PAs is concentrated around non-strict protection regimes.

== Data limitations
<data-limitations>
Our estimates rely on globally harmonized gridded population datasets. These datasets have known limitations. GHSL and WorldPop are constructed from census data combined with remote sensing and machine learning, and their accuracy varies across settings. Chen et al. (2020) evaluated four global gridded datasets and found that GHSL exhibited the closest correspondence to administrative census totals, while WorldPop showed larger deviations from census totals but high spatial consistency with other datasets. Leyk et al. (2019) and Thomson et al. (2022) provide additional assessments of WorldPop's cell-level accuracy. We use GHSL as our primary dataset and WorldPop as a robustness check. The concordance between the two (Figure 3) supports the conclusion that population orders of magnitude are robust to the choice of dataset, even if point estimates for individual countries may differ.

The exclusion of India deserves further comment. India accounts for approximately 36% of the total LMIC population, and its actual PA network -- 900 nationally designated sites, for a terrestrial coverage of approximately 7.7% (UNEP-WCMC 2026) -- is substantial. However, India has restricted public access to these data in the WDPA, making only internationally designated sites (Ramsar wetlands, World Heritage sites) available, covering less than 1% of Indian territory. This data restriction has been documented: Wani et al. (2025) note that India has stated the process of making its data publicly accessible is in progress, and Zhang et al. (2025) acknowledge that the absence of publicly available Indian PA data may underestimate conservation extent in global analyses. Including India with incomplete spatial data would mechanically depress aggregate exposure statistics, producing artefactual rather than substantive results. We therefore exclude India entirely. This means our estimates do not cover the world's most populous lower-middle-income country, which is a significant limitation. If India's PA data were publicly available, the total number of people estimated to live near protected areas would increase substantially. At the same time, the share of the population concerned would likely decrease, given India's comparatively low protected area coverage.

The treatment of missing designation years introduces temporal ambiguity (see Appendix A). In our 2000-2020 comparison, we restrict attention to PAs with a recorded designation year, which means we are comparing a subset of PAs known to exist by 2000 with a different subset known to exist by 2020. This comparison is informative about orders of magnitude but should not be read as a precise estimate of temporal change, since some PAs with missing designation years may in fact have been established before 2000. Figure S2 (Supplementary Materials) makes the resulting uncertainty band visible at the country level.

== Scope and interpretive boundaries
<scope-and-interpretive-boundaries>
This paper quantifies population magnitudes. It does not estimate socio-economic impacts, welfare effects, or causal relationships. No claim is made - explicitly or implicitly - about whether living near a protected area makes people better or worse off. Such questions require impact evaluation designs with appropriate identification strategies, which are beyond the scope of this work.

We also do not rank conservation policies or PA categories in terms of desirability. The decomposition by management category is intended to describe the demographic composition of PA-adjacent populations, not to evaluate the relative merits of different protection regimes.

The paper does not claim that spatial proximity constitutes "being affected by" a protected area. The 10 km buffer is used because it is a common reference perimeter in the impact evaluation literature, not because it represents a known threshold for social or ecological effects. The actual spatial extent of PA-related effects on livelihoods is an empirical question that varies by context and cannot be resolved by our data.

A related limitation concerns population differentiation. Our estimates treat all inhabitants within a given perimeter as demographically equivalent. In practice, populations near protected areas are heterogeneous in their dependence on natural resources, their legal status, and their vulnerability to land-use restrictions. Indigenous peoples and traditional communities - estimated at 476 million people worldwide across more than 90 countries (United Nations Department of Economic and Social Affairs 2021) - are disproportionately represented among populations living within or adjacent to protected areas, and often bear the most direct consequences of conservation management (Schleicher et al. 2019). Our gridded population data cannot identify these groups. The aggregate population magnitudes we report therefore encompass communities with very different relationships to protected areas, from urban residents who may experience diffuse ecosystem service benefits to indigenous groups whose livelihoods and territorial rights may be directly affected by PA governance. Disaggregating these populations remains an important challenge for the field.

= Conclusion
<conclusion>
Across 75 low- and lower-middle-income countries, protected areas are adjacent to populations on the order of several hundred million people. Restricting to PA interiors captures a relatively small population; extending to 10 km increases the encompassed population roughly fifteenfold. Among PAs with recorded designation years, these figures are substantially larger in 2020 than in 2000, reflecting both PA expansion and population increase -- though incomplete temporal metadata in the WDPA means that the precise magnitude of this change cannot be established with certainty. Populations near non-strict PAs (IUCN IV-VI) substantially exceed those near strict protection regimes (IUCN I-III). India, the largest LMIC by population, had to be excluded because its national PA data are absent from the WDPA; if included, global counts of people living near protected areas would likely increase in absolute terms, though the corresponding population share might decrease.

These facts are relevant for how conservation impact evaluation results are interpreted, compared, and generalized. They do not tell us what those impacts are. But they establish the demographic scale that spatial exposure definitions imply -- a scale that is almost never reported in the studies that use them.

#pagebreak()
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
[
Bibliography
]
)
]
#block[
#block[
Adams, William. M., Ros Aveling, Dan Brockington, Barney Dickson, Jo Elliott, Jon Hutton, Dilys Roe, Bhaskar Vira, and William Wolmer. 2004. “Biodiversity Conservation and the Eradication of Poverty.” #emph[Science] 306 (5699): 1146--49. #link("https://doi.org/10.1126/science.1097920");.

] <ref-adams2004>
#block[
Bingham, Heather C., Diego Juffe Bignoli, Edward Lewis, Brian MacSharry, Neil D. Burgess, Piero Visconti, Marine Deguignet, Murielle Misrachi, Matt Walpole, and Jessica L. Stewart. 2019. “Sixty Years of Tracking Conservation Progress Using the World Database on Protected Areas.” #emph[Nature Ecology & Evolution] 3 (5): 737743. #link("https://idp.nature.com/authorize/casa?redirect_uri=https://www.nature.com/articles/s41559-019-0869-3&casa_token=Zmluq7JmhxUAAAAA:Id6cAJrCDpmtydpiga8LlmkB5wb9a1O_bwCGwCeA1gz314-F6VT5ekCOeVMJo_Zrpbh_uuoXf0fwD3Txjg");.

] <ref-bingham2019>
#block[
Byerlee, Derek, and Alain de Janvry, eds. 2008. #emph[World Developpement Report 2008: Agriculture for Development];. Washington DC: World Bank.

] <ref-worldde>
#block[
Castañeda, Andrés, Dung Doan, David Newhouse, Minh Cong Nguyen, Hiroki Uematsu, and João Pedro Azevedo. 2018. “A New Profile of the Global Poor.” #emph[World Development] 101 (January): 250--67. #link("https://doi.org/10.1016/j.worlddev.2017.08.002");.

] <ref-castañeda2018>
#block[
Chen, Ruxia, Huimin Yan, Fang Liu, Wenpeng Du, and Yanzhao Yang. 2020. “Multiple Global Population Datasets: Differences and Spatial Distribution Characteristics.” #emph[ISPRS International Journal of Geo-Information] 9 (11): 637. #link("https://doi.org/10.3390/ijgi9110637");.

] <ref-chen2020>
#block[
Coetzer, Kaera L., Edward T. F. Witkowski, and Barend F. N. Erasmus. 2014. “Reviewing Biosphere Reserves Globally: Effective Conservation Action or Bureaucratic Label?” #emph[Biological Reviews] 89 (1): 82--104. #link("https://doi.org/10.1111/brv.12044");.

] <ref-coetzer2014>
#block[
Convention on Biological Diversity. 2022. “Kunming-Montreal Global Biodiversity Framework.” In #emph[Convention Biol. Divers. Kunming-Montreal Glob. Biodivers. Framew.(cbd. Int)];. Vol. CBD/COP/15/L.25. #link("https://seea.un.org/sites/seea.un.org/files/unsc_presentation_jillian_campbell_0.pdf");.

] <ref-cbd2022>
#block[
Fisher, Joshua, Summer Allen, Greg Yetman, and Linda Pistolesi. 2024. “Assessing the Influence of Landscape Conservation and Protected Areas on Social Wellbeing Using Random Forest Machine Learning.” #emph[Scientific Reports] 14 (1): 11357. #link("https://doi.org/10.1038/s41598-024-61924-4");.

] <ref-fisher2024>
#block[
Freire, Sergio, Kytt MacManus, Martino Pesaresi, Erin Doxsey-Whitfield, and Jane Mills. 2016. “Development of New Open and Free Multi-Temporal Global Population Grids at 250 m Resolution.” #emph[Population] 250: 33.

] <ref-freire2016>
#block[
Gorelick, Noel, Matt Hancher, Mike Dixon, Simon Ilyushchenko, David Thau, and Rebecca Moore. 2017. “Google Earth Engine: Planetary-Scale Geospatial Analysis for Everyone.” #emph[Remote Sensing of Environment] 202: 1827. #link("https://www.sciencedirect.com/science/article/pii/S0034425717302900");.

] <ref-gorelick2017>
#block[
Hanson, Jeffrey O. 2022. “Wdpar: Interface to the World Database on Protected Areas.” #emph[Journal of Open Source Software] 7 (78): 4594. #link("https://doi.org/10.21105/joss.04594");.

] <ref-hanson2022>
#block[
Kandel, Pratikshya, Ram Pandit, Benedict White, and Maksym Polyakov. 2022. “Do Protected Areas Increase Household Income? Evidence from a Meta-Analysis.” #emph[World Development] 159 (November): 106024. #link("https://doi.org/10.1016/j.worlddev.2022.106024");.

] <ref-kandel2022>
#block[
Leberger, Roxanne, Isabel M. D. Rosa, Carlos A. Guerra, Florian Wolf, and Henrique M. Pereira. 2020. “Global Patterns of Forest Loss Across IUCN Categories of Protected Areas.” #emph[Biological Conservation] 241 (January): 108299. #link("https://doi.org/10.1016/j.biocon.2019.108299");.

] <ref-leberger2020>
#block[
Leyk, Stefan, Andrea E. Gaughan, Susana B. Adamo, Alex De Sherbinin, Deborah Balk, Sergio Freire, Amy Rose, Forrest R. Stevens, Brian Blankespoor, and Charlie Frye. 2019. “The Spatial Allocation of Population: A Review of Large-Scale Gridded Population Data Products and Their Fitness for Use.” #emph[Earth System Science Data] 11 (3): 1385--1409. #link("https://essd.copernicus.org/articles/11/1385/2019/essd-11-1385-2019.html");.

] <ref-leyk2019>
#block[
Maxwell, Sean L., Victor Cazalis, Nigel Dudley, Michael Hoffmann, Ana SL Rodrigues, Sue Stolton, Piero Visconti, Stephen Woodley, Naomi Kingston, and Edward Lewis. 2020. “Area-Based Conservation in the Twenty-First Century.” #emph[Nature] 586 (7828): 217227.

] <ref-maxwell2020>
#block[
Naidoo, R., D. Gerkey, D. Hole, A. Pfaff, A. M. Ellis, C. D. Golden, D. Herrera, et al. 2019. “Evaluating the Impacts of Protected Areas on Human Well-Being Across the Developing World.” #emph[Science Advances] 5 (4): eaav3006. #link("https://doi.org/10.1126/sciadv.aav3006");.

] <ref-naidoo2019>
#block[
Oldekop, Johan A., George Holmes, W. Edwin Harris, and Karl L. Evans. 2016. “A Global Assessment of the Social and Conservation Outcomes of Protected Areas.” #emph[Conservation Biology] 30 (1): 133141.

] <ref-oldekop2016>
#block[
R Core Team. 2023. #emph[R: A Language and Environment for Statistical Computing];. Vienna, Austria: R Foundation for Statistical Computing. #link("https://www.R-project.org/");.

] <ref-r2023>
#block[
Runfola, Daniel, Austin Anderson, Heather Baier, Matt Crittenden, Elizabeth Dowker, Sydney Fuhrig, Seth Goodman, et al. 2020. “geoBoundaries: A Global Database of Political Administrative Boundaries.” #emph[PLOS ONE] 15 (4): e0231866. #link("https://doi.org/10.1371/journal.pone.0231866");.

] <ref-runfola2020>
#block[
Schleicher, Judith, Julie G. Zaehringer, Carine Fastré, Bhaskar Vira, and Terry C. H. Sunderland. 2019. “Protecting Half of the Planet Could Directly Affect over One Billion People.” #emph[Nature Sustainability] 2: 1094--96. #link("https://doi.org/10.1038/s41893-019-0423-y");.

] <ref-schleicher2019>
#block[
Stevens, Forrest R., Andrea E. Gaughan, Catherine Linard, and Andrew J. Tatem. 2015. “Disaggregating Census Data for Population Mapping Using Random Forests with Remotely-Sensed and Ancillary Data.” #emph[PloS One] 10 (2): e0107042. #link("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0107042");.

] <ref-stevens2015>
#block[
Thomson, Dana R, Douglas R Leasure, Tomas Bird, Nikos Tzavidis, and Andrew J Tatem. 2022. “How Accurate Are WorldPop-Global-Unconstrained Gridded Population Data at the Cell-Level?: A Simulation Analysis in Urban Namibia.” #emph[Plos One] 17 (7): e0271504.

] <ref-thomson2022>
#block[
UNEP-WCMC. 2026. “Protected Area Profile for India.” #link("https://www.protectedplanet.net/country/IND");.

] <ref-protectedplanet_india>
#block[
UNEP-WCMC and IUCN. 2021. #emph[Protected Planet Report 2021];. Cambridge, United Kingdom; Gland, Switzerland: UNEP-WCMC; IUCN.

] <ref-unep2021>
#block[
---------. 2024. #emph[Protected Planet Report 2024];. Cambridge, United Kingdom; Gland, Switzerland: UNEP-WCMC; IUCN.

] <ref-unep2024>
#block[
UNEP-WCMC, and IUCN. 2023. #emph[Protected Planet: The World Database on Protected Areas (WDPA)];. UNEP-WCMC and IUCN. Cambridge, UK. #link("https://www.protectedplanet.net")[www.protectedplanet.net];.

] <ref-unep-wcmcandiucn2023>
#block[
United Nations Department of Economic and Social Affairs. 2021. #emph[State of the World's Indigenous Peoples];. 5th ed. New York: United Nations. #link("https://www.un.org/development/desa/indigenouspeoples/publications/state-of-the-worlds-indigenous-peoples.html");.

] <ref-undesa2021>
#block[
Vaggi, Gianni. 2017. “The Rich and the Poor: A Note on Countries' Classification.” #emph[PSL Quarterly Review] 70 (279). #link("https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3100742");.

] <ref-vaggi2017>
#block[
Wani, Zubair A., Lal Singh, Sanjay Uniyal, Sunil Kumar Rana, Indra D. Bhatt, and Sunil Nautiyal. 2025. “Improving Ecosystem Vitality in India: Overcoming Barriers to Meet National and International Targets.” #emph[Environmental Sustainability] 8 (1): 17--29. #link("https://doi.org/10.1007/s42398-025-00339-x");.

] <ref-wani2025>
#block[
West, Paige, James Igoe, and Dan Brockington. 2006. “Parks and Peoples: The Social Impact of Protected Areas.” #emph[Annual Review of Anthropology] 35 (1): 251--77. #link("https://doi.org/10.1146/annurev.anthro.35.081705.123308");.

] <ref-west2006>
#block[
Zhang, Tong, Luke Gibson, Jianzhang Ma, Rachakonda Sreekar, and David Lindenmayer. 2025. “Global Patterns of Border Protected Areas Reveal Gaps in Transboundary Conservation Efforts.” #emph[One Earth];. #link("https://doi.org/10.1016/j.oneear.2025.101206");.

] <ref-zhang2025>
] <refs>



