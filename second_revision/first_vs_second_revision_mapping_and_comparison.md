# First vs Second Revision Output Mapping and Comparison

Generated on: 2026-07-13 15:55:18 UTC

## Scope

This report compares the first-revision GEE outputs in `data/reviewed_PA_Pop_GHSL_Worldpop` with the second-revision R outputs in `data/reviewed_PA_Pop_new`.

Old ADM output files found: 157.
New ADM output files found: 30.
Countries currently comparable: 16 (AFG, AGO, BEN, BGD, COG, KGZ, MNG, SEN, SLE, STP, TUN, UGA, VUT, YEM, ZMB, ZWE).
Countries present in the first revision but not yet in the current second-revision folder: 63.

Because `data/reviewed_PA_Pop_new` currently contains only a subset of countries, all country and global comparisons below are restricted to the overlap available at run time. Re-running this script after more second-revision outputs are produced will expand the comparison automatically.

## Key Computations Needed for the Analysis

1. Filter the WDPA to designated, established, or inscribed terrestrial PAs, excluding UNESCO-MAB Biosphere Reserves and marine-only sites.
2. Split PAs into strict, non-strict, and unknown IUCN categories, then impose the exclusive hierarchy strict > non-strict > unknown.
3. Build scenario-specific PA masks for Confirmed_2000, Confirmed_2020, Unknown_Year, and in the second revision All_2020.
4. Build 10 km buffer rings that exclude pixels already claimed by PA cores or higher-priority buffers, so each pixel is counted once per scenario.
5. Intersect those masks with GHSL and WorldPop population rasters and sum population and land area by ADM unit, then aggregate to country totals.
6. Derive manuscript indicators such as population inside PAs, population inside or within 10 km, PA area, and category-specific decompositions from the country aggregates.

## Output Mapping

| first_revision_output | second_revision_output | relationship |
| --- | --- | --- |
| data/reviewed_PA_Pop_GHSL_Worldpop/PA_Pop_{ISO3}_{SOURCE}.csv | data/reviewed_PA_Pop_new/PA_Pop_{ISO3}_{SOURCE}.csv | Same ADM-level artifact and same core columns; second revision adds explicit All_2020 rows. |
| Legacy 2020 all-PAs used in reviewed_produce_table_and_figures.R as Confirmed_2020 + Unknown_Year | scenario == All_2020 in data/reviewed_PA_Pop_new/PA_Pop_{ISO3}_{SOURCE}.csv | New direct replacement for the old derived all-2020 quantity; avoids double counting from separate hierarchy resets. |
| data/reviewed_PA_Pop_GHSL_Worldpop/National_PA_Totals_Refactored.csv | data/reviewed_PA_Pop_new/national_totals/National_PA_Totals_Refactored_{ISO3}.csv | Old file is one combined table; new outputs are one file per country and add *_confirmed2020 fields. |
| nat_pop_gh_20 and nat_pop_wp_20 in National_PA_Totals_Refactored.csv | nat_pop_gh_20 and nat_pop_wp_20 in National_PA_Totals_Refactored_{ISO3}.csv | Same field names, but in the new script they are aligned to the explicit All_2020 scenario and no longer mixed with unrestricted PA counts. |

## Comparison Method

1. ADM-level CSVs are aggregated to country x source x scenario by summing population and area fields and taking the scenario-level PA counts once per country.
2. The first revision has no explicit `All_2020` rows, so its all-2020 quantity is reconstructed exactly as the first-revision R analysis did: `Confirmed_2020 + Unknown_Year`.
3. The second revision is compared directly on its explicit `All_2020` rows, as well as on the three shared scenarios (`Confirmed_2000`, `Confirmed_2020`, `Unknown_Year`).
4. Global comparisons are sums across the overlapping countries only.

## Global Comparison

| source | scenario | first_pop_inside_or_10km_all | second_pop_inside_or_10km_all | pop_inside_or_10km_all_abs_change | pop_inside_or_10km_all_pct_change | first_pop_inside_all | second_pop_inside_all | pop_inside_all_abs_change | pop_inside_all_pct_change |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| GHSL | Confirmed_2000 | 46,961,479 | 41,727,419 | -5,234,060 | -11.15% | 2,330,593 | 2,211,977 | -118,616 | -5.09% |
| GHSL | Confirmed_2020 | 108,568,389 | 97,822,410 | -10,745,979 | -9.90% | 6,139,902 | 6,136,311 | -3,591 | -0.06% |
| GHSL | Unknown_Year | 14,310,929 | 11,870,767 | -2,440,162 | -17.05% | 522,743 | 519,599 | -3,144 | -0.60% |
| GHSL | All_2020 | 122,879,319 | 104,352,073 | -18,527,245 | -15.08% | 6,662,645 | 6,607,500 | -55,145 | -0.83% |
| WP | Confirmed_2000 | 31,633,517 | 33,295,455 | 1,661,938 | 5.25% | 2,806,674 | 3,281,139 | 474,465 | 16.90% |
| WP | Confirmed_2020 | 73,109,655 | 78,529,416 | 5,419,760 | 7.41% | 6,819,529 | 8,037,033 | 1,217,505 | 17.85% |
| WP | Unknown_Year | 8,814,393 | 8,696,780 | -117,612 | -1.33% | 795,833 | 920,302 | 124,468 | 15.64% |
| WP | All_2020 | 81,924,048 | 84,029,960 | 2,105,912 | 2.57% | 7,615,362 | 8,837,790 | 1,222,428 | 16.05% |

## Country-by-Country Core Comparison

| iso3 | source | scenario | first_pop_inside_or_10km_all | second_pop_inside_or_10km_all | pop_inside_or_10km_all_abs_change | pop_inside_or_10km_all_pct_change | first_pop_inside_all | second_pop_inside_all | pop_inside_all_abs_change | pop_inside_all_pct_change |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| AFG | GHSL | Confirmed_2000 | 170,845 | 0 | -170,845 | -100.00% | 17,911 | 0 | -17,911 | -100.00% |
| AGO | GHSL | Confirmed_2000 | 470,482 | 397,739 | -72,742 | -15.46% | 242,444 | 243,080 | 636 | 0.26% |
| BEN | GHSL | Confirmed_2000 | 1,615,130 | 1,045,955 | -569,175 | -35.24% | 106,762 | 107,571 | 809 | 0.76% |
| BGD | GHSL | Confirmed_2000 | 6,122,829 | 4,797,261 | -1,325,568 | -21.65% | 479,071 | 340,296 | -138,775 | -28.97% |
| COG | GHSL | Confirmed_2000 | 1,541,751 | 1,502,434 | -39,317 | -2.55% | 15,857 | 16,370 | 513 | 3.23% |
| KGZ | GHSL | Confirmed_2000 | 836,137 | 801,287 | -34,850 | -4.17% | 18,542 | 18,824 | 282 | 1.52% |
| MNG | GHSL | Confirmed_2000 | 884,940 | 922,324 | 37,384 | 4.22% | 39,608 | 40,301 | 692 | 1.75% |
| SEN | GHSL | Confirmed_2000 | 1,550,767 | 1,249,884 | -300,883 | -19.40% | 116,844 | 119,237 | 2,392 | 2.05% |
| SLE | GHSL | Confirmed_2000 | 1,199,985 | 1,131,301 | -68,684 | -5.72% | 25,269 | 21,841 | -3,428 | -13.57% |
| STP | GHSL | Confirmed_2000 | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| TUN | GHSL | Confirmed_2000 | 3,075,962 | 2,562,547 | -513,415 | -16.69% | 7,318 | 7,716 | 398 | 5.44% |
| UGA | GHSL | Confirmed_2000 | 20,491,003 | 19,314,961 | -1,176,042 | -5.74% | 407,486 | 415,491 | 8,005 | 1.96% |
| VUT | GHSL | Confirmed_2000 | 33,452 | 22,824 | -10,628 | -31.77% | 145 | 220 | 76 | 52.25% |
| YEM | GHSL | Confirmed_2000 | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| ZMB | GHSL | Confirmed_2000 | 7,208,313 | 6,529,292 | -679,021 | -9.42% | 786,907 | 813,289 | 26,382 | 3.35% |
| ZWE | GHSL | Confirmed_2000 | 1,759,882 | 1,449,608 | -310,274 | -17.63% | 66,428 | 67,741 | 1,314 | 1.98% |
| AFG | GHSL | Confirmed_2020 | 5,784,593 | 5,157,958 | -626,635 | -10.83% | 425,604 | 397,153 | -28,451 | -6.68% |
| AGO | GHSL | Confirmed_2020 | 1,329,804 | 1,048,831 | -280,972 | -21.13% | 576,096 | 575,221 | -874 | -0.15% |
| BEN | GHSL | Confirmed_2020 | 2,932,434 | 1,961,271 | -971,163 | -33.12% | 211,627 | 211,013 | -614 | -0.29% |
| BGD | GHSL | Confirmed_2020 | 18,068,567 | 15,266,083 | -2,802,484 | -15.51% | 836,061 | 702,352 | -133,709 | -15.99% |
| COG | GHSL | Confirmed_2020 | 4,190,626 | 3,951,632 | -238,994 | -5.70% | 682,487 | 681,464 | -1,023 | -0.15% |
| KGZ | GHSL | Confirmed_2020 | 1,079,733 | 1,022,219 | -57,513 | -5.33% | 27,234 | 27,056 | -179 | -0.66% |
| MNG | GHSL | Confirmed_2020 | 1,786,997 | 1,867,797 | 80,800 | 4.52% | 62,917 | 63,862 | 945 | 1.50% |
| SEN | GHSL | Confirmed_2020 | 5,695,754 | 5,080,375 | -615,379 | -10.80% | 179,096 | 205,867 | 26,771 | 14.95% |
| SLE | GHSL | Confirmed_2020 | 2,621,993 | 2,418,054 | -203,940 | -7.78% | 80,891 | 75,584 | -5,307 | -6.56% |
| STP | GHSL | Confirmed_2020 | 100,726 | 80,778 | -19,949 | -19.80% | 100 | 106 | 7 | 6.67% |
| TUN | GHSL | Confirmed_2020 | 7,550,251 | 6,770,739 | -779,512 | -10.32% | 18,223 | 21,680 | 3,456 | 18.97% |
| UGA | GHSL | Confirmed_2020 | 38,283,819 | 35,988,295 | -2,295,524 | -6.00% | 916,278 | 944,064 | 27,786 | 3.03% |
| VUT | GHSL | Confirmed_2020 | 61,220 | 46,687 | -14,534 | -23.74% | 254 | 389 | 135 | 53.38% |
| YEM | GHSL | Confirmed_2020 | 70,503 | 70,747 | 245 | 0.35% | 10,390 | 11,103 | 713 | 6.86% |
| ZMB | GHSL | Confirmed_2020 | 13,688,496 | 12,207,092 | -1,481,404 | -10.82% | 1,506,929 | 1,558,386 | 51,457 | 3.41% |
| ZWE | GHSL | Confirmed_2020 | 5,322,873 | 4,883,853 | -439,021 | -8.25% | 605,714 | 661,010 | 55,297 | 9.13% |
| AFG | GHSL | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| AGO | GHSL | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| BEN | GHSL | Unknown_Year | 258,117 | 0 | -258,117 | -100.00% | 803 | 0 | -803 | -100.00% |
| BGD | GHSL | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| COG | GHSL | Unknown_Year | 3,145 | 0 | -3,145 | -100.00% | 469 | 0 | -469 | -100.00% |
| KGZ | GHSL | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| MNG | GHSL | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| SEN | GHSL | Unknown_Year | 6,067,912 | 5,183,643 | -884,269 | -14.57% | 250,896 | 251,535 | 639 | 0.25% |
| SLE | GHSL | Unknown_Year | 1,262,586 | 1,017,899 | -244,687 | -19.38% | 48,517 | 48,768 | 251 | 0.52% |
| STP | GHSL | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| TUN | GHSL | Unknown_Year | 285,260 | 264,716 | -20,544 | -7.20% | 124 | 132 | 8 | 6.51% |
| UGA | GHSL | Unknown_Year | 2,299,347 | 1,823,588 | -475,759 | -20.69% | 13,397 | 13,475 | 78 | 0.58% |
| VUT | GHSL | Unknown_Year | 94,659 | 106,613 | 11,953 | 12.63% | 371 | 371 | 0 | -0.03% |
| YEM | GHSL | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| ZMB | GHSL | Unknown_Year | 2,915,165 | 2,604,473 | -310,691 | -10.66% | 111,056 | 110,220 | -835 | -0.75% |
| ZWE | GHSL | Unknown_Year | 1,124,738 | 869,835 | -254,903 | -22.66% | 97,111 | 95,097 | -2,014 | -2.07% |
| AFG | GHSL | All_2020 | 5,784,593 | 5,157,958 | -626,635 | -10.83% | 425,604 | 397,153 | -28,451 | -6.68% |
| AGO | GHSL | All_2020 | 1,329,804 | 1,048,831 | -280,972 | -21.13% | 576,096 | 575,221 | -874 | -0.15% |
| BEN | GHSL | All_2020 | 3,190,551 | 1,961,271 | -1,229,280 | -38.53% | 212,430 | 211,013 | -1,417 | -0.67% |
| BGD | GHSL | All_2020 | 18,068,567 | 15,266,083 | -2,802,484 | -15.51% | 836,061 | 702,352 | -133,709 | -15.99% |
| COG | GHSL | All_2020 | 4,193,771 | 3,951,632 | -242,139 | -5.77% | 682,956 | 681,464 | -1,492 | -0.22% |
| KGZ | GHSL | All_2020 | 1,079,733 | 1,022,219 | -57,513 | -5.33% | 27,234 | 27,056 | -179 | -0.66% |
| MNG | GHSL | All_2020 | 1,786,997 | 1,867,797 | 80,800 | 4.52% | 62,917 | 63,862 | 945 | 1.50% |
| SEN | GHSL | All_2020 | 11,763,666 | 8,943,336 | -2,820,329 | -23.97% | 429,992 | 435,898 | 5,906 | 1.37% |
| SLE | GHSL | All_2020 | 3,884,580 | 3,410,744 | -473,836 | -12.20% | 129,408 | 124,352 | -5,056 | -3.91% |
| STP | GHSL | All_2020 | 100,726 | 80,778 | -19,949 | -19.80% | 100 | 106 | 7 | 6.67% |
| TUN | GHSL | All_2020 | 7,835,512 | 6,878,001 | -957,511 | -12.22% | 18,347 | 21,811 | 3,464 | 18.88% |
| UGA | GHSL | All_2020 | 40,583,166 | 36,239,165 | -4,344,001 | -10.70% | 929,675 | 943,804 | 14,129 | 1.52% |
| VUT | GHSL | All_2020 | 155,879 | 142,537 | -13,342 | -8.56% | 625 | 760 | 135 | 21.65% |
| YEM | GHSL | All_2020 | 70,503 | 70,747 | 245 | 0.35% | 10,390 | 11,103 | 713 | 6.86% |
| ZMB | GHSL | All_2020 | 16,603,660 | 12,871,828 | -3,731,832 | -22.48% | 1,617,985 | 1,662,531 | 44,547 | 2.75% |
| ZWE | GHSL | All_2020 | 6,447,611 | 5,439,146 | -1,008,466 | -15.64% | 702,825 | 749,012 | 46,187 | 6.57% |
| AFG | WP | Confirmed_2000 | 105,960 | 0 | -105,960 | -100.00% | 7,385 | 0 | -7,385 | -100.00% |
| AGO | WP | Confirmed_2000 | 537,903 | 535,252 | -2,651 | -0.49% | 193,124 | 224,365 | 31,241 | 16.18% |
| BEN | WP | Confirmed_2000 | 1,344,849 | 1,177,478 | -167,372 | -12.45% | 254,817 | 297,632 | 42,815 | 16.80% |
| BGD | WP | Confirmed_2000 | 4,580,686 | 4,669,999 | 89,313 | 1.95% | 577,879 | 646,081 | 68,202 | 11.80% |
| COG | WP | Confirmed_2000 | 1,447,270 | 1,405,970 | -41,301 | -2.85% | 115,950 | 135,385 | 19,435 | 16.76% |
| KGZ | WP | Confirmed_2000 | 674,533 | 747,806 | 73,273 | 10.86% | 43,800 | 52,191 | 8,392 | 19.16% |
| SEN | WP | Confirmed_2000 | 1,507,852 | 1,307,300 | -200,552 | -13.30% | 204,913 | 243,061 | 38,148 | 18.62% |
| SLE | WP | Confirmed_2000 | 913,927 | 1,003,789 | 89,862 | 9.83% | 40,683 | 45,890 | 5,207 | 12.80% |
| STP | WP | Confirmed_2000 | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| TUN | WP | Confirmed_2000 | 2,092,513 | 2,402,657 | 310,144 | 14.82% | 55,614 | 65,901 | 10,287 | 18.50% |
| UGA | WP | Confirmed_2000 | 16,842,565 | 18,438,865 | 1,596,299 | 9.48% | 1,051,130 | 1,256,082 | 204,952 | 19.50% |
| VUT | WP | Confirmed_2000 | 16,682 | 20,804 | 4,123 | 24.71% | 153 | 203 | 50 | 32.88% |
| YEM | WP | Confirmed_2000 | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| ZWE | WP | Confirmed_2000 | 1,568,777 | 1,585,536 | 16,759 | 1.07% | 261,225 | 314,347 | 53,122 | 20.34% |
| AFG | WP | Confirmed_2020 | 3,529,653 | 3,840,004 | 310,351 | 8.79% | 240,480 | 281,104 | 40,624 | 16.89% |
| AGO | WP | Confirmed_2020 | 1,485,863 | 1,337,357 | -148,506 | -9.99% | 486,344 | 564,789 | 78,445 | 16.13% |
| BEN | WP | Confirmed_2020 | 2,848,965 | 2,548,874 | -300,091 | -10.53% | 561,373 | 658,494 | 97,121 | 17.30% |
| BGD | WP | Confirmed_2020 | 13,953,879 | 14,887,190 | 933,312 | 6.69% | 1,090,493 | 1,245,974 | 155,481 | 14.26% |
| COG | WP | Confirmed_2020 | 2,416,541 | 2,460,373 | 43,833 | 1.81% | 823,286 | 959,594 | 136,308 | 16.56% |
| KGZ | WP | Confirmed_2020 | 739,021 | 802,224 | 63,203 | 8.55% | 44,247 | 51,886 | 7,639 | 17.26% |
| SEN | WP | Confirmed_2020 | 4,875,187 | 5,235,498 | 360,311 | 7.39% | 488,169 | 590,581 | 102,412 | 20.98% |
| SLE | WP | Confirmed_2020 | 1,731,536 | 1,869,135 | 137,599 | 7.95% | 85,660 | 97,453 | 11,792 | 13.77% |
| STP | WP | Confirmed_2020 | 98,761 | 140,680 | 41,919 | 42.44% | 14,960 | 17,408 | 2,448 | 16.36% |
| TUN | WP | Confirmed_2020 | 5,745,024 | 6,444,624 | 699,601 | 12.18% | 109,250 | 130,112 | 20,863 | 19.10% |
| UGA | WP | Confirmed_2020 | 31,214,643 | 34,141,882 | 2,927,239 | 9.38% | 2,110,475 | 2,507,154 | 396,678 | 18.80% |
| VUT | WP | Confirmed_2020 | 30,737 | 41,074 | 10,337 | 33.63% | 257 | 377 | 120 | 46.78% |
| YEM | WP | Confirmed_2020 | 57,960 | 68,069 | 10,109 | 17.44% | 41,846 | 49,162 | 7,316 | 17.48% |
| ZWE | WP | Confirmed_2020 | 4,381,886 | 4,712,429 | 330,544 | 7.54% | 722,689 | 882,947 | 160,257 | 22.18% |
| AFG | WP | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| AGO | WP | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| BEN | WP | Unknown_Year | 209,361 | 0 | -209,361 | -100.00% | 3,165 | 0 | -3,165 | -100.00% |
| BGD | WP | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| COG | WP | Unknown_Year | 8,913 | 0 | -8,913 | -100.00% | 210 | 0 | -210 | -100.00% |
| KGZ | WP | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| SEN | WP | Unknown_Year | 4,826,860 | 4,933,849 | 106,989 | 2.22% | 571,257 | 664,171 | 92,914 | 16.26% |
| SLE | WP | Unknown_Year | 852,832 | 867,116 | 14,284 | 1.67% | 35,912 | 41,668 | 5,756 | 16.03% |
| STP | WP | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| TUN | WP | Unknown_Year | 209,542 | 240,817 | 31,276 | 14.93% | 1,472 | 1,691 | 219 | 14.90% |
| UGA | WP | Unknown_Year | 1,824,444 | 1,800,147 | -24,297 | -1.33% | 73,432 | 85,511 | 12,079 | 16.45% |
| VUT | WP | Unknown_Year | 83,692 | 105,344 | 21,652 | 25.87% | 1,001 | 1,169 | 168 | 16.78% |
| YEM | WP | Unknown_Year | 0 | 0 | 0 | NA | 0 | 0 | 0 | NA |
| ZWE | WP | Unknown_Year | 798,750 | 749,507 | -49,243 | -6.17% | 109,385 | 126,092 | 16,707 | 15.27% |
| AFG | WP | All_2020 | 3,529,653 | 3,840,004 | 310,351 | 8.79% | 240,480 | 281,104 | 40,624 | 16.89% |
| AGO | WP | All_2020 | 1,485,863 | 1,337,357 | -148,506 | -9.99% | 486,344 | 564,789 | 78,445 | 16.13% |
| BEN | WP | All_2020 | 3,058,326 | 2,548,874 | -509,452 | -16.66% | 564,538 | 658,494 | 93,956 | 16.64% |
| BGD | WP | All_2020 | 13,953,879 | 14,887,190 | 933,312 | 6.69% | 1,090,493 | 1,245,974 | 155,481 | 14.26% |
| COG | WP | All_2020 | 2,425,454 | 2,460,373 | 34,919 | 1.44% | 823,496 | 959,594 | 136,098 | 16.53% |
| KGZ | WP | All_2020 | 739,021 | 802,224 | 63,203 | 8.55% | 44,247 | 51,886 | 7,639 | 17.26% |
| SEN | WP | All_2020 | 9,702,047 | 9,005,876 | -696,170 | -7.18% | 1,059,426 | 1,164,725 | 105,298 | 9.94% |
| SLE | WP | All_2020 | 2,584,368 | 2,712,903 | 128,534 | 4.97% | 121,572 | 139,121 | 17,549 | 14.43% |
| STP | WP | All_2020 | 98,761 | 140,680 | 41,919 | 42.44% | 14,960 | 17,408 | 2,448 | 16.36% |
| TUN | WP | All_2020 | 5,954,565 | 6,537,991 | 583,426 | 9.80% | 110,721 | 131,803 | 21,082 | 19.04% |
| UGA | WP | All_2020 | 33,039,087 | 34,395,577 | 1,356,490 | 4.11% | 2,183,907 | 2,576,434 | 392,527 | 17.97% |
| VUT | WP | All_2020 | 114,428 | 136,966 | 22,538 | 19.70% | 1,257 | 1,536 | 279 | 22.18% |
| YEM | WP | All_2020 | 57,960 | 68,069 | 10,109 | 17.44% | 41,846 | 49,162 | 7,316 | 17.48% |
| ZWE | WP | All_2020 | 5,180,636 | 5,155,875 | -24,760 | -0.48% | 832,074 | 995,761 | 163,687 | 19.67% |

## National Totals Comparison

| iso3 | first_area_total_pa | second_area_total_pa | area_total_pa_abs_change | area_total_pa_pct_change | first_nat_pop_gh_20 | second_nat_pop_gh_20 | nat_pop_gh_20_abs_change | nat_pop_gh_20_pct_change | first_nat_pop_wp_20 | second_nat_pop_wp_20 | nat_pop_wp_20_abs_change | nat_pop_wp_20_pct_change | first_count_total | second_count_total | count_total_abs_change |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| AFG | 22,951.6 | 21,797.7 | -1,153.9 | -5.03% | 7,547,176 | 38,895,580 | 31,348,404 | 415.37% | 4,160,807 | 30,111,459 | 25,950,652 | 623.69% |   9 |   9 | 0 |
| AGO | 82,327.3 | 82,205.5 | -121.8 | -0.15% | 5,438,810 | 33,354,494 | 27,915,684 | 513.27% | 4,982,202 | 36,164,342 | 31,182,141 | 625.87% |  12 |  12 | 0 |
| BEN | 27,012.1 | 24,729.5 | -2,282.6 | -8.45% | 2,025,930 | 12,442,913 | 10,416,984 | 514.18% | 1,788,793 | 12,839,377 | 11,050,584 | 617.77% |  28 |  28 | 0 |
| BGD | 5,563.0 | 3,857.0 | -1,706.1 | -30.67% | 28,783,391 | 163,635,842 | 134,852,451 | 468.51% | 22,494,876 | 158,489,122 | 135,994,246 | 604.56% |  38 |  39 | 1 |
| COG | 124,033.0 | 117,639.2 | -6,393.8 | -5.15% | 907,181 | 5,643,849 | 4,736,668 | 522.13% | 551,881 | 3,924,158 | 3,372,276 | 611.05% |  31 |  31 | 0 |
| KGZ | 5,937.6 | 5,332.3 | -605.3 | -10.19% | 1,372,513 | 6,468,317 | 5,095,804 | 371.28% | 811,808 | 5,587,948 | 4,776,140 | 588.33% |  29 |  29 | 0 |
| SEN | 51,147.7 | 47,435.4 | -3,712.3 | -7.26% | 2,698,789 | 16,430,191 | 13,731,401 | 508.80% | 2,222,898 | 16,104,866 | 13,881,967 | 624.50% | 124 | 126 | 2 |
| SLE | 4,044.7 | 3,896.4 | -148.3 | -3.67% | 1,316,472 | 8,182,856 | 6,866,385 | 521.57% | 922,660 | 6,712,970 | 5,790,309 | 627.57% |  36 |  36 | 0 |
| STP | 312.2 | 313.6 | 1.4 | 0.45% | 32,783 | 213,768 | 180,986 | 552.08% | 26,820 | 200,432 | 173,613 | 647.34% |   2 |   2 | 0 |
| TUN | 11,677.2 | 11,664.4 | -12.8 | -0.11% | 2,388,468 | 12,146,089 | 9,757,621 | 408.53% | 1,602,788 | 11,658,073 | 10,055,285 | 627.36% | 125 | 125 | 0 |
| UGA | 35,614.9 | 31,354.1 | -4,260.8 | -11.96% | 7,100,137 | 44,350,879 | 37,250,742 | 524.65% | 5,793,796 | 41,974,556 | 36,180,760 | 624.47% | 700 | 700 | 0 |
| VUT | 452.1 | 446.7 | -5.5 | -1.21% | 37,928 | 295,684 | 257,757 | 679.60% | 30,361 | 263,039 | 232,678 | 766.37% |  12 |  13 | 1 |
| YEM | 2,765.2 | 2,800.8 | 35.6 | 1.29% | 5,266,488 | 32,232,379 | 26,965,890 | 512.03% | 4,144,885 | 30,173,288 | 26,028,404 | 627.96% |   1 |   1 | 0 |
| ZWE | 102,135.0 | 85,544.0 | -16,591.0 | -16.24% | 2,650,821 | 15,671,150 | 13,020,329 | 491.18% | 1,978,362 | 14,328,623 | 12,350,262 | 624.27% | 181 | 181 | 0 |

## Largest All_2020 Country-Level Changes

| iso3 | source | first_pop_inside_or_10km_all | second_pop_inside_or_10km_all | pop_inside_or_10km_all_abs_change | pop_inside_or_10km_all_pct_change |
| --- | --- | --- | --- | --- | --- |
| UGA | GHSL | 40,583,166 | 36,239,165 | -4,344,001 | -10.70% |
| ZMB | GHSL | 16,603,660 | 12,871,828 | -3,731,832 | -22.48% |
| SEN | GHSL | 11,763,666 | 8,943,336 | -2,820,329 | -23.97% |
| BGD | GHSL | 18,068,567 | 15,266,083 | -2,802,484 | -15.51% |
| UGA | WP | 33,039,087 | 34,395,577 | 1,356,490 | 4.11% |
| BEN | GHSL | 3,190,551 | 1,961,271 | -1,229,280 | -38.53% |
| ZWE | GHSL | 6,447,611 | 5,439,146 | -1,008,466 | -15.64% |
| TUN | GHSL | 7,835,512 | 6,878,001 | -957,511 | -12.22% |
| BGD | WP | 13,953,879 | 14,887,190 | 933,312 | 6.69% |
| SEN | WP | 9,702,047 | 9,005,876 | -696,170 | -7.18% |
| AFG | GHSL | 5,784,593 | 5,157,958 | -626,635 | -10.83% |
| TUN | WP | 5,954,565 | 6,537,991 | 583,426 | 9.80% |
| BEN | WP | 3,058,326 | 2,548,874 | -509,452 | -16.66% |
| SLE | GHSL | 3,884,580 | 3,410,744 | -473,836 | -12.20% |
| AFG | WP | 3,529,653 | 3,840,004 | 310,351 | 8.79% |
| AGO | GHSL | 1,329,804 | 1,048,831 | -280,972 | -21.13% |
| COG | GHSL | 4,193,771 | 3,951,632 | -242,139 | -5.77% |
| AGO | WP | 1,485,863 | 1,337,357 | -148,506 | -9.99% |
| SLE | WP | 2,584,368 | 2,712,903 | 128,534 | 4.97% |
| MNG | GHSL | 1,786,997 | 1,867,797 | 80,800 | 4.52% |
| KGZ | WP | 739,021 | 802,224 | 63,203 | 8.55% |
| KGZ | GHSL | 1,079,733 | 1,022,219 | -57,513 | -5.33% |
| STP | WP | 98,761 | 140,680 | 41,919 | 42.44% |
| COG | WP | 2,425,454 | 2,460,373 | 34,919 | 1.44% |
| ZWE | WP | 5,180,636 | 5,155,875 | -24,760 | -0.48% |
| VUT | WP | 114,428 | 136,966 | 22,538 | 19.70% |
| STP | GHSL | 100,726 | 80,778 | -19,949 | -19.80% |
| VUT | GHSL | 155,879 | 142,537 | -13,342 | -8.56% |
| YEM | WP | 57,960 | 68,069 | 10,109 | 17.44% |
| YEM | GHSL | 70,503 | 70,747 | 245 | 0.35% |

## Files Written by This Script

- `C:/Users/fbede/Documents/Statistiques/population_near_protected_areas/second_revision/first_vs_second_revision_mapping_and_comparison.md`
- `C:/Users/fbede/Documents/Statistiques/population_near_protected_areas/second_revision/first_vs_second_revision_country_core_comparison.csv`
- `C:/Users/fbede/Documents/Statistiques/population_near_protected_areas/second_revision/first_vs_second_revision_global_core_comparison.csv`
- `C:/Users/fbede/Documents/Statistiques/population_near_protected_areas/second_revision/first_vs_second_revision_national_totals_comparison.csv`
