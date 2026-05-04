# Hermes Geometry Wakeup — 2026-05-04 morning

*Written 2026-05-03 by the Wave 3 synthesizer at the close of the overnight push spec'd in `docs/superpowers/specs/2026-05-03-hermes-geometry-overnight-design.md`. Five Wave-2 diggers (Van de Walle, Van Hiele dissertation, Lakoff & Núñez, N103, misconception harvester) plus three Wave-1 workers (schema, LK_RB audit, standards mapper) ran ahead of me. The KB is at `/Users/tio/Documents/GitHub/umedcta-formalization/geometry/`. This document is the morning report — read it first, then walk the Tier 3 review queue, then settle the open questions.*

---

## Q-Walk Resolution — 2026-05-04

After Tio reviewed this wakeup and signed off on resolutions, the OPEN_QUESTIONS Resolution Worker executed the Q-walk across the KB.

**12 of 13 OPEN_QUESTIONS resolved**; only Q-001's residual universal-phase-theory claim stays open (now tracked as Q-001-residual at the top of `geometry/OPEN_QUESTIONS.md`).

**Resolution methods used:**
- **Developmental-arc resolution** (Q-004, Q-005, Q-008/Q-MH-A/Q-N103-F, Q-N103-D): four `developmental_marker/4` arcs authored in new `concepts/developmental_arcs.pl`. Each captures the FromStance → ToStance transition as the unit worth modeling (Tio's commitment that A-vs-B disputes resolve as developmental achievements). Source concepts preserved intact.
- **Re-keying** (Q-006): eight VdW transformation activities re-keyed from `polygon_recognition` catch-all to specific transformation concepts (translation, reflection, rotation, line_of_symmetry, reflectional_symmetry, rigid_motion_properties, congruence_by_superposition, similarity_as_uniform_scaling).
- **Topic-file migrations** (Q-007): five concepts and their associated misconceptions moved out of `classification.pl` into `area_perimeter.pl`, `angles.pl`, and a new `concepts/measurement.pl` file (using the new `measurement` topic atom). Two more concepts migrated from `synthesizer_anchors.pl` and `standards_anchors.pl` to `measurement.pl`.
- **Tier flips** (Q-002, Q-008): six `vh_fuys_clinical_*`-citing tier records demoted from Tier 1 to Tier 2 with `triangulation/2` annotations; one material_inference under `parallelogram_as_trapezoid` anchored at Tier 3 with note about exclusive-stance scope.
- **Tier 4 preservation** (Q-001 split): `phases_drive_transition` tier record removed (claim now genuinely open); five `vh_phase_*_general` bootstrap activities kept at Tier 1.
- **Confirmation only** (Q-003, Q-N103-A, Q-N103-C, Q-N103-E, Q-MH-C): existing digger choices ratified with notes in OPEN_QUESTIONS.md.
- **Q-N103-B convexity-pluralism**: four convexity tests tagged to their native van Hiele levels (rubber_band → 0, kids_crawl_space → 1, reflex_angle → 2, line_segment → 3). New geom_concept `reflex_angle_convexity_test` added to anchor the level-2 marker.
- **Q-N103-C metaphor cross-link**: tracing_paper_diagnostic linked to L&N's fictive_motion metaphor.
- **Q-MH-B PCK syntheses**: five `pck_synthesis/5` records authored in new `pck/classification.pl` directory, anchored to the most-cited PST PCK gaps. PCK kept as flat synthesis pointers (per Tio's flatter PCK framing).

**New artifacts:**
- `geometry/concepts/developmental_arcs.pl` — 4 arcs (square_classification_arc, triangle_angle_sum_arc, trapezoid_classification_arc, reflectional_symmetry_arc), each with geom_concept + developmental_marker + Tier-1 tier + bridging material_inference.
- `geometry/concepts/measurement.pl` — new file holding 3 concepts (ruler_units_vs_marks migrated from classification.pl, length_measurement_as_unit_iteration migrated from synthesizer_anchors.pl, measurement_unit_conversion migrated from standards_anchors.pl).
- `geometry/pck/classification.pl` — 5 pck_synthesis/5 records over quadrilateral_hierarchy, square_rectangle_classification, orientation_invariant_naming, definition_requires_necessary_and_sufficient_conditions, and exclusive_to_inclusive_transition.

**Coverage delta:**
- Concepts: 225 → 230 (+5: 4 dev arcs + reflex_angle_convexity_test).
- Metaphors: 34 → 35 (+1 tracing_paper-as-fictive_motion).
- Van Hiele markers: 27 → 31 (+4 convexity-test markers).
- Pck syntheses: 0 → 5.
- Developmental markers: 0 → 4.
- Tier counts: 337/136/100 → 335/148/100 (Q-002 demotions + new Tier-2 cross-links + new Tier-1 arcs).
- Validator: clean (zero errors); two cosmetic singleton-variable warnings in `metaphors/lakoff_nunez_inventory.pl` predate this push and were noted in the original wakeup.

**Unexpected complications:**
- The `reflex_angle_convexity_test` concept did not exist when the Q-N103-B markers were added; an `orphan_van_hiele_marker` validation error surfaced and was fixed by adding the missing concept record (same source/citation as the other three convexity tests, anchored to N103 Activity 1.13).
- Q-007's `height_vs_slanted_side` topic atom: the original record had `topic = area_perimeter` but Tio's plan said the concept moves to `concepts/angles.pl`. The migration uses topic = `angles` to keep file location and topic atom consistent, with a note in the migrated record explaining the call.

---

## Standards Re-run Resolution — 2026-05-03 evening

After Tio reviewed this wakeup and rejected the `is_an_alias_of` indirection plan ("it is just so important that the standards are correct and I worry that adding a mapping statement will make it too easy to break the links"), the Standards Re-run Worker did a direct concept-aware second pass over all 84 dangling `standard_anchor` records flagged by the new `orphan_standard_anchor/3` validator clause.

**Resolution counts (per the rules in the worker's charter):**

- Rule 1 — direct match against an existing canonical concept: **0**. (No dangling ID had a verbatim existing geom_concept; the standards mapper and concept diggers really had used disjoint conventions.)
- Rule 2 — near-match re-anchored in place: **9 standard_anchor records** repointed across CCSS, IM, and Indiana files. The merges:
  - `shape_naming_orientation_invariant` → `orientation_invariant_naming` (K.G.A.2)
  - `line_symmetry` → `line_of_symmetry` (4.G.A.3, IM-G4-U8)
  - `category_subcategory_attributes` → `category_of_shapes_as_container` (5.G.B.3 — collapses to the L&N digger's anchor for the metaphor that licenses van Hiele level-2 inclusion)
  - `pythagorean_theorem_proof`, `pythagorean_theorem_application` → `pythagorean_theorem` (8.G.B.6, 8.G.B.7, IM-G8-U8, 8.GM.3 — four standards now share the canonical pythagoras-cluster anchor; the `proof/application/distance` distinctions are reading-of-the-same-theorem rather than separate concepts)
  - `interior_angle_sum_polygons` → `polygon_angle_sum_via_triangulation` (6.GM.2)
- Rule 3 — new `geom_concept/4` records authored in `concepts/standards_anchors.pl`: **40 concepts** covering the remaining 75 standard_anchor records. These are pedagogical-unit-grained concepts (e.g., `shape_recognition_2d_3d`, `defining_vs_nondefining_attributes`, `area_compose_decompose_polygons`, `rigid_motion_properties`) at the standard's intended level of abstraction, sitting above the finer-grained VdW / N103 / L&N concepts. All Tier 2 (anchored by the standards framework on one side and the synthesizer's judgment on the other).
- Rule 4 — Tier-4 ambiguity questions: **0**. Every dangling case resolved cleanly to (2) or (3).

**Final state:**

- Validator: `geometry KB validates clean.` — zero errors.
- Coverage: `report(225, 96, 34, 27, 96, 87, 0, 0, 337, 136, 100)` — **225 concepts** (+40 from standards_anchors.pl), **136 Tier-2 records** (was 96; +40 new geom_concepts each came in at Tier 2). All other counts unchanged.

**Patterns worth knowing about:**

1. **The standards-mapper's IDs are mostly correctly grained** — they describe intended pedagogical units rather than fine-grained mathematical objects. Treating them as new canonical concepts (rather than aliasing them onto smaller pieces) preserves what the standards actually claim. The exceptions were the `pythagorean_theorem_*` family, where the standards over-distinguished a single theorem into proof/application/distance facets, and the symmetry / orientation / category-container cases where the digger-side concept was clearly the same idea named differently.
2. **The four pythagorean standards collapsing to one concept** is a feature: the standard's split between proof-of, application-of, distance-via, and Indiana's plain-application is a reading-distinction, not a concept-distinction. After the merge they all share `pythagorean_theorem` as the canonical anchor, which is the correct relationship.
3. **Indiana's standards mostly mirror CCSS** but added five Indiana-specific pedagogical units (compose_decompose_shapes at 2.G.2, points_lines_segments at 3.G.2, parallelogram_rhombus_trapezoid at 4.G.1, triangles_circles_radius_diameter at 5.G.1, measurement_unit_conversion at 6.GM.1, volume_cylinders_prisms at 7.GM.3). Each got its own concept rather than being forced under a CCSS sibling.
4. **No `valid_topic` for measurement** — `measurement_unit_conversion` (6.GM.1) is a poor fit for any of the ten existing topics; placed in `area_perimeter` as the closest match. If a `measurement` topic atom gets added later, this concept should move there. Same family: `length_measurement_as_unit_iteration` (already in area_perimeter from synthesizer_anchors.pl).
5. **The `is_an_alias_of` decision was right to reject.** The standards layer is now structurally simple: every `standard_anchor` points directly at a `geom_concept` that exists. No indirection, no chance of an alias being deleted and silently breaking the link. Q-008 / Q-MH-A and any future inclusive-vs-exclusive merges can still use aliases for *concept-to-concept* relations without affecting the standards layer.

The standards-reconciliation debt is now zero. Coverage thinness in pythagoras and similarity_congruence (noted below) is unaffected by this pass.

---

## TL;DR

The overnight pass landed. **185 concepts, 96 misconceptions, 34 metaphors, 27 van Hiele markers, 96 bootstraps, 87 standards anchors. Tier breakdown: 337 Tier-1, 96 Tier-2, 100 Tier-3.** The KB validates clean (zero `validate_geom_kb` errors). The L&N digger's 27 orphan-metaphor concepts are now anchored as Tier-2 synthesizer ratifications, and 31 cross-source triangulations got promoted to Tier 2. The five diggers are agreeing more than expected on the central concepts (square/rectangle classification, polygon recognition, quadrilateral hierarchy, the area-perimeter cluster) — the synthesizer was able to ratify those without controversy. **The big morning surprise** is that the standards mapper and the concept diggers used disjoint concept-ID conventions: 39 of 40 CCSS K-8.G standards are anchored to IDs no concept declares, so the standards layer is almost entirely dangling. That's not a validator error (no `orphan_standard_anchor` rule), but it's the biggest structural debt to clean up tomorrow. Below: Tier 3 review queue (the headline ~30), Tier 4 pointer to the cleaned-up `OPEN_QUESTIONS.md`, schema/scope holes, coverage gaps, recommended next session.

---

## Headline numbers

| Axis | Count |
|---|---|
| Concepts | **185** (+27 from synthesizer anchors) |
| Misconceptions | 96 |
| Metaphors | 34 |
| Van Hiele markers | 27 (across 16 concepts) |
| Bootstraps | 96 |
| Standards anchors | 87 (40 CCSS, 25 Indiana, 22 IM lessons) |
| Tier 1 records | **337** |
| Tier 2 records | **96** (was 46; +50 from Wave 3) |
| Tier 3 records | 100 |
| Validation errors | **0** |

Topic distribution (concepts):
- classification: heaviest (quadrilateral hierarchy + ~25 N103 concepts + VdW)
- area_perimeter: dense (16 misconceptions; the area cluster is fully anchored)
- transformations: dense (translation/reflection/rotation triad + symmetry)
- shape_recognition, attributes, similarity_congruence, angles, volume_surface_area, coordinate_geometry, pythagoras: medium
- All ten valid_topic atoms have at least one concept

---

## What landed

**Wave 1.** Schema + LK_RB audit + standards mapping. 87 standards anchored across CCSS, Indiana, IM. Audit flags the Container × Measuring-Stick distinction the L&N digger then carried through. ✓

**Wave 2.**
- **Van de Walle digger.** Ch. 19–20 mined; level-0/1/2/3 vH markers anchored to `square_recognition`, `rectangle_class`, `cube_class`, `polygon_recognition`, `quadrilateral_classification`, `minimal_defining_list`, `triangle_angle_sum`, `diagonals_of_rectangles_proof`, plus 35 Activity 20.x bootstraps. Five out-of-charter concepts (area/perimeter/angles/ruler) registered in classification.pl with self-flag — see Q-007.
- **Van Hiele dissertation digger.** Levels 0–4 with kid-talk markers from Fuys-replication clinicals; transitions; `geometric_language` meta-concept; `axiom_system_comparison` at level 4. Phase theory left at Tier 3 — see Q-001.
- **L&N digger.** 27 metaphor records across SPG, fictive motion, spaces-as-sets-of-points, unit-circle blend, rotation-plane blend, BMI projective, and Container schema. Wave 1 audit prevented re-extraction. Concept IDs were *proposed* by the digger and ratified by the synthesizer (Wave 3, this file).
- **N103 digger.** ~30 chapter-level concepts (convexity tests, polyhedra, Pick's formula, transformation diagnostics, Julie's Way / Sean's Idea). Six adjudication questions (Q-N103-A through Q-N103-F).
- **Misconception harvester.** 13 BENNY ports (Tier 1) + 20 multi-source Tier-2 + 48 single-source Tier-3 from research_corpus. Three Tier-4 questions (Q-MH-A/B/C).

**Wave 3 (synthesizer, this writer).**
- `concepts/synthesizer_anchors.pl` — 27 concept records anchoring the L&N digger's metaphor IDs. All `orphan_metaphor` errors cleared.
- `concepts/synthesizer_triangulations.pl` — 31 Tier-2 ratifications where two or more diggers attached to the same concept ID. No Tier 1 records were rewritten.
- `OPEN_QUESTIONS.md` — restructured from per-digger appended sections into topic-grouped document. Two synthesizer resolutions added.

---

## Tier 3 review queue

The 100 Tier-3 records break down as: 49 misconceptions (single-source from research corpus or single-digger paraphrase), 33 concepts (mostly N103 chapter-specific or L&N inferred extensions), 9 metaphors, 5 bootstraps, 3 vH markers, 1 material_inference. The headline ~30 are listed below in three groups; for each, the workflow is **keep / promote-to-T1 / demote-to-T4 / drop**.

The remaining ~70 are mostly single-source research-corpus misconceptions documented in `geometry/corpus/misconception_harvest_log.md` Part B Tier 3. Walk that file directly when you need them.

### Group A — Single-source research-corpus misconceptions (the big residue)

These come from one paper each in `research_corpus/research.db`. Repair texts are good; sources are real but solitary. The default move is **keep at Tier 3** (legitimate, just not corroborated). Promote to T1 only if you recognize the misconception from your own classroom experience.

| ID | Concept | Source |
|---|---|---|
| `parallelogram_trapezoid_confusion` | quadrilateral_hierarchy | corpus_39098 |
| `cylinder_cone_prototype_image` | quadrilateral_hierarchy | corpus_39828 |
| `definitional_under_or_over_specification` | quadrilateral_hierarchy | corpus_37729 |
| `compensating_dimensions_preserve_perimeter` | perimeter_as_boundary_traversal | corpus_38063 |
| `area_unit_conversion_linear` | area_scales_quadratically | corpus_38993 |
| `area_in_linear_units` | area_unit_is_a_square | corpus_38992 |
| `area_only_for_measurable_polygons` | area_as_interior_coverage | corpus_38995 |
| `diagonal_grid_segment_unit` | perimeter_as_boundary_traversal | corpus_38694 |
| `side_from_area_by_halving` | area_as_array_structure | corpus_40241 |
| `ribbon_uses_volume_or_faces` | perimeter_as_boundary_traversal | corpus_40443 |
| `angle_as_single_ray` | angle_as_two_rays_from_vertex | corpus_38239 |
| `angle_as_static_region` | angle_measure_as_rotation | corpus_40350 |
| `triangle_angle_sum_depends_on_size` | triangle_angle_sum_180 | corpus_37875 |
| `hexagon_angle_sum_six_triangles` | polygon_angle_sum_via_triangulation | corpus_37650 |
| `rotation_as_one_arm_movement` | angle_measure_as_rotation | corpus_39401 |
| `translation_static_endpoint_relationship` | translation | corpus_38387 |
| `zero_vector_means_no_translation` | translation | corpus_38389 |
| `flip_out_of_plane_for_reflection` | reflection_distinct_from_rotation | corpus_40528 |
| `oblique_mirror_distorted_image` | reflection_preserves_perpendicular_distance | corpus_39627 |
| `angles_scale_with_sides_in_similar` | similar_figures | corpus_37756 |
| `volume_conversion_with_linear_factor` | volume_scales_cubically | corpus_39662 |
| `lateral_vs_total_surface_confusion` | surface_area_of_sphere | corpus_38052 — note: harvester flagged this anchor as off-target; consider creating `surface_area_of_solid` parent |
| `slope_increases_along_hill` | slope_invariant_along_a_line | corpus_38342 |
| `slope_measured_as_length` | slope_as_ratio_of_change | corpus_38411 |
| `right_triangle_without_longest_side` | pythagorean_theorem | corpus_37755 |
| `pythagorean_pattern_misapplied` | pythagorean_theorem | corpus_40244 |
| `tetrahedron_called_triangle` | shape_identified_by_properties_not_appearance | corpus_38912 |
| `parallel_as_equal_length` | parallelism_as_constant_distance | corpus_38807 |
| `equal_sides_imply_equal_angles` | equal_sides_does_not_imply_equal_angles | corpus_39508 |
| `oblique_axis_as_negative_direction` | coordinate_axes_3d_orthogonal | corpus_37622 |

### Group B — N103 chapter-specific concepts (Tier 3 because N103-only)

These are concepts N103 (Aichele & Wolfe 2008) names that no other source attests in our corpus. Default move is **keep** (the N103 vocabulary is what the preservice-teacher methods tool will recognize). Promote to T1 if VdW or research-corpus parallels surface; demote to T4 if they look too N103-specific to be load-bearing.

- `julies_way_area_method`, `seans_idea_area_method`, `cut_up_area_method`, `take_away_area_method` — student-invented area methods. Q-N103-A asks whether these belong as concepts at all.
- `rubber_band_convexity_test`, `kids_crawl_space_convexity_test`, `line_segment_convexity_test` — the four convexity tests. Q-N103-B asks whether to canonicalize.
- `tracing_paper_diagnostic` — Q-N103-C asks concept vs. methodology.
- `one_sided_figure`, `two_sided_figure` — Q-N103-D asks terminology choice.
- `tile_shape`, `solid_tile_shape`, `skinny_tile_shape`, `skew_geoboard_figure`, `medial_quadrilateral`, `n103_seven_quadrilateral_types`, `picks_formula` — N103-specific named objects.
- `erikas_idea_only_works_under_conditions` — Tier 3 misconception, N103 Activity 1.6.

### Group C — L&N digger inferred extensions (Tier 3 because L&N silent or partial)

These are metaphor concepts where L&N's own text is silent on the geometric specialization but the digger inferred from the underlying schema. Default move is **keep** until VdW or N103 cross-attest, then promote to T1.

- `ray_as_oriented_path` — SPG inferred for unbounded ray.
- `geometric_rotation_as_motion` — L&N treat point-rotation; figure-rotation is inferred.
- `circle_as_polygon_limit` — L&N flag this as the *foil* for BMI; the geometric reading requires adding BMI explicitly.
- `space_set_blend_for_line` — Tier 3 because it's a blend, not a stable concept.
- `length_measurement_as_unit_iteration`, `congruence_by_superposition`, `similarity_as_uniform_scaling`, `area_as_2d_unit_iteration`, `volume_as_3d_unit_iteration` — measuring-stick generalizations L&N don't formalize.

The remaining Tier 3 records (vH paraphrases, bootstrap-anchored) are best read in context. `corpus/misconception_harvest_log.md` Part B and the per-file tier annotations carry the original digger reasoning.

---

## Tier 4 open questions

The full restructured catalogue is at `/Users/tio/Documents/GitHub/umedcta-formalization/geometry/OPEN_QUESTIONS.md`. One-line summaries (so you can prioritize without opening it):

**Concept-ID and terminology** (the densest group)
- **Q-003** — Original van Hiele numbering (1–5 vs 0–4) — schema change vs corpus-only.
- **Q-004** — `square_recognition` (VdW) vs `square_rectangle_classification` (VH) — keep both or merge. Synthesizer ratified the latter as load-bearing (3-source triangulation); strong vote for merge.
- **Q-005** — `triangle_angle_sum` vs `triangle_angle_sum_180` — level-2 conjecture vs level-3 formal statement.
- **Q-006** — `polygon_recognition` catch-all risk — should 8 transformation activities re-key to specific transformation concepts.
- **Q-007** — Five concepts in classification.pl that should live in area_perimeter / angles topic files.
- **Q-008 / Q-MH-A** — Trapezoid inclusive vs exclusive (likely fold into one resolution).
- **Q-N103-A through Q-N103-F** — six N103-specific terminology adjudications.

**Theory disputes**
- **Q-001** — Status of van Hiele's "five phases" theory: keep Tier 3, demote to Tier 4, or split.

**Provenance**
- **Q-002** — `vh_fuys_clinical_*` utterances are 1988 replications, not Pierre or Dina's 1957 transcripts: Tier 1 or Tier 2.

**Pedagogical commitments**
- **Q-MH-B** — PCK gaps as KB records: separate `pck/` subdir, `population` slot on misconceptions, or stay strictly student-side.

**Schema or scope**
- **Q-MH-C** — `area_as_array_structure` in `area_perimeter.pl` vs `attributes.pl`.

---

## Schema or scope holes

These are the slots diggers wanted but didn't have. Cross-digger consistency suggests they're real.

1. **`OriginalLabel` slot on `van_hiele_marker/4`** (Q-003). Multiple diggers had to drop the original Dutch / 1959 numbering when porting van Hiele's own writing. The corpus file preserves it but downstream consumers can't query it. Likely-real schema extension.

2. **`population` slot on `geom_misconception/6`** (Q-MH-B). Several research-corpus rows describe teacher (PCK) failures, not student misconceptions. Harvester skipped them rather than misclassify. If you want PCK in the same KB, this is the right slot.

3. **No `orphan_standard_anchor` validation rule.** `validate_geom_kb` doesn't catch standards anchored to non-existent concepts — see the coverage-gap finding below. Adding the rule would have flagged this overnight. Recommend adding tomorrow.

4. **No `is_an_alias_of` polarity in `material_inference/4`.** I considered using `material_inference(canonical, is_an_alias_of(deprecated), entitled)` for L&N orphan canonicalization but chose to skip it because no competing canonical was proposed. If Q-004 or Q-005 resolves to a merge, the alias mechanism will need to formalize.

5. **No `pedagogy` topic atom in `valid_topic/1`** (Q-N103-E). The CD-problem framework would land cleanly there if you decide to record it.

---

## Coverage gaps

### CCSS K-8.G — almost entirely dangling

This is the surprise. The Wave 1 standards mapper anchored 40 CCSS K-8.G standards to concept IDs like `shape_recognition_2d_3d`, `defining_vs_nondefining_attributes`, `compose_2d_3d_shapes`, etc. The concept diggers (VdW, N103, etc.) used different IDs (`polygon_recognition`, `quadrilateral_classification`, `square_rectangle_classification`, etc.). **Of 40 CCSS standards, only 1 (`quadrilateral_hierarchy` ↔ 3.G.A.1) has a matching `geom_concept`.** The other 39 are dangling.

Examples (all need either a concept declaration with the standards-mapper ID, or an alias / migration to an existing concept ID):
- K.G.A.1 → `shape_recognition_2d_3d` (no concept; closest existing: `polygon_recognition` + `three_d_shape_recognition`)
- K.G.A.2 → `shape_naming_orientation_invariant` (no concept; closest existing: `orientation_invariant_naming`)
- 1.G.A.1 → `defining_vs_nondefining_attributes` (no concept; closest: `shape_identified_by_properties_not_appearance`, `definition_requires_necessary_and_sufficient_conditions`)
- 4.G.A.3 → `line_symmetry` (no concept; closest: `line_of_symmetry`, `reflectional_symmetry`)
- 5.G.A.1 → `coordinate_system_axes` (no concept; closest: `coordinate_axes_3d_orthogonal`)
- 8.G.A.1 → `rigid_motion_properties` (no concept; closest: `translation`, `reflection`, `rotation`)
- 8.G.B.6/7/8 → `pythagorean_theorem_proof`, `_application`, `_distance_coordinates` (no concepts; closest: `pythagorean_theorem`, `converse_of_pythagorean_theorem`, the area_perimeter pythagorean cluster)

A full reconciliation table is in `geometry/standards/ccss_geometry.pl` next to each anchor; the concept side is in `geometry/concepts/*.pl`. The cleanest fix is a Wave-3.5 standards-reconciliation pass: declare aliases for the standards-mapper IDs (Tier 2, since the standards mapper and concept diggers both attest the same underlying concept), then `material_inference(<concept-id>, is_an_alias_of(<standards-id>), entitled)` to make queries route either way. I left this for tomorrow rather than risk silent miscoupling overnight.

### Indiana standards

25 anchors. Same structure: Indiana's IDs almost certainly mirror CCSS's standards-mapper IDs (the standards mapper ran them in the same pass). Fixing the CCSS reconciliation will fix Indiana too.

### IM lesson anchors

22. Anchored as IDs-only per spec — no full content. Out of scope unless you want to extract IM lesson content (separate session).

### Topics that are thin

- **pythagoras** — 7 concepts, but most other topics' coverage is 15-25 concepts. Two of the 7 are `pythagorean_theorem` itself and `converse_of_pythagorean_theorem`. Worth a focused pass against VdW Ch. 20 + Indiana 8.G + Eureka.
- **similarity_congruence** — 13 concepts but only 4 misconceptions. The misconception harvester explicitly flagged this as sparse in the K-8 band.

---

## Validator-flagged residue

None. `validate_geom_kb` returns clean.

The only warnings during loading are two singleton-variable warnings in `metaphors/lakoff_nunez_inventory.pl` (lines 216 and 240, in the BMI projective and curve-as-limit-of-curves clauses). These are cosmetic — single-occurrence variable names in `source_target/2` mapping pairs that should be quoted atoms or replaced with `_AC_lengths_unbounded`. Not a load-blocker; cleanup later.

---

## Recommended next session

In priority order:

1. **Standards reconciliation pass** (~1 hour). The 39 dangling CCSS anchors are the biggest structural debt. Two paths: (a) declare each standards-mapper ID as an `is_an_alias_of` material_inference pointing at the existing concept, or (b) rename the standards-mapper IDs to match the concept-side IDs. Path (a) is reversible and preserves the standards-mapper's authored statements; path (b) is cleaner but harder to audit. I'd take (a). Add the `orphan_standard_anchor/1` validator rule at the same time so this category never hides again.

2. **Walk Q-004 + Q-005 + Q-006 + Q-007** (~30 min). Four concept-ID adjudications, all relatively quick. Q-004 likely merges; Q-005 likely keeps both; Q-006 wants a re-keying pass; Q-007 wants a topic-file move.

3. **Walk the N103 questions (Q-N103-A through Q-N103-F)** (~20 min). These are mostly "is this a concept or a methodology" calls. Most were left at the digger's default; you confirm or override.

4. **Walk Q-001 + Q-002** (~10 min). Two theory/provenance calls.

5. **Tier 3 review queue Group A** — read-and-mark workflow over the ~30 listed misconceptions. Most are keep; the lateral_vs_total_surface_confusion one wants a `surface_area_of_solid` parent created.

6. **(Optional) Coverage extension passes**:
   - pythagoras topic deepening (~30 min against VdW + Eureka)
   - similarity_congruence misconception harvest (separate corpus pass)
   - PCK gap harvesting if Q-MH-B resolves to (a)

7. **Re-run `validate_geom_kb` and `coverage_report/1`** after each batch of changes, tracking the tier numbers as your durable progress signal.

---

## Files to read first

- `/Users/tio/Documents/GitHub/umedcta-formalization/geometry/OPEN_QUESTIONS.md` — restructured Tier-4 catalogue.
- `/Users/tio/Documents/GitHub/umedcta-formalization/geometry/concepts/synthesizer_anchors.pl` — Wave 3 anchor records (27 concepts).
- `/Users/tio/Documents/GitHub/umedcta-formalization/geometry/concepts/synthesizer_triangulations.pl` — Wave 3 Tier-2 promotions (31 records).
- `/Users/tio/Documents/GitHub/umedcta-formalization/geometry/corpus/misconception_harvest_log.md` — full harvest narrative (Tier 3 single-source detail).
- `/Users/tio/Documents/GitHub/umedcta-formalization/geometry/standards/ccss_geometry.pl` — standards-mapper output, the dangling-anchor source.

---

*Synthesizer's note: this push went well. The five diggers chose largely compatible vocabulary on the central concepts, and the orphan_metaphor cluster turned out to be a clean canonicalization rather than a real disagreement. The big structural debt — standards reconciliation — is mechanical work, not a thinking problem. The taste calls in OPEN_QUESTIONS.md are the actual interactive-session work for tomorrow.*
