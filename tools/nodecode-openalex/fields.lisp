;;;; fields.lisp --- valid filter and select names, shipped rather than fetched.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; OpenAlex will enumerate every valid field for an entity in the 400 it
;;;; answers a wrong one with, but asking costs a list call, and a tool whose
;;;; job is to save the model a wasted call should not itself cost money. So the
;;;; vocabulary lives here and OPENALEX_FIELDS makes no request at all.
;;;;
;;;; The filter set and the select set are not the same list, which is the
;;;; mistake this file exists to prevent: authorships.author.id filters and
;;;; does not select, and authors_count is a valid filter and an invalid
;;;; select.

(in-package #:nodecode-openalex)

(defparameter +entities+ '("works" "authors" "sources" "institutions"
                           "topics" "publishers" "funders" "keywords")
  "The collections OpenAlex serves, in the order the tools advertise them.")

(defparameter +filter-fields+
  '(("works"
     ("identity" "doi" "ids.pmid" "ids.pmcid" "has_doi" "has_pmid" "openalex_id")
     ("authorship" "authorships.author.id" "authorships.author.orcid"
      "authorships.institutions.id" "authorships.institutions.ror"
      "authorships.institutions.country_code" "authorships.institutions.lineage"
      "authorships.is_corresponding" "authors_count" "has_orcid")
     ("venue" "primary_location.source.id" "primary_location.source.issn"
      "primary_location.source.is_in_doaj" "primary_location.source.host_organization"
      "locations.source.id" "best_oa_location.license")
     ("time" "publication_year" "publication_date" "from_publication_date"
      "to_publication_date")
     ("type and access" "type" "type_crossref" "is_oa" "open_access.oa_status"
      "has_fulltext" "has_abstract" "language" "version")
     ("impact" "cited_by_count" "fwci" "citation_normalized_percentile.is_in_top_1_percent"
      "citation_normalized_percentile.is_in_top_10_percent" "referenced_works_count")
     ("subject" "primary_topic.id" "primary_topic.field.id" "primary_topic.subfield.id"
      "primary_topic.domain.id" "topics.id" "keywords.id"
      "sustainable_development_goals.id" "concepts.id")
     ("funding" "awards.funder_id" "funders.id" "grants.funder")
     ("citation graph" "cites" "cited_by" "related_to" "referenced_works")
     ("quality" "is_retracted" "is_paratext" "has_references")
     ("search" "default.search" "title.search" "abstract.search"
      "title_and_abstract.search" "fulltext.search" "keyword.search"
      "raw_affiliation_strings.search"))
    ("authors"
     ("identity" "orcid" "has_orcid" "openalex_id")
     ("affiliation" "affiliations.institution.id" "affiliations.institution.ror"
      "affiliations.institution.country_code" "last_known_institutions.id"
      "last_known_institutions.country_code")
     ("impact" "works_count" "cited_by_count" "summary_stats.h_index"
      "summary_stats.i10_index")
     ("subject" "topics.id" "topic_share.id" "x_concepts.id")
     ("search" "default.search" "display_name.search"))
    ("sources"
     ("identity" "issn" "issn_l" "has_issn" "openalex_id")
     ("nature" "type" "is_oa" "is_in_doaj" "is_core" "is_preprint_repository"
      "oa_flip_year")
     ("publisher" "host_organization" "host_organization_lineage" "country_code")
     ("impact" "works_count" "cited_by_count" "summary_stats.h_index" "apc_usd")
     ("search" "default.search" "display_name.search"))
    ("institutions"
     ("identity" "ror" "has_ror" "openalex_id")
     ("place" "country_code" "continent" "is_global_south")
     ("nature" "type" "lineage" "status" "repositories.id" "roles.id")
     ("impact" "works_count" "cited_by_count" "summary_stats.h_index")
     ("search" "default.search" "display_name.search")))
  "Valid filter names by entity and by family. A plain filter costs nothing to
read and a rejected one costs a list call, so these are worth carrying.")

(defparameter +work-select+
  '("id" "doi" "title" "display_name" "relevance_score" "publication_year"
    "publication_date" "ids" "language" "primary_location" "sources" "type"
    "type_crossref" "indexed_in" "open_access" "authorships" "institutions"
    "countries_distinct_count" "institutions_distinct_count"
    "corresponding_author_ids" "corresponding_institution_ids" "apc_list"
    "apc_paid" "fwci" "has_fulltext" "fulltext_origin" "cited_by_count"
    "citation_normalized_percentile" "biblio" "is_retracted" "is_paratext"
    "primary_topic" "topics" "keywords" "concepts" "mesh" "locations_count"
    "locations" "best_oa_location" "sustainable_development_goals" "awards"
    "funders" "datasets" "versions" "has_content" "content_urls"
    "referenced_works_count" "referenced_works" "related_works"
    "abstract_inverted_index" "cited_by_api_url" "counts_by_year"
    "updated_date" "created_date")
  "Every top-level field a work can select. Nested paths are not selectable.")

(defparameter +vocabularies+
  '(("work types" "article" "book" "book-chapter" "dataset" "dissertation"
     "editorial" "erratum" "grant" "letter" "libguides" "other" "paratext"
     "peer-review" "preprint" "reference-entry" "report" "retraction" "review"
     "standard" "supplementary-materials")
    ("open access status" "diamond" "gold" "green" "hybrid" "bronze" "closed")
    ("source types" "journal" "repository" "conference" "ebook platform"
     "book series" "metadata" "other")
    ("institution types" "education" "healthcare" "company" "archive" "nonprofit"
     "government" "facility" "funder" "other")
    ("topic domains" "Health Sciences" "Life Sciences" "Physical Sciences"
     "Social Sciences"))
  "The closed vocabularies, so a filter value is never guessed.")

(defparameter +groupable-hints+
  '("publication_year" "type" "open_access.oa_status" "is_oa" "language"
    "authorships.institutions.country_code" "authorships.institutions.id"
    "authorships.author.id" "primary_location.source.id" "primary_topic.id"
    "primary_topic.field.id" "primary_topic.domain.id"
    "sustainable_development_goals.id" "is_retracted" "cited_by_count")
  "What a count most usefully breaks down by.")

(defparameter +syntax-lines+
  '("AND: a comma between filters - publication_year:>2020,is_oa:true"
    "OR: a pipe within one field, at most 100 values - type:article|review"
    "NOT: ! before the value - type:!paratext"
    "range: 2020-2024, or cited_by_count:>100"
    "sort: field:desc or field:asc; the -field form the API documents does not work")
  "The filter language in five lines, which is all most queries need.")

(defun filter-fields (entity)
  "ENTITY's filter families, or a small common set for the collections the
original did not enumerate."
  (or (cdr (assoc entity +filter-fields+ :test #'equal))
      '(("common" "display_name.search" "works_count" "cited_by_count"
         "openalex_id"))))
