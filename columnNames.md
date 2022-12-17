### This file lists all the column names in `Graphem_MAIN.xlsb` with a short explanation

-   `index`: case ID person_ID: text ID

-   `first_letter`: first letter of the word (not used in analysis, just for data handling)

-   `word`: word

-   `lemma`: lemmatized word / lexical part of the word

-   `word_index`: position of the word within the text letter_index: position of the letter within the word

-   **`letter`: the current letter** (this is the base variable of each case)

-   `letter_rec`: recoded letter (without diacritics, upper-case letters are coded as "0")

-   `next_letter`: following letter

-   `prev_letter`: preceding letter

-   `word_length`: number of letters in the word

-   `letter_count`: frequency of the letter in the whole corpus

-   `word_count`: frequency of the word in the whole corpus

-   `word_frequency_class`: frequency class of the word according to Zipf's law

-   `bigramm_prev`: bigram with the preceding letter

-   `bigramm_next`: bigram with the following letter

-   `bigramm_prev_count:` frequency of the bigram with the preceding letter

-   `bigramm_next_count`: frequency of the bigram with the following letter

-   `upper_case`: is the letter uper_case or not?

-   `junc_border`: is the letter joined with the next one or not?

-   `junc_border_before`: is the letter joined with the previous one or not?

-   `WaZ`: is there a hyphen after the letter? (hyphenation at the end of a line)

-   `word_struc`: position of the letter within the word (initial, medial, final)

#### Morphology

-   `morph_border`: does a morphological border exist after the letter?

-   `morph_cat`: category of the letter's morpheme

-   `morph_border_type`: type of the morphological border after the letter

-   `morph_process_type`: morphological process that happens at the letter

-   `word_type`: does the word have inflection or not?

#### Segmental phonology

-   `phon_class`: phonological class (full vowel, reduced vowel, consonant)

-   `phon_vred_type`: type of reduced vowel (/ə/ or /ɐ/)

-   `phon_vposition`: position of tongue

-   `phon_vopen`: openness of the vowel

-   `phon_vround`: roundedness of the vowel

-   `phon_vtension`: tension of the vowel

-   `phon_ctype`: type of consonant

-   `phon_cloc`: place of articulation of the vowel

-   `phon_cvoiced`: voicedness of the vowel

-   `phon_complexity`: phoneme complexity (diphthong, affricate, ambisyllabic consonant)

#### Syllabic phonology

-   `psyll_count`: number of phonological syllables in the word

-   `psyll_index`: position of phonological syllable in the word

-   `psyll_struc`: position within the syllable (onset, nucleus, key, coda, extra-syllabic)

-   `psyll_border`: is there a phonological syllable border after the letter?

-   `psyll_type`: type of phonological syllable (prominent, not prominent, reduced)

#### Metric phonology

-   `pfoot`: phonological foot (trochee, dactylus, degenerated, extra-metric)

-   `pfoot_can`: canonicity of phonological foot

-   `pfoot_border`: is there a phonological foot border after the letter?

-   `pfoot_count`: number of phonological feet in the word

-   `pfoot_index`: position of the phonological foot within the word

#### Graphotactics

-   `double_cons`: segment of a double consonant

-   `double_index`: first oder second part of a double consonant

-   `graph_complexity`: does the letter belong to a complex grapheme?

-   `graph_complexity_both`: see `graph_complexity`, but with both segments of the complex grapheme annotated

#### Syllabic graphematics

-   `gsyll_count`: number of graphematic syllables in the word

-   `gsyll_index`: position of graphematic syllable in the word

-   `gsyll_struc`: position within the syllable (onset, nucleus, key, coda, extra-syllabic)

-   `gsyll_border`: is there a graphematic syllable border after the letter?

-   `gsyll_type`: type of graphematic syllable (prominent, not prominent, reduced)

#### Metric graphematics

-   `gfoot`: graphematic foot (trochee, dactylus, degenerated, extra-metric)

-   `gfoot_can`: canonicity of graphematic foot

-   `gfoot_border`: is there a graphematic foot border after the letter?

-   `gfoot_count`: number of graphematic feet in the word

-   `gfoot_index`: position of the graphematic foot within the word

-   `gfoot_noDac`: same as `gfoot`, but without graphematic dactyli

-   `gfoot_can_noDac`: same as `gfoot_can`, but without graphematic dactyli

-   `gfoot_border_noDac`: same as `gfoot_border`, but without graphematic dactyli

-   `gfoot_count_noDac`: same as `gfoot_count`, but without graphematic dactyli

-   `gfoot_index_noDac`: same as `gfoot_index`, but without graphematic dactyli

#### Graphematic functions

-   `h_func:` lenghtening-h, syllable-h, phonographic h, etymologic h, graphic h

-   `e_func`: writing of reduction vowel, full vowel, part of a diphthong, \<ie\>, etymologic, umlaut-e

-   `morphographic`: can the letter only be explained with morphological operations?

#### Graphetics

-   `head_exist`: does the letter have a head?

-   `coda1_exist`: does the letter have first coda?

-   `coda2_exist`: does the letter have a second coda?

-   `head_form`: shape of head

-   `coda1_form`: shape of coda 1

-   `coda2_form`: shape of coda 2

-   `close`: closedness of the letter shape

-   `head_free`: is the head free?

-   `contacts`: number of contact points of each coda with the head

-   **`code`: short name of letter form**

#### Intra-rater-reliability

-   `irr_subset`: flag variable wether this case is used for irr

-   `code_val`: letter form that has been annotated in irr

-   `diff`: is the rating the same as in the first rating?
