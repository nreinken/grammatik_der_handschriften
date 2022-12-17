---
editor_options: 
  markdown: 
    wrap: 72
---

### This file lists all the column names in Graphem_MAIN.xlsb with a short explanation

***(Todo: translate)***

-   `index`: case ID person_ID: text ID

-   `first_letter`: first letter of the word (not used in analysis, just
    for data handling)

-   `word`: word

-   `lemma`: lemmatized word / lexical part of the word

-   `word_index`: position of the word within the text letter_index:
    position of the letter within the word

-   **`letter`: the current letter** (this is the base variable of each
    case)

-   `letter_rec`: recoded letter (without diacritics, upper-case letters
    are coded as "0")

-   `next_letter`: following letter

-   `prev_letter`: preceding letter

-   `word_length`: number of letters in the word

-   letter_count: frequency of the letter in the whole corpus

-   word_count: frequency of the word in the whole corpus

-   word_frequency_class: frequency class of the word according to
    Zipf's law

-   bigramm_prev: bigram with the preceding letter

-   bigramm_next: bigram with the following letter

-   bigramm_prev_count: frequency of the bigram with the preceding
    letter

-   bigramm_next_count: frequency of the bigram with the following
    letter

-   upper_case: is the letter uper_case or not?

-   junc_border: is the letter joined with the next one or not?

-   junc_border_before: is the letter joined with the previous one or
    not?

-   WaZ: is there a hyphen after the letter? (hyphenation at the end of
    a line)

-   word_struc: position of the letter within the word (initial, medial,
    final)

#### Morphology

morph_border: Morphologische Grenze nach dem Buchstaben\
morph_cat: Morphologische Kategorie\
morph_border_type: Typ der morphologischen Grenze\
morph_process_type: Prozess, der an der morphologischen Grenze passiert\
word_type: flektierendes oder nicht-flektierendes Wort

#### Phonologie (Segmente)

phon_class: Phonologische Klasse (Vollvokal, Reduktionsvokal,
Konsonant)\
phon_vred_type: Typ des Reduktionsvokals (Zentralschwa, Tiefschwa)\
phon_vposition: Zungenposition des Vollvokals\
phon_vopen: Mundöffnung des Vollvokals\
phon_vround: Lippenrundung des Vollvokals\
phon_vtension: Gespanntheit des Vollvokals\
phon_ctype: Konsonantentyp\
phon_cloc: Artikulationsort des Konsonanten\
phon_cvoiced: Stimmhaftigkeit des Konsonanten\
phon_complexity: Komplexität des Phonems (Diphthong, Affrikate,
Ambisilbischer Konsonant)

#### Phonologie (Silben)

psyll_count: Anzahl der phonologischen Silben im Wort\
psyll_index: Position der phonologischen Silbe im Wort\
psyll_struc: Position innerhalb der phonologischen Silbe (Onset, Kern,
Key, Coda, extrasilbisch)\
psyll_border: phonologische Silbengrenze nach dem Buchstaben\
psyll_type: Typ der phonologischen Grenze (prominent, nicht prominent,
reduziert)

#### Phonolgoie (Füße)

pfoot: phonologischer Fuß (Trochäus, Daktylus, degeneriert,
extrametrisch)\
pfoot_can: Kanonizität des phonologischen Fußes\
pfoot_border: Grenze des phonologischen Fußes nach dem Buchstaben\
pfoot_count: Anzahl der phonologischen Füße im Wort\
pfoot_index: Position des Fußes im Wort

#### Graphematik (Graphotaktik)

graph_complexity_2: siehe graph_complexity, aber hier sind beide
Bestandteile des komplexen Graphems annotiert\
doppelt: Bestandteil eines Doppelkonsonanten\
doppelt_index: erster oder zweiter Bestandteil des Doppelkonsonanten\
graph_complexity: gehört der Graph zu einem komplexen Graphem?

#### Graphematik (Silbe)

gsyll_count: Anzahl der graphematischen Silben im Wort\
gsyll_index: Position der graphematischen Silbe im Wort\
gsyll_struc: Position innerhalb der graphematischen Silbe (Onset, Kern,
Key, Coda, extrasilbisch)\
gsyll_border: graphematische Silbengrenze nach dem Buchstaben\
gsyll_type: Typ der phonologischen Grenze (prominent, nicht prominent,
reduziert)

##### Graphematik (Fuß)

gfoot: graphematischer Fuß (Trochäus, Daktylus, degeneriert,
extrametrisch)\
gfoot_can: Kanonizität des graphematischen Fußes\
gfoot_border: Grenze des graphematischen Fußes nach dem Buchstaben\
gfoot_count: Anzahl der graphematischen Füße im Wort\
gfoot_index: Position des Fußes im Wort\
gsyll_type_2: Recodierung unter der Annahme, dass es nur graphematische
Trochäen gibt\
gfoot_2: Recodierung unter der Annahme, dass es nur graphematische
Trochäen gibt\
gfoot_can_2: Recodierung unter der Annahme, dass es nur graphematische
Trochäen gibt\
gfoot_border_2: Recodierung unter der Annahme, dass es nur
graphematische Trochäen gibt\
gfoot_count_2 Recodierung unter der Annahme, dass es nur graphematische
Trochäen gibt\
gfoot_index_2: Recodierung unter der Annahme, dass es nur graphematische
Trochäen gibt

#### Graphematik (Graphemfunktionen)

h_func: Funktion des h (Dehnungs-h, silbeninitiales h, phonographisches
h, etymologisches h, graphisches h)\
e_func: Funktions des e (Reduktionsvokal, Vollvokal, Diphthong, Dehnung,
etymologisch, Umlautschreibung)\
morph_belastet: Ist der Buchstabe durch eine morphologische Schreibung
zu erklären?

#### Graphetik

letter_form: Kontrollspalte, ob letter form schon erfasst wurde\
kopf_exist: Hat die Form einen Kopf?\
koda1_exist: Hat die Form eine Koda?\
koda2:exist: Hat die Form eine zweite Koda?\
kopf_form: Form des Kopfs\
koda1_form: Form der ersten Koda\
koda2_form: Form der zweiten Koda\
close: Geschlossenheit\
head_free: Kopffreiheit\
contacts: Berührungspunkte der Kodas mit dem Kopf\
code: Kurzbezeichnung für die Buchstabenform (VERALTET)\
**code_neu: Kurzbezeichnung für die Buchstabenform nach einigen
Anpassungen**

#### Intra-Rater-Reliabilität

drin: Kontrollspalte, ob der für die Intra-Rater-Reliabilität
mitberechnet werden soll\
Zufallszahl: Hilfsspalte für die Auswahl der Subsets zum Prüfen der
Intra-Rater-Reliabilität\
zufall_drin: Ist der Fall im Subset für die Intra-Rater-Reliabilität?\
code_val: Buchstabenform, die bei der Validierung der
Intra-Rater-Reliabilität annotiert wurde\
ungleich: stimmt das Rating zu beiden Zeitpunkten überein?
