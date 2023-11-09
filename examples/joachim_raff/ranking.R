## Dieses Skript beantwortet die Frage, welchen Rang Joachim
## Raffs Werke unter den Klavierwerken einnimmt, die zwischen
## 1853 und 1882 bei C. F. Peters bzw. Rieter-Biedermann
## erschienen sind.

source('../../mpdbapi/mpdbapi.R')

## Rang RB-Klavierwerke

ranking_query <- function(publisher) {
    #Abfrage
    query <- publisher %>% paste('_*', sep='')
    rb_items <- mpdbapi_search(q=query, fields='mvdb_id')

    # Auffächern der Druck- und Werkdaten
    rb_items %>% unnest(published_subitems, names_sep='.') %>%
        unnest(published_subitems.prints, names_sep='.') %>%
        unnest(works, names_sep='.') %>%
        unnest(works.gnd_instruments, names_sep='.') %>%
        unnest(works.composers, names_sep='.') ->
            rb_data

    # Ausfiltern der Klavierwerke zwischen 1853 und 1882
    rb_data %>%
        filter(works.gnd_instruments.name != 'Klavier') %>%
        select(works.gnd_id) %>%
        unique ->
            non_piano_works

    rb_data %>%
        select(mvdb_id, works.gnd_id) %>%
        unique %>%
        group_by(mvdb_id) %>%
        summarise(n = n()) %>%
        filter(n==1) ->
            one_work_items

    rb_data %>%
        # alle Werke, in denen ein Klavier vorkommt
        filter(works.gnd_instruments.name == 'Klavier') %>%
        # kein Werk, in dem etwas anderes als ein Klavier vorkommt
        filter(!(works.gnd_id %in% non_piano_works$works.gnd_id)) %>%
        # nur Verlagsartikel, die exakt ein Werk verlegen
        filter(mvdb_id %in% one_work_items$mvdb_id) %>%
        # nur Druckdaten bis 1882
        filter(published_subitems.prints.date_of_action <= '1882-12-31') %>%
        # nur Druckdaten ab 1853
        filter(published_subitems.prints.date_of_action >= '1853-01-01') ->
            target_data

    # Ermitteln der Rangfolge
    target_data %>%
        select(
            Komponist = works.composers.name,
            GND_ID_Komponist = works.composers.gnd_id,
            quantity = published_subitems.prints.quantity,
            uid = published_subitems.prints.uid
            ) %>%
        unique %>%
        group_by(Komponist, GND_ID_Komponist) %>%
        summarise(Total = sum(quantity)) %>%
        arrange(desc(Total))
}

ranking_query('RB') %>% write_csv('csv/rb_ranking.csv')
ranking_query('PE') %>% write_csv('csv/pe_ranking.csv')
