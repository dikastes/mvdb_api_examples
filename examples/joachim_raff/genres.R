## Dieses Skript beantwortet die Frage, wie sich die Genres
## Reverie bzw. Nocturne bei Hofmeister und Rieter-
## Biedermann zwischen 1850 und 1875 entwickelt haben

source('../../mpdbapi/mpdbapi.R')
library(zoo)

plot_genre <- function(genre) {
    genre %>% mpdbapi_search(fi='works.gnd_genres.name') %>%
        unnest(published_subitems, names_sep='.') %>%
        unnest(published_subitems.prints, names_sep='.') %>%
        mutate(
            Verlag = substr(mvdb_id, 0, 2),
            Jahr = year(published_subitems.prints.date_of_action)
            ) %>%
        filter(Verlag != 'PE') %>%
        filter(Verlag != 'PV') %>%
        filter(published_subitems.prints.date_of_action <= '1875-12-31') %>%
        filter(published_subitems.prints.date_of_action >= '1850-01-01') %>%
        group_by(Jahr) %>%
        summarise(Total = sum(published_subitems.prints.quantity)) %>%
        mutate(glMw = rollmean(Total, 3, na.pad=TRUE)) %>%
        ggplot( aes(x = Jahr) ) +
            geom_col( aes(y = Total), color='gray', alpha=.5 ) +
            geom_line( aes(y = glMw), color='darkgreen' )
    paste('graphs/', genre, '.png', sep='') %>% ggsave
}

plot_genre('Notturno')
plot_genre('Reverie')
