---
title: "Analisis de Sentimientos Noticieros Chilenos"
author: "crsd"
date: "16 de julio de 2019"
output: 
      html_document:
        code_folding: hide
        css: air.css
        highlight: tango
        keep_md: yes
        theme: united
        toc: yes
        toc_float: yes
---

# Resumen

Twitter es actualmente una dinámica fuente de contenidos que, dada su popularidad e impacto, se ha convertido en uno de los principales medios de difusión de los principales medios de comunicación tradicionales (radio y televisión).
A continuación se realizará un análisis de sentimientos de los tweets de noticieros chilenos para así concluir si hay algún patrón entre los sentimientos evocados por los tweets de los noticieros. 
Para ello, se analizan las publicaciones que han hecho en Twitter los noticieros de **@CNNChile**, **@ahoranoticiasAN**, **@24HorasTVN**, **@T13** y **@CHVNoticias**.

Los puntos tratados son:

- Automatización de la extracción de datos de Twitter.

- Análisis de las palabras empleadas por cada uno de los usuarios.

- Análisis de sentimientos.


# Extraccion datos Twitter

Como ocurre en muchas redes sociales, la propia plataforma cuenta con una API que permite extraer información. Para comunicarnos con la API de Tweeter utilizaremos la librería ```rtweet```.

Para poder extraer datos de Twitter es necesario estar registrado en la plataforma y, a partir de la cuenta, crear una Twitter App asociada. Twitter App es el mecanismo que proporciona Twitter para desarrolladores que quieran acceder a los contenidos de Twitter a través de programas. Al crear una Twitter App, Twitter proporciona una serie de claves y tokens de identificación que permiten acceder a la aplicación y extraer información. Para mayor detalles revisar https://apps.twitter.com/app/new.

Twitter tiene una normativa que regula la frecuencia máxima de peticiones, así como la cantidad máxima de tweets que se pueden extraer rate limiting. Es importante cumplir estos lmites para evitar ser bloqueado.

Para el trabajo de análisis que se quiere realizar, es conveniente recuperar tantos tweets como sea posible, para esto se sigue la siguiente estrategia:

Todo tweet tiene un ID global numérico que sigue un orden temporal, lo que permite identificar si un tweet es más reciente que otro.

Entre los argumentos de ```api.GetUserTimeline()``` se puede especificar el ```max_id``` para recuperar únicamente tweets más antiguos.

Antes de cada consulta, se lee el fichero donde se están almacenando los tweets y se identifica el ID del úlltimo tweet recuperado. Si no existe fichero de almacenamiento para el usuario en cuestión, se crea uno.

Se realiza una nueva consulta empleando como argumento ```max_id``` el ID recuperado en el paso anterior. Esta nueva consulta se realiza tras un tiempo determinado por ```rate_limit()``` que entrega los limites de extracción de la conexión actual.

Luego, se incorporan los nuevos datos al archivo de almacenamiento.



```r
suppressMessages(library(httpuv)) # Proporciona soporte para manejar solicitudes HTTP desde R
suppressMessages(library(tidyverse)) # análisis y manipulación de datos
suppressMessages(library(rtweet)) # conecta con API de Twitter
suppressMessages(library(knitr)) # conecta con API de Twitter
```
     

```r
# Identificación y obtención de tokens

# appname <- "app-------me"
# key     <- "l9--------------------sLH"
# secret  <- "7Cr--------------------------------------------cGk"
# twitter_token <- create_token(app = appname, consumer_key = key,
#                               consumer_secret = secret)
```
     

```r
extraccion_tweets <- function(usuario, maxtweets = 100, output_file_name = NULL){
      # Esta función extrae los tweets publicados por un usuario y los almacena en
      # un fichero rds. Si existe un fichero con el mismo nombre, lo lee, concatena
      # los nuevos tweets y lo sobrescribe.
      #
      # Argumentos:
      #   usuario: identificador del usuario de twitter
      #   maxtweets: número de tweets que se recuperan
      #   output_file_name: nombre del fichero de escritura
      
      # Si no se especifica el nombre del archivo de almacenamiento, se crea un
      # nombre por defecto
      if(is.null(output_file_name)){
            output_file_name <- paste0("datos_tweets_", usuario, ".rds")
      }
      # Si no existe el fichero de almacenamiento, se crea uno nuevo con los
      # resultados de la primera recuperación
      if(!(output_file_name %in% list.files())){
            datos_new <- get_timeline(user = usuario, n = maxtweets, 
                                      parse = TRUE, check = TRUE,
                                      include_rts = FALSE)
            saveRDS(object = datos_new, file = output_file_name)
            print(paste0("Nuevo fichero creado: ", output_file_name))
      }else{
            # Se leen los datos antiguos
            datos_old <- readRDS(file = output_file_name)
            # Se identifica el último Id recuperado
            ultimo_id <- tail(datos_old, 1)["status_id"] %>% pull()  %>% as.numeric()
            # Para no recuperar de nuevo el último tweet de la consulta anterior
            # se incrementa en 1 el Id
            ultimo_id = ultimo_id + 1
            # Para que no haya errores de compatibilidad, se convierten todas las
            # columnas numéricas a character
            datos_old <- map_if(.x = datos_old, .p = is.numeric, .f = as.character)
            # Extracción de nuevos tweets
            datos_new <- get_timeline(user = usuario, n = maxtweets,
                                      max_id = ultimo_id, parse = TRUE, 
                                      check = TRUE, include_rts = FALSE)
            datos_new <- map_if(.x = datos_new, .p = is.numeric, .f = as.character)
            # Concatenación de los datos nuevos, viejos y escritura en disco
            datos_concatenados <- bind_rows(datos_old, datos_new)
            saveRDS(object = datos_concatenados, file = output_file_name)
            print(paste("Número total de tweets:", nrow(datos_concatenados)))
      }
}
```
    

```r
# noticieros = c("@CNNChile", "@ahoranoticiasAN", "@24HorasTVN", 
#                "@T13", "@CHVNoticias")
# n <- 3 # número de iteraciones, ajustar según se requiera.
# mxt <- 3200
# for (k in 1:n) {
#       print(paste0("ciclo: ", k))
#       # extrae maximo 3200 tweets cada 15 minutos (aprox.)
#       for (j in 1:length(noticieros)) {
#             extraccion_tweets(usuario  = noticieros[j], maxtweets  = mxt)
#       }
#       # crea una pausa hasta que se reinicie el permiso de tweeter
#       if(k<n){
#             (60*rate_limit(query = "get_timeline")$reset[[1]]) %>% 
#                   round() %>% 
#                   Sys.sleep()
#             }
# }
```


# Carga de datos

Los datos utilizados en este análisis se han obtenido mediante la función definida en el apartado anterior. Los ficheros *.rds* pueden encontrarse en mi repositorio de github.


```r
#Carga de paquetes
suppressMessages(library(syuzhet))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(purrr))
suppressMessages(library(tidyr))
suppressMessages(library(lubridate))
suppressMessages(library(tm))
suppressMessages(library(wordcloud))
suppressMessages(library(RColorBrewer))
suppressMessages(library(gridExtra))
suppressMessages(library(scales))
suppressMessages(library(corrplot))
suppressMessages(library(caret))
suppressMessages(library(dplyr))
```

A continuación se puede apreciar una vista resumida de los datos recolectados.


```r
tweets <- rbind(readRDS("datos_tweets_@CNNChile.rds"), 
                readRDS("datos_tweets_@ahoranoticiasAN.rds"),
                readRDS("datos_tweets_@24HorasTVN.rds"),
                readRDS("datos_tweets_@T13.rds"),
                readRDS("datos_tweets_@CHVNoticias.rds"))
tweets %>% group_by(screen_name) %>% summarise(numero_tweets = n()) 
```

```
## # A tibble: 5 x 2
##   screen_name     numero_tweets
##   <chr>                   <int>
## 1 24HorasTVN               2836
## 2 ahoranoticiasAN          3164
## 3 CHVNoticias              2983
## 4 CNNChile                 3102
## 5 T13                      3124
```

```r
head(tweets)
```

```
## # A tibble: 6 x 90
##   user_id status_id created_at          screen_name text  source
##   <chr>   <chr>     <dttm>              <chr>       <chr> <chr> 
## 1 182486~ 11403332~ 2019-06-16 19:00:01 CNNChile    “Han~ Tweet~
## 2 182486~ 11403181~ 2019-06-16 18:00:01 CNNChile    Lanz~ Tweet~
## 3 182486~ 11403030~ 2019-06-16 17:00:01 CNNChile    La m~ Tweet~
## 4 182486~ 11402916~ 2019-06-16 16:15:00 CNNChile    Todo~ Tweet~
## 5 182486~ 11402879~ 2019-06-16 16:00:01 CNNChile    Más ~ Tweet~
## 6 182486~ 11402765~ 2019-06-16 15:15:00 CNNChile    "Est~ Tweet~
## # ... with 84 more variables: display_text_width <chr>,
## #   reply_to_status_id <chr>, reply_to_user_id <chr>,
## #   reply_to_screen_name <chr>, is_quote <lgl>, is_retweet <lgl>,
## #   favorite_count <chr>, retweet_count <chr>, quote_count <chr>,
## #   reply_count <chr>, hashtags <list>, symbols <list>, urls_url <list>,
## #   urls_t.co <list>, urls_expanded_url <list>, media_url <list>,
## #   media_t.co <list>, media_expanded_url <list>, media_type <list>,
## #   ext_media_url <list>, ext_media_t.co <list>,
## #   ext_media_expanded_url <list>, ext_media_type <chr>,
## #   mentions_user_id <list>, mentions_screen_name <list>, lang <chr>,
## #   quoted_status_id <chr>, quoted_text <chr>, quoted_created_at <dttm>,
## #   quoted_source <chr>, quoted_favorite_count <chr>,
## #   quoted_retweet_count <chr>, quoted_user_id <chr>,
## #   quoted_screen_name <chr>, quoted_name <chr>,
## #   quoted_followers_count <chr>, quoted_friends_count <chr>,
## #   quoted_statuses_count <chr>, quoted_location <chr>,
## #   quoted_description <chr>, quoted_verified <lgl>,
## #   retweet_status_id <chr>, retweet_text <chr>,
## #   retweet_created_at <dttm>, retweet_source <chr>,
## #   retweet_favorite_count <chr>, retweet_retweet_count <chr>,
## #   retweet_user_id <chr>, retweet_screen_name <chr>, retweet_name <chr>,
## #   retweet_followers_count <chr>, retweet_friends_count <chr>,
## #   retweet_statuses_count <chr>, retweet_location <chr>,
## #   retweet_description <chr>, retweet_verified <lgl>, place_url <chr>,
## #   place_name <chr>, place_full_name <chr>, place_type <chr>,
## #   country <chr>, country_code <chr>, geo_coords <list>,
## #   coords_coords <list>, bbox_coords <list>, status_url <chr>,
## #   name <chr>, location <chr>, description <chr>, url <chr>,
## #   protected <lgl>, followers_count <chr>, friends_count <chr>,
## #   listed_count <chr>, statuses_count <chr>, favourites_count <chr>,
## #   account_created_at <dttm>, verified <lgl>, profile_url <chr>,
## #   profile_expanded_url <chr>, account_lang <lgl>,
## #   profile_banner_url <chr>, profile_background_url <chr>,
## #   profile_image_url <chr>
```

```r
names(tweets)
```

```
##  [1] "user_id"                 "status_id"              
##  [3] "created_at"              "screen_name"            
##  [5] "text"                    "source"                 
##  [7] "display_text_width"      "reply_to_status_id"     
##  [9] "reply_to_user_id"        "reply_to_screen_name"   
## [11] "is_quote"                "is_retweet"             
## [13] "favorite_count"          "retweet_count"          
## [15] "quote_count"             "reply_count"            
## [17] "hashtags"                "symbols"                
## [19] "urls_url"                "urls_t.co"              
## [21] "urls_expanded_url"       "media_url"              
## [23] "media_t.co"              "media_expanded_url"     
## [25] "media_type"              "ext_media_url"          
## [27] "ext_media_t.co"          "ext_media_expanded_url" 
## [29] "ext_media_type"          "mentions_user_id"       
## [31] "mentions_screen_name"    "lang"                   
## [33] "quoted_status_id"        "quoted_text"            
## [35] "quoted_created_at"       "quoted_source"          
## [37] "quoted_favorite_count"   "quoted_retweet_count"   
## [39] "quoted_user_id"          "quoted_screen_name"     
## [41] "quoted_name"             "quoted_followers_count" 
## [43] "quoted_friends_count"    "quoted_statuses_count"  
## [45] "quoted_location"         "quoted_description"     
## [47] "quoted_verified"         "retweet_status_id"      
## [49] "retweet_text"            "retweet_created_at"     
## [51] "retweet_source"          "retweet_favorite_count" 
## [53] "retweet_retweet_count"   "retweet_user_id"        
## [55] "retweet_screen_name"     "retweet_name"           
## [57] "retweet_followers_count" "retweet_friends_count"  
## [59] "retweet_statuses_count"  "retweet_location"       
## [61] "retweet_description"     "retweet_verified"       
## [63] "place_url"               "place_name"             
## [65] "place_full_name"         "place_type"             
## [67] "country"                 "country_code"           
## [69] "geo_coords"              "coords_coords"          
## [71] "bbox_coords"             "status_url"             
## [73] "name"                    "location"               
## [75] "description"             "url"                    
## [77] "protected"               "followers_count"        
## [79] "friends_count"           "listed_count"           
## [81] "statuses_count"          "favourites_count"       
## [83] "account_created_at"      "verified"               
## [85] "profile_url"             "profile_expanded_url"   
## [87] "account_lang"            "profile_banner_url"     
## [89] "profile_background_url"  "profile_image_url"
```

```r
# Selección de variables
tweets <- tweets %>% select(screen_name, created_at, status_id, is_retweet,
                            favorite_count, retweet_count, text)
# Se renombran las variables con nombres más prácticos
tweets <- tweets %>% 
      rename(autor = screen_name, fecha = created_at, texto = text,
             tweet_id = status_id, is_retweet = is_retweet,
             cont_fav = favorite_count, cont_rt = retweet_count)
head(tweets)
```

```
## # A tibble: 6 x 7
##   autor  fecha               tweet_id is_retweet cont_fav cont_rt texto    
##   <chr>  <dttm>              <chr>    <lgl>      <chr>    <chr>   <chr>    
## 1 CNNCh~ 2019-06-16 19:00:01 1140333~ FALSE      9        3       “Han def~
## 2 CNNCh~ 2019-06-16 18:00:01 1140318~ FALSE      13       6       Lanzan t~
## 3 CNNCh~ 2019-06-16 17:00:01 1140303~ FALSE      7        5       La miste~
## 4 CNNCh~ 2019-06-16 16:15:00 1140291~ FALSE      2        1       Todo en ~
## 5 CNNCh~ 2019-06-16 16:00:01 1140287~ FALSE      3        11      Más de u~
## 6 CNNCh~ 2019-06-16 15:15:00 1140276~ FALSE      11       4       "Este do~
```

# Limpieza de texto y tokenización


El proceso de limpieza de texto consiste en eliminar del texto todo aquello que no aporte información sobre su temática, estructura o contenido. No existe una única forma de hacerlo, depende en gran medida de la finalidad del análisis y de la fuente de la que proceda el texto. En este análisis, dado que los principales objetivos son estudiar el perfil de los noticieros y analizar el sentimiento que transmiten, se procede a eliminar:

- Patrones no informativos (urls de páginas web)

- Signos de puntuación

- Etiquetas HTML

- Caracteres sueltos

- Números

- Hashtags y usuarios

- Texto en mayúscula.

- Stop words (palabras no relevantes)

Luego tokenizamos el texto, esto consiste en dividir el texto en las unidades que lo conforman, en este caso, las palabras.

Al realizar la tokenización, se han creado varios elementos en una observación, esto es un tweet tiene varias palabras tokenizadas. Para poder trabajar de manera ordenada es necesario realizar una transformación dejando a cada una de estas palabras como observación individual.



```r
cleanTweet <- function(text, users = TRUE, hashtags = TRUE,
                       token = TRUE, stopword = TRUE){
      # Variables.
      # ----------
      # text:     texto limpiar. Sí lo acepta formato caracter.
      # users:    deja o elimina usuarios de twitter (de todas 
      #           formas borra los @).
      # hashtags: deja o elimina hashtags de twitter 
      #           (de todas formas borra los #).
      # token:    tokeniza el resultado.
      # stopword: Elimina las stop words
      
      # Quitando los links en los tweets
      tweets <- gsub("http.*","",text)
      tweets <- gsub("https.*","",tweets)
      # Quitando los hashtags y usuarios en los tweets
      if(!hashtags){tweets <- gsub("#\\w+","",tweets)}
      if(!users){tweets <- gsub("@\\w+","",tweets)}
      # Quitando los signos de puntuación, números y textos con números
      tweets <- gsub("[[:punct:]]","",tweets)
      tweets <- gsub("\\w*[0-9]+\\w*\\s*", "",tweets)
      tweets <- stringr::str_replace_all(tweets, "\\p{quotation mark}", "")
      tweets <- gsub("\\n", " ",tweets)
      tweets <- stringr::str_replace_all(tweets,"[\\s]+", " ")
      tweets <- stringr::str_replace_all(tweets," $", "")
      #tweets <- chartr('??????','aeioun',tweets) # Problemas con tildes
      # Elimina Emojis
      tweets <- iconv(tweets, from = "UTF-8", to = "latin1", sub = "byte")
      tweets <- gsub("<\\w+>","",tweets)
      # Transforma todo a minuscula
      tweets <- tolower(tweets)
      if(stopword){
            tweets <- tm::removeWords(tweets, 
                                      iconv(tm::stopwords("spanish"), 
                                            from = "", 
                                            to = "UTF-8",
                                            sub = "byte"))
      }
      if(token){
            # Tokenización por palabras individuales
            tweets <- stringr::str_split(tweets, " ")[[1]]
            # Eliminación de tokens con una longitud < 2
            tweets <- purrr::keep(.x = tweets,
                                  .p = function(x){stringr::str_length(x) > 1})
      }
      return(tweets)
}
```



```r
# limpieza de tweets
cleanTweets <- tweets %>% mutate(texto_tokenizado = map(.x = texto,
                                                   .f = cleanTweet))
# ordenando datos limpios
tweets_tidy <- cleanTweets %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy)
```

```
## # A tibble: 6 x 7
##   autor  fecha               tweet_id    is_retweet cont_fav cont_rt token 
##   <chr>  <dttm>              <chr>       <lgl>      <chr>    <chr>   <chr> 
## 1 CNNCh~ 2019-06-16 19:00:01 1140333221~ FALSE      9        3       defin~
## 2 CNNCh~ 2019-06-16 19:00:01 1140333221~ FALSE      9        3       movim~
## 3 CNNCh~ 2019-06-16 19:00:01 1140333221~ FALSE      9        3       lgbtq 
## 4 CNNCh~ 2019-06-16 19:00:01 1140333221~ FALSE      9        3       billb~
## 5 CNNCh~ 2019-06-16 19:00:01 1140333221~ FALSE      9        3       desta~
## 6 CNNCh~ 2019-06-16 19:00:01 1140333221~ FALSE      9        3       javie~
```


# Análisis exploratorio

## Distribución temporal de los tweets

La función ```get_timeline()``` extrae los últimos tweets (máximo 3200), por lo que su distribución temporal puede no ser igual para cada usuario.


```r
# gráfico número de tweets por autor
ggplot(tweets, aes(x = as.Date(fecha), fill = autor)) +
      geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
      scale_x_date(date_labels = "%d-%m", date_breaks = "1 day") +
      labs(x = "fecha de publicación", y = "número de tweets") +
      facet_wrap(~ autor, ncol = 1) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90))
```

![](Analisis_Sentimiento_files/figure-html/DistTemp-1.png)<!-- -->

```r
# gráfico número de tweets
tweets_mes_anyo <- tweets %>% mutate(mes_anyo = format(fecha, "%Y-%m-%d"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
      ggplot(aes(x = mes_anyo, y = n, color = autor)) +
      geom_line(aes(group = autor)) +
      labs(title = "Número de tweets publicados", x = "fecha de publicación",
           y = "número de tweets") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 6),
            legend.position = "bottom")
```

![](Analisis_Sentimiento_files/figure-html/DistTemp-2.png)<!-- -->

Puede observarse que lo usuarios **@CHVNoticias** y **@CNNChile** presentan una menor cantidad de tweets diarios en el periodo de tiempo eindicado en la gráfica, esto hace que se extrajeran tweets más antiguos que para los usuarios **@24HorasTVN**, **@ahoranoticiasAN** y **@T13**.

## Frecuencia de palabras

A la hora de entender que caracteriza los mensajes de cada noticiero, es interesante estudiar qué palabras emplea, con qué frecuencia, así como el significado de las mismas. 

### Total palabras utilizadas


```r
tweets_tidy %>%  ggplot(aes(x = autor, fill = autor)) + geom_bar() +
      coord_flip() + 
      theme_bw() + labs(title = "Número de palabras por usuario")
```

![](Analisis_Sentimiento_files/figure-html/FreqWord-1.png)<!-- -->

### Palabras distintas utilizadas


```r
tweets_tidy %>% select(autor, token) %>% distinct() %>%
      ggplot(aes(x = autor, fill = autor)) + geom_bar() + coord_flip() + 
      theme_bw() + labs(title = "Número de palabras distintas por usuario")
```

![](Analisis_Sentimiento_files/figure-html/DistinctWord-1.png)<!-- -->

Las cantidd de palabras utilizadas por los noticieros varía desde 26.000 a 35.000 aproximadamente, siendo **@CNNChile** la que más utiliza. Del mismo modo, los noticieros utilizan palabras distintas en un rango de 6.500 a 8.200 palabras, siendo nuevamente **@CNNChile** quien lleva la delantera.

### Longitud media de los tweets por usuario


```r
tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%   
      group_by(autor) %>%
      summarise(media_longitud = mean(longitud),
                sd_longitud = sd(longitud)) %>%
      ggplot(aes(x = autor, y = media_longitud, fill = autor)) +
      geom_col() +
      geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                        ymax = media_longitud + sd_longitud)) +
      coord_flip() + theme_bw() + 
      labs(title = "Longitud media de los tweets por usuario")
```

![](Analisis_Sentimiento_files/figure-html/LongtWord-1.png)<!-- -->

De los datos se observa que el mínimo de palabras utilizadas por todos los noticieros es de entre 6 y 7 palabras.

El noticiero que más alterna entre tweets cortos y largos en **@CNNChile**, que indica, junto a la cantidad diferente de palabras, una mayor diversidad en los mensajes.

### Palabras más utilizadas


```r
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
      top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=10)
```

```
## # A tibble: 50 x 3
## # Groups:   autor [5]
##    autor      token      n
##    <chr>      <chr>  <int>
##  1 24HorasTVN sigue    542
##  2 24HorasTVN vivo     500
##  3 24HorasTVN señal    461
##  4 24HorasTVN envivo   208
##  5 24HorasTVN minuto   193
##  6 24HorasTVN chile    175
##  7 24HorasTVN copa     164
##  8 24HorasTVN más      151
##  9 24HorasTVN tras     146
## 10 24HorasTVN roja     142
## # ... with 40 more rows
```

```r
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
      top_n(10, n) %>% arrange(autor, desc(n)) %>%
      ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
      geom_col() +
      theme() +
      labs(y = "", x = "") +
      theme(legend.position = "none") +
      coord_flip() +
      facet_wrap(~autor,scales = "free", ncol = 2, drop = TRUE)
```

![](Analisis_Sentimiento_files/figure-html/UseWord-1.png)<!-- -->

Los resultados obtenidos tienen sentido si ponemos en contexto la actividad analizada.

En general se centran en palabras similares, siendo *vivo*, *video*, *señal* y *chile* las palabras más repetidas en los 5 noticieros, de lo que se puede inferir que los temas más tratados son noticias sobre actualidad chilena, que es coherente con la actividad analizada.


## Correlación entre usuarios por palabras utilizadas

Una forma de cuantificar la similitud entre los perfiles de dos usuarios de Twitter es calculando la correlación en el uso de palabras. La idea es que, si dos usuarios escriben de forma similar, tenderán a utilizar las mismas palabras y con frecuencias similares.



```r
tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
      spread(key = autor, value = n, fill = NA, drop = TRUE)
tweets_spread[is.na(tweets_spread)] <- 0

names(tweets_spread) <- c("token", "TVN24Horas", "ahoranoticiasAN", 
                          "CHVNoticias", "CNNChile", "T13")
method <- "pearson"
m_cor <- matrix(nrow = 5, ncol = 5)
for (i in 1:dim(m_cor)[1]) {
      for (j in 1:dim(m_cor)[2]) {
            form <- as.formula(paste("~", names(tweets_spread)[i+1], 
                                      "+", names(tweets_spread)[j+1]))
            if(i!=j){
                  m_cor[i,j] <- cor.test(form, method = method, 
                                   data = tweets_spread)$estimate
            }
            if(i==j){m_cor[i,j] <- 1}
      }
}
colnames(m_cor) <- names(tweets_spread)[2:6]
rownames(m_cor) <- names(tweets_spread)[2:6]
corrplot(m_cor, method="color", type="upper", order="hclust", 
         addCoef.col = "black", tl.col="black", tl.srt=45,
         sig.level = 0.01, insig = "blank", diag=FALSE)
```

![](Analisis_Sentimiento_files/figure-html/corrWord-1.png)<!-- -->



```r
p1 <- list()
k <- 1
for (i in 2:dim(tweets_spread)[2]) {
      for (j in 2:dim(tweets_spread)[2]) {
            if(j>i){
                  tweets_spread2 <- tweets_spread[,c(1,i,j)]
                  names(tweets_spread2) <- c("token", "usuario1", "usuario2")

                  p1[[k]] <- ggplot(tweets_spread2, aes(usuario1, usuario2)) +
                        geom_jitter(alpha = 0.1, size = 2.5,
                                    width = 0.25, height =0.25) +
                        geom_text(aes(label = token),
                                  check_overlap = TRUE, vjust = 1.5) +
                        scale_x_log10(labels = percent_format(),
                                      name = names(tweets_spread)[i]) +
                        scale_y_log10(labels = percent_format(),
                                      name = names(tweets_spread)[j]) +
                        geom_abline(color = "red") +
                        theme(axis.text.x = element_blank(),
                              axis.text.y = element_blank())
                  k <- k + 1
            }
      }
}
grid.arrange(p1[[1]], p1[[2]], nrow = 1)
```

![](Analisis_Sentimiento_files/figure-html/plotcorrWord-1.png)<!-- -->

```r
grid.arrange(p1[[3]], p1[[4]], nrow = 1)
```

![](Analisis_Sentimiento_files/figure-html/plotcorrWord-2.png)<!-- -->

```r
grid.arrange(p1[[5]], p1[[6]], nrow = 1)
```

![](Analisis_Sentimiento_files/figure-html/plotcorrWord-3.png)<!-- -->

```r
grid.arrange(p1[[7]], p1[[8]], nrow = 1)
```

![](Analisis_Sentimiento_files/figure-html/plotcorrWord-4.png)<!-- -->

```r
grid.arrange(p1[[9]], p1[[10]], nrow = 1)
```

![](Analisis_Sentimiento_files/figure-html/plotcorrWord-5.png)<!-- -->

Para poder valorar adecuadamente el nivel de correlación es interesante conocer el número de palabras comunes entre cada par de autores.


```r
k <- 1
users <- unique(tweets_tidy$autor)
comWord <- character()
for (i in 1:length(users)) {
      for (j in 1:length(users)) {
            if(j>i){
                  nwc <- c(users[i], users[j])
                  wc <- dplyr::intersect(tweets_tidy %>% 
                                               filter(autor==users[i]) %>%
                                               select(token), tweets_tidy %>%
                                               filter(autor==users[j]) %>%
                                               select(token)) %>% nrow()
                  comWord[k] <- paste("Palabras comunes entre", 
                                    users[i], "y", users[j], ":", wc)
                  k <- k + 1

            }
      }
}
comWord
```

```
##  [1] "Palabras comunes entre CNNChile y ahoranoticiasAN : 3769"   
##  [2] "Palabras comunes entre CNNChile y 24HorasTVN : 3487"        
##  [3] "Palabras comunes entre CNNChile y T13 : 3418"               
##  [4] "Palabras comunes entre CNNChile y CHVNoticias : 3936"       
##  [5] "Palabras comunes entre ahoranoticiasAN y 24HorasTVN : 3719" 
##  [6] "Palabras comunes entre ahoranoticiasAN y T13 : 3686"        
##  [7] "Palabras comunes entre ahoranoticiasAN y CHVNoticias : 3810"
##  [8] "Palabras comunes entre 24HorasTVN y T13 : 3369"             
##  [9] "Palabras comunes entre 24HorasTVN y CHVNoticias : 3454"     
## [10] "Palabras comunes entre T13 y CHVNoticias : 3417"
```

El número de palabras comunes entre cada par de noticieros es similar y representa alrededor del 12% del total de palabras de cada noticiero, por lo que las relaciones entre noticieros se pueden determinar con el porcentaje restante.

Observando el resultado de la correlación mostrados anteriormente se deduce que existen 2 grupos con similitudes:

- **@T13**, **@24HorasTVN** y **@ahoranoticiasAN**.

- **@CHVNoticias** y **@CNNChile**.


## Comparación en el uso de palabras

A continuación, se estudia que palabras se utilizan de forma más diferenciada por cada usuario, es decir, palabras que utiliza mucho un autor y que no utiliza el otro. Una forma de hacer este análisis es mediante el *log of odds ratio* de las frecuencias (esta comparación se hace por pares).


$$log\ of\ odds\ ratio =  \log(\frac{[\frac{n_k + 1}{N + 1}]_{user_1}}{[\frac{n_k + 1}{N + 1}]_{user_2}})$$

Siendo $n_k$ el número de veces que aparece el término $k$ en los textos de cada autor y $N$ el número total de términos de cada autor.

Para realizar este cálculo es necesario que, para todos los usuarios, se cuantifique la frecuencia de cada una de las palabras que aparecen en el conjunto de tweets, es decir, si un autor no ha utilizado una de las palabras que sí ha utilizado otro, debe aparecer esa palabra en su registro con frecuencia igual a cero. Existen varias formas de conseguir esto, una de ellas es pivotar y despivotar el dataframe sustituyendo los missing values por cero.



```r
k <- 1
users <- unique(tweets_tidy$autor)
list_logOdds <- list()
p2 <- list()
# Pivotaje y despivotaje
tweets_unpivot <- tweets_tidy %>% group_by(autor, token) %>%
      count(token) %>%
      spread(key = autor, value = n, fill = 0, drop = TRUE) %>% 
      gather(key = "autor", value = "n", -token)
for (i in 1:length(users)) {
      for (j in 1:length(users)) {
            if(j>i){

                  # Selección de los autores
                  tweets_unpivot2 <- tweets_unpivot %>% 
                        filter(autor %in% c(users[i], users[j]))
                  # Se añade el total de palabras de cada autor
                  tweets_unpivot2 <- tweets_unpivot2 %>%
                        left_join(tweets_tidy %>% group_by(autor) %>%
                                        summarise(N = n()), by = "autor")
                  # Cálculo de odds y log of odds de cada palabra
                  tweets_logOdds <- tweets_unpivot2 %>% 
                        mutate(odds = (n + 1) / (N + 1)) %>%
                        select(autor, token, odds) %>% 
                        spread(key = autor, value = odds)
                  tweets_logOdds[,4] <- log(tweets_logOdds[,2]/tweets_logOdds[,3])
                  names(tweets_logOdds)[4] <- "log_odds"
                  tweets_logOdds[,5] <- abs(tweets_logOdds$log_odds)
                  names(tweets_logOdds)[5] <- "abs_log_odds"
                  tweets_logOdds <- tweets_logOdds %>%
                        mutate(autor_frecuente = if_else(log_odds > 0,
                                                         names(tweets_logOdds)[2],
                                                         names(tweets_logOdds)[3]))
                  list_logOdds[[k]] <- tweets_logOdds
                  p2[[k]] <- tweets_logOdds %>% group_by(autor_frecuente) %>% 
                        top_n(15, abs_log_odds) %>%
                        ggplot(aes(x = reorder(token, log_odds),
                                   y = log_odds, fill = autor_frecuente)) +
                        geom_col() +
                        labs(x = "palabra",
                             y = paste0("log odds ratio (", users[i], " / ",
                                        users[j],")")) + coord_flip() + 
                        theme()
                  k <- k + 1
            }
      }
}
head(list_logOdds[[1]])
```

```
## # A tibble: 6 x 6
## # Groups:   token [6]
##   token      ahoranoticiasAN CNNChile log_odds abs_log_odds autor_frecuente
##   <chr>                <dbl>    <dbl>    <dbl>        <dbl> <chr>          
## 1 aa               0.0000347  2.82e-5    0.205        0.205 ahoranoticiasAN
## 2 abajo            0.0000347  8.47e-5   -0.894        0.894 CNNChile       
## 3 abalanzó         0.0000693  2.82e-5    0.898        0.898 ahoranoticiasAN
## 4 abanderada       0.0000347  5.65e-5   -0.489        0.489 CNNChile       
## 5 abandona         0.000104   2.82e-5    1.30         1.30  ahoranoticiasAN
## 6 abandonada       0.0000693  2.82e-5    0.898        0.898 ahoranoticiasAN
```

Representación de las 30 palabras más diferenciadas.


```r
p2
```

```
## [[1]]
```

![](Analisis_Sentimiento_files/figure-html/plotcompUsoWord-1.png)<!-- -->

```
## 
## [[2]]
```

![](Analisis_Sentimiento_files/figure-html/plotcompUsoWord-2.png)<!-- -->

```
## 
## [[3]]
```

![](Analisis_Sentimiento_files/figure-html/plotcompUsoWord-3.png)<!-- -->

```
## 
## [[4]]
```

![](Analisis_Sentimiento_files/figure-html/plotcompUsoWord-4.png)<!-- -->

```
## 
## [[5]]
```

![](Analisis_Sentimiento_files/figure-html/plotcompUsoWord-5.png)<!-- -->

```
## 
## [[6]]
```

![](Analisis_Sentimiento_files/figure-html/plotcompUsoWord-6.png)<!-- -->

```
## 
## [[7]]
```

![](Analisis_Sentimiento_files/figure-html/plotcompUsoWord-7.png)<!-- -->

```
## 
## [[8]]
```

![](Analisis_Sentimiento_files/figure-html/plotcompUsoWord-8.png)<!-- -->

```
## 
## [[9]]
```

![](Analisis_Sentimiento_files/figure-html/plotcompUsoWord-9.png)<!-- -->

```
## 
## [[10]]
```

![](Analisis_Sentimiento_files/figure-html/plotcompUsoWord-10.png)<!-- -->

Se observa que las palabras más representativas para cada usuario son las referidas a periodistas, integrantes de algún programa o directamente algún programa emitido por su cadena. Estas palabras posiblemente tendrán mucho peso a la hora de clasificar los tweets. 


# Análisis de sentimientos

Para realizar el análisis de sentimientos utilizaremos la clasificación $nrc$, esta clasifica cada palabra en sentimientos *positivos* o *negativos* y las emociones como una o más de las siguientes emociones: *ira*, *anticipación*, *aversión*, *miedo*, *alegria*, *tristeza*, *sorpresa* y *confianza.*

En este análisis se utilizará el valor acumalado de cada sentimiento/emoción de las palabras de cada tweet, por lo que es necesario realizar algunas consideraciones:

- Cuando la suma acumulada de cada emición sea igual a 0 en un tweet, este tweet no será considerado en el análisis.

- Cuando la suma acumulada de sentimientos negativos y positivos sea igual a 0 en un tweet, este tweet no será considerado en el análisis.

- Cuando la suma acumulada de los sentimientos positivos y negativos sean iguales se considerará como sentimiento neutro.



```r
SentTweet <- function(text, onlyResult = FALSE, summary = TRUE){
      # Variables:
      # text: texto a revisar
      # onlyResult: si TRUE, devuelve solo el conteo.
      
      #Transformamos la base de textos importados en un vector para
      #poder utilizar la función get_nrc_sentiment
      
      if(summary){text <- as.vector(text)}
      #Aplicamos la función indicando el vector y el idioma y creamos
      #un nuevo data frame llamado emocion.df
      emocion.df <- get_nrc_sentiment(char_v = text, language = "spanish")
      #Traduce nombre de emociones a español
      colnames(emocion.df)[1] <- "ira"
      colnames(emocion.df)[2] <- "anticipacion"
      colnames(emocion.df)[3] <- "aversion"
      colnames(emocion.df)[4] <- "miedo"
      colnames(emocion.df)[5] <- "alegria"
      colnames(emocion.df)[6] <- "tristeza"
      colnames(emocion.df)[7] <- "sorpresa"
      colnames(emocion.df)[8] <- "confianza"
      colnames(emocion.df)[9] <- "negativo"
      colnames(emocion.df)[10] <- "positivo"
      if(summary){
            #Unimos emocion.df con el vector tweets.df para ver como
            #trabajó la función get_nrc_sentiment cada uno de los tweets
            #   emocion.df2 <- cbind(tweets.df2, emocion.df)
            #Creamos un data frame en el cual las filas serán las emociones
            #y las columnas los puntajes totales
            #Empezamos transponiendo emocion.df
            emocion.df3 <- data.frame(t(emocion.df))
            #Sumamos los puntajes de cada uno de los tweets para cada emoción
            emocion.df3 <- data.frame(rowSums(emocion.df3))
            #Nombramos la columna de puntajes como cuenta
            names(emocion.df3)[1] <- "cuenta"
            #Dado que las emociones son los nombres de las filas y no una variable
            #transformamos el data frame para incluirlas dentro
            emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)
            #Quitamos el nombre de las filas
            rownames(emocion.df3) <- NULL
            #Devuelve el data frame resultante
            if(onlyResult==TRUE){emocion.df4 <- emocion.df3[,2]}
            if(onlyResult==FALSE){emocion.df4 <- emocion.df3}
      }
      if(!summary){
            emocion.df <- data.frame(texto = text, emocion.df, stringsAsFactors = FALSE)
            #Devuelve el data frame resultante
            if(onlyResult==TRUE){emocion.df4 <- emocion.df[,-1]}
            if(onlyResult==FALSE){emocion.df4 <- emocion.df}
            }
      emocion.df4
}
calcSent <- function(x){
      x2 <- x[,8:15]
      y <- x
      # x2 <- x2[rowSums(x2)>0,]
      l=1
      for (k in 1:dim(x)[1]) {
            a <- which.max(x2[k,])
            mat <- x2[k, a]==x2[k,]
            mat <- names(x2[k,])[mat]
            if(sum(x2[k,])==0){
                  y[l,1:dim(x)[2]] <- x[k,]
                  y[l,dim(x)[2]+1] <- ""
                  l <- l + 1
            }
            if(sum(x2[k,])>0){
                  for (i in 1:length(mat)) {
                        y[l,1:dim(x)[2]] <- x[k,]
                        y[l,dim(x)[2]+1] <- mat[i]
                        l <- l + 1
                  }
            }
      }
      names(y)[dim(y)[2]] <- "emocion"
      y
}
```


```r
sent <- SentTweet(tweets_tidy$token, onlyResult = FALSE, summary = FALSE)
```


```r
tweets_sent <- cbind(tweets_tidy, sent[,-1], tipo =
                           ifelse(sent$positivo>sent$negativo,"positivo",
                                  ifelse(sent$positivo<sent$negativo,
                                         "negativo","neutro")))
tweets_sent2 <- calcSent(tweets_sent)
```


```r
tweets_sent$fecha <- as.Date(tweets_sent$fecha)
tweets_sent2$fecha <- as.Date(tweets_sent2$fecha)

# positivo - negativo
# tweets_sent[tweets_sent$tipo!="",] %>%
tweets_sent[sent$positivo!=0|sent$negativo!=0,] %>%
      count(autor, tipo) %>%
      group_by(autor) %>%
      mutate(Proporcion = n / sum(n)) %>%
      ggplot() +
      aes(autor, Proporcion, fill = tipo) +
      geom_col() +
      scale_y_continuous(labels = percent_format()) +
      theme(legend.position = "top")
```

![](Analisis_Sentimiento_files/figure-html/plotSentTweetNot1-1.png)<!-- -->

```r
# tweets_sent[tweets_sent$tipo!="",] %>%
tweets_sent[sent$positivo!=0|sent$negativo!=0,] %>%
      group_by(autor, fecha) %>%
      count(tipo) %>%
      mutate(Proporcion = n / sum(n)) %>%
      ggplot() +
      aes(fecha, Proporcion, fill = tipo) +
      geom_col(width = 1) +
      facet_grid(autor~.) +
      scale_y_continuous(labels = percent_format()) +
      scale_x_date(expand = c(0, 0)) +
      theme(legend.position = "top")
```

![](Analisis_Sentimiento_files/figure-html/plotSentTweetNot1-2.png)<!-- -->

Se observa que todos los usuarios tienen un comportamiento similar respecto a los sentimientos, siendo equilibrado con una leve tendencia a sentimientos *positivos* (55% - 65%). Esto indica que als noticias publicadas por los noticieros están relativamente equilibradas en cuanto al tipo de sentimiento que evocan. Esta tendencia además se observa estable en el tiempo.


```r
# emociones
tweets_sent2[tweets_sent2$emocion!="",] %>%
      count(autor, emocion) %>%
      group_by(autor) %>%
      mutate(Proporcion = n / sum(n)) %>%
      ggplot() +
      aes(autor, Proporcion, fill = emocion) +
      geom_col() +
      scale_y_continuous(labels = percent_format()) +
      theme(legend.position = "top")
```

![](Analisis_Sentimiento_files/figure-html/plotSentTweetNot2-1.png)<!-- -->

```r
tweets_sent2[tweets_sent2$emocion!="",] %>%
      group_by(autor, fecha) %>%
      count(emocion) %>%
      mutate(Proporcion = n / sum(n)) %>%
      ggplot() +
      aes(fecha, Proporcion, fill = emocion) +
      geom_col(width = 1) +
      facet_grid(autor~.) +
      scale_y_continuous(labels = percent_format()) +
      scale_x_date(expand = c(0, 0)) +
      theme(legend.position = "top")
```

![](Analisis_Sentimiento_files/figure-html/plotSentTweetNot2-2.png)<!-- -->

Por otro lado, las emociones predominantes en los tweets son *confianza*, *miedo* y *anticipación*. Además, en menor medida se observa tweets con emoción de *alegría*.
Los tweets presentan un bajo nivel de *sorpresa*, *tristeza*, *ira* y *aversión*.

De lo anterior se puede concluir que los tweets entregan una sensación de certeza, lo que generaría, dependiendo de la temática del tweet, sensaciones de fatalismo y ansiedad o de bienestar y optimismo.


# Código

https://github.com/desareca/Analisis-Sentimientos-Noticieros

---

# Información de sesión


```r
sessionInfo()
```

```
## R version 3.6.0 (2019-04-26)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 17134)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=Spanish_Chile.1252  LC_CTYPE=Spanish_Chile.1252   
## [3] LC_MONETARY=Spanish_Chile.1252 LC_NUMERIC=C                  
## [5] LC_TIME=Spanish_Chile.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] caret_6.0-84       lattice_0.20-38    corrplot_0.84     
##  [4] scales_1.0.0       gridExtra_2.3      wordcloud_2.6     
##  [7] RColorBrewer_1.1-2 tm_0.7-6           NLP_0.2-0         
## [10] lubridate_1.7.4    syuzhet_1.0.4      knitr_1.23        
## [13] rtweet_0.6.9       forcats_0.4.0      stringr_1.4.0     
## [16] dplyr_0.8.1        purrr_0.3.2        readr_1.3.1       
## [19] tidyr_0.8.3        tibble_2.1.3       ggplot2_3.1.1     
## [22] tidyverse_1.2.1    httpuv_1.5.1      
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.4.0         jsonlite_1.6       splines_3.6.0     
##  [4] foreach_1.4.4      prodlim_2018.04.18 modelr_0.1.4      
##  [7] assertthat_0.2.1   stats4_3.6.0       cellranger_1.1.0  
## [10] yaml_2.2.0         slam_0.1-45        ipred_0.9-9       
## [13] pillar_1.4.1       backports_1.1.4    glue_1.3.1        
## [16] digest_0.6.19      promises_1.0.1     rvest_0.3.4       
## [19] colorspace_1.4-1   recipes_0.1.5      htmltools_0.3.6   
## [22] Matrix_1.2-17      plyr_1.8.4         timeDate_3043.102 
## [25] pkgconfig_2.0.2    broom_0.5.2        haven_2.1.0       
## [28] later_0.8.0        gower_0.2.1        lava_1.6.5        
## [31] generics_0.0.2     withr_2.1.2        nnet_7.3-12       
## [34] lazyeval_0.2.2     cli_1.1.0          survival_2.44-1.1 
## [37] magrittr_1.5       crayon_1.3.4       readxl_1.3.1      
## [40] evaluate_0.14      fansi_0.4.0        nlme_3.1-139      
## [43] MASS_7.3-51.4      xml2_1.2.0         class_7.3-15      
## [46] data.table_1.12.2  tools_3.6.0        hms_0.4.2         
## [49] munsell_0.5.0      compiler_3.6.0     rlang_0.3.4       
## [52] grid_3.6.0         iterators_1.0.10   rstudioapi_0.10   
## [55] labeling_0.3       rmarkdown_1.13     ModelMetrics_1.2.2
## [58] gtable_0.3.0       codetools_0.2-16   reshape2_1.4.3    
## [61] R6_2.4.0           zeallot_0.1.0      utf8_1.1.4        
## [64] stringi_1.4.3      parallel_3.6.0     Rcpp_1.0.1        
## [67] vctrs_0.1.0        rpart_4.1-15       tidyselect_0.2.5  
## [70] xfun_0.7
```
