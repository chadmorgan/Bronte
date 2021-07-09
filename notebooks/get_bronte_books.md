---
title: "Get Brontë works"
output: 
  html_document:
    keep_md: true
---

Get the major works of the Brontë sisters and pre-process them

I used the following references as guides:

 https://cran.r-project.org/web/packages/gutenbergr/vignettes/intro.html
https://www.tidytextmining.com



```r
library(gutenbergr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidytext)
library(stringr)
library(ggplot2)
```

## Use Gutenbergr package to get Brontë sisters' works



```r
bronte_works <- gutenberg_works(author %in% c("Brontë, Anne","Brontë, Charlotte","Brontë, Emily"))

bronte_works
```

```
## # A tibble: 7 x 8
##   gutenberg_id title author gutenberg_autho… language gutenberg_books… rights
##          <int> <chr> <chr>             <int> <chr>    <chr>            <chr> 
## 1          767 Agne… Bront…              404 en       <NA>             Publi…
## 2          768 Wuth… Bront…              405 en       Gothic Fiction/… Publi…
## 3          969 The … Bront…              404 en       <NA>             Publi…
## 4         1028 The … Bront…              408 en       <NA>             Publi…
## 5         1260 Jane… Bront…              408 en       <NA>             Publi…
## 6         9182 Vill… Bront…              408 en       <NA>             Publi…
## 7        30486 Shir… Bront…              408 en       Historical Fict… Publi…
## # … with 1 more variable: has_text <lgl>
```


```r
books <- gutenberg_download(c(768, 1260,969),
                            meta_fields = c("title","author")
                            )
```

```
## Determining mirror for Project Gutenberg from http://www.gutenberg.org/robot/harvest
```

```
## Using mirror http://aleph.gutenberg.org
```


```r
table(books$title)
```

```
## 
## Jane Eyre: An Autobiography The Tenant of Wildfell Hall 
##                       21001                       18477 
##           Wuthering Heights 
##                       12314
```


```r
head(books,30)
```

```
## # A tibble: 30 x 4
##    gutenberg_id text                title             author       
##           <int> <chr>               <chr>             <chr>        
##  1          768 "Wuthering Heights" Wuthering Heights Brontë, Emily
##  2          768 ""                  Wuthering Heights Brontë, Emily
##  3          768 "by Emily Brontë"   Wuthering Heights Brontë, Emily
##  4          768 ""                  Wuthering Heights Brontë, Emily
##  5          768 ""                  Wuthering Heights Brontë, Emily
##  6          768 ""                  Wuthering Heights Brontë, Emily
##  7          768 ""                  Wuthering Heights Brontë, Emily
##  8          768 "CHAPTER I"         Wuthering Heights Brontë, Emily
##  9          768 ""                  Wuthering Heights Brontë, Emily
## 10          768 ""                  Wuthering Heights Brontë, Emily
## # … with 20 more rows
```



```r
books %>%
  unnest_tokens(sentence, text, token = "lines") %>%
    head()
```

```
## # A tibble: 6 x 4
##   gutenberg_id title        author     sentence                                 
##          <int> <chr>        <chr>      <chr>                                    
## 1          768 Wuthering H… Brontë, E… wuthering heights                        
## 2          768 Wuthering H… Brontë, E… by emily brontë                          
## 3          768 Wuthering H… Brontë, E… chapter i                                
## 4          768 Wuthering H… Brontë, E… 1801—i have just returned from a visit t…
## 5          768 Wuthering H… Brontë, E… neighbour that i shall be troubled with.…
## 6          768 Wuthering H… Brontë, E… country! in all england, i do not believ…
```

We need to do something else to correctly break this up into sentences


```r
bronte_chapters <- books %>%
  group_by(title,author) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  mutate(
        chapter_number = row_number()
    ) %>%
  ungroup() %>%
  filter(chapter_number > 1) %>%
  mutate(
        chapter_number = chapter_number-1
    )
```


```r
bronte_chapters %>% 
    group_by(title) %>%
    summarize(n_chapters=n())
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 3 x 2
##   title                       n_chapters
##   <chr>                            <int>
## 1 Jane Eyre: An Autobiography         38
## 2 The Tenant of Wildfell Hall         53
## 3 Wuthering Heights                   34
```

```r
str(bronte_chapters)
```

```
## tibble [125 × 4] (S3: tbl_df/tbl/data.frame)
##  $ title         : chr [1:125] "Wuthering Heights" "Wuthering Heights" "Wuthering Heights" "Wuthering Heights" ...
##  $ author        : chr [1:125] "Brontë, Emily" "Brontë, Emily" "Brontë, Emily" "Brontë, Emily" ...
##  $ chapter       : chr [1:125] "\n\n\n1801—i have just returned from a visit to my landlord—the solitary\nneighbour that i shall be troubled wi"| __truncated__ "i\n\n\nyesterday afternoon set in misty and cold. i had half a mind to spend\nit by my study fire, instead of w"| __truncated__ "ii\n\n\nwhile leading the way upstairs, she recommended that i should hide the\ncandle, and not make a noise; f"| __truncated__ "v\n\n\nwhat vain weathercocks we are! i, who had determined to hold myself\nindependent of all social intercour"| __truncated__ ...
##  $ chapter_number: num [1:125] 1 2 3 4 5 6 7 8 9 10 ...
```



```r
bronte_chapters %>%
    filter(title == 'Jane Eyre: An Autobiography') %>%
    head()
```

```
## # A tibble: 6 x 4
##   title           author     chapter                              chapter_number
##   <chr>           <chr>      <chr>                                         <dbl>
## 1 Jane Eyre: An … Brontë, C… "\n\n\nthere was no possibility of …              1
## 2 Jane Eyre: An … Brontë, C… "i\n\n\ni resisted all the way: a n…              2
## 3 Jane Eyre: An … Brontë, C… "ii\n\n\nthe next thing i remember …              3
## 4 Jane Eyre: An … Brontë, C… "v\n\n\nfrom my discourse with mr. …              4
## 5 Jane Eyre: An … Brontë, C… "\n\n\nfive o’clock had hardly stru…              5
## 6 Jane Eyre: An … Brontë, C… "i\n\n\nthe next day commenced as b…              6
```



```r
bronte_words <- bronte_chapters %>% 
    group_by(title,author,chapter_number) %>%
    unnest_tokens(line, chapter, token = "lines") %>%
    ungroup() %>%
    group_by(title) %>%
    mutate(
        line_number = row_number()
    ) %>%
    ungroup() %>%
    unnest_tokens(word, line, token = "words")
```



```r
bronte_words
```

```
## # A tibble: 471,348 x 5
##    title             author        chapter_number line_number word    
##    <chr>             <chr>                  <dbl>       <int> <chr>   
##  1 Wuthering Heights Brontë, Emily              1           1 1801    
##  2 Wuthering Heights Brontë, Emily              1           1 i       
##  3 Wuthering Heights Brontë, Emily              1           1 have    
##  4 Wuthering Heights Brontë, Emily              1           1 just    
##  5 Wuthering Heights Brontë, Emily              1           1 returned
##  6 Wuthering Heights Brontë, Emily              1           1 from    
##  7 Wuthering Heights Brontë, Emily              1           1 a       
##  8 Wuthering Heights Brontë, Emily              1           1 visit   
##  9 Wuthering Heights Brontë, Emily              1           1 to      
## 10 Wuthering Heights Brontë, Emily              1           1 my      
## # … with 471,338 more rows
```


```r
write.csv(bronte_words,'../data/bronte_words.csv',quote = FALSE,row.names = FALSE)
```




See stackoverflow for help with using regex to detect sentences: https://stackoverflow.com/questions/47211643/tokenizing-sentences-with-unnest-tokens-ignoring-abbreviations


```r
# dealing with abbreviated titles
titles =  c("mr", "dr", "mrs", "ms", "sr", "jr")
regex = paste(paste0("(?<!\\b(", paste(titles, collapse = "|"), c("))\\.","))\\?","))\\!")),collapse="|")


bronte_sentences <- bronte_chapters %>% 
    group_by(title,author,chapter_number) %>%
    unnest_tokens(sentence, chapter, token = "regex", pattern = regex) %>%
    ungroup() %>%
    group_by(title,author) %>%
    mutate(
        sentence_number = row_number(),
        sentence = str_replace_all(sentence,"[\n]"," ")
    ) %>%
    mutate(
        sentence = str_replace_all(sentence,"“|” ","")
    ) %>%
    ungroup()
```


```r
bronte_sentences %>% head
```

```
## # A tibble: 6 x 5
##   title     author   chapter_number sentence                     sentence_number
##   <chr>     <chr>             <dbl> <chr>                                  <int>
## 1 Wutherin… Brontë,…              1 "   1801—i have just return…               1
## 2 Wutherin… Brontë,…              1 " this is certainly a beaut…               2
## 3 Wutherin… Brontë,…              1 " in all england, i do not …               3
## 4 Wutherin… Brontë,…              1 " a perfect misanthropist’s…               4
## 5 Wutherin… Brontë,…              1 " a capital fellow"                        5
## 6 Wutherin… Brontë,…              1 " he little imagined how my…               6
```


```r
table(bronte_sentences$title)
```

```
## 
## Jane Eyre: An Autobiography The Tenant of Wildfell Hall 
##                       10029                        7357 
##           Wuthering Heights 
##                        6838
```

