library(tidyverse)
library(babynames)

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

x <- c("one\ntwo", "one\ttwo", "\u00a0", "\U0001f604")
x
str_view(x)

# combining 
str_c("x", "y")
str_c("x", "y", "z")
str_c("Hello ", c("John", "Susan"))

df <- tibble(name = c("Flora", "David", "Terra", NA))
df |> mutate(greeting = str_c("Hi ", name, "!"))

df |> 
  mutate(
    greeting1 = str_c("Hi ", coalesce(name, "you"), "!"),
    greeting2 = coalesce(str_c("Hi ", name, "!"), "Hi!") # as same as c()
  )

# combining
df |> mutate(greeting = str_glue("Hi {name}!"))
df |> mutate(greeting = str_glue("{{Hi {name}!}}"))

# introducing something in between strings
str_flatten(c("x", "y", "z"))
str_flatten(c("x", "y", "z"), ", ")
str_flatten(c("x", "y", "z"), ", ", last = ", and ")

df <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)
df |>
  group_by(name) |> 
  summarize(fruits = str_flatten(fruit, ", "))

df2 <- tibble(x = c("1211", "131", "21"))
df2 |> 
  separate_longer_position(x, width = 1) # separating each digits

df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", "edition", "year")
  )

df4 <- tibble(x = c("202215TX", "202122LA", "202325CA")) 
df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4, age = 2, state = 2)
  )

df <- tibble(x = c("1-1-1", "1-1-2", "1-3", "1-3-2", "1"))

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z")
  )

debug |> filter(!x_ok)

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "align_start"
  )

# applications

df <- tibble(x = c("1-1-1", "1-1-2", "1-3-5-6", "1-3-2", "1-3-5-7-9"))

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z")
  )

debug <- df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "debug"
  )

debug |> filter(!x_ok)

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "drop"
  )

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "merge"
  )

str_length(c("a", "R for data science", NA))

babynames |>
  count(length = str_length(name), wt = n)

babynames |> 
  filter(str_length(name) == 15) |> 
  count(name, wt = n, sort = TRUE)

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

str_sub(x, -3, -1)
str_sub("a", 1, 5)

babynames |> 
  mutate(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  )

charToRaw("Hadley")

x1 <- "text\nEl Ni\xf1o was particularly bad this year"
read_csv(x1)$text

x2 <- "text\n\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
read_csv(x2)$text

read_csv(x1, locale = locale(encoding = "Latin1"))$text

read_csv(x2, locale = locale(encoding = "Shift-JIS"))$text

u <- c("\u00fc", "u\u0308")
str_view(u)

str_length(u)

str_sub(u, 1, 1)
u[[1]] == u[[2]]

str_equal(u[[1]], u[[2]])
str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")

str_sort(c("a", "c", "ch", "h", "z"))
str_sort(c("a", "c", "ch", "h", "z"), locale = "cs")

str_view(fruit, "berry")
str_view(c("a", "ab", "ae", "bd", "ea", "eab"), "a.")
str_view(fruit, "a...e")

str_view(c("a", "ab", "abb"), "ab?")

# ab+ matches an "a", followed by at least one "b".
str_view(c("a", "ab", "abb"), "ab+")

# ab* matches an "a", followed by any number of "b"s.
str_view(c("a", "ab", "abb"), "ab*")

str_view(words, "[aeiou]x[aeiou]")
str_view(words, "[^aeiou]y[^aeiou]")
str_view(fruit, "apple|melon|nut")
str_view(fruit, "aa|ee|ii|oo|uu")
str_detect(c("a", "b", "c"), "[aeiou]")

babynames |> 
  filter(str_detect(name, "x")) |> 
  count(name, wt = n, sort = TRUE)

babynames |> 
  group_by(year) |> 
  summarize(prop_x = mean(str_detect(name, "x"))) |> 
  ggplot(aes(x = year, y = prop_x)) + 
  geom_line()

x <- c("apple", "banana", "pear")
str_count(x, "p")

str_count("abababa", "aba")

str_view("abababa", "aba")

babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

babynames |> 
  count(name) |> 
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "-")

x <- c("apple", "pear", "banana")
str_remove_all(x, "[aeiou]")

df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45", 
  "<Brandon>-N_33",
  "<Sharon>-F_38", 
  "<Penny>-F_58",
  "<Justin>-M_41", 
  "<Patricia>-F_84", 
)

df |> 
  separate_wider_regex(
    str,
    patterns = c(
      "<", 
      name = "[A-Za-z]+", 
      ">-", 
      gender = ".",
      "_",
      age = "[0-9]+"
    )
  )

dot <- "\\."

# But the expression itself only contains one \
str_view(dot)

# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
str_view(x)
str_view(x, "\\\\")

str_view(x, r"{\\}")

str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")

str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")

str_view(fruit, "^a")

str_view(fruit, "a$")

str_view(fruit, "apple")

str_view(fruit, "^apple$")

x <- c("summary(x)", "summarize(df)", "rowsum(x)", "sum(x)")
str_view(x, "sum")

str_view(x, "\\bsum\\b")

str_view("abc", c("$", "^", "\\b"))

str_replace_all("abc", c("$", "^", "\\b"), "--")

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "[abc]+")

str_view(x, "[a-z]+")

str_view(x, "[^a-z0-9]+")

# You need an escape to match characters that are otherwise
# special inside of []
str_view("a-b-c", "[a-c]")
str_view("a-b-c", "[a\\-c]")

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "\\d+")
str_view(x, "\\D+")
str_view(x, "\\s+")
str_view(x, "\\S+")
str_view(x, "\\w+")
str_view(x, "\\W+")

str_view(fruit, "(..)\\1")
str_view(words, "^(..).*\\1$")

sentences |> 
  str_replace("(\\w+) (\\w+) (\\w+)", "\\1 \\3 \\2") |> 
  str_view()

sentences |> 
  str_match("the (\\w+) (\\w+)") |> 
  head()

sentences |> 
  str_match("the (\\w+) (\\w+)") |> 
  as_tibble(.name_repair = "minimal") |> 
  set_names("match", "word1", "word2")

x <- c("a gray cat", "a grey dog")
str_match(x, "gr(e|a)y")

str_match(x, "gr(?:e|a)y")
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")

str_view(bananas, regex("banana", ignore_case = TRUE))


x <- "Line 1\nLine 2\nLine 3"
str_view(x, ".Line")
str_view(x, regex(".Line", dotall = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_view(x, "^Line")

str_view(x, regex("^Line", multiline = TRUE))

phone <- regex(
  r"(
    \(?     # optional opening parens
    (\d{3}) # area code
    [)\-]?  # optional closing parens or dash
    \ ?     # optional space
    (\d{3}) # another three numbers
    [\ -]?  # optional space or dash
    (\d{4}) # four more numbers
  )", 
  comments = TRUE
)

str_extract(c("514-791-8141", "(123) 456 7890", "123456"), phone)

str_view(c("", "a", "."), fixed("."))

str_view("x X", "X")

str_view("x X", fixed("X", ignore_case = TRUE))

str_view("i İ ı I", fixed("İ", ignore_case = TRUE))

str_view("i İ ı I", coll("İ", ignore_case = TRUE, locale = "tr"))

pos <- c("He is a boy", "She had a good time")
neg <- c("Shells come from the sea", "Hadley said 'It's a great day'")

pattern <- "^(She|He|It|They)\\b"
str_detect(pos, pattern)

str_detect(neg, pattern)

str_view(words, "^[^aeiou]+$")

str_view(words[!str_detect(words, "[aeiou]")])
str_view(words, "a.*b|b.*a")
words[str_detect(words, "a") & str_detect(words, "b")]

words[str_detect(words, "a.*e.*i.*o.*u")]

words[str_detect(words, "u.*o.*i.*e.*a")]

words[
  str_detect(words, "a") &
    str_detect(words, "e") &
    str_detect(words, "i") &
    str_detect(words, "o") &
    str_detect(words, "u")
]
#> character(0)

str_view(sentences, "\\b(red|green|blue)\\b")

rgb <- c("red", "green", "blue")
str_c("\\b(", str_flatten(rgb, "|"), ")\\b")
str_view(colors())

cols <- colors()
cols <- cols[!str_detect(cols, "\\d")]
str_view(cols)

pattern <- str_c("\\b(", str_flatten(cols, "|"), ")\\b")
str_view(sentences, pattern)

apropos("replace")
head(list.files(pattern = "\\.Rmd$"))



