# Internal message dictionary

Stores lightweight translations for user-facing messages. Extend this
list with new language codes (e.g., "pt", "es") as needed.

## Usage

``` r
.msg_env

.msg_load(lang, msg_lst)

.msg(key, ..., lang = getOption("lang", "en"))
```

## Format

An object of class `environment` of length 1.

## Arguments

- lang:

  Language override; defaults to `getOption("lang", "en")`.

- msg_lst:

  Named character vector/list of message templates.

- key:

  Message identifier to retrieve.

- ...:

  Optional `sprintf` arguments.
