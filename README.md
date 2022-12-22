---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# huggingfaceR

<!-- badges: start -->

<!-- badges: end -->

The goal of `huggingfaceR` is to to bring state-of-the-art NLP models to R. `huggingfaceR` is built on top of Hugging Face's [transformers](https://huggingface.co/docs/transformers/index) library; and has support for navigating the Hugging Face Hub [The Hub](https://huggingface.co/models).

## Installation

Prior to installing `huggingfaceR` please be sure to have your python environment set up correctly. 


```r
install.packages("reticulate")
library(reticulate)

install_miniconda()
```

If you are having issues, more detailed instructions on how to install and configure python can be found [here](https://support.rstudio.com/hc/en-us/articles/360023654474-Installing-and-Configuring-Python-with-RStudio).

After that you can install the development version of huggingfaceR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("farach/huggingfaceR")
```

## Example

`huggingfaceR` makes use of the `transformers` `pipline()` abstraction to quickly make pre-trained language models available for use in R. In this example we will load the `distilbert-base-uncased-finetuned-sst-2-english` model and its tokenizer into a pipeline object to obtain sentiment scores.


```r
library(huggingfaceR)

distilBERT <- hf_load_pipeline("distilbert-base-uncased-finetuned-sst-2-english", task = "text-classification")
#> Error in hf_load_pipeline("distilbert-base-uncased-finetuned-sst-2-english", : could not find function "hf_load_pipeline"

hf_stop_token_spam() #Use this to avoid the tokenizers fork - or set use_fast = FALSE
#> Error in hf_stop_token_spam(): could not find function "hf_stop_token_spam"
```

With the pipeline now loaded, we can begin using the model. 


```r
distilBERT("I like you. I love you")
#> Error in distilBERT("I like you. I love you"): could not find function "distilBERT"
```

We can use this pipeline in a typical tidyverse processing chunk. First we load the `tidyverse`.


```r
library(tidyverse)
```

We can use the `huggingfaceR` `hf_load_dataset()` function to pull in the [emotion](https://huggingface.co/datasets/emotion) Hugging Face dataset. This dataset contains English Twitter messages with six basic emotions: anger, fear, love, sadness, and surprise. We are interested in how well the Distilbert model classifies these emotions as either a positive or a negative sentiment.


```r
emotion <- hf_load_dataset(
  "emotion", 
  split = "test",
  label_conversion = "int2str"
  )
#> Error in hf_load_dataset("emotion", split = "test", label_conversion = "int2str"): could not find function "hf_load_dataset"

emotion_model <- emotion |>
  transmute(
    text,
    emotion_id = label,
    emotion_name = label_name,
    distilBERT_sent = distilBERT(text)
  ) |>
  unnest_wider(distilBERT_sent)
#> Error in transmute(emotion, text, emotion_id = label, emotion_name = label_name, : object 'emotion' not found

glimpse(emotion_model)
#> Error in glimpse(emotion_model): object 'emotion_model' not found
```
We can use `ggplot2` to visualize the results.


```r
emotion_model |>
  mutate(
    label = paste0("Distilbert class:\n", label),
    emotion_name = str_to_title(emotion_name)
  ) |>
  ggplot(aes(x = emotion_name, y = score, color = label)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.4, ) +
  scale_color_manual(values = c("#D55E00", "#6699CC")) +
  facet_wrap(~ label) +
  labs(
    title = "Reviewing Distilbert classification predictions",
    x = "Original label",
    y = "Model score",
    caption = "source:\nhttps://huggingface.co/datasets/emotion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
#> Error in mutate(emotion_model, label = paste0("Distilbert class:\n", label), : object 'emotion_model' not found
```


