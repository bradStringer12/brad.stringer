---
title: Test it out
author: Brad Stringer
date: '2024-01-27'
slug: []
categories: []
tags: []
subtitle: ''
excerpt: ''
draft: no
series: ~
layout: single
---


Hello!



Look at this plot...


```r
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(colour = Species)) +
  labs(title = "Flōwers") +
  theme(text = element_text(family = "dosis"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />
```
ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(colour = Species)) +
  labs(title = "Flōwers") +
  theme(text = element_text(family = "dosis"))
  
```
