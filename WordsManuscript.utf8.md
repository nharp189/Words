---
title: Ambiguous Words
shorttitle: AMBIGUOUS WORDS
author: Nicholas R. Harp\textsuperscript{1}, Catherine C. Brown\textsuperscript{1},
  \& Maital Neta\textsuperscript{1}
affiliation:
- id: '1'
  institution: University of Nebraska-Lincoln
authornote: |
  Nicholas R. Harp, Department of Psychology, Center for Brain, Biology, and Behavior, University of Nebraska-Lincoln
  Catherine C. Brown, Department of Psychology, Center for Brain, Biology, and Behavior, University of Nebraska-Lincoln
  Maital Neta, Department of Psychology, Center for Brain, Biology, and Behavior, University of Nebraska-Lincoln
abstract: |-
  We found some ambiugous words.

  Two or three sentences explaining what the **main result** reveals in direct comparison to what was thought to be the case previously, or how the  main result adds to previous knowledge.

  One or two sentences to put the results into a more **general context**.

  Two or three sentences to provide a **broader perspective**, readily comprehensible to a scientist in any discipline.

  <!-- https://tinyurl.com/ybremelq -->

  !!!papaja-author-note(Nicholas R. Harp, Department of Psychology, Center for Brain, Biology, and Behavior, University of Nebraska-Lincoln
  Catherine C. Brown, Department of Psychology, Center for Brain, Biology, and Behavior, University of Nebraska-Lincoln
  Maital Neta, Department of Psychology, Center for Brain, Biology, and Behavior, University of Nebraska-Lincoln


  Correspondence concerning this article should be addressed to Nicholas R. Harp, Postal address. E-mail: nharp@huskers.unl.edu)papaja-author-note!!!
keywords: ambiguity
wordcount: X
bibliography: CANLab_UNL.bib
floatsintext: no
figurelist: no
tablelist: no
footnotelist: no
linenumbers: yes
mask: no
draft: no
documentclass: apa6
classoption: man
output: papaja::apa6_pdf
header-includes: |
  ```{=latex}
  \shorttitle{AMBIGUOUS WORDS}
  \affiliation{
  \vspace{0.5cm}
  \textsuperscript{1} University of Nebraska-Lincoln}
  \keywords{ambiguity\newline\indent Word count: X}
  \usepackage{csquotes}
  \usepackage{upgreek}
  \captionsetup{font=singlespacing,justification=justified}

  \usepackage{longtable}
  \usepackage{lscape}
  \usepackage{multirow}
  \usepackage{tabularx}
  \usepackage[flushleft]{threeparttable}
  \usepackage{threeparttablex}

  \newenvironment{lltable}{\begin{landscape}\begin{center}\begin{ThreePartTable}}{\end{ThreePartTable}\end{center}\end{landscape}}

  \makeatletter
  \newcommand\LastLTentrywidth{1em}
  \newlength\longtablewidth
  \setlength{\longtablewidth}{1in}
  \newcommand{\getlongtablewidth}{\begingroup \ifcsname LT@\roman{LT@tables}\endcsname \global\longtablewidth=0pt \renewcommand{\LT@entry}[2]{\global\advance\longtablewidth by ##2\relax\gdef\LastLTentrywidth{##2}}\@nameuse{LT@\roman{LT@tables}} \fi \endgroup}


  \DeclareDelayedFloatFlavor{ThreePartTable}{table}
  \DeclareDelayedFloatFlavor{lltable}{table}
  \DeclareDelayedFloatFlavor*{longtable}{table}
  \makeatletter
  \renewcommand{\efloat@iwrite}[1]{\immediate\expandafter\protected@write\csname efloat@post#1\endcsname{}}
  \makeatother
  \usepackage{lineno}

  \linenumbers
  ```

---




# Introduction
We wanted to identify ambiguous words. Mention in this section why we also generated clearly positive and negative words.

# Study 1: Pilot 

## Methods

### Participants
Workers on Amazon's Mchanical Turk (MTurk) were invted to participate in an eligibility screener worth \$0.20 with the option to earn a bonus of \$2.05 if they met the requirements and completed the entire study. See **Supplementary Information** for specific MTurk batch settings. The Workers clicked a hyperlink that directed them to the study. The screener task included demographic questions and one block of word ratings that included 5 instances of the word "negative" and 5 instances of the word "positive" (see Procedure below for full details). Workers were invited to complete the entire study if they indicated that they were over 18 years old, had English as their native language, had no history of psychological or neurological disorder, and correctly rated the words "positive" and "negative" as positive or negative with at least 80% accuracy. Of the 145 Workers who completed the screener, 119 met the eligibility requirements, and 103 (54.37% female, 45.63% male) chose to complete the entire study. The final sample was 3.88% Asian, 5.83% Black, 85.44% White, with a mean(sd) age of 37.16(10.60). 

### Material

#### Stimuli
We compiled an initial set of 59 words that we believed had two distinct definitions, one clearly positive definition and one clearly negative definition. To create lists of clearly positive and clearly negative words, we first created a master list of words that were included in both the study by @warriner_norms_2013, for valence and arousal ratings, and the Enlgish Lexicon Project online word query [@balota_english_2007], for lexical characterisic measurements. We then elimiated any words with a mean arousal rating that was greater than 1 standard deviation away from the mean arousal of the list of 59 ambiguous words. We classified "positive" words as those with a mean valence > 7 on the 1-9 scale used by @warriner_norms_2013; "negative" words had mean valence < 3. To ensure that all words shared similar lexical characteristics, we eliminated any words from the master list whose lexical characteristics did not fall within the minimum and maximum values of the 59 ambiguous words' lexical characteristics. The following were used for the cutoffs: length, the frequency of a word as reported by the Hyperspace Analogue to Language (HAL) study [@lund_producing_1996], the log of HAL frequency, number of phonemes, number of syllables, number of morphemes, lexical decision reaction time and accuracy, and naming reaction time and accuracy. The final list of pilot words included 59 ambiguous, 267 positive, and 304 negative words. 

All of the calculations described in this section were scripted using R version 3.6.1 and are available in the **Supplementary Information**. 

#### Software
All tasks were created and presented using Gorilla Experiment Builder [@anwyl-irvine_gorilla_2019]. The study was only accessible to participants using a computer (not a phone or tablet) within the United States.
### Procedure
#### Screener task
After giving informed consent, participants first answered demographic questions about their gender, age, race, native language, and whether they had ever been diagnosed with a psychological or neurological disorder. They then were shown a brief self-guided instructional walkthrough of the task before completing the screener. 

Using a random seed, we selected 20 positive and 20 negative words from the final pilot list for use in the screener task. These 40 words, along with 5 instances of the word "positive" and 5 instances of the word "negative" were presented randomly, one at a time, following a 250 ms fixation cross. Each word remained on screen until the participant indicated that they thought it was positive or negative by pressing A or L on their keyboard (key pairing randomized across participants). If no response was made after 2000ms, a reminder appeared on screen, "Please respond as quickly as you can! A = POSITIVE. L = NEGATIVE." Participants who rated the words "positive" and "negative" with less than 80% accuracy were compensated for their time but were not invited to complete the rest of the study. Participants were also excluded at this point if they indicated that they were younger than 18, that English was not their native language, or that they had been diagnosed with a psychological or neurological disorder. 

#### Word rating rask
The 

### Data analysis

## Results

### Subjective ratings

### Reaction times

# Study 2: Comparison of words with valence bias and IPANAT

## Methods

### Participants

### Material

#### Stimuli
##### Valence Bias with Words
##### Valence Bias with Faces
##### Valence Bias with IAPS
##### IPANAT

#### Software

### Procedure
#### Valence Bias Tasks
#### IPANAT

### Data analysis
#### Valence Bias Tasks
#### IPANAT

## Results

### Subjective ratings
##### Valence Bias with Words
##### Valence Bias with Faces
##### Valence Bias with IAPS
##### IPANAT

### Reaction times
##### Valence Bias with Words
##### Valence Bias with Faces
##### Valence Bias with IAPS
##### IPANAT

### Relationships between the measures

# Discussion
We did this study.

\newpage

# References


\begingroup
\setlength{\parindent}{-0.5in}
\setlength{\leftskip}{0.5in}

<div id = "refs"></div>
\endgroup

\newpage

# Supplementary Information
## MTurk Project Settings
The following are the settings used for the first batch on MTurk. This batch only contributed 6 respondents because the batch was published before the Gorilla task was fully functioning, and the batch expired before all HITs could be filled.

* Title: Screener: Rate words as positive or negative (WARNING: This HIT may contain adult content. Worker discretion is advised.)  
* Description: Bonus available ($2.05) to those who meet eligibility. Complete short demographic questions. Use your keyboard to indicate if you think individual words are positive or negative. 
* Keywords: survey, demographics, rating, rate, words 
* Reward per response: $0.2 
* Number of respondents: 9 
* Time allotted per worker: 1 Hour 
* Survey expires in: 7 Days 
* Auto-approve and pay Workers in: 3 Days 
* Require that Workers be Masters to do your tasks: Yes 
* Specify any additional qualifications Workers must meet to work on your tasks: 
    + Location is UNITED STATES (US) 
    + HIT Approval Rate (%) for all Requesters' HITs greater than 95 
    + Number of HITs Approved greater than 5000 
* Project contains adult content: selected 
* Task Visibility: Hidden - Only Workers that meet my Qualification requirements can see and preview my tasks 

The same settings were used for the rest of the batches except that they did not require that Workers be Masters and the Number of HITs Approved was set to greater than 500, not 5000.

## Study 1 Stimuli
Insert link to repository for 'pick_words.R' 
