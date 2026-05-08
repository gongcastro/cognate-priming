// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}

//#assert(sys.version.at(1) >= 11 or sys.version.at(0) > 0, message: "This template requires Typst Version 0.11.0 or higher. The version of Quarto you are using uses Typst version is " + str(sys.version.at(0)) + "." + str(sys.version.at(1)) + "." + str(sys.version.at(2)) + ". You will need to upgrade to Quarto 1.5 or higher to use apaquarto-typst.")

// counts how many appendixes there are
#let appendixcounter = counter("appendix")
// make latex logo
// https://github.com/typst/typst/discussions/1732#discussioncomment-11286036
#let TeX = {
  set text(font: "New Computer Modern",)
  let t = "T"
  let e = text(baseline: 0.22em, "E")
  let x = "X"
  box(t + h(-0.14em) + e + h(-0.14em) + x)
}

#let LaTeX = {
  set text(font: "New Computer Modern")
  let l = "L"
  let a = text(baseline: -0.35em, size: 0.66em, "A")
  box(l + h(-0.32em) + a + h(-0.13em) + TeX)
}

#let firstlineindent=0.5in

// documentmode: man
#let man(
  title: none,
  runninghead: none,
  margin: (x: 1in, y: 1in),
  paper: "us-letter",
  font: ("Times", "Times New Roman"),
  fontsize: 12pt,
  leading: 18pt,
  spacing: 18pt,
  firstlineindent: 0.5in,
  toc: false,
  lang: "en",
  cols: 1,
  numbersections: false,
  numberdepth: 3,
  first-page: 1,
  suppresstitlepage: false,
  doc,
) = {

  if suppresstitlepage {counter(page).update(first-page)}
  
  show raw.where(block: true): set par(
    spacing: 6pt,
    leading: 6pt
  )
  
  show raw.where(block: true): set text(
    size: 10pt
  )

  set page(
    margin: margin,
    paper: paper,
    header-ascent: 50%,
    header: grid(
      columns: (9fr, 1fr),
      align(left)[#upper[#runninghead]],
      align(right)[#context counter(page).display()]
    )
  )
  

  

 

  set table(    
    stroke: (x, y) => (
        top: if y <= 1 { 0.5pt } else { 0pt },
        bottom: .5pt,
      )
  )

  set par(
    justify: false, 
    leading: leading,
    first-line-indent: firstlineindent
  )

  // Also "leading" space between paragraphs
  set block(spacing: spacing, above: spacing, below: spacing)

  set text(
    font: font,
    size: fontsize,
    lang: lang
  )
  
  show link: set text(blue)
  show "al.'s": "al.\u{2019}s"

  show quote: set pad(x: 0.5in)
  show quote: set par(leading: leading)
  show quote: set block(spacing: spacing, above: spacing, below: spacing)
  // show LaTeX
  show "TeX": TeX
  show "LaTeX": LaTeX

  // format figure captions
  show figure.where(kind: "quarto-float-fig"): it => block(width: 100%, breakable: false)[
    #if int(appendixcounter.display().at(0)) > 0 [
      #heading(level: 2, outlined: false)[#it.supplement #appendixcounter.display("A")#it.counter.display()]
    ] else [
      #heading(level: 2, outlined: false)[#it.supplement #it.counter.display()]
    ]
    #align(left)[#par[#emph[#it.caption.body]]]
    #align(center)[#it.body]
  ]
  
  // format table captions
  show figure.where(kind: "quarto-float-tbl"): it => block(width: 100%, breakable: false)[#align(left)[
  
    #if int(appendixcounter.display().at(0)) > 0 [
      #heading(level: 2, outlined: false, numbering: none)[#it.supplement #appendixcounter.display("A")#it.counter.display()]
    ] else [
      #heading(level: 2, outlined: false, numbering: none)[#it.supplement #it.counter.display()]
    ]
    #par[#emph[#it.caption.body]]
    #block[#it.body]
  ]]
  
    set heading(numbering: "1.1")
    
    show heading: set text(size: fontsize)


 // Redefine headings up to level 5 
  show heading.where(
    level: 1
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(center)
    #if(numbersections and it.outlined and numberdepth > 0 and counter(heading).get().at(0) > 0) [#counter(heading).display()] #it.body
  ]
  
  show heading.where(
    level: 2
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #if(numbersections and it.outlined and numberdepth > 1 and counter(heading).get().at(0) > 0) [#counter(heading).display()] #it.body
  ]
  
  show heading.where(
    level: 3
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #set text(style: "italic")
    #if(numbersections and it.outlined and numberdepth > 2 and counter(heading).get().at(0) > 0) [#counter(heading).display()] #it.body
  ]

  show heading.where(
    level: 4
  ): it => text(
    weight: "bold",
    it.body
  )

  show heading.where(
    level: 5
  ): it => text(
    weight: "bold",
    style: "italic",
    it.body
  )
  
  

  if cols == 1 {
    doc
  } else {
    columns(cols, gutter: 4%, doc)
  }
  



}


#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
)

#show: document => man(
  runninghead: "Developmental trajectories of bilingual word recognition",
  lang: "en",
  numberdepth: 3,
  document,
)

\
\
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Developmental trajectories of bilingual word recognition
]
)
]
#set align(center)
#block[
\
Gonzalo Garcia-Castro#super[1,2];, Serene Siow#super[3];, Kim Plunkett#super[3];, and Nuria Sebastian-Galles#super[2]

#super[1];Neurodevelopment and Comparative Cognition, Institut de Recerca Sant Joan de Déu (IRSJD)

#super[2];Center for Brain and Cognition, Universitat Pompeu Fabra

#super[3];Department of Experimental Psychology, University of Oxford

]
#set align(left)
\
\
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Author Note
]
)
]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Gonzalo Garcia-Castro #box(image("_extensions/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) #link("https://orcid.org/0000-0002-8553-4209")

Serene Siow #box(image("_extensions/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) #link("https://orcid.org/0000-0001-6482-2191")

Kim Plunkett #box(image("_extensions/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) #link("https://orcid.org/0000-0003-0216-7480")

Nuria Sebastian-Galles #box(image("_extensions/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) #link("https://orcid.org/0000-0001-6938-2498")

Correspondence concerning this article should be addressed to Gonzalo Garcia-Castro, Neurodevelopment and Comparative Cognition, Institut de Recerca Sant Joan de Déu (IRSJD), Barcelona, 08005, Email: #link("mailto:gonzalo.garcia@sjd.es")[gonzalo.garcia\@sjd.es]

#emph[Keywords];: cognate, word recognition, lexicon, language acquisition, vocabulary, bilingualism, general additive mixed models, bayesian

#pagebreak()

#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Developmental trajectories of bilingual word recognition
]
)
]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Building a mental lexicon is a major achievement in the development of an infant: by storing representations of how familiar words sound and what they mean, an infant is able to make sense of their linguistic input. The foundations of an initial lexicon are in place before the end the first year of life (#link(<ref-bergelson2012months>)[Bergelson & Swingley, 2012];, #link(<ref-bergelson2015early>)[2015];; #link(<ref-halle1994emergence>)[Hallé & de Boysson-Bardies, 1994];; #link(<ref-parise2012electrophysiological>)[Parise & Csibra, 2012];; #link(<ref-tincoff1999beginnings>)[Tincoff & Jusczyk, 1999];; #link(<ref-vihman2004crosslinguistic>)[Vihman, 2004];). This initial lexicon consists of only a few items; mainly words for people, interjections, body parts, and food (#link(<ref-tardif2008baby>)[Tardif et al., 2008];; #link(<ref-tincoff2012sixmontholds>)[Tincoff & Jusczyk, 2012];), but it undergoes rapid growth during the second year of life (#link(<ref-bergelson2020comprehension>)[Bergelson, 2020];; #link(<ref-bloom2002how>)[Bloom, 2002];; #link(<ref-ganger2004reexamining>)[Ganger & Brent, 2004];; #link(<ref-goldfield1990early>)[Goldfield & Reznick, 1990];; #link(<ref-mcmurray2007defusing>)[McMurray, 2007];). According to parental reports, the average 15-month-old infant already understands more than 100 words, and by two years of age, they understand more than 400 (#link(<ref-frank2021variability>)[Frank et al., 2021];). This accelerated lexical developmental is reflected in infants' trajectories of word recognition: infants recognise familiar words faster and more efficiently as they approach their second birthday (#link(<ref-fernald1998rapid>)[Fernald et al., 1998];, #link(<ref-fernald2001when>)[2001];; #link(<ref-hurtado2007spoken>)[Hurtado et al., 2007];). Despite being exposed to a more complex linguistic input, bilinguals show equivalent trajectories of word acquisition and word recognition to their monolingual peers' (#link(<ref-bialystok2009bilingualism>)[Bialystok, 2009];; #link(<ref-byers-heinlein2023sometimes>)[Byers-Heinlein et al., 2023];; #link(<ref-dehouwer2014bilingual>)[De Houwer et al., 2014];; #link(<ref-hoff2012dual>)[Hoff et al., 2012];; #link(<ref-legacy2018vocabulary>)[Legacy et al., 2018];; #link(<ref-pearson1994patterns>)[Pearson & Fernández, 1994];; #link(<ref-vihman2007onset>)[Vihman et al., 2007];). This is a remarkable deed for two reasons. First, bilingual infants receive a relatively reduced linguistic input in each of their languages, compared to monolinguals (#link(<ref-cattani2014how>)[Cattani et al., 2014];; #link(<ref-costa2014does>)[Costa & Sebastián-Gallés, 2014];; #link(<ref-thordardottir2011relationship>)[Thordardottir, 2011];). Second, they face a more complex referential context: they often learn two labels for each referent (one in each language), which additionally may not be direct translations of each other (#link(<ref-au1990principle>)[Au & Glusman, 1990];; #link(<ref-bilson2015semantic>)[Bilson et al., 2015];; #link(<ref-dehouwer2006early>)[De Houwer et al., 2006];; #link(<ref-tsui2022are>)[Tsui et al., 2022];). The mechanisms that allow bilingual' trajectories of lexical developmental to keep up with monolinguals' are still unclear.

Previous studies have pointed to the similarity between the two languages of exposure as a facilitator of lexical acquisition in bilinguals (#link(<ref-blom2020crosslanguage>)[Blom et al., 2020];; #link(<ref-floccia2018vocabulary>)[Floccia et al., 2018];; #link(<ref-gampe2021does>)[Gampe et al., 2021];). Floccia et al. (#link(<ref-floccia2018vocabulary>)[2018];) reported larger vocabulary sizes in bilingual toddlers leaning two languages that shared high lexical similarity. The authors collected parental reports of vocabulary data from a sample of 367 bilingual children living in the United Kingdom, who were learning English and an additional language (out of a diverse pool of 13 languages). The authors then calculated the average phono-lexical similarity between English and each of the additional languages. English and Dutch shared the highest similarity, while English and Mandarin shared the lowest. Overall, children's vocabulary sizes in the additional language was positively associated with the amount of language similarity between their two languages. For instance, English-Dutch bilinguals showed larger vocabulary sizes in Dutch than English-Mandarin bilinguals did in Mandarin. The authors suggested that the acquisition of words in the additional language might be facilitated by their cognate status (i.e., being phonologically similar to their translation equivalent). If this is the case, larger vocabulary sizes might then be expected in bilinguals learning two languages sharing a high proportion of cognates. This would be consistent with available evidence of an earlier acquisition of cognate words (#link(<ref-bosch2014first>)[Bosch & Ramon-Casas, 2014];; #link(<ref-garcia2025cognate>)[Garcia-Castro et al., 2025];; #link(<ref-mitchell2022cognates>)[Mitchell et al., 2022];; #link(<ref-schelletter2002effect>)[Schelletter, 2002];).

The facilitation effect of cognateness is in line with the language non-selective account of bilingual lexical access. This account proposes that bilinguals activate both languages in parallel, even during monolingual situations. In adults, there is robust evidence in favour of this language non-selective account of lexical access (#link(<ref-degroot1992determinants>)[de Groot, 1992];; #link(<ref-dijkstra1999recognition>)[Dijkstra et al., 1999];, #link(<ref-dijkstra2010cross>)[2010];; #link(<ref-dufour1995matching>)[Dufour & Kroll, 1995];; #link(<ref-marian1999activation>)[Marian & Spivey, 1999];; #link(<ref-schwartz2007reading>)[Schwartz et al., 2007];; #link(<ref-spivey1999cross>)[Spivey & Marian, 1999];). Costa et al. (#link(<ref-costa2000cognate>)[2000];) presented highly-proficient Catalan-Spanish bilinguals with a series of pictures of familiar objects. Participants were asked to name each picture in Spanish. Unbeknownst to participants, the authors manipulated the cognate status pictures' labels in Catalan and Spanish. In half of the trials, the labels associated with the pictures were cognates (e.g., #emph[cat];-#emph[gat] \[cat\]), whereas in the other half of the trials the labels were non-cognates (e.g., #emph[taula];-#emph[mesa] \[table\]). Participants named pictures faster in cognate trials than in non-cognate trials. Spanish monolinguals showed equivalent naming times in both conditions. These results revealed that bilinguals activated their Catalan phonology, despite performing the naming task exclusively in Spanish: the visual recognition of the presented pictures led to the parallel activation of its associated phonological forms in both languages, which influenced the subsequent dynamics of word production.

Parallel activation has also been reported in the developing lexicon (#link(<ref-bosma2019longitudinal>)[Bosma et al., 2019];; #link(<ref-bosma2020cognate>)[Bosma & Nota, 2020];; #link(<ref-floccia2020translation>)[Floccia et al., 2020];; #link(<ref-jardak2019labels>)[Jardak & Byers-Heinlein, 2019];; #link(<ref-poarch2012crosslanguage>)[Poarch & Van Hell, 2012];; #link(<ref-singh2014one>)[Singh, 2014];; #link(<ref-vonholzen2019impact>)[Von Holzen et al., 2019];). Von Holzen and Mani (#link(<ref-vonholzen2012language>)[2012a];) found evidence of cross-language phonological priming in a sample of 20 German-English bilinguals aged 21 to 43 months. In their experimental task, each trial begun with the auditory presentation of an English prime word, followed by a target word in German, and a pair of target and distractor pictures. The authors recorded participants' target picture looking as a measure of target word recognition. The authors manipulated the cross-linguistic phonological overlap between the prime and the target labels. In a #emph[priming through translation] condition, the English prime labels (leg) did not overlap with the German target labels (#emph[Stein] \[stone\]), but with their German translations (#emph[Bein];) did. In the #emph[unrelated] condition, prime and target labels were not phonologically related in either German or English. If participants accessed their lexicon in a language non-selective way, the auditory presentation of the prime label in English should lead to the co-activation of its German translation. If this is the case, target word recognition should be interfered by the prior activation of a phonologically related German prime label. Under this hypothesis, the authors anticipated an delayed target looking in priming through translation trials, compared to unrelated trials. The results supported this hypothesis. In spite of the relevance of Von Holzen and Mani (#link(<ref-vonholzen2012language>)[2012a];) study, some methodological issues deserve some consideration. First, for most participants, exposure to English (the less prevalent language) was lower than the minimal amount conventionally considered the threshold for bilingual exposure (#link(<ref-byers-heinlein2021multilab>)[Byers-Heinlein et al., 2021];; #link(<ref-rocha-hidalgo2023defining>)[Rocha-Hidalgo & Barr, 2023];). Second, some of the prime labels in the priming through translation condition were cognates. If both English and German labels overlap phonologically with the German target label, priming effects can be explained by interference between words from the same language, as opposed to cross-language interference. Third---and most critically---participants were exposed to both English and German word in a by-trial basis. This creates a context in which interference effects may not have arised from the competition between the prime translation and the target words, but between the target word and any other word in the other language. Paradigms in which the experimental task is conducted exclusively in one language, while cross-linguistic features are covertly manipulated, offer a methodologically stronger basis for studying language non-selectivity in the developing lexicon (#link(<ref-grosjean1997bilingual>)[Grosjean, 1997];).

Mani and Plunkett (#link(<ref-mani2010infant>)[2010];) designed an implicit naming task, in which primes consisted of pictures presented in silence, instead of auditory labels. In each trial, English monolingual infants were first presented with pictures of familiar objects for 1,500 ms. Then, a target-distractor picture pair was presented for 2,000 ms, and then the auditory label of the target picture was presented. Post-naming target looking was recorded for another 2,000 ms until the end of the trial, as a measure of target word recognition. The authors manipulated the phonological overlap between the prime and the target labels, so that in half of the trials both labels were phonologically related, sharing phonological onset (#emph[cat];-#emph[cup];), or phonologically unrelated (#emph[ball];-#emph[comb];). Prime, target and distractor were unrelated otherwise. At 18 months of age, participants showed a stronger looking preference for the target pictures after phonologically related prime pictures, compared to after phonologically unrelated primes. Since the prime pictures were presented in silence, their results suggested that infants implicitly named the prime pictures, and that the phonology of the resulting word interacted with the subsequent recognition of the auditory target word. Later, Mani and Plunkett (#link(<ref-mani2011phonological>)[2011];) tested 21-month-old infants in the same task. This time, priming effects were observed in the opposite direction: when prime and target labels were phonologically related, participants showed significantly weaker target looking preference, compared to unrelated trials. The size of this interference effect was associated with participants' vocabulary size, and to the cohort size of the prime label. The authors interpreted this finding as indicating a developmental shift. At 18 months, participants' word recognition might have experienced a sub-lexical facilitation effect, in which the prior activation of the shared phonological onset between the prime and target labels boosted word recognition. In older participants, the lexicon might have reached a critical size at which the recognition of the target was delayed by the activation of its phonological cohort.

The implicit naming paradigm provides an ideal experimental paradigm to study the developing bilingual lexicon. By covertly manipulating the cross-linguistic relationship between the prime and target labels, parallel activation can be tested while participants are presented with auditory stimuli (target labels) exclusively in one of their languages (see #link(<ref-vonholzen2014bilinguals>)[Von Holzen & Mani, 2014] for a similar approach in bilingual adults). Capitalizing on the language non-selective account of lexical access, we exploited the implicit naming to investigate the mechanisms behind the emergence of phonological priming effects in the bilingual developing lexicon. We tested a cohort of monolingual and bilingual infants learning Catalan and Spanish between 20 and 32 months of age. We compared the performance of participants with differing vocabulary sizes in the word recognition task. In order to circumvent the problem of limited vocabulary knowledge in the non-dominant language, we tested participants only in their dominant language (#link(<ref-costa2014does>)[Costa & Sebastián-Gallés, 2014];).

Following Mani and Plunkett (#link(<ref-mani2010infant>)[2010];), each trial in the task started with the silent presentation of a prime picture. Both monolingual and bilingual infants were expected to implicitly name the prime picture. According to the language non-selective hypothesis of lexical access, bilinguals should generate two labels for the prime picture, one in each language. To test this prediction, we manipulated the phonological similarity between the prime and the target words in both languages (see #link(<fig-hypotheses>)[Figure~1];). In #emph[Related/Non-cognate] trials, prime and target labels shared phonological onset in only the language of test. For instance an infant tested in Catalan would be presented with a chair as prime picture (/kəˈði.ɾə/#sub[CAT];--/ˈsi.ʝa/#sub[SPA] \[chair\]) and with /kuˈʎe.ɾə/#sub[CAT] \[spoon\] as target label as target label. In line with Mani and Plunkett (#link(<ref-mani2011phonological>)[2011];), we anticipated that the phonological overlap between prime and target should modulate target word recognition in both monolinguals and bilinguals. This should be reflected in a delayed target looking preference, compared to #emph[Unrelated] trials, in which prime and target did not share phonological onset. In #emph[Related/Cognate] trials, the prime shared phonological onset with the target in both languages. For instance, the same infants tested in Catalan would be presented with a car as prime picture (/ˈko.t͡ʃə/#sub[CAT];--/ˈko.t͡ʃe/#sub[SPA];) and with /kuˈʎe.ɾə/#sub[CAT] \[spoon\] as target label. In bilinguals, parallel activation of the prime in both languages should increase the cohort of the target word, leading to stronger interference effects in this condition, compared to #emph[Related/Non-cognate] and #emph[Unrelated] trials.

In line with previous studies in monolinguals, we further predicted that the strength of the lexical interference effects in the #emph[Related/Non-cognate] and #emph[Related/Cognate] conditions would be associated with participants' vocabulary size. Target word recognition should be delayed by the activation of a larger cohort of phonologically related words (#link(<ref-avila2021longitudinal>)[Avila-Varela et al., 2021];; #link(<ref-chow2017spoken>)[Chow et al., 2017];; #link(<ref-mani2011phonological>)[Mani & Plunkett, 2011];; #link(<ref-mayor2014infant>)[Mayor & Plunkett, 2014];). We defined vocabulary size as the amount of words participants were reported to understand in their dominant language by their caregivers. The choice of the dominant language for calculating vocabulary sizes is due to several reasons. First, it allows a more fair comparison between monolinguals (who do not know any language other than their dominant language) and bilinguals (who may know words in a second language). Second, since participants were tested exclusively in their dominant language, their vocabulary size in the dominant language is more likely to be associated with participants' performance in the task. Third, previous work on word recognition in bilinguals suggests that vocabulary size in the dominant language predicts participants' performance better than total vocabulary (in which vocabulary sizes in both languages are summed together) (#link(<ref-marchman2010vocabulary>)[Marchman et al., 2010];).

Because of the short-lived effects of cross-language activation on lexical processing, and to maximise the probability of detecting priming effects, we introduced a change in the sequence of the trials relative to the original implementation by Mani and Plunkett (#link(<ref-mani2010infant>)[2010];). We presented target auditory labels immediately after the offset of the prime picture, and before the onset of the target and distractor pictures. By presenting prime pictures and target auditory labels closer in time, implicit naming of the prime picture should be more likely to influence the recognition of the target word. To test the effects of this methodological change, we first run a control experiment, Study 1, in which we tested a group of same-aged English monolinguals. In Study 2, we tested a group of monolinguals and bilinguals learning Catalan and Spanish.

= Study 1
<study-1>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
In this study, we conducted a conceptual replication of Mani and Plunkett (#link(<ref-mani2010infant>)[2010];) and Mani and Plunkett (#link(<ref-mani2011phonological>)[2011];). We tested a group of English monolinguals aged 20 to 32 months, living in the Oxfordshire area (United Kingdom). As just said, participants were tested exclusively in English. As in the original studies, we manipulated the phonological relationship between the prime and the target label. We expected participants' target looking to change as a function of the phonological relatedness between the prime and target English labels. This would reveal that participants implicitly named the prime pictures, generating a phonological label that influenced the subsequent recognition of a phonologically related word. In half of the trials the English prime label was phonologically related to its Spanish translation, that it they were cognates; in the other half they were not phonologically related (non-cognates)#footnote[It was initially planned to collected data from English monolinguals and English-Spanish bilinguals in Oxford, therefore the manipulation of the cognate status of the words in English and Spanish. Due to time limitations imposed by the COVD-19 lockdown between 2020 and 2022, collecting data from bilinguals was not possible. We report the available data from English monolinguals as a control for Catalan and Spanish monolinguals and bilinguals from Barcelona in Study 2.];. Given participants' lack of knowledge of Spanish (or any language other than English), participants' performance was predicted to be unaffected by the cognate status of the primes.

== Methods
<methods>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
All materials, data, and reproducible code can be found at the OSF (#link("https://osf.io/ckydb/")[https:\/\/osf.io/hy984/];) and GitHub (#link("https://github.com/gongcastro/cognate-priming");) repositories. For reproducibility, a Docker image of the RStudio session is available on DockerHub (#link("https://hub.docker.com/repository/docker/gongcastro/cognate-priming/");). This study was conducted according to guidelines laid down in the Declaration of Helsinki, and was approved by the Drug Research Ethical Committee (CEIm) of the IMIM Parc de Salut Mar, reference 2020/9080/I and the Medical Sciences Research Ethics Board at the University of Oxford, reference R60939/RE009. Before every testing session, caregivers were asked to read and sign an informed consent form, and were given a token of appreciation at the end of it.

=== Participants
<participants>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We collected data from 112 children (41 female, 68 male, with three additional participants' sex not being reported; Age: #emph[Mean] = 26.36 months, #emph[SD] = 4.01, #emph[Range] = 20.03--32.5) (see #link(<tbl-participants>)[Table~1] for a detailed summary of participants' age and language profile), living in the Oxfordshire area (United Kingdom). Participants were tested at the Oxford BabyLab at the University of Oxford. Families were recruited from maternity rooms in private hospitals and social media, and contacted via phone when the child's age spanned between 20 and 32 months. From the 112 children that participated, 97 participated once, and 15 participated twice. Recurrent participants were tested with at least 2.82 months of difference. We gathered a total of 127 testing sessions. All participants were being raised in exclusively British English monolingual homes. Participants' vision was normal, none used glasses or any other type of vision corrector.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We collected vocabulary data using parental responses to the Oxford Communicative Development Inventory (OCDI) (#link(<ref-hamilton2000infant>)[Hamilton et al., 2000];). The OCDI is an adaptation of the MacArthur-Bates Communicative Development Inventory (#link(<ref-fenson1994variability>)[Fenson et al., 1994];) to British English. The OCDI includes a vocabulary checklist containing 418 words from 21 semantic-functional categories (e.g., action words, animals, household objects, adverbs, etc.). For each word, caregivers are asked to answer if they child is able to #emph[understand];, #emph[understand and say] or does not understand or say the word. We calculated participants' receptive vocabulary size scores as the number of words that caregivers marked as #emph[understands] or #emph[understands and says];. Families were sent the questionnaire immediately after each experimental session, and were given two weeks to fill it. In the case that a complete response to the OCDI was not provided within the two-week limit, the participants' testing session was excluded from the analyses (#emph[n] = 3). #link(<fig-vocabulary-oxf>)[Figure~2] shows the distribution of participants' vocabulary sizes across ages.

=== Design
<design>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Participants were presented with 32 trials in random order, which belonged to two conditions: #emph[Related] and #emph[Unrelated] trials. In #emph[Related] trials (#emph[n] = 16), the English label of the prime was phonologically related to the target label, sharing phonological onset (e.g., /tɹiː/ \[tree\]--/tɹʌk/ \[truck\]). In #emph[Unrelated] trials, the prime and target labels did not share phonological onset (e.g., /dɔː/ \[door\]--/sɒk/ \[sock\]). Especial attention was paid to avoiding semantic or taxonomic relationships between prime and target words, and between prime and distractor words. Distractors were always phonologically unrelated to the prime and target labels in the same trial.

#link(<fig-task>)[Figure~3] illustrates the sequence of a trial. Each trial started with the presentation of an attention getter for 3,000 ms. Then, the prime picture was presented in silence in the centre of the screen for 1,500 milliseconds. Fifty milliseconds after the offset of the prime image, an auditory label was played, 700 milliseconds after the onset of the word, the target and distractor pictures were presented side-by-side during 1,000 milliseconds until the end of the trial. After this, the attention getter of the next trial was immediately presented. Each experimental session lasted approximately 10 minutes.

=== Stimuli
<stimuli>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We created four lists of trials, across which the same target-distractor pair appeared with a different prime, counterbalancing the condition to which it belonged. For instance, in list one the #emph[ball];-#emph[trousers] pair was preceded by #emph[bike] (#emph[Related/Cognate];), by #emph[butterfly] (#emph[Related/Non-cognate];) in list two, and by #emph[star] and #emph[nose] (#emph[Unrelated];) in lists three and four (see #strong[?\@sec-appendix-d] for a detailed description of the stimuli). #link(<tbl-stimuli>)[Table~2] shows a detailed summary of the stimuli properties, broken down by trial type and testing language. Trials included in each condition had equivalent length (number of phonemes) and lexical frequency. Lexical frequencies were extracted from the English corpora from the CHILDES database (#link(<ref-macwhinney2000childes>)[MacWhinney, 2000];; #link(<ref-sanchez2019childesdb>)[Sanchez et al., 2019];) as counts per million words, and transformed into Zipf scores for easier cross-language comparison (#link(<ref-vanheuven2014subtlexuk>)[Van Heuven et al., 2014];; #link(<ref-zipf1945meaning>)[Zipf, 1945];). Audios had an average duration of 864.23 ms (#emph[SD] = 148.53, #emph[Range] = 570--1,250).

The auditory stimuli were natural exemplars of the selected target words, spoken by a Southern British English female speaker who was instructed to pronounce each word in a toddler-directed manner. We used the Audacity and Praat (#link(<ref-boersma2001speak>)[Boersma & Van Heuven, 2001];) software packages to trim, denoised, and normalised their amplitude. The visual stimuli were realistic photographic representations of a typical exemplars of the prime, target, and distractor words. Image backgrounds were removed from the original pictures using the GNU Image Manipulation Program (GIMP), resized to a rectangle of a maximum of 400 pixels height or wide, and finally placed in the centre of a 50% grey rectangle square of 500 $times$ 500 pixels. The final stimuli had a resolution of 72 dpi. When presented in the eye-tracker screen, the areas of interest (AOI) occupied an area of 13.23 $times$ 13.23 cm (11.613$""^circle.stroked.tiny$ visual angle from participants' perspective).

=== Procedure
<procedure>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Testing took place in a sound-proof room at the BabyLab of the University of Oxford. Participants sat on their caregivers' lap in a dimly lit testing booth while the experimenter conducted the experiment from outside. Caregivers were instructed to keep their eyes shut (to avoid recording their gaze, instead of the participant's), to be still, and to avoid interacting with the participant verbally or non-verbally. Participants sat at approximately 65 cm from the eye-tracker and a 23-inches screen with 1920 $times$ 1080 resolution. The study was run on Windows 7 (64-bit), using a custom Matlab script, PresentMate, based on the PsychToolbox-3 extension (3.0.10, 32 bit) (#link(<ref-brainard1997psychophysics>)[Brainard & Vision, 1997];; #link(<ref-kleiner2007s>)[Kleiner et al., 2007];; #link(<ref-pelli1997videotoolbox>)[Pelli & Vision, 1997];). Visual fixations were recorded using a Tobii TX300 eye-tracker (Tobii Technology, Stockholm, Sweden). The Tobii Analytics SDK 3.0 was used to interact with the eye-tracking while the experiment was running. Sampling rate was set at 120 Hz. A 9-point calibration was performed before every experimental session, in which the picture of a colourful beach ball was presented. We set a 55% grey background for the screen during calibration and stimuli presentation. Auditory stimuli were presented through two loudspeakers located behind the screen, one to each side. The experimenter monitored the experimental from outside the room using a centrally located video camera place above the screen. After a successful calibration the experimenter triggered the onset of the first trial. Trials were presented uninterruptedly and without intervention of the experimenter until the 32 trials were presented, or the experimental session had to be stopped because of the participant's behaviour.

=== Data analysis
<data-analysis>
#block[
#heading(
level: 
4
, 
numbering: 
none
, 
[
Data processing.
]
)
]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We defined a time window of interest from 200 ms after target and distractor pictures onset until the end of the trial at 2,000 ms when both pictures disappeared from screen. The first 200 ms of the test phase were discarded to avoid modelling fixations driven by processes other than auditory word recognition (#link(<ref-fernald1998rapid>)[Fernald et al., 1998];, #link(<ref-fernald2001when>)[2001];). Missing eye-tracker samples were interpolated using the last-observation-carried-forward (see #link(<ref-zettersten2023peekbank>)[Zettersten et al., 2023] for a similar approach), with a maximum of 20 maximum consecutive missing samples being interpolated (an equivalent of 166.67). Target looking probability was calculated as the empirical logit, using the number of samples inside the time bin in which the participant was looking at the target and distractor AOI (see #ref(<eq-elogit>, supplement: [Equation])) (#link(<ref-agresti2012categorical>)[Agresti, 2012];; #link(<ref-barr2008analyzing>)[Barr, 2008];), as follows:

#math.equation(block: true, numbering: "(1)", [ $ eta' = ln #scale(x: 240%, y: 240%)[\(] frac(upright("Target") + 0.5, upright("Distractor") + 0.5) #scale(x: 240%, y: 240%)[\)] $ ])<eq-elogit>

We gathered data from 3,484 trials from 110 testing sessions, generated from 97 distinct participants. We excluded trials in which participants failed to provide 50% valid eye-tracking samples (equivalent to 750 ms) during the prime phase (#emph[n] = 829) or 50% valid samples (equivalent to 1,000 ms) during the target-distractor phase (#emph[n] = 650). We also excluded trials in which participants did not provide at least 5% of valid samples (equivalent to 100 ms) of target or distractor looking in the test phase (#emph[n] = 1,003) (see #link(<ref-floccia2020translation>)[Floccia et al., 2020];; #link(<ref-mani2012activation>)[Mani et al., 2012] for a similar approach).

After trials that matched any of the aforementioned exclusion criteria from the dataset, we excluded participants who did not provide at least two valid trials in each condition (#emph[n] = 19), and participants with a vocabulary size lower than 42, which corresponds to 10% of the words in the OCDI vocabulary checklist (#emph[n] = 3). The final dataset included 1,861 trials from 78 testing sessions, generated by 79 distinct participants. Of those participants, 69 provided data from one experimental session, 10 provided data from two experimental sessions, and NA provided data from three experimental sessions. From the trials included in the final dataset, 915 were #emph[Unrelated] trials (502 previously excluded), and 946 were #emph[Related] trials(470 previously excluded).

#block[
#heading(
level: 
4
, 
numbering: 
none
, 
[
Modelling approach.
]
)
]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We used Bayesian Hierarchical General Additive Models (HGAMs) to model the data (#link(<ref-pedersen2019hierarchical>)[Pedersen et al., 2019];), using a Gaussian distribution to model the the logit of target looking. First, we fit a model ($cal(M)_0$) that included the main effects of #emph[Condition] and #emph[Age] as fixed effects in the model. We set an #emph[a priori] contrast for the #emph[Condition] predictor (#link(<ref-schad2020how>)[Schad et al., 2020];), comparing #emph[Unrelated] and #emph[Related] trials (sum-coded as `-0.5` and `+0.5`. Before entering the model, the #emph[Age] predictor was standardised by subtracting from each observation the mean of the predictor, and dividing the result from the standard deviation of the predictor. We included the variable #emph[Session];---which indexes individual testing sessions that may belong to the same participant---as grouping variable, nested within the #emph[Participant] grouping variable---which indexes a distinct participant. This nested random effects structure incorporates the longitudinal design of data collection, in which multiple participants were tested more than once at different ages. We added by-session intercepts and #emph[Condition] slopes, and by-participant intercepts and #emph[Age] slopes. To model the time course of target looking across time bins, we included B-splines for the main effect of #emph[Time];, and for the #emph[Condition] predictor (#link(<ref-wood2017generalized>)[Wood, 2017];). For both splines, we specified $k = 8$ basis functions or #emph[knots];. #ref(<eq-model>, supplement: [Equation]) shows a formal implementation of the model. We implemented this model using `brms` (#link(<ref-burkner2017brms>)[Bürkner, 2017];), an R interface to the Stan probabilistic language (2.33.0) (#link(<ref-carpenter2017stan>)[Carpenter et al., 2017];). We ran two iteration chains using the by-default No U-Turn Sampler algorithm with 1,000 iterations each and an additional 1,000 warm-up iterations per chain.

#math.equation(block: true, numbering: "(1)", [ $  & bold("Target looking by participant ") i bold(" in session ") j\
y_(i j) & tilde.op cal(N) (mu_(i j) \, sigma_(i j))\
\
 & bold("Distributional parameters:")\
eta' (mu_(i j)) & = (beta_0 + u_(0_(i j))) + (beta_1 + u_(1_(i j))) upright("Condition") + beta_2 upright("Age") + sum_(w = 1)^k b_w beta_(3_k) upright("Time") +\
upright("where:")\
 & eta' upright(" is the empirical logit of target fixations")\
 & b_w upright(" is the cubic spline of the ") w upright(" basis function")\
 & k upright(" is the number of knots in the spline ") (k = 8)\
 & bold("Prior:")\
beta_(0 - 3) & tilde.op cal(N) (0 \, 0.5)\
u_(0 - 1_(i j)) & tilde.op cal(N) (0 \, sigma_(0 - 2))\
b_w & tilde.op upright("MVN") (0 \, tau)\
sigma_(0 - 1) \, tau & tilde.op upright("Exponential") (6)\
rho_(0 - 1) & tilde.op L K J C o r r (6)\
upright("where:")\
 & rho_(0 - 1) upright(" are the correlation parameters for ") sigma_(0 - 2) $ ])<eq-model>

We implemented this model using `brms` (#link(<ref-burkner2017brms>)[Bürkner, 2017];), an R interface to the Stan probabilistic language (2.33.0) (#link(<ref-carpenter2017stan>)[Carpenter et al., 2017];). We ran two iteration chains using the by-default No U-Turn Sampler algorithm with 1,000 iterations each and an additional 1,000 warm-up iterations per chain.

== Results
<results>
=== Priming effects
<priming-effects>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We tested the differences between the conditions of interest in two ways. First, we examined the posterior distribution of the regression coefficients of the linear predictors in model $cal(M)_0$ (see #ref(<eq-model>, supplement: [Equation])). We assessed the practical relevance of the coefficients following Kruschke and Liddell (#link(<ref-kruschke2018bayesian>)[2018];). We specified a region of practical equivalence (ROPE) from -0.1 to +0.1, in the logit scale. This region indicates the range of values that we considered equivalent to zero. We then summarised the posterior distribution of each regression coefficient with the 95% highest density interval (HDI). This interval contains the true value of this coefficient with 95% probability, given the data. Finally, we calculated the proportion of posterior samples in the 95% HDI that fell into the ROPE, noted as $p (upright("ROPE"))$, which indicates the probability that the true value of the regression coefficient falls into the ROPE (and therefore should be considered equivalent to zero). For example, $p (upright("ROPE")) = .80$ indicates that, given our data, there is a 80% probability that the true value of the coefficient falls within the ROPE, and can therefore be considered equivalent to zero.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Overall, the average participant' target looking time exceeded chance levels, as indicated by the fact that the 95% HDI of the intercept term excluded zero ($beta$ = 0.382, 95% HDI = \[0.297, 0.463\]) and all of its posterior samples fell outside of the ROPE. The 95% HDI of the coefficient of #emph[Age] had a positive sign, but did not exclude zero ($beta$ = -0.024, 95% HDI = \[-0.112, 0.053\]), and overlapped completely with the ROPE, indicating that participants from all ages showed equivalent overall target word recognition. The 95% HDI of the contrast of the #emph[Condition] predictor---comparing #emph[Unrelated] and #emph[Related] trials---included zero ($beta$ = 0.097, 95% HDI = \[-0.030, 0.235\]), and 49.05% of its posterior samples fell within the ROPE.

Second, we examined the differences between the priming conditions in the time course of the trial, incorporating the smooth functions of the HGAMs to generate marginal posterior predictions for each condition across for each time point. #link(<fig-epreds-oxf>)[Figure~4] shows the posterior predictions of the model for each condition, and a summary of the difference between the #emph[Unrelated] and #emph[Related] conditions, at each time point to test the practical relevance of these differences, were compared their 95% HDI against the \[-0.1, +0.1\] ROPE. This analysis revealed a similar pattern of results to the previously shown: predicted target looking for the three conditions overlaps across the full time course of the trial.

=== Age and vocabulary size effects
<age-and-vocabulary-size-effects>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
To test our hypotheses regarding the role of age and vocabulary size on the emergence of priming effects, we compared the fit of model $cal(M)_0$ against the fit of other models including the two-way interaction between #emph[Condition] and #emph[Age] ($cal(M)_1$), or the two-way interaction between #emph[Condition] and #emph[Vocabulary] ($cal(M)_2$), and all main effects involved. As with #emph[Age];, the #emph[Vocabulary] predictor was standardised before entering the model. We compared the models using one-out cross-validation (LOO-CV) as a benchmark of model performance, using a Pareto-smoothed importance sampling (PSIS) approximation (#link(<ref-vehtari2017practical>)[Vehtari et al., 2017];). A better performance by models $cal(M)_1$ or $cal(M)_2$ over $cal(M)_0$ would point to #emph[Age] or #emph[Vocabulary];, respectively, playing a substantial role in participants' word-recognition, or on the emergence of priming effects. #link(<tbl-loos-oxf>)[Table~4] shows a summary of the predictive performance of the models, as quantified by the expected log-predictive density (#emph[ELPD];), and its standard error (#emph[SE];, a measure of uncertainty around the #emph[ELPD];). Overall, all models, performed equivalently, as shown by the small difference in #emph[ELPD];, relative to the uncertainty of the estimates. This suggests that participants' target looking during the test phase can be predicted with relative accuracy without taking into account the age or vocabulary size of the participants.

== Discussion
<discussion>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We found strong evidence of successful word recognition across participants of all ages, but we did not observe any evidence of phonological priming. English monolingual participants from all ages showed an equivalent pattern of target looking in both #emph[Related] and #emph[Unrelated] trials. In conclusion, we failed to replicate the original studies by Mani and Plunkett (#link(<ref-mani2010infant>)[2010];) and Mani and Plunkett (#link(<ref-mani2011phonological>)[2011];). The absence of a phonological priming effect suggests that either English monolinguals did not generate implicit labels for the prime pictures presented in silence, or that, if generated, such labels did not interact with the subsequent recognition of the target word. Both explanations conflict with both Mani and Plunkett's studies, and also with previous studies suggesting that infants 12-months and older already generate internal labels when presented with pictures of familiar objects (#link(<ref-duta2012erp>)[Duta et al., 2012];; #link(<ref-styles2015infant>)[Styles et al., 2015];).

Adding the predictors #emph[Age] or #emph[Vocabulary size] as predictors in the model, in interaction with #emph[Condition] did not increase the fit of the model. This points to neither variable having a substantial influence in participants' target looking behaviour across conditions. These results diverge from previous studies reporting an increment in word recognition speed (#link(<ref-fernald1998rapid>)[Fernald et al., 1998];; #link(<ref-marchman2008speed>)[Marchman & Fernald, 2008];), and stronger phonological priming effects in children with larger vocabulary sizes (#link(<ref-avila2021longitudinal>)[Avila-Varela et al., 2021];; #link(<ref-chow2017spoken>)[Chow et al., 2017];; #link(<ref-mani2011phonological>)[Mani & Plunkett, 2011];). Overall, these results suggest that our modification of the implicit naming task resulted in the loss of the originally reported effect.

= Study 2
<study-2>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
The original planning of the present investigation was to run Study 1 first and once the procedure had been validated to start Study 2. However, right at the beginning of data collection, the outbreak of COVID-19 pandemic took place. At this point it was decided to run both experiments in parallel. Data collection at the Barcelona site proceeded at a faster rate than at Oxford. It was not until data collection was well advanced in Barcelona that the results of study 1 were available. This is the reason why Study 2 was run with the same procedure as Experiment 1.

== Methods
<methods-1>
=== Participants
<participants-1>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We collected data from 162 children living in the Metropolitan Area of Barcelona (Spain), tested at the Laboratori de Recerca en Infància at the Universitat Pompeu Fabra. Families were recruited from maternity rooms in private hospitals and social media, and contacted via phone when the child's age spanned between 20 and 32 months. From the 162 children that participated, 81 participated once, 55 participated twice, and 26 participated three times. Recurrent participants were tested with at least 2.06 months of difference. We gathered a total of 269 testing sessions. Participants were divided into monolinguals and bilinguals based on their relative degree of exposure to Catalan and Spanish, estimated using an adaptation of the Language Exposure Questionnaire (LEQ, #link(<ref-bosch2001evidence>)[Bosch & Sebastián-Gallés, 2001];). We categorised participants as monolingual if exposed to more than 80% or more of the time to their dominant language, and as bilingual otherwise. Eighty-three of the participants were categorised as monolinguals (49 female, 34 male) and 80 as Catalan/Spanish bilinguals (34 female, 48 male) (see #link(<tbl-participants>)[Table~1] for a detailed summary of participants' age and language profile). Participants' vision was normal, none used glasses or any other type of vision corrector.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We collected vocabulary data using parental responses to the Barcelona Vocabulary Questionnaire (BVQ, #link(<ref-garcia-castro2023bvq>)[Garcia-Castro et al., 2023];), an online vocabulary checklist developed to assess the vocabulary size of Catalan-Spanish bilingual toddlers, and inspired by several adaptations of the Communicative Developmental Inventory (CDI, #link(<ref-fenson1994variability>)[Fenson et al., 1994];). This questionnaire has four versions, each including a different but overlapping subset of words, from a total pool of 542 words from 26 functional-semantic categories. Each version included a Catalan and a Spanish vocabulary checklist. Catalan checklists contained between 343 and 349 words, and Spanish checklists contained between 349 and 349. Participants were randomly allocated to one of the four versions. Recurrent participants were always allocated to the same version. Families received a link to the BVQ immediately after each experimental session, and were given two weeks to fill it. It is common for children living in the Metropolitan area of Barcelona to be exposed to both Catalan and Spanish in some degree, even monolinguals. For this reason, we collected Catalan #emph[and] Spanish vocabulary data from all participants in Study 2, but for consistency with Study 1, we calculated vocabulary sizes for participants in Study 2 as the number of words that caregivers reported their child to #emph[understand] or #emph[understand and say] only in the dominant language of the child (i.e., the language of test).

One hundred thirty-six (51%) families failed to provide a complete response to the vocabulary checklist within the two-week time limit. We imputed missing vocabulary size scores using single imputation, taking the vocabulary size scores of a pool of 542 additional participants for which a successful response for the questionnaire had been gathered. We used participants' age in months and their language profile (monolingual or bilingual) as predictors. We used the `mice` R package (#link(<ref-vanbuuren2011mice>)[Van Buuren & Groothuis-Oudshoorn, 2011];) to perform imputation using the Bayesian linear regression method (see #strong[?\@sec-appendix-e];).

=== Design
<design-1>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Participants were presented with 32 trials in random order, which belonged to three conditions: #emph[Unrelated] trials (#emph[n] = 16), #emph[Related/Non-cognate] (#emph[n] = 8), and #emph[Related/Cognate] (#emph[n] = 8). In #emph[Unrelated] trials, the target label shared phonological onset with the Catalan and Spanish labels of the prime picture (e.g., prime: /ˈgos/#sub[CAT];--/ˈpe.ro/#sub[SPA] \[dog\], target: /ˈka.za/#sub[CAT] \[house\], for a child tested in Catalan). In #emph[Related/Non-cognate] trials, the target shared phonological onset with the prime label in the test language, but not with the prime label in the other language (e.g., prime: /miˈd͡ʒo/#sub[CAT] (/kal.θeˈtin/#sub[SPA];) \[dog\], target: /muˈnɛ.ðə/#sub[CAT] \[coin\]). In #emph[Related/Cognate] trials, the target shared phonological overlap with both English and Spanish prime labels (e.g., prime: /ˈa.βɾə/#sub[CAT];--/ˈaɾ.bol/#sub[SPA];) \[tree\], target: /əˈβɛ.ʎə/#sub[CAT] \[bee\]).

=== Stimuli
<stimuli-1>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We created six stimuli lists: three in Catalan, and three in Spanish. Lists were created following the same constraints as in Study 1, but now considering the cross-linguistic phonological relationship between Catalan and Spanish. Extracting lexical frequencies from the Catalan and Spanish corpora in the CHILDES database was not possible, given the low number of participants and tokens included. We mapped the English lexical frequencies onto their Catalan and Spanish translation equivalents Garcia-Castro et al. (#link(<ref-garcia2025cognate>)[2025];). The auditory stimuli were natural exemplars of the selected target words, spoken by a proficient female bilingual speaker of Catalan (Central variety) and Castilian Spanish, who was instructed to pronounce each word in a toddler-directed manner. Catalan audios had an average duration of 1,229.84 ms (#emph[SD] = 171.43, #emph[Range] = 860--1,550), and Spanish audios had an average duration of 1,080.47 ms (#emph[SD] = 134.58, #emph[Range] = 830--1,390). New visual stimuli were created to accommodate the words included in the new stimuli lists, and possible cultural differences in the typicality of the exemplars shown in the pictures (see #strong[?\@sec-appendix-d] for a detailed description of the stimuli).

=== Procedure
<procedure-1>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Same as in Study 1. We run the study on Windows 10 64-bit, using custom Matlab (2018a 64-bit) script using the PsychToolbox-3 extension (3.0.15 64-bit) to present the stimuli on a 23-inches screen with 1929 $times$ 1080 resolution, and the Tobii Analytics SDK 3.0 to interact with the eye-tracker (Tobii TX300 and Tobii Pro Sprectrum, Tobii Technology, Stockholm, Sweden) while the experiment was running.

=== Data analysis
<data-analysis-1>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
#strong[Data processing];. We gathered data from 8,608 trials from 269 testing sessions, generated from 162 distinct participants. We excluded trials in which participants failed to provide 50% valid eye-tracking samples (equivalent to 750 ms) during the prime phase (#emph[n] = 1,815) or 50% valid samples (equivalent to 1,000 ms) during the target-distractor phase (#emph[n] = 1,262). We also excluded trials in which participants did not provide at least 10% of valid samples (equivalent to 100 ms) for both the target #emph[and] the distractor (#emph[n] = 2,461).

After excluding trials that matched any of the aforementioned criteria from the dataset, we excluded participants who did not provide at least two valid trials in each experimental condition (#emph[n] = 29), and participants with a dominant-language vocabulary size lower than 10% (which depending on the version of the vocabulary questionnaire they were allocated to, varied from 34 and 37) (#emph[n] = 0). The final dataset included 5,072 trials from 240 testing sessions, generated by 151 distinct participants. Of those participants, 81 provided data from one experimental session, and 51 provided data from two experimental sessions.

#strong[Modelling approach];. We modelled the data following a similar approach as in Study 1, with the main difference that participants' language profile (#emph[Group];) was now included as a predictor in the model, in interaction with the #emph[Condition] predictor. We set two #emph[a priori] contrasts for the #emph[Condition] predictor: one comparing #emph[Unrelated] and #emph[Related/Non-cognate] trials (sum-coded as `-0.5` and `+0.5`, with #emph[Related/Cognate] trials coded as `0`), and another comparing #emph[Related/Non-cognate] and #emph[Related/Cognate] trials (sum-coded as `-0.5` and `+0.5`, with #emph[Unrelated] trials coded as `0`). In Study 2, the base model $cal(M)_0$ included the main effects of #emph[Age];, #emph[Condition];, and #emph[Group];, and the two-way interaction between the #emph[Condition] and #emph[Group] predictors. Contrast coding of the #emph[Condition] predictor was the same as in Study 1. We set one #emph[a priori] contrasts for the #emph[Group] predictor, comparing #emph[Monolingual] with #emph[Bilingual] participants (sum-coded as `-0.5` and `+0.5`, respectively). To model the time course of target looking, we included B-splines for the main effect of #emph[Time];, and for the two-way interaction between #emph[Condition] and #emph[Group];.

#strong[Statistical inference];. Same procedure as in Study 1.

== Results
<results-1>
=== Priming effects
<priming-effects-1>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Overall, the average participants' looking time exceeded chance levels, as indicated by the fact that the 95% HDI of the intercept term excluded zero ($beta$ = 0.214, 95% HDI = \[0.173, 0.256\]), and that all of its posterior samples fell outside of the ROPE. The coefficient of #emph[Age] had a positive sign, but its 95% HDI overlapped completely with the ROPE ($beta$ = 0.024, 95% HDI = \[-0.013, 0.065\]), indicating that participants from all ages showed equivalent overall target word recognition. The 95% HDI of the coefficient of #emph[Group] also included zero ($beta$ = -0.015, 95% HDI = \[-0.103, 0.059\]) and completely overlapped with the ROPE, indicating an equivalent overall target preference in monolinguals and bilinguals,

The 95% HDI of the first contrast of the #emph[Condition] predictor---comparing #emph[Unrelated] and #emph[Related/Non-cognate] trials---included zero ($beta$ = 0.054, 95% HDI = \[-0.026, 0.141\]), and 75.31% of its posterior samples overlapped with the ROPE. The 95% HDI of the second contrast, comparing #emph[Related/Non-cognate] and #emph[Related/Cognate] trials, also included zero ($beta$ = -0.014, 95% HDI = \[-0.113, 0.088\]), and 93.64% of its posterior samples overlapped with the ROPE. The overall target preference was equivalent across both pairwise condition comparisons. The interaction term between the first #emph[Condition] contrast contained zero ($beta$ = -0.010, 95% HDI = \[-0.180, 0.159\]), with 58.99% of its posterior samples overlapping with the ROPE. The interaction term between the second #emph[Condition] contrast also contained zero ($beta$ = 0.090, 95% HDI = \[-0.102, 0.300\]), and 75.31% of its posterior samples fell within the ROPE. The outcomes of this model provide strong evidence against differences between monolinguals and monolinguals, and inconclusive evidence for differences in overall target looking time across conditions.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
An analysis of the time course of target looking revealed a similar pattern of results (see #link(<fig-epreds>)[Figure~6];). Posterior mean prediction for the three conditions overlap across the full time course of the trial in both language groups.

=== Age and vocabulary size effects
<age-and-vocabulary-size-effects-1>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
A comparison between models including #emph[Age] ($cal(M)_1$), #emph[L1 vocabulary] ($cal(M)_2$), and #emph[Total vocabulary] ($cal(M)_3$) against model $cal(M)_0$, which only included #emph[Age] as a main effect is shown in #strong[?\@tbl-loos-bcn];. Overall, all models performed equivalently, with the model $cal(M)_2$ showing slightly better performance. The equivalent performance of all models suggests that participants' target looking during the test phase can be predicted with relative accuracy without taking into account #emph[L1 vocabulary];, or #emph[Total vocabulary] sizes. We now report the median and 95% HDI of the coefficients of $cal(M)_2$, the best-fitting model.

== Discussion
<discussion-1>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Paralleling the results from Study 1 participant' looking behaviour suggested robust word recognition, regardless of experimental condition, participant language profile, age, or vocabulary size. Monolinguals and bilinguals showed equivalent target looking behaviour in #emph[Unrelated];, #emph[Related/Non-cognate];, and #emph[Related/Cognate] trials, suggesting that no phonological priming took place, either within languages or across languages. These results contrast with those of previous studies using a similar paradigm, which reported within-language priming effects in same-aged monolinguals (#link(<ref-avila2021longitudinal>)[Avila-Varela et al., 2021];; #link(<ref-mani2011phonological>)[Mani & Plunkett, 2011];) and younger (#link(<ref-duta2012erp>)[Duta et al., 2012];; #link(<ref-mani2010infant>)[Mani & Plunkett, 2010];; #link(<ref-styles2015infant>)[Styles et al., 2015];), and cross-language priming in adults (#link(<ref-vonholzen2014bilinguals>)[Von Holzen & Mani, 2014];).

We anticipated participants' sensitivity to phonological priming to increase with the size of their lexicon, in the light of previous studies in which the maturation of the lexicon was associated with larger phonological interference in word recognition (#link(<ref-chow2017spoken>)[Chow et al., 2017];; #link(<ref-mani2011phonological>)[Mani & Plunkett, 2011];). In Study 2, incorporating participants' age as a predictor in the model in interaction with the two contrasts of the #emph[Condition] predictor did not increase the predictive performance of the model. Neither did vocabulary size. This suggests that the lack of evidence of phonological priming in participants in this study, either within or across languages, did not depend of participants lexical development status.

= General discussion
<general-discussion>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We investigated the developmental trajectories of cross-language co-activation in the initial lexicon. We tested a large cohort of monolingual and bilingual toddlers in an implicit naming paradigm, in which we designed three experimental conditions to manipulate the phonological overlap between the prime and target words within and across languages. In Unrelated trials, prime and target were phonologically unrelated in both languages. In Related/Non-cognate trials, prime and target labels shared phonological onset only in the dominant language of participants, in which they were tested. In Related/Cognate trials, the prime label was a cognate: prime and target labels shared phonological onset in both languages. In Study 1, we attempted to replicate the original findings by Mani and Plunkett (#link(<ref-mani2010infant>)[2010];) and Mani and Plunkett (#link(<ref-mani2011phonological>)[2011];) in a same-aged English monolingual cohort. We found no evidence of phonological priming. In Study 2, we tested a cohort of monolingual and bilingual infants learning Catalan, Spanish, or both, and found similar results, with no evidence of phonological priming effect in either monolinguals or bilinguals. We did not find any effect of participants' age or vocabulary size.

The lack of priming effects in Studies 1 and 2 contrasts with previous findings of within- and cross-language priming using an implicit naming paradigm. In their seminal study, Mani and Plunkett (#link(<ref-mani2010infant>)[2010];) and Mani and Plunkett (#link(<ref-mani2011phonological>)[2011];) reported within-language priming effects in English monolingual infants. In bilinguals, evidence of cross-linguistic priming in a implicit naming paradigm was available in adults (#link(<ref-vonholzen2014bilinguals>)[Von Holzen & Mani, 2014];). The priming effects shown in these studies reveal that infants and adults retrieve phonologically detailed word-forms when presented with pictures of familiar objects, which later interact with the subsequent auditory recognition of phonologically related words. Evidence of such implicit naming is available in infants as young as 14 months. Electrophysiological evidence reported by Duta et al. (#link(<ref-duta2012erp>)[2012];) and Styles et al. (#link(<ref-styles2015infant>)[2015];) suggests that, at this, age, infants lexicalise name-known pictures presented in silence, and that the generated phonological form is sensitive to subsequent mispronunciations of the word. The possibility that infants in the present investigation failed to retrieve phonological word forms is therefore unlikely.

We consider three scenarios under which implicit naming might have occurred in the experiments presented in this cthis study, but our design failed to capture it. First, it is be possible that infants in both Studies 1 and 2 implicitly generated phonological labels for the primes, but such labels lacked the phonological detail to interact with the subsequent recognition of a phonologically related target word. This is unlikely, given that both monolinguals (#link(<ref-bailey2002phonological>)[Bailey & Plunkett, 2002];; #link(<ref-swingley2000spoken>)[Swingley & Aslin, 2000];; #link(<ref-tamasi2017pupillometry>)[Tamási et al., 2017];) and bilinguals (#link(<ref-ramon-casas2009vowel>)[Ramon-Casas et al., 2009];; #link(<ref-tamasi2016measuring>)[Tamási et al., 2016];) have been shown to encode lexical representations with high phonological detail from early ages.

A second possibility is that participants successfully retrieved a detailed phonological form of the prime labels, but such forms failed to interact with target recognition. This would be explained by the lack of strong associations between phonologically related lexical representations at these ages. But even if one considers the possibility that participants in Study 1 failed to show priming effects for this reason (for instance, the emergence of phonological associations might follow different trajectories in Catalan-Spanish infants, compared to English infants), the fact that English monolingual infants in Study 2 failed to show such priming effects contradicts previous findings on the same population, reporting priming phonological priming effects in even younger infants (#link(<ref-mani2010infant>)[Mani & Plunkett, 2010];, #link(<ref-mani2011phonological>)[2011];).

Third, and most likely, the modifications of the implicit naming task in the present investigation might have reduced the chances of detecting the anticipated effects. The most critical difference between the original design of the implicit naming task by Mani and Plunkett and that of the present study is the absence of a pre-naming phase during the test phase. Target auditory labels were presented immediately after the offset of the prime picture. It is possible that such time interval was too short for participants to retrieve the phonological label of the prime picture before the target was presented. Such failure to generate phonological word-forms for prime labels would have prevented participants in Studies 1 and 2 from being affected by phonological priming effects during target word recognition.

A difference in the difficulty of the stimuli might have influenced the results in the present study, compared to those of the original studies. When designing the stimuli lists, we considered three variables as indices of word difficult during recognition: lexical frequency, age of acquisition, and number of phonemes. The distribution of the three variables was equivalent across the three experimental conditions (see #link(<tbl-stimuli>)[Table~2];), so it is unlikely that such differences cancelled out a possible priming effect. However, the stricter limitations under which we build the stimuli lists, might have lead to out stimuli lists including more difficult words than in the original study by Mani and Plunkett (#link(<ref-mani2010infant>)[2010];). This possibility is unlikely, given that the distribution of lexical frequencies and word familiarity (proportion of children reported to have acquired each word at 18 months, according to OCDI norms) was equivalent for the stimuli lists of Study 1 and those of Mani and Plunkett (#link(<ref-mani2010infant>)[2010];) and Mani and Plunkett (#link(<ref-mani2011phonological>)[2011];). It is therefore unlikely that the lack of priming effects in Study 1 is due to an increased difficulty in the items included.

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Another possibility is that participants in the present study had smaller vocabulary sizes than those of participants in the original studies. This hypothesis is not easy to investigate, as quantitative vocabulary sizes were not reported in original studies. A more recent study by Avila-Varela et al. (#link(<ref-avila2021longitudinal>)[2021];), in which phonological priming effects were found associated with participants vocabulary size, did provide summary statistics for participants vocabulary size scores. This study tested a cohort of monolingual German infants in a word recognition task, in which participants in which participants were presented with auditory primes and targets, which were phonologically related or unrelated. The authors estimated participants' receptive vocabulary sizes using the #emph[Fragebogen zur frühkindlichen Sprachentwicklung] (#link(<ref-szagun2009fragebogen>)[Szagun et al., 2009];), an adaptation of the CDI to German. Their cohort of participants showed receptive vocabulary sizes larger than those of participants in the present study. Participants in Avila-Varela et al. (#link(<ref-avila2021longitudinal>)[2021];) knew an average of 405.24 (#emph[SD] = 96.29) at 21 months and 501.97 (#emph[SD] = 73.41) at 24 months, which contrast with receptive vocabulary sizes of participants in Study 1: 293 at 21 months (#emph[SD] = 71.24), and 304.33 (#emph[SD] = 134.63) at 25 months.

The present study introduces several methodological contributions. First the longitudinal design of the data collection provided a more suitable design for drawing conclusions about developmental change in word recognition trajectories, compared to studies that tested participants from a single age group (#link(<ref-floccia2020translation>)[Floccia et al., 2020];), or studies that with an exclusively cross-sectional design (#link(<ref-von2012language>)[Von Holzen & Mani, 2012b];). Second, the modelling approach in the present study incorporates the double source of repeated measures simultaneously into the random effects structure of the multilevel model. The models in Studies 1 and 2 accounts for the time course of target looking across the trials and its associated autocorrelation using Hierarchical General Additive Models (#link(<ref-barr2008analyzing>)[Barr, 2008];; #link(<ref-pedersen2019hierarchical>)[Pedersen et al., 2019];). The models also incorporate the observations from the same testing session as a nested grouping variable inside participants, so that the test session-level parameters in each model are estimated taking into account the estimations of other testing sessions from the same participants. Overall, this approach allowed us to simultaneously model the participant-level variability, the testing session-level variability, and the time series consisting of participants' gaze behaviour during the task. Finally, the adoption of a Bayesian approach to implement and estimate the model allowed the incorporation of prior knowledge about the distribution of the parameters to generate stable estimates, despite the complexity of the model.

In summary, we aimed to test the language non-selective hypothesis of lexical access in bilingual toddlers using an adaptation of the implicit naming paradigm. This adaptation involved target auditory labels immediately after the offset of prime pictures, instead of presenting the target labels after a baseline period of 2,000 after the offset of the prime pictures. In Study 1, we tested English monolinguals (same population as in the original studies) to establish a baseline to later test bilingual participants. We attempted to replicate the previously reported within-language phonological priming effect. We did not find evidence of such effect, suggesting that our modification of the original task was unsuccessful. Because data collection was conducted simultaneously for Studies 1 and 2, data in Catalan-Spanish monolinguals and bilinguals was available despite the failed replication in Study 1. In Study 2, we also found null pattern of results, in which neither monolinguals nor bilinguals showed evidence of within- or cross-language priming effects. Overall, our results suggest that the change in the timing of the trial disrupted the dynamics of word recognition in such way that priming effects were no longer detectable in our adaptation of the paradigm.

#pagebreak(weak: true)
#figure([
#{set text(font: ("system-ui", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji") , size: 12pt); table(
  columns: (14.29%, 14.29%, 14.29%, 14.29%, 14.29%, 14.29%, 14.29%),
  align: (left,left,left,left,left,left,left,),
  table.header([], table.cell(align: center, colspan: 3, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); ], table.cell(align: center, colspan: 3, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); #block[
    Degree of Exposure (%)
    ]],
    table.cell(align: bottom + left, rowspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); ], table.cell(align: center, colspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); #block[
    Sample size
    ]], table.cell(align: center, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); #block[
    Age (months)
    ]], table.cell(align: center, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); #block[
    Spanish
    ]], table.cell(align: center, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); #block[
    Catalan
    ]], table.cell(align: center, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); #block[
    English
    ]],
    table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Test sessions], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Participants], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); M (SD)], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); M (SD)], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); M (SD)], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); M (SD)],),
  table.hline(),
  table.cell(align: horizon + left, colspan: 7, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Study 1 - Monolingual],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); English dominant], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[89 (21)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[79 (21)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[26.47 (4.05)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[0.00 (0.00)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[0.00 (0.00)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[100.00 (0.00)],
  table.cell(align: horizon + left, colspan: 7, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Study 2 - Monolingual],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Catalan dominant], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[87 (8)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[50 (7)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[25.78 (3.91)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[4.54 (5.98)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[95.11 (6.17)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[0.37 (2.20)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Spanish dominant], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[46 (7)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[28 (6)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[25.18 (3.80)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[90.80 (6.38)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[8.74 (6.40)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.24 (0.85)],
  table.cell(align: horizon + left, colspan: 7, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Study 2 - Bilingual],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Catalan dominant], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[65 (7)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[46 (6)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[25.19 (3.81)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[37.65 (10.47)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[61.98 (10.31)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[0.18 (0.79)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Spanish dominant], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[42 (7)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[31 (7)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[25.58 (3.41)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[61.12 (11.00)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[38.55 (10.42)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[0.38 (1.56)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Total], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(fill: rgb("#333333")); 329], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(fill: rgb("#333333")); 234], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(fill: rgb("#333333")); 25.64], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(fill: rgb("#333333")); ---], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(fill: rgb("#333333")); ---], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(fill: rgb("#333333")); ---],
)}
], caption: figure.caption(
position: top, 
[
Demographic and linguistic profile of testing sessions. The number of excluded testing sessions and participants is indicated between parentheses.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-participants>


#pagebreak(weak: true)
#figure([
#{set text(font: ("system-ui", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji") , size: 12pt); table(
  columns: (14.29%, 14.29%, 14.29%, 14.29%, 14.29%, 14.29%, 14.29%),
  align: (left,left,left,left,left,left,left,),
  table.header(table.cell(align: bottom + left, rowspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); ], table.cell(align: center, colspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); #block[
    \# Phonemes
    ]], table.cell(align: center, colspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); #block[
    Frequency (Zipf)
    ]], table.cell(align: center, colspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); #block[
    Familiarity (%)
    ]],
    table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Prime], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Target], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Prime], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Target], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Prime], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Target],),
  table.hline(),
  table.cell(align: horizon + left, colspan: 7, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Study 1: English],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Cognate], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[5.00 (1.24)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[4.50 (1.34)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[4.63 (0.53)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[4.75 (0.39)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[61.79 (10.04)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[70.28 (15.28)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Non-cognate], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[5.75 (2.72)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.50 (1.34)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.75 (0.18)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.75 (0.39)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[61.43 (12.89)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[70.68 (15.10)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Unrelated], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[5.24 (2.27)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.33 (1.47)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.68 (0.38)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.73 (0.38)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[67.04 (18.36)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[73.35 (16.77)],
  table.cell(align: horizon + left, colspan: 7, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Study 2: Catalan],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Cognate], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[4.50 (1.34)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[4.88 (1.28)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[5.07 (0.33)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[4.83 (0.26)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[85.00 (8.75)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[69.17 (22.67)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Non-cognate], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.88 (1.47)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[5.17 (1.33)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[5.01 (0.37)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.76 (0.25)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[83.33 (10.65)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[70.00 (21.57)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Unrelated], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[5.00 (1.51)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.98 (1.31)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.91 (0.31)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.89 (0.25)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[76.00 (15.25)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[68.33 (22.12)],
  table.cell(align: horizon + left, colspan: 7, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Study 2: Spanish],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Cognate], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[4.50 (0.88)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[6.12 (1.55)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[5.10 (0.31)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[4.77 (0.29)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[64.77 (24.06)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[47.35 (24.45)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Non-cognate], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[5.25 (1.21)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[5.92 (1.54)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.94 (0.42)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.71 (0.26)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[68.18 (21.05)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[50.00 (26.25)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Unrelated], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.62 (1.06)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[5.73 (1.53)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.94 (0.28)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[4.69 (0.23)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[64.20 (22.95)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[45.64 (27.38)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Total], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(style: "italic" , fill: rgb("#333333")); 4.97], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(style: "italic" , fill: rgb("#333333")); 5.12], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(style: "italic" , fill: rgb("#333333")); 4.89], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(style: "italic" , fill: rgb("#333333")); 4.76], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(style: "italic" , fill: rgb("#333333")); 70.20], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(style: "italic" , fill: rgb("#333333")); 62.75],
)}
], caption: figure.caption(
position: top, 
[
Summary of stimuli properties by trial type. Values are summarised using the mean and the standard deviation (between parentheses).
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-stimuli>


#pagebreak(weak: true)
#figure([
#{set text(font: ("system-ui", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji") , size: 12pt); table(
  columns: (20%, 20%, 20%, 20%, 20%),
  align: (left,left,left,left,left,),
  table.header(table.cell(align: bottom + left, rowspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); ], table.cell(align: bottom + right, rowspan: 2, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Participants], table.cell(align: center, colspan: 3, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); #block[
    Trials
    ]],
    table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Unrelated], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Non-cognate], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); Cognate],),
  table.hline(),
  table.cell(align: horizon + left, colspan: 5, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Study 1: English (Oxford)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Monolingual (English)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[89 (21)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[1,011 (733)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[511 (360)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[510 (359)],
  table.cell(align: horizon + left, colspan: 5, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Study 2: Catalan/Spanish (Barcelona)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Bilingual], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[107 (14)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[1,140 (796)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[555 (413)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 1.5pt)))[596 (372)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Monolingual], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[133 (15)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[1,506 (862)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[747 (437)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[741 (443)],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(size: 1.0em , fill: rgb("#333333")); #emph[N];], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(fill: rgb("#333333")); 329], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(fill: rgb("#333333")); 3,657], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(fill: rgb("#333333")); 1,813], table.cell(align: horizon + right, fill: rgb("#ffffff"), stroke: (bottom: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 4.5pt)))[#set text(fill: rgb("#333333")); 1,847],
)}
], caption: figure.caption(
position: top, 
[
Participant-level and trial-level sample size after applying inclusion criteria. The number of excluded participants and trials is indicated between parentheses.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-attrition>


#pagebreak(weak: true)
#figure([
#{set text(font: ("system-ui", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji") , size: 12pt); table(
  columns: 5,
  align: (left,right,right,right,right,),
  table.header(table.cell(align: bottom + left, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); ], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); ELPD], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); SE ELPD], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); ELPD (diff.)], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); SE ELPD (diff)],),
  table.hline(),
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Model 2 (Vocabulary by-condition)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[−8,468.55], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[73.06], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[---], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[---],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Model 0 (Age as main effect)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[−8,468.61], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[73.04], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[−0.06], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[2.03],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Model 1 (Age-by-condition)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[−8,469.04], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[72.97], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[−0.50], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[1.52],
  table.hline(),
  table.footer(table.cell(colspan: 5)[#emph[ELPD];: sum of expected log pointwise predictive density for a new data set. #emph[SE (ELPD)];: standard error of the #emph[ELPD];, which indictes the uncertainty about the predictive performance for unknown future data. #emph[ELPD (diff)];: pairwise difference in #emph[ELPD] for two models. The difference is computed relative to the model with lowest #emph[ELPD] (best fitting model). #emph[SE ELPD (diff.)];: standard error of component-wise differences of #emph[ELPD] between two models.],),
)}
], caption: figure.caption(
position: top, 
[
Leave-one-out cross validation outcomes, comparing the predictive performance of the models in Study 1.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-loos-oxf>


#pagebreak(weak: true)
#figure([
#{set text(font: ("system-ui", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji") , size: 12pt); table(
  columns: 5,
  align: (left,right,right,right,right,),
  table.header(table.cell(align: bottom + left, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); ], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); ELPD], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); SE ELPD], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); ELPD (diff.)], table.cell(align: bottom + right, fill: rgb("#ffffff"))[#set text(size: 1.0em , weight: "regular" , style: "italic" , fill: rgb("#333333")); SE ELPD (diff)],),
  table.hline(),
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Model 1 (Age-by-condition)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[−20,865.97], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[136.67], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[---], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[---],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Model 0], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[−20,867.70], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[136.82], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[−1.73], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[1.94],
  table.cell(align: horizon + left, fill: rgb("#ffffff"), stroke: (right: (paint: rgb("#d3d3d3"), thickness: 1.5pt), top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[#set text(size: 1.0em , fill: rgb("#333333")); Model 0 (Age as main effect)], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[−20,869.06], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[136.74], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[−3.09], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#d3d3d3"), thickness: 0.75pt)))[1.84],
  table.hline(),
  table.footer(table.cell(colspan: 5)[#emph[ELPD];: sum of expected log pointwise predictive density for a new data set. #emph[SE (ELPD)];: standard error of the #emph[ELPD];, which indictes the uncertainty about the predictive performance for unknown future data. #emph[ELPD (diff)];: pairwise difference in #emph[ELPD] for two models. The difference is computed relative to the model with lowest #emph[ELPD] (best fitting model). #emph[SE ELPD (diff.)];: standard error of component-wise differences of #emph[ELPD] between two models.],),
)}
], caption: figure.caption(
position: top, 
[
Leave-one-out cross validation outcomes, comparing the predictive performance of the models in Study 1.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-loos>


#pagebreak(weak: true)
#figure([
], caption: figure.caption(
position: top, 
[
Stimuli lists for each language. The condition to which the prime belonged is indicated between parentheses. #emph[R/C];: Related/Cognate, #emph[R/N];: Related/Non-cognate, #emph[U];: Unrelated.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-stimuli-list>


#pagebreak(weak: true)
#figure([
#box(image("img/hypotheses.png"))
], caption: figure.caption(
position: top, 
[
Predicted priming effects (or their absence) in the #emph[Related/Cognate];, #emph[Related/Non-cognate];, and #emph[Unrelated] conditions, with examples for a participant tested in Catalan. Words represent lexical representations. Lexical representations of the task-relevant language (Catalan) are depicted inside grey boxes. Solid arrows indicate within-language priming effects, and dashed lines indicate cross-language priming effects. In #emph[Related/Cognate] (A) and #emph[Related/Non-cognate] (B) trials, the recognition of the Catalan target word /kuˈʎe.ɾə/ \[spoon\] is predicted to be modulated by the prior activation of the prime label in Catalan. In #emph[Related/Cognate] trials, the parallel activation of the prime label in Spanish is predicted to increase the strength of the priming effect.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-hypotheses>


#pagebreak(weak: true)
#figure([
#box(image("index_files/figure-typst/fig-vocabulary-oxf-1.svg", width: 70.0%))
], caption: figure.caption(
position: top, 
[
Participant receptive vocabulary sizes across ages and language profiles.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-vocabulary-oxf>


#pagebreak(weak: true)
#figure([
#box(image("img/design.png"))
], caption: figure.caption(
position: top, 
[
Experimental task design with examples in Catalan. In each trial, the prime image is presented in silence for 1,500 ms. Then the auditory target label is presented, and finally the target and distractor pictures are presented side-by-side for 2,000 ms. In cognate trials (#emph[n] = 8), Catalan #emph[and] Spanish prime labels shared phonological onset with the target label. In non-cognate trials (#emph[n] = 8), only the Catalan prime label shared phonological onset with the target label. In unrelated trials (#emph[n] = 16), none of the prime labels shared phonological onset with the target label.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-task>


#pagebreak(weak: true)
#figure([
#box(image("index_files/figure-typst/fig-epreds-oxf-1.svg", width: 80.0%))
], caption: figure.caption(
position: top, 
[
A) Posterior mean predictions of the time course of target fixation in the test phase. B) Posterior mean prediction of the time course of the differences in target looking time between conditions. Intervals represent the 95% CrI of the posterior predictions. Lines indicate the mean of the posterior predictions.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-epreds-oxf>


#pagebreak(weak: true)
#figure([
#box(image("index_files/figure-typst/fig-vocabulary-bcn-1.svg"))
], caption: figure.caption(
position: top, 
[
Participant receptive vocabulary sizes across ages and language profiles.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-vocabulary-bcn>


#pagebreak(weak: true)
#figure([
#box(image("index_files/figure-typst/fig-epreds-1.svg"))
], caption: figure.caption(
position: top, 
[
A) Posterior mean predictions of the time course of target fixation in the test phase. B) Posterior mean prediction of the time course of the differences in target looking time between conditions. Intervals represent the 95% CrI of the posterior predictions. Lines indicate the mean of the posterior predictions.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-epreds>


#pagebreak(weak: true)
#figure([
#box(image("index_files/figure-typst/fig-mp-1.svg"))
], caption: figure.caption(
position: top, 
[
Distribution of lexical frequencies and word familiarity at 18 months for the stimuli in Mani and Plunkett (2010, 2011), and in Study 1. A) Lexical frequencies (expressed as Zipf scores) for prime and target words. Lexical frequences are shown for related and unrelated primes separately. B) Word familiarity scores for prime and target words, calculated as the proportion of participants in the OCDI norms that were reported by their caregivers to understand the word. Word familiarity scores are shown for related and unrelated primes separately.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-mp>


#pagebreak(weak: true)
#figure([
#box(image("index_files/figure-typst/fig-vocabulary-imputation-1.svg"))
], caption: figure.caption(
position: top, 
[
Imputation of vocabulary missing data.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-vocabulary-imputation>


#pagebreak(weak: true)
= Appendix 
#counter(figure.where(kind: "quarto-float-fig")).update(0)
#counter(figure.where(kind: "quarto-float-tbl")).update(0)
#appendixcounter.step()
== Imputing vocabulary size scores
<apx-vocab-imputation>
#pagebreak(weak: true)
= Appendix
#counter(figure.where(kind: "quarto-float-fig")).update(0)
#counter(figure.where(kind: "quarto-float-tbl")).update(0)
#appendixcounter.step()
= Vocabulary checklist validity
<apx-vocab-validity>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
To test the validity of caregivers' estimates of word comprehension in the vocabulary questionnaires, we compared participants' target looking for target words reported as acquired, compared to those reported as not acquired. If parents' responses are accurate, target looking preference should be stronger in for acquired words. We conducted this analysis by computing the logit of the probability of target looking at during the presentation of the target and the distractor pictures for each participant across conditions, and then across participants. In #link(<fig-validity-target>)[Figure~A1];, we report the target looking trends in the three groups tested in this study: English monolinguals in Study 1, Catalan and Spanish monolinguals in Study 2, and Catalan-Spanish bilinguals in Study 3.

#figure([
#box(image("index_files/figure-typst/fig-validity-target-1.svg"))
], caption: figure.caption(
position: top, 
[
Target validity
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-validity-target>


= References
<references>
#set par(first-line-indent: 0in, hanging-indent: 0.5in)
#block[
#block[
Agresti, A. (2012). #emph[Categorical data analysis] (Vol. 792). John Wiley & Sons.

] <ref-agresti2012categorical>
#block[
Au, T. K., & Glusman, M. (1990). The principle of mutual exclusivity in word learning: To honor or not to honor? #emph[Child Development];, #emph[61];(5), 1474--1490. #link("https://doi.org/10.2307/1130757")

] <ref-au1990principle>
#block[
Avila-Varela, D. S., Arias-Trejo, N., & Mani, N. (2021). A longitudinal study of the role of vocabulary size in priming effects in early childhood. #emph[Journal of Experimental Child Psychology];, #emph[205];, 105071.

] <ref-avila2021longitudinal>
#block[
Bailey, T. M., & Plunkett, K. (2002). Phonological specificity in early words. #emph[Cognitive Development];, #emph[17];(2), 1265--1282. #link("https://doi.org/10.1016/s0885-2014(02)00116-8")

] <ref-bailey2002phonological>
#block[
Barr, D. J. (2008). Analyzing ‘visual world'eyetracking data using multilevel logistic regression. #emph[Journal of Memory and Language];, #emph[59];(4), 457--474. #link("https://doi.org/10.1016/j.jml.2007.09.002")

] <ref-barr2008analyzing>
#block[
Bergelson, E. (2020). The comprehension boost in early word learning: Older infants are better learners. #emph[Child Development Perspectives];, #emph[14];(3), 142--149. #link("https://doi.org/10.1111/cdep.12373")

] <ref-bergelson2020comprehension>
#block[
Bergelson, E., & Swingley, D. (2012). At 69 months, human infants know the meanings of many common nouns. #emph[Proceedings of the National Academy of Sciences];, #emph[109];(9), 3253--3258. #link("https://doi.org/10.1073/pnas.1113380109")

] <ref-bergelson2012months>
#block[
Bergelson, E., & Swingley, D. (2015). Early word comprehension in infants: Replication and extension. #emph[Language Learning and Development];, #emph[11];(4), 369--380. #link("https://doi.org/10.1080/15475441.2014.979387")

] <ref-bergelson2015early>
#block[
Bialystok, E. (2009). Bilingualism: The good, the bad, and the indifferent. #emph[Bilingualism: Language and Cognition];, #emph[12];(1), 3--11. #link("https://doi.org/10.1017/s1366728908003477")

] <ref-bialystok2009bilingualism>
#block[
Bilson, S., Yoshida, H., Tran, C. D., Woods, E. A., & Hills, T. T. (2015). Semantic facilitation in bilingual first language acquisition. #emph[Cognition];, #emph[140];, 122--134. #link("https://doi.org/10.1016/j.cognition.2015.03.013")

] <ref-bilson2015semantic>
#block[
Blom, E., Boerma, T., Bosma, E., Cornips, L., van den Heuij, K., & Timmermeister, M. (2020). Cross-language distance influences receptive vocabulary outcomes of bilingual children. #emph[First Language];, #emph[40];(2), 151--171. #link("https://doi.org/10.1177/0142723719892794")

] <ref-blom2020crosslanguage>
#block[
Bloom, P. (2002). #emph[How Children Learn the Meanings of Words];. MIT Press.

] <ref-bloom2002how>
#block[
Boersma, P., & Van Heuven, V. (2001). Speak and unSpeak with PRAAT. #emph[Glot International];, #emph[5];(9/10), 341--347.

] <ref-boersma2001speak>
#block[
Bosch, L., & Ramon-Casas, M. (2014). First translation equivalents in bilingual toddlers' expressive vocabulary: Does form similarity matter? #emph[International Journal of Behavioral Development];, #emph[38];(4), 317--322. #link("https://doi.org/10.1177/0165025414532559")

] <ref-bosch2014first>
#block[
Bosch, L., & Sebastián-Gallés, N. (2001). Evidence of early language discrimination abilities in infants from bilingual environments. #emph[Infancy];, #emph[2];(1), 29--49. #link("https://doi.org/10.1207/s15327078in0201_3")

] <ref-bosch2001evidence>
#block[
Bosma, E., Blom, E., Hoekstra, E., & Versloot, A. (2019). A longitudinal study on the gradual cognate facilitation effect in bilingual children's frisian receptive vocabulary. #emph[International Journal of Bilingual Education and Bilingualism];, #emph[22];(4), 371--385. #link("https://doi.org/10.1080/13670050.2016.1254152")

] <ref-bosma2019longitudinal>
#block[
Bosma, E., & Nota, N. (2020). Cognate facilitation in FrisianDutch bilingual children's sentence reading: An eye-tracking study. #emph[Journal of Experimental Child Psychology];, #emph[189];, 104699. #link("https://doi.org/10.1016/j.jecp.2019.104699")

] <ref-bosma2020cognate>
#block[
Brainard, D. H., & Vision, S. (1997). The psychophysics toolbox. #emph[Spatial Vision];, #emph[10];(4), 433--436. #link("https://doi.org/10.1163/156856897x00357")

] <ref-brainard1997psychophysics>
#block[
Bürkner, P.-C. (2017). Brms: An R package for Bayesian multilevel models using Stan. #emph[Journal of Statistical Software];, #emph[80];(1), 1--28. #link("https://doi.org/10.18637/jss.v080.i01")

] <ref-burkner2017brms>
#block[
Byers-Heinlein, K., Gonzalez-Barrero, A. M., Schott, E., & Killam, H. (2023). Sometimes larger, sometimes smaller: Measuring vocabulary in monolingual and bilingual infants and toddlers. #emph[First Language];, #emph[0];(0), 01427237231204167. #link("https://doi.org/10.1177/01427237231204167")

] <ref-byers-heinlein2023sometimes>
#block[
Byers-Heinlein, K., Tsui, A. S. M., Bergmann, C., Black, A. K., Brown, A., Carbajal, M. J., & Wermelinger. (2021). A multilab study of bilingual infants: Exploring the preference for infant-directed speech. #emph[Advances in Methods and Practices in Psychological Science];, #emph[4];(1). #link("https://doi.org/10.1177/2515245920974622")

] <ref-byers-heinlein2021multilab>
#block[
Carpenter, B., Gelman, A., Hoffman, M. D., Lee, D., Goodrich, B., Betancourt, M., Brubaker, M. A., Guo, J., Li, P., & Riddell, A. (2017). Stan: A probabilistic programming language. #emph[Journal of Statistical Software];, #emph[76];. #link("https://doi.org/10.18637/jss.v076.i01")

] <ref-carpenter2017stan>
#block[
Cattani, A., Abbot-Smith, K., Farag, R., Krott, A., Arreckx, F., Dennis, I., & Floccia, C. (2014). How much exposure to English is necessary for a bilingual toddler to perform like a monolingual peer in language tests? #emph[International Journal of Language & Communication Disorders];, #emph[49];(6), 649--671. #link("https://doi.org/10.1111/1460-6984.12082")

] <ref-cattani2014how>
#block[
Chow, J., Davies, A. A., & Plunkett, K. (2017). Spoken-word recognition in 2-year-olds: The tug of war between phonological and semantic activation. #emph[Journal of Memory and Language];, #emph[93];, 104--134.

] <ref-chow2017spoken>
#block[
Costa, A., Caramazza, A., & Sebastian-Galles, N. (2000). The cognate facilitation effect: Implications for models of lexical access. #emph[Journal of Experimental Psychology: Learning, Memory, and Cognition];, #emph[26];(5), 1283. #link("https://doi.org/10.1037/0278-7393.26.5.1283")

] <ref-costa2000cognate>
#block[
Costa, A., & Sebastián-Gallés, N. (2014). How does the bilingual experience sculpt the brain? #emph[Nature Reviews Neuroscience];, #emph[15];(5), 336--345.

] <ref-costa2014does>
#block[
de Groot, A. M. (1992). Determinants of word translation. #emph[Journal of Experimental Psychology: Learning, Memory, and Cognition];, #emph[18];(5). #link("https://doi.org/10.1037/0278-7393.18.5.1001")

] <ref-degroot1992determinants>
#block[
De Houwer, A., Bornstein, M. H., & De Coster, S. (2006). Early understanding of two words for the same thing: A CDI study of lexical comprehension in infant bilinguals. #emph[International Journal of Bilingualism];, #emph[10];(3), 331--347. #link("https://doi.org/10.1177/13670069060100030401")

] <ref-dehouwer2006early>
#block[
De Houwer, A., Bornstein, M. H., & Putnick, D. L. (2014). A bilingualmonolingual comparison of young children's vocabulary size: Evidence from comprehension and production. #emph[Applied Psycholinguistics];, #emph[35];(6), 1189--1211. #link("https://doi.org/10.1017/s0142716412000744")

] <ref-dehouwer2014bilingual>
#block[
Dijkstra, T., Grainger, J., & Heuven, W. J. B. van. (1999). Recognition of cognates and interlingual homographs: The neglected role of phonology. #emph[Journal of Memory and Language];, #emph[41];(4), 496--518. #link("https://doi.org/10.1006/jmla.1999.2654")

] <ref-dijkstra1999recognition>
#block[
Dijkstra, T., Miwa, K., Brummelhuis, B., Sappelli, M., & Baayen, H. (2010). How cross-language similarity and task demands affect cognate recognition. #emph[Journal of Memory and Language];, #emph[62];(3), 284--301.

] <ref-dijkstra2010cross>
#block[
Dufour, R., & Kroll, J. F. (1995). Matching words to concepts in two languages: A test of the concept mediation model of bilingual representation. #emph[Memory & Cognition];, #emph[23];(2), 166--180. #link("https://doi.org/10.3758/bf03197219")

] <ref-dufour1995matching>
#block[
Duta, M., Styles, S., & Plunkett, K. (2012). ERP correlates of unexpected word forms in a pictureword study of infants and adults. #emph[Developmental Cognitive Neuroscience];, #emph[2];(2), 223--234. #link("https://doi.org/10.1016/j.dcn.2012.01.003")

] <ref-duta2012erp>
#block[
Fenson, L., Dale, P. S., Reznick, J. S., Bates, E., Thal, D. J., Pethick, S. J., Tomasello, M., Mervis, C. B., & Stiles, J. (1994). Variability in early communicative development. #emph[Monographs of the Society for Research in Child Development];, #emph[59];(5), 1--185. #link("https://doi.org/10.2307/1166093")

] <ref-fenson1994variability>
#block[
Fernald, A., Pinto, J. P., Swingley, D., Weinberg, A., & McRoberts, G. W. (1998). Rapid gains in speed of verbal processing by infants in the 2nd year. #emph[Psychological Science];, #emph[9];(3), 228--231. #link("https://doi.org/10.1111/1467-9280.00044")

] <ref-fernald1998rapid>
#block[
Fernald, A., Swingley, D., & Pinto, J. P. (2001). When half a word is enough: Infants can recognize spoken words using partial phonetic information. #emph[Child Development];, #emph[72];(4), 1003--1015. #link("https://doi.org/10.1111/1467-8624.00331")

] <ref-fernald2001when>
#block[
Floccia, C., Delle Luche, C., Lepadatu, I., Chow, J., Ratnage, P., & Plunkett, K. (2020). Translation equivalent and cross-language semantic priming in bilingual toddlers. #emph[Journal of Memory and Language];, #emph[112];, 104086. #link("https://doi.org/10.1016/j.jml.2019.104086")

] <ref-floccia2020translation>
#block[
Floccia, C., Sambrook, T. D., Delle Luche, C., Kwok, R., Goslin, J., White, L., Cattani, A., Sullivan, E., Abbot-Smith, K., Krott, A., et al. (2018). Vocabulary of 2-year-olds learning English and an additional language: Norms and effects of linguistic distance. #emph[Monographs of the Society for Research in Child Development];, #emph[83];(1), 7--29. #link("https://doi.org/10.1111/mono.12348")

] <ref-floccia2018vocabulary>
#block[
Fourtassi, A., Bian, Y., & Frank, M. C. (2020). The growth of children's semantic and phonological networks: Insight from 10 languages. #emph[Cognitive Science];, #emph[44];(7), e12847. #link("https://doi.org/10.1111/cogs.12847")

] <ref-fourtassi2020growth>
#block[
Frank, M. C., Braginsky, M., Yurovsky, D., & Marchman, V. A. (2021). #emph[Variability and consistency in early language learning: The wordbank project];. MIT Press.

] <ref-frank2021variability>
#block[
Gampe, A., Quick, A. E., & Daum, M. M. (2021). Does linguistic similarity affect early simultaneous bilingual language acquisition? #emph[Journal of Language Contact];, #emph[13];(3), 482--500. #link("https://doi.org/10.1163/19552629-13030001")

] <ref-gampe2021does>
#block[
Ganger, J., & Brent, M. R. (2004). Reexamining the vocabulary spurt. #emph[Developmental Psychology];, #emph[40];(4), 621. #link("https://doi.org/10.1037/0012-1649.40.4.621")

] <ref-ganger2004reexamining>
#block[
Garcia-Castro, G., Avila-Varela, D. S., Castillejo, I., & Sebastian-Galles, N. (2025). Cognate beginnings to bilingual lexical acquisition. #emph[Child Development];, #emph[96];(1), 286--300.

] <ref-garcia2025cognate>
#block[
Garcia-Castro, G., Ávila-Varela, D. S., & Sebastian-Galles, N. (2023). #emph[Bvq: Barcelona vocabulary questionnaire database and helper functions] \[Computer software\]. #link("https://gongcastro.github.io/bvq")

] <ref-garcia-castro2023bvq>
#block[
Goldfield, B. A., & Reznick, J. S. (1990). Early lexical acquisition: Rate, content, and the vocabulary spurt. #emph[Journal of Child Language];, #emph[17];(1), 171--183. #link("https://doi.org/10.1017/s0305000900013167")

] <ref-goldfield1990early>
#block[
Grosjean, F. (1997). The bilingual individual. #emph[Interpreting];, #emph[2];(1-2), 163--187. #link("https://doi.org/10.1075/intp.2.1-2.07gro")

] <ref-grosjean1997bilingual>
#block[
Hallé, P. A., & de Boysson-Bardies, B. (1994). Emergence of an early receptive lexicon: Infants' recognition of words. #emph[Infant Behavior and Development];, #emph[17];(2), 119--129. #link("https://doi.org/10.1016/0163-6383(94)90047-7")

] <ref-halle1994emergence>
#block[
Hamilton, A., Plunkett, K., & Schafer, G. (2000). Infant vocabulary development assessed with a british communicative development inventory. #emph[Journal of Child Language];, #emph[27];(3), 689--705. #link("https://doi.org/10.1017/s0305000900004414")

] <ref-hamilton2000infant>
#block[
Hoff, E., Core, C., Place, S., Rumiche, R., Señor, M., & Parra, M. (2012). Dual language exposure and early bilingual development. #emph[Journal of Child Language];, #emph[39];(1), 1--27. #link("https://doi.org/10.1017/s0305000910000759")

] <ref-hoff2012dual>
#block[
Hurtado, N., Marchman, V. A., & Fernald, A. (2007). Spoken word recognition by latino children learning Spanish as their first language. #emph[Journal of Child Language];, #emph[34];(2), 227--249. #link("https://doi.org/10.1017/s0305000906007896")

] <ref-hurtado2007spoken>
#block[
Jardak, A., & Byers-Heinlein, K. (2019). Labels or concepts? The development of semantic networks in bilingual two-year-olds. #emph[Child Development];, #emph[90];(2), e212--e229. #link("https://doi.org/10.1111/cdev.13050")

] <ref-jardak2019labels>
#block[
Kleiner, M., Brainard, D., & Pelli, D. (2007). #emph[What's new in psychtoolbox-3?]

] <ref-kleiner2007s>
#block[
Kruschke, J. K., & Liddell, T. M. (2018). The Bayesian new statistics: Hypothesis testing, estimation, meta-analysis, and power analysis from a Bayesian perspective. #emph[Psychonomic Bulletin & Review];, #emph[25];, 178--206. #link("https://doi.org/10.3758/s13423-016-1221-4")

] <ref-kruschke2018bayesian>
#block[
Legacy, J., Zesiger, P., Friend, M., & Poulin-Dubois, D. (2018). Vocabulary size and speed of word recognition in very young FrenchEnglish bilinguals: A longitudinal study. #emph[Bilingualism: Language and Cognition];, #emph[21];(1), 137--149. #link("https://doi.org/10.1017/s1366728916000833")

] <ref-legacy2018vocabulary>
#block[
MacWhinney, B. (2000). #emph[The CHILDES project: The database] (Vol. 2). Psychology Press.

] <ref-macwhinney2000childes>
#block[
Mani, N., Durrant, S., & Floccia, C. (2012). Activation of phonological and semantic codes in toddlers. #emph[Journal of Memory and Language];, #emph[66];(4), 612--622. #link("https://doi.org/10.1016/j.jml.2012.03.003")

] <ref-mani2012activation>
#block[
Mani, N., & Plunkett, K. (2010). In the infant's mind's ear: Evidence for implicit naming in 18-month-olds. #emph[Psychological Science];, #emph[21];(7), 908--913. #link("https://doi.org/10.1177/0956797610373371")

] <ref-mani2010infant>
#block[
Mani, N., & Plunkett, K. (2011). Phonological priming and cohort effects in toddlers. #emph[Cognition];, #emph[121];(2), 196--206. #link("https://doi.org/10.1016/j.cognition.2011.06.013")

] <ref-mani2011phonological>
#block[
Marchman, V. A., & Fernald, A. (2008). Speed of word recognition and vocabulary knowledge in infancy predict cognitive and language outcomes in later childhood. #emph[Developmental Science];, #emph[11];(3), F9--F16. #link("https://doi.org/10.1111/j.1467-7687.2008.00671.x")

] <ref-marchman2008speed>
#block[
Marchman, V. A., Fernald, A., & Hurtado, N. (2010). How vocabulary size in two languages relates to efficiency in spoken word recognition by young spanish--english bilinguals. #emph[Journal of Child Language];, #emph[37];(4), 817--840.

] <ref-marchman2010vocabulary>
#block[
Marian, V., & Spivey, M. (1999). Activation of Russian and english cohorts during bilingual spoken word recognition. #emph[Proceedings of the 21st Annual Conference of the Cognitive Science Society];, 349--354.

] <ref-marian1999activation>
#block[
Mayor, J., & Plunkett, K. (2014). Infant word recognition: Insights from TRACE simulations. #emph[Journal of Memory and Language];, #emph[71];(1), 89--123. #link("https://doi.org/10.1016/j.jml.2013.09.009")

] <ref-mayor2014infant>
#block[
McMurray, B. (2007). Defusing the childhood vocabulary explosion. #emph[Science];, #emph[317];(5838), 631--631. #link("https://doi.org/10.1126/science.1144073")

] <ref-mcmurray2007defusing>
#block[
Mitchell, L., Tsui, R. K., & Byers-Heinlein, K. (2022). #emph[Cognates are advantaged in early bilingual expressive vocabulary development];. PsyArXiv. #link("https://doi.org/10.31234/osf.io/daktp")

] <ref-mitchell2022cognates>
#block[
Parise, E., & Csibra, G. (2012). Electrophysiological evidence for the understanding of maternal speech by 9-month-old infants. #emph[Psychological Science];, #emph[23];(7), 728--733. #link("https://doi.org/10.1177/0956797612438734")

] <ref-parise2012electrophysiological>
#block[
Pearson, B. Z., & Fernández, S. C. (1994). Patterns of interaction in the lexical growth in two languages of bilingual infants and toddlers. #emph[Language Learning];, #emph[44];(4), 617--653. #link("https://doi.org/10.1111/j.1467-1770.1994.tb00633.x")

] <ref-pearson1994patterns>
#block[
Pedersen, E. J., Miller, D. L., Simpson, G. L., & Ross, N. (2019). Hierarchical generalized additive models in ecology: An introduction with mgcv. #emph[PeerJ];, #emph[7];, e6876. #link("https://doi.org/10.7717/peerj.6876")

] <ref-pedersen2019hierarchical>
#block[
Pelli, D. G., & Vision, S. (1997). The VideoToolbox software for visual psychophysics: Transforming numbers into movies. #emph[Spatial Vision];, #emph[10];, 437--442. #link("https://doi.org/10.1163/156856897x00366")

] <ref-pelli1997videotoolbox>
#block[
Poarch, G. J., & Van Hell, J. G. (2012). Cross-language activation in children's speech production: Evidence from second language learners, bilinguals, and trilinguals. #emph[Journal of Experimental Child Psychology];, #emph[111];(3), 419--438. #link("https://doi.org/10.1016/j.jecp.2011.09.008")

] <ref-poarch2012crosslanguage>
#block[
Ramon-Casas, M., Swingley, D., Sebastián-Gallés, N., & Bosch, L. (2009). Vowel categorization during word recognition in bilingual toddlers. #emph[Cognitive Psychology];, #emph[59];(1), 96--121. #link("https://doi.org/10.1016/j.cogpsych.2009.02.002")

] <ref-ramon-casas2009vowel>
#block[
Rocha-Hidalgo, J., & Barr, R. (2023). Defining bilingualism in infancy and toddlerhood: A scoping review. #emph[International Journal of Bilingualism];, #emph[27];(3), 253--274. #link("https://doi.org/10.1177/13670069211069067")

] <ref-rocha-hidalgo2023defining>
#block[
Sanchez, A., Meylan, S. C., Braginsky, M., MacDonald, K. E., Yurovsky, D., & Frank, M. C. (2019). Childes-db: A flexible and reproducible interface to the child language data exchange system. #emph[Behavior Research Methods];, #emph[51];, 1928--1941. #link("https://doi.org/10.3758/s13428-018-1176-7")

] <ref-sanchez2019childesdb>
#block[
Schad, D. J., Vasishth, S., Hohenstein, S., & Kliegl, R. (2020). How to capitalize on a priori contrasts in linear (mixed) models: A tutorial. #emph[Journal of Memory and Language];, #emph[110];, 104038. #link("https://doi.org/10.1016/j.jml.2019.104038")

] <ref-schad2020how>
#block[
Schelletter, C. (2002). The effect of form similarity on bilingual children's lexical development. #emph[Bilingualism: Language and Cognition];, #emph[5];(2), 93--107. #link("https://doi.org/10.1017/s1366728902000214")

] <ref-schelletter2002effect>
#block[
Schwartz, A. I., Kroll, J. F., & Diaz, M. (2007). Reading words in Spanish and English: Mapping orthography to phonology in two languages. #emph[Language and Cognitive Processes];, #emph[22];(1), 106--129. #link("https://doi.org/10.1080/01690960500463920")

] <ref-schwartz2007reading>
#block[
Singh, L. (2014). One world, two languages: Cross-language semantic priming in bilingual toddlers. #emph[Child Development];, #emph[85];(2), 755--766. #link("https://doi.org/10.1111/cdev.12133")

] <ref-singh2014one>
#block[
Spivey, M. J., & Marian, V. (1999). Cross talk between native and second languages: Partial activation of an irrelevant lexicon. #emph[Psychological Science];, #emph[10];(3), 281--284. #link("https://doi.org/10.1111/1467-9280.00151")

] <ref-spivey1999cross>
#block[
Styles, S. J., Plunkett, K., & Duta, M. D. (2015). Infant VEPs reveal neural correlates of implicit naming: Lateralized differences between lexicalized versus name-unknown pictures. #emph[Neuropsychologia];, #emph[77];, 177--184. #link("https://doi.org/10.1016/j.neuropsychologia.2015.07.027")

] <ref-styles2015infant>
#block[
Swingley, D., & Aslin, R. N. (2000). Spoken word recognition and lexical representation in very young children. #emph[Cognition];, #emph[76];(2), 147--166. #link("https://doi.org/10.1016/s0010-0277(00)00081-0")

] <ref-swingley2000spoken>
#block[
Szagun, G., Stumper, B., & Schramm, S. A. (2009). #emph[Fragebogen zur frühkindlichen sprachentwicklung (FRAKIS) und FRAKIS-K (kurzform)];. Pearson Frankfurt.

] <ref-szagun2009fragebogen>
#block[
Tamási, K., McKean, C., Gafos, A., Fritzsche, T., & Höhle, B. (2017). Pupillometry registers toddlers' sensitivity to degrees of mispronunciation. #emph[Journal of Experimental Child Psychology];, #emph[153];, 140--148. #link("https://doi.org/10.1016/j.jecp.2016.07.014")

] <ref-tamasi2017pupillometry>
#block[
Tamási, K., Wewalaarachchi, T. D., Hoehle, B., & Singh, L. (2016). Measuring sensitivity to phonological detail in monolingual and bilingual infants using pupillometry. #emph[Proceedings of the 16th Speech Science and Technology Conference];.

] <ref-tamasi2016measuring>
#block[
Tardif, T., Fletcher, P., Liang, W., Zhang, Z., Kaciroti, N., & Marchman, V. A. (2008). Baby's first 10 words. #emph[Developmental Psychology];, #emph[44];(4), 929. #link("https://doi.org/10.1037/0012-1649.44.4.929")

] <ref-tardif2008baby>
#block[
Thordardottir, E. (2011). The relationship between bilingual exposure and vocabulary development. #emph[International Journal of Bilingualism];, #emph[15];(4), 426--445. #link("https://doi.org/10.1177/1367006911403202")

] <ref-thordardottir2011relationship>
#block[
Tincoff, R., & Jusczyk, P. W. (1999). Some beginnings of word comprehension in 6-month-olds. #emph[Psychological Science];, #emph[10];(2), 172--175. #link("https://doi.org/10.1111/1467-9280.00127")

] <ref-tincoff1999beginnings>
#block[
Tincoff, R., & Jusczyk, P. W. (2012). Six-month-olds comprehend words that refer to parts of the body. #emph[Infancy];, #emph[17];(4), 432--444. #link("https://doi.org/10.1111/j.1532-7078.2011.00084.x")

] <ref-tincoff2012sixmontholds>
#block[
Tsui, R. K.-Y., Gonzalez-Barrero, A. M., Schott, E., & Byers-Heinlein, K. (2022). Are translation equivalents special? Evidence from simulations and empirical data from bilingual infants. #emph[Cognition];, #emph[225];, 105084. #link("https://doi.org/10.1016/j.cognition.2022.105084")

] <ref-tsui2022are>
#block[
Van Buuren, S., & Groothuis-Oudshoorn, K. (2011). Mice: Multivariate imputation by chained equations in R. #emph[Journal of Statistical Software];, #emph[45];, 1--67.

] <ref-vanbuuren2011mice>
#block[
Van Heuven, W. J., Mandera, P., Keuleers, E., & Brysbaert, M. (2014). SUBTLEX-UK: A new and improved word frequency database for British English. #emph[Quarterly Journal of Experimental Psychology];, #emph[67];(6), 1176--1190. #link("https://doi.org/10.1080/17470218.2013.850521")

] <ref-vanheuven2014subtlexuk>
#block[
Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC. #emph[Statistics and Computing];, #emph[27];, 1413--1432. #link("https://doi.org/10.48550/arXiv.1507.04544")

] <ref-vehtari2017practical>
#block[
Vihman, M. (2004). Cross-linguistic experiments in word-form recognition. #emph[The Journal of the Acoustical Society of America];, #emph[115];(5\_Supplement), 2502--2502. #link("https://doi.org/10.1121/1.4783026")

] <ref-vihman2004crosslinguistic>
#block[
Vihman, M., Thierry, G., Lum, J., Keren-Portnoy, T., & Martin, P. (2007). Onset of word form recognition in English, Welsh, and EnglishWelsh bilingual infants. #emph[Applied Psycholinguistics];, #emph[28];(3), 475--493. #link("https://doi.org/10.1017/s0142716407070269")

] <ref-vihman2007onset>
#block[
Von Holzen, K., Fennell, C. T., & Mani, N. (2019). The impact of cross-language phonological overlap on bilingual and monolingual toddlers' word recognition. #emph[Bilingualism: Language and Cognition];, #emph[22];(3), 476--499. #link("https://doi.org/10.1017/s1366728918000597")

] <ref-vonholzen2019impact>
#block[
Von Holzen, K., & Mani, N. (2012b). Language nonselective lexical access in bilingual toddlers. #emph[Journal of Experimental Child Psychology];, #emph[113];(4), 569--586.

] <ref-von2012language>
#block[
Von Holzen, K., & Mani, N. (2012a). Language nonselective lexical access in bilingual toddlers. #emph[Journal of Experimental Child Psychology];, #emph[113];(4), 569--586. #link("https://doi.org/10.1016/j.jecp.2012.08.001")

] <ref-vonholzen2012language>
#block[
Von Holzen, K., & Mani, N. (2014). Bilinguals implicitly name objects in both their languages: An ERP study. #emph[Frontiers in Psychology];, #emph[5];, 1415. #link("https://doi.org/10.3389/fpsyg.2014.01415")

] <ref-vonholzen2014bilinguals>
#block[
Wood, S. N. (2017). #emph[Generalized additive models: An introduction with R];. Chapman and Hall/CRC.

] <ref-wood2017generalized>
#block[
Zettersten, M., Yurovsky, D., Xu, T. L., Uner, S., Tsui, A. S. M., Schneider, R. M., Saleh, A. N., Meylan, S. C., Marchman, V. A., Mankewitz, J., et al. (2023). Peekbank: An open, large-scale repository for developmental eye-tracking data of children's word recognition. #emph[Behavior Research Methods];, #emph[55];(5), 2485--2500. #link("https://doi.org/10.3758/s13428-022-01906-4")

] <ref-zettersten2023peekbank>
#block[
Zipf, G. K. (1945). The meaning-frequency relationship of words. #emph[The Journal of General Psychology];, #emph[33];(2), 251--256.

] <ref-zipf1945meaning>
] <refs>
#set par(first-line-indent: 0.5in, hanging-indent: 0in)


 
  
#set bibliography(style: "../\_extensions/wjschne/apaquarto/apa.csl") 


