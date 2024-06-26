# Stable long-term individual variation in chimpanzee technological efficiency

### Berdugo, S., Cohen, E., Davis, A. J., Matsuzawa, T., & Carvalho, S.

## Abstract
Using tools to access hard-to-reach and high-quality resources, such as termites, honey, and nuts, initiated a fundamental adaptive shift in human and nonhuman primate cognitive and behavioural evolution. Variation in the efficiency of extracting calorie-rich and nutrient-dense resources directly impacts energy expenditure, and potentially has significant repercussions for cultural transmission where model selection biases are employed during skill acquisition. Assessing variation in efficiency is key to understanding the evolution of complex behavioural traits in primates. Yet, individual-level differences beyond age- and sex-class in primate extractive foraging efficiency have never been investigated. Here, we used 25 years (1992–2017) of video data of the Bossou chimpanzee community (Guinea), to investigate whether individual differences in nut-cracking efficiency exist across the life span of chimpanzees aged ≥ 6 years. Data from 3,882 oil-palm nut-cracking bouts from over 800 hours of observation were collected. We found long-term stable and reliable individual differences in four (out of five) measures of efficiency. We found no sex effect, challenging previous research on a female bias in chimpanzee tool use. These life-long differences in extractive foraging impacts daily energy budgets, which potentially have significant individual fitness and life history consequences. Additionally, the establishment of long-term individual variation in chimpanzee stone tool use has implications for interpreting archaeological records of hominins. Our findings highlight the importance of longitudinal data from long-term field sites when investigating underlying cognitive and behavioural diversity across individual lifespans and between populations. 

**Please cite:**
`Berdugo, S., Cohen, E., Davis, A. J., Matsuzawa, T., & Carvalho, S. (2023). Stable long-term individual variation in chimpanzee technological efficiency. BioRxiv, 2023.11.21.568000. https://doi.org/10.1101/2023.11.21.568000`

## Repository contents

The *chimpanzee_nut_cracking_efficiency* repository contains the `code` and `data` for the above pre-print: 

### `code`
- `video_selection.R`: The data collection protocol code used to randomly select videos from the Bossou video archive.
- `clean_all_efficiency_data.R`: The data cleaning code used on the raw data.
- `efficiency_inference.R`: The data analysis code, including code for plotting and model assumption checks.

### `data`
- `minimum_data.csv`: A subset of data used in analysis.
- `1995_subject_UIDs.csv`: The video unique identifier (UID) codes for the 1995 field season used to randomly sample footage for data collection.
