# non-UK RASFFs

## Background
The Rapid Alert System for Food and Feed (RASFF) consists of alerts raised by several countries. While some of these alerts are either raised by the UK, or list it as a country at risk, many do not (non-UK RASFFs). This project began with the idea that non-UK RASFFs may still be informative of imminent UK issues.

The project was a collaboration with experts, who use the RASFF system in their daily work, and data scientists. A Bayesian network was built which linked to RASFFs to the probability for a UK RASFF in the near future. An [interactive dashboard]( https://foodstandards.shinyapps.io/nonuk_rasffs/) was then prototyped. 


## Technologies
- R 3.6.0
    - Packages: [Tidyverse](https://www.tidyverse.org/), [bnlearn](http://www.bnlearn.com/), [Shiny](https://shiny.rstudio.com/) 


# Licence

MIT License

Copyright (c) 2019 Food Standards Agency

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
