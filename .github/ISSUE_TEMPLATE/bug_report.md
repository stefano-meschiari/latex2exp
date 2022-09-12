---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''

---

**Describe the bug**
Please briefly describe your problem and what output you expect. 

**To Reproduce**
1. Paste the output of `sessionInfo()` (this should show, at minimum, the version of R, the platform, and the architecture you are running on)
2. Include a short snippet of code I can use to reproduce the problem
3. If the bug comprises unexpected output from `TeX()`, please paste a screenshot of what you see on your system. You can plot a single TeX expression by running
```r
# example
library(latex2exp)
plot(TeX(r"($\alpha + \beta$)")) # <-- this doesn't render correctly
```

**Expected behavior**
A clear and concise description of what you expected to happen.

**Additional context**
Add any other context about the problem here.
