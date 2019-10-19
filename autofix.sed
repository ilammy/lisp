s/ "--- / — /g
s/^"--- / — /g
s/ "---$/ —/g
s/<</«/g
s/>>/»/g
s/\. /.\
/g
s/"~/‑/g
s/"=/-/g
s/%[% ]*(.*)/<!-- \1 -->/g
s/~/ /g
s/т\.\\,д\./т. д./g
s/\\cite\{([a-z0-9]*)\}/[[\1]](zz_bibliography.html#\1)/g
s/\\chapter\{([^{}]*)\}\\label\{([^{}]*)\}/<a id="\2"><\/a>\
# \1/g
s/\\section(\*)?\{([^{}]*)\}\\label\{([^{}]*)\}/<a id="\3"><\/a>\
## \2/g
s/\\textit\{([^{}]*)\}/*\1*/g
s/\\ii\{([^{}]*)\}/*\1*/g
s/\\emph\{([^{}]*)\}/*\1*/g
s/\\term\{([^{}]*)\}/_\1_/g
s/\\ic\{([^{}]*)\}/`\1`/g
s/\\begin\{code:lisp\}/```scheme/g
s/\\end\{code:lisp\}/```/g
s/\\indexR\{([^{}]*)\}/<a id="indexR:\1"><\/a>/g
s/\\indexC\{([^{}]*)\}/<a id="indexC:\1"><\/a>/g
s/\\indexE\{([^{}]*)\}/<a id="indexE:\1"><\/a>/g
s/\\indexCS\{([^{}]*)\}\{([^{}]*)\}/<a id="indexC:\1!\2"><\/a>/g
s/\\indexC\*\{([^{}]*)\}\{([^{}]*)\}/<a id="indexC:\1"><\/a>/g
s/\\pageref\{([^{}]*)\}/[\1](xxx_appropriate.html#\1)/g
