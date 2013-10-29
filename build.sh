#!/bin/sh

JOBNAME='lisp'
MAIN_FILE='!main.tex'
INDEX_FILES='lisp-eng.idx lisp.idx'
LATEX_KEYS="-jobname=$JOBNAME -interaction=batchmode"

cd book-src

pdflatex $LATEX_KEYS $MAIN_FILE
python ../makeindex.py $INDEX_FILES > $JOBNAME.ind
pdflatex $LATEX_KEYS $MAIN_FILE
python ../makeindex.py $INDEX_FILES > $JOBNAME.ind
pdflatex $LATEX_KEYS $MAIN_FILE

mv $JOBNAME.pdf ..
rm *.aux *.idx *.ind *.log *.out *.toc
