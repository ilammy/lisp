@echo off
setlocal

set JOBNAME=lisp
set MAIN_FILE=!main.tex
set INDEX_FILES=lisp-eng.idx lisp.idx
set LATEX_KEYS=-job-name=%JOBNAME% -interaction=batchmode

cd book-src

pdflatex %LATEX_KEYS% %MAIN_FILE%
python ..\makeindex.py %INDEX_FILES% > %JOBNAME%.ind
pdflatex %LATEX_KEYS% %MAIN_FILE%
python ..\makeindex.py %INDEX_FILES% > %JOBNAME%.ind
pdflatex %LATEX_KEYS% %MAIN_FILE%

move %JOBNAME%.pdf ..
del *.aux *.idx *.ind *.log *.out *.toc
