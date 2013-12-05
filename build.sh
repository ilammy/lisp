#!/bin/bash

JOBNAME='lisp'
MAIN_FILE='!main.tex'
FIGURES_FILE='!figures_only'
INDEX_FILES='lisp-eng.idx lisp.idx'
LATEX_KEYS="-jobname=$JOBNAME -interaction=batchmode"
ROOT=$PWD

function prepare_pdf {
    echo "== Preparing PDF =="
    cd $ROOT/book-src

    if command -v git >/dev/null 2>&1
    then
        git log -1 --format='\newcommand{\GitCommit}{%H}
\newcommand{\GitCommitDate}{%ci}' > git-version.tex
    fi

    pdflatex $LATEX_KEYS $MAIN_FILE
    python $ROOT/makeindex.py $INDEX_FILES > $JOBNAME.ind
    pdflatex $LATEX_KEYS $MAIN_FILE
    python $ROOT/makeindex.py $INDEX_FILES > $JOBNAME.ind
    pdflatex $LATEX_KEYS $MAIN_FILE

    echo "== Done =="
    mv $JOBNAME.pdf $ROOT
    rm -f *.aux *.idx *.ind *.log *.out *.toc git-version.tex
}

function prepare_epub {
    mkdir $ROOT/epub/OEBPS/images

    echo "== Rendering images =="
    cd $ROOT/book-src

    pdflatex $LATEX_KEYS $FIGURES_FILE
    pdflatex $LATEX_KEYS $FIGURES_FILE

    mv $JOBNAME.pdf $ROOT/epub/OEBPS/images
    rm *.aux *.log *.out

    echo "== Converting to PNG =="
    cd $ROOT/epub/OEBPS/images

    convert -monitor -density 300 $JOBNAME.pdf -trim -quality 90 png%02d.png
    rm $JOBNAME.pdf

    echo "== Packaging EPUB =="
    cd $ROOT/epub

    for size in `ls $ROOT/epub/fontconfig`
    do
        echo " - ${size%.css}"
        cp $ROOT/epub/fontconfig/$size $ROOT/epub/OEBPS/zz_fontsize.css
        zip --quiet --recurse-paths --compression-method deflate \
            --unicode UTF8 $JOBNAME-${size%.css}.epub mimetype META-INF OEBPS
    done

    echo "== Done =="
    mv *.epub $ROOT
    rm -rf $ROOT/epub/OEBPS/images
    rm -f $ROOT/epub/OEBPS/zz_fontsize.css
}

function print_usage {
    echo "Usage: $0 [epub | pdf]"
}

if [ $# -gt 1 ]
then
    print_usage
    exit 1
fi

case "$1" in
    epub)
        prepare_epub
        ;;

    pdf|"")
        prepare_pdf
        ;;

    *)
        print_usage
        exit 1
esac
