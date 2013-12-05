@echo off
setlocal

set JOBNAME=lisp
set MAIN_FILE=!main.tex
set FIGURES_FILE=!figures_only.tex
set INDEX_FILES=lisp-eng.idx lisp.idx
set LATEX_KEYS=-job-name=%JOBNAME% -interaction=batchmode
set ROOT=%CD%

if "%1"=="pdf"  goto :prepare_pdf
if "%1"==""     goto :prepare_pdf
if "%1"=="epub" goto :prepare_epub

echo Usage: %0 [epub ^| pdf]
exit /b 1

:prepare_pdf
    echo == Preparing PDF ==
    cd %ROOT%\book-src

    git 2> NUL
    if not %ERRORLEVEL%==9009 (
        git log -1 --format='\newcommand{\GitCommit}{%H}
\newcommand{\GitCommitDate}{%ci}' > git-version.tex
    )

    pdflatex %LATEX_KEYS% %MAIN_FILE%
    python %ROOT%\makeindex.py %INDEX_FILES% > %JOBNAME%.ind
    pdflatex %LATEX_KEYS% %MAIN_FILE%
    python %ROOT%\makeindex.py %INDEX_FILES% > %JOBNAME%.ind
    pdflatex %LATEX_KEYS% %MAIN_FILE%

    echo == Done ==
    move %JOBNAME%.pdf %ROOT%
    del /q *.aux *.idx *.ind *.log *.out *.toc git-version.tex
goto:eof

:prepare_epub
    mkdir %ROOT%\epub\OEBPS\images

    echo == Rendering images ==
    cd %ROOT%\book-src

    pdflatex %LATEX_KEYS% %FIGURES_FILE%
    pdflatex %LATEX_KEYS% %FIGURES_FILE%

    move %JOBNAME%.pdf %ROOT%\epub\OEBPS\images
    del *.aux *.log *.out

    echo == Converting to PNG ==
    cd %ROOT%\epub\OEBPS\images

    convert -monitor -density 300 %JOBNAME%.pdf -trim -quality 90 png%%02d.png
    del %JOBNAME%.pdf

    echo == Packaging EPUB ==
    cd %ROOT%\epub

    for /f %%a in ('dir /B %ROOT%\epub\fontconfig') do (
        echo  - %%~na
        copy /y %ROOT%\epub\fontconfig\%%a %ROOT%\epub\OEBPS\zz_fontsize.css
        zip --quiet --recurse-paths --compression-method deflate ^
            --unicode UTF8 %JOBNAME%-%%~na.epub mimetype META-INF OEBPS
    )

    echo == Done ==
    move *.epub %ROOT%
    del /q %ROOT%\epub\OEBPS\images
    del /q %ROOT%\epub\OEBPS\zz_fontsize.css
    rmdir %ROOT%\epub\OEBPS\images
goto:eof
