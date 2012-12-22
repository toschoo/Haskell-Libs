
Readme: LaTeX source for Introduction to Mathematical Philosophy

I apologize in advance for the mess. If I had it to do over again, I would
have used fewer files, and more use of the LaTeX ifthen package.


The LaTeX souce for creating the six PDFs includes the following files:

imp-core.tex: This is the core of the book. It is not directly compiliable 
              but is called by the others through the \input command.

(The other files are generally "wrappers" for setting the individual
parameters for the different sizes. Compile them with pdflatex to 
generate a version of IMP. Of these, only imp.tex is properly annotated,
though the rest may be intelligible by comparing it thereto.)

imp.tex: wrapper for the single-sided Letter paper version.
impx.tex: wrapper for creating single pages of the two-sided Letter paper
          version; compile this before impx2.tex.
impx2.tex: compile this to combine the results of compiling impx.tex into
           a two-page letter version; compile impx.tex first.
imp-a4.tex: wrapper for the single-sided A4 paper version.
imp-a4x.tex: wrapper for creating single pages of the two-sided A4 version;
             compile this before imp-a4x2.tex.
imp-a4x2.tex: compile this to combine the resultings of compiling 
              imp-a4x.tex into a two-page A4 version; compile imp-a4x.tex 
              first.
imp-ebk.tex: wrapper for 6" e-ink screen ebook version
imp-iphone.tex: wrapper for iPhone version.
reader.sty: style file used by imp-ebk.tex to set its page attributes.
            (if I had to do it over, I'd use the geometry package)
CoverDesign.jpg: cover image for electronic versions.

VERSION HISTORY:
1.0: initial stable release

LEGAL
Bertrand Russell's Introduction to Mathematical Philosophy is in the
Public Domain.
See http://creativecommons.org/licenses/publicdomain/

This typesetting (including LaTeX code) and list of changes are licensed
under a Creative Commons Attribution-Share Alike 3.0 United
States License.
See http://creativecommons.org/licenses/by-sa/3.0/us/
