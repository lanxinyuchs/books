%%
%% This is file 'ericsson.cls',
%% NOT generated with the docstrip utility.  :-)
%% part of  LXA 119 1314.
%%
%% Instructions:
%% =============
%%
%% Use \documentclass{ericsson} and set the header fields with the
%% commands \infotype, \doctype, \file, \docresp, \docprep, \docchk, 
%% \docno, \docrev and \docdate.
%%
%% The value of \docdate is by default set to today's date, but may be
%% overridden. All other fields default to empty.
%%
%% Authors:
%% ========
%%
%% Morten Vadskjær Jensen
%% Morten Heiberg
%% Michael Vittrup Larsen (michael.vittrup.larsen@ericsson.com)
%%
%% Change Log:
%% ===========
%%
%% Version  Date        Author  Change
%% --------------------------------------------------------------------------
%% 0.0      2000-06-01  MVJ     Original .sty file.
%% 0.1      2000-06-12  MHR     Changed original .sty file to a class and 
%%                              remade the header delimiters to be regular 
%%                              tabular delimiters. Uses koma-script article
%%                              class for most of the work. Passes a few
%%                              options to the babel package. There is still
%%                              an annoying underfull hbox hiding somewhere.
%%                              Probably needs lots more work, but is usable. 
%%                              Screw MS Word!
%% 0.2      2000-06-16  MVL     Underfull hbox fixed.
%%          2000-06-16  MHR     Less picky with the versions of required
%%                              packages. Passes a4paper option to
%%                              scrartcl.
%% 0.3      2000-06-17  MHR     A bug fixed that caused the top of the
%%                              Ericsson logo to be cut off when documents
%%                              were converted to PDF. Postscript printing
%%                              was OK, though.
%% 0.4      2001-02-27  MHR     Changed the default fonts to Helvetica and
%%                              Adobe Courier. These generate proper PDF
%%                              files when using ps2pdf. The CM fonts were
%%                              sampled to ugly bitmaps.
%% 0.5      2004-05-19  MVL     Changed thebibliography environment to use
%%                              numbered section heading according to E///
%%                              tradition.
%% 0.6      2004-06-30  MVL     Added twocolumn layout option
%% 0.7      2006-03-06  MVL     Margin section numbering, changed one-column
%%                              margins
%% 0.7.1    2006-10-24  ELINWAL Released from eforge as EriLaTeX 0.7.1.
%% 0.7.2    2007-03-21  QHENGUS Added revision history template and  a
%%                              Ericsson-like tabular environment.
%%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ericsson}  [2004/06/30 v0.6
  Ericsson LaTeX document class]
\newcommand*{\ericsson@enc}{ansinew}
\DeclareOption{ascii}{\renewcommand*{\ericsson@enc}{ascii}}
\DeclareOption{applemac}{\renewcommand*{\ericsson@enc}{applemac}}
\DeclareOption{ansinew}{\renewcommand*{\ericsson@enc}{ansinew}}
\DeclareOption{latin1}{\renewcommand*{\ericsson@enc}{latin1}}
\DeclareOption{cp850}{\renewcommand*{\ericsson@enc}{cp850}}
\DeclareOption{english}{\PassOptionsToPackage{english}{babel}%
  \AtEndOfClass{\main@language{english}}}
\DeclareOption{danish}{\PassOptionsToPackage{danish}{babel}%
  \AtEndOfClass{\main@language{danish}}}
\DeclareOption{swedish}{\PassOptionsToPackage{swedish}{babel}%
  \AtEndOfClass{\main@language{swedish}}}
\DeclareOption{twocolumn}{\PassOptionsToPackage{twocolumn}{scrartcl}%
  \AtEndOfClass{\@twocolumntrue}}
\ExecuteOptions{english}
\ProcessOptions\relax
\RequirePackage[\ericsson@enc]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage{babel}
\RequirePackage[pdftex]{graphicx}
\LoadClass[11pt,a4paper,bibtotocnumbered]{scrartcl}
\RequirePackage{titlesec}
\RequirePackage[nooneline,hang]{caption2}
\sloppy

\AtEndDocument{\label{lastpage}}

%%%% Page layout %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\parindent 0mm
\parskip 2mm
\setlength\headsep{1.8cm}
\setlength\headheight{2cm}
\setlength\topmargin{-1in}
\addtolength\textheight{2cm}
\addtolength{\oddsidemargin}{16mm}
\addtolength{\evensidemargin}{16mm}

%%%% Font setup %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Font for fixed header strings
\DeclareFixedFont{\fhdf}{T1}{phv}{b}{n}{8pt}

% Font for customizable header strings
\DeclareFixedFont{\chdf}{T1}{phv}{m}{n}{11pt}

%\renewcommand{\rmdefault}{ptm}
\renewcommand{\rmdefault}{phv}
\renewcommand{\sfdefault}{phv}
\renewcommand{\ttdefault}{pcr}

%%%% Ericsson document fields %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\infotype}{\def\@einfotype}
\newcommand{\doctype}{\def\@edoctype}
\newcommand{\file}{\def\@efile}
\newcommand{\docresp}{\def\@edocresp}
\newcommand{\docprep}{\def\@edocprep}
\newcommand{\docchk}{\def\@edocchk}
\newcommand{\docno}{\def\@edocno}
\newcommand{\docrev}{\def\@edocrev}
\newcommand{\docdate}{\def\@edate}
\newcommand{\doctitle}[1]{\begin{center}{\LARGE\bf\textsf{#1}}\end{center}}
% Initialize header fields to empty. The user may override any of these.
\infotype{}
\doctype{}
\file{}
\docresp{}
\docprep{}
\docchk{}
\docno{}
\docrev{}
\docdate{\the\year-\two@digits{\the\month}-\two@digits{\the\day}}

%%%% Header setup %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage{calc}
\newlength{\ERICSSON@hdwidth}
\setlength{\ERICSSON@hdwidth}{14cm}
\newlength{\ERICSSON@cw}
\setlength{\ERICSSON@cw}{\ERICSSON@hdwidth/8}
\newlength{\ERICSSON@tcw}
\setlength{\ERICSSON@tcw}{\ERICSSON@hdwidth/12}
\newcounter{ERICSSONhdleftoffset}
\setcounter{ERICSSONhdleftoffset}{27}

\renewcommand{\@oddhead}{%
\setlength{\unitlength}{1mm}%
\begin{picture}(0,0)(\theERICSSONhdleftoffset,0)\scriptsize%
\begin{tabular}{p{2\ERICSSON@cw}p{\ERICSSON@cw}p{\ERICSSON@cw}p{2\ERICSSON@tcw}p{\ERICSSON@tcw}p{\ERICSSON@tcw}p{\ERICSSON@tcw}p{\ERICSSON@tcw}}%
% The next line serves two purposes. It makes the fields be as wide as we
% specified in the tabular declaration. Due to the multicolumns they are
% not fixed width. And it keeps the top of the Ericsson logo within the
% Postscript bounding box for the page. Small hack.
 & & & & & & & \\%
\multicolumn{3}{l}{} & \multicolumn{5}{l}{\chdf\@einfotype}\\%
\multicolumn{3}{l}{\raisebox{4pt}[0cm][0cm]{\includegraphics[width=3.5cm]{elogo}}} & \multicolumn{4}{l}{\chdf\makebox[0pt][l]{\MakeUppercase{\@edoctype}}} & \multicolumn{1}{r}{\chdf\thepage (\pageref{lastpage})}\\\hline%
\multicolumn{3}{|l|}{\fhdf Prepared (also subject responsible, if other)} & \multicolumn{1}{l}{\fhdf No.} & \multicolumn{2}{l}{} & \multicolumn{2}{l}{}\\%
\multicolumn{3}{|l|}{\chdf\@edocprep} & \multicolumn{4}{l}{\chdf\@edocno}\\\hline%
\multicolumn{2}{|l|}{\fhdf Document responsible/Approved} & \multicolumn{1}{l|}{\fhdf Checked} & \multicolumn{1}{l|}{\fhdf Date} & \multicolumn{1}{l|}{\fhdf Rev.} & \multicolumn{2}{l}{\fhdf File}\\%
\multicolumn{2}{|l}{\chdf\@edocresp} & \multicolumn{1}{l|}{\chdf\@edocchk} & \multicolumn{1}{l}{\chdf\@edate} & \multicolumn{1}{l|}{\chdf\@edocrev} & \multicolumn{2}{l}{\chdf\@efile}\\\hline%
% The next line makes the fields be as wide as we specified in the tabular
% declaration. Due to the multicolumns they are not fixed width. Small hack.
\end{tabular}%
\end{picture}%
\hfill
}
\renewcommand{\@evenhead}{\@oddhead}
\renewcommand{\@oddfoot}{\@empty}
\renewcommand{\@oddfoot}{\@evenfoot}

%%%% Add another section level %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\setcounter{secnumdepth}{4}
\setcounter{tocdepth}{3}
\newcommand{\subsubsubsection}[1]{\paragraph{#1}}

%%%% Margin ection numbering %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\if@twocolumn
%\typeout{111111111111111111111111111111}
\else
%\typeout{222222222222222222222222222222}
\titleformat{\chapter}[display]
  {\normalfont\huge\bfseries}{\chaptertitlename\ \thechapter}{20pt}{\Huge}
\titleformat{\section}
  {\normalfont\Large\bfseries}{\makebox[0cm]{\makebox[2.6cm][l]{\thesection}}}{0em}{}
\titleformat{\subsection}
  {\normalfont\large\bfseries}{\makebox[0cm]{\makebox[2.6cm][l]{\thesubsection}}}{0em}{}
\titleformat{\subsubsection}
  {\normalfont\normalsize\bfseries}{\makebox[0cm]{\makebox[2.6cm][l]{\thesubsubsection}}}{0em}{}
\titleformat{\paragraph}[runin]
  {\normalfont\normalsize\bfseries}{\makebox[0cm]{\makebox[2.6cm][l]{\theparagraph}}}{0em}{}
\titleformat{\subparagraph}[runin]
  {\normalfont\normalsize\bfseries}{\makebox[0cm]{\makebox[2.6cm][l]{\thesubparagraph}}}{0em}{}
\fi

%%%% Float setup %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Allow laaaarge floats before creating a float page
\renewcommand\floatpagefraction{.9}
\renewcommand\topfraction{.9}
\renewcommand\bottomfraction{.9}
\renewcommand\textfraction{.1}   
\setcounter{totalnumber}{50}
\setcounter{topnumber}{50}
\setcounter{bottomnumber}{50}

\renewcommand{\captionfont}{\sl\small}
\renewcommand{\captionlabelfont}{\bf}

\newcommand{\normalfigure}[3]{% \normalfigure{filename}{caption}{width}
  \begin{figure}
  \centering
  \resizebox{(\textwidth/100)*#3}{!}{\input{#1.pdftex_t}}%
  \caption{#2}%
  \label{fig:#1}%
  \end{figure}}

\newcommand{\widefigure}[3]{% \widefigure{filename}{caption}{width-in-percent}
  \begin{figure}[bp]
    \hspace{-\oddsidemargin}\parbox[c]{\textwidth+\oddsidemargin+5mm}{%
      \resizebox{((\textwidth+\oddsidemargin+5mm)/100)*#3}{!}{\input{#1.pdftex_t}}%
      \caption{#2}%
      \label{fig:#1}}%
  \end{figure}}

%%%% Ericsson style table %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newenvironment{eritabular}[1]
  {\medskip\begin{center}\begin{tabular}{#1}\hline}
  {\hline\end{tabular}\end{center}\medskip}

%%%% Revision history environment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newenvironment{dochistory}
  {\begin{eritabular}{|l|l|p{0.6\textwidth}|}{\bf Revision} & {\bf Date} & {\bf Changes since prior revision}\\\hline}
  {\end{eritabular}}

\endinput
