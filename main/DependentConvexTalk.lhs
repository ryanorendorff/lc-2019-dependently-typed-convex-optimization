%if False

> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE DataKinds              #-}
>
> module DependentConvexTalk where
>

%endif

\documentclass{beamer}

\usepackage[backend=biber,citestyle=authoryear,style=authoryear]{biblatex}
\bibliography{bib.bib}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Beamer Style (based on Berkeley colors)  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\usetheme{metropolis}

\definecolor{BerkeleyBlue}{RGB}{0,50,98}
\definecolor{FoundersRock}{RGB}{59,126,161}
\definecolor{Medalist}{RGB}{196,130,14}

\setbeamercolor{frametitle}{fg=white,bg=FoundersRock}
\setbeamercolor{title separator}{fg=Medalist,bg=white}


\usefonttheme[onlymath]{serif}


\usepackage{amsmath}
\usepackage{amsthm}
\usepackage{textcomp}
\usepackage{amssymb}

\usepackage{hyperref}

\usepackage{appendixnumberbeamer}

\usepackage[utf8]{inputenc}
\usepackage{pmboxdraw}

\usepackage{fancyvrb}
\usepackage{xcolor}

\usepackage{mathpartir}
\usepackage{fontspec}
\usepackage{cmll}
\usepackage{unicode-math}

\usepackage[plain]{fancyref}

%% The frownie face
\usepackage{wasysym}

\usepackage{minted}

%% Footnotes without an accompanying numerical binding.
\newcommand\blfootnote[1]{%
  \begingroup
  \renewcommand\thefootnote{}\footnote{#1}%
  \addtocounter{footnote}{-1}%
  \endgroup
}

\newcommand{\m}[1]{$#1$}

\newcommand{\novspacepause}{\vspace*{-\baselineskip}\pause}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lhs2tex formatting rules                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%include polycode.fmt
%include beamer.fmt
%include forall.fmt
%format .         = ". "
%format _ (a)         = "_{" a "}"
%format ω = "\omega"
%format π = "\pi"

\author{Ryan~Orendorff}
\title{Using Dependent Types in Mathematical Optimization Algorithms}
\subtitle{\verb|github.com/ryanorendorff/lc-2019-dependently-typed-convex-optimization|}
\date{June 7th, 2019}
\hypersetup{pdflang={English}}


\begin{document}

\frame{\titlepage}


\begin{frame}
\frametitle{Outline}
\tableofcontents[]
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Motivation                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For the first section, do not highlight the current section (overview slide)
\AtBeginSection[] {}
\section{Motivation: Properly handling a resource}

% For all subsequent sections, highlight what section we are currently in.
\AtBeginSection[]
{
  \begin{frame}
    \frametitle{Table of Contents}
    \tableofcontents[currentsection]
  \end{frame}
}


\begin{frame}{What challenge are we solving?}

In this talk we will be using the KnownNat construction in Haskell to type our
linear algebra.

\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%
% Section of some sort %
%%%%%%%%%%%%%%%%%%%%%%%%

\section{A section}

\begin{frame}{A slide}

Something

\end{frame}


\begin{frame}{Code example}

Code!

> a :: Int
> a = 4

\end{frame}

\end{document}
