%if False

> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE TypeApplications       #-}
> {-# LANGUAGE DataKinds              #-}
> {-# LANGUAGE KindSignatures         #-}
>
> -- Allows us to infer additional constraints from the definition
> -- Instead of (KnownNat n, KnownNat (n+2)) =>
> -- we only need (KnownNat n) =>
> {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
>
> -- Allows us to equate terms like (a + b) + c and a + (b + c) as the same thing.
> {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
> module DependentConvexTalk where
>
> import GHC.TypeLits
> import qualified Numeric.LinearAlgebra as LA
> import Numeric.LinearAlgebra.Static
>
> lmatrix = LA.matrix
> lvector = LA.vector
> linv = LA.inv
> (#>>) = (LA.#>)
>
> sq = (^2)
>
> type Vector = LA.Vector
> type RDouble = Double

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
% TODO: do I need this? format .         = ". "
%format _ (a)         = "_{" a "}"
%format _ (a)         = "_{" a "}"
%format ω = "\omega"
%format π = "\pi"
%format L m n = "\mathbb{R}^{" m " \times " n "}"
%format R n = "\mathbb{R}^{" n "}"
%format RDouble = "\mathbb{R} "
%format c2d = "c_2d"
%format c2d_static = "c_2d_{\mathbb{N}}"

% TODO: FIGURE OUT HOW TO REMOVE THE DOT.
%  format .  = " "
%  format LA = " "
%format #> = "\ \#\rangle\, "
%format #>> = "\ \#\rangle\, "
%format lmatrix = "matrix "
%format lvector = "vector "
%format linv = "inv "
%format pi = "\pi "
%format sq (a) = a "^{2} "
%format Rs = "\Varid{R}"
%format Rst = R


\author{Ryan~Orendorff}
\title{Using Dependent Types in Mathematical Optimization Algorithms}
\subtitle{\verb|github.com/ryanorendorff/|\\\verb|lc-2019-dependently-typed-convex-optimization|}
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
\section{Example problem: how to reconstruct an inverse problem}

%if False

> i_0 :: Double
> i_0 = 1e-12
>
> db_to_w_per_m2 :: Double -> Double
> db_to_w_per_m2 d = i_0 * 10 ** (d/10)
>
> w_per_m2_to_db :: Double -> Double
> w_per_m2_to_db i = 10 * logBase 10 (i/i_0)

%endif

\begin{frame}{Measuring sound power}

  Say Bob and Alice want to measure how much power two speakers are putting out
  using a decibel meter program on their phone.

  \begin{center}
    \includegraphics[]{fig/sound-setup.pdf}
  \end{center}

  This system is described by

  \begin{itemize}
    \item Two sources of sound emitting with power $p_1$ and $p_2$, and
    \item Two measurements: Alice ($m_1$) and Bob ($m_2$)
  \end{itemize}

\end{frame}

\begin{frame}{Model of how sound propagates}

  For a speaker outputting with power $p$, we can describe the intensity of the
  measurement $m$ (in $W/m^2$) with the following equation.

  \begin{equation*}
    m = \frac{p}{4 \pi r^2}
  \end{equation*}

  \pause

  We can write what Alice ($m_1$)and Bob ($m_2$) will measure as follows.

  \begin{align*}
    \frac{p_1}{4 \pi r_{m_1\text{-to-}p_1}^2} +  \frac{p_2}{4 \pi r_{m_1\text{-to-}p_2}^2}  & = m_1 \\
    \frac{p_1}{4 \pi r_{m_2\text{-to-}p_1}^2} +  \frac{p_2}{4 \pi r_{m_2\text{-to-}p_2}^2}  & = m_2
  \end{align*}

\end{frame}

\begin{frame}{Model of how sound propagates using Linear Algebra}

  We can rewrite our equations into the following matrix form.

  \begin{align*}
    &
    \begin{bmatrix}
      \frac{1}{4 \pi r_{m_1\text{-to-}p_1}^2} & \frac{1}{4 \pi r_{m_1\text{-to-}p_2}^2} \\
      \frac{1}{4 \pi r_{m_2\text{-to-}p_1}^2} & \frac{1}{4 \pi r_{m_2\text{-to-}p_2}^2}
    \end{bmatrix} &
    \begin{bmatrix}
      p_1 \\ p_2
    \end{bmatrix} & =
    \begin{bmatrix}
      m_1 \\ m_2
    \end{bmatrix} & \\
    & S & p & = m &
  \end{align*}

  where

  \begin{itemize}
    \item $S$ is a \emph{model} of how the sound propagates, of shape $\mathbb{R}^{2\times2}$
    \item $p$ is the vector of sound powers, of size $\mathbb{R}^2$.
    \item $m$ is the vector of measurements, of size $\mathbb{R}^2$.
  \end{itemize}

\end{frame}

\begin{frame}{Forward model based reconstruction}
  In general, we can solve this using a forward model based approach, which
  aims to solve the problem

  \begin{align*}
    % I think we need to double these up because of lhs2TeX interpreting them
    % as the inline code environment.
    \text{minimize}\ & ||||Ax - b||||_2 \\
    \text{subject to}\ & x \in \mathbb{R}^n
  \end{align*}

  \pause

  For our particular case, the solution can be achieved with a simple inverse.

  \begin{equation*}
    p = S^{-1}m
  \end{equation*}
\end{frame}

\begin{frame}{Solving the sound problem}

  To solve the problem we can invert the model $S$ and multiply it by $m$.

  \begin{equation*}
    p = S^{-1}m
  \end{equation*}

  \pause

> soundPower :: Vector Double -> Vector Double 
> soundPower m = linv s #>> m
>   where
>     eq r = 1 / (4*pi*sq r)
>     s = lmatrix 2 [  eq 1  ,  eq 2,
>                      eq 2  ,  eq 1]

  where Vector, etc come from \verb|Numeric.LinearAlgebra| in the HMatrix
  package and |#>| is matrix-vector multiplication.

\end{frame}

\begin{frame}{Let's try it out!}

  Say the speaker powers are

  \begin{itemize}
    \item $p_1=10\,\mathrm{mW}$ and
    \item $p_2=5\,\mathrm{mW}$ and
  \end{itemize}

  we measure

  \begin{itemize}
  \item $m_1 = 8.9524\times10^{-5}\,\mathrm{W/m}^2$ and
  \item $m_2 = 5.9683\times10^{-5}\,\mathrm{W/m}^2$.
  \end{itemize}

   Do we recover the original powers?

> p = soundPower (lvector [8.9524e-5, 5.9683e-5])

  \pause

< print p -- [10e-4, 5.0-4] Hooray, same correct answer!


\end{frame}


\begin{frame}{Let's try another}

Let's try to see what happens when we measure different powers!

< p = soundPower (lvector [1e-2])

  \pause

  \begin{verbatim}
  *** Exception: inconsistent dimensions in
  matrix product (2,2) x (1,1)
  CallStack (from HasCallStack):
  error, called at src/Internal/LAPACK.hs in
  hmatrix-0.19.0.0:Internal.LAPACK
  \end{verbatim}

  We forgot to apply the second measurement! :-(

\end{frame}

\begin{frame}{How we would really like to write }

  Ideally we would be able to ensure that the vectors are the right size.

< soundPower :: Vector (n :: Nat) Double -> Vector n  Double
< soundPower m = linv s #>> m
<   where
<     eq r = 1 / (4*pi*sq r)
<
<     s :: Matrix n n
<     s = lmatrix 2 [  eq 1  ,  eq 2,
<                      eq 2  ,  eq 1]

  where all of the |n|s represent the same natural number.

\end{frame}


\section{Type Level Literals}

\begin{frame}{GHC Type Level Literals}

  GHC provides an interface for allowing natural numbers to appear on the type
  level,

  With this one can ``simulate'' some dependently typed structures, where the
  type of an expression depends on the value level.

\end{frame}


\begin{frame}{HMatrix contains a static interface that puts matrix sizes on the
    type level}

< R :: Nat -> * -- In code this is written R n
< L :: Nat -> Nat -> * -- In code this is written L n m

  \pause

  For example,
  \begin{itemize}
    \item |R 2| is a vector containing 3 elements.
    \item |L 2 3| is a matrix with 2 columns and 3 rows.
  \end{itemize}

  \pause

  You will need the @DataKinds@ extension to be able to lift numbers onto the
  type level.

\end{frame}


\begin{frame}{How is KnownNat used to make safe vectors?}

  It uses the following (fill out)

> data Dim (n :: Nat) t = Dim t
>
> newtype Rst n = Rs (Dim n (Vector Double))
>
> mkR :: Vector Double -> Rst n
> mkR = Rs . Dim

  Similarly for the construstor |L|

\end{frame}

\begin{frame}{How is this safe?}

  Could we not just do the following?

> broken_vector = mkR (lvector [1, 2, 3, 4]) :: Rst 2

  HMatrix gets around this by using smart constructors or other checks. This is
  not perfect but allows all previously defined functions to be used easily.

\end{frame}

\begin{frame}{Making a simple 2x2 matrix}

  HMatrix defines the following functions for combining 

< (|||) :: (KnownNat r, KnownNat c1, KnownNat c2) =>
<       -> L r c1
<       -> L r c2
<       -> L r (c1 + c2)

< (===) :: (KnownNat c, KnownNat r1, KnownNat r2) =>
<       -> L r c
<       -> L r c
<       -> L (r1 + r2) c

\end{frame}


\begin{frame}{Making a simple 2x2 matrix}

  HMatrix defines the following functions for combining

> -- Single element matrix
> sem :: Double -> L 1 1
> sem = konst :: (Double -> L 1 1)

> matrix2x2 :: Double -> Double -> Double -> Double -> L 2 2
> matrix2x2 a b c d =  sem a  |||  sem b
>                             ===
>                      sem c  |||  sem d
>   where
>      toSingleM = konst :: (Double -> L 1 2)

\end{frame}

\begin{frame}{Compiler extensions to the rescue}

  Describe typelit-natnormalize and typelit-knownnat

  These help ease the property that the values are not defined inductively,
  which we could otherwise do with things like the singletons library.

\end{frame}

\begin{frame}{We can implement a type safe variant of |soundPower|}

  Using shape index vectors and matrices we can make our function more robust.

> soundPower' :: R 2 -> R 2
> soundPower' m = inv s #> m
>   where
>     eq r = 1 / (4*pi*sq r)
>
>     s =  matrix2x2 (eq 1) (eq 2)
>                    (eq 2) (eq 1)

\end{frame}


\begin{frame}{What if we try the same mistake as before?}

< soundPower' (1e-2 :: R 1)

\pause

\begin{verbatim}
  • Couldn't match type ‘1’ with ‘2’
  Expected type: R 2
  Actual type: R 1
  • In the first argument of ‘soundPower'’,
    namely ‘(1e-2 :: R 1)’
  In the expression: soundPower' (1e-2 :: R 1)
\end{verbatim}

\end{frame}

\section{What is a convex optimization problem}

\begin{frame}{General Formulation of an optimization problem}
  In optimization, we are trying to solve problems of the following form.

  \begin{align*}
    \text{minimize } & f(x) \\
    \text{subject to } & g_i(x) \leq 0,\ i = 1\ldots m \\
                       & h_j(x) = 0,\ j = 1\ldots n
  \end{align*}

  where
  \begin{itemize}
    \item $f$ is a function from $\mathbb{R}^n \rightarrow \mathbb{R}$
    \item $g$ are constraints on regions where the answer could be
      (inequality constraints).
    \item $h$ are equations that have to hold for the solution
      (equality constraints.)
  \end{itemize}

\end{frame}

\begin{frame}{General Formulation of a convex optimization problem}
  In optimization, we are trying to solve problems of the following form.

  \begin{align*}
    \text{minimize } & f(x) \\
    \text{subject to } & g_i(x) \leq 0,\ i = 1\ldots m \\
                     & h_j(x) = 0,\ j = 1\ldots n
  \end{align*}

  where
  \begin{itemize}
  \item $f$ is a \textbf{convex} function from $\mathbb{R}^n \rightarrow \mathbb{R}$
  \item $g$ are \textbf{convex} constraints on regions where the answer could be
    (inequality constraints).
  \item $h$ are \textbf{convex} equations that have to hold for the solution
    (equality constraints.)
  \end{itemize}

\end{frame}


\begin{frame}{What does it mean to be convex?}

  Convexity is the property of a shape/set such that, for any two points $a$ and
  $b$ in that set, the line between those points does not leave the set.

  % TODO: INSERT GRAPHIC HERE

\end{frame}

\begin{frame}{Example of sets that are not convex}
  A set is often non-convex because it contains a ``divit''.


  % TODO: INSERT GRAPHIC HERE

\end{frame}


\begin{frame}{What does it mean to be a convex function?}

  A convex function is one where the \emph{epigraph} of the function is
  a convex set.

  % TODO: INSERT GRAPHIC HERE

\end{frame}


\begin{frame}{Graphically what are we trying to solve then?}
  We want our functions to be convex as well as the set they are defined on
  to be convex.

  % TODO: INSERT GRAPHIC HERE

\end{frame}


\begin{frame}{Convenient properties of convex optimization}
  If a problem can be described by a convex optimization problem, then it has
  two convenient properties.

  \begin{enumerate}
    \item The solution is guaranteed to be the global minimum.
    \item The problem is often much easier to solve than a similar non-convex
      one (for example, integer programming).
  \end{enumerate}

\end{frame}

\section{How could we solve the example problem?}

\begin{frame}{Remember our }

\end{frame}

%if False

> -- Form a block matrix out of component pieces
> blockMatrix :: forall m n p q. (KnownNat m, KnownNat n,
>                                 KnownNat p, KnownNat q) => 
>                L m n -> L m p ->
>                L q n -> L q p ->
>                L (m + q) (n + p) 
> blockMatrix a b c d =  a  |||  b 
>                           === 
>                        c  |||  d

%endif

\end{document}

