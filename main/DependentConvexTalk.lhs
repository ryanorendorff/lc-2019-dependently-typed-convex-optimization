%if False

> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE DataKinds              #-}
> {-# LANGUAGE KindSignatures         #-}
> {-# LANGUAGE ViewPatterns           #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE FlexibleContexts       #-}
>
> -- Allows us to infer additional constraints from the definition
> -- Instead of (KnownNat n, KnownNat (n+2)) =>
> -- we only need (KnownNat n) =>
> {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
>
> -- Allows us to equate terms like (a + b) + c and a + (b + c) as the same thing.
> {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
> 
> module DependentConvexTalk where
>
> import GHC.TypeLits
> import qualified Numeric.LinearAlgebra as LA
> import Numeric.LinearAlgebra (Transposable)
> import Numeric.LinearAlgebra.Static
> import Data.Maybe (fromMaybe, fromJust)
> import Data.Tuple.HT (fst3)
> import Data.Proxy(Proxy(..))
> import Data.Type.Equality ((:~:)(Refl))
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
\usepackage{amssymb}
\usepackage{amsthm}
\usepackage{textcomp}

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
% \usepackage[bottom]{footmisc}

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
%format * = "\cdot "
%format _ (a)         = "_{" a "}"
%format _ (a)         = "_{" a "}"
%format sqrt (n) = "\sqrt{ " n "} "
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
%format #> = "\ \#\rangle\ "
%format #>> = "\ \#\rangle\ "
%format lmatrix = "matrix "
%format lvector = "vector "
%format linv = "inv "
%format pi = "\pi "
%format sq (a) = a "^{2} "
%format Rs = "\Varid{R}"
%format Rst = R

%format grad_f = "\nabla_{\!f} "
%format prox_g

%format norm_0
%format norm_1
%format norm_2
%format norm_inf = "norm_{\infty} "



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

% For the first section, do not highlight the current section (overview slide)
\section{Example problem: how to reconstruct an inverse problem}

% For all subsequent sections, highlight what section we are currently in.
\AtBeginSection[]
{
  \begin{frame}
    \frametitle{Table of Contents}
    \tableofcontents[currentsection]
  \end{frame}
}

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

\begin{frame}{Example Problem: Measuring sound power from two speakers}

  Say Alice and Bob want to measure how much power two speakers are putting out
  using a decibel meter program on their phone.

  \begin{center}
    \includegraphics[width=\textwidth]{fig/sound-setup.pdf}
  \end{center}

  This system is described by

  \begin{itemize}
    \item Two sources of sound emitting with power $p_1$ and $p_2$, and
    \item Two measurements: $m_1$ and $m_2$
  \end{itemize}

\end{frame}

\begin{frame}{Model of how sound propagates}

  For a speaker outputting with power $p$, we can describe the intensity of the
  measurement $m$ (in $W/m^2$) with the following equation.

  \begin{equation*}
    m = \frac{p}{4 \pi r^2}
  \end{equation*}

  \pause

  We can write what $m_1$ and $m_2$ will measure as follows.

  \begin{align*}
    \frac{p_1}{4 \pi r_{m_1\text{-to-}p_1}^2} +  \frac{p_2}{4 \pi r_{m_1\text{-to-}p_2}^2}  & = m_1 \\
    \frac{p_1}{4 \pi r_{m_2\text{-to-}p_1}^2} +  \frac{p_2}{4 \pi r_{m_2\text{-to-}p_2}^2}  & = m_2
  \end{align*}

  % Remember that the detected sound goes with the square of the distance between the speaker and the detection device!

\end{frame}

\begin{frame}{Model of how sound propagates using linear algebra}

  We can rewrite our equations into the following matrix form.

  \begin{equation*}
    \begin{gathered}
      \begin{bmatrix}
        \frac{1}{4 \pi r_{m_1\text{-to-}p_1}^2} & \frac{1}{4 \pi r_{m_1\text{-to-}p_2}^2} \\
        \frac{1}{4 \pi r_{m_2\text{-to-}p_1}^2} & \frac{1}{4 \pi r_{m_2\text{-to-}p_2}^2}
      \end{bmatrix} \\
      S
    \end{gathered}
    \begin{gathered}
      \begin{bmatrix}
        p_1 \\
        p_2
      \end{bmatrix} \\
      p
    \end{gathered}
    % This is gross. Don't do this.
    \begin{gathered}
      \raisebox{-4pt}{=} \\
      \raisebox{-14.5pt}{=} 
    \end{gathered}
    \begin{gathered}
      \begin{bmatrix}
        m_1 \\
        m_2
      \end{bmatrix} \\
      m
    \end{gathered}
  \end{equation*}

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
    \text{minimize}\ & ||||Sp - m||||_2 \\
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

\begin{frame}{Let's reconstruct the sound output power from measurements}

  Say we don't know the power output of each speaker, but we measure

  \begin{itemize}
  \item $m_1 = 8.9524\times10^{-5}\,\mathrm{W/m}^2$ and
  \item $m_2 = 5.9683\times10^{-5}\,\mathrm{W/m}^2$.
  \end{itemize}

  Do we recover the original powers?

> p = soundPower (lvector [8.9524e-5, 5.9683e-5])

  \pause

< print p -- [0.0010, 0.0005] That is correct!


\end{frame}


\begin{frame}{Let's try another set of measurements}

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

\begin{frame}{How we would really like to write |soundPower|}

  Ideally we would be able to ensure that the vectors are the right size.

< soundPower :: Vector (2 :: Nat) Double -> Vector 2 Double
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

\begin{frame}{We can express natural numbers on the type level}

  GHC has a mechanism for encoding strings, booleans and natural numbers in the
  type level\footnote{Needs @-XDataKinds@ and the |GHC.TypeLits| module. See
  Thomas Dietert's talk for all the details!}.

  \pause

  In GHCi we can ask what the type of |5| is.

  \vspace{-0.5em}

< :t 5
< 5 :: Num p => p

  \pause

  \vspace{-2em}

  And also notice that we have a type level |5| too!

  \vspace{-0.5em}

< :k 5
< 5 :: Nat

  With this one can simulate some dependently typed structures, where the type
  of an expression depends on the value level.

  \vspace{1em}

\end{frame}


\begin{frame}{We can also do some basic type level arithmetic}

  The GHC type level literal module also provides some convenient type level
  functions to do basic arithmetic on the type level\footnote{We use the
    packages @ghc-typelits-natnormalise@ and @ghc-typelits-knownnat@ to normalize
    algebraic expressions}.

< type family (m :: Nat) * (n :: Nat) :: Nat
< type family (m :: Nat) - (n :: Nat) :: Nat
< type family (m :: Nat) + (n :: Nat) :: Nat

\pause

< type family (m :: Nat) <=? (n :: Nat) :: Bool

\end{frame}

\begin{frame}{HMatrix uses type level naturals to guarantee matrix sizes}

< R :: Nat -> Type -- In code this is written R n
< L :: Nat -> Nat -> Type -- In code this is written L n m

  \pause

  For example,
  \begin{itemize}
    \item |R 2| is a vector containing 2 elements.
    \item |L 2 3| is a matrix with 2 rows and 3 columns.
  \end{itemize}

\end{frame}


\begin{frame}{HMatrix uses phantom types to wrap up vectors with their shape}

  HMatrix uses a phantom type-level natural number to encode the length of the
  vector.

> data Dim (n :: Nat) t = Dim t
>
> newtype Rst n = Rs (Dim n (Vector Double))
>
> mkR :: Vector Double -> Rst n
> mkR = Rs . Dim

  \pause

  Similarly for the construstor |L|

\end{frame}

\begin{frame}{Can we make improperly sized vectors?}

  Could we not just do the following?

> brokenVector = mkR (lvector [1, 2, 3, 4]) :: Rst 2

  \pause

  HMatrix gets around this by using smart constructors. This is not the perfect
  encoding but allows all previously defined functions linear algebra to be
  used easily.

\end{frame}

%format r_1
%format r_2
%format c_1
%format c_2

\begin{frame}{Example size safe operation: making a simple 2x2 matrix}

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

  Let's use the concatenation operations to make a safe 2x2 matrix.

> -- Single element matrix (sem)
> sem :: RDouble -> L 1 1
> sem = konst :: (RDouble -> L 1 1)

\pause

> matrix2x2 :: RDouble -> RDouble -> RDouble -> RDouble -> L 2 2
> matrix2x2 a b c d =  sem a  |||  sem b
>                             ===
>                      sem c  |||  sem d

\end{frame}


\begin{frame}{What if we made our matrix function incorrectly?}

  What if we accidently did not make our single element matrices correctly?

> sem' :: RDouble -> L 1 2
> sem' = konst :: (RDouble -> L 1 2)

\pause

< matrix2x2' :: RDouble -> RDouble -> RDouble -> RDouble -> L 2 2
< matrix2x2' a b c d =  sem' a  |||  sem' b
<                               ===
<                       sem' c  |||  sem' d

  \pause

  % It does the first |||, which returns a 1 x 4

  \begin{verbatim}
  Couldn't match type ‘4’ with ‘2’
  Expected type: L 1 2
    Actual type: L 1 (2 + 2)
  \end{verbatim}

\end{frame}

\begin{frame}{We can implement a type safe variant of |soundPower|}

  Using shape index vectors and matrices we can make our function more robust.

> soundPower' :: R 2 -> R 2
> soundPower' m = inv s #> m
>   where
>     eq r = 1 / (4*pi*sq r)
>
>     s =  matrix2x2  (eq 1)  (eq 2)
>                     (eq 2)  (eq 1)

\end{frame}


\begin{frame}{What if we try the same mistake as before?}

< soundPower' (konst 1e-2 :: R 1)

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

\begin{frame}{Type level naturals allow us to make partial code robust}

  What did we gain from this example?

  \begin{itemize}
    \item We started with a way to solve a numeric problem that was mathematically correct,
    \item found out our function was partial,
    \item and then made the function more robust by representing the shapes of our data in the type.
  \end{itemize}

\end{frame}

\section{What other kinds of problems can we solve?}

\begin{frame}{Proximal optimization problems}

  We will be trying to solve problems in this particular form

  \begin{align*}
    \text{minimize } & F(x) \equiv f(x) + g(x)
  \end{align*}

  where

  \begin{itemize}
    \item $F$ is the cost function from $\mathbb{R}^n \rightarrow \mathbb{R}$
    \item $f$ is a smooth convex function with a bounded derivative.
    \item $g$ is a (potentially non-smooth) convex function.
  \end{itemize}

\end{frame}

\begin{frame}{Reconstructing sparse signals}

  Say we have 1,000 speakers but we only make 200 measurements. However, we know
  that only a handful of speakers are on. Can we reconstruct the signal?

  \pause

  \begin{align*}
    \text{minimize } & \frac{1}{2} ||||Sp-m||||_2^2 + \lambda ||||p||||_1
  \end{align*}

  where

  \begin{itemize}
    \item $||||Sp-m||||_2^2$ is the data consistency term,
    \item $||||p||||_1$ says that our signal should be sparse (have few non-zero components)
    \item $\lambda$ trades off which we care about more: data consistency or sparsity.
  \end{itemize}

\end{frame}


\begin{frame}{Graphical depiction of how to solve F with FISTA}

  Fast Iterative Soft Thresholding Algorithm (FISTA)

  \centering
  \includegraphics<1>[]{fig/descent-F.pdf}
  \includegraphics<2>[]{fig/descent-start.pdf}
  \includegraphics<3>[]{fig/descent-grad.pdf}
  \includegraphics<4>[]{fig/descent-prox.pdf}

\end{frame}

\newcommand{\ffunc}{
  
> f  ::  (KnownNat m, KnownNat n, 1 <= n, 1 <= m) =>
>        L m n ->  R m -> R n -> RDouble
> f a b x = norm_2 (a #> x - b)
>
> grad_f  ::  (KnownNat m, KnownNat n, 1 <= n, 1 <= m) =>
>             L m n -> R m -> R n -> R n
> grad_f a b x = tr a #> (a #> x - b)

}

\begin{frame}{We need a few pieces to solve this problem}

  To solve this problem we need a few pieces.

  \begin{itemize}
    \item The function |f(x)| and its gradient $\nabla f(x)$.
    \item The function |g| and its proximal operator $prox(g)(x)$.
  \end{itemize}

\end{frame}

\begin{frame}{The $f$ function}

  Our function $f$ and its derivative $\nabla f$ are defined as

  \begin{align*}
    f & =  \frac{1}{2}||||Ax - b||||_2^2 \\
    \nabla_x f & =  A^T(Ax - b)
  \end{align*}

  \pause

The types make sure our functions are correct in terms of size.

\ffunc

\end{frame}


\begin{frame}{The $g$ function}

  Our function $g$ and its proximal operator $prox(g)$ are defined as

  \begin{align*}
    g & = \lambda ||||x||||_1 \\
   \mathrm{prox}_{\lambda {\left|||| \cdot \right||||}_{1}} \left( x \right)  & = \mathrm{sign} \left( {x}_{i} \right) \max \left( \left|| {x}_{i} \right|| - \lambda, 0 \right)
  \end{align*}

  \pause

In Haskell we can write this as

> g :: (KnownNat n, 1 <= n) => Double -> R n -> RDouble
> g lambda = (lambda*) . norm_1
>
> prox_g :: (KnownNat n, 1 <= n) => Double -> R n -> R n
> prox_g lambda = dvmap f
>     where
>         f xi = signum xi * max (abs xi - lambda) 0


\end{frame}


\begin{frame}{Now that we have all the pieces, how do we solve this problem?}

  We now have the pieces to find the minimum of our function.

  \includegraphics[]{fig/descent-prox.pdf}

\end{frame}

\begin{frame}{FISTA definition}

> -- Solve problems like $\mathrm{min}\ ||||Ax - b||||_2 + \lambda ||||x||||_1$
> fista :: (KnownNat n, 1 <= n) =>
>          (R n -> R n)     --  $\nabla f$: gradient of f
>      ->  (R n -> R n)     --  $prox(g)$: proximal operator of g
>      ->  Double           --  $l$: Lipschitz constant
>      ->  Maybe (R n)      --  Initial guess at a solution
>      ->  [R n]            --  Iterative solutions

\end{frame}

\begin{frame}{FISTA definition}

%format lipschitz = "l "
%format kL l = l
%format divv (a) (b) = "\frac{ " a "}{" b "} "
%format x_0
%format x_1
%format x_2
%format y_1
%format y_2
%format t_1
%format t_2

%if False

> divv :: Double -> Double -> Double
> divv = (/)
> kL l = konst l

%endif

> fista grad_f prox_g lipschitz (fromMaybe (konst 0) -> x0) =
>       map fst3 $ iterate go (x0, x0, 1)
>     where
>         -- Main update step
>         gradient x = x - kL (divv 1 lipschitz) * grad_f x
>         update = prox_g . gradient
>
>         -- Full algorithm with linear point combination
>         go (x_1, y_1, t_1) = let
>             x_2 = update y_1
>             t_2 = divv (1 + sqrt (1 + 4*sq t_1)) 2
>             y_2 = x_2 + kL (divv (t_1 - 1) t_2) * (x_2 - x_1)
>             in (x_2, y_2, t_2)

\end{frame}

\begin{frame}{Now we just have to call our solver.}
  Let's assume we have a system with the following problem.

  \begin{itemize}
    \item A model of a system, |s :: L 200 1000|. This means 200 measurements
      of a system with 1000 components. In the speaker example this would be
      200 cellphone measurements and 1000 speakers.
    \item A set of measurements system, |m :: R 200|.
    \item A sparse system (few non-zero elements)
  \end{itemize}

  \pause

%if False

% We solve this in the notebook instead.

> s = undefined :: L 200 1000
> m = undefined :: R 200
> lambda = 1 :: RDouble
> l = 1 :: RDouble

%endif

> cost x = f s m x + g lambda x
>
> sol = fista (grad_f s m) (prox_g lambda) l Nothing
> costs = map cost sol

\end{frame}

\begin{frame}{Example reconsutrction of a sparse signal with FISTA}

  We use a random model $S \in \mathbb{R}^{200\times 1000}$ with a signal with 30 non-zero elements.

  \centering
  \includegraphics<1>[width=\textwidth]{fig/l1-signal.pdf}
  \includegraphics<2>[width=\textwidth]{fig/l1-recon.pdf}
  \includegraphics<3>[width=\textwidth]{fig/l1-data-consistency.pdf}


\end{frame}


\begin{frame}{We used a lot of memory though...}

  When we used FISTA, we potentially used a lot of memory because $A$ could be
  huge.

  \ffunc
  
  \pause

  For example, in a real world imaging problem we used |A :: L 3374000 23232400|, which if fully provided would be 570 TB!

  \pause

  Can we replace our |a|, which is a |L m n|, with what it represents?

\end{frame}

\begin{frame}

  \centering

  {\Huge Ditch the Matrix}

  \includegraphics[width=\textwidth]{fig/matrix-inefficient.jpg}

\end{frame}

%if False

> forward = undefined
> adjoint = undefined

%endif

\begin{frame}{Overview of a matrix}

  When we write a matrix such as |L m n|, what this really represents is a function

> forward :: (KnownNat m, KnownNat n) => R n -> R m

  \pause

  Additionally we often use the transpose of the matrix, which is another function

> adjoint :: (KnownNat m, KnownNat n) => R m -> R n

\end{frame}

%format LinearOperator (n) (m) = "\mathbb{R}_F^{" n "\times " m "} "
%format LO = "\mathbb{R}_F"

\begin{frame}{Definition of a linear function without the matrix}

  We can define a linear operator |L n m| using functions of its matrix-vector
   multiplication and the transpose matrix-vector multiplication.

> data LinearOperator (m :: Nat) (n :: Nat) where
>   LO  ::  (KnownNat m, KnownNat n) =>
>           (R n -> R m) -- The forward function
>       ->  (R m -> R n) -- The adjoint function
>       ->  LinearOperator m n

\end{frame}

%format fromL =  "from\mathbb{R}^{\cdot \times \cdot}"

\begin{frame}{Converting a matrix to a LinearOperator as a sanity check}

  We can lift our normal matrices into this form in the following way.

> fromL :: (KnownNat m, KnownNat n) => L m n -> LinearOperator m n
> fromL a = LO (a #>) (tr a #>)

  This is a good sanity check that we are going in the right direction.

\end{frame}

%format eye_free = "eye_{\mathbb{R}_F}"

\begin{frame}{Example matrix free linear operators: Identity}

  The identity matrix is represented as a diagonal matrix of all ones in linear
  algebra.

  \begin{equation*}
  I = \begin{bmatrix}1 & 0 & 0 \\ 0 & \ddots & 0 \\ 0 & 0 & 1 \end{bmatrix}
  \end{equation*}

  \pause

  We can implement this function by just returning the vector we get in.

> eye_free :: (KnownNat n) => LinearOperator n n
> eye_free = LO id id

\end{frame}


%format avgRepeated_free = "avgRepeated_{\mathbb{R}_F} "
%format <.> = "\ \langle\cdot\rangle\ "

\begin{frame}[t]{Example matrix free linear operators: Repeated Average}

  The matrix describing an average, where the average is repeated for the number
  of elements in the input, is defiend as follows.

  \begin{equation*}
  M = \begin{bmatrix}1/n & \ldots & 1/n \\ \vdots & \ddots & \vdots \\ 1/n & \ldots & 1/n \end{bmatrix} \in \mathbb{R}^{n \times n}
  \end{equation*}

  \pause

  We can implement this function by a simple repetition of the mean value

\only<2>{

> avg :: forall n. (KnownNat n) => R n -> RDouble
> avg v = v <.> 1/n'
>     where
>         n' = fromIntegral . natVal $ (Proxy :: Proxy n)

}\only<3>{

> avgRepeated_free :: (KnownNat n) => LinearOperator n n
> avgRepeated_free = LO f f
>     where
>         f v = konst (avg v)

}

\end{frame}

%if False

> fft :: (KnownNat n) => R n -> R n
> fft = undefined
> ifft :: (KnownNat n) => R n -> R n
> ifft = undefined

%endif

%format fft_free = "f\!ft_{\mathbb{R}_F} "

\begin{frame}{Example matrix free linear operators: Fourier Transform}

  The Fourier transform is a linear operator that takes a signal and describes
  the frequencies that make up that signal\blfootnote{Figure from \url{https://www.nti-audio.com/en/support/know-how/fast-fourier-transform-fft}}.

  \only<1>{\centering \includegraphics[width=200pt]{fig/fft.png}}
  \only<2->{

  Using unitary versions of the FFT, we can define the matrix free version as
  follows.

> fft_free :: (KnownNat n) => LinearOperator n n
> fft_free = LO fft ifft

  }
  \visible<3>{

  The matrix version requires $O(n^2)$ time and $O(n^2)$ space, while the matrix
  free version takes $O(n\log(n))$ time and $O(n)$ space.

  }

\end{frame}

%format trLO = "\Varid{tr} "

%if False

% We are keeping this out here so we can not introduce the type class to them.

> instance  (KnownNat n, KnownNat m) =>
>           Transposable (LinearOperator n m) (LinearOperator m n) where
>   tr = trLO
>   tr' = tr

%endif

%format ##> = "\ \#\!\rangle_F\ "
%format <#> = "\ \Varid{<>}_F\ "


\begin{frame}{We can apply our matrix free forms by matrix-vector multiply}

  To match the gradient function we saw before, we need to implement
  matrix-vector multiply and transpose.

\pause

> (##>) :: (KnownNat n, KnownNat m) => LinearOperator n m -> R m -> R n
> (##>) (LO f a) = f

\pause

> trLO :: (KnownNat n, KnownNat m) => LinearOperator n m -> LinearOperator m n
> trLO (LO f a) = LO a f

  \pause

  There is only one way to make these functions without introducing other
  components.

\end{frame}
%if False

> infixr 8 ##>
> infixr 8 <#>

%endif

%format scale_LO = "scale_{\mathbb{R}_F} "
%format *# = "\cdot "
%format +# = "+ "

%format f_a
%format f_b
%format b_a
%format b_b
%format a_a
%format a_b

\begin{frame}{Combine matrixes using the definition of linearity: scale}

  Linear operations can be multiplied by a constant. We can either multiply the
  elements of the matrix by this constant, or equivalently scale the resulting
  vector.

  \begin{align*}
    (s\cdot A)x & = s\cdot (Ax)\\
    (s\cdot A^T)x & = s\cdot (A^Tx)
  \end{align*}

  \pause

> (*#)  ::  (KnownNat m, KnownNat n) =>
>           Double -> LinearOperator m n -> LinearOperator m n
> (*#) s (LO f a) = LO  (\v -> konst s * f v)
>                       (\v -> konst s * a v)

\end{frame}

\begin{frame}{Combine linear operators using the definition of linearity: add}

  Linear operations can also be added together either by adding the matrices
  element wise, or equivalently by adding the resulting vectors together.

  \begin{align*}
    (A + B)x & = Ax + Bx \\
    (A + B)^Tx & = A^Tx + B^Tx
  \end{align*}

  \pause

> (+#) :: (KnownNat m, KnownNat n) => LinearOperator m n -> LinearOperator m n -> LinearOperator m n
> (+#) (LO f_a a_a) (LO f_b a_b) = LO  (\v -> f_b v + f_a v)
>                                      (\v -> a_b v + a_a v)

\end{frame}

\begin{frame}{Example of linearity: removing the average from a vector}

  For example, we can remove the average value from a vector as follows.

  \begin{equation*}
    I - M
  \end{equation*}

  \pause

  With our linearity combinators this is defined as

> removeAvg :: (KnownNat n) => LinearOperator n n
> removeAvg = eye_free +# ((-1) *# avgRepeated_free)

\end{frame}

\begin{frame}{Model composition is matrix multiplication}

  Sometimes our model of the system (such as $S$) can be broken into multiple
   pieces.

  \begin{align*}
    A = A_0 A_1 \ldots A_n
  \end{align*}

\end{frame}

%if False

> model_0 :: LinearOperator 3 6
> model_0 = undefined
> model_1 :: LinearOperator 6 8
> model_1 = undefined

> exactDims' :: forall n m j k . (KnownNat n, KnownNat m, KnownNat j, KnownNat k) => LinearOperator m n -> Maybe (LinearOperator j k)
> exactDims' m@(LO f b) = do
>   Refl <- sameNat (Proxy :: Proxy m) (Proxy :: Proxy j)
>   Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy k)
>   return $ LO f b

> innerDims :: forall n m p q. (KnownNat m, KnownNat n, KnownNat p, KnownNat q) => LinearOperator m n -> LinearOperator p q -> Maybe (LinearOperator m n, LinearOperator p q, n :~: p)
> innerDims a@(LO fa ba) b@(LO fb bb) = do
>   Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy p)
>   return $ ((LO fa ba), (LO fb bb), Refl)

%endif

\begin{frame}{Types guide our implementation of matrix multiplication}

  The shape of the linear operators guides the developer to the correct
  definition.

> (<#>)  ::  (KnownNat m, KnownNat n, KnownNat p) =>
>            LinearOperator m n -> LinearOperator n p -> LinearOperator m p
> (<#>) (LO f_a a_a) (LO f_b a_b) = LO (f_a . f_b) (a_b . a_a)

  \pause

  We can now easily compose our models like before.

> model = model_0 <#> model_1

\end{frame}

%format f_F
%format grad_f_F = "\Varid{grad_f}_F "

\begin{frame}{We can now remake our gradient descent functions}

  With what we have now, we can easily define a linear operator

< a = (b +# c) <#> s *# d

  \novspacepause

  And use the same definition of gradient descent we had before

> f_F  ::  (KnownNat m, KnownNat n, 1 <= n, 1 <= m) =>
>          LinearOperator m n -> R m -> R n -> RDouble
> f_F a b x = norm_2 (a ##> x - b)
>
> grad_f_F  ::  (KnownNat m, KnownNat n, 1 <= n, 1 <= m) =>
>               LinearOperator m n -> R m -> R n -> R n
> grad_f_F a b x = tr a ##> (a ##> x - b)

  but now using far less memory and compute time!

\end{frame}

%format model_0
%format model_1
%format model_2

\section{Medical Imaging Example}

\begin{frame}{Magnetic Particle Imaging detects iron nanoparticles}

\begin{center}
  \includegraphics{fig/mpi.png}
\end{center}

\end{frame}


\begin{frame}{We have a model for MPI made up of two parts: separation and DC removal}

  In MPI, we have a model of how the system:

  \begin{itemize}
    \item We can only detect small patches of the FOV at once. We have a matrix free
   model for how these patches are collected, called $S$.

    \item Each patch is missing a DC offset. We model this DC removal using a matrix
  free operator $D$.
  \end{itemize}

\end{frame}

\begin{frame}[t]{$S$ operator: MPI images are taken in panels/patches}
  Due to power limitations, we acquire the image field of view (FOV) in small
  chunks called either panels or patches.

  \centering
  \includegraphics<1>[width=\textwidth]{fig/lc-logo-fov}
  \includegraphics<2>[width=\textwidth]{fig/lc-logo-panel}

\end{frame}

\begin{frame}{$D$ operator: each panel loses its average value}
  Due to the physics of our imaging, we lose the average value in each panel. We
  can represent this in 1D as the signal shifting to center around zero. This is just |removeAvg|!

  \centering
  \includegraphics[width=\textwidth]{fig/lc-logo-dc-removal}

% > dc_removal :: (KnownNat n) => LinearOperator n n
% > dc_removal

\end{frame}

\begin{frame}{We can image interesting geometries like the LC logo}

  \centering
  \includegraphics[width=\textwidth]{fig/lc-logo-picture.jpg}

\end{frame}

\begin{frame}{And use our previous results to reconstruct an image!}

  \centering
  \includegraphics[width=\textwidth]{fig/lc-logo-mpi.png}

\end{frame}

\begin{frame}[t]{Matrix free makes untenable problems possible}

  We are also using the same sparse idea to descrease our 3D imaging time by
  40\%.

  \begin{center}
    \includegraphics<1>[width=\textwidth]{fig/radon-full.png}
    \includegraphics<2>[width=\textwidth]{fig/radon-inverse.png}
    \includegraphics<3>[width=\textwidth]{fig/radon-l1.png}
  \end{center}

  \visible<3>{This approach reduced space requirements from 12 GB to 30 MB. We sped up our
  process 20 fold using a GPU.}

\end{frame}

\begin{frame}{Lessons learned}

  \begin{itemize}
    \item We had algorithms with provable properties such as convergence,
      optimality, and 
    \item We added rigor by giving our data structures shapes on the type level.
    \item This guided our implementations and in some cases made the
      implementation of an algorithm immediately apparent.
  \end{itemize}

\end{frame}

\begin{frame}{Questions?}

  \centering
  \includegraphics[height=\textheight]{fig/question.jpg}

\end{frame}

\begin{frame}{References}
  
  \begin{itemize}
    \item GHC.TypeNats: \url{https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeNats.html}
    \item HMatrix: \url{https://hackage.haskell.org/package/hmatrix-0.19.0.0/docs/Numeric-LinearAlgebra-Static.html}
    \item Fixed-Length Vector Types in Haskell: \url{https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html}
    \item L1 formulation from Stanford EE364b: \url{https://web.stanford.edu/class/ee364b/lectures/l1_slides.pdf}
    \item Beck, Amir, and Marc Teboulle. ``A fast iterative shrinkage-thresholding algorithm for linear inverse problems.'' SIAM journal on imaging sciences 2.1 (2009): 183-202.
    \item Konkle, Justin J., et al.  ``A convex formulation for magnetic particle imaging x-space reconstruction.'' PloS one 10.10 (2015): e0140137.
  \end{itemize}

\end{frame}

\begin{frame}{References}
  
  \begin{itemize}
  \item Goodwill, Patrick W., and Steven M. Conolly. ``Multidimensional x-space magnetic particle imaging.'' IEEE transactions on medical imaging 30.9 (2011): 1581-1590.
  \item Driscoll, Michael, et al. ``Indigo: A Domain-Specific Language for Fast, Portable Image Reconstruction.'' 2018 IEEE International Parallel and Distributed Processing Symposium (IPDPS). IEEE, 2018.
  \end{itemize}

\end{frame}

\end{document}

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

< -- This almost works except the functional dependencies
< -- prevent me from defining a new instance of just the matrix component.
< instance Domain Double R LinearOperator where
<   mul (LO f b) (LO f' b') = LO (f . f') (b . b')
<   app (LO f b) = f
<   dot = undefined
<   cross = undefined
<   diagR = undefined
<   dvmap = undefined
<   outer = undefined
<   zipWithVector = undefined
<   det =  undefined
<   invlndet = undefined
<   expm = undefined
<   sqrtm = undefined
<   inv = undefined

%endif


