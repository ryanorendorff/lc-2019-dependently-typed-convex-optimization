%if False

> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE TypeApplications       #-}
> {-# LANGUAGE DataKinds              #-}
> {-# LANGUAGE KindSignatures         #-}
> {-# LANGUAGE ViewPatterns           #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
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
> import Numeric.LinearAlgebra (Transposable)
> import Numeric.LinearAlgebra.Static
> import Data.Maybe (maybe)
> import Data.Tuple.HT (fst3)
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
%format * = "\cdot "
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Motivation                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For the first section, do not highlight the current section (overview slide)
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

  Remember that the detected sound goes with the square of the distance between the speaker and the detection device!

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

\begin{frame}{Let's try it out!}

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

  With this one can simulate some dependently typed structures, where the type
   of an expression depends on the value level.

\end{frame}


\begin{frame}{HMatrix contains a static interface that puts matrix sizes on the
    type level}

< R :: Nat -> * -- In code this is written R n
< L :: Nat -> Nat -> * -- In code this is written L n m

  In code these are written @R n@ and @L m n@

  \pause

  For example,
  \begin{itemize}
    \item |R 2| is a vector containing 2 elements.
    \item |L 2 3| is a matrix with 2 rows and 3 columns.
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

  Let's use the concatenation operations to make a safe 2x2 matrix.

> -- Single element matrix
> sem :: RDouble -> L 1 1
> sem = konst :: (RDouble -> L 1 1)
>
> matrix2x2 :: RDouble -> RDouble -> RDouble -> RDouble -> L 2 2
> matrix2x2 a b c d =  sem a  |||  sem b
>                             ===
>                      sem c  |||  sem d

\end{frame}


\begin{frame}{What if we made our matrix function incorrectly?}

  HMatrix defines the following functions for combining

< sem' :: RDouble -> L 1 2
< sem' = konst :: (RDouble -> L 1 2)

< matrix2x2' :: RDouble -> RDouble -> RDouble -> RDouble -> L 2 2
< matrix2x2' a b c d =  sem' a  |||  sem' b
<                               ===
<                       sem' c  |||  sem' d

  \pause

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

\begin{frame}{Type level naturals allow us to make partial code total}

  What did we gain from this example?

  \begin{itemize}
    \item We started with a way to solve a numeric problem that made sense,
    \item found out our function was partial,
    \item and then made the function total\footnote{almost total: S could be singular, fix with |withCompactSVD|} by representing the shapes of our data in the type.
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

\begin{frame}{Graphically, we are attempting to inch towards to better answers}
  \includegraphics[]{fig/quad-1d-opt.png}
\end{frame}

\begin{frame}{Reconstructing sparse signals}

  Say we have 1,000 speakers but we only make 200 measurements. However, we know
  that only a handful of speakers are on. Can we reconstruct the signal?

  \pause

  \begin{align*}
    \text{minimize } & \frac{1}{2} ||||Ax-b||||_2^2 + \lambda ||||x||||_1
  \end{align*}

  where

  \begin{itemize}
    \item $||||Ax-b||||_2^2$ is a data consistency (same as the sound example)
    \item $||||x||||_1$ says that our signal should be sparse (have few non-zero components)
  \end{itemize}

\end{frame}


\begin{frame}{We need a few pieces to solve this problem}

  To solve this problem we need a few pieces.

  \begin{itemize}
    \item The function |f(x)| and its gradient $\nabla f(x)$.
    \item The function |g| and its proximal operator $prox(g)(x)$.
  \end{itemize}

\end{frame}

\begin{frame}{The $f$ function}

  Our function $f$ is defined as

  \begin{equation*}
    \frac{1}{2}||||Ax - b||||_2^2 = \frac{1}{2} \left(x^TA^TAx - 2 b^TAx + b^Tb\right)
  \end{equation*}

  \pause

In Haskell we can write this as

> f :: (KnownNat n, KnownNat m) => L n m -> R n -> R m -> RDouble
> f a b x = norm_2 (a #> x - b)

\end{frame}

\begin{frame}{The $\nabla f$ function}

  Our function $\nabla f$ is defined as

  \begin{align*}
    \nabla_x f = & \nabla_x \frac{1}{2} \left(x^TA^TAx - 2 b^TAx + b^Tb\right) \\
               = & A^T(Ax - b)
  \end{align*}

  \pause

In Haskell we can write this as

> grad_f :: (KnownNat n, KnownNat m) => L n m -> R n -> R m -> R m
> grad_f a b x = tr a #> (a #> x - b)

\end{frame}


\begin{frame}{The $g$ function}

  Our function $g$ is defined as

  \begin{align*}
    \lambda ||||x||||_1
  \end{align*}

  \pause

In Haskell we can write this as

> g :: (KnownNat n) => Double -> R n -> RDouble
> g lambda = (lambda*) . norm_1

\end{frame}


\begin{frame}{The $prox(g)$ function}

  Our function $prox(g)$ is defined as

  \begin{align*}
    \mathrm{prox}(\lambda {\left|| \cdot \right||}_{1}) \left( x \right) & = \arg \min_{u} \left\{ \frac{1}{2} {\left|||| u - x \right||||}^{2} + \lambda {\left|| u \right||}_{1} \right\}
   \\
   & = \mathrm{sign} \left( {x}_{i} \right) \max \left( \left|| {x}_{i} \right|| - \lambda, 0 \right)
  \end{align*}

  \pause

In Haskell we can write this as

> prox_g :: (KnownNat n) => Double -> R n -> R n
> prox_g lambda = dvmap f
>     where
>         f xi = signum xi * max (abs xi - lambda) 0

\end{frame}

\begin{frame}{Now that we have all the pieces, how do we solve this problem?}

  We now have all of these pieces. Time for a a gradient descent algorithm!

< f :: (KnownNat n, KnownNat m) => L n m -> R n -> R m -> RDouble
< f a b x = norm_2 (a #> x - b)
<
< grad_f :: (KnownNat n, KnownNat m) => L n m -> R n -> R m -> R m
< grad_f a b x = tr a #> (a #> x - b)
<
< g :: (KnownNat n) => Double -> R n -> RDouble
< g lambda = (lambda*) . norm_1
<
< prox_g :: (KnownNat n) => Double -> R n -> R n
< prox_g lambda = dvmap f
<     where
<         f xi = signum xi * max (abs xi - lambda) 0


\end{frame}

\begin{frame}{Gradient descent graphically}

  INSERT A GRAPHIC HERE!

  The solver we will be using is called the Fast Iterative Soft Thresholding Algorithm (FISTA).

\end{frame}

\begin{frame}{FISTA definition}

> -- Solve problems like $\mathrm{min}\ ||||Ax - b||||_2 + \lambda ||||x||||_1$
> fista :: (KnownNat n) =>
>          (R n -> Double)  --  $f$: Smooth convex function
>      ->  (R n -> Double)  --  $g$: non-smooth convex function
>      ->  (R n -> R n)     --  $\nabla f$: gradient of f
>      ->  (R n -> R n)     --  $prox(g)$: proximal operator of g
>      ->  Double           --  $L$: Lipschitz constant
>      ->  Maybe (R n)      --  Initial guess at a solution
>      ->  [R n]            --  Iterative solutions

\end{frame}

\begin{frame}{FISTA definition}

%format lipschitz = "L "
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

> divv = (/)
> kL (l) = konst l

%endif

> fista f g grad_f prox_g lipschitz (maybe (konst 0) id -> x0) =
>       map fst3 $ iterate go (x0, x0, 1)
>     where
>         -- Main update step
>         gradient = \x -> x - kL (divv 1 lipschitz) * grad_f x
>         update = prox_g . gradient
> 
>         -- Full algorithm with linear point combination
>         go (x_1, y_1, t_1) = let
>             x_2 = update y_1
>             t_2 = divv (1 + sqrt (1 + 4*sq t_1)) 2
>             y_2 = x_2 + kL (divv (t_1 - 1) t_2) * (x_2 - x_1)
>             in (x_2, y_2, t_2)

\end{frame}

\begin{frame}{Results}
  If we use a random $A \in \mathbb{R}^{200\times 1000}$ with a signal with 30 non-zero elements, we can reconstruct the result beautifully!
  \includegraphics[width=\textwidth]{fig/l1.pdf}

\end{frame}


\begin{frame}{Hmm, something's up with our }

  If we look at the definition of FISTA again, we notice we didn't mention $A$ anywhere.

< f :: (KnownNat n, KnownNat m) => L n m -> R n -> R m -> RDouble
< f a b x = norm_2 (a #> x - b)
<
< grad_f :: (KnownNat n, KnownNat m) => L n m -> R n -> R m -> R m
< grad_f a b x = tr a #> (a #> x - b)

  \pause

  Can we replace our |a|, which is a |L m n|, with a pair of functions
  |a_forward :: R n -> R m| and |a_transpose :: R m -> R n|?

  \pause

  We sure can!

\end{frame}

\section{Matrix Free Optimization}

%format LinearOperator (n) (m) = "\mathbb{R}_F^{" n "\times " m "} "
%format LO = "\mathbb{R}_F"

\begin{frame}{Definition of a linear function without the matrix}

  We can define a linear operator |L n m| using functions of its matrix-vector
   multiplication and the transpose matrix-vector multiplication.

> data LinearOperator (n :: Nat) (m :: Nat) where
>   LO  ::  (KnownNat n, KnownNat m) =>
>           (R m -> R n)
>       ->  (R n -> R m)
>       ->  LinearOperator n m

\end{frame}

%format fromL =  "from\mathbb{R}^{\cdot \times \cdot}"

\begin{frame}{Converting a matrix to a LinearOperator}

  We can lift our normal matrices into this form in the following way.

> fromL :: (KnownNat n, KnownNat m) => L n m -> LinearOperator n m
> fromL a = LO (a #>) (tr a #>)

\end{frame}


\begin{frame}{Our matrix free forms can also be transposed}

  HMatrix defines a typeclass for transposable functions.

< class Transposable m mt | m -> mt, mt -> m where
<   tr  :: m -> mt -- conjugate transpose
<   tr' :: m -> mt -- transpose

> instance  (KnownNat n, KnownNat m) =>
>           Transposable (LinearOperator n m) (LinearOperator m n) where
>   tr (LO f b) = LO b f
>   tr' = tr

\end{frame}

%if False

> (##>) :: (KnownNat m, KnownNat n) => LinearOperator m n -> R n -> R m
> (##>) (LO f b) = f
> infixr 8 ##>

> (<#>) :: (KnownNat m, KnownNat n, KnownNat p) => LinearOperator m n -> LinearOperator n p -> LinearOperator m p
> (<#>) (LO f b) (LO f' b') = LO (f . f') (b' . b)
> infixr 8 <#>

%endif

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

\begin{frame}{What does it mean to be convex?}

  Convexity is the property of a shape/set such that, for any two points $a$ and
  $b$ in that set, the line between those points does not leave the set.

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

\begin{frame}{Compiler extensions to the rescue}

  Describe typelit-natnormalize and typelit-knownnat

  These help ease the property that the values are not defined inductively,
  which we could otherwise do with things like the singletons library.

\end{frame}

-- This almost works except the functional dependencies
-- prevent me from defining a new instance of just the matrix component.
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


