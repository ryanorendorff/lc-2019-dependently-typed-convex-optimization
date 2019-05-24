# Using Dependent Types in Mathematical Optimization Algorithms

Presentation for LambdaConf 2019.

How does a drone remain stable in a chaotic flying environment? By convex
optimization, of course! This talk will delve into how to solve real world
problems via convex optimization. This technique pairs strong mathematical
guarantees with implementation correctness using dependent type theory.
Mathematical optimization is a subfield of mathematics that focuses on selecting
the best element from a set of elements, often in the form of finding the
element that minimizes the value of a chosen function. The algorithms developed
in this field of mathematics are used to train machine learning algorithms,
develop self driving cars, build safe buildings with minimal materials, and
improve battery efficiency, to name only a few applications. Much of the
mechanical operation of these algorithms is performed by repeated calls to
linear algebra packages which, while highly tuned over many decades, provide no
form of compile time guarantee that the given algorithm is correct.

In this talk, we will discuss writing numerical optimization algorithms using
dependent types as a form of compile time check on the correctness of the
algorithm. Dependent types allow the programmer to assert that the dimensions
used in the linear algebra are correct at compile time. We will discuss the
advantages of the dependently typed algorithms, as well as additional methods in
which to ensure they are correct.

In addition, we will provide real world example of using dependently typed
numerical optimizations for applications such as scheduling and controls. In
particular, we will discuss a novel method of reconstructing medical images from
raw data using numerical optimization. Through this example, we will explore
both dependently typed linear algebra as well as functional approaches to linear
algebra through matrix-free methods.

Numerical optimization is the algorithmic backbone behind many modern numerical
computing applications, such as machine learning, fluid dynamics, physics
simulations, and many more. Attendees will leave with an understanding of the
basics of the mathematics behind these algorithms, as well as an understanding
of how functional programming and type theory can assist with proving an
algorithm’s correctness.
