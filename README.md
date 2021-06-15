# Functional-Programming-212
Introduction to Functional Programming course assignments with my partner <b><a href="#">Matan Nagar</a></b> ..
In this course we learn and practice more with Functional Programming Paradigm

<h1>Assignment 1 — F-numerals</h1>
<p>The main porpuse of tis assignment is to be more familiar with Lambda-Calculus (λ-Calculus)</p>
<p>F-numerals is a perverse variation of Church Numerals (Alonzo Church), in this version the Numerals defined as: </p>
<p style="margin-left:30px;">f0:= λaλbλc.c</p>
<p style="margin-left:30px;">f1:= λaλbλc.(a(a(b c)))</p>
<p style="margin-left:30px;">fn:= λaλbλc.(a(a(b ..n-times.. c)))</p>
<p>etc...</p>
<p>We were required in this assignment to implement the basic functions on this numerals, defined and implemented as a pure combinator in the λ-calculusand should make no use of anything other than lambda-expressions and their applications, like:
<ul>
  <li>f->integer</li>
  <li>integer->f</li>
  <li>Successor</li>
  <li>Double</li>
  <li>Triple</li>
  <li>Plus</li>
  <li>Multiply</li>
  <li>Power</li>
  <li>Show: that returns the concrete syntax for the source-code of the numeral</li>
  </ul>
</p>
<br>
<hr>

