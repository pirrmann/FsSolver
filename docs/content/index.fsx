(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
FsSolver
======================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The FsSolver library can be <a href="https://nuget.org/packages/FsSolver">installed from NuGet</a>:
      <pre>PM> Install-Package FsSolver</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates using a function defined in this sample library.

*)
#r "FsSolver.dll"
open FsSolver

printfn "hello = %i" <| Library.hello 0

