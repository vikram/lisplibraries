{include resources/ug-header.md}
{set-property title "CL-Graph User's Guide"}
{set-property style-sheet user-guide}
{set-property docs-package cl-graph}

# CL-Graph User's Guide

# Table of Contents 

{table-of-contents :start 2 :depth 3}

## Introduction

In Mathematics and Computer Science, a _graph_ is a
collection of _vertexes_ connected by _edges_. Edges may be
_directed_ or _undirected_ (i.e., sometimes you really can't
get there from here). Both edges and vertexes may have
additional data associated with them. Graphs are useful
because you can use them to represent most anything: food
webs, hypertext, the world wide web, protein/protein
interactions, language, who publishes with whom, etc.

CL-Graph is a general graph library built on
[cl-containers][]. It provides an open-ended API for
building, examining and manipulating graphs as well as
implementations of many of the usual suspects of graph
algorithms and measures. 

## Using a graph

### Creation and manipulation

{include "user-guide-details/manipulation.mmd"}

### Tell me about yourself - introspection

{include "user-guide-details/introspection.mmd"}

### Search

{include "user-guide-details/search.mmd"}

### Algorithms

{include "user-guide-details/algorithms.mmd"}

### Iteration

{include "user-guide-details/iteration.mmd"}

### CL-Graph and dotty

{include "user-guide-details/dotty.mmd"}

### Random Graphs

{include user-guide-details/random-graphs.mmd}

### Miscellaneous

{include user-guide-details/miscellaneous.mmd}

### Deprecated - to be removed soon 

{include user-guide-details/deprecated.mmd}

## Index of Functions

{docs-index (function macro) function}

<hr>

{comment
  
#### Glossary

{glossary}


#### Footnotes

{footnotes}
}

{include resources/ug-footer.md}
