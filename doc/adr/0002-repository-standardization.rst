ADR 2: Repository Standardization
=======================================

Date: 2022-07-06

Authors
---------

Lorenzo Calegari <lorenzo.calegari@iohk.io>

Status
------

Draft

Context
-------

IOG is undertaking a company-wide effort to restructure and standardize its 
repositories, favoring mono-repos and enforcing shared GitOps and DevOps 
processes. Parallel to this, a new CI infrastructure is being developed.

Examples of this are:

* `input-output-hk/cardano-world <https://github.com/input-output-hk/cardano-world>`_
* `input-output-hk/ci-world <https://github.com/input-output-hk/ci-world>`_
* `input-output-hk/atala-world <https://github.com/input-output-hk/atala-world>`_
  
This initiative appears to be championed by the SRE team who are the creators of 
`divnix/std <https://github.com/divnix/std>`_. Indeed `std` is at the heart of 
the standardization dream.

In short, `std` aims to answer the one critical question that pops in the mind 
of newcomers and veterans alike: 

*What can I do with this repository?*

In practice, `std` is flake-based nix library code that provides a 
strongly-but-sensibly-opinionated top-level interface for structuring all your 
nix code.

This is wonderful news for the owner of the repository's nix code, but what
about every other stakeholder? Especially developers who don't care/know about 
nix? 

Contributors of a standardized codebase will be gifted with a TUI to discover 
and interact with the repository, which is probably something that is long 
overdue as an industry-level best-practice.

Who wouldn't want to clone a repository, type `std` and be presented with a TUI 
that gives you an interactive tour of the repository's artifacts, together with 
a list of all possible DevOps and GitOps actions (build, test, develop, run, 
deploy, benchmark, publish, package, monitor, ...) in addition to any other 
action that you may define. 

And for power users and automators, there is an equivalent CLI to the TUI. 
This makes `README` files obsolete to an extent. 
A TUI/CLI combo represents the best conceivable solution in terms of user 
experience (only a GUI could top that perhaps).

Decision
--------

* Meetings will take place to evaluate the decision and define a roadmap

Implications
------------

The plutus repositories now exhibit a large amount of duplicated nix 
(and configuration) code, as a result of the split into `plutus-core`
and `plutus-apps`.

The goal is to standardize both repositories, by introducing `std` and 
refactoring all existing nix code accordingly. 

The SRE team has also created several other satellite repositories containing 
reusable nix code to support this process.

The standardization process would follow the `4 Layers SRE Mental Model <https://sre-manual.infra.aws.iohkdev.io/mental-model.html>`_, 
which begins by introducing `std`` in Layer 1 (binary packaging).
Then, the SRE team will guide us through layers 2-3-4 and integration 
with the new CI.

This is a significant undertaking that will require weeks or longer to complete.
