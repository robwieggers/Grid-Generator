Grid-Generator
============

[![GitHub release]](https://github.com/robwieggers/grid-generator/releases/latest)
A Fortran grid generator to create triangular grids for B2.5-Eunomia (or other Scrape Off Layer nuclear fusion codes)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [JSON-Fortran](#json-fortran)
    - [Status](#status)
    - [Brief description](#brief-description)
    - [Download](#download)
    - [Building the library](#building-the-library)
    - [Documentation](#documentation)
    - [Contributing](#contributing)
    - [License](#license)
    - [Miscellaneous](#miscellaneous)

<!-- markdown-toc end -->

Status
------
[![Build Status]](https://travis-ci.org/robwieggers/grid-generator)
[![Codecov](someurl)](https://codecov.io/gh/robwieggers/grid-generator)

[![GitHub issues]](https://github.com/robwieggers/grid-generator/issues)
[![Ready in backlog](someurl)](https://github.com/robwieggers/grid-generator/#contributing)
[![In Progress](someurl)](https://zenhub.io/robwieggers/grid-generator)
[![Needs Review](someurl)](https://zenhub.io/robwieggers/grid-generator)

Take a look at the
[CHANGELOG](https://github.com/robwieggers/grid-generator/blob/master/CHANGELOG.md#unreleased)
for a list of changes since the latest release.

[top](#grid-generator)

Brief description
---------------

A helper tool to generate a triangular grid for Eunomia. Eunomia is a 3D kinetic neutral transport code, coupled to B2.5, a 2D multifluid description of the plasma in the scrape-off layer of a nuclear fusion reactor.

[top](#grid-generator)

Download
--------------------

[![GitHub release]](https://github.com/robwieggers/grid-generator/releases)

Download the official versioned releases
[here](https://github.com/robwieggers/grid-generator/releases/latest).
Or, get the latest development code from the master branch
[here](https://github.com/robwieggers/grid-generator.git).

__NEWS:__

[top](#grid-generator)

Building the library
--------------------

The code requires a Fortran compiler that supports
various Fortran 95, Fortran 2003 and Fortran 2008 features.
It has been successfully compiled with the [GNU gfortran
compiler](http://gcc.gnu.org/wiki/GFortran) [4.9 and greater].

Currently, the tool / library can be build using Make.
To get started with the Make based build, set the
environment variable `FC` to point to your Fortran compiler. Then `make depend` to set dependencies and `make` to build.

[top](#grid-generator)

Documentation
--------------


[top](#grid-generator)

Contributing
------------

Want to contribute? Take a quick look at our [contributing guidelines](https://github.com/robwieggers/grid-generator/blob/master/.github/CONTRIBUTING.md) then claim something in [the "ready" column on our Zenhub.io](https://zenhub.io/robwieggers/grid-generator) and [Fork. Commit. Pull request.](https://help.github.com/articles/fork-a-repo/)

[top](#grid-generator)

License
--------
The Grid-Generator source code and related files and documentation are distributed under a permissive free software license (BSD-style). See the [LICENSE](https://raw.githubusercontent.com/robwieggers/grid-generator/master/LICENSE) file for more details.

[top](#grid-generator)

Miscellaneous
---------------

* Grid-generator is a library to automate the generation of a the triangular grid required by Eunomia (and similar codes). Eunomia is a 3D neutral transport code to model the Scrape-Off Layer (SOL) of a nuclear fusion reactor. Eunomia is typically coupled to B2.5, a multifluid plasma description. This grid-generator is also suitable to create Eunomia grids for other geometries like a linear plasma generator. 
* The grid-generator uses [Triangle](https://www.cs.cmu.edu/~quake/triangle.html) for the actual triangulation.

[top](#grid-generator)
