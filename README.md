# Fortran Unit Testing Exercises

This repo contains exercises to help learn unit testing in Fortran and is intended to be used alongside the
[Unit Testing in Fortran lesson](https://github-pages.arc.ucl.ac.uk/fortran-unit-testing-lesson/).

## Using this repo

In the [episodes](./episodes/) directory you will find exercises which match up to episodes in the
[Unit Testing in Fortran lesson](https://github-pages.arc.ucl.ac.uk/fortran-unit-testing-lesson/).

Each episode contains its own build system and README.md with information on how to build and what the tasks are for that episode.

### Solutions

Every exercise has a provided solution. To use this solution, you will often need to make a small change to some code or build/run
a different project.

### devcontainer

Provided in this repo is a [devcontainer setup](./.devcontainer/). This devcontainer allows working with the repo within a
pre-defined Docker environment with all of the necessary dependencies installed. There are two convenient ways to use the
devcontainer. You can clone the repo and then run the container locally using
[VSCode's devcontainer functionality](https://code.visualstudio.com/docs/devcontainers/containers). Another, perhaps more
convenient, method is to use [GitHub codespaces](https://github.com/features/codespaces).

> To use the local VSCode method, you will require [Docker](https://www.docker.com/) installed on your local machine.

#### GitHub Codespaces

To open a GitHub Codespace for this repository, select the `<> Code` drop-down within the home page of this repository. Then, from
the Codespaces tab, select `Create codespace on main`. This should open a new tab with a VSCode interface, running inside the
pre-built container. When you first create a codespace it may take a few moments to start up.

> Note that any codespace you create from the repository will be paid for out of
> [your monthly free allowance](https://docs.github.com/en/billing/concepts/product-billing/github-codespaces#monthly-included-storage-and-core-hours-for-personal-accounts)<!-- markdownlint-disable-line MD013 -->
> provided by GitHub. Therefore, make sure you [delete the codespace](https://docs.github.com/en/codespaces/developing-in-a-codespace/deleting-a-codespace)<!-- markdownlint-disable-line MD013 -->
> when you are done. You can check your running codespaces at [github.com/codespaces](https://github.com/codespaces)

## Dependencies

> If you are using the devcontainer provided, these dependencies are already available in your environment.

There are several prerequisites to being able to use this repo.

- [Fortran Package Manager (FPM)](https://fpm.fortran-lang.org/)
- [CMake](https://cmake.org/)
- A Fortran compiler which supports Fortran 2003 or above
- pFUnit (see below)

### pFUnit

Several of the exercises rely on the [pFUnit testing library](https://github.com/Goddard-Fortran-Ecosystem/pFUnit). This library
needs to be built locally for these relevant exercises to work. For convenience a [script](./scripts/build-pfunit.sh) is provided
to fetch and build pFUnit. Run this script with the `-h` flag for more information.

> pFUnit has already been installed within the devcontainer at `/home/vscode/pfunit`.

### Dev dependencies

This repo utilises [fortitude](https://fortitude.readthedocs.io/en/stable/) alongside [pre-commit](https://pre-commit.com/) for
linting. To install these tools we use pip therefore contributors will require python version 3.9 or above.

To setup pre-commit and fortitude

1. Create a python virtual environment and activate it

   ```sh
   python3 -m venv .venv
   source .venv/bin/activate # or `source .venv/scripts/activate` on windows
   ```

2. Install the dev dependencies

   ```sh
   python -m pip install -r requirements.txt
   ```

3. Turn on pre-commit

   ```sh
   pre-commit install
   ```
