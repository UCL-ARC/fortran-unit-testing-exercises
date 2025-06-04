
> WORK IN PROGRESS 
> 
> This repo is still under construction. As such, it is likely much of what is documented may not yet be implemented.

# Fortran Unit Testing Exercises

This repo contains exercises to help learn unit testing in Fortran and is intended to be used alongside the [UCL-ARC/fortran-unit-testing-lesson](https://github.com/UCL-ARC/fortran-unit-testing-lesson) repo.

## Using this repo

In the [episodes](./episodes/) directory you will find exercises which match up to episodes in [UCL-ARC/fortran-unit-testing-lesson](https://github.com/UCL-ARC/fortran-unit-testing-lesson).

Each episode contains its own build systems however there is a top level build script ([build.sh](./build.sh)) to make things easier.

## Dependencies

There are several prerequisites to being able to use this repo. 

- fpm
- cmake
- A Fortran compiler which supports Fortran XX or above

### pFUnit 

For convenience pFUnit is included as a submodule and can be built using the provided build.sh script. Run `./build.sh -h` for details.

### Dev dependencies

This repo utilises [fortitude](https://fortitude.readthedocs.io/en/stable/) alongside [pre-commit](https://pre-commit.com/) for linting. To install these tools we use pip therefore contibutors will require python version 3.9 or above.

To setup pre-commit and fortitude
1. Create a python virtual environment and activate it
   ```sh
   python3 -m venv .venv
   source .venv/bin/activate
   ```
2. Install the dev dependencies
   ```sh
   python -m pip install -e .
   ```
3. Turn on pre-commit
   ```sh
   pre-commit install
   ```
