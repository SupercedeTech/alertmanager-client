# Alertmanager Client library

This is a WIP library for using the Alertmanager HTTP API for creating & modifying alerts from Haskell code.

It is split into two parts:
  * `alertmanager-openapi` - a Haskell library which contains auto-generated bindings for the Alertmanager v2 HTTP API, generated using `openapi-generator` from the Alertmanager API Swagger file in their repository (pulled in via submodule).
  * `alertmanager-client` - an `amazonka`-inspired Haskell library which provides a high-level interface, including cleaner types and a monad transformer, for running queries against the Alertmanager v2 HTTP API.

## Generating the OpenAPI bindings

A command to re-generate the bindings is provided in the top-level `Makefile` - simply run `make generate`.

Note that some files normally generated by the tool have to be modified by hand. For example, the `.cabal` file contains copyright information, so the tool is forbidden from overwriting it. The full list of such files is contained in `.openapi-generator-ignore`.

In order to generate the bindings correctly, you will need a version of `openapi-generator` that incorporates [this PR](https://github.com/OpenAPITools/openapi-generator/pull/9916).

## Building the project

At the moment, it should be possible to simply `stack build` the two libraries. In the future, we will probably want to use Nix.
