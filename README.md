# Proact

## Table of Contents

<!-- vim-markdown-toc GFM -->

* [Introduction](#introduction)
* [Installation](#installation)
* [Building](#building)
* [Examples](#examples)

<!-- vim-markdown-toc -->

## Introduction

Proact is a reactive framework that communicates with Facebook's [React](https://reactjs.org) library through commands expressed as Free programs and extensible effects. These interactions provide access to a global single state that is composable via profunctor lenses.

Proact enables the development of robust real-world Web Applications by achieving the following goals:

- **Integration with React**. Purescript's community is relatively small and while very dedicated and creative, chances are it won't be able to offer the same repertoire of GUI components that the React community showcases. Furthermore, the ability of React to enhance the semantic value of HTML code certainly gives elegance to GUI development. For example, React components encapsulate a lot of imperative low-level JavaScript code and provide a clean declarative interface with expressive tags and properties. An application written in Proact may include any React component, provided an appropriate Purescript wrapper exists or is written for it.
- **Aspect Oriented Programming**. Proact represents [cross-cutting concerns](https://en.wikipedia.org/wiki/Cross-cutting_concern) as extensible effects that a Web Application defines for all of its components. Thus, logging, error handling or ajax communication can be developed independently of the application code and provide different functionality based on the environment they are running (e.g. test vs production).
- **Functional Reactive Programming**. Proact provides functional operations to manipulate asynchronous data flows and introduces data types to represent events and signals (behaviors). FRP decouples application code from the logic that handles user interactions which makes for more readable and simpler code.
- **Consistent state**. Having a single state prompts an application to behave more consistently, unfortunately, it also makes it harder for the state to be modularized. Proact uses the concept of [lenses](https://github.com/purescript-contrib/purescript-profunctor-lenses) to represent the state of individual components so they can be encapsulated but at the same time can also be composed into a larger overarching state.

## Installation
```sh
git clone https://github.com/alvart/purescript-proact.git proact
cd purescript-proact
spago install
```

## Building

```sh
spago build
```

## Examples

- [To-do Application](https://github.com/alvart/proact-todo). The "Hello World!" of Web Applications.
