# purescript-web3-tests

[![Build Status](https://travis-ci.org/f-o-a-m/purescript-web3-tests.svg?branch=master)](https://travis-ci.org/f-o-a-m/purescript-web3-tests)

<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">

## End to End Testing
This repo contains the end to end tests for [purescript-web3](https://github.com/f-o-a-m/purescript-web3) and [purescript-web3-generator](https://github.com/f-o-a-m/purescript-web3). It uses the [chanterelle](https://github.com/f-o-a-m/chanterelle) build tool for deployment management.

## Requirements
- a running ethereum node with at least one unlocked account with ether. 

## Build and Test Instructions

```bash
> npm install
> make compile-contracts
> make test
```


