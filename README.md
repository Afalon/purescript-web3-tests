# purescript-web3-tests

[![Build Status](https://travis-ci.org/f-o-a-m/purescript-web3-tests.svg?branch=master)](https://travis-ci.org/f-o-a-m/purescript-web3-tests)

<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">

## End to End Testing
This repo contains the end to end tests for [purescript-web3](https://github.com/f-o-a-m/purescript-web3) and [purescript-web3-generator](https://github.com/f-o-a-m/purescript-web3)

## Requirements
- a running ethereum node with at least one unlocked account with ether. 

## Build instructions

Note that we will provide a workaround for the `pulp run` step until [this issue](https://github.com/purescript-contrib/pulp/issues/309) is fixed. It is embedded in `npm i`

```bash
> npm install
> npm run truffle -- --network=localhost
> npm run generator
> NODE_URL=http://node.you.deployed.to:8545/ npm run test
```


