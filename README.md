# purescript-web3-tests
<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">

## End to End Testing
This repo contains the end to end tests for [purescript-web3](https://github.com/f-o-a-m/purescript-web3) and [purescript-web3-generator](https://github.com/f-o-a-m/purescript-web3)


## Requirements
- psc-package
- truffle
- a running ethereum node with at least one unlocked account with ether. 

Note that we will provide a workaround for the `pulp run` step until [this issue](https://github.com/purescript-contrib/pulp/issues/309) is fixed. It is embedded in `npm i`

## Build instructions

Note that we will provide a workaround for the `pulp run` step until [this issue](https://github.com/purescript-contrib/pulp/issues/309) is fixed. It is embedded in `npm i`

```bash
> npm i -g truffle
> truffle deploy
> npm i
> NODE_URL=http://node.you.deployed.to:8545/ pulp test
```


