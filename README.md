# purescript-web3-tests
truffle + ps-web3

## Requirements
```bash
stack install psc-package
```

Note that we will provide a workaround for the `pulp run` step until [this issue](https://github.com/purescript-contrib/pulp/issues/309) is fixed. It is embedded in `npm i`

## Build instructions
```bash
> npm i -g truffle
> truffle deploy
> npm i
> # skip for now
> # pulp run -m Generator -- --abis build/contracts --dest ./src
> NODE_URL=http://node.you.deployed.to:8545/ pulp test
```


