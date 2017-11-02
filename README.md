# purescript-web3-tests
truffle + ps-web3

## Requirements
```bash
stack install psc-package
```

Note that we will provide a workaround for the `pulp run` step until https://github.com/purescript-contrib/pulp/issues/309 is fixed. It is embedded in `npm install`

## Build instructions
```bash
> npm install -g truffle
> truffle deploy
> npm install
> pulp run -m Generator -- --abis build/contracts --dest ./src
> NODE_URL=http://node.you.deployed.to:8545/ pulp test
```


