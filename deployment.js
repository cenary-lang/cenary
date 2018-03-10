/**
 * Start testrpc with the following command:
 *
 * testrpc -u 0 -u 1
 *
 * This starts testrpc and unlocks the first two users. Pick one of them's address and set the address below.
 */
var address = "0xe57efaf27eb1da9258e6206276d70243f8f59bd5";

/**
 * This is the bytecode for execution. You are already familiar with this.
 */
var bin = '601f80600c6000396000f30063ac37eebb60e060020a600035041415601e5760043560005260206000f35b';

var abi =
  [
    {"name": "identity",
      "type": "function",
      "constant": true,
      "payable": false,
      "inputs": [
        {
          "name": "input",
          "type":"uint256"
        }
      ],
      "outputs": [
        {
          "name": "",
          "type":"uint256"
        }
      ]
    }
  ]

var Web3 = require("web3");
var web3 = new Web3(new Web3.providers.HttpProvider("http://localhost:8545"));
var Contract = new web3.eth.Contract(abi, {data: bin, from: address, gasPrice: 100000});
var deployment = Contract.deploy().send();
deployment.then(newContract => {
  newContract.methods.identity(4).call().then(console.log);
});
