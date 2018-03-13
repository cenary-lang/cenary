/**
 * Start testrpc with the following command:
 *
 * testrpc -u 0 -u 1
 */

/**
 * This is the bytecode created by the following Ivy program:
 *
 * int fib(int n) {
 *   int a = 0;
 *   int b = 1;
 *   int sum = 6;
 *   while (n > 1) {
 *     sum = a + b;
 *     a = b;
 *     b = sum;
 *     n = n - 1;
 *   };
 *   return sum;
 * }
 */
var bin = '@bin@';
var fs = require('fs');
var abi =
  [
    {
      "name": "identity",
      "type": "function",
      "constant": true,
      "payable": false,
      "inputs": [
        {
          "name": "n",
          "type":"uint256"
        }
      ],
      "outputs": [
        {
          "name": "",
          "type":"uint256"
        }
      ]
    },
    {
      "name": "addvals",
      "type": "function",
      "constant": true,
      "payable": false,
      "inputs": [
        {
          "name": "m",
          "type":"uint256"
        },
        {
          "name": "n",
          "type":"uint256"
        }
      ],
      "outputs": [
        {
          "name": "",
          "type":"uint256"
        }
      ]
    },
    {
      "name": "fib",
      "type": "function",
      "constant": true,
      "payable": false,
      "inputs": [
        {
          "name": "n",
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
web3.eth.getAccounts().then(addresses => {
  var Contract = new web3.eth.Contract(abi, {data: bin, from: addresses[0], gasPrice: 20000000000, gas: 1000000 });
  var deployment = Contract.deploy().send();
  deployment.then(newContract => {
    newContract.methods.identity(3).call().then(console.log);
    newContract.methods.addvals(15, 25).call().then(console.log);
    newContract.methods.fib(15).call().then(console.log);
  });
});
