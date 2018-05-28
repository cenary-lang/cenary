/**
 * Start testrpc with the following command:
 *
 * testrpc -u 0 -u 1
 */

var fs = require('fs');

var bin = '@bin@';
var abi = @abi@

var Web3 = require("web3");
var web3 = new Web3(new Web3.providers.HttpProvider("http://localhost:8545"));
web3.eth.getAccounts().then(addresses => {
  var Contract = new web3.eth.Contract(abi, {data: bin, from: addresses[0], gasPrice: 100000000000, gas: 4712388 });
  var deployment = Contract.deploy().send();
  deployment.then(newContract => {
    newContract.methods.@call@.call()
      .then(result => {
        console.log(result);
      })
      .catch(err => {
        console.log(err); 
      });
    // newContract.methods.@call@.send()
    //   .then(tx => {
    //     console.log(tx.events);
    //     for(var i = 0; i < tx.events.length; i++) {
    //       var ev = tx.events[i];
    //       console.log(ev.raw); 
    //     };
    //   })
    //   .catch(err => {
    //     console.log(err); 
    //   });
  });
});
