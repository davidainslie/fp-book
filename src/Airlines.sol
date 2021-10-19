// SPDX-License-Identifier: MIT

// Accounts we will test (the only ones we are interested in):
// Account address or identity                               Airline
// 0xca35b7d915458ef540ade6068dfe2f44e8fa733c                ASK consortium chairperson
// 0x14723a09acff6d2a60dcdf7aa4aff308fddc160c                fromAirline (for testing airline A)
// 0x4b0897b0513fdc7c541b6d9d7e929c4e5364d2db                toAirline (for testing airline B)

pragma solidity ^0.8.9; 

contract Airlines {
    address chairperson;
    
    struct details {
        uint escrow; // Deposit for payment settlement
        uint status;
        uint hashOfDetails;
    }
    
    mapping (address => details) public balanceDetails;
    mapping (address => uint) membership;
    
    // Modifiers or rules
    modifier onlyChairperson {
        require(msg.sender == chairperson);
        _;
    }
    
    modifier onlyMember {
        require(membership[msg.sender] == 1);
        _;
    }
    
    // Constructor function
    constructor() payable {
        chairperson = msg.sender;
        membership[msg.sender] = 1; // Automatically registered
        balanceDetails[msg.sender].escrow = msg.value;
    }
    
    function register() public payable {
        address AirlineA = msg.sender;
        membership[AirlineA] = 1;
        balanceDetails[msg.sender].escrow = msg.value;
    }
        
   function unregister(address payable AirlineZ) onlyChairperson public {
        if (chairperson != msg.sender) {
            revert();
        }
        
        membership[AirlineZ] = 0;
        
        // Return escrow to leaving airline: verify other conditions 
        AirlineZ.transfer(balanceDetails[AirlineZ].escrow);
        balanceDetails[AirlineZ].escrow = 0; 
    }
    
    function request(address toAirline, uint hashOfDetails) onlyMember public {
        if (membership[toAirline] != 1){
            revert();
        }
        
        balanceDetails[msg.sender].status = 0;
        balanceDetails[msg.sender].hashOfDetails = hashOfDetails;
    }
    
    function response(address fromAirline, uint hashOfDetails, uint done) onlyMember public {
        if (membership[fromAirline] != 1) {
            revert();
        }
        
        balanceDetails[msg.sender].status = done;
        balanceDetails[fromAirline].hashOfDetails = hashOfDetails;
    }
    
    
    function settlePayment(address payable toAirline) onlyMember payable public {
        address fromAirline = msg.sender;
        uint amt = msg.value; 
        balanceDetails[toAirline].escrow = balanceDetails[toAirline].escrow  + amt;
        balanceDetails[fromAirline].escrow = balanceDetails[fromAirline].escrow - amt;
       
        // amt subtracted from msg.sender and given to toAirline
        toAirline.transfer(amt);
    }
}