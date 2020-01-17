//This is test solidity contract
//Owner : Sungjae Hwang (sjhwang87@kaist.ac.kr)

pragma solidity 0.4.25;

contract A{
    uint public a;
    function setA() public{a = 1;}
}

contract B is A{
}

contract C{
    uint a;
    function order(uint z){a = 6;}//****Detect**** Inheritance order confusion
}

contract D{
    uint a;
    function order(uint x){a = 7;}//****Detect**** Inheritance order confusion
}

contract Test is A, B, C, D{
    uint public a = 0; //****Detect**** storage variable shadowing confusion
    uint public b = 0;
    uint public u = 0;
    address owner = 0x123;
    
    modifier onlyOwner{
        require(msg.sender == owner);
        _;
    }
    
    //This is typo constructor
    function test() public{ //****Detect**** Misuse of constructors
        a = 3;
    }
    
    function usp() public onlyOwner{
        uint[3] un_storage;
        un_storage[1] = 2; //****Detect**** uninitialized storage pointer
    }
    
    function unarytypo() public {
        u =+ 5; //****Detect**** Typo of the += Operator
    }
    
    function deprecatedAPI() public {
        b = msg.gas; //****Detect**** usage of msg.gas deprecated deprecated API
	bytes32 blockx = block.blockhash(block.number.sub(1)); //****Detect**** usage of block.blockhash deprecated API
        throw; //****Detect**** usage of throw deprecated API
    }
    
    function fv(address addr){
        owner = addr; //****Detect**** function without visibility
    }
    
    function downcasting(){
        A a = new A();
        B b = new B();
        
        B t = B(a); //****Detect**** type casting toarbitrary contracts
    }
}
