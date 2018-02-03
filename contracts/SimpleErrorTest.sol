pragma solidity ^0.4.18;

contract SimpleErrorTest {
    
    mapping(uint => uint[2]) public table;
    
    string[] public names;
    
    function set(uint _key, uint _x, uint _y) public {
        table[_key] = [_x,_y];
    }
    
}
