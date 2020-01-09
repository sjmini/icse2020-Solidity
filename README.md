# Smart Contract Analyzer
This repository provides the tool for the paper "Gap between Theory and Practice : An Empirical Study of Security Patches in Solidity" accepted at ICSE 2020.

## Getting Started

### Prerequisites

* sbt version 1.2.0
* Java version 1.8
* Scala version 2.12.3

### Data Set

To obtain 55,046 contracts used in the paper, please contact the author Sungjae Hwang (sjhwang87@kaist.ac.kr). 


### Building & Executing 

1. Enter sbt in the root directory and goes into sbt terminal.

```
bash-3.2$ sbt
```
2. Enter compile command in sbt terminal

```
> compile
```

3. Build success message would be printed as follow:

```
[success] Total time: 32 s, completed Jan 3, 2020 2:29:55 AM
```

4. To execute the static analzer enter the following command

```
run inhCheck start
```

### Result 
| Vulnerability | Result File | 
|---|:---:|
| `Misuse of constructors` | result_constructor.txt | 
| `Functions without visibility` | result_fv.txt |  
| `Storage variable shadowing confusion` | result_shadow.txt |  
| `Type casting to arbitrary contracts` | type_result_object.txt | 
| `Inheritance order confusion` | result_order.txt | 
| `Uninitialized storage pointers` | result_usp.txt | 
| `Typo of the += operator` | result_unary.txt | 
| `Deprecated Throw API` | result_throw.txt | 
| `Deprecated Sucide API` | result_sucide.txt | 
| `Deprecated block.blockhash API` | result_block.txt | 
| `Deprecated callcode API` | result_callcode.txt | 
| `Deprecated sha3 API` | result_sha3.txt | 
| `Deprecated msg.gas API` | result_gas.txt | 


## Publication
```
Gap between Theory and Practice : An Empirical Study of Security Patches in Solidity. IEEE/ACM 42nd International Conference on Software Engineering, Seoul, South Korea, 23-29 May 2020.

@inproceedings{Hwang2020soliditypatch,
  title={Gap between Theory and Practice : An Empirical Study of Security Patches in Solidity},
  author={Sungjae Hwang, Sukyoung Ryu},
  booktitle={Proceedings of the 42nd International Conference on Software Engineering},
  year={2020}
}
```

Copyright (c) 2020, KAIST
