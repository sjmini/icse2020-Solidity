# Smart Contract Analyzer [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3603544.svg)](https://doi.org/10.5281/zenodo.3603544)
This repository provides the tool for the paper "Gap between Theory and Practice : An Empirical Study of Security Patches in Solidity" accepted at ICSE 2020.

## Getting Started

### Prerequisites

* sbt version 0.13 or later
* Java version 1.8
* Scala version 2.12.3

### Data Set

To obtain 55,046 contracts used in the paper, please contact the author, Sungjae Hwang (sjhwang87@kaist.ac.kr) and Sukyoung Ryu (sryu.cs@kaist.ac.kr). 


### Building & Executing 

1. Enter sbt command in the root directory and goes into sbt terminal.

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

## Adding New Checker

Our framework parses Solidity contracts and produce AST.

Instead of running our static analyzer, you can easily implement your own analyzer on top of our framework.

To add new analyzer in our framework, follow below steps:
```
1. Add new command for your analyzer in Command.scala and Saf.scala
2. Implement analyzer code and place it in the ast_checker directory
3. Implement initialization code and place it in the phase directory
```

Reference below 4 files:

```
src/main/scala/kr/ac/kaist/saf/phase/InhCheck.scala
src/main/scala/kr/ac/kaist/saf/ast_checker/INHChecker.scala
src/main/scala/kr/ac/kaist/saf/Command.scala
src/main/scala/kr/ac/kaist/saf/Saf.scale
```

## New CVEs

Using our tool, we found hundreds of vulnerable contracts and received eight new CVEs.

| CVE Number | Vulnerability | 
|---|:---:|
| `CVE-2019-15078` | Misuse of constructors | 
| `CVE-2019-15079` | Misuse of constructors |
| `CVE-2019-15080` | Misuse of constructors |
| `CVE-2019-18775` | Misuse of constructors |
| `CVE-2019-18776` | Uninitialize Storage Pointer |
| `CVE-2019-18777` | Type casting to arbitrary contracts |
| `CVE-2019-18778` | Storage variable shadowing confusion |
| `CVE-2019-18779` | Function without visibility |


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
