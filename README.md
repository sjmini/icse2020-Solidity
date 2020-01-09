# icse_tool


## Static Analyzer Getting Started

### Prerequisites

* sbt version 1.2.0
* Java version 1.8
* Scala version 2.12.3

Contact author Sungjae Hwang (sjhwang87@kaist.ac.kr) to obtain contracts used in the paper, and place contracts in the test folder in root directory.

Check src/main/scala/kr/ac/kaist/saf/phase/InhCheck.scala : line 50 to 57.

To analyze contracts placed in other directories, change the file locations specified on line 50~57


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


Copyright (c) 2020, KAIST
