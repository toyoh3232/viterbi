# viterbi
A Convolutional Encoder/Decoder  

## Demo
1 bit input and 3 bit output at a time with 3 registers. Each output bit is generated from

|OUTPUT | Polynomial|
|-------|-----------|
|[0] | REGISTER[2]|
|[1] | REGISTER[0] ⊕ REGISTER[1]|
|[2] | REGISTER[1] ⊕ REGISTER[2]|

Test case is like following

![result](https://github.com/somnus0208/viterbi/blob/master/tty.gif)
