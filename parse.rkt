#lang brag
_filter : _operator1* _unit (_operator2 _operator1* _unit)*
_unit : ID | EXP
_operator2 : OP2
_operator1 : OP1
