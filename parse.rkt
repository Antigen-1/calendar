#lang brag
_filter : _operator* _unit (_operator+ _unit)*
_unit : ID | EXP
_operator : OP
