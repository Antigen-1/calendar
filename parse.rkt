#lang brag
_script : _filter
_filter : _application | _primitive
_application : _operator LP [_filter (SP _filter)*] RP
_primitive : ID | EXP
_operator : OP | MOP
