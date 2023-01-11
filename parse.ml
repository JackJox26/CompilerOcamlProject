
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | VIRGULE
    | VAR
    | UMOINS
    | THEN
    | STR of (
# 6 "parse.mly"
       (string)
# 15 "parse.ml"
  )
    | RETURN
    | POINTVIRGULE
    | POINT
    | PLUS
    | PARENT_G
    | PARENT_D
    | OPERATEUR of (
# 7 "parse.mly"
       (Ast.opType)
# 26 "parse.ml"
  )
    | OBJECT
    | NOMCLASSE of (
# 4 "parse.mly"
       (string)
# 32 "parse.ml"
  )
    | NEW
    | MUL
    | MOINS
    | IS
    | IF
    | ID of (
# 4 "parse.mly"
       (string)
# 42 "parse.ml"
  )
    | EXTENDS
    | EOF
    | ELSE
    | DIV
    | DEUXPOINTS
    | DEF
    | CSTE of (
# 5 "parse.mly"
       (int)
# 53 "parse.ml"
  )
    | CONCAT
    | CLASS
    | AUTO
    | AFFECT
    | ACCOLADE_G
    | ACCOLADE_D
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState70
  | MenhirState68
  | MenhirState62
  | MenhirState57
  | MenhirState53
  | MenhirState49
  | MenhirState47
  | MenhirState44
  | MenhirState39
  | MenhirState33
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState8
  | MenhirState7
  | MenhirState3
  | MenhirState2
  | MenhirState0

# 1 "parse.mly"
  
open Ast

# 105 "parse.ml"

let rec _menhir_goto_lDeclVar : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.declType list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ACCOLADE_G ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | CSTE _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | IF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | MOINS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | PLUS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | RETURN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (d : (Ast.declType))), _, (l : (Ast.declType list))) = _menhir_stack in
        let _v : (Ast.declType list) = 
# 79 "parse.mly"
                            ( d::l )
# 154 "parse.ml"
         in
        _menhir_goto_lDeclVar _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_lInstruc : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instructionType list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (l : (Ast.instructionType list))) = _menhir_stack in
        let _v : (Ast.instructionType list) = 
# 71 "parse.mly"
                            ( l )
# 171 "parse.ml"
         in
        _menhir_goto_optLInstruc _menhir_env _menhir_stack _menhir_s _v
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACCOLADE_D ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (ld : (Ast.declType list))), _, (li : (Ast.instructionType list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.blocType) = 
# 67 "parse.mly"
                                                        ( BlocDecl(ld,li) )
# 190 "parse.ml"
             in
            _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i : (Ast.instructionType))), _, (l : (Ast.instructionType list))) = _menhir_stack in
        let _v : (Ast.instructionType list) = 
# 75 "parse.mly"
                            ( i::l )
# 206 "parse.ml"
         in
        _menhir_goto_lInstruc _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NOMCLASSE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (
# 4 "parse.mly"
       (string)
# 225 "parse.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.typeType) = 
# 56 "parse.mly"
                            ( Type(s) )
# 232 "parse.ml"
         in
        (match _menhir_s with
        | MenhirState39 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (t : (Ast.typeType)) = _v in
            let (_menhir_stack, _menhir_s, (l : (string list))) = _menhir_stack in
            let _v : (Ast.declType) = 
# 82 "parse.mly"
                            ( Decl(l,t) )
# 243 "parse.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | IS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (d : (Ast.declType))) = _menhir_stack in
                let _v : (Ast.declType list) = 
# 78 "parse.mly"
                            ( [d] )
# 258 "parse.ml"
                 in
                _menhir_goto_lDeclVar _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
        | MenhirState57 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (t : (Ast.typeType)) = _v in
            let (_menhir_stack, (s : (
# 4 "parse.mly"
       (string)
# 272 "parse.ml"
            ))) = _menhir_stack in
            let _v : (Ast.paramType) = 
# 53 "parse.mly"
                            ( Param(s,t) )
# 277 "parse.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (p : (Ast.paramType)) = _v in
            let ((_menhir_stack, _menhir_s), (a : (bool))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.champsType) = 
# 63 "parse.mly"
                               ( Champs(a,p) )
# 287 "parse.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | ACCOLADE_D ->
                _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | MOINS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | PLUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | MOINS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | PLUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | MOINS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | PLUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | MOINS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | PLUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | MOINS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | PLUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_goto_lChamp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.champsType list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACCOLADE_D ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (lc : (Ast.champsType list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.corpsType) = 
# 41 "parse.mly"
                                                        ( Corps(lc(*,lm*)) )
# 431 "parse.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (c : (Ast.corpsType)) = _v in
            let (((_menhir_stack, _menhir_s), (n : (
# 4 "parse.mly"
       (string)
# 439 "parse.ml"
            ))), _, (b : (Ast.blocType option))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.objetIsoleType) = 
# 50 "parse.mly"
                                                        ( { nomObjetIsole=n ; oConstruct=b ; corpsObjetIsole=c } )
# 445 "parse.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (o : (Ast.objetIsoleType)) = _v in
            let _v : (Ast.objetType) = 
# 35 "parse.mly"
                            ( ObjetIsole(o) )
# 453 "parse.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | OBJECT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | ACCOLADE_G ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (c : (Ast.champsType))), _, (l : (Ast.champsType list))) = _menhir_stack in
        let _v : (Ast.champsType list) = 
# 60 "parse.mly"
                            ( c::l )
# 481 "parse.ml"
         in
        _menhir_goto_lChamp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_AUTO_ : _menhir_env -> 'ttv_tail -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEUXPOINTS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_bloc : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.blocType) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 | MenhirState44 | MenhirState47 | MenhirState25 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (b : (Ast.blocType))) = _menhir_stack in
        let _v : (Ast.instructionType) = 
# 90 "parse.mly"
                                                        ( Bloc(b) )
# 524 "parse.ml"
         in
        _menhir_goto_instruc _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Ast.blocType))) = _menhir_stack in
        let _v : (Ast.blocType option) = 
# 116 "<standard.mly>"
    ( Some x )
# 534 "parse.ml"
         in
        _menhir_goto_option_bloc_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (Ast.objetType list))), _, (b : (Ast.blocType))) = _menhir_stack in
            let _3 = () in
            let _v : (Ast.progType) = 
# 27 "parse.mly"
                            ( Prog(l,b) )
# 550 "parse.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.progType)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_instruc : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instructionType) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ACCOLADE_G ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | CSTE _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | IF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | MOINS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | PLUS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | RETURN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (e : (Ast.expType))), _, (i1 : (Ast.instructionType))), _, (i2 : (Ast.instructionType))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.instructionType) = 
# 92 "parse.mly"
                                                        ( IfThenElse(e,i1,i2) )
# 615 "parse.ml"
         in
        _menhir_goto_instruc _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 | MenhirState47 | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACCOLADE_G ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | CSTE _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | ID _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | IF ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MOINS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PLUS ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | RETURN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | STR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | ACCOLADE_D ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (i : (Ast.instructionType))) = _menhir_stack in
            let _v : (Ast.instructionType list) = 
# 74 "parse.mly"
                            ( [i] )
# 645 "parse.ml"
             in
            _menhir_goto_lInstruc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | _ ->
        _menhir_fail ()

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "parse.mly"
       (string)
# 658 "parse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_lIdent : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (s : (
# 4 "parse.mly"
       (string)
# 675 "parse.ml"
        ))), _, (l : (string list))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) = 
# 86 "parse.mly"
                            ( s::l )
# 681 "parse.ml"
         in
        _menhir_goto_lIdent _menhir_env _menhir_stack _menhir_s _v
    | MenhirState49 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEUXPOINTS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | _ ->
        _menhir_fail ()

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "parse.mly"
       (string)
# 701 "parse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VIRGULE ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
    | DEUXPOINTS ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expType) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCAT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MOINS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | POINTVIRGULE | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expType))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expType) = 
# 104 "parse.mly"
                            ( MoinsU(e) )
# 745 "parse.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCAT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MOINS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | POINTVIRGULE | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expType))), _, (e2 : (Ast.expType))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expType) = 
# 98 "parse.mly"
                            ( Plus(e1,e2) )
# 776 "parse.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCAT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MOINS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | POINTVIRGULE | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expType))), _, (e2 : (Ast.expType))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expType) = 
# 100 "parse.mly"
                            ( Mult(e1,e2) )
# 807 "parse.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCAT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MOINS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | POINTVIRGULE | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expType))), _, (e2 : (Ast.expType))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expType) = 
# 99 "parse.mly"
                            ( Moins(e1,e2) )
# 838 "parse.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCAT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MOINS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | POINTVIRGULE | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expType))), _, (e2 : (Ast.expType))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expType) = 
# 101 "parse.mly"
                            ( Div(e1,e2) )
# 869 "parse.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCAT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MOINS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | POINTVIRGULE | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expType))), _, (e2 : (Ast.expType))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expType) = 
# 102 "parse.mly"
                            ( Concat(e1,e2) )
# 900 "parse.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCAT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MOINS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | POINTVIRGULE | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expType))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expType) = 
# 103 "parse.mly"
                            ( PlusU(e) )
# 931 "parse.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCAT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MOINS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ACCOLADE_G ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | CSTE _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | IF ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | MOINS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | PLUS ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | RETURN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 | MenhirState44 | MenhirState47 | MenhirState25 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCAT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MOINS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | POINTVIRGULE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expType))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.instructionType) = 
# 89 "parse.mly"
                                                        ( Exp(e) )
# 1010 "parse.ml"
             in
            _menhir_goto_instruc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.champsType list) = 
# 59 "parse.mly"
                            ( [] )
# 1032 "parse.ml"
     in
    _menhir_goto_lChamp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AUTO ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 1050 "parse.ml"
         in
        _menhir_goto_boption_AUTO_ _menhir_env _menhir_stack _v
    | ID _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 1058 "parse.ml"
         in
        _menhir_goto_boption_AUTO_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_optLInstruc : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instructionType list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ACCOLADE_D ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (o : (Ast.instructionType list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.blocType) = 
# 66 "parse.mly"
                                                        ( BlocLInst(o) )
# 1085 "parse.ml"
         in
        _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parse.mly"
       (string)
# 1098 "parse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 6 "parse.mly"
       (string)
# 1106 "parse.ml"
    )) = _v in
    let _v : (Ast.expType) = 
# 97 "parse.mly"
                            ( Str(s) )
# 1111 "parse.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | POINTVIRGULE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.instructionType) = 
# 91 "parse.mly"
                                                        ( Return )
# 1131 "parse.ml"
         in
        _menhir_goto_instruc _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | MOINS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | PLUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | MOINS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | PLUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | MOINS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | PLUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_reduce9 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 4 "parse.mly"
       (string)
# 1207 "parse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (s : (
# 4 "parse.mly"
       (string)
# 1213 "parse.ml"
    ))) = _menhir_stack in
    let _v : (Ast.expType) = 
# 95 "parse.mly"
                            ( Id(s) )
# 1218 "parse.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce27 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 4 "parse.mly"
       (string)
# 1225 "parse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (s : (
# 4 "parse.mly"
       (string)
# 1231 "parse.ml"
    ))) = _menhir_stack in
    let _v : (string list) = 
# 85 "parse.mly"
                            ( [s] )
# 1236 "parse.ml"
     in
    _menhir_goto_lIdent _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 4 "parse.mly"
       (string)
# 1243 "parse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "parse.mly"
       (int)
# 1259 "parse.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (v : (
# 5 "parse.mly"
       (int)
# 1267 "parse.ml"
    )) = _v in
    let _v : (Ast.expType) = 
# 96 "parse.mly"
                            ( Cste(v) )
# 1272 "parse.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_lObjets : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.objetType list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (o : (Ast.objetType))), _, (l : (Ast.objetType list))) = _menhir_stack in
        let _v : (Ast.objetType list) = 
# 31 "parse.mly"
                            ( o::l )
# 1287 "parse.ml"
         in
        _menhir_goto_lObjets _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACCOLADE_G ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_bloc_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.blocType option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACCOLADE_G ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | ACCOLADE_D ->
                _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ACCOLADE_G ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | CSTE _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState3 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | VIRGULE ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | DEUXPOINTS ->
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
        | CONCAT | DIV | MOINS | MUL | PLUS | POINTVIRGULE ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | IF ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | MOINS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | PLUS ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | RETURN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | ACCOLADE_D ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState3 in
        let _v : (Ast.instructionType list) = 
# 70 "parse.mly"
                            ( [] )
# 1386 "parse.ml"
         in
        _menhir_goto_optLInstruc _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.objetType list) = 
# 30 "parse.mly"
                            ( [] )
# 1492 "parse.ml"
     in
    _menhir_goto_lObjets _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NOMCLASSE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACCOLADE_G ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | IS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState2 in
            let _v : (Ast.blocType option) = 
# 114 "<standard.mly>"
    ( None )
# 1516 "parse.ml"
             in
            _menhir_goto_option_bloc_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.progType) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OBJECT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ACCOLADE_G ->
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 1566 "parse.ml"
