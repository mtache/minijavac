infix_operator:
        |  OR {"||"}
        |  AND {"&&"}
        |  BOR {"|"}
        |  BXOR {"^"}
        |  BAND {"&"}
        |  EQUAL {"=="}
        |  NOTEQUAL {"!="}
        |  LESS {"<"}
        |  GREATER {">"}
        |  LESSEQUAL {"<="}
        |  GREATEREAQUAL {">="}
        |  LSHIFT {"<<"}
        |  RSHIFT {">>"}
        |  ZFRSHIFT {">>>"}
        |  PLUS {"+"}
        |  MINUS {"-"}
        |  TIMES {"*"}
        |  DIV {"/"}
        |  MOD {"%"}


        prefix_operator:
                  | NEGATION {"!"}
                    | BCOMPLEMENT {"~"}

                    postfix_operator:
                              | INCREMENT {"++"}
                                | DECREMENT   {"--"}

                                assignment_operator:
                                          | EQ {"="}
                                            | SELFADD {"+="}
                                              | SELFSUB {"-="}
                                                | SELFMUL {"*="}
                                                  | SELFDIV {"/="}
                                                    | SELFAND {"&="}
                                                      | SELFOR  {"|="}
                                                        | SELFXOR {"^="}
                                                          | SELFMOD {"%="}
                                                            | SELFLEFTSHIFT {"<<="}
                                                              | SELFRIGHTSHIFT {">>="}
                                                                | USELFRIGHTSHIFT  {">>>="}


                                                                class_body:
                                                                            | d=method_declaration* { { cMethods = d } }

                                                                            method_declaration:
                                                                                        | h=method_header { { mHeader = h } }

                                                                                        method_header:
                                                                                                    | m=class_modifier* r=jType n=IDENTIFIER LEFTP p=comma_separated_parameters? RIGHTP LBRACKET RBRACKET  { match p with
                                                                                                                                                                                                                    | (Some(param)) -> { mModifier = m; mResultType = r; mName = n; mParameters = param }
                                                                                                                                                                                                                                                                                                                                    | _ -> { mModifier = m; mResultType = r; mName = n; mParameters = [] } }

                                                                                                    jType:
                                                                                                                | BYTE { Byte }
                                                                                                                    | SHORT { Short }
                                                                                                                        | INT { Int }
                                                                                                                            | LONG { Long }
                                                                                                                                | CHAR { Char }
                                                                                                                                    | FLOAT { Float }
                                                                                                                                        | DOUBLE { Double }
                                                                                                                                            | VOID { Void }

                                                                                                                                            parameters:
                                                                                                                                                        | t=jType i=IDENTIFIER { { pType=t; pName=i} }

                                                                                                                                                        comma_separated_parameters:
                                                                                                                                                                    | p=parameters { [p] }
                                                                                                                                                                        | c=comma_separated_parameters COMMA p=parameters { match c with h::t -> p::h::t }



                                                                                                                                                                        %type <classBody> class_body
                                                                                                                                                                        %type <methodHeader> method_header
                                                                                                                                                                        %type <methodDeclaration> method_declaration
                                                                                                                                                                        %type <parameters> parameters
                                                                                                                                                                        %type <jType> jType
