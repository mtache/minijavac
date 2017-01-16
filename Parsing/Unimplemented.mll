
    | "?"           { QUESTION }
        | "+="          {SELFADD}
            | "-="          {SELFSUB}
                | "*="          {SELFMUL}
                    | "/="          {SELFDIV}
                        | "&="          {SELFAND}
                            | "|="          {SELFOR}
                                | "^="          {SELFXOR}
                                    | "%="          {SELFMOD}
                                        | "<<="         {SELFLEFTSHIFT}
                                            | ">>="         {SELFRIGHTSHIFT}
                                                | ">>>="        {USELFRIGHTSHIFT}
                                                    | "||"          {OR}
                                                        | "&&"          {AND}
                                                            | "|"           {BOR}
                                                                | "^"           {BXOR}
                                                                    | "&"           {BAND}
                                                                        | "=="          {EQUAL}
                                                                            | "!="          {NOTEQUAL}
                                                                                | "<"           {LESS}
                                                                                    | ">"           {GREATER}
                                                                                        | "<="          {LESSEQUAL}
                                                                                            | ">="          {GREATEREAQUAL}
                                                                                                | "<<"          {LSHIFT}
                                                                                                    | ">>"          {RSHIFT}
                                                                                                        | ">>>"         {ZFRSHIFT}
                                                                                                            | "++"          {INCREMENT}
                                                                                                                | "--"          {DECREMENT}
                                                                                                                    | "!"           {NEGATION}
                                                                                                                        | "~"         {BCOMPLEMENT}

