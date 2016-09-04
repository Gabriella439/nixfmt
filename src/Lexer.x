{
module Lexer where

import Control.Monad.Reader (ask)
import Data.ByteString.Lazy (ByteString)
import Prelude hiding (EQ)

import qualified Data.ByteString.Lazy
}

%wrapper "monad-bytestring"

@ID    =     [a-zA-Z\_][a-zA-Z0-9\_\'\-]*
@INT   =     [0-9]+
@FLOAT =     (([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+))([Ee][\+\-]?[0-9]+)?
@PATH  =     [a-zA-Z0-9\.\_\-\+]*(\/[a-zA-Z0-9\.\_\-\+]+)+
@HPATH =     \~(\/[a-zA-Z0-9\.\_\-\+]+)+
@SPATH =     \<[a-zA-Z0-9\.\_\-\+]+(\/[a-zA-Z0-9\.\_\-\+]+)*\>
@URI   =     [a-zA-Z][a-zA-Z0-9\+\-\.]*\:[a-zA-Z0-9\%\/\?\:\@\&\=\+\$\,\-\_\.\!\~\*\']+

tokens :-
  $white+ ;
  <0,iNSIDE_DOLLAR_CURLY>{
    if      { emit IF       }
    then    { emit THEN     }
    else    { emit ELSE     }
    assert  { emit ASSERT   }
    with    { emit WITH     }
    let     { emit LET      }
    in      { emit IN       }
    rec     { emit REC      }
    inherit { emit INHERIT  }
    or      { emit OR_KW    }
    \.\.\.  { emit ELLIPSIS }
    \=\=    { emit EQ       }
    \!\=    { emit NEQ      }
    \<\=    { emit LEQ      }
    \>\=    { emit GEQ      }
    \&\&    { emit AND      }
    \|\|    { emit OR       }
    \-\>    { emit IMPL     }
    \/\/    { emit UPDATE   }
    \+\+    { emit CONCAT   }
    @ID     { capture ID    }
    @INT    { capture INT   }
    @FLOAT  { capture FLOAT }
    \$\{    { emit DOLLAR_CURLY `andBegin` iNSIDE_DOLLAR_CURLY
}

{
emit :: Token -> AlexAction Token
emit x = \_ _ -> return x

alexEOF :: Alex Token
alexEOF = return EOF

capture :: (ByteString -> Token) -> AlexAction Token
capture k (_, _, rest, _) len = return (k bytes)
  where
    bytes = Data.ByteString.Lazy.take len rest

data Token
    = IF
    | THEN
    | ELSE
    | ASSERT
    | WITH
    | LET
    | IN
    | REC
    | INHERIT
    | OR_KW
    | ELLIPSIS
    | EQ
    | NEQ
    | LEQ
    | GEQ
    | AND
    | OR
    | IMPL
    | UPDATE
    | CONCAT
    | ID ByteString
    | INT ByteString
    | FLOAT ByteString
    | EOF
    deriving (Show)
}
