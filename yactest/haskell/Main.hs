import Test.HUnit

import Frontend.Ca.Parser
import Frontend.Ca.AST

test1 = TestCase (assertEqual "parse 'extern a;'"
                              (parseString "extern a;")
                              (MkProgram [ScopeDef (Extern "a")]))

tests = TestList [TestLabel "test1" test1]

main = runTestTT tests
