import Frontend.Scheme.Mangler

main = do
  let s = "$get-template"
      s' = mangle s
      s'' = demangle s'

  --putStrLn $ show demangleMap
  mapM_ putStrLn [s, s', s'']

