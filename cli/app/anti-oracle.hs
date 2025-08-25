import Oracle.Process (oracleProcess, parseArgs)

main :: IO ()
main = parseArgs >>= oracleProcess
