converter :: Int -> String -> String
converter ano palavra = converterAux ano palavra []

converterAux :: Int -> String -> String -> String
converterAux ano [] retorno = retorno
converterAux ano (a:as) retorno
	| (ano == 5) && (as == []) && ((a == 'a') || (a == 'e') || (a == 'i') || (a == 'o') || (a == 'u')) = retorno
	| (ano >= 3) && (as == []) && (a == 'h') = retorno
	| (as == []) = retorno ++ [a]
	| (a == 's') && (b == 's') = converterAux ano bs (retorno ++ ['s'])
	| (a == 'c') && ((b == 'a') || (b == 'e') || (b == 'i')) =  converterAux ano as (retorno ++ ['s'])
	| (a == 'c') && ((b == 'o') || (b == 'u')) =  converterAux ano as (retorno ++ ['k'])
	| (ano >= 2) && (a == 'q') && (b == 'u') = converterAux ano bs (retorno ++ ['k'])
	| (ano >= 3) && (b == 'h') && ((a == 'c')) = converterAux ano bs (retorno ++ ['x'])
	| (ano >= 3) && (b == 'h') && ((a == 'n')) = converterAux ano bs (retorno ++ ['n', 'i'])
	| (ano >= 3) && (b == 'h') && ((a == 'l')) = converterAux ano bs (retorno ++ ['l', 'i'])
	| (ano >= 3) && (a == 'h') = converterAux ano as retorno
	| (ano >= 4) && (a == 'j') && ((b == 'a') || (b == 'o')) =  converterAux ano as (retorno ++ ['x'])
	| (ano >= 4) && (a == 'j') && ((b == 'e') || (b == 'i') || (b == 'u')) =  converterAux ano as (retorno ++ ['g'])
	| (ano == 5) && ((a == 'a') || (a == 'e') || (a == 'i') || (a == 'o') || (a == 'u')) = converterAux ano as retorno
	| otherwise = converterAux ano as (retorno ++ [a])
		where (b:bs) = as