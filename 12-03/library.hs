
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo =
    [("Amb","Algoritmos"),
     ("Suruba","IHS"),
     ("Kinho","POG"),
     ("Rodrigo","imaginarium"),
     ("Wash","Anjo"),
     ("Fernando","A sutil arte de ligar o foda-se"),
     ("Amb","Como ter ideias geniais"),
     ("Amb","A sutil arte de ligar o foda-se")]
     
livrosDaPessoa :: BancoDados -> Pessoa -> [Livro]
livrosDaPessoa banco pessoa = [b|(a,b) <- banco, a == pessoa]

emprestimosDoLivro :: BancoDados -> Livro -> [Pessoa]
emprestimosDoLivro banco livro = [a|(a,b) <- banco, b == livro]

emprestado :: BancoDados -> Livro -> Bool
emprestado banco livro = [b|(a,b) <- banco, b == livro] /= []

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos banco pessoa = length([a|(a,b) <- banco, a == pessoa])

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar banco pessoa livro = (pessoa,livro):banco

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver banco pessoa livro = [(a,b)|(a,b) <- banco, a /= pessoa || b /= livro]

