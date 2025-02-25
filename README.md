# Linguagens de Programação

Este repositório contém materiais e exercícios da disciplina de Linguagens de Programação.

## Como Compilar e Executar um Arquivo `.pl`

1. **Abra o terminal e execute:**
   ```bash
   swipl
   ```

2. **Carregue (compile) um arquivo no interpretador:**
   - Utilize o predicado `consult/1`:
     ```prolog
     ?- consult('nome_do_arquivo').
     ```

3. **Após carregar, você pode executar os predicados definidos no arquivo.**

## Como Compilar e Executar um Arquivo `.hs`

1. **Instale o ghc**

2. **Abra o terminal e execute:**
```bash
ghci
```

3. **Carregue o arquivo .hs**
```haskell
ghci> :l nomedoarquivo.hs
```

