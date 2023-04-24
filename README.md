# FILE_ASSIST
Assistente para criação/leitura de arquivos.

Extensões implementadas:
- XLSX
- XLS
- CSV
- TXT
- PDF

Sistemas:
- Local
- Serv.Aplicação (AL11)

--------------------------------

1) Interface LIF_ABAP_FILE: Tipos e constantes

--------------------------------

1) Classe LCL_ABAP_FILE_FACTORY: Classe Factory
1.1) Método estático CREATE_FILE_FROM_BINARY: Cria arquivo a partir do binário (SOLIX_TAB)
1.2) Método estático CREATE_FILE_FROM_ITAB: Cria arquivo a partir de uma tabela interna (XLS/XLSX/CSV/TXT)
1.3) Método estático READ_FILE_TO_BINARY: Recupera binário (SOLIX_TAB) do arquivo
1.4) Método estático READ_FILE_TO_ITAB: Recupera valores do arquivo em uma tabela interna

2) Classe LCL_ABAP_FILE: Classe comum para leitura e criação
2.1) Método estático GET_EXTENSION: Le extensão a partir do caminho do arquivo
2.2) Método SET_FILEATTR: Seta atributos do arquivo

3) Classe LCL_ABAP_FILECREATE: Classe para criação de arquivo
3.1) Método estático GET_INSTANCE: Recupera instancia da classe filha correspondente à extensão
3.1) Método GET_ITABREF: Recupera instancia da tabela interna (LCL_ABAP_ITAB)
3.1) Método SAVE: Salva arquivo
3.1) Método SET_BINARY: Insere dados do arquivo em binario (SOLIX_TAB)
3.1) Método SET_ITABREF: Insere instancia da tabela interna (LCL_ABAP_ITAB)

4) Classe LCL_ABAP_FILEREAD: Classe para leitura de arquivos
4.4) Método estático GET_INSTANCE: Recupera instancia da classe filha correspondente à extensão
4.8) Método SET_ITABREF: Insere instancia da tabela interna (LCL_ABAP_ITAB)
4.7) Método READ: Le arquivo
4.1) Método DISPLAY: Exibe dados
4.2) Método DOWNLOAD: Realiza download do arquivo
4.3) Método GET_BINARY: Recupera arquivo em binario (SOLIX_TAB)
4.5) Método GET_ITABREF: Recupera instancia da tabela interna (LCL_ABAP_ITAB)
4.6) Método GET_XSTRING: Recupera arquivo em XSTRING

5) Classe LCX_ABAP_FILE: Exceções para classes LCL_ABAP_FILE*
5.1) Método CONSTRUCTOR: Recebe exceção previa, ou textid, ou mensagem do sistema (SYMSG), ou texto simples
5.2) Método GET_SYMSG: Recupera mensagem do sistema (SYMSG)
5.3) Método GET_TEXT: Recupera texto da mensagem (independente da origem)

--------------------------------

# ITAB
Classe auxiliar para tabelas internas dinamicas.

1) Classe LCL_ABAP_ITAB_FACTORY: Classe Factory para tabelas internas dinamicas
1.1) Método estático GET_COMPONENTS_FROM_DDIC: Recupera campos/chaves de um tipo do dicionario
1.2) Método estático GET_COMPONENTS_FROM_ITAB: Recupera campos/chaves de uma tabela interna
1.3) Método estático GET_COMPONENTS_FROM_LIST: Recupera campos/chaves de uma lista de campos
1.4) Método estático CREATE_FROM_ITAB: Cria instancia (LCL_ABAP_ITAB) a partir de uma tabela interna

2) Classe LCL_ABAP_ITAB: Tratamentos para tabelas internas dinamicas
2.1) Método CONSTRUCTOR: Recebe campos e chaves para criacao da tabela interna
2.2) Método SET_VALUES: Seta dados na tabela interna
2.3) Método DISPLAY: Exibe tabela interna (CL_SALV_TABLE)
2.4) Método GET_DATA: Recupera referencia da tabela interna (TYPE REF TO DATA)
2.5) Método GET_VALUES: Recupera valores da tabela interna
2.6) Método GET_XSTRING: Recupera XSTRING da tabela interna

3) Classe LCX_ABAP_ITAB: Exceções para classe LCL_ABAP_ITAB
3.1) Método CONSTRUCTOR: Recebe exceção previa, ou textid, ou mensagem do sistema (SYMSG), ou texto simples
3.2) Método GET_SYMSG: Recupera mensagem do sistema (SYMSG)
3.3) Método GET_TEXT: Recupera texto da mensagem (independente da origem)
