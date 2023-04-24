# FILE_ASSIST
Assistente para criação/leitura de arquivos.

--------------------------------
<b> Utilização: </b>
As classes foram criadas de forma local dentro do include ZI_ABAP_FILE_LOCAL.
É necessário criar o include no ambiente e chamar onde for necessário.

--------------------------------
<b> Suporte: </b>

Operações:
- Leitura
- Criação

Extensões:
- XLSX / XLS
- CSV / TXT
- PDF

Sistemas:
- Local
- Serv.Aplicação (AL11)

--------------------------------
<b> Objetos: </b>

1. Interface LIF_ABAP_FILE: Tipos e constantes

2. Classe LCL_ABAP_FILE_FACTORY: Classe Factory;
- Método estático CREATE_FILE_FROM_BINARY: Cria arquivo a partir do binário (SOLIX_TAB);
- Método estático CREATE_FILE_FROM_ITAB: Cria arquivo a partir de uma tabela interna (XLS/XLSX/CSV/TXT);
- Método estático READ_FILE_TO_BINARY: Recupera binário (SOLIX_TAB) do arquivo;
- Método estático READ_FILE_TO_ITAB: Recupera valores do arquivo em uma tabela interna;

3. Classe LCL_ABAP_FILE: Classe comum para leitura e criação
- Método estático GET_EXTENSION: Le extensão a partir do caminho do arquivo
- Método SET_FILEATTR: Seta atributos do arquivo

4. Classe LCL_ABAP_FILECREATE: Classe para criação de arquivo
- Método estático GET_INSTANCE: Recupera instancia da classe filha correspondente à extensão
- Método GET_ITABREF: Recupera instancia da tabela interna (LCL_ABAP_ITAB)
- Método SAVE: Salva arquivo
- Método SET_BINARY: Insere dados do arquivo em binario (SOLIX_TAB)
- Método SET_ITABREF: Insere instancia da tabela interna (LCL_ABAP_ITAB)

5. Classe LCL_ABAP_FILEREAD: Classe para leitura de arquivos
- Método estático GET_INSTANCE: Recupera instancia da classe filha correspondente à extensão
- Método SET_ITABREF: Insere instancia da tabela interna (LCL_ABAP_ITAB)
- Método READ: Le arquivo
- Método DISPLAY: Exibe dados
- Método DOWNLOAD: Realiza download do arquivo
- Método GET_BINARY: Recupera arquivo em binario (SOLIX_TAB)
- Método GET_ITABREF: Recupera instancia da tabela interna (LCL_ABAP_ITAB)
- Método GET_XSTRING: Recupera arquivo em XSTRING

6. Classe LCX_ABAP_FILE: Exceções para classes LCL_ABAP_FILE*
- Método CONSTRUCTOR: Recebe exceção previa, ou textid, ou mensagem do sistema (SYMSG), ou texto simples
- Método GET_SYMSG: Recupera mensagem do sistema (SYMSG)
- Método GET_TEXT: Recupera texto da mensagem (independente da origem)

7. Classe LCL_ABAP_ITAB_FACTORY: Classe Factory para tabelas internas dinamicas
- Método estático GET_COMPONENTS_FROM_DDIC: Recupera campos/chaves de um tipo do dicionario
- Método estático GET_COMPONENTS_FROM_ITAB: Recupera campos/chaves de uma tabela interna
- Método estático GET_COMPONENTS_FROM_LIST: Recupera campos/chaves de uma lista de campos
- Método estático CREATE_FROM_ITAB: Cria instancia (LCL_ABAP_ITAB) a partir de uma tabela interna

8. Classe LCL_ABAP_ITAB: Tratamentos para tabelas internas dinamicas
- Método CONSTRUCTOR: Recebe campos e chaves para criacao da tabela interna
- Método SET_VALUES: Seta dados na tabela interna
- Método DISPLAY: Exibe tabela interna (CL_SALV_TABLE)
- Método GET_DATA: Recupera referencia da tabela interna (TYPE REF TO DATA)
- Método GET_VALUES: Recupera valores da tabela interna
- Método GET_XSTRING: Recupera XSTRING da tabela interna

9. Classe LCX_ABAP_ITAB: Exceções para classe LCL_ABAP_ITAB
- Método CONSTRUCTOR: Recebe exceção previa, ou textid, ou mensagem do sistema (SYMSG), ou texto simples
- Método GET_SYMSG: Recupera mensagem do sistema (SYMSG)
- Método GET_TEXT: Recupera texto da mensagem (independente da origem)
