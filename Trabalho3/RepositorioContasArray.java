public class RepositorioContasArray {
	private Conta[]  contas;
	private int indice;
	private final static int tamanhoCache = 100;

	public RepositorioContasArray() {
	  indice = 0;
	  contas = new Conta[tamanhoCache];
	}

    public Conta [] getContas() {
        return contas;
	}

    public int getIndice() {
        return indice;
    }
    public void inserir(Conta c){
	    contas[indice] = c;
	    indice = indice + 1;
	}

	private int procurarIndice(String num) {
	  int     i = 0;
	  int     ind = -1;
	  boolean achou = false;

	  while ((i < indice) &&!achou) {
	    if ((contas[i].getNumero()).equals(num)) {
		ind = i;
		achou = true;
	    }
	    i = i + 1;
	  }
	  return ind;
	}

	public boolean existe(String num) {
	  boolean resp = false;

	  int i = this.procurarIndice(num);
	  if(i != -1){
		resp = true;
	  }

	  return resp;
	}

	public int atualizar(Conta c){
	  int r = -1;
	  int i = procurarIndice(c.getNumero());
	  if (i != -1) {
	    contas[i] = c;
	    r = 1;
	  } 
	  return r;
	}

	public Conta procurar(String num){
	  Conta c = null;
	  if (existe(num)) {
  	    int i = this.procurarIndice(num);
	    c = contas[i];
	  } else {
	    InterfaceTextual.mostra_mensagem("Conta nao encontrada");
	  }
	  return c;
	}

	public int remover(String num){
	  int r = -1;
	  if (existe(num)) {
  	    int i = this.procurarIndice(num);
	    contas[i] = contas[indice - 1];
	    contas[indice - 1] = null;
	    indice = indice - 1;
	    r = 1;
	  } 
	  return r;
	}
}
